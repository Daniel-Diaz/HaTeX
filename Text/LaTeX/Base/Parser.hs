{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module     : Text/LaTeX/Base/Parser.hs
-- Copyright  : (c) Tobias Schoofs
-- License    : LGPL 
-- Stability  : experimental
-- Portability: portable
--
-- LaTeX Parser based on Attoparsec
-------------------------------------------------------------------------------
module Text.LaTeX.Base.Parser (
    parseLaTeX
  , latexParser
  , latexBlockParser
  , latexAtOnce
  , latexDocParser
#ifdef _TEST
  , specials
#endif
    ) where

import           Data.Attoparsec.Text hiding (take, takeTill)
import qualified Data.Attoparsec.Text as A   (takeTill)
import           Data.Char (toLower)
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T 

import           Control.Applicative ((<|>), (<$>))
import           Control.Monad (unless)

import           Text.LaTeX.Base.Syntax

-- | Parse a 'Text' sequence as a 'LaTeX' block. If it fails, it returns
--   an error string.
parseLaTeX :: Text -> Either String LaTeX
parseLaTeX t | T.null t  = return TeXEmpty
             | otherwise = 
  case parse latexParser t of
    Fail _ _ e     -> Left e
    Done _ r       -> Right r
    rx@(Partial _) -> -- Left "incomplete input"
                      case feed rx T.empty of
                        Fail _ _ e -> Left e
                        Partial _  -> Left "incomplete input"
                        Done _ r   -> Right r

{-# DEPRECATED latexAtOnce "Use parseLaTeX instead." #-}

latexAtOnce :: Text -> Either String LaTeX
latexAtOnce = parseLaTeX

------------------------------------------------------------------------
-- | Incremental Parser that terminates after the /document/ envionment
------------------------------------------------------------------------
latexDocParser :: Parser LaTeX
latexDocParser = blockTillDoc

------------------------------------------------------------------------
-- | Incremental 'LaTeX' parser.
------------------------------------------------------------------------
latexParser :: Parser LaTeX
latexParser = mconcat <$> latexBlockParser `manyTill` endOfInput 

blockTillDoc :: Parser LaTeX
blockTillDoc = do
  b <- latexBlockParser
  if isMainDoc b then return  b
                 else mappend b <$> blockTillDoc

-- | Test if a 'LaTeX' block is a @document@ environment.
isMainDoc :: LaTeX -> Bool
isMainDoc (TeXEnv "document" _ _) = True
isMainDoc _ = False

-- | Parser of a single 'LaTeX' constructor, no appending blocks.
latexBlockParser :: Parser LaTeX
latexBlockParser = foldr1 (<|>) [text, dolMath, comment, text2, command, environment]
-- Note: text stops on ']'; if the other parsers fail on the rest
--       text2 handles it, starting with ']' 
  
------------------------------------------------------------------------
-- Text
------------------------------------------------------------------------
text :: Parser LaTeX
text = do
  mbC <- peekChar
  case mbC of
    Nothing -> fail "text: Empty input."
    Just c | c `elem` "$%\\{]}" -> fail "not text"
           | otherwise          -> TeXRaw <$> A.takeTill (`elem` "$%\\{]}")

------------------------------------------------------------------------
-- Text without stopping on ']'
------------------------------------------------------------------------
text2 :: Parser LaTeX
text2 = do
  _ <- char ']'
  t <- try (text <|> return (TeXRaw T.empty))
  return $ TeXRaw (T.pack "]") <> t

------------------------------------------------------------------------
-- Environment
------------------------------------------------------------------------
environment :: Parser LaTeX
environment = anonym <|> env

anonym :: Parser LaTeX
anonym = char '{' >> 
    TeXBraces . mconcat <$> latexBlockParser `manyTill` char '}'

env :: Parser LaTeX
env = do
  _  <- char '\\'
  n  <- envName "begin"
  as <- fmap (fromMaybe []) cmdArgs
  b  <- envBody n 
  return $ TeXEnv (T.unpack n) as b

envName :: Text -> Parser Text
envName k = do
  _ <- string k
  _ <- char '{'
  n <- A.takeTill (== '}')
  _ <- char '}'
  return n

envBody :: Text -> Parser LaTeX
envBody n = mconcat <$> (bodyBlock n) `manyTill` endenv
  where endenv = try $ string ("\\end{" <> n <> "}")

bodyBlock :: Text -> Parser LaTeX
bodyBlock n = do
  c <- peekChar
  case c of 
     Just _ -> latexBlockParser
     _ -> fail $ "Environment '" <> T.unpack n <> "' not finalized."

------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------
command :: Parser LaTeX
command = do
  _    <- char '\\'
  mbX  <- peekChar
  case mbX of
    Nothing -> return TeXEmpty
    Just x  -> if isSpecial x
                  then special
                  else do
                    c  <- A.takeTill endCmd
                    if c `elem` ["begin","end"]
                       then fail $ "Command not allowed: " ++ T.unpack c
                       else maybe (TeXCommS $ T.unpack c) (TeXComm $ T.unpack c) <$> cmdArgs

------------------------------------------------------------------------
-- Command Arguments
------------------------------------------------------------------------
cmdArgs :: Parser (Maybe [TeXArg])
cmdArgs = try (string "{}" >> return (Just []))
            <|> fmap Just (many1 cmdArg)
            <|> return Nothing

cmdArg :: Parser TeXArg
cmdArg = do
  c <- char '[' <|> char '{'
  let e = case c of
            '[' -> "]"
            '{' -> "}"
            _   -> error "this cannot happen!"
  b <- mconcat <$> latexBlockParser `manyTill` string e
  case c of  
    '[' -> return $ OptArg b
    '{' -> return $ FixArg b
    _   -> error "this cannot happen!"

------------------------------------------------------------------------
-- Special commands (consisting of one char)
------------------------------------------------------------------------
special :: Parser LaTeX
special = do
  x <- anyChar
  case x of
    '('  -> math Parentheses "\\)"
    '['  -> math Square      "\\]"
    '{'  -> lbrace
    '}'  -> rbrace
    '|'  -> vert
    '\\' -> lbreak
    _    -> commS [x]

------------------------------------------------------------------------
-- Line break
------------------------------------------------------------------------
lbreak :: Parser LaTeX
lbreak = do
  y <- try (char '[' <|> char '*' <|> return ' ')  
  case y of
    '[' -> linebreak False
    '*' -> do z <- try (char '[' <|> return ' ')
              case z of
               '[' -> linebreak True
               _   -> return (TeXLineBreak Nothing True)
    _   -> return (TeXLineBreak Nothing False)

linebreak :: Bool -> Parser LaTeX
linebreak t = do m <- measure
                 _ <- char ']'
                 s <- try (char '*' <|> return ' ')
                 return $ TeXLineBreak (Just m) (t || s == '*')

measure :: Parser Measure
measure = try (double >>= unit) <|> CustomMeasure <$> latexBlockParser

unit :: Double -> Parser Measure
unit f = do
  u1 <- anyChar
  u2 <- anyChar
  case map toLower [u1, u2] of
    "pt" -> return $ Pt f
    "mm" -> return $ Mm f
    "cm" -> return $ Cm f
    "in" -> return $ In f
    "ex" -> return $ Ex f
    "em" -> return $ Em f
    _    -> fail "NaN"

------------------------------------------------------------------------
-- Right or left brace or vertical
------------------------------------------------------------------------
rbrace, lbrace,vert :: Parser LaTeX
lbrace = brace "{"
rbrace = brace "}"
vert   = brace "|"

brace :: String -> Parser LaTeX
brace = return . TeXCommS

commS :: String -> Parser LaTeX
commS = return . TeXCommS

------------------------------------------------------------------------
-- Math
------------------------------------------------------------------------
dolMath :: Parser LaTeX
dolMath = do
  _ <- char '$' 
  b <- mconcat <$> latexBlockParser `manyTill` char '$'
  return $ TeXMath Dollar b

math :: MathType -> Text -> Parser LaTeX
math t eMath = do
   b <- mconcat <$> latexBlockParser `manyTill` try (string eMath)
   return $ TeXMath t b

------------------------------------------------------------------------
-- Comment 
------------------------------------------------------------------------
comment :: Parser LaTeX
comment = do
  _  <- char '%'
  c  <- A.takeTill (== '\n')
  e  <- atEnd
  unless e (char '\n' >>= \_ -> return ())
  return $ TeXComment c

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------
isSpecial :: Char -> Bool
isSpecial = (`elem` specials)

endCmd :: Char -> Bool
endCmd c = notLowercaseAlph && notUppercaseAlph
 where c' = fromEnum c
       notLowercaseAlph = c' < fromEnum 'a' || c' > fromEnum 'z'
       notUppercaseAlph = c' < fromEnum 'A' || c' > fromEnum 'Z'


specials :: String
specials = "'(),.-\"!^$&#{}%~|/:;=[]\\` "
