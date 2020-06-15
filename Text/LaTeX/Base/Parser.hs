
{-# LANGUAGE OverloadedStrings, CPP #-}

-- | The /LaTeX/ parser.
-- 
--   Use 'parseLaTeX' to parse a 'Text' containing /LaTeX/ code.
--   If the 'Text' is in a file, you may want to use 'parseLaTeXFile'.
--   Use this module together with "Text.LaTeX.Base.Syntax" to perform
--   analysis and transformations of /LaTeX/ code. The parser ('parseLaTeX')
--   is related with the renderer ('render') by the following property:
--
--   /If @t :: Text@ is a syntactically valid LaTeX block, then:/
--
-- > fmap render (parseLaTeX t) == Right t
-- 
--   This property says two things:
--
-- * Given a valid LaTeX input, 'parseLaTeX' returns a 'LaTeX' value.
-- * If the parsed value is again rendered, you get the initial input.
--
--   In other words, 'parseLaTeX' is a partial function defined over the
--   set of valid LaTeX files, and 'render' is its /left/ inverse.
--
module Text.LaTeX.Base.Parser (
    -- * The parser
    parseLaTeX
  , parseLaTeXFile
  , parseLaTeXPos
  , parseLaTeXPosFile
    -- * Parsing errors
  , ParseError
  , errorPos
  , errorMessages
    -- ** Error messages
  , Message (..)
  , messageString
    -- ** Source positions
  , SourcePos
  , sourceLine
  , sourceColumn
  , sourceName
    -- * Configuring your parser
  , ParserConf (..)
  , defaultParserConf
  , parseLaTeXWith
  , parseLaTeXFileWith
  , parseLaTeXPosWith
  , parseLaTeXPosFileWith
    -- * Parser combinators
  , Parser
  , latexParser
  , latexBlockParser
    ) where

import           Text.Parsec hiding ((<|>),many)
import           Text.Parsec.Error
import           Data.Char (toLower,digitToInt)
import           Data.Functor(($>))
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif
import           Data.Maybe (fromMaybe)
import           Data.Set(Set, fromList, member)
import qualified Data.Text as T 

import           Control.Applicative
import           Control.Monad (unless)

import           Text.LaTeX.Base.Syntax
import           Text.LaTeX.Base.Render

------------------------------------------------------------------------
-- Parser configuration
------------------------------------------------------------------------

-- | Configuration for the LaTeX parser.
newtype ParserConf = ParserConf
  { -- | This is the list of names of the environments such that
    --   their content will be parsed verbatim.
    verbatimEnvironments :: [String]
    }

-- | Default parser configuration, used by 'parseLaTeX' and 'parseLaTeXFile'.
--
--   Defaults:
--
-- > verbatimEnvironments = ["verbatim"]
--
defaultParserConf :: ParserConf
defaultParserConf = ParserConf
  { verbatimEnvironments = ["verbatim"]
    }

-- | Parser with 'Text' input and 'ParserConf' environment.
type Parser = Parsec Text ParserConf

------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------

-- | Parse a 'Text' sequence as a 'LaTeX' block. If it fails, it returns
--   an error string.
parseLaTeX :: Text -> Either ParseError LaTeX
parseLaTeX = fmap (fmap (const ())) . parseLaTeXPos

-- | Parse a 'Text' sequence as a 'LaTeXL' block. If it fails, it returns
--   an error string.
parseLaTeXPos :: Text -> Either ParseError (LaTeXL SourcePos)
parseLaTeXPos = parseLaTeXPosWith defaultParserConf

parseLaTeXWith :: ParserConf -> Text -> Either ParseError LaTeX
parseLaTeXWith conf = fmap (fmap (const ())) . parseLaTeXPosWith conf

parseLaTeXPosWith :: ParserConf -> Text -> Either ParseError (LaTeXL SourcePos)
parseLaTeXPosWith conf t
  | T.null t  = return TeXEmpty
  | otherwise = runParser latexParser conf "parseLaTeX input" t

-- | Read a file and parse it as 'LaTeX'.
parseLaTeXPosFile :: FilePath -> IO (Either ParseError (LaTeXL SourcePos))
parseLaTeXPosFile = parseLaTeXPosFileWith defaultParserConf

-- | Read a file and parse it as 'LaTeX'.
parseLaTeXFile :: FilePath -> IO (Either ParseError LaTeX)
parseLaTeXFile = fmap (fmap $ fmap $ const ()) . parseLaTeXPosFileWith defaultParserConf

parseLaTeXFileWith :: ParserConf -> FilePath -> IO (Either ParseError LaTeX)
parseLaTeXFileWith conf = fmap (fmap $ fmap $ const ()) . parseLaTeXFileWith conf

parseLaTeXPosFileWith :: ParserConf -> FilePath -> IO (Either ParseError (LaTeXL SourcePos))
parseLaTeXPosFileWith conf fp = runParser latexParser conf fp <$> readFileTex fp

-- | The 'LaTeX' parser.
latexParser :: Parser (LaTeXL SourcePos)
latexParser = mconcat <$> latexBlockParser `manyTill` eof

-- | Parser of a single 'LaTeX' constructor, no appending blocks.
latexBlockParser :: Parser (LaTeXL SourcePos)
latexBlockParser = foldr1 (<|>)
  [ text            <?> "text"
  , dolMath         <?> "inline math ($)"
  , comment         <?> "comment"
  , text2           <?> "text2"
  , try environment <?> "environment"
  , command         <?> "command"
    ]
-- Note: text stops on ']'; if the other parsers fail on the rest
--       text2 handles it, starting with ']' 

------------------------------------------------------------------------
-- Text
------------------------------------------------------------------------
nottext :: Set Char
nottext = fromList "$%\\{]}"

text :: Parser (LaTeXL SourcePos)
text = do
  pos <- getPosition
  mbC <- peekChar
  case mbC of
    Nothing -> fail "text: Empty input."
    Just c | c `member` nottext -> fail "not text"
           | otherwise          -> TeXRawL pos <$> takeTill (`member` nottext)

------------------------------------------------------------------------
-- Text without stopping on ']'
------------------------------------------------------------------------
text2 :: Parser (LaTeXL SourcePos)
text2 = do
  _ <- char ']'
  p <- getPosition
  t <- try (text <|> return (TeXRawL p T.empty))
  return $ TeXRawL p (T.pack "]") <> t

------------------------------------------------------------------------
-- Environment
------------------------------------------------------------------------
environment :: Parser (LaTeXL SourcePos)
environment = anonym <|> env

anonym :: Parser (LaTeXL SourcePos)
anonym = do
  _ <- char '{'
  l <- TeXBraces . mconcat <$> many latexBlockParser
  _ <- char '}'
  return l

env :: Parser (LaTeXL SourcePos)
env = do
  p0  <- getPosition
  n   <- char '\\' *> envName "begin"
  sps <- many $ char ' '
  let lsps = if null sps then mempty else TeXRawL p0 $ T.pack sps
  as <- cmdArgs
  verbatims <- verbatimEnvironments <$> getState
  if n `elem` verbatims
     then let endenv = try $ string "\\end" >> spaces >> string ("{" <> n <> "}")
          in do cont <- manyTill anyChar endenv
                p1   <- getPosition
                return $ TeXEnvL p0 p1 n (fromMaybe [] as) $ TeXRawL p0 (T.pack cont)
     else do b  <- envBody n 
             p1 <- getPosition
             return $ TeXEnvL p0 p1 n (fromMaybe [] as) $
               case as of
                Just [] -> lsps <> TeXBraces mempty <> b
                Nothing -> lsps <> b
                _ -> b

envName :: String -> Parser String
envName k = do
  _ <- string k
  _ <- char '{'
  n <- takeTill (== '}')
  _ <- char '}'
  return $ T.unpack n

envBody :: String -> Parser (LaTeXL SourcePos)
envBody n = mconcat <$> bodyBlock n `manyTill` endenv
  where endenv = try $ string "\\end" >> spaces >> string ("{" <> n <> "}")

bodyBlock :: String -> Parser (LaTeXL SourcePos)
bodyBlock n = do
  c <- peekChar
  case c of 
     Just _ -> latexBlockParser
     _ -> fail $ "Environment '" <> n <> "' not finalized."

------------------------------------------------------------------------
-- Command
------------------------------------------------------------------------
command :: Parser (LaTeXL SourcePos)
command = do
  _    <- char '\\'
  mbX  <- peekChar
  case mbX of
    Nothing -> return TeXEmpty
    Just x  -> if isSpecial x
                  then special
                  else do
                    p0 <- getPosition
                    c  <- takeTill endCmd
                    maybe (TeXCommSL p0 $ T.unpack c) (TeXCommL p0 $ T.unpack c) <$> cmdArgs

------------------------------------------------------------------------
-- Command Arguments
------------------------------------------------------------------------
cmdArgs :: Parser (Maybe [TeXArgL SourcePos])
cmdArgs = try (string "{}" >> return (Just []))
            <|> fmap Just (try $ many1 cmdArg)
            <|> return Nothing

cmdArg :: Parser (TeXArgL SourcePos)
cmdArg = do
  c <- char '[' <|> char '{'
  let e = case c of
            '[' -> "]"
            '{' -> "}"
            _   -> error "this cannot happen!"
  b <- mconcat <$> manyTill latexBlockParser (string e)
  case c of  
    '[' -> return $ OptArg b
    '{' -> return $ FixArg b
    _   -> error "this cannot happen!"

------------------------------------------------------------------------
-- Special commands (consisting of one char)
------------------------------------------------------------------------
special :: Parser (LaTeXL SourcePos)
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

isSpecial :: Char -> Bool
isSpecial = (`elem` specials)

------------------------------------------------------------------------
-- Line break
------------------------------------------------------------------------

lbreak :: Parser (LaTeXL SourcePos)
lbreak = do
  y <- try (char '[' <|> char '*' <|> return ' ')  
  case y of
    '[' -> linebreak False
    '*' -> do z <- try (char '[' <|> return ' ')
              case z of
               '[' -> linebreak True
               _   -> return (TeXLineBreak Nothing True)
    _   -> return (TeXLineBreak Nothing False)

linebreak :: Bool -> Parser (LaTeXL SourcePos)
linebreak t = do m <- measure <?> "measure"
                 _ <- char ']'
                 s <- try (char '*' <|> return ' ')
                 return $ TeXLineBreak (Just m) (t || s == '*')

measure :: Parser (MeasureL SourcePos)
measure = try (floating >>= unit) <|> (CustomMeasure . mconcat) <$> manyTill latexBlockParser (lookAhead $ char ']')

unit :: Double -> Parser (MeasureL SourcePos)
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
rbrace, lbrace,vert :: Parser (LaTeXL SourcePos)
lbrace = brace "{"
rbrace = brace "}"
vert   = brace "|"

brace :: String -> Parser (LaTeXL SourcePos)
brace s = flip TeXCommSL s <$> getPosition -- The same as commS?

commS :: String -> Parser (LaTeXL SourcePos)
commS s = flip TeXCommSL s <$> getPosition

------------------------------------------------------------------------
-- Math
------------------------------------------------------------------------
dolMath :: Parser (LaTeXL SourcePos)
dolMath = do
  p <- getPosition
  _ <- char '$' 
  choice
    [ do _ <- char '$'
         b <- mconcat <$> latexBlockParser `manyTill` try (string "$$")
         return $ TeXMathL p DoubleDollar b
    , do b <- mconcat <$> latexBlockParser `manyTill` char '$'
         return $ TeXMathL p Dollar b
      ]

math :: MathType -> String -> Parser (LaTeXL SourcePos)
math t eMath = do
   p <- getPosition
   b <- mconcat <$> latexBlockParser `manyTill` try (string eMath)
   return $ TeXMathL p t b

------------------------------------------------------------------------
-- Comment 
------------------------------------------------------------------------
comment :: Parser (LaTeXL SourcePos)
comment = do
  _  <- char '%'
  c  <- takeTill (== '\n')
  e  <- atEnd
  unless e (char '\n' >>= \_ -> return ())
  return $ TeXComment c

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

endCmd :: Char -> Bool
endCmd c = notLowercaseAlph && notUppercaseAlph
 where c' = fromEnum c
       notLowercaseAlph = c' < fromEnum 'a' || c' > fromEnum 'z'
       notUppercaseAlph = c' < fromEnum 'A' || c' > fromEnum 'Z'

specials :: String
specials = "'(),.-\"!^$&#{}%~|/:;=[]\\` "

peekChar :: Parser (Maybe Char)
peekChar = Just <$> try (lookAhead anyChar) <|> pure Nothing

atEnd :: Parser Bool
atEnd = (eof $> True) <|> pure False

takeTill :: (Char -> Bool) -> Parser Text
takeTill p = T.pack <$> many (satisfy (not . p))

-- Parsing doubles
--
-- Code for 'floating', 'fractExponent', and 'sign' comes from parsers package:
--
-- http://hackage.haskell.org/package/parsers
--

floating :: Parser Double
floating = decimal <**> fractExponent

fractExponent :: Parser (Integer -> Double)
fractExponent = (\fract expo n -> (fromInteger n + fract) * expo) <$> fraction <*> option 1.0 exponent'
            <|> (\expo n -> fromInteger n * expo) <$> exponent' where
  fraction = foldr op 0.0 <$> (char '.' *> (some digit <?> "fraction"))
  op d f = (f + fromIntegral (digitToInt d))/10.0
  exponent' = ((\f e -> power (f e)) <$ oneOf "eE" <*> sign <*> (decimal <?> "exponent")) <?> "exponent"
  power e
    | e < 0     = 1.0/power(-e)
    | otherwise = fromInteger (10^e)

decimal :: Parser Integer
decimal = read <$> many1 digit

sign :: Parser (Integer -> Integer)
sign = negate <$ char '-'
   <|> id <$ char '+'
   <|> pure id
