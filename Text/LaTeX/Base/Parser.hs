
-- | This is the 'LaTeX' parser module.
module Text.LaTeX.Base.Parser
 ( parseLaTeX
 , ParseError (..)
   ) where

import Text.LaTeX.Base.Syntax
--
import Text.Parsec
import Text.Parsec.Text
--
import Control.Applicative hiding ((<|>),many)
import Data.Monoid
import Data.String
import Data.Text

p_Comm :: Parser LaTeX
p_Comm = do
  char '\\'
  str <- many1 alphaNum
  args <- try   (string "{}" >> return [])
            <|> (many1 p_Arg)
  return $ TeXComm str args

p_CommS :: Parser LaTeX
p_CommS = do
  char '\\'
  TeXCommS <$> many1 alphaNum

p_Env :: Parser LaTeX
p_Env = do
  string "\\begin{"
  str <- many1 alphaNum
  char '}'
  args <- many p_Arg
  ls <- manyTill p_TeXU $ try $ string $ "\\end{" ++ str ++ "}"
  return $ TeXEnv str args $ mconcat ls

p_Math :: Parser LaTeX
p_Math = choice $ try <$> [p_inline, p_display]
 where p_inline = do
         char '$'
         TeXMath InlineTeXMath . mconcat <$> p_TeXU `manyTill` char '$'
       p_display = do
         string "\\["
         TeXMath DisplayTeXMath . mconcat <$> p_TeXU `manyTill` string "\\]"

p_NewLine :: Parser LaTeX
p_NewLine = do
 string "\\\\"
 fmap TeXNewLine $ (char '*' >> return True) <|> return False

p_RawG :: [Char] -> Parser LaTeX
p_RawG xs = TeXRaw . fromString <$> manyTill anyChar (lookAhead $ (oneOf xs >> return mempty) <|> p_TeXURaw)

p_Raw :: Parser LaTeX
p_Raw = p_RawG []

p_RawNC :: Parser LaTeX
p_RawNC = p_RawG ","

p_TeXU :: Parser LaTeX
p_TeXU = choice $ try <$> [p_NewLine , p_Math , p_Env , p_Comm , p_CommS , p_Raw]

p_TeXURaw :: Parser LaTeX
p_TeXURaw = choice $ try <$> [p_NewLine , p_Math , p_Env , p_Comm , p_CommS]

p_TeX :: Parser LaTeX
p_TeX = mconcat <$> many p_TeXU

p_TeXUNC :: Parser LaTeX
p_TeXUNC = choice $ try <$> [p_NewLine , p_Math , p_Env , p_Comm , p_CommS , p_RawNC]

p_TeXNC :: Parser LaTeX
p_TeXNC = mconcat <$> many p_TeXUNC

p_SimpleArg :: (Char,Char) -> (LaTeX -> TeXArg) -> Parser TeXArg
p_SimpleArg (c1,c2) f = do
  char c1
  fmap (f . mconcat) $ manyTill (p_TeXURaw <|> p_RawG [c2]) $ char c2

p_MultiArg :: (Char,Char) -> ([LaTeX] -> TeXArg) -> Parser TeXArg
p_MultiArg (c1,c2) f = do
  char c1
  fmap f $ sepBy1 (mconcat <$> manyTill p_TeXUNC (char c2)) $ char ','

p_Opt :: Parser TeXArg
p_Opt = p_SimpleArg ('[',']') OptArg

p_MOpt :: Parser TeXArg
p_MOpt = p_MultiArg ('[',']') MOptArg

p_Fix :: Parser TeXArg
p_Fix = p_SimpleArg ('{','}') FixArg

p_Sym :: Parser TeXArg
p_Sym = p_SimpleArg ('<','>') SymArg

p_MSym :: Parser TeXArg
p_MSym = p_MultiArg ('<','>') MSymArg

p_Arg :: Parser TeXArg
p_Arg = choice $ try <$> [ p_MOpt , p_MSym , p_Opt , p_Fix , p_Sym ]

-- | Parse a LaTeX expression written in 'Text', to a 'LaTeX' AST.
--   It returns a 'ParseError' in case of parsing error.
parseLaTeX :: Text -> Either ParseError LaTeX
parseLaTeX = parse (p_TeX >>= \l -> eof >> return l) "LaTeX input"