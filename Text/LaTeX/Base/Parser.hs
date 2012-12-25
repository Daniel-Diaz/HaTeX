{-# LANGUAGE CPP #-}
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
                        latexParser,
                        latexBlockParser,
                        latexAtOnce,
                        latexDocParser,
                        isMainDoc
#ifdef _TEST
                        , specials
#endif
                      )
where

  import           Data.Attoparsec.Text hiding (take, takeTill)
  import qualified Data.Attoparsec.Text as A   (takeTill)
  import           Data.Char (toLower)
  import           Data.Monoid
  import           Data.Text (Text)
  import qualified Data.Text as T 

  import           Control.Applicative ((<|>), (<$>))
  import           Control.Monad (unless)

  import           Text.LaTeX.Base.Syntax

  import           GHC.Float (double2Float)

  ------------------------------------------------------------------------
  -- | Parses a Text sequence at once;
  --   may fail or conclude.
  ------------------------------------------------------------------------
  latexAtOnce :: Text -> Either String LaTeX
  latexAtOnce t | T.null t  = return TeXEmpty
                | otherwise = 
    case parse latexParser t of
      Fail _ _ e     -> Left e
      Done _ r       -> Right r
      rx@(Partial _) -> -- Left "incomplete input"
                        case feed rx T.empty of
                         Fail _ _ e -> Left e
                         Partial _  -> Left "incomplete input"
                         Done _ r   -> Right r

  ------------------------------------------------------------------------
  -- | The incremental LaTeX Parser
  ------------------------------------------------------------------------
  latexParser :: Parser LaTeX
  latexParser = blocks 

  ------------------------------------------------------------------------
  -- | Incremental Parser for single blocks of LaTeX
  ------------------------------------------------------------------------
  latexBlockParser :: Parser LaTeX
  latexBlockParser = block

  ------------------------------------------------------------------------
  -- | Incremental Parser that terminates after the /document/ envionment
  ------------------------------------------------------------------------
  latexDocParser :: Parser LaTeX
  latexDocParser = blockTillDoc

  ------------------------------------------------------------------------
  -- Blocks
  ------------------------------------------------------------------------
  blocks :: Parser LaTeX
  blocks = mconcat <$> block `manyTill` endOfInput 

  blockTillDoc :: Parser LaTeX
  blockTillDoc  = do
    b <- block
    if isMainDoc b then return  b
                   else mappend b <$> blockTillDoc

  isMainDoc :: LaTeX -> Bool
  isMainDoc b = case b of
                  TeXEnv "document" _ _ -> True
                  _                     -> False

  ------------------------------------------------------------------------
  -- Block
  -- Note: text stops on ']';
  --       if the other parser fail on the rest
  --          text2 handles it, starting with ']' 
  ------------------------------------------------------------------------
  block :: Parser LaTeX
  block = choice [text, dolMath, comment, environment, command, text2]
    
  ------------------------------------------------------------------------
  -- Text
  ------------------------------------------------------------------------
  text :: Parser LaTeX
  text = do
    mbC <- peekChar
    case mbC of
      Nothing -> return TeXEmpty
      Just c | c `elem` "$%\\{]}" -> fail "not text"
             | otherwise          -> TeXRaw <$> A.takeTill (`elem` "$%\\{]}")

  ------------------------------------------------------------------------
  -- Text without stopping on ']'
  ------------------------------------------------------------------------
  text2 :: Parser LaTeX
  text2 = do
    _ <- char ']'
    t <- try (text <|> return (TeXRaw T.empty))
    return $ TeXRaw endlessSq <> t

  ------------------------------------------------------------------------
  -- Environment
  ------------------------------------------------------------------------
  environment :: Parser LaTeX
  environment = choice [anonym, env]

  anonym :: Parser LaTeX
  anonym = char oBr >> 
      TeXBraces . mconcat <$> block `manyTill` char eBr

  env :: Parser LaTeX
  env = do
    n  <- envName begin
    if n `elem` [mathEnv, displayEnv, eqEnv]
      then mathEnvironment n
      else do
        as <- cmdArgs
        b  <- envBody n 
        return $ TeXEnv (T.unpack n) as b

  envName :: Text -> Parser Text
  envName k = do
    _ <- string k
    _ <- char oBr
    n <- A.takeTill (== eBr)
    _ <- char eBr
    return n

  envBody :: Text -> Parser LaTeX
  envBody n = mconcat <$> block `manyTill` endenv
    where endenv = try $ string (end `T.snoc` oBr <> n 
                                     `T.snoc` eBr)

  ------------------------------------------------------------------------
  -- Command
  ------------------------------------------------------------------------
  command :: Parser LaTeX
  command = do
    _    <- char bsl
    mbX  <- peekChar
    case mbX of
      Nothing -> return TeXEmpty
      Just x  -> if isSpecial x
                   then special
                   else do
                     c  <- A.takeTill endCmd
                     as <- cmdArgs
                     if null as
                        then return $ TeXCommS (T.unpack c)
                        else return $ TeXComm  (T.unpack c) as

  ------------------------------------------------------------------------
  -- Command Arguments
  ------------------------------------------------------------------------
  cmdArgs :: Parser [TeXArg]
  cmdArgs = try (whitespace >> string emptyArg >> return [FixArg TeXEmpty])
              <|> many1 cmdArg 
              <|> return []

  cmdArg :: Parser TeXArg
  cmdArg = do
    whitespace
    c <- char '[' <|> char '{'
    let e = case c of
              '[' -> endlessSq
              '{' -> endlessBr
              _   -> error "this cannot happen!"
    b <- mconcat <$> block `manyTill` string e
    case c of  
      '[' -> return $ OptArg b
      '{' -> return $ FixArg b
      _   -> error "this cannot happen!"

  whitespace :: Parser ()
  whitespace = try (do _ <- char ' '
                       whitespace)
                   <|> return ()

  ------------------------------------------------------------------------
  -- Special commands (consisting of one char)
  ------------------------------------------------------------------------
  special :: Parser LaTeX
  special = do
    x <- anyChar
    case x of
      '('  -> math Parentheses endPa
      '['  -> math Square      endSq
      '{'  -> lbrace
      '}'  -> rbrace
      '|'  -> vert
      '\\' -> lbreak
      _    -> commS [x]

  ------------------------------------------------------------------------
  -- line break
  ------------------------------------------------------------------------
  lbreak :: Parser LaTeX
  lbreak = do
    y <- try (char oSq <|> char str <|> return ' ')  
    case y of
      '[' -> linebreak False
      '*' -> do z <- try (char oSq <|> return ' ')
                case z of
                 '[' -> linebreak True
                 _   -> return (TeXLineBreak Nothing True)
      _   -> return (TeXLineBreak Nothing False)

  linebreak :: Bool -> Parser LaTeX
  linebreak t = do m <- measure
                   _ <- char eSq
                   s <- try (char str <|> return ' ')
                   return $ TeXLineBreak (Just m) (t || s == str)

  measure :: Parser Measure
  measure = try  (double2Float  <$> double >>= unit)
              <|> CustomMeasure <$> block

  unit :: Float -> Parser Measure
  unit f = do
    u1 <- anyChar
    u2 <- anyChar
    case map toLower [u1, u2] of
      "pt" -> return $ Pt (truncate f)
      "mm" -> return $ Mm f
      "cm" -> return $ Cm f
      "in" -> return $ In f
      "ex" -> return $ Ex f
      "em" -> return $ Em f
      _    -> fail "NaN"

  ------------------------------------------------------------------------
  -- right or left brace or vertical
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
    _ <- char dol 
    b <- mconcat <$> block `manyTill` char dol
    return $ TeXMathX Dollar b -- []

  math :: MathType -> Text -> Parser LaTeX
  math t eMath = do
     b <- mconcat <$> block `manyTill` try (string eMath)
     return $ TeXMathX t b -- []

  mathEnvironment :: Text -> Parser LaTeX
  mathEnvironment e = 
    let eMath = end `T.snoc` '{' <> e `T.snoc` '}'
        mType = name2MathType e
     in do b <- mconcat <$> block `manyTill` try (string eMath)
           return $ TeXMathX mType b -- []

  ------------------------------------------------------------------------
  -- Comment 
  ------------------------------------------------------------------------
  comment :: Parser LaTeX
  comment = do
    _  <- char per
    c  <- A.takeTill (== '\n')
    e  <- atEnd
    unless e (char '\n' >>= \_ -> return ())
    return $ TeXComment c

  ------------------------------------------------------------------------
  -- Helpers
  ------------------------------------------------------------------------
  isSpecial :: Char -> Bool
  isSpecial = (`elem` specials) -- [bsl, oSq, oPa, oBr, eBr]

  name2MathType :: Text -> MathType
  name2MathType n = 
    case T.unpack n of
      "math"        -> MathEnv
      "displaymath" -> DispEnv
      "equation"    -> EqEnv
      "\\("         -> Parentheses
      "\\["         -> Square
      _             -> Dollar

  begin, end :: Text
  begin     = T.pack "\\begin"
  end       = T.pack "\\end"

  mathEnv, displayEnv, eqEnv :: Text
  mathEnv    = T.pack "math"
  displayEnv = T.pack "displaymath"
  eqEnv      = T.pack "equation"

  endCmd :: Char -> Bool
  endCmd = flip elem symbols

  nul, eol, spc :: Char
  nul  = '\0'
  eol  = '\n' 
  spc  = ' '

  oBr, eBr, oSq, eSq, oPa, ePa, bsl, dol, per, str :: Char
  oBr  = '{'
  eBr  = '}'
  oSq  = '['
  eSq  = ']'
  oPa  = '('
  ePa  = ')'
  bsl  = '\\'
  dol  = '$'
  per  = '%'
  str  = '*'

  endPa, endSq, endlessBr, endlessSq :: Text
  endPa     = T.pack "\\)"
  endSq     = T.pack "\\]"
  endlessBr = T.pack "}"
  endlessSq = T.pack "]"

  emptyArg :: Text
  emptyArg = T.pack "{}"

  symbols :: String
  symbols = [nul, eol, spc, oBr, eBr, eSq, oSq, oPa, ePa, bsl, dol, per]

  specials :: String
  specials = "'(),.-\"!^$&#{}%~|/:;=[]\\` "
