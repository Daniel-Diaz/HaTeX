
{-# LANGUAGE CPP #-}

-- | LaTeX syntax description in the definition of the 'LaTeX' datatype.
--   If you want to add new commands or environments not defined in
--   the library, import this module and use 'LaTeX' data constructors.
module Text.LaTeX.Base.Syntax
 ( -- * @LaTeX@ datatype
   Measure (..)
 , MathType (..)
 , LaTeX (..)
 , TeXArg (..)
 , (<>)
   -- * Escaping reserved characters
 , protectString
 , protectText
   -- * Syntax analysis
 , lookForCommand
 , lookForEnv
 , getBody
   ) where

import Data.Text (Text)
import qualified Data.Text
import Data.Monoid
import Data.String

-- | Measure units defined in LaTeX. Use 'CustomMeasure' to use commands like 'textwidth'.
--   For instance:
--
-- > rule Nothing (CustomMeasure linewidth) (Pt 2)
--
-- This will create a black box (see 'rule') as wide as the text and two points tall.
--
data Measure =
   Pt Double -- ^ A point is 1/72.27 inch, that means about 0.0138 inch or 0.3515 mm.
 | Mm Double -- ^ Millimeter.
 | Cm Double -- ^ Centimeter.
 | In Double -- ^ Inch.
 | Ex Double -- ^ The height of an \"x\" in the current font.
 | Em Double -- ^ The width of an \"M\" in the current font.
 | CustomMeasure LaTeX -- ^ You can introduce a 'LaTeX' expression as a measure.
   deriving (Eq, Show)

-- | Different types of syntax for mathematical expressions.
data MathType = Parentheses | Square | Dollar
  deriving (Eq,Show)

-- | Type of @LaTeX@ blocks.
data LaTeX =
   TeXRaw Text -- ^ Raw text.
 | TeXComm String [TeXArg] -- ^ Constructor for commands.
                           -- First argument is the name of the command.
                           -- Second, its arguments.
 | TeXCommS String -- ^ Constructor for commands with no arguments.
 | TeXEnv String [TeXArg] LaTeX -- ^ Constructor for environments.
                                -- First argument is the name of the environment.
                                -- Second, its arguments.
                                -- Third, its content.
 | TeXMath MathType LaTeX -- ^ Mathematical expressions.
 | TeXLineBreak (Maybe Measure) Bool -- ^ Line break command.
 | TeXOp String LaTeX LaTeX -- ^ Operators.
 | TeXBraces LaTeX -- ^ A expression between braces.
 | TeXComment Text -- ^ Comments.
 | TeXSeq LaTeX LaTeX -- ^ Sequencing of 'LaTeX' expressions.
                      -- Use '<>' preferably.
 | TeXEmpty -- ^ An empty block.
            -- /Neutral element/ of '<>'.
   deriving (Eq,Show)

-- | An argument for a 'LaTeX' command or environment.
data TeXArg =
   FixArg LaTeX    -- ^ Fixed argument.
 | OptArg LaTeX    -- ^ Optional argument.
 | MOptArg [LaTeX] -- ^ Multiple optional argument.
 | SymArg LaTeX    -- ^ An argument enclosed between @\<@ and @\>@.
 | MSymArg [LaTeX] -- ^ Version of 'SymArg' with multiple options.
   deriving (Eq,Show)

-- Monoid instance for 'LaTeX'.

-- | Method 'mappend' is strict in both arguments (except in the case when the first argument is 'TeXEmpty').
instance Monoid LaTeX where
 mempty = TeXEmpty
 mappend TeXEmpty x = x
 mappend x TeXEmpty = x
 mappend x y = TeXSeq x y

-- Since GHC starting from 7.4 provides (<>) as synonym to 'mappend' (see "Data.Monoid"),
-- we avoid an overlapping definition with a CPP conditional.
#if __GLASGOW_HASKELL__ < 704
-- | Alias for 'mappend'.
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

-- | Method 'fromString' escapes LaTeX reserved characters using 'protectString'.
instance IsString LaTeX where
 fromString = TeXRaw . fromString . protectString

-- | Escape LaTeX reserved characters in a 'String'.
protectString :: String -> String
protectString = mconcat . fmap protectChar

-- | Escape LaTeX reserved characters in a 'Text'.
protectText :: Text -> Text
protectText = Data.Text.concatMap (fromString . protectChar)

protectChar :: Char -> String
protectChar '#'  = "\\#"
protectChar '$'  = "\\$"
protectChar '%'  = "\\%"
protectChar '^'  = "\\^{}"
protectChar '&'  = "\\&"
protectChar '{'  = "\\{"
protectChar '}'  = "\\}"
protectChar '~'  = "\\~{}"
protectChar '\\' = "\\textbackslash{}"
protectChar '_'  = "\\_{}"
protectChar x = [x]

-- Syntax analysis

-- | Look into a 'LaTeX' syntax tree to find any call to the command with
--   the given name. It returns a list of arguments with which this command
--   is called.
--
--   If the returned list is empty, the command was not found. However,
--   if the list contains empty lists, those are callings to the command
--   with no arguments.
--
--   For example
--
-- > lookForCommand "author" l
--
--   would look for the argument passed to the @\\author@ command in @l@.
lookForCommand :: String -- ^ Name of the command.
               -> LaTeX  -- ^ LaTeX syntax tree.
               -> [[TeXArg]] -- ^ List of arguments passed to the command.
lookForCommand str (TeXComm n as) =
  let xs = concatMap (lookForCommandArg str) as
  in  if n == str then as : xs else xs
lookForCommand str (TeXCommS n) = if n == str then [[]] else []
lookForCommand str (TeXEnv _ as l) =
  let xs = concatMap (lookForCommandArg str) as
  in  xs ++ lookForCommand str l
lookForCommand str (TeXMath _ l) = lookForCommand str l
lookForCommand str (TeXOp _ l1 l2) = lookForCommand str l1 ++ lookForCommand str l2
lookForCommand str (TeXBraces l) = lookForCommand str l
lookForCommand str (TeXSeq l1 l2) = lookForCommand str l1 ++ lookForCommand str l2
lookForCommand _ _ = []

lookForCommandArg :: String -> TeXArg -> [[TeXArg]]
lookForCommandArg str (OptArg l) = lookForCommand str l
lookForCommandArg str (FixArg l) = lookForCommand str l
lookForCommandArg str (MOptArg ls) = concatMap (lookForCommand str) ls
lookForCommandArg str (SymArg l) = lookForCommand str l
lookForCommandArg str (MSymArg ls) = concatMap (lookForCommand str) ls

-- | Similar to 'lookForCommand', but applied to environments.
--   It returns a list with arguments passed and content of the
--   environment in each call.
lookForEnv :: String -> LaTeX -> [([TeXArg],LaTeX)]
lookForEnv str (TeXComm _ as) = concatMap (lookForEnvArg str) as
lookForEnv str (TeXEnv n as l) =
  let xs = concatMap (lookForEnvArg str) as
      ys = lookForEnv str l
      zs = xs ++ ys
  in  if n == str then (as,l) : zs else zs
lookForEnv str (TeXMath _ l) = lookForEnv str l
lookForEnv str (TeXOp _ l1 l2) = lookForEnv str l1 ++ lookForEnv str l2
lookForEnv str (TeXBraces l) = lookForEnv str l
lookForEnv str (TeXSeq l1 l2) = lookForEnv str l1 ++ lookForEnv str l2
lookForEnv _ _ = []

lookForEnvArg :: String -> TeXArg -> [([TeXArg],LaTeX)]
lookForEnvArg str (OptArg  l ) = lookForEnv str l
lookForEnvArg str (FixArg  l ) = lookForEnv str l
lookForEnvArg str (MOptArg ls) = concatMap (lookForEnv str) ls
lookForEnvArg str (SymArg  l ) = lookForEnv str l
lookForEnvArg str (MSymArg ls) = concatMap (lookForEnv str) ls

-- | Extract the content of the 'document' environment, if present.
getBody :: LaTeX -> Maybe LaTeX
getBody l =
  case lookForEnv "document" l of
    ((_,b):_) -> Just b
    _ -> Nothing
