
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
   ) where

import Data.Text (Text,concatMap)
import Data.Monoid
import Data.String

data Measure =
   Pt Int   -- ^ A point is 1/72.27 inch, that means about 0.0138 inch or 0.3515 mm.
 | Mm Float -- ^ Millimeter.
 | Cm Float -- ^ Centimeter.
 | In Float -- ^ Inch.
 | Ex Float -- ^ The height of an \"x\" in the current font.
 | Em Float -- ^ The width of an \"M\" in the current font.
 | CustomMeasure LaTeX -- ^ You can introduce a 'LaTeX' expression as a measure.
   deriving (Eq, Show)

data MathType = MathEnv | DispEnv | EqEnv | Parentheses | Square | Dollar
  deriving (Eq,Show,Ord)

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
 | TeXMath LaTeX -- ^ Mathematical expressions.
 | TeXMathX MathType LaTeX -- ^ Mathematical expressions (experimental).
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
   OptArg LaTeX -- ^ Optional argument.
 | FixArg LaTeX -- ^ Fixed argument.
 | MOptArg [LaTeX] -- ^ Multiple optional argument.
 | SymArg LaTeX -- ^ An argument enclosed between @\<@ and @\>@.
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