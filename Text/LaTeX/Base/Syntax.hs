
{-# LANGUAGE CPP #-}

-- | LaTeX syntax description in the definition of the 'LaTeX' datatype.
--   If you want to add new commands or environments not defined in
--   the library, import this module and use 'LaTeX' data constructors.
module Text.LaTeX.Base.Syntax
 ( -- * @LaTeX@ datatype
   LaTeX (..)
 , TeXArg (..)
 , (<>)
   -- * Escaping reserved characters
 , protectString
 , protectText
   -- * Math-specific data
 , TeXMathKind(..)
 , TeXBracketKind(..)
 , showTeXBrackets
 , TeXBracketSize(..)
 , teXBracketBigness
   ) where

import Data.Text (Text,concatMap)
import Data.Monoid
import Data.String

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
 | TeXMath TeXMathKind LaTeX -- ^ Mathematical expressions.
 | TeXNewLine Bool -- ^ Newline character.
 | TeXOp String LaTeX LaTeX -- ^ Operators.
 | TeXBraces LaTeX -- ^ An expression between braces.
 | TeXMathBrackets TeXBracketKind (Maybe TeXBracketSize) LaTeX
                -- ^ A math expression surrounded by
                -- parens, square- angle- etc. brackets, or just virtual delimiters,
                -- in general suitably sized to fit the expression's rendered height;
                -- if no size is specified this will be automatically done by LaTeX.
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



-- | The various ways to present maths in LaTeX. (There are actually many more than these,
-- but they use the generic @\begin{ε}...\end{ε}@ syntax covered by 'TeXEnv'.)
data TeXMathKind = InlineTeXMath           -- ^ A simple inline math expression, in LaTeX surrounded by @$@s.
                 | DisplayTeXMath          -- ^ The standard displayed (i.e., in a seperate line) environment, @\[ ... \]@.
                 deriving (Eq,Show)


-- | In LaTeX math, many different delimiters can be used to surround subexpressions
-- with proper height-scaling. 'TeXBracketKind' has the most important ones as constructors,
-- the remaining ones can be expressed by e.g. @'TeXMathOtherBrackets' "\\lceil" "\\rceil"@.
data TeXBracketKind = TeXMathParens
                    | TeXMathSquareBrackets
                    | TeXMathBraces
                    | TeXMathAngleBrackets
                    | TeXMathOtherBrackets LaTeX LaTeX
                    deriving (Eq,Show)

showTeXBrackets :: TeXBracketKind -> (LaTeX,LaTeX)
showTeXBrackets TeXMathParens = ("(",")")
showTeXBrackets TeXMathSquareBrackets = ("[","]")
showTeXBrackets TeXMathBraces = ("{","}")
showTeXBrackets TeXMathAngleBrackets = (TeXCommS"langle",TeXCommS"rangle")
showTeXBrackets (TeXMathOtherBrackets l r) = (l,r)


-- | Each possible bracket type comes in various automatic or manually defined sizes.
data TeXBracketSize = TeXBracket_defSize     -- ^ No scaling, just use the normal e.g. paren character.
                    | TeXBracket_bigSize     -- ^ Barely larger than defSize.
                    | TeXBracket_BigSize     -- ^ Between big and bigg.
                    | TeXBracket_biggSize    -- ^ Between Big and Bigg.
                    | TeXBracket_BiggSize    -- ^ Largest fixed size available by standard.
                    deriving (Eq,Ord,Enum,Show)
                    
teXBracketBigness :: TeXBracketSize -> Text
teXBracketBigness TeXBracket_defSize = undefined
teXBracketBigness TeXBracket_bigSize = "big"
teXBracketBigness TeXBracket_BigSize = "Big"
teXBracketBigness TeXBracket_biggSize = "bigg"
teXBracketBigness TeXBracket_BiggSize = "Bigg"