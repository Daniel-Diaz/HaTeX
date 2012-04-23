
{-# LANGUAGE CPP #-}

-- | LaTeX syntax description in the definition of the 'LaTeX' datatype.
--   If you want to add new commands or environments not defined in
--   the library, import this module and use 'LaTeX' data constructors.
module Text.LaTeX.Base.Syntax
 ( -- * @LaTeX@ datatype
   LaTeX (..)
 , TeXArg (..)
 , (<>)
   -- * Utils
 , braces
 , comm
   -- * Escaping reserved characters
 , protectString
 , protectText
   ) where

import Data.Text (Text,concatMap)
import Data.Monoid
import Data.String

-- | A 'LaTeX' object represents some expression written in LaTeX.
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
 | TeXNewLine Bool -- ^ Newline character.
 | TeXOp String LaTeX LaTeX -- ^ Operators.
 | TeXBraces LaTeX -- ^ A expression between braces.
 | TeXComment Text -- ^ Comments.
 | TeXSeq LaTeX LaTeX -- ^ Sequencing of 'LaTeX' expressions.
                      -- Use '<>' preferably.
 | TeXEmpty -- ^ An empty expression.
            -- Neutral element of '<>'.
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

---- UTILS

-- | Alias for 'TeXBraces'.
braces :: LaTeX -> LaTeX
braces = TeXBraces

-- | A simple (without arguments) command generator,
--   given the name of the command.
--
-- > comm str = TeXComm str []
--
comm :: String -> LaTeX
comm str = TeXComm str []