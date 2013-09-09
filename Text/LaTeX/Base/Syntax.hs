
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
 , matchCommand
 , lookForCommand
 , matchEnv
 , lookForEnv
 , texmap
 , texmapA
   -- ** Utils
 , getBody
   ) where

import Data.Text (Text)
import qualified Data.Text
import Data.Monoid
import Data.String
import Control.Applicative
import Data.Functor.Identity (runIdentity)

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
-- > lookForCommand = (fmap snd .) . matchCommand . (==)
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
lookForCommand = (fmap snd .) . matchCommand . (==)

-- | Traverse a 'LaTeX' syntax tree and returns the commands (see 'TeXComm' and
--   'TeXCommS') that matches the condition and their arguments in each call.
matchCommand :: (String -> Bool) -> LaTeX -> [(String,[TeXArg])]
matchCommand f (TeXComm str as) =
  let xs = concatMap (matchCommandArg f) as
  in  if f str then (str,as) : xs else xs
matchCommand f (TeXCommS str) = if f str then [(str,[])] else []
matchCommand f (TeXEnv _ as l) =
  let xs = concatMap (matchCommandArg f) as
  in  xs ++ matchCommand f l
matchCommand f (TeXMath _ l) = matchCommand f l
matchCommand f (TeXOp _ l1 l2) = matchCommand f l1 ++ matchCommand f l2
matchCommand f (TeXBraces l) = matchCommand f l
matchCommand f (TeXSeq l1 l2) = matchCommand f l1 ++ matchCommand f l2
matchCommand _ _ = []

matchCommandArg :: (String -> Bool) -> TeXArg -> [(String,[TeXArg])]
matchCommandArg f (OptArg  l ) = matchCommand f l
matchCommandArg f (FixArg  l ) = matchCommand f l
matchCommandArg f (MOptArg ls) = concatMap (matchCommand f) ls
matchCommandArg f (SymArg  l ) = matchCommand f l
matchCommandArg f (MSymArg ls) = concatMap (matchCommand f) ls

-- | Similar to 'lookForCommand', but applied to environments.
--   It returns a list with arguments passed and content of the
--   environment in each call.
--
-- > lookForEnv = (fmap (\(_,as,l) -> (as,l)) .) . matchEnv . (==)
--
lookForEnv :: String -> LaTeX -> [([TeXArg],LaTeX)]
lookForEnv = (fmap (\(_,as,l) -> (as,l)) .) . matchEnv . (==)

-- | Traverse a 'LaTeX' syntax tree and returns the environments (see
--   'TeXEnv') that matches the condition, their arguments and their content
--   in each call.
matchEnv :: (String -> Bool) -> LaTeX -> [(String,[TeXArg],LaTeX)]
matchEnv f (TeXComm _ as) = concatMap (matchEnvArg f) as
matchEnv f (TeXEnv str as l) =
  let xs = concatMap (matchEnvArg f) as
      ys = matchEnv f l
      zs = xs ++ ys
  in  if f str then (str,as,l) : zs else zs
matchEnv f (TeXMath _ l) = matchEnv f l
matchEnv f (TeXOp _ l1 l2) = matchEnv f l1 ++ matchEnv f l2
matchEnv f (TeXBraces l) = matchEnv f l
matchEnv f (TeXSeq l1 l2) = matchEnv f l1 ++ matchEnv f l2
matchEnv _ _ = []

matchEnvArg :: (String -> Bool) -> TeXArg -> [(String,[TeXArg],LaTeX)]
matchEnvArg f (OptArg  l ) = matchEnv f l
matchEnvArg f (FixArg  l ) = matchEnv f l
matchEnvArg f (MOptArg ls) = concatMap (matchEnv f) ls
matchEnvArg f (SymArg  l ) = matchEnv f l
matchEnvArg f (MSymArg ls) = concatMap (matchEnv f) ls

-- | The function 'texmap' looks for subexpressions that match a given
--   condition and applies a function to them.
--
-- > texmap c f = runIdentity . texmapA c (pure . f)
texmap :: (LaTeX -> Bool) -- ^ Condition.
       -> (LaTeX -> LaTeX) -- ^ Function to apply when the condition matches.
       ->  LaTeX -> LaTeX
texmap c f = runIdentity . texmapA c (pure . f)

-- | Version of 'texmap' where the function returns values in a 'Monad'.
texmapA :: (Applicative m, Monad m)
        => (LaTeX -> Bool) -- ^ Condition.
        -> (LaTeX -> m LaTeX) -- ^ Function to apply when the condition matches.
        ->  LaTeX -> m LaTeX
texmapA c f = go
  where
   go l@(TeXComm str as)  = if c l then f l else TeXComm str <$> mapM go' as
   go l@(TeXEnv str as b) = if c l then f l else TeXEnv str <$> (mapM go' as) <*> go b
   go l@(TeXMath t b)     = if c l then f l else TeXMath t <$> go b
   go l@(TeXOp str l1 l2) = if c l then f l else liftA2 (TeXOp str) (go l1) (go l2)
   go l@(TeXBraces b)     = if c l then f l else TeXBraces <$> go b
   go l@(TeXSeq l1 l2)    = if c l then f l else liftA2 TeXSeq (go l1) (go l2)
   go l = if c l then f l else pure l
   --
   go' (FixArg  l ) = FixArg  <$> go l
   go' (OptArg  l ) = OptArg  <$> go l
   go' (MOptArg ls) = MOptArg <$> mapM go ls
   go' (SymArg  l ) = SymArg  <$> go l
   go' (MSymArg ls) = MSymArg <$> mapM go ls

-- | Extract the content of the 'document' environment, if present.
getBody :: LaTeX -> Maybe LaTeX
getBody l =
  case lookForEnv "document" l of
    ((_,b):_) -> Just b
    _ -> Nothing
