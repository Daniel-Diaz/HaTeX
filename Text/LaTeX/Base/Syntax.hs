
{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric #-}

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
 , texmapM
   -- ** Utils
 , getBody
 , getPreamble
   ) where

import Data.Text (Text,pack)
import qualified Data.Text
import Data.Monoid
#if MIN_VERSION_base(4,9,0)
import qualified Data.Semigroup as Semigroup
#endif
import Data.String
import Control.Applicative
import Control.Monad (replicateM)
import Data.Functor.Identity (runIdentity)
import Data.Data (Data)
import Data.Typeable
import GHC.Generics (Generic)
import Test.QuickCheck
import Data.Hashable

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
   deriving (Data, Eq, Generic, Show, Typeable)

-- | Different types of syntax for mathematical expressions.
data MathType = Parentheses | Square | Dollar | DoubleDollar
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Type of @LaTeX@ blocks.
data LaTeX =
   TeXRaw Text -- ^ Raw text.
 | TeXComm String [TeXArg] -- ^ Constructor for commands.
                           -- First argument is the name of the command.
                           -- Second, its arguments.
 | TeXCommS String -- ^ Constructor for commands with no arguments.
                   --   When rendering, no space or @{}@ will be added at
                   --   the end.
 | TeXEnv String [TeXArg] LaTeX -- ^ Constructor for environments.
                                -- First argument is the name of the environment.
                                -- Second, its arguments.
                                -- Third, its content.
 | TeXMath MathType LaTeX -- ^ Mathematical expressions.
 | TeXLineBreak (Maybe Measure) Bool -- ^ Line break command.
 | TeXBraces LaTeX -- ^ A expression between braces.
 | TeXComment Text -- ^ Comments.
 | TeXSeq LaTeX LaTeX -- ^ Sequencing of 'LaTeX' expressions.
                      -- Use '<>' preferably.
 | TeXEmpty -- ^ An empty block.
            -- /Neutral element/ of '<>'.
   deriving (Data, Eq, Generic, Show, Typeable)

-- | An argument for a 'LaTeX' command or environment.
data TeXArg =
   FixArg LaTeX    -- ^ Fixed argument.
 | OptArg LaTeX    -- ^ Optional argument.
 | MOptArg [LaTeX] -- ^ Multiple optional argument.
 | SymArg LaTeX    -- ^ An argument enclosed between @\<@ and @\>@.
 | MSymArg [LaTeX] -- ^ Version of 'SymArg' with multiple options.
 | ParArg LaTeX    -- ^ An argument enclosed between @(@ and @)@.
 | MParArg [LaTeX] -- ^ Version of 'ParArg' with multiple options.
   deriving (Data, Eq, Generic, Show, Typeable)

-- Monoid instance for 'LaTeX'.

-- | Method 'mappend' is strict in both arguments (except in the case when the first argument is 'TeXEmpty').
instance Monoid LaTeX where
 mempty = TeXEmpty
 mappend TeXEmpty x = x
 mappend x TeXEmpty = x
 -- This equation is to make 'mappend' associative.
 mappend (TeXSeq x y) z = TeXSeq x $ mappend y z
 --
 mappend x y = TeXSeq x y

-- Since GHC starting from 7.4 provides (<>) as synonym to 'mappend' (see "Data.Monoid"),
-- we avoid an overlapping definition with a CPP conditional.
#if __GLASGOW_HASKELL__ < 704
-- | Alias for 'mappend'.
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

#if MIN_VERSION_base(4,9,0)
instance Semigroup.Semigroup LaTeX where
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
matchCommand f (TeXBraces l) = matchCommand f l
matchCommand f (TeXSeq l1 l2) = matchCommand f l1 ++ matchCommand f l2
matchCommand _ _ = []

matchCommandArg :: (String -> Bool) -> TeXArg -> [(String,[TeXArg])]
matchCommandArg f (OptArg  l ) = matchCommand f l
matchCommandArg f (FixArg  l ) = matchCommand f l
matchCommandArg f (MOptArg ls) = concatMap (matchCommand f) ls
matchCommandArg f (SymArg  l ) = matchCommand f l
matchCommandArg f (MSymArg ls) = concatMap (matchCommand f) ls
matchCommandArg f (ParArg  l ) = matchCommand f l
matchCommandArg f (MParArg ls) = concatMap (matchCommand f) ls

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
matchEnv f (TeXBraces l) = matchEnv f l
matchEnv f (TeXSeq l1 l2) = matchEnv f l1 ++ matchEnv f l2
matchEnv _ _ = []

matchEnvArg :: (String -> Bool) -> TeXArg -> [(String,[TeXArg],LaTeX)]
matchEnvArg f (OptArg  l ) = matchEnv f l
matchEnvArg f (FixArg  l ) = matchEnv f l
matchEnvArg f (MOptArg ls) = concatMap (matchEnv f) ls
matchEnvArg f (SymArg  l ) = matchEnv f l
matchEnvArg f (MSymArg ls) = concatMap (matchEnv f) ls
matchEnvArg f (ParArg  l ) = matchEnv f l
matchEnvArg f (MParArg ls) = concatMap (matchEnv f) ls

-- | The function 'texmap' looks for subexpressions that match a given
--   condition and applies a function to them.
--
-- > texmap c f = runIdentity . texmapM c (pure . f)
texmap :: (LaTeX -> Bool) -- ^ Condition.
       -> (LaTeX -> LaTeX) -- ^ Function to apply when the condition matches.
       ->  LaTeX -> LaTeX
texmap c f = runIdentity . texmapM c (pure . f)

-- | Version of 'texmap' where the function returns values in a 'Monad'.
texmapM :: (Applicative m, Monad m)
        => (LaTeX -> Bool) -- ^ Condition.
        -> (LaTeX -> m LaTeX) -- ^ Function to apply when the condition matches.
        ->  LaTeX -> m LaTeX
texmapM c f = go
  where
   go l@(TeXComm str as)  = if c l then f l else TeXComm str <$> mapM go' as
   go l@(TeXEnv str as b) = if c l then f l else TeXEnv str <$> mapM go' as <*> go b
   go l@(TeXMath t b)     = if c l then f l else TeXMath t <$> go b
   go l@(TeXBraces b)     = if c l then f l else TeXBraces <$> go b
   go l@(TeXSeq l1 l2)    = if c l then f l else liftA2 TeXSeq (go l1) (go l2)
   go l = if c l then f l else pure l
   --
   go' (FixArg  l ) = FixArg  <$> go l
   go' (OptArg  l ) = OptArg  <$> go l
   go' (MOptArg ls) = MOptArg <$> mapM go ls
   go' (SymArg  l ) = SymArg  <$> go l
   go' (MSymArg ls) = MSymArg <$> mapM go ls
   go' (ParArg  l ) = ParArg  <$> go l
   go' (MParArg ls) = MParArg <$> mapM go ls

-- | Extract the content of the 'document' environment, if present.
getBody :: LaTeX -> Maybe LaTeX
getBody l =
  case lookForEnv "document" l of
    ((_,b):_) -> Just b
    _ -> Nothing

-- | Extract the preamble of a 'LaTeX' document (everything before the 'document'
--   environment). It could be empty.
getPreamble :: LaTeX -> LaTeX
getPreamble (TeXEnv "document" _ _) = mempty
getPreamble (TeXSeq l1 l2) = getPreamble l1 <> getPreamble l2
getPreamble l = l

---------------------------------------
-- LaTeX Arbitrary instance

arbitraryChar :: Gen Char
arbitraryChar = elements $
     ['A'..'Z']
  ++ ['a'..'z']
  ++ "\n-+*/!\"().,:;'@<>? "

-- | Utility for the instance of 'LaTeX' to 'Arbitrary'.
--   We generate a short sequence of characters and
--   escape reserved characters with 'protectText'.
arbitraryRaw :: Gen Text
arbitraryRaw = do
  n <- choose (1,20)
  protectText . pack <$> replicateM n arbitraryChar

-- | Generator for names of command and environments.
--   We use only alphabetical characters.
arbitraryName :: Gen String
arbitraryName = do
  n <- choose (1,10)
  replicateM n $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z']

instance Arbitrary Measure where
  arbitrary = do
     n <- choose (0,5)
     let f = [Pt,Mm,Cm,In,Ex,Em] !! n
     f <$> arbitrary

instance Arbitrary LaTeX where
  arbitrary = arbitraryLaTeX False

arbitraryLaTeX :: Bool -> Gen LaTeX
arbitraryLaTeX inDollar = do
  -- We give more chances to 'TeXRaw'.
  -- This results in arbitrary 'LaTeX' values
  -- not getting too large.
  n <- choose (0,16 :: Int)
  case n of
    0 -> if inDollar then arbitraryLaTeX True else pure TeXEmpty
    1 -> do m <- choose (0,5)
            TeXComm <$> arbitraryName <*> vectorOf m arbitrary
    2 -> TeXCommS <$> arbitraryName
    3 -> do m <- choose (0,5)
            TeXEnv <$> arbitraryName <*> vectorOf m arbitrary <*> arbitrary
    4 -> if inDollar
            then arbitraryLaTeX True
            else do do m <- choose (0,3)
                       let t = [Parentheses,Square,Dollar,DoubleDollar] !! m
                       TeXMath <$> pure t <*> arbitraryLaTeX (t == Dollar || t == DoubleDollar)
    5 -> TeXLineBreak <$> arbitrary <*> arbitrary
    6 -> TeXBraces <$> arbitrary
    7 -> TeXComment <$> arbitraryRaw
    8 -> TeXSeq <$> (if inDollar then arbitraryLaTeX True else arbitrary) <*> arbitrary
    _ -> TeXRaw <$> arbitraryRaw

instance Arbitrary TeXArg where
  arbitrary = do
     n <- choose (0,6 :: Int)
     case n of
       0 -> OptArg <$> arbitrary
       1 -> do m <- choose (1,5)
               MOptArg <$> vectorOf m arbitrary
       2 -> SymArg <$> arbitrary
       3 -> do m <- choose (1,5)
               MSymArg <$> vectorOf m arbitrary
       4 -> ParArg <$> arbitrary
       5 -> do m <- choose (1,5)
               MParArg <$> vectorOf m arbitrary
       _ -> FixArg <$> arbitrary


instance Hashable Measure
instance Hashable MathType
instance Hashable TeXArg
instance Hashable LaTeX
