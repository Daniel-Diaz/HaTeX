{-# LANGUAGE FlexibleInstances, CPP, DeriveDataTypeable, DeriveGeneric, DeriveFunctor #-}

-- | LaTeX syntax description in the definition of the 'LaTeX' datatype.
--   If you want to add new commands or environments not defined in
--   the library, import this module and use 'LaTeX' data constructors.
module Text.LaTeX.Base.Syntax.WithParm
 ( -- * @LaTeX@ datatype
   Measure (..)
 , MathType (..)
 , LaTeX (..)
 , TeXArg (..)
 , (<>), between
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
import qualified Data.Semigroup as Semigroup
import Data.String
import Control.Applicative
import Control.Monad (replicateM)
import Data.Functor.Identity (runIdentity)
import Data.Data (Data)
import Data.Typeable
import Test.QuickCheck
import Data.Hashable
import GHC.Generics (Generic)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

-- | Measure units defined in LaTeX. Use 'CustomMeasure' to use commands like 'textwidth'.
--   For instance:
--
-- > rule Nothing (CustomMeasure linewidth) (Pt 2)
--
-- This will create a black box (see 'rule') as wide as the text and two points tall.
--
data Measure a =
   Pt Double -- ^ A point is 1/72.27 inch, that means about 0.0138 inch or 0.3515 mm.
 | Mm Double -- ^ Millimeter.
 | Cm Double -- ^ Centimeter.
 | In Double -- ^ Inch.
 | Ex Double -- ^ The height of an \"x\" in the current font.
 | Em Double -- ^ The width of an \"M\" in the current font.
 | CustomMeasure (LaTeX a) -- ^ You can introduce a 'LaTeX' expression as a measure.
   deriving (Data, Eq, Generic, Show, Typeable, Functor)

-- | Different types of syntax for mathematical expressions.
data MathType = Parentheses | Square | Dollar | DoubleDollar | LHSInline
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Type of @LaTeX@ blocks. The @a@ type variable can be used for
-- a number of things but mainly stores source location.
data LaTeX a =
   TeXRaw a Text -- ^ Raw text.
 | TeXComm a String [TeXArg a] -- ^ Constructor for commands.
                               -- First argument is the name of the command.
                               -- Second, its arguments.
 | TeXCommS a String -- ^ Constructor for commands with no arguments.
                     --   When rendering, no space or @{}@ will be added at
                     --   the end.
 | TeXEnv a a String [TeXArg a] (LaTeX a) -- ^ Constructor for environments.
                                          -- First argument is the name of the environment.
                                          -- Second, its arguments.
                                          -- Third, its content.
 | TeXMath a MathType (LaTeX a) -- ^ Mathematical expressions.
 | TeXLineBreak a (Maybe (Measure a)) Bool -- ^ Line break command.
 | TeXBraces a (LaTeX a) -- ^ A expression between braces.
 | TeXComment a Text -- ^ Comments.
 | TeXSeq (LaTeX a) (LaTeX a) -- ^ Sequencing of 'LaTeX' expressions.
                              -- Use '<>' preferably.
 | TeXEmpty -- ^ An empty block.
            -- /Neutral element/ of '<>'.
   deriving (Data, Eq, Generic, Show, Typeable, Functor)

-- | An argument for a 'LaTeX' command or environment.
data TeXArg a =
   FixArg a (LaTeX a)  -- ^ Fixed argument.
 | OptArg a (LaTeX a)  -- ^ Optional argument.
 | MOptArg a [LaTeX a] -- ^ Multiple optional argument.
 | SymArg a (LaTeX a)  -- ^ An argument enclosed between @\<@ and @\>@.
 | MSymArg a [LaTeX a] -- ^ Version of 'SymArg' with multiple options.
 | ParArg a (LaTeX a)  -- ^ An argument enclosed between @(@ and @)@.
 | MParArg a [LaTeX a] -- ^ Version of 'ParArg' with multiple options.
   deriving (Data, Eq, Generic, Show, Typeable, Functor)

-- Monoid instance for 'LaTeX'.

-- | Method 'mappend' is strict in both arguments (except in the case when the first argument is 'TeXEmpty').
instance Monoid (LaTeX a) where
 mempty = TeXEmpty
 mappend TeXEmpty x = x
 mappend x TeXEmpty = x
 -- This equation is to make 'mappend' associative.
 mappend (TeXSeq x y) z = TeXSeq x $ mappend y z
 --
 mappend x y = TeXSeq x y

instance Semigroup.Semigroup (LaTeX a) where
  (<>) = mappend

-- | Calling 'between' @c l1 l2@ puts @c@ between @l1@ and @l2@ and
--   appends them.
--
-- > between c l1 l2 = l1 <> c <> l2
between :: Monoid m => m -> m -> m -> m
between c l1 l2 = l1 <> c <> l2

-- | Method 'fromString' escapes LaTeX reserved characters using 'protectString'.
instance IsString (LaTeX ()) where
 fromString = TeXRaw () . fromString . protectString

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
lookForCommand :: String  -- ^ Name of the command.
               -> LaTeX a -- ^ LaTeX syntax tree.
               -> [[TeXArg a]] -- ^ List of arguments passed to the command.
lookForCommand = (fmap snd .) . matchCommand . (==)

-- | Traverse a 'LaTeX' syntax tree and returns the commands (see 'TeXComm' and
--   'TeXCommS') that matches the condition and their arguments in each call.
matchCommand :: (String -> Bool) -> LaTeX a -> [(String,[TeXArg a])]
matchCommand f (TeXComm _ str as) =
  let xs = concatMap (matchCommandArg f) as
  in  if f str then (str,as) : xs else xs
matchCommand f (TeXCommS _ str) = [(str, []) | f str]
matchCommand f (TeXEnv _ _ _ as l) =
  let xs = concatMap (matchCommandArg f) as
  in  xs ++ matchCommand f l
matchCommand f (TeXMath _ _ l) = matchCommand f l
matchCommand f (TeXBraces _ l) = matchCommand f l
matchCommand f (TeXSeq l1 l2) = matchCommand f l1 ++ matchCommand f l2
matchCommand _ _ = []

matchCommandArg :: (String -> Bool) -> TeXArg a -> [(String,[TeXArg a])]
matchCommandArg f (OptArg  _ l ) = matchCommand f l
matchCommandArg f (FixArg  _ l ) = matchCommand f l
matchCommandArg f (MOptArg _ ls) = concatMap (matchCommand f) ls
matchCommandArg f (SymArg  _ l ) = matchCommand f l
matchCommandArg f (MSymArg _ ls) = concatMap (matchCommand f) ls
matchCommandArg f (ParArg  _ l ) = matchCommand f l
matchCommandArg f (MParArg _ ls) = concatMap (matchCommand f) ls

-- | Similar to 'lookForCommand', but applied to environments.
--   It returns a list with arguments passed and content of the
--   environment in each call.
--
-- > lookForEnv = (fmap (\(_,as,l) -> (as,l)) .) . matchEnv . (==)
--
lookForEnv :: String -> LaTeX a -> [([TeXArg a],LaTeX a)]
lookForEnv = (fmap (\(_,as,l) -> (as,l)) .) . matchEnv . (==)

-- | Traverse a 'LaTeX' syntax tree and returns the environments (see
--   'TeXEnv') that matches the condition, their arguments and their content
--   in each call.
matchEnv :: (String -> Bool) -> LaTeX a -> [(String,[TeXArg a],LaTeX a)]
matchEnv f (TeXComm _ _ as) = concatMap (matchEnvArg f) as
matchEnv f (TeXEnv _ _ str as l) =
  let xs = concatMap (matchEnvArg f) as
      ys = matchEnv f l
      zs = xs ++ ys
  in  if f str then (str,as,l) : zs else zs
matchEnv f (TeXMath _ _ l) = matchEnv f l
matchEnv f (TeXBraces _ l) = matchEnv f l
matchEnv f (TeXSeq l1 l2) = matchEnv f l1 ++ matchEnv f l2
matchEnv _ _ = []

matchEnvArg :: (String -> Bool) -> TeXArg a -> [(String,[TeXArg a],LaTeX a)]
matchEnvArg f (OptArg  _ l ) = matchEnv f l
matchEnvArg f (FixArg  _ l ) = matchEnv f l
matchEnvArg f (MOptArg _ ls) = concatMap (matchEnv f) ls
matchEnvArg f (SymArg  _ l ) = matchEnv f l
matchEnvArg f (MSymArg _ ls) = concatMap (matchEnv f) ls
matchEnvArg f (ParArg  _ l ) = matchEnv f l
matchEnvArg f (MParArg _ ls) = concatMap (matchEnv f) ls

-- | The function 'texmap' looks for subexpressions that match a given
--   condition and applies a function to them.
--
-- > texmap c f = runIdentity . texmapM c (pure . f)
texmap :: (LaTeX a -> Bool)    -- ^ Condition.
       -> (LaTeX a -> LaTeX a) -- ^ Function to apply when the condition matches.
       ->  LaTeX a -> LaTeX a
texmap c f = runIdentity . texmapM c (pure . f)

-- | Version of 'texmap' where the function returns values in a 'Monad'.
texmapM :: (Applicative m, Monad m)
        => (LaTeX a -> Bool) -- ^ Condition.
        -> (LaTeX a -> m (LaTeX a)) -- ^ Function to apply when the condition matches.
        ->  LaTeX a -> m (LaTeX a)
texmapM c f = go
  where
   go l@(TeXComm a str as)  = if c l then f l else TeXComm a str <$> mapM go' as
   go l@(TeXEnv a a' str as b) = if c l then f l else TeXEnv a a' str <$> mapM go' as <*> go b
   go l@(TeXMath a t b)     = if c l then f l else TeXMath a t <$> go b
   go l@(TeXBraces a b)     = if c l then f l else TeXBraces a <$> go b
   go l@(TeXSeq l1 l2)    = if c l then f l else liftA2 TeXSeq (go l1) (go l2)
   go l = if c l then f l else pure l
   --
   go' (FixArg  a l ) = FixArg  a <$> go l
   go' (OptArg  a l ) = OptArg  a <$> go l
   go' (MOptArg a ls) = MOptArg a <$> mapM go ls
   go' (SymArg  a l ) = SymArg  a <$> go l
   go' (MSymArg a ls) = MSymArg a <$> mapM go ls
   go' (ParArg  a l ) = ParArg  a <$> go l
   go' (MParArg a ls) = MParArg a <$> mapM go ls

-- | Extract the content of the 'document' environment, if present.
getBody :: LaTeX a -> Maybe (LaTeX a)
getBody l =
  case lookForEnv "document" l of
    ((_,b):_) -> Just b
    _ -> Nothing

-- | Extract the preamble of a 'LaTeX' document (everything before the 'document'
--   environment). It could be empty.
getPreamble :: LaTeX a -> LaTeX a
getPreamble (TeXEnv _ _ "document" _ _) = mempty
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

instance Arbitrary (Measure a) where
  arbitrary = do
     n <- choose (0,5)
     let f = [Pt,Mm,Cm,In,Ex,Em] !! n
     f <$> arbitrary

instance Arbitrary a => Arbitrary (LaTeX a) where
  arbitrary = arbitraryLaTeX False

arbitraryLaTeX :: Arbitrary a => Bool -> Gen (LaTeX a)
arbitraryLaTeX inDollar = do
  -- We give more chances to 'TeXRaw'.
  -- This results in arbitrary 'LaTeX' values
  -- not getting too large.
  n <- choose (0,16 :: Int)
  case n of
    0 -> if inDollar then arbitraryLaTeX True else pure TeXEmpty
    1 -> do m <- choose (0,5)
            TeXComm <$> arbitrary <*> arbitraryName <*> vectorOf m arbitrary
    2 -> TeXCommS <$> arbitrary <*> arbitraryName
    3 -> do m <- choose (0,5)
            TeXEnv <$> arbitrary <*> arbitrary <*> arbitraryName <*> vectorOf m arbitrary <*> arbitrary
    4 -> if inDollar
            then arbitraryLaTeX True
            else do m <- choose (0,3)
                    let t = [Parentheses,Square,Dollar,DoubleDollar] !! m
                    TeXMath <$> arbitrary <*> pure t <*> arbitraryLaTeX (t == Dollar || t == DoubleDollar)
    5 -> TeXLineBreak <$> arbitrary <*> arbitrary <*> arbitrary
    6 -> TeXBraces <$> arbitrary <*> arbitrary
    7 -> TeXComment <$> arbitrary <*> arbitraryRaw
    8 -> TeXSeq <$> (if inDollar then arbitraryLaTeX True else arbitrary) <*> arbitrary
    _ -> TeXRaw <$> arbitrary <*> arbitraryRaw

instance Arbitrary a => Arbitrary (TeXArg a) where
  arbitrary = do
     n <- choose (0,6 :: Int)
     case n of
       0 -> OptArg <$> arbitrary <*> arbitrary
       1 -> do m <- choose (1,5)
               MOptArg <$> arbitrary <*> vectorOf m arbitrary
       2 -> SymArg <$> arbitrary <*> arbitrary
       3 -> do m <- choose (1,5)
               MSymArg <$> arbitrary <*> vectorOf m arbitrary
       4 -> ParArg <$> arbitrary <*> arbitrary
       5 -> do m <- choose (1,5)
               MParArg <$> arbitrary <*> vectorOf m arbitrary
       _ -> FixArg <$> arbitrary <*> arbitrary


instance Hashable a => Hashable (Measure a)
instance Hashable MathType
instance Hashable a => Hashable (TeXArg a)
instance Hashable a => Hashable (LaTeX a)
