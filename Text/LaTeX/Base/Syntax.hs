{-# LANGUAGE FlexibleInstances, CPP, DeriveDataTypeable, DeriveGeneric
           , DeriveFunctor , PatternSynonyms #-}

-- | LaTeX syntax description in the definition of the 'LaTeX' datatype.
--   If you want to add new commands or environments not defined in
--   the library, import this module and use 'LaTeX' data constructors.
module Text.LaTeX.Base.Syntax.WithParm
 ( -- * @LaTeX@ datatype
   MeasureL (..) , Measure
 , MathType (..)
 , LaTeXL (..)
 , LaTeX , pattern TeXRaw , pattern TeXComm , pattern TeXCommS
 , pattern TeXEnv , pattern TeXMath
 , TeXArgL (..) , TeXArg
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
data MeasureL a =
   Pt Double -- ^ A point is 1/72.27 inch, that means about 0.0138 inch or 0.3515 mm.
 | Mm Double -- ^ Millimeter.
 | Cm Double -- ^ Centimeter.
 | In Double -- ^ Inch.
 | Ex Double -- ^ The height of an \"x\" in the current font.
 | Em Double -- ^ The width of an \"M\" in the current font.
 | CustomMeasure (LaTeXL a) -- ^ You can introduce a 'LaTeX' expression as a measure.
   deriving (Data, Eq, Generic, Show, Typeable, Functor)

-- | Different types of syntax for mathematical expressions.
data MathType = Parentheses | Square | Dollar | DoubleDollar | LHSInline
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Type of @LaTeX@ blocks with additional information of type @a@ annotated
-- through the tree. This is used, for example, to track source location
-- on the parser. If you wish to use the AST with no annotations, see 'LaTeX'.
data LaTeXL a =
   TeXRawL a Text -- ^ Raw text, first argument is the location in the source file.
 | TeXCommL a String [TeXArgL a] -- ^ Constructor for commands.
                                 -- First is the location in the source
                                 -- Second argument is the name of the command.
                                 -- Third, its arguments.
 | TeXCommSL a String -- ^ Constructor for commands with no arguments.
                      --   When rendering, no space or @{}@ will be added at
                      --   the end.
 | TeXEnvL a a String [TeXArgL a] (LaTeXL a) -- ^ Constructor for environments.
                                             -- First two arguments are the locations of
                                             -- its \begin and \end; then the name of the environment.
                                             -- Fourth, its arguments.
                                             -- Fifth, its content.
 | TeXMathL a MathType (LaTeXL a) -- ^ Mathematical expressions.
 | TeXLineBreak (Maybe (MeasureL a)) Bool -- ^ Line break command.
 | TeXBraces (LaTeXL a) -- ^ A expression between braces.
 | TeXComment Text -- ^ Comments.
 | TeXSeq (LaTeXL a) (LaTeXL a) -- ^ Sequencing of 'LaTeXL' expressions.
                                -- Use '<>' preferably.
 | TeXEmpty -- ^ An empty block.
            -- /Neutral element/ of '<>'.
   deriving (Data, Eq, Generic, Show, Typeable, Functor)

-- | Type of @LaTeX@ blocks without source locations.
type LaTeX  = LaTeXL ()

-- | Type of @LaTeX@ arguments without source locations.
type TeXArg = TeXArgL ()

-- | Type of @LaTeX@ measures without source locations.
type Measure = MeasureL ()


{-# COMPLETE TeXRaw, TeXComm , TeXCommS , TeXEnv , TeXMath , TeXLineBreak , TeXBraces , TeXSeq , TeXEmpty #-}

-- | Same as 'TeXRawL' but defaults @a@ to @()@
pattern TeXRaw :: Text -> LaTeX
pattern TeXRaw t = TeXRawL () t

-- | Same as 'TeXCommL' but defaults @a@ to @()@
pattern TeXComm :: String -> [TeXArg] -> LaTeX
pattern TeXComm s l = TeXCommL () s l

-- | Same as 'TeXCommSL' but defaults @a@ to @()@
pattern TeXCommS :: String -> LaTeX
pattern TeXCommS s = TeXCommSL () s

-- | Same as 'TeXEnv' but defaults @a@ to @()@
pattern TeXEnv :: String -> [TeXArg] -> LaTeX -> LaTeX
pattern TeXEnv s l e = TeXEnvL () () s l e

-- | Same as 'TeXMath' but defaults @a@ to @()@
pattern TeXMath :: MathType -> LaTeX -> LaTeX
pattern TeXMath m e = TeXMathL () m e

-- | An argument for a 'LaTeXL' command or environment that can carry additional
-- information through the parameter @a@. See 'TeXArg' if you wish to ignore the
-- parameter @a@ entirely.
data TeXArgL a =
   FixArg (LaTeXL a)  -- ^ Fixed argument.
 | OptArg (LaTeXL a)  -- ^ Optional argument.
 | MOptArg [LaTeXL a] -- ^ Multiple optional argument.
 | SymArg (LaTeXL a)  -- ^ An argument enclosed between @\<@ and @\>@.
 | MSymArg [LaTeXL a] -- ^ Version of 'SymArg' with multiple options.
 | ParArg (LaTeXL a)  -- ^ An argument enclosed between @(@ and @)@.
 | MParArg [LaTeXL a] -- ^ Version of 'ParArg' with multiple options.
   deriving (Data, Eq, Generic, Show, Typeable, Functor)

-- Monoid instance for 'LaTeXL'.

-- | Method 'mappend' is strict in both arguments (except in the case when the first argument is 'TeXEmpty').
instance Monoid (LaTeXL a) where
 mempty = TeXEmpty
 mappend TeXEmpty x = x
 mappend x TeXEmpty = x
 -- This equation is to make 'mappend' associative.
 mappend (TeXSeq x y) z = TeXSeq x $ mappend y z
 --
 mappend x y = TeXSeq x y

instance Semigroup.Semigroup (LaTeXL a) where
  (<>) = mappend

-- | Calling 'between' @c l1 l2@ puts @c@ between @l1@ and @l2@ and
--   appends them.
--
-- > between c l1 l2 = l1 <> c <> l2
between :: Monoid m => m -> m -> m -> m
between c l1 l2 = l1 <> c <> l2

-- | Method 'fromString' escapes LaTeX reserved characters using 'protectString'.
instance IsString (LaTeXL ()) where
 fromString = TeXRawL () . fromString . protectString

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

-- | Look into a 'LaTeXL' syntax tree to find any call to the command with
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
               -> LaTeXL a -- ^ LaTeX syntax tree.
               -> [[TeXArgL a]] -- ^ List of arguments passed to the command.
lookForCommand = (fmap snd .) . matchCommand . (==)

-- | Traverse a 'LaTeXL' syntax tree and returns the commands (see 'TeXComm' and
--   'TeXCommS') that matches the condition and their arguments in each call.
matchCommand :: (String -> Bool) -> LaTeXL a -> [(String,[TeXArgL a])]
matchCommand f (TeXCommL _ str as) =
  let xs = concatMap (matchCommandArg f) as
  in  if f str then (str,as) : xs else xs
matchCommand f (TeXCommSL _ str) = [(str, []) | f str]
matchCommand f (TeXEnvL _ _ _ as l) =
  let xs = concatMap (matchCommandArg f) as
  in  xs ++ matchCommand f l
matchCommand f (TeXMathL _ _ l) = matchCommand f l
matchCommand f (TeXBraces l)   = matchCommand f l
matchCommand f (TeXSeq l1 l2)  = matchCommand f l1 ++ matchCommand f l2
matchCommand _ _ = []

matchCommandArg :: (String -> Bool) -> TeXArgL a -> [(String,[TeXArgL a])]
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
lookForEnv :: String -> LaTeXL a -> [([TeXArgL a],LaTeXL a)]
lookForEnv = (fmap (\(_,as,l) -> (as,l)) .) . matchEnv . (==)

-- | Traverse a 'LaTeXL' syntax tree and returns the environments (see
--   'TeXEnv') that matches the condition, their arguments and their content
--   in each call.
matchEnv :: (String -> Bool) -> LaTeXL a -> [(String,[TeXArgL a],LaTeXL a)]
matchEnv f (TeXCommL _ _ as) = concatMap (matchEnvArg f) as
matchEnv f (TeXEnvL _ _ str as l) =
  let xs = concatMap (matchEnvArg f) as
      ys = matchEnv f l
      zs = xs ++ ys
  in  if f str then (str,as,l) : zs else zs
matchEnv f (TeXMathL _ _ l) = matchEnv f l
matchEnv f (TeXBraces l)   = matchEnv f l
matchEnv f (TeXSeq l1 l2)  = matchEnv f l1 ++ matchEnv f l2
matchEnv _ _ = []

matchEnvArg :: (String -> Bool) -> TeXArgL a -> [(String,[TeXArgL a],LaTeXL a)]
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
texmap :: (LaTeXL a -> Bool)    -- ^ Condition.
       -> (LaTeXL a -> LaTeXL a) -- ^ Function to apply when the condition matches.
       ->  LaTeXL a -> LaTeXL a
texmap c f = runIdentity . texmapM c (pure . f)

-- | Version of 'texmap' where the function returns values in a 'Monad'.
texmapM :: (Applicative m, Monad m)
        => (LaTeXL a -> Bool) -- ^ Condition.
        -> (LaTeXL a -> m (LaTeXL a)) -- ^ Function to apply when the condition matches.
        ->  LaTeXL a -> m (LaTeXL a)
texmapM c f = go
  where
   go l@(TeXCommL a str as)  = if c l then f l else TeXCommL a str <$> mapM go' as
   go l@(TeXEnvL a a' str as b) = if c l then f l else TeXEnvL a a' str <$> mapM go' as <*> go b
   go l@(TeXMathL a t b)     = if c l then f l else TeXMathL a t <$> go b
   go l@(TeXBraces b)        = if c l then f l else TeXBraces <$> go b
   go l@(TeXSeq l1 l2)       = if c l then f l else liftA2 TeXSeq (go l1) (go l2)
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
getBody :: LaTeXL a -> Maybe (LaTeXL a)
getBody l =
  case lookForEnv "document" l of
    ((_,b):_) -> Just b
    _ -> Nothing

-- | Extract the preamble of a 'LaTeXL' document (everything before the 'document'
--   environment). It could be empty.
getPreamble :: LaTeXL a -> LaTeXL a
getPreamble (TeXEnvL _ _ "document" _ _) = mempty
getPreamble (TeXSeq l1 l2) = getPreamble l1 <> getPreamble l2
getPreamble l = l

---------------------------------------
-- LaTeXL Arbitrary instance

arbitraryChar :: Gen Char
arbitraryChar = elements $
     ['A'..'Z']
  ++ ['a'..'z']
  ++ "\n-+*/!\"().,:;'@<>? "

-- | Utility for the instance of 'LaTeXL' to 'Arbitrary'.
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

instance Arbitrary (MeasureL a) where
  arbitrary = do
     n <- choose (0,5)
     let f = [Pt,Mm,Cm,In,Ex,Em] !! n
     f <$> arbitrary

instance Arbitrary a => Arbitrary (LaTeXL a) where
  arbitrary = arbitraryLaTeXL False

arbitraryLaTeXL :: Arbitrary a => Bool -> Gen (LaTeXL a)
arbitraryLaTeXL inDollar = do
  -- We give more chances to 'TeXRaw'.
  -- This results in arbitrary 'LaTeXL' values
  -- not getting too large.
  n <- choose (0,16 :: Int)
  case n of
    0 -> if inDollar then arbitraryLaTeXL True else pure TeXEmpty
    1 -> do m <- choose (0,5)
            TeXCommL <$> arbitrary <*> arbitraryName <*> vectorOf m arbitrary
    2 -> TeXCommSL <$> arbitrary <*> arbitraryName
    3 -> do m <- choose (0,5)
            TeXEnvL <$> arbitrary <*> arbitrary <*> arbitraryName <*> vectorOf m arbitrary <*> arbitrary
    4 -> if inDollar
            then arbitraryLaTeXL True
            else do m <- choose (0,3)
                    let t = [Parentheses,Square,Dollar,DoubleDollar] !! m
                    TeXMathL <$> arbitrary <*> pure t <*> arbitraryLaTeXL (t == Dollar || t == DoubleDollar)
    5 -> TeXLineBreak <$> arbitrary <*> arbitrary
    6 -> TeXBraces <$> arbitrary
    7 -> TeXComment <$> arbitraryRaw
    8 -> TeXSeq <$> (if inDollar then arbitraryLaTeXL True else arbitrary) <*> arbitrary
    _ -> TeXRawL <$> arbitrary <*> arbitraryRaw

instance Arbitrary a => Arbitrary (TeXArgL a) where
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


instance Hashable a => Hashable (MeasureL a)
instance Hashable MathType
instance Hashable a => Hashable (TeXArgL a)
instance Hashable a => Hashable (LaTeXL a)
