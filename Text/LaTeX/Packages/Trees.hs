
-- | Tree definition with some class instances.
module Text.LaTeX.Packages.Trees (
   -- * Tree
   Tree (..)
   -- * @LaTeXTree@ class
 , LaTeXTree (..)
 ) where

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Data.String
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Writer
import Text.LaTeX.Base.Render

-- | Tree datatype.
data Tree a =
   Leaf a -- ^ Leafs are non-empty.
 | Node (Maybe a) [Tree a] -- ^ Node values are optional.

instance Functor Tree where
 fmap f (Leaf a) = Leaf $ f a
 fmap f (Node ma ts) = Node (fmap f ma) $ fmap (fmap f) ts

instance Foldable Tree where
 foldMap f (Leaf a) = f a
 foldMap f (Node ma ts) = foldMap f ma `mappend` mconcat (fmap (foldMap f) ts)

instance Traversable Tree where
 sequenceA (Leaf fa) = Leaf <$> fa
 sequenceA (Node mfa ts) = liftA2 Node (sequenceA mfa) $ sequenceA $ fmap sequenceA ts

-- This class is experimental and provisional. Please, don't use it yourself!
class (Monoid l, IsString l) => LaTeXTree l where
 texbraces :: l -> l
 texcomms :: String -> l
 totex :: Render a => a -> l

instance LaTeXTree LaTeX where
 texbraces = braces
 texcomms = TeXCommS
 totex = rendertex

instance Monad m => LaTeXTree (LaTeXT m a) where
 texbraces = liftFun braces
 texcomms = (>> return undefined) . textell . texcomms
 totex = (>> return undefined) . textell . rendertex