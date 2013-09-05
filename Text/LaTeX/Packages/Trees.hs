
-- | Tree definition with some class instances.
module Text.LaTeX.Packages.Trees (
   -- * Tree
   Tree (..)
 ) where

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative

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
