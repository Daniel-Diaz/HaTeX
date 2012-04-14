
module Text.LaTeX.Packages.Trees (
  -- * Tree
  Tree (..)
 ) where

-- | Tree datatype.
data Tree a =
   Leaf a -- ^ Leafs are non-empty.
 | Node (Maybe a) [Tree a] -- ^ Node values are optional.
