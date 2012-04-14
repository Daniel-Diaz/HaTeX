
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.Trees.Qtree (
    -- * Tree re-export
    module Text.LaTeX.Packages.Trees
    -- * Qtree package
  , qtree
    -- * Tree to LaTeX rendering
  , tree
  , rendertree
  ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Trees
--
import Data.Monoid
import Data.List (intersperse)

qtree :: PackageName
qtree = "qtree"

tree_ :: LaTeXTree l => (a -> l) -> Tree a -> l
tree_ f (Leaf x) = texbraces $ f x
tree_ f (Node mx ts) =
  mconcat [ "["
          , maybe mempty (("." <>) . texbraces . f) mx
          , " "
          , mconcat $ intersperse " " $ fmap (tree_ f) ts
          , " ]"
            ]

tree :: LaTeXTree l => (a -> l) -> Tree a -> l
tree f t = texcomms "Tree" <> " " <> tree_ f t

rendertree :: (Render a, LaTeXTree l) => Tree a -> l
rendertree = tree totex