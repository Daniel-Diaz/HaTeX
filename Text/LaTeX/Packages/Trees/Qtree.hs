
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

tree_ :: (a -> LaTeX) -> Tree a -> LaTeX
tree_ f (Leaf x) = braces $ f x
tree_ f (Node mx ts) =
  mconcat [ "["
          , maybe mempty (("." <>) . braces . f) mx
          , " "
          , mconcat $ intersperse " " $ fmap (tree_ f) ts
          , " ]"
            ]

tree :: (a -> LaTeX) -> Tree a -> LaTeX
tree f t = TeXCommS "Tree" <> " " <> tree_ f t

rendertree :: Render a => Tree a -> LaTeX
rendertree = tree rendertex