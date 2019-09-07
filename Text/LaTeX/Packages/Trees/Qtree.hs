{-# LANGUAGE OverloadedStrings, CPP #-}
#if __GLASGOW_HASKELL__ >= 801
{-# OPTIONS_GHC -Wno-orphans #-}
#else
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

-- | Tree interface using the @qtree@ package.
--   An example of usage is provided in the /examples/ directory of
--   the source distribution.
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
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.Trees
--
import Data.List (intersperse)

-- | The 'qtree' package.
qtree :: PackageName
qtree = "qtree"

tree_ :: LaTeXC l => (a -> l) -> Tree a -> l
tree_ f (Leaf x) = braces $ f x
tree_ f (Node mx ts) =
  mconcat [ "["
          , maybe mempty (("." <>) . braces . f) mx
          , " "
          , mconcat $ intersperse " " $ fmap (tree_ f) ts
          , " ]"
            ]

-- | Given a function to @LaTeX@ values, you can create a @LaTeX@ tree from a
--   Haskell tree. The function specifies how to render the node values.
tree :: LaTeXC l => (a -> l) -> Tree a -> l
tree f t = commS "Tree" <> " " <> tree_ f t

-- | Instance defined in "Text.LaTeX.Packages.Trees.Qtree".
instance Texy a => Texy (Tree a) where
 texy = tree texy

-- | This function works as 'tree', but use 'render' as rendering function.
rendertree :: (Render a, LaTeXC l) => Tree a -> l
rendertree = tree (raw . protectText . render)
