-- | Ti/k/Z ist /kein/ Zeichenprogramm.
--
-- Ti/k/Z is a frontend for PGF (Portable Graphics Format), a package for creating graphics
-- using scripts embedded in a LaTeX document.
--
-- Using this library you will be able to generate Ti/k/Z scripts using Haskell functions.
--
-- The interface given here is pretty close to the original Ti/k/Z interface. Another
-- layer of abstraction is given in "Text.LaTeX.Packages.TikZ.Simple", module built
-- from the entities exported here. Usually, one chooses one of the interfaces and
-- work with it. However, if you want to use both of them, you will have to use
-- qualified imports or you will get name clashes.
--
-- Also, the module exported here, "Text.LaTeX.Packages.TikZ.PathBuilder", provides
-- an interface to create paths (see 'TPath') using monads.
--
-- Once you have generated a Ti/k/Z script, use 'tikzpicture' to include it in a LaTeX
-- document.
module Text.LaTeX.Packages.TikZ (
   -- * TikZ package
   tikz
   -- * TikZ modules
 , module Text.LaTeX.Packages.TikZ.Syntax
 , module Text.LaTeX.Packages.TikZ.PathBuilder
   -- * Insertion in LaTeX
 , tikzpicture
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Render
import Text.LaTeX.Packages.TikZ.Syntax
import Text.LaTeX.Packages.TikZ.PathBuilder

-- | Import the 'tikz' package to use the functions exported
--   by this module. For example, adding this line to your
--   document preamble:
--
-- > usepackage [] tikz
--
tikz :: PackageName
tikz = "tikz"

-- | Transform a Ti/k/Z script to a 'LaTeX' block.
tikzpicture :: LaTeXC l => TikZ -> l
tikzpicture = fromLaTeX . TeXEnv "tikzpicture" [] . rendertex 
