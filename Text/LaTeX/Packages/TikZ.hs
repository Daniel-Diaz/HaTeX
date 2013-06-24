
-- | Ti/k/Z ist /kein/ Zeichenprogramm.
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
