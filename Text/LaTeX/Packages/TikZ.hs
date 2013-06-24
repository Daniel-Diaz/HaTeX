
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

tikz :: PackageName
tikz = "tikz"

tikzpicture :: LaTeXC l => TikZ -> l
tikzpicture = fromLaTeX . TeXEnv "tikzpicture" [] . rendertex 
