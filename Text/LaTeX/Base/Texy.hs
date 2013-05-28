
-- | 'Texy' class, as proposed in <http://deltadiaz.blogspot.com.es/2013/04/hatex-36-proposal-texy-class.html>.
module Text.LaTeX.Base.Texy (
   -- * Texy class
   Texy (..)
 ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Commands
--
import Numeric
import Data.Fixed
import Data.List (intersperse)

-- | Class of types that can be pretty-printed as 'LaTeX' values.
class Texy t where
 texy :: LaTeXC l => t -> l

-- Basic instances

instance Texy LaTeX where
 texy = fromLaTeX

instance Texy Text where
 texy = raw . protectText

instance Texy Int where
 texy = rendertex

instance Texy Integer where
 texy = rendertex

instance Texy Float where
 texy x = fromString $ showFFloat Nothing x ""

instance Texy Double where
 texy x = fromString $ showFFloat Nothing x ""

instance Texy Char where
 texy c = fromString ['`' , c , '`']

instance HasResolution a => Texy (Fixed a) where
 texy = fromString . show

instance Texy Bool where
 texy = fromString . show
