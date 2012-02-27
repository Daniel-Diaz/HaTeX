
-- | Re-export of all packages.
module Text.LaTeX.Packages
 ( -- * Encoding packages
   module Text.LaTeX.Packages.Inputenc
   -- * Cross-reference packages
 , module Text.LaTeX.Packages.Hyperref
   -- * Presentation packages
 , module Text.LaTeX.Packages.Beamer
   -- * Mathematics packages
 , module Text.LaTeX.Packages.AMSMath
 , module Text.LaTeX.Packages.AMSFonts
 , module Text.LaTeX.Packages.AMSThm
   -- * Graphics packages
 , module Text.LaTeX.Packages.Color
 , module Text.LaTeX.Packages.Graphicx
   ) where

import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.Hyperref
import Text.LaTeX.Packages.Beamer
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSFonts
import Text.LaTeX.Packages.AMSThm
import Text.LaTeX.Packages.Color
import Text.LaTeX.Packages.Graphicx