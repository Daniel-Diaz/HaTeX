
-- | Module for the package @amsfonts@.
module Text.LaTeX.Packages.AMSFonts
 ( -- * AMSFonts package
   amsfonts
   -- * Fonts
 , mathbb, mathfrak
   -- * Number sets
 , naturals, integers, rationals, reals, quaternions
   -- ** Complex numbers
 , complexes, trealPart, timagPart
   ) where

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types
import Data.Monoid

-- | AMSFonts package.
-- Example:
--
-- > usepackage [] amsfonts
amsfonts :: ClassName
amsfonts = "amsfonts"

--

-- | This font is useful for representing sets like
--   \(\mathbb{R}\) (real numbers) or \(\mathbb{Z}\) (integers). For instance:
--
-- > "The set of real numbers are represented by " <> mathbb "R" <> "."
--
-- Or in monadic form:
--
-- > "The set of real numbers are represented by " >> mathbb "R" >> "."
--
-- /Note the use of overloaded strings./
mathbb :: LaTeXC l => l -> l
mathbb = comm1 "mathbb"

-- | Fraktur font, like \(\mathfrak{abcXYZ}\).
mathfrak :: LaTeXC l => l -> l
mathfrak = comm1 "mathfrak"

-- | \(\mathbb{N}\)
naturals :: LaTeXC l => l
naturals = mathbb "N"

-- | \(\mathbb{Z}\)
integers :: LaTeXC l => l
integers = mathbb "Z"

-- | \(\mathbb{Q}\)
rationals :: LaTeXC l => l
rationals = mathbb "Q"

-- | \(\mathbb{R}\)
reals :: LaTeXC l => l
reals = mathbb "R"

-- | \(\mathbb{C}\)
complexes :: LaTeXC l => l
complexes = mathbb "C"

-- | \(\mathbb{H}\)
quaternions :: LaTeXC l => l
quaternions = mathbb "H"

-- | \(\Re\)
trealPart :: LaTeXC l => l -> l
trealPart z = comm0 "Re" <> z

-- | \(\Im\)
timagPart :: LaTeXC l => l -> l
timagPart z = comm0 "Im" <> z
