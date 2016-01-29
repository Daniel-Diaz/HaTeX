
-- | Module for the package @amsfonts@.
module Text.LaTeX.Packages.AMSFonts
 ( -- * AMSFonts package
   amsfonts
   -- * Fonts
 , mathbb, mathfrak
   -- * Number sets
 , naturals, integers, rationals, reals
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
--   R (real numbers) or Z (integers). For instance:
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

-- | Fraktur font.
mathfrak :: LaTeXC l => l -> l
mathfrak = comm1 "mathfrak"

-- | Number sets

naturals :: LaTeXC l => l
naturals = mathbb "N"

integers :: LaTeXC l => l
integers = mathbb "Z"

rationals :: LaTeXC l => l
rationals = mathbb "Q"

reals :: LaTeXC l => l
reals = mathbb "R"

complexes :: LaTeXC l => l
complexes = mathbb "C"

trealPart :: LaTeXC l => l -> l
trealPart z = comm0 "Re" <> z

timagPart :: LaTeXC l => l -> l
timagPart z = comm0 "Im" <> z
