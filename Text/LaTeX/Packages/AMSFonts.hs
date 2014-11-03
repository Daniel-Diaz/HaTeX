
-- | Module for the package @amsfonts@.
module Text.LaTeX.Packages.AMSFonts
 ( -- * AMSFonts package
   amsfonts
   -- * Fonts
 , mathbb
   -- * Number sets
 , naturals, integers, rationals, reals, complexes
   ) where

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types

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
