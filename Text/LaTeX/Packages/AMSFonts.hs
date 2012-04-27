
module Text.LaTeX.Packages.AMSFonts
 ( -- * AMSFonts package
   amsfonts
   -- * Fonts
 , mathbb
   ) where

import Text.LaTeX.Base.Syntax
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
mathbb = liftL $ \l -> TeXComm "mathbb" [FixArg l]
