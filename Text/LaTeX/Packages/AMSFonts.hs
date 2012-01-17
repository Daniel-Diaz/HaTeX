
{-# OPTIONS_HATEX MakeMonadic #-}

module Text.LaTeX.Packages.AMSFonts
 ( -- * AMSFonts package
   amsfonts
   -- * Fonts
 , mathbb
   ) where

import Text.LaTeX.Base.Syntax

-- | AMSFonts package.
-- Example:
--
-- > usepackage [] amsfonts
amsfonts :: String
amsfonts = "amsfonts"

--

-- | This font is useful for representing sets like
--   R (real numbers) or Z (integers). For instance:
--
-- > "The set of real numbers are represented by " <> mathbb "R" <> "."
--
-- /Note the use of overloaded strings./
mathbb :: LaTeX -> LaTeX
mathbb l = TeXComm "mathbb" [FixArg l]
