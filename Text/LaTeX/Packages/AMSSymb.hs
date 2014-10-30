
-- | Module for the package @amssymb@.
module Text.LaTeX.Packages.AMSSymb
 ( -- * AMSSymb package
   amssymb
   -- * Fonts
 , checkmark
   ) where

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types

-- | AMSSymb package.
-- Example:
--
-- > usepackage [] amssymb
amssymb :: ClassName
amssymb = "amssymb"

--

-- | /✔/ symbol.
checkmark :: LaTeXC l => l
checkmark = comm0 "checkmark"

