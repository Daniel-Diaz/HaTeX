
-- | This module is a re-export of the main modules.
--   Importing this will you give access to almost all
--   functionality of the library. However, it is recommended
--   to import the @Base@ module and, then, import only
--   packages you actually need.
module Text.LaTeX
 ( module Text.LaTeX.Base
 , module Text.LaTeX.Packages
   ) where

import Text.LaTeX.Base
import Text.LaTeX.Packages
