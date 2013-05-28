
-- | This module is a re-export of the Base module.
--   You may find it shorter to import.
--
--   Historically, this module also exported the Packages
--   module. But, since it's more common to import the Base
--   module and, then, only the packages you need (instead
--   of all of them), this module has been upgraded supporting
--   it.
--
--   For this reason, the module @Text.LaTeX.Packages@ no longer
--   exists.
module Text.LaTeX
 ( -- * Base module
   module Text.LaTeX.Base
   ) where

import Text.LaTeX.Base
