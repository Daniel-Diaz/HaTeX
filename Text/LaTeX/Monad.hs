
-- | This module is a re-export of the main modules.
--   Importing this will you give access to almost all
--   functionality of the library. It is recommended
--   to import the @Base@ module and, then, import only
--   packages you will need.
module Text.LaTeX.Monad
 ( module Text.LaTeX.Base.Monad
 , module Text.LaTeX.Packages.Monad
   ) where

import Text.LaTeX.Base.Monad
import Text.LaTeX.Packages.Monad
