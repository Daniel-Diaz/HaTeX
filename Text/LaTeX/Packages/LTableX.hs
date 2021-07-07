
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.LTableX
 ( -- * ltablex package
   ltablex
   -- * ltablex commands
 , keepXColumns
 , convertXColumns
 , module Text.LaTeX.Packages.TabularX
 , module Text.LaTeX.Packages.LongTable
 ) where

import Text.LaTeX.Base.Syntax 
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX)
import Text.LaTeX.Base.Types (PackageName)
import Text.LaTeX.Packages.TabularX (tabularx)
import Text.LaTeX.Packages.LongTable (endfirsthead, endhead, endfoot, endlastfoot)

-- | ltablex package. Use it to import it like this:
--
-- > usepackage [] ltablex
ltablex :: PackageName
ltablex = "ltablex"

keepXColumns :: LaTeXC l => l
keepXColumns = fromLaTeX $ TeXComm "keepXColumns" []

-- | Treet the specified width as the maximum allowed, not the exact width of the table.
--
-- ltablex has added a feature that treats the X columns like ‘l’
-- columns if the table contents would allow that to happen without
-- exceeding the specified width of the table. In other words, the
-- specified width is treated as the maximum allowed and not the exact
-- width of the table. This feature is the default but can be disabled
-- (or enabled) with \keepXColumns (or \convertXColumns).

convertXColumns :: LaTeXC l => l
convertXColumns = fromLaTeX $ TeXComm "convertXColumns" []
