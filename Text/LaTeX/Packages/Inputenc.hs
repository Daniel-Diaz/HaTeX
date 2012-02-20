
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HATEX MakeMonadic #-}

module Text.LaTeX.Packages.Inputenc
 ( -- * Inputenc package
   inputenc
   -- * Encodings
 , utf8
 , latin1
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Types

-- | Inputenc package.
-- Example:
--
-- > usepackage [utf8] inputenc
inputenc :: PackageName
inputenc = "inputenc"

-- | UTF-8 encoding.
utf8 :: LaTeX
utf8 = "utf8"

-- | Latin-1 encoding.
latin1 :: LaTeX
latin1 = "latin1"
