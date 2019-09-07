{-# LANGUAGE OverloadedStrings #-}

-- | This package is of vital importance if you use non-ASCII characters in your document.
--   For example, if you type the word /Ángela/, the /Á/ character will not appear correctly in the
--   output. To solve this problem, use:
--
-- > usepackage [utf8] inputenc
--
--   And make sure that your Haskell source is encoded in UTF-8.
module Text.LaTeX.Packages.Inputenc
 ( -- * Inputenc package
   inputenc
   -- * Encodings
 , utf8
 , latin1
   ) where

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types

-- | Inputenc package.
-- Example:
--
-- > usepackage [utf8] inputenc
inputenc :: PackageName
inputenc = "inputenc"

-- | UTF-8 encoding.
utf8 :: LaTeXC l => l
utf8 = "utf8"

-- | Latin-1 encoding.
latin1 :: LaTeXC l => l
latin1 = "latin1"
