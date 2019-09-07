{-# LANGUAGE OverloadedStrings #-}

-- | Select new font encodings using the @fontenc@ package.
module Text.LaTeX.Packages.Fontenc (
   -- * Fontenc package
   fontenc
   -- * Font encodings
 , FontEnc (..)
 , useencoding
   ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class

-- Fontenc package

-- | The @fontenc@ package.
--   It is recommended to use the 'useencoding' function
--   to import it.
fontenc :: PackageName
fontenc = "fontenc"

-- Font encodings

-- | Font encodings.
data FontEnc = T1 | OT1 deriving Show

instance Render FontEnc where
 render T1 = "T1"
 render OT1 = "OT1"

instance Texy FontEnc where
 texy = texy . render

-- | In the preamble, select encodings to use in your document.
--   The last one will be the default encoding. Example:
--
-- > useencoding [T1]
--
--   It imports the @fontenc@ package. In fact:
--
-- > useencoding xs = usepackage (fmap texy xs) fontenc
--
useencoding :: LaTeXC l => [FontEnc] -> l
useencoding xs = usepackage (fmap texy xs) fontenc
