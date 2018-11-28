
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.QRCode
 ( -- * qrcode package
   qrcode
   -- * qrcode commands
 , ErrorLevel(..)
 , CodeOptions(..)
 , defaultOptions
 , qr
   -- * Package Options
 , draft
 , final
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Texy
import qualified Data.Text as T

-- | qrcode package. Use it to import it like this:
--
-- > usepackage [] qrcode
qrcode :: PackageName
qrcode = "qrcode"

-- | The degree of error-correction redundancy to
-- include in the generated code.
data ErrorLevel = Low -- ^ Error recovery up to 7%.
                | Medium -- ^ Error recovery up to 15%.
                | Quality -- ^ Error recovery up to 25%.
                | High -- ^ Error recovery up to 30%.
  deriving (Eq, Ord, Read, Show)

-- | Options to use when generating a QR code.
data CodeOptions = CodeOptions {
                  includePadding :: Bool -- ^ Whether to include 4 modules of whitespace around the code. False is the default.
                , link :: Bool -- ^ Whether, if the code encodes a link, it should be hyperlinked in the PDF document. The default is true. Links will only be generated when the document uses the hyperref package.
                , errorLevel :: ErrorLevel -- ^ The desired degree of error-correction redundancy to include in the code. The default is 'Medium'.
                }
  deriving (Eq, Show)



-- | The default QR code generation options.
defaultOptions :: CodeOptions
defaultOptions = CodeOptions { includePadding = False, link = True, errorLevel = Medium }

-- | This package option sets the qrcode package to generate draft-quality placeholders for QR codes.
draft :: LaTeXC l => l
draft = "draft"

-- | This package option (which is the default) sets the qrcode package to generate print-quality QR codes.
final :: LaTeXC l => l
final = "final"

-- | Generates a QR code with specified options and content.
--
-- This uses the \qrcode command from the package, but the identifier
-- 'qrcode' is already in use as the 'PackageName'.
qr :: LaTeXC l => CodeOptions -> Text -> l
qr opt payload = fromLaTeX $ TeXComm "qrcode" [opts, FixArg . raw . escape $ payload]
  where
    opts = MOptArg [ if includePadding opt then "padding" else "tight"
                   , if link opt then "link" else "nolink"
                   , texy . ("level=" <>) . T.singleton . head . show . errorLevel $ opt
                   ]

-- Helper functions for escaping code contents.
escape :: Text -> Text
escape = T.concatMap handleChar
  where handleChar c | isSpecial c = T.pack ['\\', c]
                     | otherwise   = T.singleton c

isSpecial :: Char -> Bool
isSpecial c = elem c ("#$&^_~% \\{}" :: String)
