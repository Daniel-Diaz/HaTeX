
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HATEX MakeMonadic #-}

module Text.LaTeX.Packages.Hyperref
 ( -- * Hyperref package
   hyperref
   -- * Hyperref commands
 , HRefOption (..)
 , URL
 , createURL
 , href
 , url
 , nolinkurl
 , hyperbaseurl
 , hyperimage
 , autoref
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types

-- | The 'hyperref' package.
--
-- > usepackage [] hyperref
hyperref :: String
hyperref = "hyperref"

data HRefOption =
   PDFRemoteStartView
 | PDFNewWindow
 | HRefPage Int
   deriving Show

instance Render HRefOption where
 render PDFRemoteStartView = "pdfremotestartview"
 render PDFNewWindow = "pdfnewwindow"
 render (HRefPage n) = "page=" <> render n

newtype URL = URL String deriving Show

instance Render URL where
 render (URL str) = fromString str

createURL :: String -> URL
createURL = URL

-- | Reference to an 'URL'.
href :: [HRefOption] -> URL -> LaTeX -> LaTeX
href options u t = TeXComm "href" [ MOptArg $ fmap (TeXRaw . render) options
                                  , FixArg  $ TeXRaw $ render u
                                  , FixArg t ]

-- | Write an 'URL' hyperlinked.
url :: URL -> LaTeX
url u = TeXComm "url" [ FixArg $ TeXRaw $ render u ]

-- | Write an 'URL' without creating a hyperlink.
nolinkurl :: URL -> LaTeX
nolinkurl u = TeXComm "nolinkurl" [ FixArg $ TeXRaw $ render u ]

-- | Establish a base 'URL'.
hyperbaseurl :: URL -> LaTeX
hyperbaseurl u = TeXComm "hyperbaseurl" [ FixArg $ TeXRaw $ render u ]

-- | @hyperimage imgURL t@:
--  The link to the image referenced by the @imgURL@ is inserted, using @t@ as the anchor.
hyperimage :: URL -> LaTeX -> LaTeX
hyperimage u t = TeXComm "hyperimage" [ FixArg $ TeXRaw $ render u
                                      , FixArg t ]

-- | This is a replacement for the usual 'ref' command that places a contextual label in front of the reference.
autoref :: Label -> LaTeX
autoref l = TeXComm "autoref" [ FixArg $ TeXRaw $ render l ]
