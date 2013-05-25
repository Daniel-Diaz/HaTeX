
{-# LANGUAGE OverloadedStrings #-}

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
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types

-- | The 'hyperref' package.
--
-- > usepackage [] hyperref
hyperref :: PackageName
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
href :: LaTeXC l => [HRefOption] -> URL -> l -> l
href options u = liftL $ \t -> TeXComm "href" [ MOptArg $ fmap rendertex options
                                              , FixArg  $ rendertex u
                                              , FixArg t ]

-- | Write an 'URL' hyperlinked.
url :: LaTeXC l => URL -> l
url u = fromLaTeX $ TeXComm "url" [ FixArg $ rendertex u ]

-- | Write an 'URL' without creating a hyperlink.
nolinkurl :: LaTeXC l => URL -> l
nolinkurl u = fromLaTeX $ TeXComm "nolinkurl" [ FixArg $ rendertex u ]

-- | Establish a base 'URL'.
hyperbaseurl :: LaTeXC l => URL -> l
hyperbaseurl u = fromLaTeX $ TeXComm "hyperbaseurl" [ FixArg $ rendertex u ]

-- | @hyperimage imgURL t@:
--  The link to the image referenced by the @imgURL@ is inserted, using @t@ as the anchor.
hyperimage :: LaTeXC l => URL -> l -> l
hyperimage u = liftL $ \t -> TeXComm "hyperimage" [ FixArg $ rendertex u , FixArg t ]

-- | This is a replacement for the usual 'ref' command that places a contextual label in front of the reference.
autoref :: LaTeXC l => Label -> l
autoref l = fromLaTeX $ TeXComm "autoref" [ FixArg $ rendertex l ]
