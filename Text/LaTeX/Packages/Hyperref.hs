
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
 , nameref
   -- * Package options
 , pdftex
 , pdftitle
 , pdfauthor
 , pdfsubject
 , pdfcreator
 , pdfproducer
 , pdfkeywords
 , pdftrapped
 , pdfstartpage
 , pdfpagelayout
 , PdfPageLayout(..)
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Data.Text (pack)

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
-- TODO: This function should check that the input
-- String is a valid URL.

-- | 'fromString' = 'createURL'.
instance IsString URL where
 fromString = createURL

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

-- | Similar to 'autoref', but inserts text corresponding to the section name.
--   Note that this command comes from the /nameref/ package, but it's automatically
--   included when importing 'hyperref'.
nameref :: LaTeXC l => Label -> l
nameref l = fromLaTeX $ TeXComm "nameref" [ FixArg $ rendertex l ]

-- | Creates a single-parameter package option.
packageOption :: LaTeXC l => Text -> l -> l
packageOption n p = raw n <> raw "={" <> p <> raw "}"

-- | This package option selects the pdfTeX backend for the Hyperref package.
pdftex :: LaTeXC l => l
pdftex = raw "pdftex"

-- | This package option sets the document information Title field.
pdftitle :: LaTeXC l => l -> l
pdftitle = packageOption "pdftitle"

-- | This package option sets the document information Author field.
pdfauthor :: LaTeXC l => l -> l
pdfauthor = packageOption "pdfauthor"

-- | This package option sets the document information Subject field.
pdfsubject :: LaTeXC l => l -> l
pdfsubject = packageOption "pdfsubject"

-- | This package option sets the document information Creator field.
pdfcreator :: LaTeXC l => l -> l
pdfcreator = packageOption "pdfcreator"

-- | This package option sets the document information Producer field.
pdfproducer :: LaTeXC l => l -> l
pdfproducer = packageOption "pdfproducer"

-- | This package option sets the document information Keywords field.
pdfkeywords :: LaTeXC l => l -> l
pdfkeywords = packageOption "pdfkeywords"

-- | This package option sets the document information Trapped entry.
-- An 'Nothing' value means, the entry is not set. 
pdftrapped :: LaTeXC l => Maybe Bool -> l
pdftrapped Nothing = packageOption "pdftrapped" mempty
pdftrapped (Just t) = packageOption "pdftrapped" . raw . pack . show $ t

-- | This package option determines on which page the PDF file is opened.
pdfstartpage :: LaTeXC l => l -> l
pdfstartpage = packageOption "pdfstartpage"

-- | This package option sets the layout of PDF pages.
pdfpagelayout :: LaTeXC l => PdfPageLayout -> l
pdfpagelayout l = packageOption "pdfpagelayout" . raw . pack . show $ l

-- | Specification for how pages of a PDF should be displayed.
data PdfPageLayout = SinglePage -- ^ Displays a single page; advancing flips the page.
                   | OneColumn -- ^ Displays a single page; advancing flips the page.
                   | TwoColumnLeft -- ^ Displays the document in two columns, odd-numbered pages to the left.
                   | TwoColumnRight -- ^ Displays the document in two columns, odd-numbered pages to the right.
                   | TwoPageLeft -- ^ Displays two pages, odd-numbered pages to the left (since PDF 1.5).
                   | TwoPageRight -- ^ Displays two pages, odd-numbered pages to the right (since PDF 1.5).
  deriving (Eq, Ord, Read, Show)                   
