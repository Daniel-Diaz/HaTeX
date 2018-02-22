{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.Lscape
 ( -- * lscape package
   lscape
   -- * lscape commands
 , landscape
   -- * Package Options
 , pdftex
 ) where

import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv))
import Text.LaTeX.Base.Class (LaTeXC, liftL)
import Text.LaTeX.Base.Types (PackageName)

-- | lscape package. Use it to import it like this:
--
-- > usepackage [] lscape
lscape :: PackageName
lscape = "lscape"

-- | This package option makes 'lscape' rotate the PDF paper – not
-- just the text on the page – when given the 'pdftex' option.
-- (Naturally, this works only with pdfLaTeX.) The result is that the
-- text is viewable online without the reader having to rotate his/her
-- head 90 degrees. The document still prints normally.
pdftex :: LaTeXC l => l
pdftex = "pdftex"

-- | All text within the 'landscape' environment is rotated through 90
-- degrees. The environment may span several pages. It works well
-- with, and was originally created for, use with 'longtable' to
-- produce long wide tables.
landscape :: LaTeXC l =>
             l           -- ^ Text to be rotated
          -> l           -- ^ Resulting rotated text.
landscape = liftL $ TeXEnv "landscape" []
