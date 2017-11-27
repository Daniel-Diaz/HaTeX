
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import Text.LaTeX
import Text.LaTeX.Packages.BibLaTeX
import Text.LaTeX.Packages.Inputenc
import System.IO (writeFile)
import System.Process (callProcess)

main :: IO ()
main = do
  execLaTeXT bibLaTeXExample >>= renderFile "biblatexDOI.tex"
  mapM_ (`callProcess`["biblatexDOI"]) ["pdflatex", "biber", "pdflatex"]

bibLaTeXExample :: LaTeXT IO ()
bibLaTeXExample = do
   documentclass [] article
   usepackage ["backend=biber"] biblatex
   documentWithDOIReferences
        (masterBibFile "/home/data/promotion/Literatur/literatur.bib") $ do
     "We can cite documents by simply giving a DOI, such as "
     citeDOI "123456789" "J Doe et al 1950: Investigation of Foo"
     printbibliography
