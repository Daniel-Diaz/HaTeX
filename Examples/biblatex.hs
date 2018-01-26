
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import Text.LaTeX
import Text.LaTeX.Packages.BibLaTeX
import Text.LaTeX.Packages.Inputenc
import qualified "bibtex" Text.BibTeX.Entry as BibTeX
import qualified "bibtex" Text.BibTeX.Format as BibTeX
import System.IO (writeFile)
import System.Process (callProcess)

main :: IO ()
main = do
  execLaTeXT bibLaTeXExample >>= renderFile "biblatex.tex"
  writeFile bibFile . unlines $ BibTeX.entry <$> exampleBibliography
  mapM_ (`callProcess`["biblatex"]) ["pdflatex", "biber", "pdflatex"]

bibFile = "biblatex-example.bib"

bibLaTeXExample :: Monad m => LaTeXT_ m
bibLaTeXExample = thePreamble >> document theBody

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
 documentclass [] article
 usepackage [utf8] inputenc
 usepackage ["backend=biber"] biblatex
 addbibresource bibFile
 author "Justus Sageműller"
 title "BibLaTeX example using HaTeX"

theBody :: Monad m => LaTeXT_ m
theBody = do
 "Bib"<>latex<>cite "bibLaTeX-manual"
 " is a package for creating references to literature listed in a Bib"<>tex<>" file"
 " (extension "<>verb".bib"<>")."
 printbibliography

exampleBibliography :: [BibTeX.T]
exampleBibliography =
 [ BibTeX.Cons
    "manual"
    "bibLaTeX-manual"
    [ ("author", "P Lehman and P Kime and A Boruvka and J Wright")
    , ("title", "The biblatex Package. Programmable Bibliographies and Citations.")
    , ("URL", "http://mirrors.ctan.org/macros/latex/contrib/biblatex/doc/biblatex.pdf") ]
 ]
