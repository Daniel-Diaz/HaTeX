
{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.TikZ

main :: IO ()
main = execLaTeXT tikztest >>= renderFile "tikz.tex"

tikztest :: LaTeXT IO ()
tikztest = do
 thePreamble
 document theBody

thePreamble :: LaTeXT IO ()
thePreamble = do
 documentclass [] article
 usepackage [utf8] inputenc
 usepackage [] tikz
 author "Daniel Díaz"
 title "Example using TikZ"

theBody :: LaTeXT IO ()
theBody = do
 "Below a picture generated using the TikZ DSL of "
 hatex
 "."
 center $ tikzpicture $ draw $
      vertex (pointAtXY 0 0)
  ->- vertex (pointAtXY 1 0)
  ->- vertex (pointAtXY 0 1)
  ->- tcycle
