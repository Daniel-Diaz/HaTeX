
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
 maketitle
 "Below a picture generated using the TikZ DSL of "
 hatex
 "."
 center $ tikzpicture $ draw $
  Cycle $ Start (pointAtXY 0 0) ->- pointAtXY 1 0 ->- pointAtXY 0 1
 "And some pictures more."
 center $ tikzpicture $
      draw  (Rectangle (Start $ pointAtXY 0   0  ) (pointAtXY 1 1))
  ->> fill  (Circle    (Start $ pointAtXY 1.5 0.5)  0.5)
  ->> shade (Ellipse   (Start $ pointAtXY 3   0.5 ) 1 0.5)
 center $ tikzpicture $ draw $
  (Cycle $ Start (pointAtXY 0 0) ->- pointAtXY 1 0 ->- pointAtXY 0 1) ->- pointAtXY 1 1
