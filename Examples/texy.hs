{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX

import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.AMSMath

import Data.Ratio
import Data.Matrix

main :: IO ()
main = renderFile "texy.tex" example

example :: LaTeX
example = thePreamble <> document theBody

thePreamble :: LaTeX
thePreamble =
    documentclass [] article <> usepackage [utf8] inputenc
 <> usepackage [] amsmath
 <> title "Using the Texy Class" <> author "Daniel Díaz"

theBody :: LaTeX
theBody =
    maketitle
 <> "Different types pretty-printed using the " <> texttt "Texy" <> " class:"
 <> itemize theItems

theItems :: LaTeX
theItems = 
    item Nothing <> math (texy (2 :: Int ,3 :: Integer))
 <> item Nothing <> math (texy [True,False,False])
 <> item Nothing <> math (texy (1 % 2 :: Rational,2.5 :: Float))
 <> item Nothing <> equation_ (texy $ fromList 3 3 [1 .. 9 :: Int])
 <> item Nothing <> math (texy ("This is a String" :: String))
