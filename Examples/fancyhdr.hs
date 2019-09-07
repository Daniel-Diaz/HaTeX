{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Packages.Fancyhdr

main :: IO ()
main = renderFile "fancyhdr.tex" hdrexample

hdrexample :: LaTeX
hdrexample = thePreamble <> document theBody

thePreamble :: LaTeX
thePreamble = documentclass [] article <> applyHdrSettings mySettings

mySettings :: HdrSettings
mySettings = defaultHdrSettings
  { centerHeader = textbf "Amazing header"
    }

theBody :: LaTeX
theBody = "This page use " <> texttt "fancyhdr" <> " to customize both "
 <> "header and footer."
