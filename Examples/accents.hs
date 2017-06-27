
{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Packages.Inputenc

main :: IO ()
main = renderFile "accents.tex" $ execLaTeXM accents

accents :: LaTeXM ()
accents = thePreamble >> document theBody

thePreamble :: LaTeXM ()
thePreamble = do
  documentclass [] article
  usepackage [utf8] inputenc
  author "Daniel Díaz"
  title "HaTeX and accents"

theBody :: LaTeXM ()
theBody = do
  maketitle
  flushleft "ÁáÉéÍíÓóÚú"
  flushleft "ÀàÈèÌìÒòÙù"
  flushleft "ÄäËëÏïÖöÜü"
  flushleft "ÂâÊêÎîÔôÛû"
