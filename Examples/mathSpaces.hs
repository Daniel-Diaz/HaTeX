{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.LaTeX
import Text.LaTeX.Packages.AMSMath

main :: IO ()
main = execLaTeXT mathSpaces >>= renderFile "mathSpaces.tex"

mathSpaces :: Monad m => LaTeXT_ m
mathSpaces = do
 documentclass [] article
 usepackage [] amsmath
 document $
   tabular Nothing [RightColumn, LeftColumn] $ do
     texttt "quad"       & math ("H" >> quad       >> "H") ; lnbk
     texttt "qquad"      & math ("H" >> qquad      >> "H") ; lnbk
     texttt "thinspace"  & math ("H" >> thinspace  >> "H") ; lnbk
     texttt "medspace"   & math ("H" >> medspace   >> "H") ; lnbk
     texttt "thickspace" & math ("H" >> thickspace >> "H") ; lnbk
     texttt "negspace"   & math ("H" >> negspace   >> "H") ; lnbk
     texttt "space"      & math ("H" >> space      >> "H")
