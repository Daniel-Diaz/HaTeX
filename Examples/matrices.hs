import Text.LaTeX
import Text.LaTeX.Packages.AMSMath

import Data.Matrix

main :: IO ()
main = execLaTeXT matrices >>= renderFile "matrices.tex"

matrices :: Monad m => LaTeXT_ m
matrices = do
 thePreamble
 document theBody

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
 documentclass [] article
 -- You need to import 'amsmath' to use 'pmatrix'.
 usepackage [] amsmath

theBody :: Monad m => LaTeXT_ m
theBody = equation_ $ pmatrix Nothing $ (identity 5 :: Matrix Int)
