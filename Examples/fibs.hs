{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX.Base

main :: IO ()
main = execLaTeXT example >>= renderFile "Fibs.tex"

example :: Monad m => LaTeXT_ m
example = do
 documentclass [] article
 document exampleBody

exampleBody :: Monad m => LaTeXT_ m
exampleBody = do
 "This is an example of how "
 hatex3
 " works, printing a table of "
 "the thirteen first elements of the "
 "Fibonacci sequence."
 bigskip
 center $ underline $ textbf "Fibonacci table"
 center $ tabular Nothing [RightColumn,VerticalLine,LeftColumn] $ do
   textbf "Fibonacci number" & textbf "Value"
   lnbk
   hline
   foldr (\n l -> do texy n & texy (fib n)
                     lnbk
                     l ) (return ()) [0 .. 12]

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Int
fib = (fibs!!)
