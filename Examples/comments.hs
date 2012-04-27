{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX.Base

main :: IO ()
main = execLaTeXT example >>= renderFile "Comments.tex"

example :: Monad m => LaTeXT_ m
example = do
 documentclass [] article
 document exampleBody

exampleBody :: Monad m => LaTeXT_ m
exampleBody = do
 "This is a basic example for testing the "
 "comments functionality in HaTeX." % "A short comment here."
 "Multi-line comments are separated by lines." % "First line.\nSecond line."
 "After a comment, the following code will start in a new line of the output."