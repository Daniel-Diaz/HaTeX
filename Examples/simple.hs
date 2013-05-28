
{- Simple example

This example is intended to be as simple as possible, but containing the most significant parts.

The Overloaded Strings language extension is quite handy because it allows you to write text without
using 'fromString' everywhere.

-}

{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX

-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
main :: IO ()
main = execLaTeXT simple >>= renderFile "simple.tex"

-- It's a good idea to separate the preamble of the body.
simple :: Monad m => LaTeXT_ m
simple = do
 thePreamble
 document theBody

-- Preamble with some basic info.
thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
 documentclass [] article
 author "Daniel Diaz"
 title "Simple example"

-- Body with a section.
theBody :: Monad m => LaTeXT_ m
theBody = do
 maketitle
 section "Hello"
 "This is a simple example using the "
 hatex
 " library. "
 -- 'textbf' turns characters to bold font (as you already may know).
 textbf "Enjoy!"
 " "
 -- This is how we nest commands.
 textbf (large "Yoohoo!")
