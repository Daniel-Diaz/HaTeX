
{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Packages.Beamer
import Text.LaTeX.Packages.Inputenc

main :: IO ()
main = execLaTeXT beamerExample >>= renderFile "beamer.tex"

beamerExample :: Monad m => LaTeXT_ m
beamerExample = thePreamble >> document theBody

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
 -- To start using beamer, set the document class to be 'beamer'.
 documentclass [] beamer
 usepackage [utf8] inputenc
 author "Daniel Díaz"
 title "Beamer example using HaTeX"
 -- Use a theme to improve the aspect of your slides.
 usetheme CambridgeUS

theBody :: Monad m => LaTeXT_ m
theBody = do
 -- Each slide is a call to the function 'frame' with the content of the slide.
 frame maketitle
 frame $ do
  frametitle "Example with block"
  "Here comes "
  alert [FromSlide 2] "the block!"
  uncover [FromSlide 5] " Ha! I was hiding here!"
  pause
  pause
  block "The block" $ do
   pause
   "The content of the block appears after a pause."
  uncover [FromSlide 6] $ center "And that's it!"