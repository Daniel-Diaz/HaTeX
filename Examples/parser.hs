
{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Base.Parser
import Data.Text (unlines)
import qualified Data.Text.IO as T

main :: IO ()
main = case parseLaTeX example of
  Left err -> print err
  Right l  -> do
    putStrLn "Printing LaTeX AST..."
    print l
    putStrLn "Checking that (render . parse == id)..."
    let t = render l
    print $ example == t
    putStrLn "All done."

example :: Text
example = Data.Text.unlines
  [ "\\documentclass{article}"
  , "\\usepackage[utf8]{inputenc}"
  , "\\author{Daniel Díaz}"
  , "\\title{LaTeX parser}"
  , "\\begin{document}"
  , "\\maketitle"
  , "This is an example of how to parse LaTeX using the"
  , "\\HaTeX library."
  , "\\end{document}"
    ]
