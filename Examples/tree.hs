
{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX.Base
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.Inputenc

treeExample1 :: Tree String
treeExample1 = Node (Just "Root") [Leaf "Leaf 1",Leaf "Leaf 2"]

treeExample2 :: Tree Int
treeExample2 = Node (Just 0) [Node Nothing [Leaf 1,Leaf 2] , Leaf 3]

treeExample3 :: Tree LaTeX
treeExample3 = Node (Just $ textbf $ textit "Bold and italic")
                  [ Leaf $ textbf "Bold"
                  , Leaf $ textit "Italic"
                    ]

-- Main

main :: IO ()
main = renderFile "tree.tex" example

example :: LaTeX
example = thePreamble <> document theBody

thePreamble :: LaTeX
thePreamble =
    documentclass [] article
 <> usepackage [] qtree
 <> usepackage [utf8] inputenc
 <> title "Examples with trees"
 <> author "Daniel Díaz"

theBody :: LaTeX
theBody =
    maketitle
 <> tree fromString treeExample1
 <> rendertree treeExample2
 <> tree id treeExample3
