
{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Packages.TikZ.Simple

main :: IO ()
main = execLaTeXT tikzsimple >>= renderFile "tikzsimple.tex"

tikzsimple :: LaTeXT IO ()
tikzsimple = thePreamble >> document theBody

thePreamble :: LaTeXT IO ()
thePreamble = do
  documentclass [] article
  usepackage [] tikz

theBody :: LaTeXT IO ()
theBody = mapM_ (center . tikzpicture . figuretikz) [myFigure,myFigure2]

myFigure :: Figure
myFigure = Scale 3 $ Figures
 [ RectangleFilled (0,0) 1 1
 , Colored Green $ RectangleFilled (-1,1) 1 1
 , Colored Red   $ RectangleFilled ( 0,2) 1 1
 , Colored Blue  $ RectangleFilled ( 1,1) 1 1
   ]

myFigure2 :: Figure
myFigure2 = Scale 3 $ Figures
 [ Colored Blue $ PolygonFilled [(-1,0),(0,1),(1,0)]
 , Text (0,1.2) $ "Is this a " <> textit "blue" <> " triangle?"
 , Colored Yellow $ Text (0,0.2) "Yes, it is"
   ]
