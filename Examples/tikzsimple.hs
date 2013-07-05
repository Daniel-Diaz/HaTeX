
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
theBody = mapM_ (center . tikzpicture . figuretikz) [myFigure,myFigure2,myFigure3]

myFigure :: Figure
myFigure = Scale 3 $ Figures
 [ RectangleFilled (0,0) 1 1
 , Colored Green $ RectangleFilled (-1,1) 1 1
 , Colored Red   $ RectangleFilled ( 0,2) 1 1
 , Colored Blue  $ RectangleFilled ( 1,1) 1 1
   ]

myFigure2 :: Figure
myFigure2 = Scale 2 $ Figures
 [ Colored Blue $ PolygonFilled [(-2,-1),(0,1),(2,-1)]
 , Text (0,1.4) $ "Is this a " <> textit "blue" <> " triangle?"
 , Rotate (pi/9) $ Colored Yellow $ Text (-0.2,-0.2) "Yes, it is!"
   ]

myFigure3 :: Figure
myFigure3 = LineWidth (Pt 2) $
   pathImage 0.01 (0,4) $
      -- Spira mirabilis (logarithmic spiral)
      \t -> ( a * exp t * cos (b*t)
            , a * exp t * sin (b*t)
              )
  where
    a = 0.1 ; b = 4
