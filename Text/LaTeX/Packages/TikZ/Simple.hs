
-- | A simple interface to create Ti/k/Z graphics. Just build pictures using
--   the 'Figure' data constructors, and get the Ti/k/Z script using the function
--   'figuretikz'. Use the function 'tikzpicture' to insert the Ti/k/Z script in
--   the LaTeX document.
module Text.LaTeX.Packages.TikZ.Simple (
   -- * Figures
   Figure (..)
 , Color (..)
 , figuretikz
 , tikzpicture
   ) where

import Text.LaTeX.Base.Types (Measure)
import Text.LaTeX.Packages.TikZ (TikZ,Color,tikzpicture,emptytikz)
import qualified Text.LaTeX.Packages.TikZ as T

type Point = (Double,Double)

data Figure =
   Line [Point]
 | Polygon [Point]
 | PolygonFilled [Point]
 | Circle Point Double
 | CircleFilled Point Double
 | Ellipse Point Double Double
 | EllipseFilled Point Double Double
 | Colored Color Figure
 | Width Measure Figure
 | Figures [Figure]

castpoint :: Point -> T.TPoint
castpoint (x,y) = T.pointAtXY x y

figuretikz :: Figure -> TikZ
figuretikz (Line []) = emptytikz
figuretikz (Line (p:ps)) = T.draw $ foldl (\y x -> y T.->- castpoint x) (T.Start $ castpoint p) ps
figuretikz (Polygon []) = emptytikz
figuretikz (Polygon (p:ps)) = T.draw $ T.Cycle $ foldl (\y x -> y T.->- castpoint x) (T.Start $ castpoint p) ps
figuretikz (PolygonFilled []) = emptytikz
figuretikz (PolygonFilled (p:ps)) = T.fill $ T.Cycle $ foldl (\y x -> y T.->- castpoint x) (T.Start $ castpoint p) ps
figuretikz (Circle p r) = T.draw $ T.Circle (T.Start $ castpoint p) r
figuretikz (CircleFilled p r) = T.fill $ T.Circle (T.Start $ castpoint p) r
figuretikz (Ellipse p r1 r2) = T.draw $ T.Ellipse (T.Start $ castpoint p) r1 r2
figuretikz (EllipseFilled p r1 r2) = T.fill $ T.Ellipse (T.Start $ castpoint p) r1 r2
figuretikz (Colored c f) = T.scope [T.TColor c] $ figuretikz f
figuretikz (Width m f) = T.scope [T.TWidth m] $ figuretikz f
figuretikz (Figures fs) = foldr (\x y -> figuretikz x T.->> y) emptytikz fs
