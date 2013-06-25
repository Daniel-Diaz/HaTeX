
-- | A simple interface to create Ti/k/Z graphics. Just build pictures using
--   the 'Figure' data constructors, and get the Ti/k/Z script using the function
--   'figuretikz'. Use the function 'tikzpicture' to insert the Ti/k/Z script in
--   the LaTeX document.
--
--   Please, note that this module is not intended to be imported in the same module
--   than Text.LaTeX.Packages.TikZ. This module is itself a self-contained /alternative/
--   of that module. If still want to use both modules, please, use qualified imports
--   to avoid name clashes.
module Text.LaTeX.Packages.TikZ.Simple (
   -- TikZ package
   tikz
   -- * Figures
 , Figure (..)
 , Point
 , Color (..)
   -- * Figure scripting
 , figuretikz
 , tikzpicture
   ) where

import Text.LaTeX.Base.Types (Measure)
import Text.LaTeX.Packages.TikZ (TikZ,Color,tikzpicture,emptytikz,tikz)
import qualified Text.LaTeX.Packages.TikZ as T

-- | A point in the plane.
type Point = (Double,Double)

-- | A figure in the plane.
data Figure =
   Line [Point]
 | Polygon [Point]
 | PolygonFilled [Point]
 | Rectangle Point Double Double -- ^ Rectangle with top-right corner at the given point and
                                 --   width and height given by the other parameters.
 | RectangleFilled Point Double Double -- ^ Same as 'Rectangle', but filled.
 | Circle Point Double
 | CircleFilled Point Double
 | Ellipse Point Double Double
 | EllipseFilled Point Double Double
 | Colored Color Figure
 | LineWidth Measure Figure
 | Scale Double Figure
 | Figures [Figure]

castpoint :: Point -> T.TPoint
castpoint (x,y) = T.pointAtXY x y

-- | Translate a 'Figure' to a 'TikZ' script.
figuretikz :: Figure -> TikZ
figuretikz (Line []) = emptytikz
figuretikz (Line (p:ps)) = T.draw $ foldl (\y x -> y T.->- castpoint x) (T.Start $ castpoint p) ps
figuretikz (Polygon []) = emptytikz
figuretikz (Polygon (p:ps)) = T.draw $ T.Cycle $ foldl (\y x -> y T.->- castpoint x) (T.Start $ castpoint p) ps
figuretikz (PolygonFilled []) = emptytikz
figuretikz (PolygonFilled (p:ps)) = T.fill $ T.Cycle $ foldl (\y x -> y T.->- castpoint x) (T.Start $ castpoint p) ps
figuretikz (Rectangle p w h) = T.draw $ T.Rectangle (T.Start $ castpoint p) $ T.relPoint $ castpoint (w,-h)
figuretikz (RectangleFilled p w h) = T.fill $ T.Rectangle (T.Start $ castpoint p) $ T.relPoint $ castpoint (w,-h)
figuretikz (Circle p r) = T.draw $ T.Circle (T.Start $ castpoint p) r
figuretikz (CircleFilled p r) = T.fill $ T.Circle (T.Start $ castpoint p) r
figuretikz (Ellipse p r1 r2) = T.draw $ T.Ellipse (T.Start $ castpoint p) r1 r2
figuretikz (EllipseFilled p r1 r2) = T.fill $ T.Ellipse (T.Start $ castpoint p) r1 r2
figuretikz (Colored c f) = T.scope [T.TColor c] $ figuretikz f
figuretikz (LineWidth m f) = T.scope [T.TWidth m] $ figuretikz f
figuretikz (Scale q f) = T.scope [T.TScale q] $ figuretikz f
figuretikz (Figures fs) = foldr (\x y -> figuretikz x T.->> y) emptytikz fs
