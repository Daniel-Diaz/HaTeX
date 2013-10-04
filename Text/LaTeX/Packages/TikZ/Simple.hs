
-- | A simple interface to create Ti/k/Z graphics. Just build pictures using
--   the 'Figure' data constructors, and get the Ti/k/Z script using the function
--   'figuretikz'. Use the function 'tikzpicture' to insert the Ti/k/Z script in
--   the LaTeX document. And do not forget to import the 'tikz' package in the
--   preamble.
--
--   Please, note that this module is not intended to be imported in the same module
--   than Text.LaTeX.Packages.TikZ. This module is itself a self-contained /alternative/
--   of that module. If you still want to use both modules, please, use qualified imports
--   to avoid name clashes.
--
--   In the /Examples/ directory of the source distribution, the file @tikzsimple.hs@
--   contains a complete example of usage of this module with several pictures.
--   Below you can see a picture along with the code it came from.
--
--   <<docfiles/tikz/tikzsimple.png>>
--
-- > myFigure :: Figure
-- > myFigure = Scale 2 $ Figures
-- >   [ RectangleFilled (0,0) 1 1
-- >   , Colored Green $ RectangleFilled (-1,1) 1 1
-- >   , Colored Red   $ RectangleFilled ( 0,2) 1 1
-- >   , Colored Blue  $ RectangleFilled ( 1,1) 1 1
-- >     ]
--
module Text.LaTeX.Packages.TikZ.Simple (
   -- TikZ package
   tikz
   -- * Figures
 , Figure (..)
 , Point
 , TikZColor (..)
 , Color (..)
   -- * Additional functions
 , pathImage
   -- * Figure scripting
 , figuretikz
 , (T.->>)
 , tikzpicture
   ) where

import Text.LaTeX.Base.Syntax (LaTeX)
import Text.LaTeX.Base.Types (Measure)
import Text.LaTeX.Packages.TikZ (TikZ,TikZColor,Color,tikzpicture,emptytikz,tikz)
import qualified Text.LaTeX.Packages.TikZ as T

-- | A point in the plane.
type Point = (Double,Double)

-- | A figure in the plane.
data Figure =
   Line [Point] -- ^ Line along a list of points.
 | Polygon [Point] -- ^ Line along a list of points, but the last point will be joined
                   --   with the first one.
 | PolygonFilled [Point] -- ^ Same as 'Polygon', but the inner side will be filled with color.
 | Rectangle Point Double Double -- ^ Rectangle with top-right corner at the given point and
                                 --   width and height given by the other parameters.
 | RectangleFilled Point Double Double -- ^ Same as 'Rectangle', but filled with color.
 | Circle Point Double -- ^ Circle centered at the given point with the given radius.
 | CircleFilled Point Double -- ^ As in 'Circle', but it will be filled with some color.
 | Ellipse Point Double Double -- ^ Ellipse centered at the given point with width and
                               --   height given by the other parameters.
 | EllipseFilled Point Double Double -- ^ Same as 'Ellipse', but filled with some color.
 | Text Point LaTeX -- ^ Insert some 'LaTeX' code, centered at the given 'Point'.
                    --   The text should not be very complex to fit nicely in the picture.
 | Colored TikZColor Figure -- ^ Color for the given 'Figure'.
 | LineWidth Measure Figure -- ^ Line width for the given 'Figure'.
 | Scale Double Figure -- ^ Scaling of the given 'Figure' by a factor.
 | Rotate Double Figure -- ^ Rotate a 'Figure' by a given angle (in radians).
 | Figures [Figure] -- ^ A figure composed by a list of figures.

castpoint :: Point -> T.TPoint
castpoint (x,y) = T.pointAtXY x y

radiansToDegrees :: Double -> Double
radiansToDegrees x = (180 * x) / pi

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
figuretikz (Ellipse p r1 r2) = T.draw $ T.Ellipse (T.Start $ castpoint p) (r1/2) (r2/2)
figuretikz (EllipseFilled p r1 r2) = T.fill $ T.Ellipse (T.Start $ castpoint p) r1 r2
figuretikz (Text p l) = T.draw $ T.Node (T.Start $ castpoint p) l
figuretikz (Colored c f) = T.scope [T.TColor c] $ figuretikz f
figuretikz (LineWidth m f) = T.scope [T.TWidth m] $ figuretikz f
figuretikz (Scale q f) = T.scope [T.TScale q] $ figuretikz f
figuretikz (Rotate a f) = T.scope [T.TRotate $ radiansToDegrees a] $ figuretikz f
figuretikz (Figures fs) = foldr (\x y -> figuretikz x T.->> y) emptytikz fs

-- | The figure of a /path/. A /path/ (in this context) means a function from an interval to
--   the plane. The image of such a function is what this function returns as a 'Figure'.
--   An additional argument is needed to set the precision of the curve.
--
--   The actual implementation builds a spline of degree one joining different points of the
--   image. Given that the interval is /(a,b)/ and the precision argument is &#949;, the points
--   in the spline will be /f(a)/, /f(a+/&#949;/)/, /f(a+2/&#949;/)/, and so on, until reaching /f(b)/.
--   The smaller is &#949;, the closer is the figure to the original image.
--
--   Here is an example with a logarithmic spiral.
--
--   <<docfiles/tikz/spiral.png>>
--
-- > spiral :: Figure
-- > spiral = LineWidth (Pt 2) $
-- >     pathImage 0.01 (0,4) $
-- >       \t -> ( a * exp t * cos (b*t)
-- >             , a * exp t * sin (b*t)
-- >               )
-- >   where
-- >     a = 0.1 ; b = 4
--
pathImage :: Double -- ^ Precision argument, &#949;.
          -> (Double,Double) -- ^ Interval, /(a,b)/.
          -> (Double -> Point) -- ^ Path function, /f/.
          -> Figure -- ^ Output figure.
pathImage eps (a,b) f = Line $ listFrom a
  where
   listFrom x =
     if x >= b then [f b]
               else f x : listFrom (x+eps)
