
{-# LANGUAGE OverloadedStrings, CPP #-}

-- | This module defines the syntax of a Ti/k/Z script.
--
-- To generate a Ti/k/Z script, first create a 'TPath' using
-- data constructors, or alternatively, use a 'PathBuilder'
-- from the "Text.LaTeX.Packages.TikZ.PathBuilder" module.
--
-- Once a 'TPath' is created, use 'path' to render a picture
-- from it. Use 'scope' to apply some parameters to your picture,
-- such line width or color.
module Text.LaTeX.Packages.TikZ.Syntax (
    -- * Points
    TPoint
  , pointAt , pointAtXY , pointAtXYZ
  , relPoint , relPoint_
    -- * Paths
    -- ** Types
  , TPath (..)
  , GridOption (..)
  , Step (..)
    -- ** Critical points
  , startingPoint
  , lastPoint
    -- ** Functions
  , (->-)
    -- * Parameters
  , Parameter (..)
  , TikZColor (..)
  , Color (..)
  , Word8
    -- * TikZ
  , TikZ
  , emptytikz
  , path
  , scope
  , ActionType (..)
  , (->>)
    -- * Sugar
  , draw , fill , clip , shade
  , filldraw , shadedraw
  ) where
 
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Color
import qualified Data.Sequence as S
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Data.Foldable (foldMap)
#endif

-- POINTS

-- | A point in Ti/k/Z.
data TPoint =
    DimPoint Measure Measure
  | XYPoint Double Double
  | XYZPoint Double Double Double
  | RelPoint TPoint
  | RelPoint_ TPoint
    deriving Show

instance Render TPoint where
 render (DimPoint x y) = "(" <> renderCommas [x,y] <> ")"
 render (XYPoint x y) = "(" <> renderCommas [x,y] <> ")"
 render (XYZPoint x y z) = "(" <> renderCommas [x,y,z] <> ")"
 render (RelPoint p) = "++" <> render p
 render (RelPoint_ p) = "+" <> render p

-- | Point using 'Measure's for coordinantes.
pointAt :: Measure -> Measure -> TPoint
pointAt = DimPoint

-- | Point using numbers as coordinates.
pointAtXY :: Double -> Double -> TPoint
pointAtXY = XYPoint

-- | Three-dimensional point.
pointAtXYZ :: Double -> Double -> Double -> TPoint
pointAtXYZ = XYZPoint

-- | Makes a point relative to the previous.
relPoint :: TPoint -> TPoint
relPoint (RelPoint x) = RelPoint x
relPoint (RelPoint_ x) = RelPoint x
relPoint p = RelPoint p

relPoint_ :: TPoint -> TPoint
relPoint_ (RelPoint x) = RelPoint_ x
relPoint_ (RelPoint_ x) = RelPoint_ x
relPoint_ p = RelPoint_ p

-- PATHS

-- | Type for TikZ paths. Every 'TPath' has two fundamental points: the /starting point/
--   and the /last point/.
--   The starting point is set using the 'Start' constructor.
--   The last point then is modified by the other constructors.
--   Below a explanation of each one of them.
--   Note that both starting point and last point may coincide.
--   You can use the functions 'startingPoint' and 'lastPoint' to calculate them.
--   After creating a 'TPath', use 'path' to do something useful with it.
data TPath =
    Start TPoint -- ^ Let @y = Start p@.
                 --
                 -- /Operation:/ Set the starting point of a path.
                 --
                 -- /Last point:/ The last point of @y@ is @p@.
  | Cycle TPath  -- ^ Let @y = Cycle x@.
                 --
                 -- /Operation:/ Close a path with a line from the last point of @x@ to
                 -- the starting point of @x@.
                 --
                 -- /Last point:/ The last point of @y@ is the starting point of @x@.
  | Line TPath TPoint -- ^ Let @y = Line x p@.
                      --
                      -- /Operation:/ Extend the current path from the last point of @x@
                      -- in a straight line to @p@.
                      --
                      -- /Last point:/ The last point of @y@ is @p@.
  | Rectangle TPath TPoint -- ^ Let @y = Rectangle x p@.
                           --
                           -- /Operation:/ Define a rectangle using the last point of
                           -- @x@ as one corner and @p@ as the another corner.
                           --
                           -- /Last point:/ The last point of @y@ is @p@.
  | Circle TPath Double -- ^ Let @y = Circle x r@.
                        --
                        -- /Operation:/ Define a circle with center at the last point
                        -- of x and radius @r@.
                        --
                        -- /Last point:/ The last point of @y@ is the same as the last
                        -- point of @x@.
  | Ellipse TPath Double Double -- ^ Let @y = Ellipse x r1 r2@.
                                --
                                -- /Operation:/ Define a ellipse with center at the last
                                -- point of @x@, width the double of @r1@ and height
                                -- the double of @r2@.
                                --
                                -- /Last point:/ The last point of @y@ is the same as the
                                -- last point of @x@.
  | Grid TPath [GridOption] TPoint
  | Node TPath LaTeX -- ^ Let @y = Node x l@.
                     --
                     -- /Operation:/ Set a text centered at the last point of @x@.
                     --
                     -- /Last point:/ The last point of @y@ is the same as the last
                     -- point of @x@.
    deriving Show

newtype GridOption =
   GridStep Step
   deriving Show

data Step =
   DimStep Measure
 | XYStep Double
 | PointStep TPoint
   deriving Show

instance Render TPath where
 render (Start p) = render p
 render (Cycle p) = render p <> " -- cycle"
 render (Line p1 p2) = render p1 <> " -- " <> render p2
 render (Rectangle p1 p2) = render p1 <> " rectangle " <> render p2
 render (Circle p r) = render p <> " circle (" <> render r <> ")"
 render (Ellipse p r1 r2) = render p <> " ellipse (" <> render r1 <> " and " <> render r2 <> ")"
 render (Grid p1 [] p2) = render p1 <> " grid " <> render p2
 render (Grid p1 xs p2) = render p1 <> " grid " <> render xs <> " " <> render p2
 render (Node p l) = render p <> " node[transform shape] " <> render (TeXBraces l)

instance Render GridOption where
 render (GridStep s) = "step=" <> render s

instance Render Step where
 render (DimStep m) = render m
 render (XYStep q) = render q
 render (PointStep p) = render p

-- Starting and Last points

-- | Calculate the starting point of a 'TPath'.
startingPoint :: TPath -> TPoint
startingPoint (Start p) = p
startingPoint (Cycle x) = startingPoint x
startingPoint (Line x _) = startingPoint x
startingPoint (Rectangle x _) = startingPoint x
startingPoint (Circle x _) = startingPoint x
startingPoint (Ellipse x _ _) = startingPoint x
startingPoint (Grid x _ _) = startingPoint x
startingPoint (Node x _) = startingPoint x

-- | Calculate the last point of a 'TPath'.
lastPoint :: TPath -> TPoint
lastPoint (Start p) = p
lastPoint (Cycle x) = startingPoint x
lastPoint (Line _ p) = p
lastPoint (Rectangle _ p) = p
lastPoint (Circle x _) = lastPoint x
lastPoint (Ellipse x _ _) = lastPoint x
lastPoint (Grid _ _ p) = p
lastPoint (Node x _) = lastPoint x

-- Path builders

-- | Alias of 'Line'.
(->-) :: TPath -> TPoint -> TPath
(->-) = Line

-- Parameters

-- | Color models accepted by Ti/k/Z.
data TikZColor =
   BasicColor Color
 | RGBColor Word8 Word8 Word8
   deriving Show

instance Render TikZColor where
  render (BasicColor c) = render c
  render (RGBColor r g b) = "{rgb,255:red," <> render r <> ";green," <> render g <> ";blue," <> render b <> "}"

-- | Parameters to use in a 'scope' to change how things
--   are rendered within that scope.
data Parameter =
   TWidth Measure
 | TColor TikZColor
 | TScale Double
 | TRotate Double -- ^ Angle is in degrees.
     deriving Show

renderPair :: Render a => Text -> a -> Text
renderPair x y = x <> "=" <> render y

instance Render Parameter where
 render (TWidth m)  = renderPair "line width" m
 render (TColor c)  = renderPair "color" c
 render (TScale q)  = renderPair "scale" q
 render (TRotate a) = renderPair "rotate" a

-- TikZ

-- | A Ti/k/Z script.
data TikZ =
    PathAction [ActionType] TPath
  | Scope [Parameter] TikZ
  | TikZSeq (S.Seq TikZ)
    deriving Show

-- | Different types of actions that can be performed
--   with a 'TPath'. See 'path' for more information.
data ActionType = Draw | Fill | Clip | Shade deriving Show

-- | Just an empty script.
emptytikz :: TikZ
emptytikz = TikZSeq mempty

instance Render TikZ where
 render (PathAction ts p) = "\\path" <> render ts <> " " <> render p <> " ; "
 render (Scope ps t) = "\\begin{scope}" <> render ps <> render t <> "\\end{scope}"
 render (TikZSeq ts) = foldMap render ts

instance Render ActionType where
 render Draw  = "draw"
 render Fill  = "fill"
 render Clip  = "clip"
 render Shade = "shade"

-- | A path can be used in different ways.
--
-- * 'Draw': Just draw the path.
--
-- * 'Fill': Fill the area inside the path.
--
-- * 'Clip': Clean everything outside the path.
--
-- * 'Shade': Shade the area inside the path.
--
--   It is possible to stack different effects in the list.
--
--   Example of usage:
--
-- > path [Draw] $ Start (pointAtXY 0 0) ->- pointAtXY 1 1
--
--   Most common usages are exported as functions. See
--   'draw', 'fill', 'clip', 'shade', 'filldraw' and
--   'shadedraw'.
path :: [ActionType] -> TPath -> TikZ
path = PathAction

-- | Applies a scope to a Ti/k/Z script.
scope :: [Parameter] -> TikZ -> TikZ
scope = Scope

-- | Sequence two Ti/k/Z scripts.
(->>) :: TikZ -> TikZ -> TikZ
(TikZSeq s1) ->> (TikZSeq s2) = TikZSeq (s1 <> s2)
(TikZSeq s) ->> a = TikZSeq $ s S.|> a
a ->> (TikZSeq s) = TikZSeq $ a S.<| s
a ->> b = TikZSeq $ a S.<| S.singleton b

-- SUGAR

-- | Equivalent to @path [Draw]@.
draw :: TPath -> TikZ
draw = path [Draw]

-- | Equivalent to @path [Fill]@.
fill :: TPath -> TikZ
fill = path [Fill]

-- | Equivalent to @path [Clip]@.
clip :: TPath -> TikZ
clip = path [Clip]

-- | Equivalent to @path [Shade]@.
shade :: TPath -> TikZ
shade = path [Shade]

-- | Equivalent to @path [Fill,Draw]@.
filldraw :: TPath -> TikZ
filldraw = path [Fill,Draw]

-- | Equivalent to @path [Shade,Draw]@.
shadedraw :: TPath -> TikZ
shadedraw = path [Shade,Draw]
