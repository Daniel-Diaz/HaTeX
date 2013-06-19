
{-# LANGUAGE OverloadedStrings #-}

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
    -- ** Functions
  , (->-)
    -- * Parameters
  , Parameter (..)
    -- * TikZ
  , TikZ
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
import Text.LaTeX.Packages.Color
--
import Data.Monoid
import Data.Foldable (foldMap)
import qualified Data.Sequence as S

-- POINTS

-- | A point in TikZ.
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

pointAt :: Measure -> Measure -> TPoint
pointAt = DimPoint

pointAtXY :: Double -> Double -> TPoint
pointAtXY = XYPoint

pointAtXYZ :: Double -> Double -> Double -> TPoint
pointAtXYZ = XYZPoint

relPoint :: TPoint -> TPoint
relPoint (RelPoint x) = RelPoint x
relPoint (RelPoint_ x) = RelPoint x
relPoint p = RelPoint p

relPoint_ :: TPoint -> TPoint
relPoint_ (RelPoint x) = RelPoint_ x
relPoint_ (RelPoint_ x) = RelPoint_ x
relPoint_ p = RelPoint_ p

-- PATHS

-- | Type for TikZ paths.
data TPath =
    Start TPoint -- ^ The starting point of a path.
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
    deriving Show

data GridOption =
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
 render (Grid p1 xs p2) = render p1 <> " grid " <> render xs <> " " <> render p2

instance Render GridOption where
 render (GridStep s) = "step=" <> render s

instance Render Step where
 render (DimStep m) = render m
 render (XYStep q) = render q
 render (PointStep p) = render p

-- Path builders

(->-) :: TPath -> TPoint -> TPath
(->-) = Line

-- Parameters

data Parameter =
   TWidth Measure
 | TColor Color
     deriving Show

renderPair :: Render a => Text -> a -> Text
renderPair x y = x <> "=" <> render y

instance Render Parameter where
 render (TWidth m) = renderPair "line width" m
 render (TColor c) = renderPair "color" c

-- TikZ

data TikZ =
    PathAction [ActionType] TPath
  | Scope [Parameter] TikZ
  | TikZSeq (S.Seq TikZ)
    deriving Show

data ActionType = Draw | Fill | Clip | Shade deriving Show

instance Render TikZ where
 render (PathAction ts p) = "\\path" <> render ts <> " " <> render p <> " ; "
 render (Scope ps t) = "\\begin{scope}" <> render ps <> render t <> "\\end{scope}"
 render (TikZSeq ts) = foldMap render ts

instance Render ActionType where
 render Draw  = "draw"
 render Fill  = "fill"
 render Clip  = "clip"
 render Shade = "shade"

path :: [ActionType] -> TPath -> TikZ
path = PathAction

scope :: [Parameter] -> TikZ -> TikZ
scope = Scope

(->>) :: TikZ -> TikZ -> TikZ
(TikZSeq s1) ->> (TikZSeq s2) = TikZSeq (s1 <> s2)
(TikZSeq s) ->> a = TikZSeq $ s S.|> a
a ->> (TikZSeq s) = TikZSeq $ a S.<| s
a ->> b = TikZSeq $ a S.<| S.singleton b

-- SUGAR

draw :: TPath -> TikZ
draw = path [Draw]

fill :: TPath -> TikZ
fill = path [Fill]

clip :: TPath -> TikZ
clip = path [Clip]

shade :: TPath -> TikZ
shade = path [Shade]

filldraw :: TPath -> TikZ
filldraw = path [Fill,Draw]

shadedraw :: TPath -> TikZ
shadedraw = path [Shade,Draw]
