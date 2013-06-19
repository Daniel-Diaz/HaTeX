
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.TikZ.Syntax (
    -- * Points
    TPoint
  , pointAt , pointAtXY , pointAtXYZ
  , relPoint , relPoint_
    -- * Paths
  , TPath
  , vertex
  , (->-)
  , tcycle
    -- * Parameters
  , Parameter (..)
    -- * TikZ
  , TikZ
  , path
  , scope
  , ActionType (..)
  , (->>)
    -- * Sugar
  , draw , fill , clip
  , filldraw
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
    Vertex TPoint
  | Cycle
  | Line TPath TPath
  | Rectangle
  | Circle
  | Ellipse
  | Grid
    deriving Show

instance Render TPath where
 render (Vertex p) = render p
 render (Line p1 p2) = render p1 <> " -- " <> render p2
 render Cycle = "cycle"
 render Rectangle = "rectangle"
 render Circle = "circle"
 render Ellipse = "ellipse"
 render Grid = "grid"

vertex :: TPoint -> TPath
vertex = Vertex

tcycle :: TPath
tcycle = Cycle

(->-) :: TPath -> TPath -> TPath
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

data ActionType = Draw | Fill | Clip deriving Show

instance Render TikZ where
 render (PathAction ts p) = "\\path[" <> renderCommas ts <> "] " <> render p <> " ; "
 render (TikZSeq as) = foldMap render as

instance Render ActionType where
 render Draw = "draw"
 render Fill = "fill"
 render Clip = "clip"

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

filldraw :: TPath -> TikZ
filldraw = path [Fill,Draw]
