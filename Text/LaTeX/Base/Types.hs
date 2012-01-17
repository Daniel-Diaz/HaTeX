
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Base.Types (
   Label
 , createLabel , labelName
 , Pos (..)
 , TableSpec (..)
 , Measure (..)
 ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render

newtype Label = Label String deriving (Eq, Show)

createLabel :: String -> Label
createLabel = Label

labelName :: Label -> String
labelName (Label str) = str

instance Render Label where
 render (Label str) = fromString str

instance IsString Label where
 fromString = createLabel

data Pos = Bottom | Center | Top deriving Show

instance Render Pos where
 render Bottom = "b"
 render Center = "c"
 render Top    = "t"

data TableSpec =
   LeftColumn         -- ^ Left-justified column.
 | CenterColumn       -- ^ Centered column.
 | RightColumn        -- ^ Right-justified column.
 | ParColumnTop LaTeX -- ^ Paragraph column with text vertically aligned at the top.
 | ParColumnMid LaTeX -- ^ Paragraph column with text vertically aligned at the middle. Requires 'array' package.
 | ParColumnBot LaTeX -- ^ Paragraph column with text vertically aligned at the bottom. Requires 'array' package.
 | VerticalLine       -- ^ Vertical line between two columns.
 | DVerticalLine      -- ^ Double vertical line between two columns.
   deriving Show

instance Render TableSpec where
 render LeftColumn       = "l"
 render CenterColumn     = "c"
 render RightColumn      = "r"
 render (ParColumnTop l) = "p" <> render (FixArg l)
 render (ParColumnMid l) = "m" <> render (FixArg l)
 render (ParColumnBot l) = "b" <> render (FixArg l)
 render VerticalLine     = "|"
 render DVerticalLine    = "||"

data Measure =
   Pt Int   -- ^ A point is 1/72.27 inch, that means about 0.0138 inch or 0.3515 mm.
 | Mm Float -- ^ Millimeter.
 | Cm Float -- ^ Centimeter.
 | In Float -- ^ Inch.
 | Ex Float -- ^ The height of an \"x\" in the current font.
 | Em Float -- ^ The width of an \"M\" in the current font.
 | CustomMeasure LaTeX -- ^ You can introduce a 'LaTeX' expression as a measure.
   deriving Show

instance Render Measure where
 render (Pt x) = render x <> "pt"
 render (Mm x) = render x <> "mm"
 render (Cm x) = render x <> "cm"
 render (In x) = render x <> "in"
 render (Ex x) = render x <> "ex"
 render (Em x) = render x <> "em"
 render (CustomMeasure x) = render x

