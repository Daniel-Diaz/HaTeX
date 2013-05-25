
{-# LANGUAGE OverloadedStrings #-}

-- | Some types shared along the library.
module Text.LaTeX.Base.Types (
   ClassName
 , PackageName
 , Label
 , createLabel , labelName
 , Pos (..) , HPos (..)
 , TableSpec (..)
 , Measure (..)
 ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render

-- | Class names are represented by a 'String'.
type ClassName = String

-- | Package names are represented by a 'String'.
type PackageName = String

newtype Label = Label String deriving (Eq, Show)

-- | Create a label from its name.
createLabel :: String -> Label
createLabel = Label

-- | Get the name of a label.
labelName :: Label -> String
labelName (Label str) = str

instance Render Label where
 render (Label str) = fromString str

instance IsString Label where
 fromString = createLabel

-- | Vertical position.
data Pos = Bottom | Center | Top deriving Show

instance Render Pos where
 render Bottom = "b"
 render Center = "c"
 render Top    = "t"

-- | Horizontal position.
data HPos = HLeft | HCenter | HRight deriving Show

instance Render HPos where
 render HLeft   = "l"
 render HCenter = "c"
 render HRight  = "r"

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