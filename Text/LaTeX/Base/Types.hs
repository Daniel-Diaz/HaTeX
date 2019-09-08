
{-# LANGUAGE OverloadedStrings #-}

-- | Some types shared along the library.
module Text.LaTeX.Base.Types (
   ClassName
 , PackageName
 , PageStyle
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

-- | Page styles are represented by a 'String'.
type PageStyle = String

-- | Type of labels.
newtype Label =
    Label {
      -- | Get the name of a label.
      labelName :: String
    } deriving (Eq, Show)

-- | Create a label from its name.
createLabel :: String -> Label
createLabel = Label

instance Render Label where
    render (Label str) = fromString str

instance IsString Label where
    fromString = createLabel

-- | Vertical position. 'Here' and 'ForcePos' are used with @table@ environments.
data Pos = Bottom | Center | Top | Here | ForcePos deriving Show

instance Render Pos where
 render Bottom   = "b"
 render Center   = "c"
 render Top      = "t"
 render Here     = "h"
 render ForcePos = "!"

-- | Horizontal position.
data HPos = HLeft | HCenter | HRight deriving Show

instance Render HPos where
 render HLeft   = "l"
 render HCenter = "c"
 render HRight  = "r"

-- | Type of table specifications.
data TableSpec =
   LeftColumn         -- ^ Left-justified column.
 | CenterColumn       -- ^ Centered column.
 | RightColumn        -- ^ Right-justified column.
 | ParColumnTop LaTeX -- ^ Paragraph column with text vertically aligned at the top.
 | ParColumnMid LaTeX -- ^ Paragraph column with text vertically aligned at the middle. Requires 'array' package.
 | ParColumnBot LaTeX -- ^ Paragraph column with text vertically aligned at the bottom. Requires 'array' package.
 | NameColumn String  -- ^ User defined column. Requires 'array' package.
 | BeforeColumn LaTeX -- ^ Can be used before a 'LeftColumn', 'CenterColumn', 'RightColumn', 'ParColumnTop', 'ParColumnMid' or a 'ParColumnBot' specification. Inserts the code directly in front of the entry of the column. Requires 'array' package.
 | AfterColumn LaTeX  -- ^ Can be used after a 'LeftColumn', 'CenterColumn', 'RightColumn', 'ParColumnTop', 'ParColumnMid' or a 'ParColumnBot' specification. Inserts the code directly in front of the entry of the column. Requires 'array' package.
 | VerticalLine       -- ^ Vertical line between two columns.
 | DVerticalLine      -- ^ Double vertical line between two columns.
 | Separator LaTeX    -- ^ Column separator. Requires 'array' package.
   deriving Show

instance Render TableSpec where
 render LeftColumn       = "l"
 render CenterColumn     = "c"
 render RightColumn      = "r"
 render (ParColumnTop l) = "p" <> render (FixArg l)
 render (ParColumnMid l) = "m" <> render (FixArg l)
 render (ParColumnBot l) = "b" <> render (FixArg l)
 render (NameColumn n)   = fromString n
 render (BeforeColumn l) = ">{" <> render l <> "}"
 render (AfterColumn l)  = "<{" <> render l <> "}"
 render VerticalLine     = "|"
 render DVerticalLine    = "||"
 render (Separator l)    = "@" <> render (FixArg l)
