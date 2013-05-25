
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.Color
 ( -- * Color package
   pcolor
   -- * Package options
 , monochrome
 , dvipsnames
 , nodvipsnames
 , usenames
   -- * Types
 , Color (..)
 , ColorName (..)
 , ColorModel (..)
 , ColSpec (..)
   -- * Commands
 , pagecolor
 , color
 , textcolor
 , colorbox , fcolorbox
 , normalcolor
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
--
import Data.Monoid

-- | The 'pcolor' package.
--
-- > usepackage [] pcolor
pcolor :: PackageName
pcolor = "color"

-- | To convert all colour commands to black and white,
--   for previewers that cannot handle colour.
monochrome :: LaTeXC l => l
monochrome = "monochrome"

dvipsnames :: LaTeXC l => l
dvipsnames = "dvipsnames"

nodvipsnames :: LaTeXC l => l
nodvipsnames = "nodvipsnames"

usenames :: LaTeXC l => l
usenames = "usenames"

--

data ColSpec =
   DefColor Color
 | ModColor ColorModel
 | DvipsColor ColorName
   deriving Show

data Color =
   Red
 | Green
 | Blue
 | Yellow
 | Cyan
 | Magenta
 | Black
 | White
   deriving Show

data ColorModel =
   RGB Float Float Float
 | RGB255 Int Int Int
 | GrayM Float
 | HTML String
 | CMYK Float Float Float Float
   deriving Show

data ColorName =
   Apricot       | Aquamarine  | Bittersweet
 | BlueGreen     | BlueViolet  | BrickRed
 | Brown         | BurntOrange | CadetBlue
 | CarnationPink | Cerulean    | CornflowerBlue
 | Dandelion     | DarkOrchid  | Emerald
 | ForestGreen   | Fuchsia     | Goldenrod
 | Gray          | GreenYellow | JungleGreen
 | Lavender      | LimeGreen   | Mahogany
 | Maroon        | Melon       | MidnightBlue
 | Mulberry      | NavyBlue    | OliveGreen
 | Orange        | OrangeRed   | Orchid
 | Peach         | Periwinkle  | PineGreen
 | Plum          | ProcessBlue | Purple
 | RawSienna     | RedOrange   | RedViolet
 | Rhodamine     | RoyalBlue   | RubineRed
 | Salmon        | SeaGreen    | Sepia
 | SkyBlue       | SpringGreen | Tan
 | TealBlue      | Thistle     | Turquoise
 | Violet        | VioletRed   | WildStrawberry
 | YellowGreen   | YellowOrange
   deriving Show

instance Render Color where
 render = fromString . show

instance Render ColorModel where
 render (RGB r g b) = mconcat [ "[rgb]{" , renderCommas [r,g,b] , "}" ]
 render (RGB255 r g b) = mconcat [ "[RGB]{" , renderCommas [r,g,b] , "}" ]
 render (GrayM k) = mconcat [ "[gray]{" , render k , "}"]
 render (HTML str) = mconcat [ "[HTML]{" , fromString str , "}" ]
 render (CMYK c m y k) = mconcat [ "[cmyk]{" , renderCommas [c,m,y,k] , "}" ]

instance Render ColorName where
 render = fromString . show

instance Render ColSpec where
 render (DefColor c)   = mconcat [ "{" , render c , "}" ]
 render (ModColor cm)  = render cm
 render (DvipsColor c) = mconcat [ "{" , render c , "}" ]

-- Commands

-- | Set the background color for the current and following pages.
pagecolor :: LaTeXC l => ColSpec -> l
pagecolor = (commS "pagecolor" <>) . rendertex

-- | Switch to a new text color.
color :: LaTeXC l => ColSpec -> l
color = (commS "color" <>) . rendertex

-- | Set the text of its argument in the given colour.
textcolor :: LaTeXC l => ColSpec -> l -> l
textcolor cs l = commS "textcolor" <> rendertex cs
              <> braces l

-- | Put its argument in a box with the given colour as background.
colorbox :: LaTeXC l => ColSpec -> l -> l
colorbox cs l = commS "colorbox" <> rendertex cs
             <> braces l

-- | Application of @fcolorbox cs1 cs2 l@ put @l@ in a framed box with
--   @cs1@ as frame color and @cs2@ as background color.
fcolorbox :: LaTeXC l => ColSpec -> ColSpec -> l -> l
fcolorbox cs1 cs2 l =
    commS "fcolorbox" <> rendertex cs1
                      <> rendertex cs2
 <> braces l

-- | Switch to the colour that was active at the end of the preamble.
--   Thus, placing a 'color' command in the preamble can change the
--   standard colour of the whole document.
normalcolor :: LaTeXC l => l
normalcolor = comm0 "normalcolor"
