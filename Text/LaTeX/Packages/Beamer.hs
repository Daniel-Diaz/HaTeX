{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HATEX MakeMonadic #-}

module Text.LaTeX.Packages.Beamer
 ( -- * Beamer package
   beamer
   -- * Beamer commands
 , frame
 , frametitle
 , framesubtitle
 , alert
 , pause
 , block
   -- ** Overlay Specifications
 , OverlaySpec (..)
 , beameritem
 , uncover
 , only
   -- ** Themes
 , Theme (..)
 , usetheme
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Data.String

-- | The 'beamer' document class. Importing a package is not required. Example:
--
-- > documentclass [] beamer
beamer :: String
beamer = "beamer"

-- | A presentation is composed of a sequence of frames. Each frame is created with this function.
frame :: LaTeX -> LaTeX
frame = TeXEnv "frame" []

-- | Set the title of the current frame. Use it within a 'frame'.
frametitle :: LaTeX -> LaTeX
frametitle l = TeXComm "frametitle" [FixArg l]

-- | Set the subtitle of the current frame. Use it within a 'frame'.
framesubtitle :: LaTeX -> LaTeX
framesubtitle l = TeXComm "framesubtitle" [FixArg l]

-- | Highlight in red a piece text. With the 'OverlaySpec's, you can specify the slides where
-- the text will be highlighted.
alert :: [OverlaySpec] -> LaTeX -> LaTeX
alert os l = TeXComm "alert" [ MSymArg $ fmap (TeXRaw . render) os, FixArg l]

-- | Introduces a pause in a slide.
pause :: LaTeX
pause = TeXComm "pause" []

-- | 'beameritem' works like 'item', but allows you to specify the slides where
-- the item will be displayed.
beameritem :: [OverlaySpec] -> LaTeX
beameritem os = TeXComm "item" [ MSymArg $ fmap (TeXRaw . render) os ]

-- | With 'uncover', show a piece of text only in the slides you want.
uncover :: [OverlaySpec] -> LaTeX -> LaTeX
uncover os l = TeXComm "uncover" [ MSymArg $ fmap (TeXRaw . render) os , FixArg l ]

-- | Similar to 'uncover'.
only :: [OverlaySpec] -> LaTeX -> LaTeX
only os l = TeXComm "only" [ MSymArg $ fmap (TeXRaw . render) os , FixArg l ]

data OverlaySpec =
   OneSlide Int
 | FromSlide Int
 | ToSlide Int
 | FromToSlide Int Int
   deriving Show

instance Render OverlaySpec where
 render (OneSlide n)      = render n
 render (FromSlide n)     = render n <> "-"
 render (ToSlide n)       = "-" <> render n
 render (FromToSlide n m) = render n <> "-" <> render m

-- | A 'block' will be displayed surrounding a text.
block :: LaTeX -- ^ Title for the block
      -> LaTeX -- ^ Content of the block
      -> LaTeX -- ^ Result
block tit = TeXEnv "block" [ FixArg tit ]

-- THEMES --

--TODO: Add a short description of each theme.

-- | A 'Theme' of a presentation. See 'usetheme'.
data Theme =
   AnnArbor
 | Antibes
 | Bergen
 | Berkeley
 | Berlin
 | Boadilla
 | Boxes
 | CambridgeUS
 | Copenhagen
 | Darmstadt
 | Default
 | Dresden
 | Frankfurt
 | Goettingen
 | Hannover
 | Ilmenau
 | JuanLesPins
 | Luebeck
 | Madrid
 | Malmoe
 | Marburg
 | Montpellier
 | PaloAlto
 | Pittsburgh
 | Rochester
 | Singapore
 | Szeged
 | Warsaw
   --
 | CustomTheme String
  deriving Show

instance Render Theme where
 render (CustomTheme str) = fromString str
 render x = fromString $ show x

-- | Set the 'Theme' employed in your presentation (in the preamble).
usetheme :: Theme -> LaTeX
usetheme th = TeXComm "usetheme" [ FixArg $ TeXRaw $ render th ]

