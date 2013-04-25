{-# LANGUAGE OverloadedStrings #-}

-- | Beamer is a LaTeX package for the creation of slides.
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
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Data.String

-- | The 'beamer' document class. Importing a package is not required. Example:
--
-- > documentclass [] beamer
beamer :: ClassName
beamer = "beamer"

-- | A presentation is composed of a sequence of frames. Each frame is created with this function.
frame :: LaTeXC l => l -> l
frame = liftL $ TeXEnv "frame" []

-- | Set the title of the current frame. Use it within a 'frame'.
frametitle :: LaTeXC l => l -> l
frametitle = liftL $ \l -> TeXComm "frametitle" [FixArg l]

-- | Set the subtitle of the current frame. Use it within a 'frame'.
framesubtitle :: LaTeXC l => l -> l
framesubtitle = liftL $ \l -> TeXComm "framesubtitle" [FixArg l]

-- | Highlight in red a piece of text. With the 'OverlaySpec's, you can specify the slides where
-- the text will be highlighted.
alert :: LaTeXC l => [OverlaySpec] -> l -> l
alert os = liftL $ \l -> TeXComm "alert" [ MSymArg $ fmap (TeXRaw . render) os, FixArg l]

-- | Introduces a pause in a slide.
pause :: LaTeXC l => l
pause = comm0 "pause"

-- | 'beameritem' works like 'item', but allows you to specify the slides where
-- the item will be displayed.
beameritem :: LaTeXC l => [OverlaySpec] -> l
beameritem os = fromLaTeX $ TeXComm "item" [ MSymArg $ fmap (TeXRaw . render) os ]

-- | With 'uncover', show a piece of text only in the slides you want.
uncover :: LaTeXC l => [OverlaySpec] -> l -> l
uncover os = liftL $ \l -> TeXComm "uncover" [ MSymArg $ fmap (TeXRaw . render) os , FixArg l ]

-- TODO: What is the difference between 'uncover' and 'only'??

-- | Similar to 'uncover'.
only :: LaTeXC l => [OverlaySpec] -> l -> l
only os = liftL $ \l -> TeXComm "only" [ MSymArg $ fmap (TeXRaw . render) os , FixArg l ]

-- | Specifications for beamer functions.
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
block :: LaTeXC l =>
         l -- ^ Title for the block
      -> l -- ^ Content of the block
      -> l -- ^ Result
block = liftL2 $ \tit -> TeXEnv "block" [ FixArg tit ]

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
usetheme :: LaTeXC l => Theme -> l
usetheme th = fromLaTeX $ TeXComm "usetheme" [ FixArg $ TeXRaw $ render th ]

