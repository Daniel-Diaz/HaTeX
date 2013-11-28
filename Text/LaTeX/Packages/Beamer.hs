{-# LANGUAGE OverloadedStrings #-}

-- | Beamer is a LaTeX package for the creation of slides.
--
--   Each frame is contained within the 'frame' function. Here is an example:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Text.LaTeX
-- > import Text.LaTeX.Packages.Beamer
-- >
-- > mySlides :: Monad m => LaTeXT m ()
-- > mySlides = do
-- >   frame $ do
-- >     frametitle "First frame"
-- >     "Content of the first frame."
-- >   frame $ do
-- >     frametitle "Second frame"
-- >     "Content of the second frame." 
-- >     pause
-- >     " And actually a little more."
--
--   The 'pause' command in the second frame makes the second part of the text
--   to appear one screen later.
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
 , usetheme
 , Theme (..)
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types

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

-- | A 'Theme' of a presentation. See 'usetheme'.
--   A preview of each one is given below.
data Theme = 
    AnnArbor -- ^ <<docfiles/beamers/previewAnnArbor.png>>
  | Antibes -- ^ <<docfiles/beamers/previewAntibes.png>>
  | Bergen -- ^ <<docfiles/beamers/previewBergen.png>>
  | Berkeley -- ^ <<docfiles/beamers/previewBerkeley.png>>
  | Berlin -- ^ <<docfiles/beamers/previewBerlin.png>>
  | Boadilla -- ^ <<docfiles/beamers/previewBoadilla.png>>
  | CambridgeUS -- ^ <<docfiles/beamers/previewCambridgeUS.png>>
  | Copenhagen -- ^ <<docfiles/beamers/previewCopenhagen.png>>
  | Darmstadt -- ^ <<docfiles/beamers/previewDarmstadt.png>>
  | Dresden -- ^ <<docfiles/beamers/previewDresden.png>>
  | Frankfurt -- ^ <<docfiles/beamers/previewFrankfurt.png>>
  | Goettingen -- ^ <<docfiles/beamers/previewGoettingen.png>>
  | Hannover -- ^ <<docfiles/beamers/previewHannover.png>>
  | Ilmenau -- ^ <<docfiles/beamers/previewIlmenau.png>>
  | JuanLesPins -- ^ <<docfiles/beamers/previewJuanLesPins.png>>
  | Luebeck -- ^ <<docfiles/beamers/previewLuebeck.png>>
  | Madrid -- ^ <<docfiles/beamers/previewMadrid.png>>
  | Malmoe -- ^ <<docfiles/beamers/previewMalmoe.png>>
  | Marburg -- ^ <<docfiles/beamers/previewMarburg.png>>
  | Montpellier -- ^ <<docfiles/beamers/previewMontpellier.png>>
  | PaloAlto -- ^ <<docfiles/beamers/previewPaloAlto.png>>
  | Pittsburgh -- ^ <<docfiles/beamers/previewPittsburgh.png>>
  | Rochester -- ^ <<docfiles/beamers/previewRochester.png>>
  | Singapore -- ^ <<docfiles/beamers/previewSingapore.png>>
  | Szeged -- ^ <<docfiles/beamers/previewSzeged.png>>
  | Warsaw -- ^ <<docfiles/beamers/previewWarsaw.png>>
    --
  | Boxes
  | Default
    --
  | CustomTheme String
    deriving (Eq,Show)

instance Render Theme where
 render (CustomTheme str) = fromString str
 render x = fromString $ show x

-- | Set the 'Theme' employed in your presentation (in the preamble).
usetheme :: LaTeXC l => Theme -> l
usetheme th = fromLaTeX $ TeXComm "usetheme" [ FixArg $ TeXRaw $ render th ]

