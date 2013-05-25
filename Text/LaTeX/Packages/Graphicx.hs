
{-# LANGUAGE OverloadedStrings #-}

-- | This module allows you to use the LaTeX graphicx library in order to
--   insert graphics in a document.
module Text.LaTeX.Packages.Graphicx
 ( -- * Graphicx package
   graphicx
   -- * Package options
 , dvips
 , dvipdfm
 , pdftex
   -- * Including graphics
 , IGOption (..)
 , includegraphics
   -- * Transformations
 , rotatebox
 , scalebox
 , reflectbox
 , resizebox
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
--
import Data.Char (toLower)

-- | The 'graphicx' package.
--
-- > usepackage [] graphicx
graphicx :: PackageName
graphicx = "graphicx"

-- Package options

dvips, dvipdfm, pdftex :: LaTeXC l => l
dvips = "dvips"
dvipdfm = "dvipdfm"
pdftex = "pdftex"

-- Including graphics

-- | Include Graphics Option. These options can be passed as arguments to the 'includegraphics' function.
data IGOption =
   IGWidth Measure -- ^ Specify the preferred width of the imported image.
 | IGHeight Measure -- ^ Specify the preferred height of the imported image.
 | KeepAspectRatio Bool -- ^ When 'True', it will scale the image according to both 'IGWidth' and 'IGHeight'
                        -- , but will not distort the image, so that neither 'IGWidth' nor 'IGHeight' are exceeded.
 | IGScale Float -- ^ Scales the image by the desired scale factor.
 | IGAngle Int -- ^ Rotate the image by given degrees.
 | IGTrim Measure Measure Measure Measure -- ^ This option will crop the imported image. Arguments are from-left
                                          -- , from-bottom, from-right and from-top respectively.
 | IGClip Bool -- ^ For the 'IGTrim' option to work, you must set 'IGClip' to 'True'.
 | IGPage Int -- ^ If the image file is a pdf file with multiple pages,
              --   this parameter allows you to use a different page than the first.
   deriving Show

instance Render IGOption where
 render (IGWidth m) = "width=" <> render m
 render (IGHeight m) = "height=" <> render m
 render (KeepAspectRatio b) = "keepaspectratio=" <> fromString (fmap toLower $ show b)
 render (IGScale r) = "scale=" <> render r
 render (IGAngle a) = "angle=" <> render a
 render (IGTrim l b r t) = "trim=" <> renderChars ' ' [l,b,r,t]
 render (IGClip b) = "clip=" <> fromString (fmap toLower $ show b)
 render (IGPage p) = "page=" <> render p

-- | Include an image in the document.
includegraphics :: LaTeXC l =>
                  [IGOption] -- ^ Options
                -> FilePath  -- ^ Image file
                -> l
includegraphics opts fp = fromLaTeX $ TeXComm "includegraphics"
 [ MOptArg $ fmap rendertex opts , FixArg $ TeXRaw $ fromString fp ]

rotatebox :: LaTeXC l => Float -> l -> l
rotatebox a = liftL $ \l -> TeXComm "rotatebox" [FixArg $ rendertex a , FixArg l]

scalebox :: LaTeXC l =>
     Float       -- ^ Horizontal scale.
  -> Maybe Float -- ^ Vertical scale.
  -> l
  -> l
scalebox h mv = liftL $ \l ->
  TeXComm "rotatebox" $ [FixArg $ rendertex h]
                     ++  maybe [] ((:[]) . OptArg . rendertex) mv
                     ++ [FixArg l]

reflectbox :: LaTeXC l => l -> l
reflectbox = liftL $ \l -> TeXComm "reflectbox" [FixArg l]

resizebox :: LaTeXC l =>
     Measure -- ^ Horizontal size.
  -> Measure -- ^ Vertical size.
  -> l
  -> l
resizebox h v = liftL $ \l ->
  TeXComm "resizebox" [FixArg $ rendertex h,FixArg $ rendertex v,FixArg l]