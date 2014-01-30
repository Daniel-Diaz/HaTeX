
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.Geometry (
    -- * Geometry package
    geometry
    -- * Geometry options
  , GeometryOption (..)
  , applyGeometry
  ) where

import Text.LaTeX
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class

-- | Geometry package. Use it to import it like this:
--
-- > usepackage [] geometry
--
geometry :: PackageName
geometry = "geometry"

-- | Options of the geometry package.
data GeometryOption = 
    GHeight Measure
  | GWidth  Measure
  | GPaper PaperType
  | GCentered
  | GPaperHeight Measure
  | GPaperWidth Measure
  | GLandscape Bool
    deriving Show

renderOption :: Render a => Text -> a -> Text
renderOption t x = t <> "=" <> render x

instance Render GeometryOption where
  render (GHeight m) = renderOption "height" m
  render (GWidth  m) = renderOption "width"  m
  render (GPaper  p) = render (Paper p)
  render GCentered   = "centered"
  render (GPaperHeight m) = renderOption "paperheight" m
  render (GPaperWidth m) = renderOption "paperwidth" m
  render (GLandscape b) = renderOption "landscape" b

applyGeometry :: LaTeXC l => [GeometryOption] -> l
applyGeometry opts = fromLaTeX $ TeXComm "geometry" [FixArg $ raw $ renderCommas opts]
