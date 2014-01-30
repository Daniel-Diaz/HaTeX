
{-# LANGUAGE OverloadedStrings #-}

-- | The geometry package provides an easy interface to page dimensions.
--
-- CTAN page for geometry: <http://www.ctan.org/pkg/geometry>.
module Text.LaTeX.Packages.Geometry (
    -- * Geometry package
    geometry
  , importGeometry
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
-- In most cases, it is recommended to use 'importGeometry' instead.
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

-- | Apply the given geometry options to the document.
applyGeometry :: LaTeXC l => [GeometryOption] -> l
applyGeometry opts = fromLaTeX $ TeXComm "geometry" [FixArg $ raw $ renderCommas opts]

-- | Import the geometry package with additional options.
importGeometry :: LaTeXC l => [GeometryOption] -> l
importGeometry opts = usepackage (fmap rendertex opts) geometry
