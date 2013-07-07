
-- | This package provides extensive control of page headers and footers.
--
-- CTAN page for fancyhdr: <http://www.ctan.org/pkg/fancyhdr>.
module Text.LaTeX.Packages.Fancyhdr (
   -- * fancyhdr package
   fancyhdr
   -- * Simple interface
 , HdrSettings (..)
 , defaultHdrSettings
 , applyHdrSettings
   -- * Raw interface
 , fancy
 , lhead, chead, rhead
 , lfoot, cfoot, rfoot
 , renewheadrulewidth
 , renewfootrulewidth
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Texy

-- | The fancyhdr package.
--   Please, consider to use 'applyHdrSettings'
--   instead of importing the package manually.
--   If you really want to do it manually, use
--   the functions from the /raw interface/
--   exposed below.
fancyhdr :: PackageName
fancyhdr = "fancyhdr"

-- Header and footer settings

-- | Header and footer settings of a LaTeX document.
--   Use 'applyHdrSettings' to apply these settings
--   in your document. A default value is provided
--   by 'defaultHdrSettings', which you can modify
--   using record syntax.
--
-- > mySettings :: HdrSettings
-- > mySettings = defaultHdrSettings
-- >     { centerHeader = "Amazing header"
-- >     , headRuleWidth = Pt 2
-- >       }
--
data HdrSettings = HdrSettings
 { leftHeader    :: LaTeX
 , centerHeader  :: LaTeX
 , rightHeader   :: LaTeX
 , leftFooter    :: LaTeX
 , centerFooter  :: LaTeX
 , rightFooter   :: LaTeX
 , headRuleWidth :: Measure
 , footRuleWidth :: Measure
   } deriving (Eq,Show)

-- | Default header and footer settings.
--
--   It leaves everything empty but the
--   'centerFooter' field, which is filled
--   with 'thePage'.
--
--   Also, it sets to 0.4 points
--   the 'headRuleWidth' field.
defaultHdrSettings :: HdrSettings
defaultHdrSettings =
  HdrSettings
   { leftHeader    = mempty
   , centerHeader  = mempty
   , rightHeader   = mempty
   , leftFooter    = mempty
   , centerFooter  = thePage
   , rightFooter   = mempty
   , headRuleWidth = Pt 0.4
   , footRuleWidth = Pt 0
     }

-- | Apply custom header and footer settings to a
--   LaTeX document. It takes care of package importing
--   and page style settings, so using this function
--   is enough to get the settings applied.
--   Do /not/ import the 'fancyhdr' package again.
--   To be used in the /preamble/.
applyHdrSettings :: LaTeXC l => HdrSettings -> l
applyHdrSettings hs =
    usepackage [] fancyhdr
 <> pagestyle fancy
 <> fromLaTeX (lhead $   leftHeader hs)
 <> fromLaTeX (chead $ centerHeader hs)
 <> fromLaTeX (rhead $  rightHeader hs)
 <> fromLaTeX (lfoot $   leftFooter hs)
 <> fromLaTeX (cfoot $ centerFooter hs)
 <> fromLaTeX (rfoot $  rightFooter hs)
 <> renewheadrulewidth (headRuleWidth hs)
 <> renewfootrulewidth (footRuleWidth hs)

-- Raw interface

-- | Page style of the 'fancyhdr' package.
fancy :: PageStyle
fancy = "fancy"

-- | Set the left header.
lhead :: LaTeXC l => l -> l
lhead = comm1 "lhead"

-- | Set the center header.
chead :: LaTeXC l => l -> l
chead = comm1 "chead"

-- | Set the right header.
rhead :: LaTeXC l => l -> l
rhead = comm1 "rhead"

-- | Set the left footer.
lfoot :: LaTeXC l => l -> l
lfoot = comm1 "lfoot"

-- | Set the center footer.
cfoot :: LaTeXC l => l -> l
cfoot = comm1 "cfoot"

-- | Set the right footer.
rfoot :: LaTeXC l => l -> l
rfoot = comm1 "rfoot"

-- | Set the @headrulewidth@ attribute.
renewheadrulewidth :: LaTeXC l => Measure -> l
renewheadrulewidth m = fromLaTeX $
  TeXComm "renewcommand" [ FixArg $ TeXCommS "headrulewidth"
                         , FixArg $ texy m
                           ]

-- | Set the @footrulewidth@ attribute.
renewfootrulewidth :: LaTeXC l => Measure -> l
renewfootrulewidth m = fromLaTeX $
  TeXComm "renewcommand" [ FixArg $ TeXCommS "footrulewidth"
                         , FixArg $ texy m
                           ]
