
-- | The @relsize@ package is used to set the font size relative to the
-- current size.
--
--  CTAN page for relsize: <http://ctan.org/pkg/relsize>.
module Text.LaTeX.Packages.Relsize
 ( -- * Relsize package
   prelsize
   -- * Commands
 , relsize
 , larger, smaller
 , relscale
 , textlarger, textsmaller, textscale
 ) where

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render (rendertex)
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Types

-- | The 'prelsize' package.
-- Example:
--
-- > usepackage [] prelsize
prelsize :: ClassName
prelsize = "relsize"

-- Commands

-- | Change font size by 'i' steps. A step is a number of '\magsteps' to
-- change size; from this are defined commands '\larger', '\smaller',
-- '\textlarger', etc.
relsize :: LaTeXC l => Int -> l
relsize i = fromLaTeX $ TeXComm "relsize" [FixArg $ rendertex i]

-- | Increase font size by (optional) 'i' steps (default 1).
larger :: LaTeXC l => Maybe Int -> l
larger Nothing = comm0 "larger"
larger (Just i) = fromLaTeX $ TeXComm "larger" [OptArg $ rendertex i]

-- | Reduce font size by 'i' steps (default 1).
smaller :: LaTeXC l => Maybe Int -> l
smaller Nothing = comm0 "smaller"
smaller (Just i) = fromLaTeX $ TeXComm "smaller" [OptArg $ rendertex i]

-- | Change font size by scale factor 'f'.
relscale :: LaTeXC l => Float -> l
relscale f = fromLaTeX $ TeXComm "relscale" [FixArg $ rendertex f]

-- | Text size enlarged by (optional) 'i' steps.
textlarger :: LaTeXC l => Maybe Int -> l -> l
textlarger Nothing = comm1 "textlarger"
textlarger (Just i) = liftL $ \l -> TeXComm "textlarger" [OptArg $ rendertex i, FixArg l]

-- | Text size reduced by (optional) 'i' steps.
textsmaller :: LaTeXC l => Maybe Int -> l -> l
textsmaller Nothing = comm1 "textsmaller"
textsmaller (Just i) = liftL $ \l -> TeXComm "textsmaller" [OptArg $ rendertex i, FixArg l]

-- | Text size scaled by factor 'f'.
textscale :: LaTeXC l => Float -> l -> l
textscale f = liftL $ \l -> TeXComm "textscale" [FixArg $ rendertex f, FixArg l]
