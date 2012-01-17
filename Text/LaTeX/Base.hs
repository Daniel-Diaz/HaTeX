
{- |
This module exports those minimal things you need
to work with HaTeX. Those things are:

* The 'LaTeX' datatype.

* The '<>' operator, to append 'LaTeX' values.

* The "Text.LaTeX.Base.Render" module, to render a 'LaTeX' value into 'Text'.

* The "Text.LaTeX.Base.Types" module, which contains several types used by
  other modules.

* The "Text.LaTeX.Base.Commands" module, which exports the LaTeX standard commands
  and environments.

Here is also defined a 'Num' instance for 'LaTeX'.
-}
module Text.LaTeX.Base
 ( LaTeX , (<>)
 , module Text.LaTeX.Base.Render
 , module Text.LaTeX.Base.Types
 , module Text.LaTeX.Base.Commands
   ) where

import Text.LaTeX.Base.Syntax (LaTeX (..),(<>))
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands

-- Num instance for LaTeX

-- | Methods 'abs' and 'signum' are undefined. Don't use them!
instance Num LaTeX where
 (+) = TeXOp "+"
 (-) = TeXOp "-"
 (*) = (<>)
 negate = (TeXEmpty -)
 fromInteger = TeXRaw . fromString . show
 -- Non-defined methods
 abs _    = "Cannot use \"abs\" Num method with a LaTeX value."
 signum _ = "Cannot use \"signum\" Num method with a LaTeX value."
