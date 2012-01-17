
{- |
This module exports those minimal things you need
to work with HaTeX. Those things are:

* The 'LaTeX' datatype.

* The '<>' operator, to append 'LaTeX' values.

* The "Text.LaTeX.Base.Render" module, to render a 'LaTeX' value into 'Text'.

* The "Text.LaTeX.Base.Types" module, which contains several types used by
  other modules.

* The "Text.LaTeX.Base.Writer" module, needed to run the 'LaTeXT' monad, and
  obtain a result of type 'LaTeX'.

* The "Text.LaTeX.Base.Commands.Monad" module, which exports the LaTeX standard commands
  and environments.

Here is also defined a 'Num' instance for 'LaTeXT'.
-}
module Text.LaTeX.Base.Monad
 ( LaTeX , (<>)
 , module Text.LaTeX.Base.Render
 , module Text.LaTeX.Base.Types
 , module Text.LaTeX.Base.Writer
 , module Text.LaTeX.Base.Commands.Monad
   ) where

import Text.LaTeX.Base.Syntax (LaTeX,(<>))
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Writer
import Text.LaTeX.Base.Commands.Monad
-- Import LaTeX Num instance
import Text.LaTeX.Base ()
-- Import 'lift' function
import Control.Monad.Trans.Class (lift)

-- Num instance for LaTeXT

-- Sadly, we need 'Show' and 'Eq' instances for LaTeXT in order
-- to do its 'Num' instance.

-- | Don't use it! This instance only exists in order to
--   define a 'Num' instance to 'LaTeXT'.
instance Show (LaTeXT m a) where
 show _   = error "Cannot use \"show\" Show method with a LaTeXT value."

-- | Don't use it! This instance only exists in order to
--   define a 'Num' instance to 'LaTeXT'.
instance Eq (LaTeXT m a) where
 _ == _ = error "Cannot use \"(==)\" Eq method with a LaTeXT value."

-- | Be careful when using 'fromInteger' over a 'LaTeXT' value,
--   the returned value of the computation is bottom (i.e. 'undefined').
--   Methods 'abs' and 'signum' are undefined. Don't use them!
instance Monad m => Num (LaTeXT m a) where
 (+) = liftOp (+)
 (-) = liftOp (-)
 (*) = (>>)
 negate = liftFun negate
 fromInteger = fromString . show
 -- Non-defined methods
 abs _    = "Cannot use \"abs\" Num method with a LaTeXT value."
 signum _ = "Cannot use \"signum\" Num method with a LaTeXT value."
