
{-# LANGUAGE CPP #-}

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

* The "Text.LaTeX.Base.Writer" module, to work with the monad interface of the library.
-}
module Text.LaTeX.Base
 ( -- * @LaTeX@ datatype
   LaTeX
   -- * Escaping reserved characters
 , protectString , protectText
   -- * Internal re-exports
 , module Text.LaTeX.Base.Render
 , module Text.LaTeX.Base.Types
 , module Text.LaTeX.Base.Commands
 , module Text.LaTeX.Base.Writer
   -- * External re-exports
   --
   -- | Since the 'Monoid' instance is the only way to append 'LaTeX'
   --   values, a re-export of "Data.Monoid" is given here.
 , module Data.Monoid
#if __GLASGOW_HASKELL__ < 704
 , (<>)
#endif
   ) where

-- Internal modules
import Text.LaTeX.Base.Syntax (LaTeX,(<>),protectString,protectText)
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Writer
-- External modules
import Data.Monoid
