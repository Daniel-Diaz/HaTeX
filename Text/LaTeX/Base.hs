
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

Here are also defined 'Num' and 'Fractional' instances for both 'LaTeX' and 'LaTeXT'.
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

import Text.LaTeX.Base.Syntax (LaTeX (..), TeXArg (..),(<>),protectString,protectText)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Writer
--
import Data.Monoid

-- Num and Fractional instances for LaTeX and LaTeXT

-- | Careful! Method 'signum' is undefined. Don't use it!
instance Num LaTeX where
 (+) = TeXOp "+"
 (-) = TeXOp "-"
 (*) = (<>)
 negate = (TeXEmpty -)
 fromInteger = rendertex
 abs x = between x "|" "|"
 -- Non-defined methods
 signum _ = error "Cannot use \"signum\" Num method with a LaTeX value."

-- | 
instance Fractional LaTeX where
 p / q = TeXComm "frac" [FixArg p, FixArg q]
 fromRational = rendertex . (fromRational :: Rational -> Float)

-- | Warning: this instance only exist for the 'Num' instance.
instance Monad m => Eq (LaTeXT m a) where
 _ == _ = error "Cannot use \"(==)\" Eq method with a LaTeXT value."

-- | Warning: this instance only exist for the 'Num' instance.
instance Monad m => Show (LaTeXT m a) where
 show _ = error "Cannot use \"show\" Show method with a LaTeXT value."

-- | Careful! Method 'signum' is undefined. Don't use it!
instance Monad m => Num (LaTeXT m a) where
 (+) = liftOp (+)
 (-) = liftOp (-)
 (*) = (>>)
 negate = (mempty -)
 fromInteger = fromLaTeX . fromInteger
 abs = liftL abs
 -- Non-defined methods
 signum _ = error "Cannot use \"signum\" Num method with a LaTeXT value."

instance Monad m => Fractional (LaTeXT m a) where
 (/) = liftOp (/)
 fromRational = fromLaTeX . fromRational