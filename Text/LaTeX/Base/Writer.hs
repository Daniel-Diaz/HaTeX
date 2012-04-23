
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The writer monad applied to 'LaTeX' values. Useful to compose 'LaTeX' values
--   using the @do@ notation:
--
-- > anExample :: Monad m => LaTeXT m ()
-- > anExample = do
-- >   documentclass [] article
-- >   author "Daniel Monad"
-- >   title "LaTeX and do notation"
-- >   document $ do
-- >     maketitle
-- >     section "Some words"
-- >     "Using " ; texttt "do" ; " notation "
-- >     "you avoid many ocurrences of the "
-- >     texttt "(<>)" ; " operator and a lot of "
-- >     "parentheses. With the cost of a monad."
--
-- Since 'LaTeXT' is a monad transformer, you can do also:
--
-- > anotherExample :: Monad m => LaTeXT m ()
-- > anotherExample = lift (readFile "foo") >>= verbatim . fromString
--
-- This way, it is easy (without carrying arguments) to include IO outputs
-- in the LaTeX document, like files, times or random objects.
--
-- Another approach could be to have custom counters, label management
-- or any other user-defined feature.
--
-- Of course, you can always use the simpler interface provided by the plain 'LaTeX' type.
module Text.LaTeX.Base.Writer
 ( -- * @LaTeXT@ writer
   LaTeXT
 , LaTeXT_
 , runLaTeXT
 , execLaTeXT
 , execLaTeXTWarn
 , extractLaTeX
 , extractLaTeX_
 , textell
 , rendertexM
 , liftFun
 , liftOp
   -- * Re-export
 , lift
   ) where

import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Arrow
import Data.String
import Data.Monoid
--
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Warnings (Warning,checkAll,check)
--
import Control.Monad (liftM)

newtype LaTeXT m a =
 LaTeXT { unwrapLaTeXT :: WriterT LaTeX m a }
   deriving (Functor,Applicative,Monad,MonadIO)

instance MonadTrans LaTeXT where
 lift = LaTeXT . lift

type LaTeXT_ m = LaTeXT m ()

runLaTeXT :: LaTeXT m a -> m (a,LaTeX)
runLaTeXT = runWriterT . unwrapLaTeXT

-- | This is the usual way to run the 'LaTeXT' monad
--   and obtain a 'LaTeX' value.
execLaTeXT :: Monad m => LaTeXT m a -> m LaTeX
execLaTeXT = liftM snd . runLaTeXT

-- | Version of 'execLaTeXT' with possible warning messages.
--   This function applies 'checkAll' to the 'LaTeX' output.
execLaTeXTWarn :: Monad m => LaTeXT m a -> m (LaTeX,[Warning])
execLaTeXTWarn = liftM (id &&& check checkAll) . execLaTeXT

-- | This function run a 'LaTeXT' computation,
--   lifting the result again in the monad.
extractLaTeX :: Monad m => LaTeXT m a -> LaTeXT m (a,LaTeX)
extractLaTeX = LaTeXT . lift . runLaTeXT

extractLaTeX_ :: Monad m => LaTeXT m a -> LaTeXT m LaTeX
extractLaTeX_ = liftM snd . extractLaTeX

-- | With 'textell' you can append 'LaTeX' values to the
--   state of the 'LaTeXT' monad.
textell :: Monad m => LaTeX -> LaTeXT m ()
textell = LaTeXT . tell

-- | Lift a function over 'LaTeX' values to a function
--   acting over the state of a 'LaTeXT' computation.
liftFun :: Monad m
        => (LaTeX -> LaTeX)
        -> (LaTeXT m a -> LaTeXT m a)
liftFun f ml = do
 (a,l') <- lift $ do
            (a,l) <- runLaTeXT ml
            let l' = f l
            return (a,l')
 textell l'
 return a

-- | Lift an operator over 'LaTeX' values to an operator
--   acting over the state of two 'LaTeXT' computations.
--
-- /Note: The returned value is the one returned by the/
-- /second argument of the lifted operator./
liftOp :: Monad m
       => (LaTeX -> LaTeX -> LaTeX)
       -> (LaTeXT m a -> LaTeXT m a -> LaTeXT m a)
liftOp op ml1 ml2 = do
 (a,l') <- lift $ do
            (_,l1) <- runLaTeXT ml1
            (a,l2) <- runLaTeXT ml2
            let l' = l1 `op` l2
            return (a,l')
 textell l'
 return a

-- | Just like 'rendertex', but with 'LaTeXT' output.
--
-- > rendertexM = textell . rendertex
rendertexM :: (Render a, Monad m) => a -> LaTeXT_ m
rendertexM = textell . rendertex

-- Overloaded Strings

-- | Be careful when using 'fromString' over a 'LaTeXT' value,
--   the returned value of the computation is bottom (i.e. 'undefined').
instance Monad m => IsString (LaTeXT m a) where
 fromString = (>> return undefined) . textell . fromString

-- | 'mappend' @=@ '>>'.
instance Monad m => Monoid (LaTeXT m a) where
 mempty = return undefined
 mappend = (>>)
