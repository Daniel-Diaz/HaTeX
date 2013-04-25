
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
-- > anotherExample :: LaTeXT IO ()
-- > anotherExample = lift (readFile "foo") >>= verbatim . fromString
--
--
-- This way, it is easy (without carrying arguments) to include IO outputs
-- in the LaTeX document, like files, times or random objects.
--
-- Another approach could be to have custom counters, label management
-- or any other user-defined feature.
--
-- Of course, you can always use the simpler interface provided by the plain 'LaTeX' type.
--
-- Another thing you should know about the LaTeX Writer Monad. Don't try to get values
-- from empty 
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
   -- * Errors
 , throwError
 , merror
   -- * Re-export
 , lift
 , liftIO
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
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Warnings (Warning,checkAll,check)
--
import Control.Monad (liftM)

newtype LaTeXT m a =
  LaTeXT { unwrapLaTeXT :: WriterT LaTeX m (a,Maybe String) }

instance Functor f => Functor (LaTeXT f) where
 fmap f (LaTeXT c) = LaTeXT $ fmap (first f) c

pairNoth :: a -> (a,Maybe b)
pairNoth x = (x,Nothing)

instance Applicative f => Applicative (LaTeXT f) where
 pure = LaTeXT . pure . pairNoth
 (LaTeXT f) <*> (LaTeXT x) = LaTeXT $ fmap (first . fst) f <*> x

type LaTeXT_ m = LaTeXT m ()

instance MonadTrans LaTeXT where
 lift = LaTeXT . liftM pairNoth . lift

instance Monad m => Monad (LaTeXT m) where
 return = LaTeXT . return . pairNoth
 (LaTeXT c) >>= f = LaTeXT $ do 
  (a,_) <- c
  let LaTeXT c' = f a
  c'
 fail = throwError

instance MonadIO m => MonadIO (LaTeXT m) where
 liftIO = lift . liftIO

instance Monad m => LaTeXC (LaTeXT m a) where
 liftListL f xs = mapM extractLaTeX_ xs >>= merror "liftListL" . textell . f

runLaTeXT :: Monad m => LaTeXT m a -> m (Either String a,LaTeX)
runLaTeXT (LaTeXT c) = runWriterT c >>= (
  \((a,m),l) -> case m of
             Nothing  -> return (Right a ,l)
             Just err -> return (Left err,l)
       )

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
extractLaTeX (LaTeXT c) = LaTeXT $ do
 ((a,m),l) <- lift $ runWriterT c
 return ((a,l),m)

extractLaTeX_ :: Monad m => LaTeXT m a -> LaTeXT m LaTeX
extractLaTeX_ = liftM snd . extractLaTeX

-- | With 'textell' you can append 'LaTeX' values to the
--   state of the 'LaTeXT' monad.
textell :: Monad m => LaTeX -> LaTeXT m ()
textell = LaTeXT . liftM pairNoth . tell

-- | Lift a function over 'LaTeX' values to a function
--   acting over the state of a 'LaTeXT' computation.
liftFun :: Monad m
        => (LaTeX -> LaTeX)
        -> (LaTeXT m a -> LaTeXT m a)
liftFun f (LaTeXT c) = LaTeXT $ do
 (p,l) <- lift $ runWriterT c
 tell $ f l
 return p


-- | Lift an operator over 'LaTeX' values to an operator
--   acting over the state of two 'LaTeXT' computations.
--
-- /Note: The returned value is the one returned by the/
-- /second argument of the lifted operator./
liftOp :: Monad m
       => (LaTeX -> LaTeX -> LaTeX)
       -> (LaTeXT m a -> LaTeXT m b -> LaTeXT m b)
liftOp op (LaTeXT c) (LaTeXT c') = LaTeXT $ do
 (_,l)  <- lift $ runWriterT c
 (p,l') <- lift $ runWriterT c'
 tell $ l `op` l'
 return p

-- | Just like 'rendertex', but with 'LaTeXT' output.
--
-- > rendertexM = textell . rendertex
rendertexM :: (Render a, Monad m) => a -> LaTeXT m ()
rendertexM = textell . rendertex

-- Error throwing

throwError :: Monad m => String -> LaTeXT m a
throwError = LaTeXT . return . (error &&& Just)

-- | Function 'merror' casts a value contained in a monad @m@ to the
--   bottom value of another type. If you try to evaluate this value, you will
--   get an error message with the 'String' passed as argument to 'merror'.
merror :: Monad m => String -> LaTeXT m a -> LaTeXT m b
merror = flip (>>) . throwError

-- Overloaded Strings

-- | Be careful when using 'fromString' over a 'LaTeXT' value,
--   the returned value of the computation is bottom (i.e. 'undefined').
instance Monad m => IsString (LaTeXT m a) where
 fromString = merror "LaTeXT: fromString!" . textell . fromString

-- | 'mappend' @=@ '>>'.
instance Monad m => Monoid (LaTeXT m a) where
 mempty = throwError "LaTeXT: mempty!"
 mappend = (>>)
