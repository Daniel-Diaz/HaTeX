
-- | This module provides a monadic interface to build 'TPath' values.
--   It does so using 'PathBuilder's. The construction of a 'PathBuilder'
--   is equivalent to the construction of a 'TPath' by hand, but with
--   a possibly more convenient syntax.
--
--   For example, this path corresponds to a triangle:
--
-- > trianglePath :: TPath
-- > trianglePath = bpath (pointAtXY (-1) 0) $ do
-- >    line $ pointAtXY 1 0
-- >    line $ pointAtXY 0 1
-- >    pcycle
--
--   The equivalent syntax created by hand would be:
--
-- > trianglePath :: TPath
-- > trianglePath = Cycle (Line (Line (Start (pointAtXY (-1) 0)) (pointAtXY 1 0)) (pointAtXY 0 1))
--
module Text.LaTeX.Packages.TikZ.PathBuilder (
   -- * Path builder
   PathBuilder
 , bpath
   -- * Builder functions
 , line
 , pcycle
 , rectangle
 , circle
 , ellipse
 , grid
   ) where

import Text.LaTeX.Packages.TikZ.Syntax
import Control.Monad.Trans.State
import Control.Applicative

data PathState = PS { currentPath :: TPath }

data PathBuilder a = PB { pathBuilder :: State PathState a }

-- Instances

instance Functor PathBuilder where
 fmap f (PB st) = PB $ fmap f st

instance Applicative PathBuilder where
 pure = PB . pure
 (PB f) <*> (PB x) = PB $ f <*> x

instance Monad PathBuilder where
 return = pure
 (PB x) >>= f = PB $ x >>= pathBuilder . f

--

applyToPath :: (TPath -> TPath) -> PathBuilder ()
applyToPath f = PB $ modify $ \ps -> ps { currentPath = f (currentPath ps) }

pcycle :: PathBuilder ()
pcycle = applyToPath Cycle

line :: TPoint -> PathBuilder ()
line p = applyToPath $ (`Line`p)

rectangle :: TPoint -> PathBuilder ()
rectangle p = applyToPath $ (`Rectangle`p)

circle :: Double -> PathBuilder ()
circle r = applyToPath $ (`Circle`r)

ellipse :: Double -> Double -> PathBuilder ()
ellipse r1 r2 = applyToPath $ \x -> Ellipse x r1 r2

grid :: [GridOption] -> TPoint -> PathBuilder ()
grid xs p = applyToPath $ \x -> Grid x xs p

-- | Build a path using a /starting point/ and a 'PathBuilder'.
bpath :: TPoint -> PathBuilder a -> TPath
bpath p pb = currentPath $ execState (pathBuilder pb) (PS $ Start p)

