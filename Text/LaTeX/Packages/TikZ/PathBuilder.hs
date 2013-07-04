
-- | This module provides a monadic interface to build 'TPath' values.
--   It does so using 'PathBuilder's. The construction of a 'PathBuilder'
--   is equivalent to the construction of a 'TPath' by hand, but with
--   a sometimes more convenient syntax.
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
-- > trianglePath = Cycle $ Start (pointAtXY (-1) 0) ->- pointAtXY 1 0 ->- pointAtXY 0 1
--
--   The 'Cycle' constructor at the beginning may seem unintuitive, since we are building
--   the path from left to right. In the 'PathBuilder' monad, the instructions are always
--   written in order.
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
 , node
 , grid
   ) where

import Text.LaTeX.Base.Syntax (LaTeX)
import Text.LaTeX.Packages.TikZ.Syntax
import Control.Monad.Trans.State
import Control.Applicative

data PathState = PS { currentPath :: TPath }

-- | Use a /path builder/ to construct a value of type 'TPath'.
--   Use 'bpath' for this purpose.
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

-- | Line from the current point to the given one.
line :: TPoint -> PathBuilder ()
line p = applyToPath $ (`Line`p)

-- | Rectangle with the current point as one cornder and the given point
--   as the opposite corner.
rectangle :: TPoint -> PathBuilder ()
rectangle p = applyToPath $ (`Rectangle`p)

-- | Circle with the given radius centered at the current point.
circle :: Double -> PathBuilder ()
circle r = applyToPath $ (`Circle`r)

-- | Ellipse with width and height described by the arguments and centered
--   at the current point.
ellipse :: Double -- ^ Half width of the ellipse.
        -> Double -- ^ Half height of the ellipse.
        -> PathBuilder ()
ellipse r1 r2 = applyToPath $ \x -> Ellipse x r1 r2

grid :: [GridOption] -> TPoint -> PathBuilder ()
grid xs p = applyToPath $ \x -> Grid x xs p

-- | Text centered at the current point.
node :: LaTeX -> PathBuilder ()
node l = applyToPath $ \x -> Node x l

-- | Build a path using a /starting point/ and a 'PathBuilder'.
bpath :: TPoint -> PathBuilder a -> TPath
bpath p pb = currentPath $ execState (pathBuilder pb) (PS $ Start p)
