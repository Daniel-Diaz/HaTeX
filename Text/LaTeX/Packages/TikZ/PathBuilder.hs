
module Text.LaTeX.Packages.TikZ.PathBuilder (
   -- * Path builder
   PathBuilder
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

tcycle :: PathBuilder ()
tcycle = applyToPath Cycle

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

-- | Build a path using a 'PathBuilder' and a /starting point/.
bpath :: PathBuilder a -> TPoint -> TPath
bpath pb p = currentPath $ execState (pathBuilder pb) (PS $ Start p)

