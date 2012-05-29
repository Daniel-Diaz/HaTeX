
module Text.LaTeX.Base.Class (
   LaTeXC (..)
 , Monoid (..)
   -- * Combinators
 , fromLaTeX
 , liftL
 , liftL2
 , liftL3
 , comm0
 , commS
 , braces
 ) where

import Text.LaTeX.Base.Syntax
import Data.Text (Text)
import Data.Monoid
import Data.String

-- | This is the class of 'LaTeX' code generators. It has 'Monoid' and 'IsString' as
--   superclasses.
class (Monoid l,IsString l) => LaTeXC l where
 -- | This method must take a function that combines a list of 'LaTeX' values into a new one,
 --   and creates a function that combines @l@-typed values. The combining function can be
 --   seen as a function with 0 or more 'LaTeX' arguments with a 'LaTeX' value as output.
 liftListL :: ([LaTeX] -> LaTeX) -> [l] -> l

-- | This instance just sets @liftListL = id@.
instance LaTeXC LaTeX where
 liftListL = id

-- COMBINATORS

fromLaTeX :: LaTeXC l => LaTeX -> l
fromLaTeX l = liftListL (\_ -> l) []

liftL :: LaTeXC l => (LaTeX -> LaTeX) -> l -> l
liftL f x = liftListL (\[x] -> f x) [x]

liftL2 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX) -> l -> l -> l
liftL2 f x y = liftListL (\[x,y] -> f x y) [x,y]

liftL3 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l
liftL3 f x y z = liftListL (\[x,y,z] -> f x y z) [x,y,z]

-- | A simple (without arguments) command generator,
--   given the name of the command.
--
-- > comm0 str = fromLaTeX $ TeXComm str []
--
comm0 :: LaTeXC l => String -> l
comm0 str = fromLaTeX $ TeXComm str []

commS :: LaTeXC l => String -> l
commS = fromLaTeX . TeXCommS

braces :: LaTeXC l => l -> l
braces = liftL TeXBraces