
module Text.LaTeX.Base.Class (
   LaTeXC (..)
 , Monoid (..)
   -- * Combinators
 , fromLaTeX
 , liftL
 , liftL2
 , comm0
 , commS
 , braces
 ) where

import Text.LaTeX.Base.Syntax
import Data.Text (Text)
import Data.Monoid
import Data.String

class (Monoid l,IsString l) => LaTeXC l where
 liftListL :: ([LaTeX] -> LaTeX) -> [l] -> l

instance LaTeXC LaTeX where
 liftListL = id

-- COMBINATORS

fromLaTeX :: LaTeXC l => LaTeX -> l
fromLaTeX l = liftListL (\_ -> l) []

liftL :: LaTeXC l => (LaTeX -> LaTeX) -> l -> l
liftL f x = liftListL (\[x] -> f x) [x]

liftL2 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX) -> l -> l -> l
liftL2 f x y = liftListL (\[x,y] -> f x y) [x,y]

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