{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Definition of the 'LaTeXC' class, used to combine the classic applicative and
--   the latter monadic interfaces of /HaTeX 3/. The user can define new instances
--   as well, adding flexibility to the way /HaTeX/ is used.
module Text.LaTeX.Base.Class (
   LaTeXC (..)
 , Monoid (..)
   -- * Combinators
   -- ** From @LaTeX@
 , fromLaTeX
   -- ** Lifting
   -- | Lifting functions from 'LaTeX' functions to functions over any instance of 'LaTeXC'.
   --   In general, the implementation is as follows:
   --
   -- > liftLN f x1 ... xN = liftListL (\[x1,...,xN] -> f x1 ... xN) [x1,...,xN]
   --
 , liftL, liftL2, liftL3, liftL4, liftL5, liftL6, liftL7, liftL8, liftL9
   -- ** Others
 , comm0, comm1, comm2, comm3, comm4, comm5, comm6, comm7, comm8, comm9
 , commS
 , fixComm, optFixComm
 , braces
 , squareBraces
 , raw
 ) where

import Text.LaTeX.Base.Syntax
import Data.String
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
import Data.Text (Text)

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

-- | Map a 'LaTeX' value to its equivalent in any 'LaTeXC' instance.
fromLaTeX :: LaTeXC l => LaTeX -> l
fromLaTeX l = liftListL (const l) []

-- | Lift a inner function of 'LaTeX' values into any 'LaTeXC' instance.
liftL :: LaTeXC l => (LaTeX -> LaTeX) -> l -> l
liftL f x1 = liftListL (\[x1] -> f x1) [x1]

-- | Variant of 'liftL' with a two arguments function.
liftL2 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX) -> l -> l -> l
liftL2 f x1 x2 = liftListL (\[x1,x2] -> f x1 x2) [x1,x2]

-- | Variant of 'liftL' with a three arguments function.
liftL3 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l
liftL3 f x1 x2 x3 = liftListL (\[x1,x2,x3] -> f x1 x2 x3) [x1,x2,x3]

-- | Variant of 'liftL' with a four arguments function.
liftL4 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l -> l
liftL4 f x1 x2 x3 x4 = liftListL (\[x1,x2,x3,x4] -> f x1 x2 x3 x4) [x1,x2,x3,x4]

-- | Variant of 'liftL' with a five arguments function.
liftL5 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l -> l -> l
liftL5 f x1 x2 x3 x4 x5 = liftListL (\[x1,x2,x3,x4,x5] -> f x1 x2 x3 x4 x5) [x1,x2,x3,x4,x5]

-- | Variant of 'liftL' with a six arguments function.
liftL6 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l -> l -> l -> l
liftL6 f x1 x2 x3 x4 x5 x6 = liftListL (\[x1,x2,x3,x4,x5,x6] -> f x1 x2 x3 x4 x5 x6) [x1,x2,x3,x4,x5,x6]

-- | Variant of 'liftL' with a seven arguments function.
liftL7 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l -> l -> l -> l -> l
liftL7 f x1 x2 x3 x4 x5 x6 x7 = liftListL (\[x1,x2,x3,x4,x5,x6,x7] -> f x1 x2 x3 x4 x5 x6 x7) [x1,x2,x3,x4,x5,x6,x7]

-- | Variant of 'liftL' with an eight arguments function.
liftL8 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l -> l -> l -> l -> l -> l
liftL8 f x1 x2 x3 x4 x5 x6 x7 x8 = liftListL (\[x1,x2,x3,x4,x5,x6,x7,x8] -> f x1 x2 x3 x4 x5 x6 x7 x8) [x1,x2,x3,x4,x5,x6,x7,x8]

-- | Variant of 'liftL' with a nine arguments function.
liftL9 :: LaTeXC l => (LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX -> LaTeX) -> l -> l -> l -> l -> l -> l -> l -> l -> l -> l
liftL9 f x1 x2 x3 x4 x5 x6 x7 x8 x9 = liftListL (\[x1,x2,x3,x4,x5,x6,x7,x8,x9] -> f x1 x2 x3 x4 x5 x6 x7 x8 x9) [x1,x2,x3,x4,x5,x6,x7,x8,x9]


-- | Map the given functor of LaTeX values to fixed TeX arguments.
fixArgs :: Functor f => f LaTeX -> f TeXArg
fixArgs = fmap FixArg

-- | Map the first 'n' elements as optional arguments, and the remaining ones as
-- fixed arguments. Most LaTeX commands are structured with first a sequence of
-- optional arguments, followed by a sequence of fixed arguments.
optFixArgs :: Int -> [LaTeX] -> [TeXArg]
optFixArgs _ [] = []
optFixArgs k xa@(x:xs) | k <= 0 = map FixArg xa
                       | otherwise = OptArg x : optFixArgs (k-1) xs

-- | Call a LaTeX command where all the arguments in the list are fixed
-- arguments.
fixComm :: LaTeXC l => String -> [l] -> l
fixComm str = liftListL (TeXComm str . fixArgs)

-- | Call a LaTeX command with the first 'n' arguments as optional ones,
-- followed by fixed arguments. Most LaTeX commands are structured with first a
-- sequence of optional arguments, followed by a sequence of fixed arguments.
optFixComm :: LaTeXC l => String -> Int -> [l] -> l
optFixComm str k = liftListL (TeXComm str . optFixArgs k)

-- | A simple (without arguments) and handy command generator
--   using the name of the command.
--
-- > comm0 str = fromLaTeX $ TeXComm str []
--
comm0 :: LaTeXC l => String -> l
comm0 str = fromLaTeX $ TeXComm str []

-- | A one parameter command generator using the name of the command.
--   The parameter will be rendered as a fixed argument.
--
-- > comm1 str = liftL $ \l -> TeXComm str [FixArg l]
--
comm1 :: LaTeXC l => String -> l -> l
comm1 str l1 = fixComm str [l1]

-- | A two parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm2 str = liftL2 $ \l1 l2 -> TeXComm str [FixArg l1, FixArg l2]
--
comm2 :: LaTeXC l => String -> l -> l -> l
comm2 str l1 l2 = fixComm str [l1, l2]

-- | A three parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm3 str = liftL3 $ \l1 l2 l3 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3]
--
comm3 :: LaTeXC l => String -> l -> l -> l -> l
comm3 str l1 l2 l3 = fixComm str [l1, l2, l3]

-- | A four parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm4 str = liftL4 $ \l1 l2 l3 l4 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3, FixArg l4]
--
comm4 :: LaTeXC l => String -> l -> l -> l -> l -> l
comm4 str l1 l2 l3 l4 = fixComm str [l1, l2, l3, l4]

-- | A five parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm5 str = liftL5 $ \l1 l2 l3 l4 l5 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3, FixArg l4, FixArg l5]
--
comm5 :: LaTeXC l => String -> l -> l -> l -> l -> l -> l
comm5 str l1 l2 l3 l4 l5 = fixComm str [l1, l2, l3, l4, l5]

-- | A six parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm6 str = liftL6 $ \l1 l2 l3 l4 l5 l6 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3, FixArg l4, FixArg l5, FixArg l6]
--
comm6 :: LaTeXC l => String -> l -> l -> l -> l -> l -> l -> l
comm6 str l1 l2 l3 l4 l5 l6 = fixComm str [l1, l2, l3, l4, l5, l6]

-- | A seven parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm7 str = liftL7 $ \l1 l2 l3 l4 l5 l6 l7 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3, FixArg l4, FixArg l5, FixArg l6, FixArg l7]
--
comm7 :: LaTeXC l => String -> l -> l -> l -> l -> l -> l -> l -> l
comm7 str l1 l2 l3 l4 l5 l6 l7 = fixComm str [l1, l2, l3, l4, l5, l6, l7]

-- | An eight parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm8 str = liftL8 $ \l1 l2 l3 l4 l5 l6 l7 l8 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3, FixArg l4, FixArg l5, FixArg l6, FixArg l7, FixArgs l8]
--
comm8 :: LaTeXC l => String -> l -> l -> l -> l -> l -> l -> l -> l -> l
comm8 str l1 l2 l3 l4 l5 l6 l7 l8 = fixComm str [l1, l2, l3, l4, l5, l6, l7, l8]

-- | A nine parameter command generator using the name of the command.
--   The parameters will be rendered as fixed arguments.
--
-- > comm9 str = liftL9 $ \l1 l2 l3 l4 l5 l6 l7 l8 l9 -> TeXComm str [FixArg l1, FixArg l2, FixArg l3, FixArg l4, FixArg l5, FixArg l6, FixArg l7, FixArgs l8, l9]
--
comm9 :: LaTeXC l => String -> l -> l -> l -> l -> l -> l -> l -> l -> l -> l
comm9 str l1 l2 l3 l4 l5 l6 l7 l8 l9 = fixComm str [l1, l2, l3, l4, l5, l6, l7, l8, l9]


-- | Like 'comm0' but using 'TeXCommS', i.e. no \"{}\" will be inserted to protect
-- the command's end.
--
-- > commS = fromLaTeX . TeXCommS
--
commS :: LaTeXC l => String -> l
commS = fromLaTeX . TeXCommS

-- | A lifted version of the 'TeXBraces' constructor.
--
-- > braces = liftL TeXBraces
--
braces :: LaTeXC l => l -> l
braces = liftL TeXBraces

squareBraces :: LaTeXC l => l -> l
squareBraces = liftL $ \l -> TeXRaw "[" <> l <> TeXRaw "]" 

-- | Insert a raw piece of 'Text'.
-- This functions doesn't escape @LaTeX@ reserved characters,
-- it insert the text just as it is received.
--
-- /Warning:/ This function is /unsafe/, in the sense that it does
-- not check that the input text is a valid LaTeX /block/.
-- Make sure any braces, commands or environments are properly closed.
raw :: LaTeXC l => Text -> l
raw = fromLaTeX . TeXRaw
