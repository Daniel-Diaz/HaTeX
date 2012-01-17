{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HATEX MakeMonadic #-}

module Text.LaTeX.Packages.AMSMath
 ( -- * AMSMath package
   amsmath
   -- * AMSMath functions
 , math
   -- * Symbols and utilities
   -- ** Superscript and subscript
 , (^:) , (!:)
   -- ** Function symbols
   -- | Some symbols are preceded with /t/ to be distinguished from
   --   predefined Haskell entities (like 'sin' and 'cos').
 , tsin , arcsin
 , tcos , arccos
 , ttan , arctan
 , texp
 , tlog , ln
   -- ** Operator symbols
 , (=:) , (/=:)
 , (>:) , (>=:)
 , (<:) , (<=:)
 , in_ , ni , notin
   -- ** Other symbols
 , to
 , forall
 , dagger, ddagger
   -- * Fonts
 , mathbf
 , mathrm
 , mathcal
 , mathsf
 , mathtt
 , mathit
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Commands (raw,between)

-- | AMSMath package.
-- Example:
--
-- > usepackage [] amsmath
amsmath :: String
amsmath = "amsmath"

-- | Inline mathematical expressions.
math :: LaTeX -> LaTeX
math = TeXMath

-------------------------------------
------- Symbols and utilities -------

-- | Superscript.
(^:) :: LaTeX -> LaTeX -> LaTeX
x ^: y = x <> raw "^"  <> braces y

-- | Subscript.
(!:) :: LaTeX -> LaTeX -> LaTeX
x !: y = x <> raw "_" <> braces y

---- Function symbols

-- | Sine function symbol.
tsin :: LaTeX
tsin = TeXComm "sin" []

-- | Arcsine function symbol.
arcsin :: LaTeX
arcsin = TeXComm "arcsin" []

-- | Cosine function symbol.
tcos :: LaTeX
tcos = TeXComm "cos" []

-- | Arccosine function symbol.
arccos :: LaTeX
arccos = TeXComm "arccos" []

-- | Tangent function symbol.
ttan :: LaTeX
ttan = TeXComm "tan" []

arctan :: LaTeX
arctan = TeXComm "arctan" []

-- | Exponential function symbol.
texp :: LaTeX
texp = TeXComm "exp" []

-- | Logarithm function symbol.
tlog :: LaTeX
tlog = TeXComm "log" []

-- | Natural logarithm symbol.
ln :: LaTeX
ln = TeXComm "ln" []

---- Operator symbols

-- | Negative form of an operator.
notop :: (LaTeX -> LaTeX -> LaTeX)
      -> (LaTeX -> LaTeX -> LaTeX)
notop op =
 \l1 l2 ->
   (l1 <> TeXCommS "not") `op` l2

infix 4 =: , /=:

(=:),(/=:) :: LaTeX -> LaTeX -> LaTeX
(=:)  = TeXOp "="
(/=:) = notop (=:)

-- | Greater.
(>:) :: LaTeX -> LaTeX -> LaTeX
(>:) = TeXOp ">"

-- | Greater or equal.
(>=:) :: LaTeX -> LaTeX -> LaTeX
(>=:) = between $ TeXComm "geq" []

-- | Lesser.
(<:) :: LaTeX -> LaTeX -> LaTeX
(<:) = TeXOp "<"

-- | Lesser or equal.
(<=:) :: LaTeX -> LaTeX -> LaTeX
(<=:) = between $ TeXComm "leq" []

in_ :: LaTeX -> LaTeX -> LaTeX
in_ = between $ TeXComm "in" []

ni :: LaTeX -> LaTeX -> LaTeX
ni  = between $ TeXComm "ni" []

notin :: LaTeX -> LaTeX -> LaTeX
notin = between $ TeXComm "notin" []

---- Other symbols

-- | A right-arrow.
to :: LaTeX
to = TeXComm "to" []

forall :: LaTeX
forall = TeXComm "forall" []

-- | Dagger symbol.
dagger :: LaTeX
dagger = TeXComm "dagger" []

-- | Double dagger symbol.
ddagger :: LaTeX
ddagger = TeXComm "ddagger" []

-------------------------------------
------------ Math Fonts -------------

mathbf :: LaTeX -> LaTeX
mathbf l = TeXComm "mathbf" [FixArg l]

mathrm :: LaTeX -> LaTeX
mathrm l = TeXComm "mathrm" [FixArg l]

mathcal :: LaTeX -> LaTeX
mathcal l = TeXComm "mathcal" [FixArg l]

mathsf :: LaTeX -> LaTeX
mathsf l = TeXComm "mathsf" [FixArg l]

mathtt :: LaTeX -> LaTeX
mathtt l = TeXComm "mathtt" [FixArg l]

mathit :: LaTeX -> LaTeX
mathit l = TeXComm "mathit" [FixArg l]
