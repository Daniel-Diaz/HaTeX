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
   -- ** Greek alphabet
   -- | Functions of greek alphabet symbols.
   --
   --   Uppercase versions are suffixed with @u@.
   --   Variants are prefixed with @var@.
   --   The function 'pi_' is ended by an underscore symbol to
   --   distinguish it from the 'pi' Prelude function.
 , alpha    , beta       , gamma
 , gammau   , delta      , deltau
 , epsilon  , varepsilon , zeta
 , eta      , theta      , thetau
 , iota     , kappa      , lambda
 , lambdau  , mu         , nu
 , xi       , xiu        , pi_
 , varpi    , piu        , rho
 , varrho   , sigma      , varsigma
 , sigmau   , tau        , upsilon
 , upsilonu , phi        , varphi
 , phiu     , chi        , psi
 , psiu     , omega      , omegau
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
import Text.LaTeX.Base.Types

-- | AMSMath package.
-- Example:
--
-- > usepackage [] amsmath
amsmath :: PackageName
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

---- Greek alphabet

alpha :: LaTeX
alpha = TeXComm "alpha" []

beta :: LaTeX
beta = TeXComm "beta" []

gamma :: LaTeX
gamma = TeXComm "gamma" []

gammau :: LaTeX
gammau = TeXComm "Gamma" []

delta :: LaTeX
delta = TeXComm "delta" []

deltau :: LaTeX
deltau = TeXComm "Delta" []

epsilon :: LaTeX
epsilon = TeXComm "epsilon" []

varepsilon :: LaTeX
varepsilon = TeXComm "epsilon" []

zeta :: LaTeX
zeta = TeXComm "zeta" []

eta :: LaTeX
eta = TeXComm "eta" []

theta :: LaTeX
theta = TeXComm "theta" []

thetau :: LaTeX
thetau = TeXComm "thetau" []

iota :: LaTeX
iota = TeXComm "iota" []

kappa :: LaTeX
kappa = TeXComm "kappa" []

lambda :: LaTeX
lambda = TeXComm "lambda" []

lambdau :: LaTeX
lambdau = TeXComm "Lambda" []

mu :: LaTeX
mu = TeXComm "mu" []

nu :: LaTeX
nu = TeXComm "nu" []

xi :: LaTeX
xi = TeXComm "xi" []

xiu :: LaTeX
xiu = TeXComm "Xi" []

pi_ :: LaTeX
pi_ = TeXComm "pi" []

varpi :: LaTeX
varpi = TeXComm "varpi" []

piu :: LaTeX
piu = TeXComm "Pi" []

rho :: LaTeX
rho = TeXComm "rho" []

varrho :: LaTeX
varrho = TeXComm "varrho" []

sigma :: LaTeX
sigma = TeXComm "sigma" []

varsigma :: LaTeX
varsigma = TeXComm "varsigma" []

sigmau :: LaTeX
sigmau = TeXComm "Sigma" []

tau :: LaTeX
tau = TeXComm "tau" []

upsilon :: LaTeX
upsilon = TeXComm "upsilon" []

upsilonu :: LaTeX
upsilonu = TeXComm "Upsilon" []

phi :: LaTeX
phi = TeXComm "phi" []

varphi :: LaTeX
varphi = TeXComm "varphi" []

phiu :: LaTeX
phiu = TeXComm "Phi" []

chi :: LaTeX
chi = TeXComm "chi" []

psi :: LaTeX
psi = TeXComm "psi" []

psiu :: LaTeX
psiu = TeXComm "Psi" []

omega :: LaTeX
omega = TeXComm "omega" []

omegau :: LaTeX
omegau = TeXComm "Omega" []

---- Other symbols

-- | A right-arrow.
to :: LaTeX
to = TeXComm "to" []

-- | /For all/ symbol.
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
