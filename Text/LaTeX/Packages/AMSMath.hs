{-# LANGUAGE OverloadedStrings #-}

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
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Commands (raw,between)
import Text.LaTeX.Base.Types

-- | AMSMath package.
-- Example:
--
-- > usepackage [] amsmath
amsmath :: PackageName
amsmath = "amsmath"

-- | Inline mathematical expressions.
math :: LaTeXC l => l -> l
math = liftL TeXMath

-------------------------------------
------- Symbols and utilities -------

-- | Superscript.
(^:) :: LaTeXC l => l -> l -> l
x ^: y = x <> raw "^"  <> braces y

-- | Subscript.
(!:) :: LaTeXC l => l -> l -> l
x !: y = x <> raw "_" <> braces y

---- Function symbols

-- | Sine function symbol.
tsin :: LaTeXC l => l
tsin = comm0 "sin"

-- | Arcsine function symbol.
arcsin :: LaTeXC l => l
arcsin = comm0 "arcsin"

-- | Cosine function symbol.
tcos :: LaTeXC l => l
tcos = comm0 "cos"

-- | Arccosine function symbol.
arccos :: LaTeXC l => l
arccos = comm0 "arccos"

-- | Tangent function symbol.
ttan :: LaTeXC l => l
ttan = comm0 "tan"

-- | Arctangent function symbol.
arctan :: LaTeXC l => l
arctan = comm0 "arctan"

-- | Exponential function symbol.
texp :: LaTeXC l => l
texp = comm0 "exp"

-- | Logarithm function symbol.
tlog :: LaTeXC l => l
tlog = comm0 "log"

-- | Natural logarithm symbol.
ln :: LaTeXC l => l
ln = comm0 "ln"

---- Operator symbols

-- | Negative form of an operator.
notop :: LaTeXC l =>
         (l -> l -> l)
      -> (l -> l -> l)
notop op =
 \l1 l2 ->
   (l1 <> commS "not") `op` l2

infixr 4 =: , /=:

(=:),(/=:) :: LaTeXC l => l -> l -> l
(=:)  = liftL2 $ TeXOp "="
(/=:) = notop (=:)

-- | Greater.
(>:) :: LaTeXC l => l -> l -> l
(>:) = liftL2 $ TeXOp ">"

-- | Greater or equal.
(>=:) :: LaTeXC l => l -> l -> l
(>=:) = between $ comm0 "geq"

-- | Lesser.
(<:) :: LaTeXC l => l -> l -> l
(<:) = liftL2 $ TeXOp "<"

-- | Lesser or equal.
(<=:) :: LaTeXC l => l -> l -> l
(<=:) = between $ comm0 "leq"

in_ :: LaTeXC l => l -> l -> l
in_ = between $ comm0 "in"

ni :: LaTeXC l => l -> l -> l
ni  = between $ comm0 "ni"

notin :: LaTeXC l => l -> l -> l
notin = between $ comm0 "notin"

---- Greek alphabet

alpha :: LaTeXC l => l
alpha = comm0 "alpha"

beta :: LaTeXC l => l
beta = comm0 "beta"

gamma :: LaTeXC l => l
gamma = comm0 "gamma"

gammau :: LaTeXC l => l
gammau = comm0 "Gamma"

delta :: LaTeXC l => l
delta = comm0 "delta"

deltau :: LaTeXC l => l
deltau = comm0 "Delta"

epsilon :: LaTeXC l => l
epsilon = comm0 "epsilon"

varepsilon :: LaTeXC l => l
varepsilon = comm0 "varepsilon"

zeta :: LaTeXC l => l
zeta = comm0 "zeta"

eta :: LaTeXC l => l
eta = comm0 "eta"

theta :: LaTeXC l => l
theta = comm0 "theta"

thetau :: LaTeXC l => l
thetau = comm0 "thetau"

iota :: LaTeXC l => l
iota = comm0 "iota"

kappa :: LaTeXC l => l
kappa = comm0 "kappa"

lambda :: LaTeXC l => l
lambda = comm0 "lambda"

lambdau :: LaTeXC l => l
lambdau = comm0 "Lambda"

mu :: LaTeXC l => l
mu = comm0 "mu"

nu :: LaTeXC l => l
nu = comm0 "nu"

xi :: LaTeXC l => l
xi = comm0 "xi"

xiu :: LaTeXC l => l
xiu = comm0 "Xi"

pi_ :: LaTeXC l => l
pi_ = comm0 "pi"

varpi :: LaTeXC l => l
varpi = comm0 "varpi"

piu :: LaTeXC l => l
piu = comm0 "Pi"

rho :: LaTeXC l => l
rho = comm0 "rho"

varrho :: LaTeXC l => l
varrho = comm0 "varrho"

sigma :: LaTeXC l => l
sigma = comm0 "sigma"

varsigma :: LaTeXC l => l
varsigma = comm0 "varsigma"

sigmau :: LaTeXC l => l
sigmau = comm0 "Sigma"

tau :: LaTeXC l => l
tau = comm0 "tau"

upsilon :: LaTeXC l => l
upsilon = comm0 "upsilon"

upsilonu :: LaTeXC l => l
upsilonu = comm0 "Upsilon"

phi :: LaTeXC l => l
phi = comm0 "phi"

varphi :: LaTeXC l => l
varphi = comm0 "varphi"

phiu :: LaTeXC l => l
phiu = comm0 "Phi"

chi :: LaTeXC l => l
chi = comm0 "chi"

psi :: LaTeXC l => l
psi = comm0 "psi"

psiu :: LaTeXC l => l
psiu = comm0 "Psi"

omega :: LaTeXC l => l
omega = comm0 "omega"

omegau :: LaTeXC l => l
omegau = comm0 "Omega"

---- Other symbols

-- | A right-arrow.
to :: LaTeXC l => l
to = comm0 "to"

-- | /For all/ symbol.
forall :: LaTeXC l => l
forall = comm0 "forall"

-- | Dagger symbol.
dagger :: LaTeXC l => l
dagger = comm0 "dagger"

-- | Double dagger symbol.
ddagger :: LaTeXC l => l
ddagger = comm0 "ddagger"

-------------------------------------
------------ Math Fonts -------------

mathbf :: LaTeXC l => l -> l
mathbf = liftL $ \l -> TeXComm "mathbf" [FixArg l]

mathrm :: LaTeXC l => l -> l
mathrm =liftL $ \l -> TeXComm "mathrm" [FixArg l]

mathcal :: LaTeXC l => l -> l
mathcal = liftL $ \l -> TeXComm "mathcal" [FixArg l]

mathsf :: LaTeXC l => l -> l
mathsf = liftL $ \l -> TeXComm "mathsf" [FixArg l]

mathtt :: LaTeXC l => l -> l
mathtt = liftL $ \l -> TeXComm "mathtt" [FixArg l]

mathit :: LaTeXC l => l -> l
mathit = liftL $ \l -> TeXComm "mathit" [FixArg l]
