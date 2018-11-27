
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

-- | This module contains the maths-specific part of "Text.LaTeX.Base.Commands",
--   i.e. of the commands that are available in LaTeX out of the box without
--   any imports.
module Text.LaTeX.Base.Math
 ( -- * Math Environments
   math, mathDisplay
 , equation , equation_
   -- ** Referencing
 , nonumber
   -- * Symbols and utilities
   -- | The unicode approximations do, of course, not reliably represent how
   --   LaTeX renders these symbols.
   
   -- ** Brackets / delimiters
 , autoParens
 , autoSquareBrackets, autoBraces, autoAngleBrackets
 , autoBrackets
 
 , langle , rangle
 , lfloor , rfloor
 , lceil , rceil 
 , dblPipe
   -- ** Superscript and subscript
 , (^:) , (!:), (!^)
   -- ** Function symbols
   -- | Some symbols are preceded with /t/ to be distinguished from
   --   predefined Haskell entities (like 'sin' and 'cos').
 , tsin , arcsin
 , tcos , arccos
 , ttan , arctan
 , cot , arccot
 , tsinh , tcosh , ttanh , coth
 , sec , csc
 , texp
 , tlog , ln
 , tsqrt
   -- ** Custom function symbols
 , operatorname
   -- ** Summation \/ integration \/ differentiation
 , tsum , sumFromTo
 , prod , prodFromTo
 , integral , integralFromTo
 , partial, totald, partialOf, totaldOf
   -- ** Operator symbols
   -- *** Arithmetic
 , (+-), (-+)
 , cdot , times , div_
 , frac
 , (*:) , star
 , circ , bullet
   -- *** Comparison
 , (=:) , (/=:)
 , (<:) , (<=:)
 , (>:) , (>=:)
 , ll , gg
 , equiv
 , propto
 , parallel
 , perp
   -- *** Sets
 , in_ , ni , notin
 , subset , supset
 , cap , cup
 , setminus
   -- *** Misc operators
 , vee , wedge
 , oplus , ominus , otimes
 , oslash , odot
   -- *** Accents
 , hat, tilde, bar, vec, widehat, widetilde
 , dot
 , overline
 
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
 , eta      , theta      , vartheta , thetau
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
 , pm , mp
 , to , mapsto, implies
 , forall , exists
 , dagger, ddagger
 , infty
 , imath, jmath
 , bot
   -- * Fonts
 , mathdefault
 , mathbf
 , mathrm
 , mathcal
 , mathsf
 , mathtt
 , mathit
 ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Writer
import Text.LaTeX.Base.Render


-- | Inline mathematical expressions.
math :: LaTeXC l => l -> l
math = liftL $ TeXMath Dollar

-- | Displayed mathematical expressions, i.e. in a seperate line / block.
mathDisplay :: LaTeXC l => l -> l
mathDisplay = liftL $ TeXMath Square

-------------------------------------------------------
------- Numeric instances for LaTeX and LaTeXT --------
-------------------------------------------------------

----------- LaTeX instances

-- | The 'signum' method uses a custom 'operatorname' and will not be automatically translated by babel.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Num LaTeX where
 (+) = between "+"
 (-) = between "-"
 (*) = (<>)
 negate = (TeXEmpty -)
 fromInteger = rendertex
 abs = autoBrackets "|" "|"
 signum = (operatorname "sgn" <>)

-- | Division uses the LaTeX 'frac' command.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Fractional LaTeX where
 (/) = frac
 fromRational = rendertex . (fromRational :: Rational -> Double)

-- | The 'asinh', 'atanh' and 'acosh' methods use custom 'operatorname's and will not be automatically translated by babel.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Floating LaTeX where
 pi = pi_
 exp = (texp <>)
 sqrt = tsqrt Nothing
 log = (tlog <>)
 (**) = (^:)
 logBase b x = (commS "log" !: b) <> x
 sin = (tsin <>)
 tan = (ttan <>)
 cos = (tcos <>)
 asin = (arcsin <>)
 atan = (arctan <>)
 acos = (arccos <>)
 sinh = (tsinh <>)
 tanh = (ttanh <>)
 cosh = (tcosh <>)
 asinh = (operatorname "arsinh" <>)
 atanh = (operatorname "artanh" <>)
 acosh = (operatorname "arcosh" <>)

----------- LaTeXT instances

#if !MIN_VERSION_base(4,5,0)

-- | Warning: this instance only exists for the 'Num' instance.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Eq (LaTeXT m a) where
 _ == _ = error "Cannot use \"(==)\" Eq method with a LaTeXT value."

-- | Warning: this instance only exists for the 'Num' instance.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Show (LaTeXT m a) where
 show _ = error "Cannot use \"show\" Show method with a LaTeXT value."

#endif

-- | Careful! Method 'signum' is undefined. Don't use it!
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance (Monad m, a ~ ()) => Num (LaTeXT m a) where
 (+) = liftOp (+)
 (-) = liftOp (-)
 (*) = (>>)
 negate = liftFun negate
 fromInteger = fromLaTeX . fromInteger
 abs = liftFun abs
 signum = liftFun signum

-- | Division uses the LaTeX 'frac' command.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance (Monad m, a ~ ()) => Fractional (LaTeXT m a) where
 (/) = liftOp (/)
 fromRational = fromLaTeX . fromRational

-- | Undefined methods: 'asinh', 'atanh' and 'acosh'.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance (Monad m, a ~ ()) => Floating (LaTeXT m a) where
 pi = pi_
 exp = liftFun exp
 sqrt = liftFun sqrt
 log = liftFun log
 (**) = liftOp (**)
 logBase = liftOp logBase
 sin = liftFun sin
 tan = liftFun tan
 cos = liftFun cos
 asin = liftFun asin
 atan = liftFun atan
 acos = liftFun acos
 sinh = liftFun sinh
 tanh = liftFun tanh
 cosh = liftFun cosh
 asinh = liftFun asinh
 atanh = liftFun atanh
 acosh = liftFun acosh

-- | Prevent an equation from being numbered, where the environment would by default do that.
nonumber :: LaTeXC l => l
nonumber = comm0 "nonumber"

-- | A numbered mathematical equation (or otherwise math expression).
equation :: LaTeXC l => l -> l
equation = liftL $ TeXEnv "equation" []

-- | The unnumbered variant of 'equation'.
equation_ :: LaTeXC l => l -> l
equation_ = liftL $ TeXEnv "equation*" []

-------------------------------------
------- Symbols and utilities -------

-- | Surround a LaTeX math expression by parentheses whose height
-- automatically matches the expression's. Translates to @\\left(...\\right)@.
autoParens :: LaTeXC l => l -> l
autoParens x = commS "left(" <> x <> commS "right)"

-- | Like 'autoParens', but with square brackets. Equivalent to @'autoBrackets'\"[\"\"]\"@.
autoSquareBrackets :: LaTeXC l => l -> l
autoSquareBrackets x = commS "left[" <> x <> commS "right]"

-- | Like 'autoParens', but with curly brackets.
autoBraces :: LaTeXC l => l -> l
autoBraces x = commS "left"<>"{" <> x <> commS "right"<>"}"

-- | Like 'autoParens', but with angle brackets \(\langle\) ... \(\rangle\). Equivalent to @'autoBrackets' 'langle' 'rangle'@.
autoAngleBrackets :: LaTeXC l => l -> l
autoAngleBrackets x = commS "left"<>langle <> x <> commS "right"<>rangle

-- | Use custom LaTeX expressions as auto-scaled delimiters to surround math.
-- Suitable delimiters include \(|\ldots|\) (absolute value), \(\|\ldots\|\) (norm,
-- 'dblPipe'), \(\lfloor\ldots\rfloor\) (round-off Gauss brackets, 'lfloor' / 'rfloor') etc..
autoBrackets :: LaTeXC l => LaTeX -> LaTeX -> l -> l
autoBrackets lBrack rBrack x
  = commS "left" <> fromLaTeX lBrack <> x <> commS "right" <> fromLaTeX rBrack

-- | Left angle bracket, \(\langle\).
langle :: LaTeXC l => l
langle = comm0 "langle"

-- | Right angle bracket, \(\rangle\).
rangle :: LaTeXC l => l
rangle = comm0 "rangle"

-- | Left floor, \(\lfloor\).
lfloor :: LaTeXC l => l
lfloor = comm0 "lfloor"

-- | Right floor, \(\rfloor\).
rfloor :: LaTeXC l => l
rfloor = comm0 "rfloor"

-- | Left ceiling, \(\lceil\).
lceil :: LaTeXC l => l
lceil = comm0 "lceil"

-- | Right ceiling, \(\rceil\).
rceil :: LaTeXC l => l
rceil = comm0 "rceil"

-- | Double vertical line, used as delimiter for norms \(\| \ldots \|\).
dblPipe :: LaTeXC l => l
dblPipe = comm0 "|"

-- | Superscript.
(^:) :: LaTeXC l => l -> l -> l
x ^: y = braces x <> raw "^"  <> braces y

-- | Subscript.
(!:) :: LaTeXC l => l -> l -> l
x !: y = braces x <> raw "_" <> braces y

-- | Sub- and superscript, both stacked.
(!^) :: LaTeXC l => l -> (l,l) -> l
x !^ (y,z) = braces x <> raw "_" <> braces y <> raw "^" <> braces z

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

-- | Cotangent function symbol.
cot :: LaTeXC l => l
cot = comm0 "cot"

-- | Arccotangent function symbol.
arccot :: LaTeXC l => l
arccot = comm0 "arccot"

-- | Hyperbolic sine function symbol.
tsinh :: LaTeXC l => l
tsinh = comm0 "sinh"

-- | Hyperbolic cosine function symbol.
tcosh :: LaTeXC l => l
tcosh = comm0 "cosh"

-- | Hyperbolic tangent function symbol.
ttanh :: LaTeXC l => l
ttanh = comm0 "tanh"

-- | Hyperbolic cotangent function symbol.
coth :: LaTeXC l => l
coth = comm0 "coth"

-- | Secant function symbol.
sec :: LaTeXC l => l
sec = comm0 "sec"

-- | Cosecant function symbol.
csc :: LaTeXC l => l
csc = comm0 "csc"

-- | Exponential function symbol.
texp :: LaTeXC l => l
texp = comm0 "exp"

-- | Logarithm function symbol.
tlog :: LaTeXC l => l
tlog = comm0 "log"

-- | Natural logarithm symbol.
ln :: LaTeXC l => l
ln = comm0 "ln"

-- | Root notation. Use @tsqrt (Just n) x@ for the @n@th root of @x@.
--   When 'Nothing' is supplied, the function will output a square root.
tsqrt :: LaTeXC l => Maybe l -> l -> l
tsqrt Nothing  = liftL $ \x -> TeXComm "sqrt" [FixArg x]
tsqrt (Just n) = liftL2 (\m x -> TeXComm "sqrt" [OptArg m, FixArg x]) n

---- Custom Function Symbols
-- | Defines a new function symbol.
-- Note that function symbols defined in this way will not be automatically translated by babel.
operatorname :: LaTeXC l => l -> l
operatorname = comm1 "operatorname"

---- Sum/Integral symbols

-- | Sigma sumation symbol \(\sum\). Use 'sumFromTo' instead if you want to
--   specify the limits of the sum.
tsum :: LaTeXC l => l
tsum = comm0 "sum"

-- | Sigma sumation symbol with limits, like \[\sum_0^n\].
sumFromTo :: LaTeXC l
          => l -- ^ Expression below the sigma.
          -> l -- ^ Expression above the sigma.
          -> l
sumFromTo x y = commS "sum" <> raw"_" <> braces x <> raw"^" <> braces y

-- | Pi product symbol \(\prod\). Use 'prodFromTo' if you want to specify the
--   limits of the product.
prod :: LaTeXC l => l
prod = comm0 "prod"

-- | Pi product symbol with limits, like \[\prod_0^n\].
prodFromTo :: LaTeXC l
           => l -- ^ Expression below the pi.
           -> l -- ^ Expression above the pi.
           -> l
prodFromTo x y = commS "prod" <> raw"_" <> braces x <> raw"^" <> braces y

-- | Integral symbol. Use 'integralFromTo' if you want to specify
--   the limits of the integral.
integral :: LaTeXC l => l
integral = comm0 "int"

-- | Integral symbol with limits of integration. \(\int\limits_{-1}^1\)
integralFromTo :: LaTeXC l
               => l -- ^ Lower limit of integration.
               -> l -- ^ Upper limit of integration.
               -> l
integralFromTo x y = commS "int" <> commS "limits" <> raw"_" <> braces x <> raw"^" <> braces y

-- | Partial-differentiation symbol \(\partial\)
partial :: LaTeXC l => l
partial = comm0 "partial"

-- | Total-differentiation (or integration-variable) symbol d (non-italic!)
--   To be used as @frac (totald) (totald<>"x")@ → \(\frac{\mathrm{d}}{\mathrm{d}x}\).
totald :: LaTeXC l => l
totald = mathrm "d"

-- | Partial-differentiation of variable, e.g.
--   @frac (partialOf h) (partialOf t)@ → \(\frac{\partial h}{\partial t}\).
partialOf :: LaTeXC l => l -> l
partialOf v = comm0 "partial" <> v

-- | Total-differentiation of variable, or integration over variable.
--   To be used as
-- 
-- @
-- integralFromTo 0 infty $ totaldOf "x" <> "f(x)"
-- @
-- 
--   \[\int\limits_0^\infty\mathrm{d}x f(x),\] or
--   @frac (totaldOf"f") (totaldOf"x")@ → \(\frac{\mathrm{d}f}{\mathrm{d}x}\).
totaldOf :: LaTeXC l => l -> l
totaldOf v = mathrm "d" <> v

---- Operator symbols

-- | Negative form of an operator.
notop :: LaTeXC l =>
         (l -> l -> l)
      ->  l -> l -> l
notop op =
 \l1 l2 ->
   (l1 <> commS "not") `op` l2

infixl 6 +-, -+

-- | Plus-or-minus operator \(\pm\). Also available as symbol 'pm'.
(+-) :: LaTeXC l => l -> l -> l
(+-)  = between $ comm0 "pm"

-- | Minus-or-plus operator \(\mp\). Also available as symbol 'mp'.
(-+) :: LaTeXC l => l -> l -> l
(-+)  = between $ comm0 "mp"

-- | Centered-dot operator \(\cdot\).
cdot :: LaTeXC l => l -> l -> l
cdot  = between $ comm0 "cdot"

-- | \"x-cross\" multiplication operator \(\times\).
times :: LaTeXC l => l -> l -> l
times = between $ comm0 "times"

-- | Division operator \(\div\).
div_ :: LaTeXC l => l -> l -> l
div_  = between $ comm0 "div"

-- | Fraction operator, like @frac 1 2@ → \(\frac12\).
frac :: LaTeXC l => l -> l -> l
frac = liftL2 $ \p q -> TeXComm "frac" [FixArg p, FixArg q]

infixl 7 *:

-- | Asterisk operator \(\ast\).
--
-- > infixl 7 *:
(*:) :: LaTeXC l => l -> l -> l
(*:) = between $ comm0 "ast"

-- | Star operator \(\star\).
star :: LaTeXC l => l -> l -> l
star  = between $ comm0 "star"

-- | Ring operator \(\circ\).
circ :: LaTeXC l => l -> l -> l
circ  = between $ comm0 "circ"

-- | Bullet operator \(\bullet\).
bullet :: LaTeXC l => l -> l -> l
bullet  = between $ comm0 "bullet"

infixr 4 =: , /=:

-- | Simple equals sign \(=\).
--
-- > infixr 4 =:
(=:) :: LaTeXC l => l -> l -> l
(=:)  = between "="

-- | Not equal \(\neq\).
--
-- > infixr 4 /=:
(/=:) :: LaTeXC l => l -> l -> l
(/=:) = notop (=:)

-- | Greater \(>\).
(>:) :: LaTeXC l => l -> l -> l
(>:) = between ">"

-- | Greater or equal \(\geq\).
(>=:) :: LaTeXC l => l -> l -> l
(>=:) = between $ comm0 "geq"

-- | Lesser \(<\).
(<:) :: LaTeXC l => l -> l -> l
(<:) = between "<"

-- | Lesser or equal \(\leq\).
(<=:) :: LaTeXC l => l -> l -> l
(<=:) = between $ comm0 "leq"

-- | Much less \(\ll\).
ll :: LaTeXC l => l -> l -> l
ll = between $ comm0 "ll"

-- | Much greater \(\gg\).
gg :: LaTeXC l => l -> l -> l
gg = between $ comm0 "gg"

-- | Proportional-to \(\propto\).
propto :: LaTeXC l => l -> l -> l
propto  = between $ comm0 "propto"

-- | Perpendicular \(\perp\). This is the infix version of 'bot'.
perp :: LaTeXC l => l -> l -> l
perp = between $ comm0 "perp"

-- | Parallel \(\parallel\).
parallel :: LaTeXC l => l -> l -> l
parallel = between $ comm0 "parallel"

-- | Identical \/ defined-as \/ equivalent \(\equiv\).
equiv :: LaTeXC l => l -> l -> l
equiv  = between $ comm0 "equiv"

-- | Element-of \(\in\).
in_ :: LaTeXC l => l -> l -> l
in_ = between $ comm0 "in"

-- | Mirrored element-of \(\ni\).
ni :: LaTeXC l => l -> l -> l
ni  = between $ comm0 "ni"

-- | Not element of \(\notin\).
notin :: LaTeXC l => l -> l -> l
notin = between $ comm0 "notin"

-- | Subset-of \(\subset\).
subset :: LaTeXC l => l -> l -> l
subset  = between $ comm0 "subset"

-- | Superset-of \(supset\).
supset :: LaTeXC l => l -> l -> l
supset  = between $ comm0 "supset"

-- | Set intersection \(\cap\).
cap :: LaTeXC l => l -> l -> l
cap  = between $ comm0 "cap"

-- | Set union \(\cup\).
cup :: LaTeXC l => l -> l -> l
cup  = between $ comm0 "cup"

-- | Set minus \(\setminus\).
setminus :: LaTeXC l => l -> l -> l
setminus  = between $ comm0 "setminus"

-- | Angle pointing downwards \(\vee\).
vee :: LaTeXC l => l -> l -> l
vee  = between $ comm0 "vee"

-- | Angle pointing upwards \(\wedge\).
wedge :: LaTeXC l => l -> l -> l
wedge  = between $ comm0 "wedge"

-- | Circled plus operator \(\oplus\).
oplus :: LaTeXC l => l -> l -> l
oplus  = between $ comm0 "oplus"

-- | Circled minus operator \(\ominus\).
ominus :: LaTeXC l => l -> l -> l
ominus  = between $ comm0 "ominus"

-- | Circled multiplication cross \(\otimes\).
otimes :: LaTeXC l => l -> l -> l
otimes  = between $ comm0 "otimes"

-- | Circled slash \(\oslash\).
oslash :: LaTeXC l => l -> l -> l
oslash  = between $ comm0 "oslash"

-- | Circled dot operator \(\odot\).
odot :: LaTeXC l => l -> l -> l
odot  = between $ comm0 "odot"

--- Accemts
{-
 , hat, tilde, bar, vec, widehat, widetilde
 , dot, ddot, dddot
 , overline, underline
 -}

-- | Add a hat accent above a symbol, like \(\hat{x}\).
hat :: LaTeXC l => l -> l
hat = comm1 "hat"

-- | Add a tilde accent above a symbol, like \(\tilde{y}\).
tilde :: LaTeXC l => l -> l
tilde = comm1 "tilde"

-- | Add a bar accent above a symbol, like \(\bar{z}\).
bar :: LaTeXC l => l -> l
bar = comm1 "bar"

-- | Add a vector arrow accent above a symbol, like \(\vec{v}\).
vec :: LaTeXC l => l -> l
vec = comm1 "vec"

-- | Add a wide hat accent above a symbol, like \(\widehat{w}\).
widehat :: LaTeXC l => l -> l
widehat = comm1 "widehat"

-- | Add a wide tilde accent above a symbol, like \(\widetilde{u}\).
widetilde :: LaTeXC l => l -> l
widetilde = comm1 "widetilde"

-- | Add a dot accent above a symbol, as used to denote a derivative,
--   like \(\dot{x}\).
dot :: LaTeXC l => l -> l
dot = comm1 "dot"

-- | Add a wide line accent above a symbol, like \(\overline{abc}\).
overline :: LaTeXC l => l -> l
overline = comm1 "overline"

---- Greek alphabet

-- | \(\alpha\) symbol.
alpha :: LaTeXC l => l
alpha = comm0 "alpha"

-- | \(\beta\) symbol.
beta :: LaTeXC l => l
beta = comm0 "beta"

-- | \(\gamma\) symbol.
gamma :: LaTeXC l => l
gamma = comm0 "gamma"

-- | \(\Gamma\) symbol.
gammau :: LaTeXC l => l
gammau = comm0 "Gamma"

-- | \(\delta\) symbol.
delta :: LaTeXC l => l
delta = comm0 "delta"

-- | \(\Delta\) symbol.
deltau :: LaTeXC l => l
deltau = comm0 "Delta"

-- | \(\epsilon\) symbol.
epsilon :: LaTeXC l => l
epsilon = comm0 "epsilon"

-- | \(\varepsilon\) symbol.
varepsilon :: LaTeXC l => l
varepsilon = comm0 "varepsilon"

-- | \(\zeta\) symbol.
zeta :: LaTeXC l => l
zeta = comm0 "zeta"

-- | \(\eta\) symbol.
eta :: LaTeXC l => l
eta = comm0 "eta"

-- | \(\theta\) symbol.
theta :: LaTeXC l => l
theta = comm0 "theta"

-- | \(\vartheta\) symbol.
vartheta :: LaTeXC l => l
vartheta = comm0 "vartheta"

-- | \(\Theta\) symbol.
thetau :: LaTeXC l => l
thetau = comm0 "Theta"

-- | \(\iota\) symbol.
iota :: LaTeXC l => l
iota = comm0 "iota"

-- | \(\kappa\) symbol.
kappa :: LaTeXC l => l
kappa = comm0 "kappa"

-- | \(\lambda\) symbol.
lambda :: LaTeXC l => l
lambda = comm0 "lambda"

-- | \(\Lambda\) symbol.
lambdau :: LaTeXC l => l
lambdau = comm0 "Lambda"

-- | \(\mu\) symbol.
mu :: LaTeXC l => l
mu = comm0 "mu"

-- | \(\nu\) symbol.
nu :: LaTeXC l => l
nu = comm0 "nu"

-- | \(\xi\) symbol.
xi :: LaTeXC l => l
xi = comm0 "xi"

-- | \(\Xi\) symbol.
xiu :: LaTeXC l => l
xiu = comm0 "Xi"

-- | \(\pi\) symbol.
pi_ :: LaTeXC l => l
pi_ = comm0 "pi"

-- | \(\varpi\) symbol.
varpi :: LaTeXC l => l
varpi = comm0 "varpi"

-- | \(\Pi\) symbol.
piu :: LaTeXC l => l
piu = comm0 "Pi"

-- | \(\rho\) symbol.
rho :: LaTeXC l => l
rho = comm0 "rho"

-- | \(\varrho\) symbol.
varrho :: LaTeXC l => l
varrho = comm0 "varrho"

-- | \(\sigma\) symbol.
sigma :: LaTeXC l => l
sigma = comm0 "sigma"

-- | \(\varsigma\) symbol.
varsigma :: LaTeXC l => l
varsigma = comm0 "varsigma"

-- | \(\Sigma\) symbol.
sigmau :: LaTeXC l => l
sigmau = comm0 "Sigma"

-- | \(\tau\) symbol.
tau :: LaTeXC l => l
tau = comm0 "tau"

-- | \(\upsilon\) symbol.
upsilon :: LaTeXC l => l
upsilon = comm0 "upsilon"

-- | \(\Upsilon\) symbol.
upsilonu :: LaTeXC l => l
upsilonu = comm0 "Upsilon"

-- | \(\phi\) symbol.
phi :: LaTeXC l => l
phi = comm0 "phi"

-- | \(\varphi\) symbol.
varphi :: LaTeXC l => l
varphi = comm0 "varphi"

-- | \(\Phi\) symbol.
phiu :: LaTeXC l => l
phiu = comm0 "Phi"

-- | \(\chi\) symbol.
chi :: LaTeXC l => l
chi = comm0 "chi"

-- | \(\psi\) symbol.
psi :: LaTeXC l => l
psi = comm0 "psi"

-- | \(\Psi\) symbol.
psiu :: LaTeXC l => l
psiu = comm0 "Psi"

-- | \(\omega\) symbol.
omega :: LaTeXC l => l
omega = comm0 "omega"

-- | \(\Omega\) symbol.
omegau :: LaTeXC l => l
omegau = comm0 "Omega"

---- Other symbols

-- | Plus-or-minus symbol \(\pm\). Also available as infix '+-'.
pm :: LaTeXC l => l
pm  = comm0 "pm"

-- | Minus-or-plus symbol \(\mp\).
mp :: LaTeXC l => l
mp  = comm0 "mp"

-- | A right-arrow, \(\to\).
to :: LaTeXC l => l
to = comm0 "to"

-- | A right-arrow for function definitions, \(\mapsto\).
mapsto :: LaTeXC l => l
mapsto = comm0 "mapsto"

-- | An implication arrow, \(\implies\).
implies :: LaTeXC l => l
implies = comm0 "implies"

-- | /For all/ symbol, \(\forall\).
forall :: LaTeXC l => l
forall = comm0 "forall"

-- | /Exists/ symbol, \(\exists\).
exists :: LaTeXC l => l
exists = comm0 "exists"

-- | Dagger symbol, \(\dagger\).
dagger :: LaTeXC l => l
dagger = comm0 "dagger"

-- | Double dagger symbol, \(\ddagger\).
ddagger :: LaTeXC l => l
ddagger = comm0 "ddagger"

-- | Infinity symbol, \(\infty\).
infty :: LaTeXC l => l
infty = comm0 "infty"

-- | Dotless letter i, \(\imath\).
imath :: LaTeXC l => l
imath = comm0 "imath"

-- | Dotless letter j, \(\jmath\).
jmath :: LaTeXC l => l
jmath = comm0 "jmath"

-- | Bottom symbol, \(\bot\). For the infix version see 'perp'.
bot :: LaTeXC l => l
bot = comm0 "bot"

-------------------------------------
------------ Math Fonts -------------

-- | Default math symbol font.
mathdefault :: LaTeXC l => l -> l
mathdefault = comm1 "mathdefault"

-- | Bold face, like \(\mathbf{Abc}\).
mathbf :: LaTeXC l => l -> l
mathbf = comm1 "mathbf"

-- | Roman, i.e. not-italic math, \(\mathrm{deF}\)
mathrm :: LaTeXC l => l -> l
mathrm = comm1 "mathrm"

-- | Calligraphic math symbols. Only works (reliably) with uppercase letters, like \(\mathcal{LMN}\).
mathcal :: LaTeXC l => l -> l
mathcal = comm1 "mathcal"

-- | Sans-serif math, \(\mathsf{xyz}\).
mathsf :: LaTeXC l => l -> l
mathsf = comm1 "mathsf"

-- | Typewriter font, \(\mathtt{ijk}\).
mathtt :: LaTeXC l => l -> l
mathtt = comm1 "mathtt"

-- | Italic math. Uses the same glyphs as 'mathdefault', but with spacings
--   intended for multi-character symbols rather than juxtaposition of single-character symbols.
mathit :: LaTeXC l => l -> l
mathit = comm1 "mathit"


