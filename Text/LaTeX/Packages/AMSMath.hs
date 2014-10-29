
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

-- | \AMSMath\ support. Also numeric instances ('Num', 'Fractional' and 'Floating') for 'LaTeX' and 'LaTeXT'.
module Text.LaTeX.Packages.AMSMath
 ( -- * AMSMath package
   amsmath
   -- * Math Environments
 , math, mathDisplay
 , equation , equation_
 , align , align_
   -- ** Referencing
 , eqref , nonumber
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
 , (^:) , (!:)
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
   -- ** Sum/Integral symbols
 , tsum , sumFromTo
 , prod , prodFromTo
 , integral , integralFromTo
   -- ** Operator symbols
   -- *** Arithmetic
 , pm , mp
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
   -- *** Sets
 , in_ , ni , notin
 , subset , supset
 , cap , cup
 , setminus
   -- *** Misc operators
 , vee , wedge
 , oplus , ominus , otimes
 , oslash , odot
 
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
 , to , mapsto
 , forall , exists
 , dagger, ddagger
 , infty
   -- * Fonts
 , mathdefault
 , mathbf
 , mathrm
 , mathcal
 , mathsf
 , mathtt
 , mathit
   -- * Matrices
 , pmatrix  , bmatrix
 , b2matrix , vmatrix
 , v2matrix
   -- * Math spacing
 , quad, qquad
   ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class

-- External imports
import Data.List
import Data.Ratio
import Data.Matrix

-- | AMSMath package.
-- Example:
--
-- > usepackage [] amsmath
amsmath :: PackageName
amsmath = "amsmath"

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

-- | Careful! Method 'signum' is undefined. Don't use it!
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Num LaTeX where
 (+) = between "+"
 (-) = between "-"
 (*) = (<>)
 negate = (TeXEmpty -)
 fromInteger = rendertex
 abs = autoBrackets "|" "|"
 -- Non-defined methods
 signum _ = error "Cannot use \"signum\" Num method with a LaTeX value."

-- | Division uses the LaTeX 'frac' command.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Fractional LaTeX where
 (/) = frac
 fromRational = rendertex . (fromRational :: Rational -> Double)

-- | Undefined methods: 'asinh', 'atanh' and 'acosh'.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Floating LaTeX where
 pi = pi_
 exp = (texp <>)
 sqrt = tsqrt Nothing
 log = (tlog <>)
 (**) = (^:)
 logBase b x = (tlog !: b) <> x
 sin = (tsin <>)
 tan = (ttan <>)
 cos = (tcos <>)
 asin = (arcsin <>)
 atan = (arctan <>)
 acos = (arccos <>)
 sinh = (tsinh <>)
 tanh = (ttanh <>)
 cosh = (tcosh <>)
 -- Non-defined methods
 asinh = error "Function 'asinh' is not defined in AMSMath!"
 atanh = error "Function 'atabh' is not defined in AMSMath!"
 acosh = error "Function 'acosh' is not defined in AMSMath!"

----------- LaTeXT instances

-- | Warning: this instance only exists for the 'Num' instance.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Eq (LaTeXT m a) where
 _ == _ = error "Cannot use \"(==)\" Eq method with a LaTeXT value."

-- | Warning: this instance only exists for the 'Num' instance.
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance Show (LaTeXT m a) where
 show _ = error "Cannot use \"show\" Show method with a LaTeXT value."

-- | Careful! Method 'signum' is undefined. Don't use it!
--   This instance is defined in the "Text.LaTeX.Packages.AMSMath" module.
instance (Monad m, a ~ ()) => Num (LaTeXT m a) where
 (+) = liftOp (+)
 (-) = liftOp (-)
 (*) = (>>)
 negate = liftFun negate
 fromInteger = fromLaTeX . fromInteger
 abs = liftFun abs
 -- Non-defined methods
 signum _ = error "Cannot use \"signum\" Num method with a LaTeXT value."

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
 -- Non-defined methods
 asinh = error "Function 'asinh' is not defined in AMSMath!"
 atanh = error "Function 'atabh' is not defined in AMSMath!"
 acosh = error "Function 'acosh' is not defined in AMSMath!"

-- | A reference to a numbered equation. Use with a 'label' defined in the
-- scope of the equation refered to.
eqref :: LaTeXC l => l -> l
eqref = liftL $ \l -> TeXComm "eqref" [FixArg . TeXRaw $ render l]

-- | Prevent an equation from being numbered, where the environment would by default do that.
nonumber :: LaTeXC l => l
nonumber = comm0 "nonumber"

-- | A numbered mathematical equation (or otherwise math expression).
equation :: LaTeXC l => l -> l
equation = liftL $ TeXEnv "equation" []

-- | The unnumbered variant of 'equation'.
equation_ :: LaTeXC l => l -> l
equation_ = liftL $ TeXEnv "equation*" []

-- | An array of aligned equations. Use '&' to specify the points that should
-- horizontally match. Each equation is numbered, unless prevented by 'nonumber'.
align :: LaTeXC l => [l] -> l
align = liftL(TeXEnv "align" []) . mconcat . intersperse lnbk 

-- | The unnumbered variant of 'align'.
align_ :: LaTeXC l => [l] -> l
align_ = liftL(TeXEnv "align*" []) . mconcat . intersperse lnbk 

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

-- | Like 'autoParens', but with angle brackets 〈 ... 〉. Equivalent to @'autoBrackets' 'langle' 'rangle'@.
autoAngleBrackets :: LaTeXC l => l -> l
autoAngleBrackets x = commS "left"<>langle <> x <> commS "right"<>rangle

-- | Use custom LaTeX expressions as auto-scaled delimiters to surround math.
-- Suitable delimiters include |...| (absolute value), ‖...‖ (norm,
-- 'dblPipe'), ⌊...⌋ (round-off Gauss brackets, 'lfloor' / 'rfloor') etc..
autoBrackets :: LaTeXC l => LaTeX -> LaTeX -> l -> l
autoBrackets lBrack rBrack x
  = commS "left" <> braces (fromLaTeX lBrack) <> x <> commS "right" <> braces (fromLaTeX rBrack)

-- | Left angle bracket, 〈.
langle :: LaTeXC l => l
langle = comm0 "langle"

-- | Right angle bracket, 〉.
rangle :: LaTeXC l => l
rangle = comm0 "rangle"

-- | Left floor, ⌊.
lfloor :: LaTeXC l => l
lfloor = comm0 "lfloor"

-- | Right floor, ⌋.
rfloor :: LaTeXC l => l
rfloor = comm0 "rfloor"

-- | Left ceiling, ⌈.
lceil :: LaTeXC l => l
lceil = comm0 "lceil"

-- | Right ceiling, ⌉.
rceil :: LaTeXC l => l
rceil = comm0 "rceil"

-- | Double vertical line, used as delimiter for norms (‖ ... ‖).
dblPipe :: LaTeXC l => l
dblPipe = comm0 "|"

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

---- Sum/Integral symbols

-- | Sigma sumation symbol. Use 'sumFromTo' instead if you want to
--   specify the limits of the sum.
tsum :: LaTeXC l => l
tsum = comm0 "sum"

-- | Sigma sumation symbol with limits.
sumFromTo :: LaTeXC l
          => l -- ^ Expression below the sigma.
          -> l -- ^ Expression above the sigma.
          -> l
sumFromTo x y = commS "sum" !: x ^: y

-- | Pi product symbol. Use 'prodFromTo' if you want to specify the
--   limits of the product.
prod :: LaTeXC l => l
prod = comm0 "prod"

-- | Pi product symbol with limits.
prodFromTo :: LaTeXC l
           => l -- ^ Expression below the pi.
           -> l -- ^ Expression above the pi.
           -> l
prodFromTo x y = commS "prod" !: x ^: y

-- | Integral symbol. Use 'integralFromTo' if you want to specify
--   the limits of the integral.
integral :: LaTeXC l => l
integral = comm0 "int"

-- | Integral symbol with limits of integration.
integralFromTo :: LaTeXC l
               => l -- ^ Lower limit of integration.
               -> l -- ^ Upper limit of integration.
               -> l
integralFromTo x y = commS "int" !: x ^: y

---- Operator symbols

-- | Negative form of an operator.
notop :: LaTeXC l =>
         (l -> l -> l)
      ->  l -> l -> l
notop op =
 \l1 l2 ->
   (l1 <> commS "not") `op` l2

-- | Plus-or-minus operator (±).
pm :: LaTeXC l => l -> l -> l
pm  = between $ comm0 "pm"

-- | Minus-or-plus operator (∓).
mp :: LaTeXC l => l -> l -> l
mp  = between $ comm0 "mp"

-- | Centered-dot operator (⋅).
cdot :: LaTeXC l => l -> l -> l
cdot  = between $ comm0 "cdot"

-- | \"x-cross\" multiplication operator (×).
times :: LaTeXC l => l -> l -> l
times = between $ comm0 "times"

-- | Division operator.
div_ :: LaTeXC l => l -> l -> l
div_  = between $ comm0 "div"

-- | Fraction operator.
frac :: LaTeXC l => l -> l -> l
frac = liftL2 $ \p q -> TeXComm "frac" [FixArg p, FixArg q]

infixl 7 *:

-- | Asterisk operator (*).
--
-- > infixl 7 *:
(*:) :: LaTeXC l => l -> l -> l
(*:) = between $ comm0 "ast"

-- | Star operator (★).
star :: LaTeXC l => l -> l -> l
star  = between $ comm0 "star"

-- | Ring operator (∘).
circ :: LaTeXC l => l -> l -> l
circ  = between $ comm0 "circ"

-- | Bullet operator (∙).
bullet :: LaTeXC l => l -> l -> l
bullet  = between $ comm0 "bullet"

infixr 4 =: , /=:

-- | Equal.
--
-- > infixr 4 =:
(=:) :: LaTeXC l => l -> l -> l
(=:)  = between "="

-- | Not equal (≠).
--
-- > infixr 4 /=:
(/=:) :: LaTeXC l => l -> l -> l
(/=:) = notop (=:)

-- | Greater.
(>:) :: LaTeXC l => l -> l -> l
(>:) = between ">"

-- | Greater or equal (≥).
(>=:) :: LaTeXC l => l -> l -> l
(>=:) = between $ comm0 "geq"

-- | Lesser.
(<:) :: LaTeXC l => l -> l -> l
(<:) = between "<"

-- | Lesser or equal (≤).
(<=:) :: LaTeXC l => l -> l -> l
(<=:) = between $ comm0 "leq"

-- | Much less (≪).
ll :: LaTeXC l => l -> l -> l
ll = between $ comm0 "ll"

-- | Much greater (≫).
gg :: LaTeXC l => l -> l -> l
gg = between $ comm0 "gg"

-- | Proportional-to (∝).
propto :: LaTeXC l => l -> l -> l
propto  = between $ comm0 "propto"

-- | Identical \/ defined-as \/ equivalent (≡).
equiv :: LaTeXC l => l -> l -> l
equiv  = between $ comm0 "equiv"

-- | Element-of (∈).
in_ :: LaTeXC l => l -> l -> l
in_ = between $ comm0 "in"

-- | Mirrored element-of (∋).
ni :: LaTeXC l => l -> l -> l
ni  = between $ comm0 "ni"

-- | Not element of (∉).
notin :: LaTeXC l => l -> l -> l
notin = between $ comm0 "notin"

-- | Subset-of (⊂).
subset :: LaTeXC l => l -> l -> l
subset  = between $ comm0 "subset"

-- | Superset-of (⊃).
supset :: LaTeXC l => l -> l -> l
supset  = between $ comm0 "supset"

-- | Set intersection (∩).
cap :: LaTeXC l => l -> l -> l
cap  = between $ comm0 "cap"

-- | Set union (∪).
cup :: LaTeXC l => l -> l -> l
cup  = between $ comm0 "cup"

-- | Set minus (∖).
setminus :: LaTeXC l => l -> l -> l
setminus  = between $ comm0 "setminus"

-- | Angle pointing downwards (∨).
vee :: LaTeXC l => l -> l -> l
vee  = between $ comm0 "vee"

-- | Angle pointing upwards (∧).
wedge :: LaTeXC l => l -> l -> l
wedge  = between $ comm0 "wedge"

-- | Circled plus operator (⊕).
oplus :: LaTeXC l => l -> l -> l
oplus  = between $ comm0 "oplus"

-- | Circled minus operator (⊖).
ominus :: LaTeXC l => l -> l -> l
ominus  = between $ comm0 "ominus"

-- | Circled multiplication cross (⊗).
otimes :: LaTeXC l => l -> l -> l
otimes  = between $ comm0 "otimes"

-- | Circled slash (⊘).
oslash :: LaTeXC l => l -> l -> l
oslash  = between $ comm0 "oslash"

-- | Circled dot operator (⊙).
odot :: LaTeXC l => l -> l -> l
odot  = between $ comm0 "odot"

---- Greek alphabet

-- | /α/ symbol.
alpha :: LaTeXC l => l
alpha = comm0 "alpha"

-- | /β/ symbol.
beta :: LaTeXC l => l
beta = comm0 "beta"

-- | /γ/ symbol.
gamma :: LaTeXC l => l
gamma = comm0 "gamma"

-- | Γ symbol.
gammau :: LaTeXC l => l
gammau = comm0 "Gamma"

-- | /δ/ symbol.
delta :: LaTeXC l => l
delta = comm0 "delta"

-- | Δ symbol.
deltau :: LaTeXC l => l
deltau = comm0 "Delta"

-- | /ϵ/ symbol.
epsilon :: LaTeXC l => l
epsilon = comm0 "epsilon"

-- | /ε/ symbol.
varepsilon :: LaTeXC l => l
varepsilon = comm0 "varepsilon"

-- | /ζ/ symbol.
zeta :: LaTeXC l => l
zeta = comm0 "zeta"

-- | /η/ symbol.
eta :: LaTeXC l => l
eta = comm0 "eta"

-- | /θ/ symbol.
theta :: LaTeXC l => l
theta = comm0 "theta"

-- | /ϑ/ symbol.
vartheta :: LaTeXC l => l
vartheta = comm0 "vartheta"

-- | Θ symbol.
thetau :: LaTeXC l => l
thetau = comm0 "thetau"

-- | /ι/ symbol.
iota :: LaTeXC l => l
iota = comm0 "iota"

-- | /κ/ symbol.
kappa :: LaTeXC l => l
kappa = comm0 "kappa"

-- | /λ/ symbol.
lambda :: LaTeXC l => l
lambda = comm0 "lambda"

-- | Λ symbol.
lambdau :: LaTeXC l => l
lambdau = comm0 "Lambda"

-- | /μ/ symbol.
mu :: LaTeXC l => l
mu = comm0 "mu"

-- | /ν/ symbol.
nu :: LaTeXC l => l
nu = comm0 "nu"

-- | /ξ/ symbol.
xi :: LaTeXC l => l
xi = comm0 "xi"

-- | Ξ symbol.
xiu :: LaTeXC l => l
xiu = comm0 "Xi"

-- | /π/ symbol.
pi_ :: LaTeXC l => l
pi_ = comm0 "pi"

-- | /ϖ/ symbol.
varpi :: LaTeXC l => l
varpi = comm0 "varpi"

-- | Π symbol.
piu :: LaTeXC l => l
piu = comm0 "Pi"

-- | /ρ/ symbol.
rho :: LaTeXC l => l
rho = comm0 "rho"

-- | /ϱ/ symbol.
varrho :: LaTeXC l => l
varrho = comm0 "varrho"

-- | /σ/ symbol.
sigma :: LaTeXC l => l
sigma = comm0 "sigma"

-- | /ς/ symbol.
varsigma :: LaTeXC l => l
varsigma = comm0 "varsigma"

-- | Σ symbol.
sigmau :: LaTeXC l => l
sigmau = comm0 "Sigma"

-- | /τ/ symbol.
tau :: LaTeXC l => l
tau = comm0 "tau"

-- | /υ/ symbol.
upsilon :: LaTeXC l => l
upsilon = comm0 "upsilon"

-- | Υ symbol.
upsilonu :: LaTeXC l => l
upsilonu = comm0 "Upsilon"

-- | /ϕ/ symbol.
phi :: LaTeXC l => l
phi = comm0 "phi"

-- | /φ/ symbol.
varphi :: LaTeXC l => l
varphi = comm0 "varphi"

-- | Φ symbol.
phiu :: LaTeXC l => l
phiu = comm0 "Phi"

-- | /χ/ symbol.
chi :: LaTeXC l => l
chi = comm0 "chi"

-- | /ψ/ symbol.
psi :: LaTeXC l => l
psi = comm0 "psi"

-- | Ψ symbol.
psiu :: LaTeXC l => l
psiu = comm0 "Psi"

-- | /ω/ symbol.
omega :: LaTeXC l => l
omega = comm0 "omega"

-- | Ω symbol.
omegau :: LaTeXC l => l
omegau = comm0 "Omega"

---- Other symbols

-- | A right-arrow, →.
to :: LaTeXC l => l
to = comm0 "to"

-- | A right-arrow for function definitions, ↦.
mapsto :: LaTeXC l => l
mapsto = comm0 "mapsto"

-- | /For all/ symbol, ∀.
forall :: LaTeXC l => l
forall = comm0 "forall"

-- | /Exists/ symbol, ∃.
exists :: LaTeXC l => l
exists = comm0 "exists"

-- | Dagger symbol, †.
dagger :: LaTeXC l => l
dagger = comm0 "dagger"

-- | Double dagger symbol, ‡.
ddagger :: LaTeXC l => l
ddagger = comm0 "ddagger"

-- | Infinity symbol.
infty :: LaTeXC l => l
infty = comm0 "infty"

-------------------------------------
------------ Math Fonts -------------

-- | Default math symbol font.
mathdefault :: LaTeXC l => l -> l
mathdefault = liftL $ \l -> TeXComm "mathdefault" [FixArg l]

-- | Bold face.
mathbf :: LaTeXC l => l -> l
mathbf = liftL $ \l -> TeXComm "mathbf" [FixArg l]

-- | Roman, i.e. not-italic math.
mathrm :: LaTeXC l => l -> l
mathrm = liftL $ \l -> TeXComm "mathrm" [FixArg l]

-- | Calligraphic math symbols.
mathcal :: LaTeXC l => l -> l
mathcal = liftL $ \l -> TeXComm "mathcal" [FixArg l]

-- | Sans-serif math.
mathsf :: LaTeXC l => l -> l
mathsf = liftL $ \l -> TeXComm "mathsf" [FixArg l]

-- | Typewriter font.
mathtt :: LaTeXC l => l -> l
mathtt = liftL $ \l -> TeXComm "mathtt" [FixArg l]

-- | Italic math. Uses the same glyphs as 'mathdefault', but with spacings
--   intended for multi-character symbols rather than juxtaposition of single-character symbols.
mathit :: LaTeXC l => l -> l
mathit = liftL $ \l -> TeXComm "mathit" [FixArg l]

-------------------------------------
------------- Matrices --------------

matrix2tex :: (Texy a, LaTeXC l) => Matrix a -> l
matrix2tex m = mconcat
 [ foldr1 (&) [ texy $ m ! (i,j)
     | j <- [1 .. ncols m]
     ] <> lnbk
     | i <- [1 .. nrows m]
   ]

toMatrix :: (Texy a, LaTeXC l) => String -> Maybe HPos -> Matrix a -> l
toMatrix str Nothing  = liftL (TeXEnv str []) . matrix2tex
toMatrix str (Just p) = liftL (TeXEnv (str ++ "*") [OptArg $ rendertex p]) . matrix2tex

-- | LaTeX rendering of a matrix using @pmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- > ( M )
--
pmatrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
pmatrix = toMatrix "pmatrix"

-- | LaTeX rendering of a matrix using @bmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- > [ M ]
--
bmatrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
bmatrix = toMatrix "bmatrix"

-- | LaTeX rendering of a matrix using @Bmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- > { M }
--
b2matrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
b2matrix = toMatrix "Bmatrix"

-- | LaTeX rendering of a matrix using @vmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- > | M |
--
vmatrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
vmatrix = toMatrix "vmatrix"

-- | LaTeX rendering of a matrix using @Vmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- > || M ||
--
v2matrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
v2matrix = toMatrix "Vmatrix"

-------------------------------------
---------- Texy instances -----------

-- | Instance defined in "Text.LaTeX.Packages.AMSMath".
instance (Integral a, Texy a) => Texy (Ratio a) where
 texy x = frac (texy $ numerator x) (texy $ denominator x)

-- | Instance defined in "Text.LaTeX.Packages.AMSMath".
instance (Texy a, Texy b) => Texy (a,b) where
 texy (x,y) = autoParens $ texy x <> "," <> texy y
 
-- | Instance defined in "Text.LaTeX.Packages.AMSMath".
instance (Texy a, Texy b, Texy c) => Texy (a,b,c) where
 texy (x,y,z) = autoParens $ texy x <> "," <> texy y <> "," <> texy z

-- | Instance defined in "Text.LaTeX.Packages.AMSMath".
instance (Texy a, Texy b, Texy c, Texy d) => Texy (a,b,c,d) where
 texy (a,b,c,d) = autoParens $ texy a <> "," <> texy b <> "," <> texy c <> "," <> texy d

-- | Instance defined in "Text.LaTeX.Packages.AMSMath".
instance Texy a => Texy (Matrix a) where
 texy = pmatrix Nothing

-- | Instance defined in "Text.LaTeX.Packages.AMSMath".
instance Texy a => Texy [a] where
 texy = autoSquareBrackets . mconcat .  intersperse "," .  fmap texy

-------------------------------------
----------- Math Spacing-------------

-- | \quad space equal to the current font size (= 18 mu)
quad :: LaTeXC l => l
quad = comm0 "quad"

-- | \qquad twice of \quad (= 36 mu) 
qquad :: LaTeXC l => l
qquad = comm0 "qquad"

{-
  The following commands are pending. Someone needs to find suitable
  names for them.

  \, 				3/18 of \quad (= 3 mu)
  \: 				4/18 of \quad (= 4 mu)
  \; 				5/18 of \quad (= 5 mu)
  \! 				-3/18 of \quad (= -3 mu)
  \ (space after backslash!) 	equivalent of space in normal text

-}
