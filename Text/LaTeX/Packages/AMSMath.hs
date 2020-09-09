
{-# LANGUAGE CPP, OverloadedStrings, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 801
{-# OPTIONS_GHC -Wno-orphans #-}
#else
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

-- | <https://ctan.org/pkg/amsmath?lang=en amsmath> is a de-facto standard
--  package for maths in LaTeX. It is used in most scientific documents
--  and also available by default in MathJax.
-- 
-- Note that many of the maths commands this module exports are actually defined
--  in LaTeX itself, and do not really require the @amsmath@ package.
--  See the "Text.LaTeX.Base.Math" module for this minimal command-set.
module Text.LaTeX.Packages.AMSMath
 ( -- * AMSMath package
   amsmath
   -- * Math Environments
 , math, mathDisplay
 , equation , equation_
 , align , align_
 , cases
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
   -- ** Summation \/ integration \/ differentiation \/ relations
 , tsum , sumFromTo
 , prod , prodFromTo
 , coprod, coprodFromTo
 , integral , integralFromTo
 , partial, totald, partialOf, totaldOf
 , bigcup, bigcupFromTo
 , bigcap, bigcapFromTo 
   -- ** Operator symbols
   -- *** Arithmetic
 , (+-), (-+)
 , cdot , times , div_
 , frac, tfrac
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
 , approx
 , sim
 , simeq
 , cong
   -- *** Sets
 , in_ , ni , notin
 , subset , supset
 , subseteq , supseteq
 , cap , cup
 , setminus
   -- *** Misc operators
 , vee , wedge
 , oplus , ominus , otimes
 , oslash , odot
   -- *** Accents
 , hat, tilde, bar, vec, widehat, widetilde
 , dot, ddot, dddot
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
 , text
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
 , thinspace, medspace, thickspace, negspace, space
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


-- | A reference to a numbered equation. Use with a 'label' defined in the
-- scope of the equation refered to.
eqref :: LaTeXC l => l -> l
eqref = liftL $ \l -> TeXComm "eqref" [FixArg . TeXRaw $ render l]

-- | An array of aligned equations. Use '&' to specify the points that should
-- horizontally match. Each equation is numbered, unless prevented by 'nonumber'.
align :: LaTeXC l => [l] -> l
align = liftL(TeXEnv "align" []) . mconcat . intersperse lnbk 

-- | The unnumbered variant of 'align'.
align_ :: LaTeXC l => [l] -> l
align_ = liftL(TeXEnv "align*" []) . mconcat . intersperse lnbk 

-- | The cases environment allows the writing of piecewise functions
cases :: LaTeXC l => l -> l
cases = liftL $ TeXEnv "cases" []

-------------------------------------
------- Symbols and utilities -------


-- | Like 'frac' but smaller (uses subscript size for the numerator and denominator.
tfrac :: LaTeXC l => l -> l -> l
tfrac = liftL2 $ \p q -> TeXComm "tfrac" [FixArg p, FixArg q]


-- | Add a dot accent above a symbol, as used to denote a second derivative,
--   like \(\ddot{y}\)
ddot :: LaTeXC l => l -> l
ddot = comm1 "ddot"

-- | Add a triple dot accent above a symbol, as used to denote a third derivative,
--   like \(\dddot{z}\)
dddot :: LaTeXC l => l -> l
dddot = comm1 "dddot"




-- | Escape from math mode, into a normal-text box.
--   Unlike 'mathrm', this won't squash spaces, i.e. you can write actual sentences.
--   You can embed 'math' again within such a box.
text :: LaTeXC l => l -> l
text = comm1 "text"

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
-- \[ \begin{pmatrix} 0 & 1 \\ 2 & 3 \end{pmatrix} \]
--
pmatrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
pmatrix = toMatrix "pmatrix"

-- | LaTeX rendering of a matrix using @bmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- \[ \begin{bmatrix} 0 & 1 \\ 2 & 3 \end{bmatrix} \]
--
bmatrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
bmatrix = toMatrix "bmatrix"

-- | LaTeX rendering of a matrix using @Bmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- \[ \begin{Bmatrix} 0 & 1 \\ 2 & 3 \end{Bmatrix} \]
--
b2matrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
b2matrix = toMatrix "Bmatrix"

-- | LaTeX rendering of a matrix using @vmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- \[ \begin{vmatrix} 0 & 1 \\ 2 & 3 \end{vmatrix} \]
--
vmatrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
vmatrix = toMatrix "vmatrix"

-- | LaTeX rendering of a matrix using @Vmatrix@ and a custom function to render cells.
--   Optional argument sets the alignment of the cells. Default (providing 'Nothing') 
--   is centered.
--
-- \[ \begin{Vmatrix} 0 & 1 \\ 2 & 3 \end{Vmatrix} \]
--
v2matrix :: (Texy a, LaTeXC l) => Maybe HPos -> Matrix a -> l
v2matrix = toMatrix "Vmatrix"

-------------------------------------
---------- Texy instances -----------

-- | Instance defined in "Text.LaTeX.Packages.AMSMath".
#if MIN_VERSION_base(4,9,0)
instance Texy a => Texy (Ratio a) where
#else
instance (Integral a, Texy a) => Texy (Ratio a) where
#endif
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

-- | @\,@ space equal to 3/18 of \quad (= 3 mu). \(a\,b\)
thinspace :: LaTeXC l => l
thinspace = comm0 ","

-- | @\:@ space equal to 4/18 of \quad (= 4 mu). \(a\:b\)
medspace :: LaTeXC l => l
medspace = comm0 ":"

-- | @\:@ space equal to 5/18 of \quad (= 5 mu). \(a\;b\)
thickspace :: LaTeXC l => l
thickspace = comm0 ";"

-- | @\!@ space equal to -3/18 of \quad (= -3 mu). \(a\!b\)
negspace :: LaTeXC l => l
negspace = comm0 "!"

-- | @\ @ (space after backslash) equivalent of space in normal text. \(a\ b\)
space :: LaTeXC l => l
space = comm0 " "
