
-- | Module for the package @amssymb@.
module Text.LaTeX.Packages.AMSSymb
 ( -- * AMSSymb package
   amssymb

   -- * Arrows
  , vartriangleleft, vartriangleright 
  , leftleftarrows, rightrightarrows
  , rightleftarrows, leftrightarrows
  , upuparrows, downdownarrows
  , leftarrowtail, rightarrowtail
  , curvearrowleft, curvearrowright
  , twoheadleftarrow, twoheadrightarrow
  , rightleftharpoons
  , lsh2, rsh2
  , leftarrow3, rightarrow3
  , rightsquigarrow, leftrightsquigarrow
  , looparrowleft, looparrowright
  , circlearrowleft, circlearrowright
  , upharpoonleft, upharpoonright
  , downharpoonleft, downharpoonright
  , nleftarrow, nrightarrow
  , nleftarrow2, nrightarrow2
  , nleftrightarrow, nleftrightarrow2

   -- * Other
  , lll, ggg
  , gtrdot, lessdot
  , square, blacksquare  
  , lozenge, blacklozenge
  , checkmark, nexists
 ) where

import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Types

-- | AMSSymb package.
-- Example:
--
-- > usepackage [] amssymb
amssymb :: ClassName
amssymb = "amssymb"

--

-- | \(\vartriangleleft\) symbol.
vartriangleleft :: LaTeXC l => l
vartriangleleft = comm0 "vartriangleleft"

-- | \(\vartriangleleft\) symbol.
vartriangleright :: LaTeXC l => l
vartriangleright = comm0 "vartriangleright"

-- | \(\leftleftarrows\) symbol - double left arrows.
leftleftarrows :: LaTeXC l => l
leftleftarrows = comm0 "leftleftarrows"

-- | \(\rightrightarrows\) symbol - double right arrows
rightrightarrows :: LaTeXC l => l
rightrightarrows = comm0 "rightrightarrows"

-- | \(\rightleftarrows\) symbol - right arrow atop a left arrow
rightleftarrows :: LaTeXC l => l
rightleftarrows = comm0 "rightleftarrows"

-- | \(\leftrightarrows\) symbol - left arrow atop a right arrow.
leftrightarrows :: LaTeXC l => l
leftrightarrows = comm0 "leftrightarrows"

-- | \(\upuparrows\) symbol - double upward arrows.
upuparrows :: LaTeXC l => l
upuparrows = comm0 "upuparrows"

-- | \(\downdownarrows\) symbol - double downward arrows.
downdownarrows :: LaTeXC l => l
downdownarrows = comm0 "downdownarrows"

-- | \(\lll\) symbol - triple less than.
lll :: LaTeXC l => l
lll = comm0 "lll"

-- | \(\ggg\) symbol - triple greater than.
ggg :: LaTeXC l => l
ggg = comm0 "ggg"

-- | \(\leftarrowtail\) symbol - leftwards "mapsto"
leftarrowtail :: LaTeXC l => l
leftarrowtail = comm0 "leftarrowtail"

-- | \(\rightarrowtail\) symbol - rightwards "mapsto"
rightarrowtail :: LaTeXC l => l
rightarrowtail = comm0 "rightarrowtail"

-- | \(\curvearrowleft\) symbol - leftwards curved arrow
curvearrowleft :: LaTeXC l => l
curvearrowleft = comm0 "curvearrowleft"

-- | \(\curvearrowright\) symbol - rightwards curved arrow
curvearrowright :: LaTeXC l => l
curvearrowright = comm0 "curvearrowright"

-- | \(\twoheadleftarrow\) symbol - double head left arrow
twoheadleftarrow :: LaTeXC l => l
twoheadleftarrow = comm0 "twoheadleftarrow"

-- | \(\twoheadrightarrow\) symbol - double head right arrow
twoheadrightarrow :: LaTeXC l => l
twoheadrightarrow = comm0 "twoheadleftarrow"

-- | \(\checkmark\) symbol.
checkmark :: LaTeXC l => l
checkmark = comm0 "checkmark"

-- | \(\lozenge\) symbol - narrow diamond
lozenge :: LaTeXC l => l
lozenge = comm0 "lozenge"

-- | \(\blacklozenge\) symbol - filled narrow diamond
blacklozenge :: LaTeXC l => l
blacklozenge = comm0 "blacklozenge"

-- | \(\nexists\) symbol - does not exist
nexists :: LaTeXC l => l
nexists = comm0 "nexists"

-- | \(\lessdot\) symbol - less than with inner dot
lessdot :: LaTeXC l => l
lessdot = comm0 "lessdot"

-- | \(\gtrdot\) symbol - greater than with inner dot
gtrdot :: LaTeXC l => l
gtrdot = comm0 "gtrdot"

-- | \(\square\) symbol - square, often used to denote the end of a proof
square :: LaTeXC l => l
square = comm0 "square"

-- | \(\blacksquare\) symbol - a filled square
blacksquare :: LaTeXC l => l
blacksquare = comm0 "blacksquare"

-- | \(\rightleftharpoons\) symbol
rightleftharpoons :: LaTeXC l => l
rightleftharpoons = comm0 "rightleftharpoons"
-- | \(\Lsh\) symbol
lsh2 :: LaTeXC l => l
lsh2 = comm0 "Lsh"
-- | \(\Rsh\) symbol
rsh2 :: LaTeXC l => l
rsh2 = comm0 "Rsh"
-- | \(\Lleftarrow\) symbol
leftarrow3 :: LaTeXC l => l
leftarrow3 = comm0 "Lleftarrow"
-- | \(\Rrightarrow\) symbol
rightarrow3 :: LaTeXC l => l
rightarrow3 = comm0 "Rrightarrow"
-- | \(\rightsquigarrow\) symbol
rightsquigarrow :: LaTeXC l => l
rightsquigarrow = comm0 "rightsquigarrow"
-- | \(\leftrightsquigarrow\) symbol
leftrightsquigarrow :: LaTeXC l => l
leftrightsquigarrow = comm0 "leftrightsquigarrow"
-- | \(\looparrowleft\) symbol
looparrowleft :: LaTeXC l => l
looparrowleft = comm0 "looparrowleft"
-- | \(\looparrowright\) symbol
looparrowright :: LaTeXC l => l
looparrowright = comm0 "looparrowright"
-- | \(\circlearrowleft\) symbol
circlearrowleft :: LaTeXC l => l
circlearrowleft = comm0 "circlearrowleft"
-- | \(\circlearrowright\) symbol
circlearrowright :: LaTeXC l => l
circlearrowright = comm0 "circlearrowright"
-- | \(\upharpoonleft\) symbol
upharpoonleft :: LaTeXC l => l
upharpoonleft = comm0 "upharpoonleft"
-- | \(\upharpoonright\) symbol
upharpoonright :: LaTeXC l => l
upharpoonright = comm0 "upharpoonright"
-- | \(\downharpoonleft\) symbol
downharpoonleft :: LaTeXC l => l
downharpoonleft = comm0 "downharpoonleft"
-- | \(\downharpoonright\) symbol
downharpoonright :: LaTeXC l => l
downharpoonright = comm0 "downharpoonright"
-- | \(\nleftarrow\) symbol
nleftarrow :: LaTeXC l => l
nleftarrow = comm0 "nleftarrow"
-- | \(\nrightarrow\) symbol
nrightarrow :: LaTeXC l => l
nrightarrow = comm0 "nrightarrow"
-- | \(\nLeftarrow\) symbol
nleftarrow2 :: LaTeXC l => l
nleftarrow2 = comm0 "nLeftarrow"
-- | \(\nRightarrow\) symbol
nrightarrow2 :: LaTeXC l => l
nrightarrow2 = comm0 "nRightarrow"
-- | \(\nleftrightarrow\) symbol
nleftrightarrow :: LaTeXC l => l
nleftrightarrow = comm0 "nleftrightarrow"
-- | \(\nLeftrightarrow\) symbol
nleftrightarrow2 :: LaTeXC l => l
nleftrightarrow2 = comm0 "nLeftrightarrow"
