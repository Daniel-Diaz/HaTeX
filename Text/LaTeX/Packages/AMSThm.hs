
{-# LANGUAGE OverloadedStrings #-}

-- | Package for theorem environments.
module Text.LaTeX.Packages.AMSThm
 ( -- * AMSThm package
   amsthm
   -- * AMSThm functions
 , newtheorem
 , theorem
 , proof
 , qedhere
 , TheoremStyle (..)
 , theoremstyle
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types

-- | AMSThm package.
-- Example:
--
-- > usepackage [] amsthm
amsthm :: PackageName
amsthm = "amsthm"

-- | Create a new 'theorem' environment type.
--   Arguments are environment name (this will be the argument
--   when using the 'theorem' function) and the displayed title.
--
--   For example:
--
-- > newtheorem "prop" "Proposition"
--
-- > theorem "prop" "This is it."
newtheorem :: LaTeXC l => String -> l -> l
newtheorem str = liftL $ \l -> TeXComm "newtheorem" [ FixArg $ fromString str , FixArg l ]

theorem :: LaTeXC l => String -> l -> l
theorem str = liftL $ TeXEnv str []

-- | The 'proof' environment. The first optional argument
--   is used to put a custom title to the proof.
proof :: LaTeXC l => Maybe l -> l -> l
proof  Nothing = liftL $ TeXEnv "proof" []
proof (Just n) = liftL2 (\n -> TeXEnv "proof" [ OptArg n ]) n

-- | Insert the /QED/ symbol.
qedhere :: LaTeXC l => l
qedhere = comm0 "qedhere"

data TheoremStyle =
   Plain
 | Definition
 | Remark
 | CustomThmStyle String
   deriving Show

instance Render TheoremStyle where
 render Plain = "plain"
 render Definition = "definition"
 render Remark = "remark"
 render (CustomThmStyle str) = fromString str

-- | Set the theorem style. Call this function in the preamble.
theoremstyle :: LaTeXC l => TheoremStyle -> l
theoremstyle thmsty = fromLaTeX $ TeXComm "theoremstyle" [ FixArg $ TeXRaw $ render thmsty ]