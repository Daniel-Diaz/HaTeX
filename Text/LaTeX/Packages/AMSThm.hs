
{-# OPTIONS_HATEX MakeMonadic #-}
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
newtheorem :: String -> LaTeX -> LaTeX
newtheorem str l = TeXComm "newtheorem" [ FixArg $ fromString str , FixArg l ]

theorem :: String -> LaTeX -> LaTeX
theorem str l = TeXEnv str [] l

-- | The 'proof' environment. The first optional argument
--   is used to put a custom title to the proof.
proof :: Maybe LaTeX -> LaTeX -> LaTeX
proof  Nothing l = TeXEnv "proof" [ ] l
proof (Just n) l = TeXEnv "proof" [ OptArg n ] l

-- | Insert the /QED/ symbol.
qedhere :: LaTeX
qedhere = TeXComm "qedhere" []

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
theoremstyle :: TheoremStyle -> LaTeX
theoremstyle thmsty = TeXComm "theoremstyle" [ FixArg $ TeXRaw $ render thmsty ]

