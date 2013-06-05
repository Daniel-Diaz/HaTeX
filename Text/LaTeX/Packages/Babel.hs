
{-# LANGUAGE OverloadedStrings #-}

-- | The @babel@ package is used to write documents in languages
--   other than US English.
--
--  CTAN page for babel: <http://ctan.org/pkg/babel>.
module Text.LaTeX.Packages.Babel (
   -- * Babel package
   babel
   -- * Babel languages
 , Language (..)
 , uselanguage
 , LangConf (..)
 , uselanguageconf 
   -- * Babel commands and environments
 , selectlanguage
 , otherlanguage
 , foreignlanguage
   ) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
--
import Data.Text (toLower)

-- Babel package
 
-- | Babel package. When writing in a single language, the simplest
--   way of using it is with 'uselanguage'.
--
--   In the preamble, use the following (if your language of choice is Spanish):
--
-- > uselanguage Spanish
--
-- To see a list of available languages, check the 'Language' type.
--
babel :: PackageName
babel = "babel"

-- Babel languages

-- | Languages.
data Language =
   English
 | Spanish
 | French
 | Russian
 | German
   deriving Show

instance Render Language where
 render = toLower . fromString . show

instance Texy Language where
 texy = texy . render

-- | Import the 'babel' package using a given 'Language'.
--
-- > uselanguage l = usepackage [texy l] babel
--
--  If you are using more than one language, consider to use
--  'uselanguageconf'.
uselanguage :: LaTeXC l => Language -> l
uselanguage ln = usepackage [texy ln] babel

-- | Language configuration. You may use one with 'uselanguageconf'.
data LangConf = LangConf { mainLang :: Language , otherLangs :: [Language] }
                 deriving Show

-- | Import the 'label' package using a given language
--   configuration, featuring a main language and some
--   others. For example:
--
-- > uselanguageconf $ LangConf English [German]
--
--   This will use English as main language, and German
--   as secondary.
uselanguageconf :: LaTeXC l => LangConf -> l
uselanguageconf lc = usepackage xs babel
 where
  x = "main=" <> texy (mainLang lc)
  xs = x : fmap texy (otherLangs lc)

-- | Switch to a given 'Language'.
selectlanguage :: LaTeXC l => Language -> l
selectlanguage ln = fromLaTeX $ TeXComm "selectlanguage" [FixArg $ texy ln]

-- | Use a 'Language' locally.
otherlanguage :: LaTeXC l => Language -> l -> l
otherlanguage ln = liftL $ TeXEnv "otherlanguage" [FixArg $ texy ln]

-- | The function 'foreignlanguage' takes two arguments; the second argument is a
--   phrase to be typeset according to the rules of the language named in its first
--   argument.
foreignlanguage :: LaTeXC l => Language -> l -> l
foreignlanguage ln = liftL $ \l -> TeXComm "foreignlanguage" [OptArg $ texy ln, FixArg l]
