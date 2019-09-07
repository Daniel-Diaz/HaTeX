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
   Bulgarian  -- ^ Bulgarian.
 | Brazilian  -- ^ Brazilian Portuguese.
 | Canadien   -- ^ Canadian French.
 | Czech      -- ^ Czech.
 | Dutch      -- ^ Dutch.
 | English    -- ^ English.
 | Finnish    -- ^ Finnish.
 | Francais   -- ^ Parisian French.
 | French     -- ^ French.
 | FrenchB    -- ^ French.
 | German     -- ^ Old German.
 | NGerman    -- ^ New German.
 | Icelandic  -- ^ Icelandic.
 | Italian    -- ^ Italian.
 | Magyar     -- ^ Hungarian.
 | Portuguese -- ^ Portuguese.
 | Russian    -- ^ Russian.
 | Spanish    -- ^ Spanish.
 | Ukranian   -- ^ Ukranian.
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
uselanguage lang = usepackage [texy lang] babel

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
selectlanguage lang = fromLaTeX $ TeXComm "selectlanguage" [FixArg $ texy lang]

-- | Use a 'Language' locally.
otherlanguage :: LaTeXC l => Language -> l -> l
otherlanguage lang = liftL $ TeXEnv "otherlanguage" [FixArg $ texy lang]

-- | The function 'foreignlanguage' takes two arguments; the second argument is a
--   phrase to be typeset according to the rules of the language named in its first
--   argument.
foreignlanguage :: LaTeXC l => Language -> l -> l
foreignlanguage lang = liftL
          $ \l -> TeXComm "foreignlanguage" [OptArg $ texy lang, FixArg l]
