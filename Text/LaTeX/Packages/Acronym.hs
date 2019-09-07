{-# LANGUAGE OverloadedStrings, CPP #-}

-- | Add acronyms to your documents using this module.
--
--   Define and render acronyms in a document, where the first occurrance is the
--   long variant, and the next ones are the shorter variant.
module Text.LaTeX.Packages.Acronym
 ( -- * Acronym package
   pacronym
   -- * Package options
 , footnote
 , nohyperlinks
 , printonlyused
 , withpage
 , smaller
 , dua
 , nolist
 -- * Types
 , Acronym(..)
 -- functions
 , ac, acf, acs, acl, acp, acfp, acsp, aclp, acfi, acsu, aclu
 , iac, iac2
 , ac', acf', acs', acl', acp', acfp', acsp', aclp', acfi', acsu', aclu'
 , iac', iac2'
   ) where

import Data.String(IsString(fromString))

import Text.LaTeX.Base.Class(LaTeXC, comm1)
import Text.LaTeX.Base.Types(PackageName)

-- | The 'pacronym' package.
--
-- > usepackage [] pacronym
pacronym :: PackageName
pacronym = "acronym"

-- | Redefines the `\acf` and `\acfp` commands making the full
--   name appear as a footnote
footnote :: LaTeXC l => l
footnote = "footnote"

-- | If hyperref is loaded, all acronyms will link to their glossary entry. With
--   the `nohyperlinks` option, these links are suppressed.
nohyperlinks :: LaTeXC l => l
nohyperlinks = "nohyperlinks"

-- | We need a marker which is set if the option `printonlyused` was used.
printonlyused :: LaTeXC l => l
printonlyused = "printonlyused"

-- | A marker which tells us to print page numbers.
withpage :: LaTeXC l => l
withpage = "withpage"

-- | The option `smaller` leads to a redefinition of `\acsfont`. We want to make
--   the acronym appear smaller. Since this should be done in a
--   context-sensitive way, we rely on the macro \textsmaller provided by the
--   `relsize` package. As `\RequiredPackage` cannot be used inside
--   `\DeclareOption`, we need a boolean variable.
smaller :: LaTeXC l => l
smaller = "smaller"

-- | The option `dua` stands for "don't use acronyms". It leads to a
--   redefinition of `\ac` and `\acp` making the full name appear all the time
--   and suppressing all acronyms but the explicitly requested by `\acf` and
--   `\acfp`.
dua :: LaTeXC l => l
dua = "dua"

-- | The option `nolist` stands for "don't write the list of acronyms".
nolist :: LaTeXC l => l
nolist = "nolist"

-- | An acronym type with a label, this is used to generate commands linked to
--   this acronym.
newtype Acronym = Acronym { acronymLabel :: String }

_acronymLabel :: IsString s => Acronym -> s
_acronymLabel = fromString . acronymLabel

_acronymLabel' :: LaTeXC l => String -> Acronym -> l
_acronymLabel' = (. _acronymLabel) . comm1

ac :: LaTeXC l => Acronym -> l
ac = _acronymLabel' "ac"

acf :: LaTeXC l => Acronym -> l
acf = _acronymLabel' "acf"

acs :: LaTeXC l => Acronym -> l
acs = _acronymLabel' "acs"

acl :: LaTeXC l => Acronym -> l
acl = _acronymLabel' "acl"

acp :: LaTeXC l => Acronym -> l
acp = _acronymLabel' "acp"

acfp :: LaTeXC l => Acronym -> l
acfp = _acronymLabel' "acfp"

acsp :: LaTeXC l => Acronym -> l
acsp = _acronymLabel' "acsp"

aclp :: LaTeXC l => Acronym -> l
aclp = _acronymLabel' "aclp"

acfi :: LaTeXC l => Acronym -> l
acfi = _acronymLabel' "acfi"

acsu :: LaTeXC l => Acronym -> l
acsu = _acronymLabel' "acsu"

aclu :: LaTeXC l => Acronym -> l
aclu = _acronymLabel' "aclu"

iac :: LaTeXC l => Acronym -> l
iac = _acronymLabel' "iac"

iac2 :: LaTeXC l => Acronym -> l
iac2 = _acronymLabel' "Iac"

ac' :: LaTeXC l => Acronym -> l
ac' = _acronymLabel' "ac*"

acf' :: LaTeXC l => Acronym -> l
acf' = _acronymLabel' "acf*"

acs' :: LaTeXC l => Acronym -> l
acs' = _acronymLabel' "acs*"

acl' :: LaTeXC l => Acronym -> l
acl' = _acronymLabel' "acl*"

acp' :: LaTeXC l => Acronym -> l
acp' = _acronymLabel' "acp*"

acfp' :: LaTeXC l => Acronym -> l
acfp' = _acronymLabel' "acfp*"

acsp' :: LaTeXC l => Acronym -> l
acsp' = _acronymLabel' "acsp*"

aclp' :: LaTeXC l => Acronym -> l
aclp' = _acronymLabel' "aclp*"

acfi' :: LaTeXC l => Acronym -> l
acfi' = _acronymLabel' "acfi*"

acsu' :: LaTeXC l => Acronym -> l
acsu' = _acronymLabel' "acsu*"

aclu' :: LaTeXC l => Acronym -> l
aclu' = _acronymLabel' "aclu*"

iac' :: LaTeXC l => Acronym -> l
iac' = _acronymLabel' "iac*"

iac2' :: LaTeXC l => Acronym -> l
iac2' = _acronymLabel' "Iac*"
