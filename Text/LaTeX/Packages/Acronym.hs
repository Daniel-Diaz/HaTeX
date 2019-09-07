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
   ) where

import Text.LaTeX.Base.Class(LaTeXC)
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
