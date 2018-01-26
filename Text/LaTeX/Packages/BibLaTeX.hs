{-# LANGUAGE OverloadedStrings #-}

-- | <https://ctan.org/tex-archive/macros/latex/contrib/biblatex BibLaTeX>
--   is a reference-citation package using @.bib@ files (BibTeX) but no extra style-files.
--
module Text.LaTeX.Packages.BibLaTeX
 ( biblatex
 , addbibresource
 , cite
 , printbibliography
 ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands (cite)

-- | BibLaTeX package. Use it to import it like this:
--
-- > usepackage [] biblatex
biblatex :: PackageName
biblatex = "biblatex"

-- | Use a bibliography file as resource for reference information.
addbibresource :: LaTeXC l => FilePath -> l
addbibresource fp = fromLaTeX $ TeXComm "addbibresource" [FixArg $ TeXRaw $ fromString fp]

printbibliography :: LaTeXC l => l
printbibliography = comm0 "printbibliography"

