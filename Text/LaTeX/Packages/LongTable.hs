{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.LongTable
 ( -- * longtable package
   longtablep
   -- * longtable commands
 , longtable
 , endfirsthead
 , endhead
 , endfoot
 , endlastfoot
   -- * Package Options
   ) where

import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv, TeXRaw, TeXComm), TeXArg(FixArg, OptArg))
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX, liftL)
import Text.LaTeX.Base.Render (render, renderAppend)
import Text.LaTeX.Base.Types (PackageName, Pos, TableSpec)

-- | longtable package. Use it to import it like this:
--
-- > usepackage [] longtable
longtablep :: PackageName
longtablep = "longtable"


-- | The 'longtable' environment can be used to typeset multi-page tables.
longtable :: LaTeXC l =>
             Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                         --   Defaulted to 'Center'.
          -> [TableSpec] -- ^ Table specification of columns and vertical lines.
          -> l           -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
          -> l           -- ^ Resulting table syntax.
longtable Nothing ts  = liftL $ TeXEnv "longtable" [ FixArg $ TeXRaw $ renderAppend ts ]
longtable (Just p) ts = liftL $ TeXEnv "longtable" [ OptArg $ TeXRaw $ render p , FixArg $ TeXRaw $ renderAppend ts ]

-- | End the first head.
--
-- Everything above this command will appear at the beginning of the
-- table, in the first page.
endfirsthead :: LaTeXC l => l
endfirsthead = fromLaTeX $ TeXComm "endfirsthead" []

-- | End the head.
--
-- Whatever you put before this command and below \endfirsthead will
-- be displayed at the top of the table in every page except the first
-- one.
endhead :: LaTeXC l => l
endhead = fromLaTeX $ TeXComm "endhead" []

-- | End the foot.
--
-- Similar to \endhead, what you put after \endhead and before this
-- command will appear at the bottom of the table in every page except
-- the last one.
endfoot :: LaTeXC l => l
endfoot = fromLaTeX $ TeXComm "endfoot" []

-- | End the last foot.
--
-- Similar to \endfisthead. The elements after \endfoot and before
-- this command will be displayed at the bottom of the table but only
-- in the last page where the table appears.
endlastfoot :: LaTeXC l => l
endlastfoot = fromLaTeX $ TeXComm "endlastfoot" []
