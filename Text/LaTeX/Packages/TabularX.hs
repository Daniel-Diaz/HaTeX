
{-# LANGUAGE OverloadedStrings #-}

module Text.LaTeX.Packages.TabularX
 ( -- * tabularx package
   tabularxp
   -- * tabularx commands
 , tabularx
 ) where

import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv, TeXRaw), TeXArg(FixArg, OptArg))
import Text.LaTeX.Base.Class (LaTeXC, liftL)
import Text.LaTeX.Base.Render (render, renderAppend)
import Text.LaTeX.Base.Types (PackageName, Pos, TableSpec, Measure)

-- | tabularx package. Use it to import it like this:
--
-- > usepackage [] tabularxp
tabularxp :: PackageName
tabularxp = "tabularx"

-- | The 'tabularx' environment takes the same arguments as tabular*,
-- but modifies the widths of certain columns, rather than the inter
-- column space, to set a table with the requested total width. The
-- columns that may stretch are marked with the new token X in the
-- preamble argument.
--
tabularx :: LaTeXC l =>
            Measure      -- ^ Width of the whole tabular.
         -> Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                        --   Defaulted to 'Center'.
         -> [TableSpec] -- ^ Table specification of columns and vertical lines.
         -> l           -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
         -> l           -- ^ Resulting table syntax.
tabularx width maybePos ts =
  liftL $ TeXEnv "tabularx" args
  where
    width' = FixArg $ TeXRaw $ render width
    ts' = FixArg $ TeXRaw $ renderAppend ts
    args = case maybePos of
             Nothing -> [width', ts']
             Just p -> [width', p', ts']
               where
                 p' = OptArg $ TeXRaw $ render p
