{-# LANGUAGE OverloadedStrings #-}

-- | An extension to the standard LaTeX tabular environment that
-- creates struts which (slightly) stretch the table row in which they
-- sit.

module Text.LaTeX.Packages.Bigstrut
 ( bigstrutp
 , bigstrut
 , bigstrutTop
 , bigstrutBottom
 ) where

import Text.LaTeX.Base.Syntax 
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX)
import Text.LaTeX.Base.Types (PackageName)

-- | bigstrut package. Use it to import it like this:
--
-- > usepackage [] bigstrut
bigstrutp :: PackageName
bigstrutp = "bigstrut"

-- | 'bigstrutTop', 'bigstrutBottom' and 'bigstrut' produce a strut (a
-- rule with width 0) which is 'bigstrutjot' (2pt by default) higher,
-- lower, or both than the standard array/tabular strut. Use them in
-- table entries that are adjacent to 'hlines' to leave an extra bit
-- of space

bigstrut :: LaTeXC l => l
bigstrut = fromLaTeX $ TeXComm "bigstrut" []

bigstrutTop :: LaTeXC l => l
bigstrutTop = fromLaTeX $ TeXComm "bigstrut" [OptArg "t"]

bigstrutBottom :: LaTeXC l => l
bigstrutBottom = fromLaTeX $ TeXComm "bigstrut" [OptArg "b"]
