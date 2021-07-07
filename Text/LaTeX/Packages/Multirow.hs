{-# LANGUAGE OverloadedStrings #-}

-- | An extension to the standard LaTeX tabular environment which
-- provides a construction for table cells that span more than one row
-- of the table.

module Text.LaTeX.Packages.Multirow
 ( multirowp
 , BigStrutsCount(..)
 , multirow
 ) where

import qualified Data.Semigroup as SG ((<>))
import Data.Maybe (catMaybes)
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class (LaTeXC, liftL)
import Text.LaTeX.Base.Types (PackageName, Pos, Measure)
import Text.LaTeX.Base.Render (Render, render, rendertex)

-- | multirow package. Use it to import it like this:
--
-- > usepackage [] multirow
multirowp :: PackageName
multirowp = "multirow"

-- | Type of bigstruts count. It is mainly used if you’ve used the
-- bigstrut package. It is the total number of uses of bigstruts
-- within rows being spanned in a multirow.
data BigStrutsCount
  = BigStruts Int          -- ^ Normal bigstruts
  | BigStrutsTop Int       -- ^ Bigstruts in the top row
  | BigStrutsBottom Int    -- ^ Bigstruts in the bottom row
  | BigStrutsTopBottom Int -- ^ Bigstruts in the top and bottom rows
  deriving (Show)

instance Render BigStrutsCount where
  render (BigStruts n)          = render n
  render (BigStrutsTop n)       = "t" SG.<> render n
  render (BigStrutsBottom n)    = "b" SG.<> render n
  render (BigStrutsTopBottom n) = "tb" SG.<> render n

-- | 'multirow' sets a piece of text in a tabular or similar
-- environment, spanning multiple rows.

multirow :: LaTeXC l =>
            Maybe Pos            -- ^ Optional vertical positioning of the text in the multirow block
         -> Double               -- ^ Number of rows to span
         -> Maybe BigStrutsCount -- ^ Optinal total number of uses of bigstrut within the rows being spanned
         -> Measure              -- ^ Width to which the text is to be set
         -> Maybe Measure        -- ^ Optinal length used to raise or lower the text
         -> l                    -- ^ Actual text of the construct
         -> l
multirow mVPos nrows mBigstruts width mVMove =
  liftL (\l ->
           TeXComm "multirow" $ catMaybes  [ fmap (OptArg . rendertex) mVPos
                                           , Just (FixArg . rendertex $ nrows)
                                           , fmap (OptArg . rendertex) mBigstruts
                                           , Just (FixArg . rendertex $ width)
                                           , fmap (OptArg . rendertex) mVMove
                                           , Just (FixArg l)
                                           ]
        )
