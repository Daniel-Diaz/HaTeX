{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}

-- | <https://ctan.org/tex-archive/macros/latex/contrib/biblatex BibLaTeX>
--   is a reference-citation package using @.bib@ files (BibTeX) but no extra style-files.
--
module Text.LaTeX.Packages.BibLaTeX
 ( biblatex
 , addbibresource
 , cite
 , printbibliography
 -- * Automatic bibliography retrieval
 , DOIReference
 , ReferenceQueryT
 ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands (cite)

import Data.String (IsString)
import GHC.Generics (Generic)

import Control.Applicative

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


newtype DOIReference = DOIReference { getDOI :: String }
instance IsString DOIReference where fromString = DOIReference

type DList r = [r] -> [r]

newtype ReferenceQueryT r m a = ReferenceQueryT {
       runReferenceQueryT :: m (DList r, a, (r -> m ()) -> m ())
     }
  deriving (Generic, Functor)

instance Applicative m => Applicative (ReferenceQueryT r m) where
  pure x = ReferenceQueryT . pure $ (id, x, const $ pure ())
  ReferenceQueryT refqf <*> ReferenceQueryT refqx = ReferenceQueryT $
       liftA2 (\(urefsf, f, refref)
                (urefsx, x, refrex)
                  -> ( urefsf . urefsx
                     , f x
                     , \resolv -> mappend <$> refref resolv <*> refrex resolv ) )
              refqf refqx
instance Monad m => Monad (ReferenceQueryT r m) where
  return = pure
  ReferenceQueryT refsx >>= f
     = ReferenceQueryT $ refsx >>= \(urefsx, x, refrex)
           -> case f x of
                ReferenceQueryT refsfx
                  -> (\(urefsfx,fx,refrefx)
                        -> ( urefsx.urefsfx
                           , fx
                           , \resolve -> mappend <$> refrex resolve <*> refrefx resolve ))
                     <$> refsfx

instance (Functor m, Monoid (m a), IsString (m ()))
           => IsString (ReferenceQueryT r m a) where
  fromString s = ReferenceQueryT $ (\a -> (id, a, const $ fromString s)) <$> mempty
