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
 , documentWithDOIReferences
 , citeDOI
 , DOIReference
 , ReferenceQueryT
 , masterBibFile
 ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Text.LaTeX.Base.Commands (cite, footnote, document, raw)

import Data.String (IsString)
import GHC.Generics (Generic)

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Hashable (hash)
import Numeric (showHex)

import qualified Data.List as List

import Control.Applicative
import Control.Monad (forM)
import Control.Monad.IO.Class

import qualified Text.BibTeX.Entry as BibTeX
import qualified Text.BibTeX.Format as BibTeX
import qualified Text.BibTeX.Parse as BibTeX (file)
import qualified Text.Parsec.String as Parsec

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


documentWithDOIReferences :: (MonadIO m, LaTeXC (m ()), r ~ DOIReference)
  => (r -> m (Maybe BibTeX.T)) -- ^ Reference-resolver function, for looking up BibTeX
                               --   entries for a given DOI.
                               --   If the DOI cannot be looked up (@Nothing@), we just
                               --   include a footnote with a synopsis and the DOI in
                               --   literal form. (Mostly intended to ease offline editing.)
  -> ReferenceQueryT r m ()    -- ^ The document content, possibly containing citations
                               --   in DOI-only form.
  -> m ()                      -- ^ LaTeX rendition. The content will already be wrapped
                               --   in @\\begin…end{document}@ here and an
                               --   automatically-generated @.bib@ file included, but
                               --   you still need to 'usepackage' 'biblatex' yourself.
documentWithDOIReferences resolver (ReferenceQueryT refq) = do
    (allRefs, (), useRefs) <- refq
    resolved <- fmap catMaybes . forM (allRefs[]) $ \r -> do
       r' <- resolver r
       return $ case r' of
         Just entry -> Just (r, entry)
         Nothing -> Nothing
    let refsMap = Map.fromList resolved
        bibfileConts = unlines $ BibTeX.entry . snd <$> Map.toList refsMap
        bibfileName = showHex (abs $ hash bibfileConts) $ ".bib"
    liftIO $ writeFile bibfileName bibfileConts
    () <- addbibresource bibfileName
    document . useRefs $ \r -> case Map.lookup r refsMap of
        Just a -> cite . raw . fromString $ BibTeX.identifier a
        Nothing -> makeshift r
 where makeshift :: LaTeXC l => DOIReference -> l
       makeshift (DOIReference doi synops) = footnote $
           fromLaTeX synops <> ". DOI:" <> fromString doi
    

type PlainDOI = String

data DOIReference = DOIReference {
       referenceDOI :: PlainDOI
     , referenceSynopsis :: LaTeX
     } deriving (Generic)
instance Eq DOIReference where
  DOIReference doi₀ _ == DOIReference doi₁ _ = doi₀ == doi₁
instance Ord DOIReference where
  compare (DOIReference doi₀ _) (DOIReference doi₁ _) = compare doi₀ doi₁

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

instance (Functor m, Monoid (m a), IsString (m ()), a ~ ())
           => IsString (ReferenceQueryT r m a) where
  fromString s = ReferenceQueryT $ (\a -> (id, a, const $ fromString s)) <$> mempty

instance (Functor m, Monoid (m a), a ~ ()) => Monoid (ReferenceQueryT r m a) where
  mempty = ReferenceQueryT $ (\a -> (id, a, mempty)) <$> mempty

instance (Applicative m, LaTeXC (m a), a ~ ()) => LaTeXC (ReferenceQueryT r m a) where
  liftListL f xs = ReferenceQueryT $
    (\components -> case List.unzip3 components of
          (refs, _, rebuilds) -> ( foldr (.) id refs
                                 , ()
                                 , \resolve -> liftListL f $ ($ resolve)<$>rebuilds )
       ) <$> traverse runReferenceQueryT xs

citeDOI :: (Functor m, Monoid (m ()), IsString (m ()))
        => PlainDOI  -- ^ The unambiguous document identifier.
        -> String    -- ^ Synopsis of the cited work, in the form
                     --   @"J Doe et al 1950: Investigation of a Foo"@;
                     --   this is strictly speaking optional, the synopsis will /not/
                     --   be included in the final document (provided the DOI
                     --   can be properly resolved).
        -> ReferenceQueryT DOIReference m ()
citeDOI doi synops = ReferenceQueryT $ (\a -> ( (r :), a, ($ r) )) <$> mempty
 where r = DOIReference doi $ fromString synops


masterBibFile :: MonadIO m
      => FilePath    -- ^ A @.bib@ file containing entries for all relevant literature.
      -> (DOIReference -> m (Maybe BibTeX.T))
                     -- ^ Lookup-function, suitable for 'documentWithDOIReferences'.
masterBibFile master (DOIReference doi _) = do
   entries <- liftIO $ BibTeX.file `Parsec.parseFromFile` master
   return $ case entries of
     Right bibs -> List.find hasThisDOI bibs
     Left err   -> error $ show err
 where hasThisDOI bib = List.lookup "doi" (BibTeX.fields bib) == Just doi
