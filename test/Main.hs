{-# LANGUAGE CPP #-}

import Text.LaTeX
import Text.LaTeX.Base.Parser

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

#if !MIN_VERSION_parsec(3,1,9)
instance Eq ParseError where
  _ == _ = undefined
#endif

main :: IO ()
main = defaultMain $ testGroup "HaTeX"
  [ testGroup "LaTeX"
    [ QC.testProperty "LaTeX mempty" $
         \l -> (mempty <> l) == (l <> mempty)
            && (mempty <> l) == (l :: LaTeX)
    , QC.testProperty "LaTeX mappend" $
         \l1 l2 l3 -> l1 <> (l2 <> l3) == (l1 <> l2) <> (l3 :: LaTeX)
    ]
  , testGroup "Parser"
    [ QC.testProperty "render . parse = id" $
         \l -> let t = render (l :: LaTeX)
               in  fmap render (parseLaTeX t) == Right t
    ]
  ]
