{-# LANGUAGE CPP #-}

import Text.LaTeX
import Text.LaTeX.Base.Parser

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Data.Either (isRight)

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
    [ QC.testProperty "isRight . parse . render" $
         \l -> isRight $ parseLaTeX (render (l :: LaTeX))
    ]
  ]
