
import Text.LaTeX
import Text.LaTeX.Base.Parser

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "HaTeX"
  [ testGroup "LaTeX"
    [ QC.testProperty "LaTeX mempty" $
         \l -> (mempty <> l) == (l <> mempty)
            && (mempty <> l) == (l :: LaTeX)
    , QC.testProperty "LaTeX mappend" $
         \l1 l2 l3 -> render (l1 <> (l2 <> l3)) == render ((l1 <> l2) <> (l3 :: LaTeX))
    ]
  , testGroup "Parser"
    [ QC.testProperty "render . parse = id" $
         \l0 -> let t = render (l0 :: LaTeX)
                in case parseLaTeX t of
                     Left _ -> False
                     Right l -> render l == t
    ]
  ]
