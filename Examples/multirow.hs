{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Packages.Bigstrut
import Text.LaTeX.Packages.Multirow

main :: IO ()
main = execLaTeXT example >>= renderFile "multirow.tex"

example :: Monad m => LaTeXT_ m
example = do
  documentclass [] article
  usepackage [] multirowp
  usepackage [] bigstrutp
  document exampleBody

minitab :: Monad m => TableSpec -> LaTeXT_ m -> LaTeXT_ m
minitab tableSpec text =
  tabular Nothing [tableSpec] text

exampleBody :: Monad m => LaTeXT_ m
exampleBody = do
  "An example with both multirow and bigstrut"
  par
  tabular Nothing [VerticalLine, CenterColumn, VerticalLine, CenterColumn, VerticalLine] $ do
    hline
    multirow Nothing 4 Nothing "1in" Nothing "Common g text" & "Column g2a" <> lnbk
    mempty & "Column g2b" <> lnbk
    mempty & "Column g2c" <> lnbk
    mempty & "Column g2d" <> lnbk
    hline
    multirow Nothing 3 (Just (BigStruts 6)) "*" Nothing "Common g text" & ("Column g2a" <> bigstrut) <> lnbk <> cline 2 2
    mempty & ("Column g2b" <> bigstrut) <> lnbk <> cline 2 2
    mempty & ("Column g2c" <> bigstrut) <> lnbk
    hline
    multirow Nothing 4 (Just (BigStruts 8)) "1in" Nothing "Common g text, but a bit longer." & ("Column g2a" <> bigstrut) <> lnbk <> cline 2 2
    mempty & ("Column g2b" <> bigstrut) <> lnbk <> cline 2 2
    mempty & ("Column g2c" <> bigstrut) <> lnbk <> cline 2 2
    mempty & ("Column g2d" <> bigstrut) <> lnbk
    hline
    multirow Nothing 4 Nothing "*" Nothing (minitab CenterColumn ("Common " <> lnbk <> "g text")) & "Column g2a" <> lnbk
    mempty & "Column g2b" <> lnbk
    mempty & "Column g2c" <> lnbk
    mempty & "Column g2d" <> lnbk
    hline
