{-# LANGUAGE PatternSynonyms #-}
module Text.LaTeX.Base.Syntax 
 ( -- * @LaTeX@ datatype
   S.MathType (..)
 , Measure
 , pattern Pt
 , pattern Mm
 , pattern Cm
 , pattern In
 , pattern Ex
 , pattern Em
 , pattern CustomMeasure

 , LaTeX
 , pattern TeXRaw
 , pattern TeXComm
 , pattern TeXCommS
 , pattern TeXEnv
 , pattern TeXMath
 , pattern TeXLineBreak
 , pattern TeXBraces
 , pattern TeXComment
 , pattern TeXSeq
 , pattern TeXEmpty

 , TeXArg
 , pattern FixArg
 , pattern OptArg
 , pattern MOptArg
 , pattern SymArg
 , pattern MSymArg
 , pattern ParArg
 , pattern MParArg
 , (<>), S.between
   -- * Escaping reserved characters
 , S.protectString
 , S.protectText
   -- * Syntax analysis
 , S.matchCommand
 , S.lookForCommand
 , S.matchEnv
 , S.lookForEnv
 , S.texmap
 , S.texmapM
   -- ** Utils
 , S.getBody
 , S.getPreamble
   ) where

import           Data.Text(Text)

import qualified Text.LaTeX.Base.Syntax.WithParm as S

type Measure = S.Measure ()
type LaTeX   = S.LaTeX ()
type TeXArg  = S.TeXArg ()

{-# COMPLETE TeXRaw, TeXComm , TeXCommS , TeXEnv , TeXMath , TeXLineBreak
           , TeXBraces , TeXSeq , TeXEmpty #-}
pattern TeXRaw :: Text -> LaTeX
pattern TeXRaw t = S.TeXRaw () t

pattern TeXComm :: String -> [TeXArg] -> LaTeX
pattern TeXComm s l = S.TeXComm () s l

pattern TeXCommS :: String -> LaTeX
pattern TeXCommS s = S.TeXCommS () s

pattern TeXEnv :: String -> [TeXArg] -> LaTeX -> LaTeX
pattern TeXEnv s l e = S.TeXEnv () () s l e

pattern TeXMath :: S.MathType -> LaTeX -> LaTeX
pattern TeXMath m e = S.TeXMath () m e

pattern TeXLineBreak :: Maybe (Measure) -> Bool -> LaTeX
pattern TeXLineBreak m e = S.TeXLineBreak () m e

pattern TeXComment :: Text -> LaTeX
pattern TeXComment t = S.TeXComment () t

pattern TeXBraces :: LaTeX -> LaTeX
pattern TeXBraces t = S.TeXBraces () t

pattern TeXSeq :: LaTeX -> LaTeX -> LaTeX
pattern TeXSeq m n = S.TeXSeq m n

pattern TeXEmpty :: LaTeX
pattern TeXEmpty = S.TeXEmpty

{-# COMPLETE FixArg , OptArg , MOptArg , SymArg , MSymArg , ParArg , MParArg #-}
pattern FixArg :: LaTeX -> TeXArg
pattern FixArg m = S.FixArg () m

pattern OptArg :: LaTeX -> TeXArg
pattern OptArg x = S.OptArg () x

pattern MOptArg :: [LaTeX] -> TeXArg
pattern MOptArg x = S.MOptArg () x

pattern SymArg :: LaTeX -> TeXArg
pattern SymArg x = S.SymArg () x

pattern MSymArg :: [LaTeX] -> TeXArg
pattern MSymArg x = S.MSymArg () x

pattern ParArg :: LaTeX -> TeXArg
pattern ParArg x = S.ParArg () x

pattern MParArg :: [LaTeX] -> TeXArg
pattern MParArg x = S.MParArg () x

{-# COMPLETE Pt , Mm , Cm , In , Ex , Em , CustomMeasure #-}
pattern Pt :: Double -> Measure
pattern Pt x = S.Pt x

pattern Mm :: Double -> Measure
pattern Mm x = S.Mm x

pattern Cm :: Double -> Measure
pattern Cm x = S.Cm x

pattern In :: Double -> Measure
pattern In x = S.In x

pattern Ex :: Double -> Measure
pattern Ex x = S.Ex x

pattern Em :: Double -> Measure
pattern Em x = S.Em x

pattern CustomMeasure :: LaTeX -> Measure
pattern CustomMeasure x = S.CustomMeasure x
