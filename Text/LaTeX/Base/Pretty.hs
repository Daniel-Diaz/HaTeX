
-- | 'LaTeX' values pretty printer.
module Text.LaTeX.Base.Pretty (
    -- * @LaTeX@ pretty printer
    prettyLaTeX
    -- * Configurable printer
  , docLaTeX
  ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Text.PrettyPrint.Free
  ( Doc, text, char
  , backslash, hardline
  , braces, brackets
  , indent, align, vsep
  , list, encloseSep
  , renderSmart, displayS
    )
import Data.Text (unpack,lines)
import Data.Monoid (mconcat,mempty)

-- | This function transforms a value of type 'LaTeX' to a 'Doc'.
--   You can then choose how to print this 'Doc' value using
--   the function from the "Text.PrettyPrint.Free" module.
docLaTeX :: LaTeX -> Doc ()
docLaTeX (TeXRaw t) = text $ unpack t
docLaTeX (TeXComm n as) = backslash <> text n <> align (mconcat (fmap docTeXArg as)) <> hardline
docLaTeX (TeXCommS n) = backslash <> text n <> hardline
docLaTeX (TeXEnv n as b) =
  let a = FixArg $ fromString n
  in  mconcat
       [ hardline
       , docLaTeX $ TeXComm "begin" $ a : as
       , indent 4 $ docLaTeX b
       , hardline
       , docLaTeX $ TeXComm "end" [a]
         ]
docLaTeX (TeXMath t b) =
  let (l,r) =
        case t of
          Parentheses -> ("\\(","\\)")
          Square -> ("\\[","\\]")
          Dollar -> ("$","$")
  in  text l <> docLaTeX b <> text r
docLaTeX (TeXLineBreak m b) =
  text "\\\\" <> maybe mempty (brackets . text . unpack . render) m <> ( if b then text "*" else mempty )
docLaTeX (TeXBraces b) = braces $ docLaTeX b
docLaTeX (TeXComment t) =
  let ls = Data.Text.lines t
  in  if null ls
         then char '%' <> hardline
         else align $ vsep $ fmap (text . ("% "++) . unpack) ls
docLaTeX (TeXSeq l1 l2) = docLaTeX l1 <> docLaTeX l2
docLaTeX TeXEmpty = mempty

docTeXArg :: TeXArg -> Doc ()
docTeXArg (FixArg l) = braces $ docLaTeX l
docTeXArg (OptArg l) = brackets $ docLaTeX l
docTeXArg (MOptArg ls) =
  if null ls then mempty
             else list $ fmap docLaTeX ls
docTeXArg (SymArg l) = docTeXArg $ MSymArg [l]
docTeXArg (MSymArg ls) = encloseSep (char '<') (char '>') (char ',') $ fmap docLaTeX ls

-- | Pretty print a 'LaTeX' value. It produces a more human-friendly output than 'render'.
--
--   This function should be used only for debugging purposes since it may change
--   the semantics of the input in order to create a prettier output.
--   In other words, running a LaTeX compiler in the output file of @renderFile fp l@ may
--   produce a different document than running it in the output of @writeFile fp (prettyLaTeX l)@.
--   You should use 'renderFile' unless you really need to read the LaTeX file.
--
prettyLaTeX :: LaTeX -> String
prettyLaTeX l = displayS (renderSmart 60 (docLaTeX l)) []
