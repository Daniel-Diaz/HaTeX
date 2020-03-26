
{-# LANGUAGE OverloadedStrings #-}

-- | The final purpose of this module is to render a Text value
--   from a 'LaTeX' value. The interface is abstracted via a typeclass
--   so you can cast to 'Text' other types as well. Also, some other
--   handy 'Text'-related functions are defined.
module Text.LaTeX.Base.Render
 ( -- * Re-exports
   Text
 , module Data.String
   -- * Render class
 , Render (..)
 , renderAppend
 , renderChars
 , renderCommas
 , renderFile
 , rendertex
   -- * Reading files
 , readFileTex
   -- * Util
 , showFloat
   ) where

import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Data.String
import Data.List (intersperse)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Numeric (showFFloat)
import Data.Text (Text,lines,unlines)
import Data.Text.Encoding
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.Builder.RealFloat as Builder

-- | Class of values that can be transformed to 'Text'.
-- You mainly will use this to obtain the 'Text' output
-- of a 'LaTeX' value. If you are going to write the result
-- in a file, consider to use 'renderFile'.
--
-- Consider also to use 'rendertex' to get 'Render'able values
-- into 'LaTeX' blocks.
--
-- If you want to make a type instance of 'Render' and you already
-- have a 'Show' instance, you can use the default instance.
--
-- > render = fromString . show
--
class Show a => Render a where
 render :: a -> Text
 renderBuilder :: a -> Builder
 --
 render = fromString . show
 renderBuilder = Builder.fromText . render

renderDefault :: Render a => a -> Text
renderDefault = toStrict . Builder.toLazyText . renderBuilder

-- | This instance escapes LaTeX reserved characters.
instance Render Text where
 render = protectText

-- | Render every element of a list and append results.
renderAppend :: Render a => [a] -> Text
renderAppend = mconcat . fmap render

renderAppendBuilder :: Render a => [a] -> Builder
renderAppendBuilder = foldMap renderBuilder

-- | Render every element of a list and append results,
--   separated by the given 'Char'.
renderChars :: Render a => Char -> [a] -> Text
renderChars c = mconcat . intersperse (fromString [c]) . fmap render

renderCharsBuilder :: Render a => Char -> [a] -> Builder
renderCharsBuilder c = mconcat . intersperse (Builder.singleton c) . fmap renderBuilder

-- | Render every element of a list and append results,
--   separated by commas.
renderCommas :: Render a => [a] -> Text
renderCommas = renderChars ','

renderCommasBuilder :: Render a => [a] -> Builder
renderCommasBuilder = renderCharsBuilder ','

-- | Use this function to render a 'LaTeX' (or another
--   one in the 'Render' class) value directly
--   in a file.
renderFile :: Render a => FilePath -> a -> IO ()
renderFile f = B.writeFile f . encodeUtf8 . render

-- | If you are going to insert the content of a file
-- in your 'LaTeX' data, use this function to ensure
-- your encoding is correct.
readFileTex :: FilePath -> IO Text
readFileTex = fmap decodeUtf8 . B.readFile

-- | If you can transform a value to 'Text', you can
--   insert that 'Text' in your 'LaTeX' code.
--   That is what this function does.
--
-- /Warning: /'rendertex'/ does not escape LaTeX reserved characters./
-- /Use /'protectText'/ to escape them./
rendertex :: (Render a,LaTeXC l) => a -> l
rendertex = fromLaTeX . TeXRaw . render

-- Render instances

instance Render Measure where
 render (Pt x) = render x <> "pt"
 render (Mm x) = render x <> "mm"
 render (Cm x) = render x <> "cm"
 render (In x) = render x <> "in"
 render (Ex x) = render x <> "ex"
 render (Em x) = render x <> "em"
 render (CustomMeasure x) = render x

-- LaTeX instances

instance Render LaTeX where
  
  renderBuilder (TeXRaw t) = Builder.fromText t
  
  renderBuilder (TeXComm name []) = "\\" <> fromString name <> "{}"
  renderBuilder (TeXComm name args) =
      "\\"
   <> fromString name
   <> renderAppendBuilder args
  renderBuilder (TeXCommS name) = "\\" <> fromString name
  
  renderBuilder (TeXEnv name args c) =
      "\\begin{"
   <> fromString name
   <> "}"
   <> renderAppendBuilder args
   <> renderBuilder c
   <> "\\end{"
   <> fromString name
   <> "}"

  renderBuilder (TeXMath Dollar l) = "$" <> renderBuilder l <> "$"
  renderBuilder (TeXMath DoubleDollar l) = "$$" <> renderBuilder l <> "$$"
  renderBuilder (TeXMath Square l) = "\\[" <> renderBuilder l <> "\\]"
  renderBuilder (TeXMath Parentheses l) = "\\(" <> renderBuilder l <> "\\)"

  renderBuilder (TeXLineBreak m b) = "\\\\" <> maybe mempty (\x -> "[" <> renderBuilder x <> "]") m <> ( if b then "*" else mempty )

  renderBuilder (TeXBraces l) = "{" <> renderBuilder l <> "}"

  renderBuilder (TeXComment c) =
   let xs = Data.Text.lines c
   in if null xs then "%\n"
                 else Builder.fromText $ Data.Text.unlines $ fmap ("%" <>) xs

  renderBuilder (TeXSeq l1 l2) = renderBuilder l1 <> renderBuilder l2
  renderBuilder TeXEmpty = mempty

  render = renderDefault

instance Render TeXArg where
 renderBuilder (FixArg l) = "{" <> renderBuilder l <> "}"
 renderBuilder (OptArg l) = "[" <> renderBuilder l <> "]"
 renderBuilder (MOptArg []) = mempty
 renderBuilder (MOptArg ls) = "[" <> renderCommasBuilder ls <> "]"
 renderBuilder (SymArg l) = "<" <> renderBuilder l <> ">"
 renderBuilder (MSymArg []) = mempty
 renderBuilder (MSymArg ls) = "<" <> renderCommasBuilder ls <> ">"
 renderBuilder (ParArg l) = "(" <> renderBuilder l <> ")"
 renderBuilder (MParArg []) = mempty
 renderBuilder (MParArg ls) = "(" <> renderCommasBuilder ls <> ")"
 render = renderDefault

-- Other instances

-- | Show a signed floating number using standard decimal notation using 5 decimals.
showFloat :: RealFloat a => a -> String
showFloat x = showFFloat (Just 5) x []

instance Render Int where
  renderBuilder = Builder.decimal
  render = renderDefault

instance Render Integer where
  renderBuilder = Builder.decimal
  render = renderDefault

instance Render Float where
  renderBuilder = Builder.formatRealFloat Builder.Fixed (Just 5)
  render = renderDefault

instance Render Double where
  renderBuilder = Builder.formatRealFloat Builder.Fixed (Just 5)
  render = renderDefault

instance Render Word8 where
  renderBuilder = Builder.decimal
  render = renderDefault

-- | 'Render' instance for 'Bool'. It satisfies @render True = "true"@ and @render False = "false"@.
instance Render Bool where
  render True = "true"
  render _ = "false"

instance Render a => Render [a] where
 renderBuilder xs = "[" <> renderCommasBuilder xs <> "]"
 render = renderDefault
