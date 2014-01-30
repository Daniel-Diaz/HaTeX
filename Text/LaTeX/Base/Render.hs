
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

import Data.Text (Text,lines,unlines)
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Data.String
import Data.Text.Encoding
import Data.List (intersperse)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Numeric (showFFloat)

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
 --
 render = fromString . show

-- | This instance escapes LaTeX reserved characters.
instance Render Text where
 render = protectText

-- | Render every element of a list and append results.
renderAppend :: Render a => [a] -> Text
renderAppend = mconcat . fmap render

-- | Render every element of a list and append results,
--   separated by the given 'Char'.
renderChars :: Render a => Char -> [a] -> Text
renderChars c = mconcat . intersperse (fromString [c]) . fmap render

-- | Render every element of a list and append results,
--   separated by commas.
renderCommas :: Render a => [a] -> Text
renderCommas = renderChars ','

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
  
  render (TeXRaw t) = t
  
  render (TeXComm name []) = "\\" <> fromString name <> "{}"
  render (TeXComm name args) =
      "\\"
   <> fromString name
   <> renderAppend args
  render (TeXCommS name) = "\\" <> fromString name
  
  render (TeXEnv name args c) =
      "\\begin{"
   <> fromString name
   <> "}"
   <> renderAppend args
   <> render c
   <> "\\end{"
   <> fromString name
   <> "}"

  render (TeXMath Dollar l) = "$" <> render l <> "$"
  render (TeXMath Square l) = "\\[" <> render l <> "\\]"
  render (TeXMath Parentheses l) = "\\(" <> render l <> "\\)"

  render (TeXLineBreak m b) = "\\\\" <> maybe mempty (\x -> "[" <> render x <> "]") m <> ( if b then "*" else mempty )

  render (TeXOp sym l1 l2) = render l1 <> fromString sym <> render l2

  render (TeXBraces l) = "{" <> render l <> "}"

  render (TeXComment c) =
   let xs = Data.Text.lines c
   in if null xs then "%\n"
                 else Data.Text.unlines $ fmap ("%" <>) xs

  render (TeXSeq l1 l2) = render l1 <> render l2
  render TeXEmpty = mempty

instance Render TeXArg where
 render (OptArg l) = "[" <> render l <> "]"
 render (FixArg l) = "{" <> render l <> "}"
 render (MOptArg []) = mempty
 render (MOptArg ls) = "[" <> renderCommas ls <> "]"
 render (SymArg l) = "<" <> render l <> ">"
 render (MSymArg ls) = "<" <> renderCommas ls <> ">"

-- Other instances

-- | Show a signed floating number using standard decimal notation using 5 decimals.
showFloat :: RealFloat a => a -> String
showFloat x = showFFloat (Just 5) x []

instance Render Int where
instance Render Integer where
instance Render Float where
  render = fromString . showFloat
instance Render Double where
  render = fromString . showFloat
instance Render Word8 where

-- | 'Render' instance for 'Bool'. It satisfies @render True = "true"@ and @render False = "false"@.
instance Render Bool where
  render True = "true"
  render _ = "false"

instance Render a => Render [a] where
 render xs = "[" <> renderCommas xs <> "]"
