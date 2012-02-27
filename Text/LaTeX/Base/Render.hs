
{-# LANGUAGE OverloadedStrings #-}

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
   ) where

import Data.Text (Text)
import Text.LaTeX.Base.Syntax
import Data.String
import Data.Monoid (Monoid (..))
import qualified Data.Text.IO as T
import Data.Text.Encoding
import Data.List (intersperse)
import qualified Data.ByteString as B

-- | Class of values that can be transformed to 'Text'.
-- You mainly will use this to obtain the 'Text' output
-- of a 'LaTeX' value.
class Show a => Render a where
 render :: a -> Text
 --
 render = fromString . show

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
-- /Warning: /'rendertex'/ does not escape LaTeX reserver characters./
rendertex :: Render a => a -> LaTeX
rendertex = TeXRaw . render

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
 render (TeXMath l) = "$" <> render l <> "$"
 render (TeXNewLine b) = " \\\\" <> ( if b then "*" else mempty ) <> " "
 render (TeXOp sym l1 l2) = render l1 <> fromString sym <> render l2
 render (TeXBraces l) = "{" <> render l <> "}"
 render (TeXSeq l1 l2) = render l1 <> render l2
 render TeXEmpty = mempty

instance Render TeXArg where
 render (OptArg l) = "[" <> render l <> "]"
 render (FixArg l) = "{" <> render l <> "}"
 render (MOptArg []) = mempty
 render (MOptArg ls) =
     fromString "["
  <> renderCommas ls
  <> fromString "]"
 render (SymArg l) = "<" <> render l <> ">"
 render (MSymArg ls) = "<" <> renderCommas ls <> ">"

-- Other instances

instance Render Int where
instance Render Float where

