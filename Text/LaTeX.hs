
-- | This module is a re-export of the Base module.
--   You may find it shorter to import. Below you can
--   also find a short overview of HaTeX.
--
--   Historically, this module also exported the Packages
--   module. But, since it's more common to import the Base
--   module and, then, only the packages you need (instead
--   of all of them), this module has been upgraded supporting
--   it.
--
--   For this reason, the module @Text.LaTeX.Packages@ no longer
--   exists.
module Text.LaTeX
 ( -- * Base module re-export
   module Text.LaTeX.Base
   -- * An overview of HaTeX

   -- $ove

   -- ** The @LaTeX@ type

   -- $type

   -- ** Rendering LaTeX code

   -- $rnd

   -- ** Using more features

   -- $pkgs

   -- ** More from HaTeX

   -- $bey

   -- ** Using monads

   -- $wrt

   -- ** Examples

   -- $exm
   ) where

import Text.LaTeX.Base

-- $ove
-- HaTeX is a library that implements the LaTeX syntax for both rendering and parsing.

{- $type
The core type is called 'LaTeX'. Values of this type are always a syntactically correct
piece of LaTeX code, which we call a LaTeX /block/. To append blocks, we use the 'Monoid'
class. Thus, the operator '<>' appends blocks. To generate blocks, we use functions.
Basic functions are defined in the "Text.LaTeX.Base.Commands" module. Roughly speaking,
it contains functions to generate blocks containing LaTeX commands and environments that
are always defined. In the other hand, 'LaTeX' is an instance of the 'IsString' class.
This allow us to insert LaTeX code containing simple text by just writing it as a 'String'
and enabling the @OverlaodedStrings@ language extension.
-}

{- $rnd
Once you have a 'LaTeX' block built, the function 'render' will turn it into 'Text'. If
your intention is to write the output in a file, use 'renderFile' to write the LaTeX
code output directly into that file.
-}

{- $pkgs
Apart from the core commands and environments, HaTeX offers more functions to generate LaTeX
blocks containing more exotic things. These functions are categorized by LaTeX packages. For example,
those commands and environments that come from the LaTeX @babel@ package are under the module
"Text.LaTeX.Packages.Babel", and those that come from the @graphicx@ package are under "Text.LaTeX.Packages.Graphicx".
Import each package individually to use them.
This way, is easier to guess where to look for a particular function, and easier to detect
if a particular feature is missing.
-}

{- $bey
Beyond the implementation of existing LaTeX packages,
HaTeX also provides some useful functions to build LaTeX code blocks from Haskell values.
The Texy class allows you to pretty-print Haskell values to LaTeX blocks. This includes numbers, matrices,
vectors or trees. HaTeX also features some modules dedicated to the generation of TikZ scripts.
Everything you need to generate LaTeX code using Haskell should be included in this library.
If some feature is missing, the GitHub issue list is waiting for you <https://github.com/Daniel-Diaz/HaTeX/issues>.
-}

{- $wrt
LaTeX blocks can also be managed by the 'LaTeXT' monad transformer. Similar to the 'WriterT' monad,
it stores and append values from a 'Monoid', in this case, the 'Monoid' of the 'LaTeX' values.
Both interfaces are fused into one under the 'LaTeXC' class, the class of LaTeX blocks.
Particular documentation of each feature can be found in the corresponding module.
Further explanation of the library and its concepts can be found in the /HaTeX User's Guide/
(<http://daniel-diaz.github.com/projects/hatex/HaTeX-Guide.pdf>).
-}

{- $exm
Some examples can be found in the source code, under the /Examples/ directory. In particular, the example
contained in the file @simple.hs@ is intended to be read by new users of the library.
If you have any question regarding one of the examples, there is something you want to ask about
HaTeX, or for anything you would like to discuss, we have a mailing list at <http://projects.haskell.org/cgi-bin/mailman/listinfo/hatex>.
-}
