{-# LANGUAGE OverloadedStrings #-}

-- | LaTeX standard commands and environments.
module Text.LaTeX.Base.Commands
 ( -- * Basic functions
   raw , between , comment , (%:)
   -- * Preamble commands
 , title
 , author
 , date
 , institute
 , thanks
 , documentclass
 , usepackage
 , linespread
   -- ** Classes
   -- *** Document classes
 , article
 , proc
 , report
 , minimal
 , book
 , slides
   -- *** Class options
 , ClassOption (..)
 , customopt
 , draft
 , titlepage
 , notitlepage
 , onecolumn
 , twocolumn
 , oneside
 , twoside
 , landscape
 , openright
 , openany
 , fleqn
 , leqno
   -- ** Paper sizes
 , PaperType (..)
 , a0paper
 , a1paper
 , a2paper
 , a3paper
 , a4paper
 , a5paper
 , a6paper
 , b0paper
 , b1paper
 , b2paper
 , b3paper
 , b4paper
 , b5paper
 , b6paper
 , letterpaper
 , executivepaper
 , legalpaper
   -- ** Page styles
 , pagestyle
 , thispagestyle
 , plain
 , headings
 , empty
 , myheadings
 , markboth
 , markright
   -- * Body commands
 , document
 , maketitle
   -- ** Document structure
 , tableofcontents
 , abstract
 , appendix
   -- *** Sections
 , part
 , chapter
 , section
 , subsection
 , subsubsection
 , paragraph
 , subparagraph
   -- ** Logos & symbols
 , today
 , thePage
 , tex
 , latex
 , laTeX2
 , laTeXe
 , ldots
 , vdots
 , ddots
 -- *** HaTeX specific
 , hatex
 , hatex3
 , version
 , hatex_version
 -- ** Document layout
 , par
 , newline
 , lnbk
 , lnbk_
 , newpage
 , cleardoublepage
 , clearpage
 , linebreak
 , nolinebreak
 , pagebreak
 , nopagebreak
 , hspace
 , hspace_
 , vspace
 , hfill
 , vfill
 , stretch
 , smallskip
 , bigskip
 , indent
 , noindent
   -- *** Document measures
 , textwidth
 , linewidth
   -- ** Formatting text
 , verbatim , verb
   -- *** Fonts
   --
   -- Different font styles.
 , textbf
 , textit
 , texttt
 , textrm
 , textsf
 , textmd
 , textup
 , textsl
 , textsc
 , textnormal
 , underline
 , emph
   -- *** Sizes
   --
   -- | Sizes are sorted from smallest to biggest.
 , tiny
 , scriptsize
 , footnotesize
 , small
 , normalsize
 , large
 , large2
 , large3
 , huge
 , huge2
   -- ** Environments
   -- | Math environments, such as @equation@, defined in "Text.LaTeX.Packages.AMSMath".
 , enumerate
 , itemize
 , item
 , flushleft
 , flushright
 , center
 , quote
 , verse
 , cite
 , description
 , minipage
 , figure
   -- ** Page numbering
 , pagenumbering
 , arabic
 , roman
 , roman_
 , alph
 , alph_
   -- ** Boxes
 , mbox
 , fbox
 , parbox
 , framebox
 , makebox
 , raisebox
 , rule
   -- ** Cross references
 , caption
 , label
 , ref
 , pageref
   -- ** Tables
 , tabular
 , (&)
 , hline
 , cline
 , multicolumn
   -- ** Others
 , footnote
 , protect
 , hyphenation
 , hyp
 , qts
   -- * External files
 , input
 , include
   ) where

import Data.String
import Data.Maybe (isNothing, catMaybes)
import Data.Text (toLower)
import qualified Data.Text as T
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types
import Data.Version
import Data.List (find, intercalate)
--
import Paths_HaTeX

-- | Insert a raw piece of 'Text'.
-- This functions doesn't care about @LaTeX@ reserved characters,
-- it insert the text just as it is received.
--
-- /Warning:/ This function is /unsafe/, in the sense that it does
-- not check that the input text is a valid LaTeX /block/.
-- Make sure any braces, commands or environments are properly closed.
raw :: LaTeXC l => Text -> l
raw = fromLaTeX . TeXRaw

-- | Calling 'between' @c l1 l2@ puts @c@ between @l1@ and @l2@ and
--   appends them.
--
-- > between c l1 l2 = l1 <> c <> l2
between :: Monoid m => m -> m -> m -> m
between c l1 l2 = l1 <> c <> l2

-- | Create a comment.
comment :: LaTeXC l => Text -> l
comment = fromLaTeX . TeXComment

-- | This operator appends a comment after a expression.
--   For example:
--
-- > textbf "I'm just an example." %: "Insert a few words here."
--
-- The implementation is
--
-- > (%:) l = (l <>) . comment
--
-- Since you are writing in Haskell, you may not need to output comments
-- as you can add them in the Haskell source. I added this feature
-- for completeness. It may be useful for debugging the output as well.
(%:) :: LaTeXC l => l -> Text -> l
(%:) l = (l <>) . comment

-- | Generate the title. It normally contains the 'title' name
-- of your document, the 'author'(s) and 'date'.
maketitle :: LaTeXC l => l
maketitle = comm0 "maketitle"

-- | Set the title of your document.
title :: LaTeXC l => l -> l
title = comm1 "title"

-- | Set a date for your document.
date :: LaTeXC l => l -> l
date = comm1 "date"

-- | Set the author(s) of the document.
author :: LaTeXC l => l -> l
author = comm1 "author"

-- | Set either an institute or an organization
-- for the document. It does /not/ work for
-- a document of the 'article' class.
institute :: LaTeXC l => Maybe l -> l -> l
institute  Nothing = liftL $ \l -> TeXComm "institute" [FixArg l]
institute (Just s) = liftL2 (\l1 l2 -> TeXComm "institute" [OptArg l1,FixArg l2]) s

thanks :: LaTeXC l => l -> l
thanks = comm1 "thanks"

-- | Import a package. First argument is a list of options for
-- the package named in the second argument.
usepackage :: LaTeXC l => [l] -> PackageName -> l
usepackage ls pn = liftListL (\ls_ -> TeXComm "usepackage" [MOptArg ls_ ,FixArg $ fromString pn]) ls

-- | The @LaTeX@ logo.
latex :: LaTeXC l => l
latex = comm0 "LaTeX"

-- | Start a new paragraph
par :: LaTeXC l => l
par = comm0 "par"

-- | Start a new line.
newline :: LaTeXC l => l
newline = comm0 "newline"

part :: LaTeXC l => l -> l
part = comm1 "part"

chapter :: LaTeXC l => l -> l
chapter = comm1 "chapter"

-- | Start a new section with a given title.
section :: LaTeXC l => l -> l
section = comm1 "section"

-- | Start a new subsection.
subsection :: LaTeXC l => l -> l
subsection = comm1 "subsection"

-- | Start a new sub/sub/section.
subsubsection :: LaTeXC l => l -> l
subsubsection = comm1 "subsubsection"

-- | Start a paragraph.
paragraph :: LaTeXC l => l -> l
paragraph = comm1 "paragraph"

-- | Start a subparagraph (minimal level of sectioning).
subparagraph :: LaTeXC l => l -> l
subparagraph = comm1 "subparagraph"

-- | Create the table of contents, automatically generated
-- from your 'section's, 'subsection's, and other related stuff.
tableofcontents :: LaTeXC l => l
tableofcontents = comm0 "tableofcontents"

appendix :: LaTeXC l => l
appendix = comm0 "appendix"

-- | An item of a list (see 'enumerate' or 'itemize').
--   The optional argument sets the design of the item.
item :: LaTeXC l => Maybe l -> l
item Nothing    = commS "item "
item (Just opt) = liftL (\opt_ -> TeXComm "item" [OptArg opt_]) opt

-- | Environment of ordered lists. Use 'item' to start each list
--   item.
enumerate :: LaTeXC l => l -> l
enumerate = liftL $ TeXEnv "enumerate" []

-- | Environment of unordered lists. Use 'item' to start each list
--   item.
itemize :: LaTeXC l => l -> l
itemize = liftL $ TeXEnv "itemize" []

description :: LaTeXC l => l -> l
description = liftL $ TeXEnv "description" []

-- | Left-justify the argument.
flushleft :: LaTeXC l => l -> l
flushleft = liftL $ TeXEnv "flushleft" []

-- | Right-justify the argument.
flushright :: LaTeXC l => l -> l
flushright = liftL $ TeXEnv "flushright" []

-- | Center-justify the argument.
center :: LaTeXC l => l -> l
center = liftL $ TeXEnv "center" []

quote :: LaTeXC l => l -> l
quote = liftL $ TeXEnv "quote" []

verse :: LaTeXC l => l -> l
verse = liftL $ TeXEnv "verse" []

-- | Minipage environment.
minipage :: LaTeXC l =>
            Maybe Pos -- ^ Optional position
         -> l         -- ^ Width
         -> l         -- ^ Minipage content
         -> l
minipage Nothing  = liftL2 $ \ts -> TeXEnv "minipage" [ FixArg ts ]
minipage (Just p) = liftL2 $ \ts -> TeXEnv "minipage" [ OptArg $ rendertex p , FixArg ts ]

-- | Figure environment.
figure :: LaTeXC l =>
          Maybe Pos -- ^ Optional position
       -> l         -- ^ Figure content
       -> l
figure Nothing  = liftL $ TeXEnv "figure" []
figure (Just p) = liftL $ TeXEnv "figure" [ OptArg $ TeXRaw $ render p ]

-- | Abstract section.
abstract :: LaTeXC l => l -> l
abstract = liftL $ TeXEnv "abstract" []

cite :: LaTeXC l => l -> l
cite = liftL $ \l -> TeXComm "cite" [FixArg l]

-- Document class

-- | A class option to be passed to the 'documentclass' function.
data ClassOption =
   Draft
 | TitlePage
 | NoTitlePage
 | OneColumn
 | TwoColumn
 | OneSide
 | TwoSide
 | Landscape
 | OpenRight
 | OpenAny
 | Fleqn
 | Leqno
 | FontSize Measure
 | Paper PaperType
 | CustomOption String
   deriving Show

instance Render ClassOption where
 render (FontSize m) = render m
 render (Paper pt) = toLower (render pt) <> "paper"
 render (CustomOption str) = fromString str
 render co = toLower $ fromString $ show co

customopt :: String -> ClassOption
customopt = CustomOption

instance IsString ClassOption where
 fromString = customopt

-- | LaTeX available paper types.
data PaperType =
   A0 | A1 | A2 | A3 | A4 | A5 | A6
 | B0 | B1 | B2 | B3 | B4 | B5 | B6
 | Letter | Executive | Legal
   deriving Show

instance Render PaperType where

-- | Set the document class. Needed in all documents.
documentclass :: LaTeXC l =>
                [ClassOption] -- ^ Class options
              -> ClassName    -- ^ Class name
              -> l
documentclass opts cn = fromLaTeX $ TeXComm "documentclass" [MOptArg $ fmap rendertex opts , FixArg $ fromString cn]

article :: ClassName
article = "article"

proc :: ClassName
proc = "proc"

minimal :: ClassName
minimal = "minimal"

report :: ClassName
report = "report"

book :: ClassName
book = "book"

slides :: ClassName
slides = "slides"

a0paper :: ClassOption
a0paper = Paper A0

a1paper :: ClassOption
a1paper = Paper A1

a2paper :: ClassOption
a2paper = Paper A2

a3paper :: ClassOption
a3paper = Paper A3

a4paper :: ClassOption
a4paper = Paper A4

a5paper :: ClassOption
a5paper = Paper A5

a6paper :: ClassOption
a6paper = Paper A6

b0paper :: ClassOption
b0paper = Paper B0

b1paper :: ClassOption
b1paper = Paper B1

b2paper :: ClassOption
b2paper = Paper B2

b3paper :: ClassOption
b3paper = Paper B3

b4paper :: ClassOption
b4paper = Paper B4

b5paper :: ClassOption
b5paper = Paper B5

b6paper :: ClassOption
b6paper = Paper B6

letterpaper :: ClassOption
letterpaper = Paper Letter

executivepaper :: ClassOption
executivepaper = Paper Executive

legalpaper :: ClassOption
legalpaper = Paper Legal

draft :: ClassOption
draft = Draft

-- | Typesets displayed formulae left-aligned instead of centred.
fleqn :: ClassOption
fleqn = Fleqn

-- | Places the numbering of formulae on the left hand side instead of the right.
leqno :: ClassOption
leqno = Leqno

titlepage :: ClassOption
titlepage = TitlePage

notitlepage :: ClassOption
notitlepage = NoTitlePage

onecolumn :: ClassOption
onecolumn = OneColumn

twocolumn :: ClassOption
twocolumn = TwoColumn

oneside :: ClassOption
oneside = OneSide

twoside :: ClassOption
twoside = TwoSide

-- | Changes the layout of the document to print in landscape mode
landscape :: ClassOption
landscape = Landscape

-- | Makes chapters begin either only on right hand pages
openright :: ClassOption
openright = OpenRight

-- | Makes chapters begin on the next page available.
openany :: ClassOption
openany = OpenAny

-- | The 'document' environment contains the body of the document.
document :: LaTeXC l => l -> l
document = liftL $ TeXEnv "document" []

pagenumbering :: LaTeXC l => l -> l
pagenumbering = liftL $ \l -> TeXComm "pagenumbering" [FixArg l]

-- | Arabic numerals.
arabic :: LaTeXC l => l
arabic = fromLaTeX "arabic"

-- | Lowercase roman numerals.
roman :: LaTeXC l => l
roman = fromLaTeX "roman"

-- | Uppercase roman numerals.
roman_ :: LaTeXC l => l
roman_ = fromLaTeX "Roman"

-- | Lowercase letters.
alph :: LaTeXC l => l
alph = fromLaTeX "alph"

-- | Uppercase letters.
alph_ :: LaTeXC l => l
alph_ = fromLaTeX "Alph"

-- Page styles

pagestyle :: LaTeXC l => PageStyle -> l
pagestyle ps = fromLaTeX $ TeXComm "pagestyle" [FixArg $ fromString ps]

thispagestyle :: LaTeXC l => PageStyle -> l
thispagestyle ps = fromLaTeX $ TeXComm "thispagestyle" [FixArg $ fromString ps]

plain :: PageStyle
plain = "plain"

headings :: PageStyle
headings = "headings"

empty :: PageStyle
empty = "empty"

myheadings :: PageStyle
myheadings = "myheadings"

-- | Used in conjunction with 'myheadings' for setting both the left and the right heading.
markboth :: LaTeXC l => l -> l -> l
markboth = liftL2 $ \l1 l2 -> TeXComm "markboth" [FixArg l1 , FixArg l2]

-- | Used in conjunction with 'myheadings' for setting the right heading.
markright :: LaTeXC l => l -> l
markright = liftL $ \l -> TeXComm "markright" [FixArg l]

-- | Start a new line. In a 'tabular', it starts a new row, so use 'newline' instead.
lnbk  :: LaTeXC l => l
lnbk = fromLaTeX $ TeXLineBreak Nothing False

lnbk_ :: LaTeXC l => l
lnbk_ = fromLaTeX $ TeXLineBreak Nothing True

hyp :: LaTeXC l => l
hyp = fromLaTeX $ TeXCommS "-"

cleardoublepage :: LaTeXC l => l
cleardoublepage = comm0 "cleardoublepage"

clearpage :: LaTeXC l => l
clearpage = comm0 "clearpage"

newpage :: LaTeXC l => l
newpage = comm0 "newpage"

linebreak :: LaTeXC l => l -> l
linebreak = liftL $ \l -> TeXComm "linebreak" [OptArg l]

nolinebreak :: LaTeXC l => l -> l
nolinebreak = liftL $ \l -> TeXComm "nolinebreak" [OptArg l]

nopagebreak :: LaTeXC l => l -> l
nopagebreak = liftL $ \l -> TeXComm "nopagebreak" [OptArg l]

pagebreak :: LaTeXC l => l -> l
pagebreak = liftL $ \l -> TeXComm "pagebreak" [OptArg l]

hyphenation :: LaTeXC l => l -> l
hyphenation = liftL $ \l -> TeXComm "hyphenation" [FixArg l]

mbox :: LaTeXC l => l -> l
mbox = liftL $ \l -> TeXComm "mbox" [FixArg l]

fbox :: LaTeXC l => l -> l
fbox = liftL $ \l -> TeXComm "fbox" [FixArg l]

-- | Render the date at compilation time.
today :: LaTeXC l => l
today = comm0 "today"

-- | Render the current page.
thePage :: LaTeXC l => l
thePage = comm0 "thepage"

-- | TeX logo.
tex :: LaTeXC l => l
tex = comm0 "TeX"

-- | LaTeX logo.
laTeX2 :: LaTeXC l => l
laTeX2 = comm0 "LaTeX"

laTeXe :: LaTeXC l => l
laTeXe = comm0 "LaTeXe"

-- | Horizontal dots.
ldots :: LaTeXC l => l
ldots = comm0 "ldots"

-- | Vertical dots.
vdots :: LaTeXC l => l
vdots = comm0 "vdots"

-- | Diagonal dots.
ddots :: LaTeXC l => l
ddots = comm0 "ddots"

-- | Quotation marks.
qts :: LaTeXC l => l -> l
qts l = between l (raw "``") (raw "''")

footnote :: LaTeXC l => l -> l
footnote = liftL $ \l -> TeXComm "footnote" [FixArg l]

linespread :: LaTeXC l => Float -> l
linespread x = fromLaTeX $ TeXComm "linespread" [FixArg $ rendertex x]

indent :: LaTeXC l => l
indent = comm0 "indent"

noindent :: LaTeXC l => l
noindent = comm0 "noindent"

hspace :: LaTeXC l => Measure -> l
hspace m = fromLaTeX $ TeXComm "hspace" [FixArg $ rendertex m]

hspace_ :: LaTeXC l => Measure -> l
hspace_ m = fromLaTeX $ TeXComm "hspace*" [FixArg $ rendertex m]

stretch :: LaTeXC l => Int -> l
stretch n = fromLaTeX $ TeXComm "stretch" [FixArg $ rendertex n]

vspace :: LaTeXC l => Measure -> l
vspace m = fromLaTeX $ TeXComm "vspace" [FixArg $ rendertex m]

-- | Fill out all available horizontal space.
hfill :: LaTeXC l => l
hfill = comm0 "hfill"

-- | Fill out all available vertical space.
vfill :: LaTeXC l => l
vfill = comm0 "vfill"

protect :: LaTeXC l => l -> l
protect l = commS "protect" <> l

textwidth :: LaTeXC l => l
textwidth = comm0 "textwidth"

linewidth :: LaTeXC l => l
linewidth = comm0 "linewidth"

-- | The point of 'verbatim' is to include text that will
-- /not/ be parsed as LaTeX in any way at all, but should simply
-- appear as given in the document, in a separate display
-- in typewriter font.
verbatim :: LaTeXC l => Text -> l
verbatim = liftL (TeXEnv "verbatim" []) . raw

-- | Include text, as given and in typewriter, but in-line.
-- Note that, for LaTeX-specific technical reasons, verbatim
-- text can generally only be used \"at the top level\", not
-- in e.g. section titles or other command-arguments.
--
-- Unlike 'verbatim', which LaTeX implements as an ordinary environment,
-- its command 'verb' uses a syntax trick to avoid braking its parsing
-- when the literal text contains a closing brace: rather than using braces
-- at all, the first character after @\\verb@ will be the right delimiter as well.
-- Translating this method to HaTeX wouldn't really make sense since Haskell
-- has string literals with their own escaping possibilities; instead, we make
-- it secure by automatically choosing a delimiter that does not turn up 
-- in the given string.
verb :: LaTeXC l => Text -> l
verb vbStr = case find (isNothing . (`T.find`vbStr) . (==))
                  $ "`'\"|=-~$#+/!^_;:,." ++ ['0'..'9'] ++ ['A'..'B'] ++ ['a'..'b']
              of Just delim -> let d = T.singleton delim
                               in raw $ T.concat [ "\\verb", d, vbStr, d ]
                 Nothing    -> let (lpart, rpart)
                                     = T.splitAt (T.length vbStr `quot` 2) vbStr
                               in verb lpart <> verb rpart
             -- If all suitable delimiter characters are already used in the verbatim
             -- string (which really should never happen as this is intended for **short**
             -- in-line displays!) then split the verbatim string in two sections; at
             -- some point they will necessarily lack some of the characters.

underline :: LaTeXC l => l -> l
underline = liftL $ \l -> TeXComm "underline" [FixArg l]

emph :: LaTeXC l => l -> l
emph = liftL $ \l -> TeXComm "emph" [FixArg l]

textrm :: LaTeXC l => l -> l
textrm = liftL $ \l -> TeXComm "textrm" [FixArg l]

textsf :: LaTeXC l => l -> l
textsf = liftL $ \l -> TeXComm "textsf" [FixArg l]

-- | Set the given argument to monospaced font.
texttt :: LaTeXC l => l -> l
texttt = liftL $ \l -> TeXComm "texttt" [FixArg l]

textmd :: LaTeXC l => l -> l
textmd = liftL $ \l -> TeXComm "textmd" [FixArg l]

-- | Set the given argument to bold font face.
textbf :: LaTeXC l => l -> l
textbf = liftL $ \l -> TeXComm "textbf" [FixArg l]

textup :: LaTeXC l => l -> l
textup = liftL $ \l -> TeXComm "textup" [FixArg l]

-- Set the given argument to italic font face.
textit :: LaTeXC l => l -> l
textit = liftL $ \l -> TeXComm "textit" [FixArg l]

textsl :: LaTeXC l => l -> l
textsl = liftL $ \l -> TeXComm "textsl" [FixArg l]

-- | Set the given argument to small caps format.
textsc :: LaTeXC l => l -> l
textsc = liftL $ \l -> TeXComm "textsc" [FixArg l]

textnormal :: LaTeXC l => l -> l
textnormal = liftL $ \l -> TeXComm "textnormal" [FixArg l]

--------------------
-- Standard sizes --
--------------------

sizecomm :: LaTeXC l => String -> l -> l
sizecomm str = liftL $ \l -> TeXBraces $ comm0 str <> l

tiny :: LaTeXC l => l -> l
tiny = sizecomm "tiny"

scriptsize :: LaTeXC l => l -> l
scriptsize = sizecomm "scriptsize"

footnotesize :: LaTeXC l => l -> l
footnotesize = sizecomm "footnotesize"

small :: LaTeXC l => l -> l
small = sizecomm "small"

normalsize :: LaTeXC l => l -> l
normalsize = sizecomm "normalsize"

large :: LaTeXC l => l -> l
large = sizecomm "large"

large2 :: LaTeXC l => l -> l
large2 = sizecomm "Large"

large3 :: LaTeXC l => l -> l
large3 = sizecomm "LARGE"

huge :: LaTeXC l => l -> l
huge = sizecomm "huge"

huge2 :: LaTeXC l => l -> l
huge2 = sizecomm "Huge"

--------------------

smallskip :: LaTeXC l => l
smallskip = comm0 "smallskip"

bigskip :: LaTeXC l => l
bigskip = comm0 "bigskip"

-- | The 'tabular' environment can be used to typeset tables with optional horizontal and vertical lines.
tabular :: LaTeXC l =>
           Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                       --   Defaulted to 'Center'.
        -> [TableSpec] -- ^ Table specification of columns and vertical lines.
        -> l       -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
        -> l       -- ^ Resulting table syntax.
tabular Nothing ts  = liftL $ TeXEnv "tabular" [ FixArg $ TeXRaw $ renderAppend ts ]
tabular (Just p) ts = liftL $ TeXEnv "tabular" [ OptArg $ TeXRaw $ render p , FixArg $ TeXRaw $ renderAppend ts ]

-- | Column separator.
(&) :: LaTeXC l => l -> l -> l
(&) = between "&"

-- | Horizontal line.
hline :: LaTeXC l => l
hline = commS "hline "

-- | Cell taking multiple columns.
multicolumn :: LaTeXC l => Int -> [TableSpec] -> l -> l
multicolumn n c = liftL $ \l -> TeXComm "multicolumn"
  [ FixArg $ rendertex n
  , FixArg . TeXRaw $ renderAppend c
  , FixArg l
  ]

-- | @cline i j@ writes a partial horizontal line beginning in column @i@ and ending in column @j@.
cline :: LaTeXC l => Int -> Int -> l
cline i j = fromLaTeX $ TeXComm "cline" [ FixArg $ TeXRaw $ render i <> "-" <> render j ]

parbox :: LaTeXC l => Maybe Pos -> Measure -> l -> l
parbox Nothing w = liftL $ \t -> TeXComm "parbox" [ FixArg $ rendertex w , FixArg t ]
parbox (Just p) w = liftL $ \t -> TeXComm "parbox" [ OptArg $ TeXRaw $ render p
                                                   , FixArg $ TeXRaw $ render w
                                                   , FixArg t ]

makebox :: LaTeXC l => Maybe Measure -> Maybe Pos -> l -> l
makebox Nothing  Nothing  = liftL $ \t -> TeXComm "makebox" [ FixArg t ]
makebox (Just w) Nothing  = liftL $ \t -> TeXComm "makebox" [ OptArg $ TeXRaw $ render w
                                                            , FixArg t ]
makebox Nothing (Just p)  = liftL $ \t -> TeXComm "makebox" [ OptArg $ TeXRaw $ render p
                                                            , FixArg t ]
makebox (Just w) (Just p) = liftL $ \t -> TeXComm "makebox" [ OptArg $ TeXRaw $ render w
                                                            , OptArg $ TeXRaw $ render p
                                                            , FixArg t ]	

framebox :: LaTeXC l =>  Maybe Measure -> Maybe Pos -> l -> l
framebox Nothing Nothing   = liftL $ \t -> TeXComm "framebox" [ FixArg t ]
framebox (Just w) Nothing  = liftL $ \t -> TeXComm "framebox" [ OptArg $ TeXRaw $ render w
                                                              , FixArg t ]
framebox Nothing (Just p)  = liftL $ \t -> TeXComm "framebox" [ OptArg $ TeXRaw $ render p
                                                              , FixArg t ]
framebox (Just w) (Just p) = liftL $ \t -> TeXComm "framebox" [ OptArg $ TeXRaw $ render w
                                                              , OptArg $ TeXRaw $ render p
                                                              , FixArg t ]

raisebox :: LaTeXC l => Measure -> Maybe Measure -> Maybe Measure -> l -> l
raisebox m ma mb = liftL $ \l -> TeXComm "raisebox" $
    [ FixArg $ rendertex m ]
 ++   fmap (OptArg . rendertex) (catMaybes [ma,mb])
 ++ [ FixArg l ]

-- | Produce a simple black box.
rule :: LaTeXC l =>
        Maybe Measure -- ^ Optional lifting.
     -> Measure       -- ^ Width.
     -> Measure       -- ^ Height.
     -> l
rule Nothing w h  = fromLaTeX $ TeXComm "rule" [ FixArg $ TeXRaw $ render w
                                               , FixArg $ TeXRaw $ render h ]
rule (Just l) w h = fromLaTeX $ TeXComm "rule" [ OptArg $ TeXRaw $ render l
                                               , FixArg $ TeXRaw $ render w
                                               , FixArg $ TeXRaw $ render h ]

-- HaTeX specific symbols

-- | Print the HaTeX logo.
hatex :: LaTeXC l => l
hatex = mbox $ "H"
     <> hspace (Ex $ negate 0.3)
     <> textsc "a"
     <> hspace (Ex $ negate 0.3)
     <> tex

-- | Print the HaTeX 3 logo.
hatex3 :: LaTeXC l => l
hatex3 = hatex <> emph (textbf "3")

-- | Print the HaTeX logo, beside the complete version number.
hatex_version :: LaTeXC l => l
hatex_version = hatex
             <> emph (textbf $ rendertex x)
             <> hspace (Ex $ negate 0.3)
             <> emph ("." <> fromString (intercalate "." $ fmap show xs))
 where
  (x:xs) = versionBranch version

caption :: LaTeXC l => l -> l
caption = liftL $ \l -> TeXComm "caption" [FixArg l]

label :: LaTeXC l => l -> l
label = liftL $ \l -> TeXComm "label" [FixArg $ TeXRaw $ render l]

ref :: LaTeXC l => l -> l
ref = liftL $ \l -> TeXComm "ref" [FixArg $ TeXRaw $ render l]

pageref :: LaTeXC l => l -> l
pageref = liftL $ \l -> TeXComm "pageref" [FixArg $ TeXRaw $ render l]

-- Exteral files

-- | Import an external file and insert its content /as it is/.
input :: LaTeXC l => FilePath -> l
input fp = fromLaTeX $ TeXComm "input" [FixArg $ TeXRaw $ fromString fp]

-- | Similar to 'input', but forces a page break.
--
-- /Note: the file you are including cannot include other files./
include :: LaTeXC l => FilePath -> l
include fp = fromLaTeX $ TeXComm "include" [FixArg $ TeXRaw $ fromString fp]
