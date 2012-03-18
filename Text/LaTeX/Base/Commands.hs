{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HATEX MakeMonadic #-}

-- | LaTeX standard commands and environments.
module Text.LaTeX.Base.Commands
 ( -- * Basic functions
   raw , between , comment , (%)
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
 , hatex_meta
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
 , stretch
 , smallskip
 , bigskip
 , indent
 , noindent
   -- *** Document measures
 , textwidth
 , linewidth
   -- ** Formatting text
 , verbatim
   -- *** Fonts
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
 , equation
 , equation_
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
   -- * Cross references
 , caption
 , label
 , ref
 , pageref
   -- ** Tables
 , tabular
 , (&)
 , hline
 , cline
   -- ** Others
 , footnote
 , protect
 , hyphenation
 , hyp
 , qts
   ) where

import Data.String
import Data.Maybe (catMaybes)
import Data.Text (toLower)
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Types

-- | Insert a raw piece of 'Text'.
-- This functions doesn't care about @LaTeX@ reserved characters,
-- it insert the text just as it is received.
raw :: Text -> LaTeX
raw = TeXRaw

-- | Calling 'between' @c l1 l2@ puts @c@ between @l1@ and @l2@ and
--   appends them.
between :: LaTeX -> LaTeX -> LaTeX -> LaTeX
between c l1 l2 = l1 <> c <> l2

-- | Create a comment.
comment :: Text -> LaTeX
comment = TeXComment

-- | This operator appends a comment after a expression.
--   For example:
--
-- > textbf "I'm just an example." % "Insert a few words here."
--
-- Since you are writing in Haskell, you may not need to output comments
-- as you can add them in the Haskell source. I added this feature
-- for completeness.
(%) :: LaTeX -> Text -> LaTeX
(%) l = (l <>) . comment

-- | Generate the title. It normally contains the 'title' name
-- of your document, the 'author'(s) and 'date'.
maketitle :: LaTeX
maketitle = TeXComm "maketitle" []

-- | Set the title of your document.
title :: LaTeX -> LaTeX
title t = TeXComm "title" [FixArg t]

-- | Set a date for your document.
date :: LaTeX -> LaTeX
date t = TeXComm "date" [FixArg t]

-- | Set the author(s) of the document.
author :: LaTeX -> LaTeX
author t = TeXComm "author" [FixArg t]

-- | Set either an institute or an organization
-- for the document.
institute :: Maybe LaTeX -> LaTeX -> LaTeX
institute  Nothing l = TeXComm "institute" [FixArg l]
institute (Just s) l = TeXComm "institute" [OptArg s,FixArg l]

thanks :: LaTeX -> LaTeX
thanks x = TeXComm "thanks" [FixArg x]

-- | Import a package. First argument is a list of options for
-- the package named in the second argument.
usepackage :: [LaTeX] -> PackageName -> LaTeX
usepackage lopt str = TeXComm "usepackage" [MOptArg lopt ,FixArg $ fromString str]

-- | The @LaTeX@ logo.
latex :: LaTeX
latex = TeXComm "LaTeX" []

-- | Start a new paragraph
par :: LaTeX
par = TeXComm "par" []

-- | Start a new line.
newline :: LaTeX
newline = TeXComm "newline" []

part :: LaTeX -> LaTeX
part p = TeXComm "part" [FixArg p]

chapter :: LaTeX -> LaTeX
chapter c = TeXComm "chapter" [FixArg c]

-- | Start a new section with a given title.
section :: LaTeX -> LaTeX
section s = TeXComm "section" [FixArg s]

subsection :: LaTeX -> LaTeX
subsection sub = TeXComm "subsection" [FixArg sub]

subsubsection :: LaTeX -> LaTeX
subsubsection sub = TeXComm "subsubsection" [FixArg sub]

paragraph :: LaTeX -> LaTeX
paragraph p = TeXComm "paragraph" [FixArg p]

subparagraph :: LaTeX -> LaTeX
subparagraph p = TeXComm "subparagraph" [FixArg p]

-- | Create the table of contents, automatically generated
-- from your 'section's, 'subsection's, and other related stuff.
tableofcontents :: LaTeX
tableofcontents = TeXComm "tableofcontents" []

appendix :: LaTeX
appendix = TeXComm "appendix" []

item :: Maybe LaTeX -> LaTeX
item Nothing    = TeXCommS "item "
item (Just opt) = TeXComm "item" [OptArg opt]

equation :: LaTeX -> LaTeX
equation = TeXEnv "equation" []

equation_ :: LaTeX -> LaTeX
equation_ = TeXEnv "equation*" []

enumerate :: LaTeX -> LaTeX
enumerate = TeXEnv "enumerate" []

itemize :: LaTeX -> LaTeX
itemize = TeXEnv "itemize" []

description :: LaTeX -> LaTeX
description = TeXEnv "description" []

flushleft :: LaTeX -> LaTeX
flushleft = TeXEnv "flushleft" []

flushright :: LaTeX -> LaTeX
flushright = TeXEnv "flushright" []

center :: LaTeX -> LaTeX
center = TeXEnv "center" []

quote :: LaTeX -> LaTeX
quote = TeXEnv "quote" []

verse :: LaTeX -> LaTeX
verse = TeXEnv "verse" []

-- | Minipage environments.
minipage :: Maybe Pos           -- ^ Optional position
         -> LaTeX               -- ^ Width
         -> LaTeX               -- ^ Minipage content
         -> LaTeX
minipage Nothing  ts = TeXEnv "minipage" [ FixArg ts ]
minipage (Just p) ts = TeXEnv "minipage" [ OptArg $ TeXRaw $ render p
                                         , FixArg ts ]

-- | Figure environment
figure :: Maybe Pos             -- ^ Optional position
       -> LaTeX                 -- ^ Figure content
       -> LaTeX
figure Nothing  = TeXEnv "figure" []
figure (Just p) = TeXEnv "figure" [ OptArg $ TeXRaw $ render p ]

abstract :: LaTeX -> LaTeX
abstract = TeXEnv "abstract" []

cite :: LaTeX -> LaTeX
cite l = TeXComm "cite" [FixArg l]

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
documentclass :: [ClassOption] -- ^ Class options
              -> ClassName     -- ^ Class name
              -> LaTeX 
documentclass opts cn = TeXComm "documentclass" [MOptArg $ fmap rendertex opts , FixArg $ fromString cn]

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

document :: LaTeX -> LaTeX
document = TeXEnv "document" []

pagenumbering :: LaTeX -> LaTeX
pagenumbering l = TeXComm "pagenumbering" [FixArg l]

-- | Arabic numerals.
arabic :: LaTeX
arabic = "arabic"

-- | Lowercase roman numerals.
roman :: LaTeX
roman = "roman"

-- | Uppercase roman numerals.
roman_ :: LaTeX
roman_ = "Roman"

-- | Lowercase letters.
alph :: LaTeX
alph = "alph"

-- | Uppercase letters.
alph_ :: LaTeX
alph_ = "Alph"

pagestyle :: LaTeX -> LaTeX
pagestyle l = TeXComm "pagestyle" [FixArg l]

thispagestyle :: LaTeX -> LaTeX
thispagestyle l  = TeXComm "thispagestyle" [FixArg l]

plain :: LaTeX
plain = "plain"

headings :: LaTeX
headings = "headings"

empty :: LaTeX
empty = "empty"

myheadings :: LaTeX
myheadings = "myheadings"

-- | Used in conjunction with 'myheadings' for setting both the left and the right heading.
markboth :: LaTeX -> LaTeX -> LaTeX
markboth l1 l2 = TeXComm "markboth" [FixArg l1,FixArg l2]

-- | Used in conjunction with 'myheadings' for setting the right heading.
markright :: LaTeX -> LaTeX
markright l = TeXComm "markright" [FixArg l]

-- | Start a new line. In a 'tabular', it starts a new row, so use 'newline' instead.
lnbk  :: LaTeX
lnbk = TeXNewLine False

lnbk_ :: LaTeX
lnbk_ = TeXNewLine True

hyp :: LaTeX
hyp = TeXCommS "-"

cleardoublepage :: LaTeX
cleardoublepage = TeXComm "cleardoublepage" []

clearpage :: LaTeX
clearpage = TeXComm "clearpage" []

newpage :: LaTeX
newpage = TeXComm "newpage" []

linebreak :: LaTeX -> LaTeX
linebreak l = TeXComm "linebreak" [OptArg l]

nolinebreak :: LaTeX -> LaTeX
nolinebreak l = TeXComm "nolinebreak" [OptArg l]

nopagebreak :: LaTeX -> LaTeX
nopagebreak l = TeXComm "nopagebreak" [OptArg l]

pagebreak :: LaTeX -> LaTeX
pagebreak l = TeXComm "pagebreak" [OptArg l]

hyphenation :: LaTeX -> LaTeX
hyphenation l = TeXComm "hyphenation" [FixArg l]

mbox :: LaTeX -> LaTeX
mbox l = TeXComm "mbox" [FixArg l]

fbox :: LaTeX -> LaTeX
fbox l = TeXComm "fbox" [FixArg l]

today :: LaTeX
today = TeXComm "today" []

tex :: LaTeX
tex = TeXComm "TeX" []

laTeX2 :: LaTeX
laTeX2 = TeXComm "LaTeX" []

laTeXe :: LaTeX
laTeXe = TeXComm "LaTeXe" []

-- | Horizontal dots.
ldots :: LaTeX
ldots = TeXComm "ldots" []

-- | Vertical dots.
vdots :: LaTeX
vdots = TeXComm "vdots" []

-- | Diagonal dots.
ddots :: LaTeX
ddots = TeXComm "ddots" []

-- | Quotation marks.
qts :: LaTeX -> LaTeX
qts l = raw "``" <> l <> raw "''"

footnote :: LaTeX -> LaTeX
footnote l = TeXComm "footnote" [FixArg l]

linespread :: Float -> LaTeX
linespread f = TeXComm "linespread" [FixArg $ TeXRaw $ render f]

indent :: LaTeX
indent = TeXComm "indent" []

noindent :: LaTeX
noindent = TeXComm "noindent" []

hspace :: Measure -> LaTeX
hspace m = TeXComm "hspace" [FixArg $ TeXRaw $ render m]

hspace_ :: Measure -> LaTeX
hspace_ m = TeXComm "hspace*" [FixArg $ TeXRaw $ render m]

stretch :: Int -> LaTeX
stretch n = TeXComm "stretch" [FixArg $ TeXRaw $ render n]

vspace :: Measure -> LaTeX
vspace m = TeXComm "vspace" [FixArg $ TeXRaw $ render m]

protect :: LaTeX -> LaTeX
protect l = TeXCommS "protect" <> l

textwidth :: LaTeX
textwidth = TeXComm "textwidth" []

linewidth :: LaTeX
linewidth = TeXComm "linewidth" []

verbatim :: LaTeX -> LaTeX
verbatim = TeXEnv "verbatim" []

underline :: LaTeX -> LaTeX
underline l = TeXComm "underline" [FixArg l]

emph :: LaTeX -> LaTeX
emph l = TeXComm "emph" [FixArg l]

textrm :: LaTeX -> LaTeX
textrm l = TeXComm "textrm" [FixArg l]

textsf :: LaTeX -> LaTeX
textsf l = TeXComm "textsf" [FixArg l]

texttt :: LaTeX -> LaTeX
texttt l = TeXComm "texttt" [FixArg l]

textmd :: LaTeX -> LaTeX
textmd l = TeXComm "textmd" [FixArg l]

textbf :: LaTeX -> LaTeX
textbf l = TeXComm "textbf" [FixArg l]

textup :: LaTeX -> LaTeX
textup l = TeXComm "textup" [FixArg l]

textit :: LaTeX -> LaTeX
textit l = TeXComm "textit" [FixArg l]

textsl :: LaTeX -> LaTeX
textsl l = TeXComm "textsl" [FixArg l]

textsc :: LaTeX -> LaTeX
textsc l = TeXComm "textsc" [FixArg l]

textnormal :: LaTeX -> LaTeX
textnormal l = TeXComm "textnormal" [FixArg l]

tiny :: LaTeX -> LaTeX
tiny l = TeXComm "tiny" [FixArg l]

scriptsize :: LaTeX -> LaTeX
scriptsize l = TeXComm "scriptsize" [FixArg l]		

footnotesize :: LaTeX -> LaTeX
footnotesize l = TeXComm "footnotesize" [FixArg l]

small :: LaTeX -> LaTeX
small l = TeXComm "small" [FixArg l]

normalsize :: LaTeX -> LaTeX
normalsize l = TeXComm "normalsize" [FixArg l]

large :: LaTeX -> LaTeX
large l = TeXComm "large" [FixArg l]

large2 :: LaTeX -> LaTeX
large2 l = TeXComm "Large" [FixArg l]

large3 :: LaTeX -> LaTeX
large3 l = TeXComm "LARGE" [FixArg l]

huge :: LaTeX -> LaTeX
huge l = TeXComm "huge" [FixArg l]

huge2 :: LaTeX -> LaTeX
huge2 l = TeXComm "Huge" [FixArg l]

smallskip :: LaTeX
smallskip = TeXComm "smallskip" []

bigskip :: LaTeX
bigskip = TeXComm "bigskip" []

-- | The 'tabular' environment can be used to typeset tables with optional horizontal and vertical lines.
tabular :: Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                       -- Defaulted to 'Center'.
        -> [TableSpec] -- ^ Table specification of columns and vertical lines.
        -> LaTeX       -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
        -> LaTeX       -- ^ Resulting table syntax.
tabular Nothing ts  = TeXEnv "tabular" [ FixArg $ TeXRaw $ renderAppend ts ]
tabular (Just p) ts = TeXEnv "tabular" [ OptArg $ TeXRaw $ render p , FixArg $ TeXRaw $ renderAppend ts ]

-- | Column separator.
(&) :: LaTeX -> LaTeX -> LaTeX
(&) = TeXOp "&"

-- | Horizontal line.
hline :: LaTeX
hline = TeXCommS "hline "

-- | @cline i j@ writes a partial horizontal line beginning in column @i@ and ending in column @j@.
cline :: Int -> Int -> LaTeX
cline i j = TeXComm "cline" [ FixArg $ TeXRaw $ render i <> "-" <> render j ]

parbox :: Maybe Pos -> Measure -> LaTeX -> LaTeX
parbox Nothing w t  = TeXComm "parbox" [ FixArg $ TeXRaw $ render w
                                       , FixArg t]
parbox (Just p) w t = TeXComm "parbox" [ OptArg $ TeXRaw $ render p
                                       , FixArg $ TeXRaw $ render w
                                       , FixArg t]

makebox :: Maybe Measure -> Maybe Pos -> LaTeX -> LaTeX
makebox Nothing Nothing t  = TeXComm "makebox" [ FixArg t]
makebox Nothing (Just p) t  = TeXComm "makebox" [ OptArg $ TeXRaw $ render p, FixArg t]
makebox (Just w) Nothing t  = TeXComm "makebox" [ OptArg $ TeXRaw $ render w, FixArg t]
makebox (Just w) (Just p) t = TeXComm "makebox" [ OptArg $ TeXRaw $ render w
                                                , OptArg $ TeXRaw $ render p
                                                , FixArg t]	

framebox :: Maybe Measure -> Maybe Pos -> LaTeX -> LaTeX
framebox Nothing Nothing t  = TeXComm "framebox" [ FixArg t]
framebox Nothing (Just p) t  = TeXComm "framebox" [ OptArg $ TeXRaw $ render p, FixArg t]
framebox (Just w) Nothing t  = TeXComm "framebox" [ OptArg $ TeXRaw $ render w, FixArg t]
framebox (Just w) (Just p) t = TeXComm "framebox" [ OptArg $ TeXRaw $ render w
                                                  , OptArg $ TeXRaw $ render p
                                                  , FixArg t]

raisebox ::  Measure -> Maybe Measure -> Maybe Measure -> LaTeX -> LaTeX
raisebox l ma mb t = TeXComm "raisebox" $
    [ FixArg $ TeXRaw $ render l ]
 ++   fmap (OptArg . TeXRaw . render) (catMaybes [ma,mb])
 ++ [ FixArg t ]

-- | Produce a simple black box.
rule :: Maybe Measure -- ^ Optional lifting.
     -> Measure       -- ^ Width.
     -> Measure       -- ^ Height.
     -> LaTeX
rule Nothing w h  = TeXComm "rule" [ FixArg $ TeXRaw $ render w
                                   , FixArg $ TeXRaw $ render h ]
rule (Just l) w h = TeXComm "rule" [ OptArg $ TeXRaw $ render l
                                   , FixArg $ TeXRaw $ render w
                                   , FixArg $ TeXRaw $ render h ]

-- HaTeX specific symbols

-- | Print the HaTeX logo.
hatex :: LaTeX
hatex = "H"
     <> hspace (Ex $ negate 0.3)
     <> textsc "a"
     <> hspace (Ex $ negate 0.3)
     <> tex

-- | Print the HaTeX 3 logo.
hatex3 :: LaTeX
hatex3 = hatex <> emph (textbf "3")

-- | Print the HaTeX-meta logo.
hatex_meta :: LaTeX
hatex_meta = hatex <> emph (textsc "-meta")

-- | Print the HaTeX logo, beside the complete version number.
hatex_version :: LaTeX
hatex_version = hatex3
             <> hspace (Ex $ negate 0.3)
             <> emph ".2.0.1"

caption :: LaTeX -> LaTeX
caption l = TeXComm "caption" [FixArg l]

label :: Label -> LaTeX
label l = TeXComm "label" [FixArg $ TeXRaw $ render l]

ref :: Label -> LaTeX
ref l = TeXComm "ref" [FixArg $ TeXRaw $ render l]

pageref :: Label -> LaTeX
pageref l = TeXComm "pageref" [FixArg $ TeXRaw $ render l]
