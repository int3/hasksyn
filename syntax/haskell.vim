" Vim syntax file
" Language: Haskell
" Author: Tristan Ravitch
" Maintainer: Tristan Ravitch
" Version: 0.0.1

if version < 600
  syntax clear
elseif exists('b:current_syntax')
  finish
endif

syn sync minlines=50 maxlines=200

" Right at the start, so they get overwritten ('specialized') easily
syn match  hsDelimiter        "\((\|)\|{\|}\|\[\|\]\|,\)"

" These basic lexical definitions are taken from the orignal haskell syntax
" description from vim 7.3.
syn match  hsSpecialChar      contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match  hsSpecialChar      contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match  hsSpecialCharError contained "\\&\|'''\+"
syn region hsString           start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=hsSpecialChar
syn match  hsCharacter        "[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=hsSpecialChar,hsSpecialCharError
syn match  hsCharacter        "^'\([^\\]\|\\[^']\+\|\\'\)'" contains=hsSpecialChar,hsSpecialCharError
syn match  hsNumber           "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match  hsFloat            "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"

" This case matches the names of types and constructors: names beginning with
" a capital letter.  Note that this also handles the case of @M.lookup@ where
" M is a qualified import.  There is a big negative lookbehind assertion here
" so that we don't highlight import and module statements oddly.
syn match hsDataCons "\(^import\s.*\|^module\s.*\)\@<!\(^\|[^a-zA-Z0-9]\)\@<=[A-Z][a-zA-Z0-9_]*"
" Also make unit and the empty list easy to spot - they are constructors too.
syn match hsDataCons "()"
syn match hsDataCons "\[\]"

" These are keywords that are only highlighted if they are in comments.
syn keyword hsFIXME contained FIXME TODO XXX BUG NOTE

" Comment stuff
" The most general type of comment first.
syn region hsBlockComment start='{-' end='-}' fold contains=hsFIXME,hsBlockComment,@Spell
" Specialize.
syn region hsPragma start='{-#' end='#-}'
" FIXME: haddock block comments should be able to contain hsBlockComments, but
" it doesn't seem to work at the moment.
syn region hsHaddockComment start='{-|' end='-}' contains=hsFIXME,@Spell
syn match hsLineComment "--.*$" contains=hsFIXME,@Spell
" Line-based haddock comments are trickier - they continue until
" the next line that isn't part of the same block of comments.
syn region hsHaddockComment start='-- |' end='^\(\s*--\)\@!' contains=hsFIXME,@Spell
syn region hsHaddockComment start='-- \$\w\+' end='^\(\s*--\)\@!' contains=hsFIXME,@Spell
syn region hsHaddockComment start='-- ^' end='^\(\s*--\)\@!' contains=hsFIXME,@Spell
" Haddock sections for import lists
syn match hsHaddockSection '-- \*.*$'
" Named documentation chunks (also for import lists)
syn match hsHaddockSection '-- \$.*$'
" Treat a shebang line at the start of the file as a comment
syn match hsLineComment "\%^\#\!.*$"


" Keywords appearing in expressions, plus a few top-level keywords
syn keyword hsKeyword do mdo let in where
syn keyword hsKeyword infix infixl infixr
syn keyword hsKeyword foreign
syn match hsKeyword '\(^\(data\|type\)\s\+\)\@<=family\(\W\)\@='

" Vim has a special syntax category for conditionals, so here are all of the
" haskell conditionals.  These are just keywords with a slightly more flexible
" coloring.
syn keyword hsConditional case of if then else

" We define a region for module NNNN (...) where so that haddock section
" headers (-- *) can be highlighted specially only within this context.
syn region hsModuleHeader start="^module\s" end="where" contains=hsHaddockSection keepend fold transparent
" Treat Module imports as the #include category; it maps reasonably well
syn keyword hsImport import module
" Treat 'qualified', 'as', and 'hiding' as keywords when following 'import'
syn match hsImport '\(\<import\>.*\)\@<=\<\(qualified\|as\|hiding\)\>'

" For `data [hsTypeSyntax] = ...` and `foo :: [hsTypeSyntax]`
syn cluster hsTypeSyntax contains=hsTypeVar,hsReservedOp,hsTypeCons
syn match hsTypeVar "\<[a-z]\([a-zA-Z0-9]\|'\)*" contained " a, a1, a', aA
syn match hsTypeCons "\(^\|[^a-zA-Z0-9]\)\@<=[A-Z][a-zA-Z0-9_]*" contained

syn keyword hsTypeDecls default
syn region hsTypeDecl start="\%(class\|instance\|data\|newtype\|type\)" end="\%(where\|=\|::\)\_s" contains=hsTypeDecls,hsKeyword,@hsTypeSyntax keepend nextgroup=hsDataConDecl
" Declare the remaining hsTypeDecls as contained because keywords have higher
" matching priority than regions
syn keyword hsTypeDecls class instance data newtype type contained
syn match hsDataConDecl "\<[A-Z][a-zA-Z0-9]*" contained nextgroup=hsDataConDeclArgs
" newlines get consumed, pipes don't, so hsPipeDataConDecl can follow
" the space in 'start' seems necessary, no idea why
syn region hsDataConDeclArgs start=" " end="\%(\zederiving\|\n\|\ze|\)" contained contains=@hsTypeSyntax,hsDelimiter keepend nextgroup=hsPipeDataConDecl
syn match hsPipeDataConDecl "\s*|\s*" contained nextgroup=hsDataConDecl

" deriving Eq, deriving (Eq, Ord)
syn match hsTypeDecls "deriving\s\+" nextgroup=hsTypeCons,_hsListTypeCons
syn region _hsListTypeCons start="(" end=")" matchgroup=hsDelimiter contains=hsTypeCons contained

" FIXME: Maybe we can do something fancy for data/type families?  'family' is
" only a keyword if it follows data/type...

" We want to let '-' participate in operators, but we can't let it match
" '--', '---', etc. because it interferes with comments. The same goes for
" '#!' at the start of a file. Also, the dot (.) is an operator character,
" but not when it comes immediately after a module name.
syn match hsOperator "\(\%^\#\!\)\@!\(\(\<[A-Z]\w*\)\@64<=\.\)\@!\(--\+\([^.%\~\&\*/\$\^|@:+<!>=#!\?]\|$\)\)\@![-.%\~\&\*/\$\^|@:+<!>=#!\?]\+"
" Include support for infix functions as operators
syn match hsOperator "`[a-zA-Z0-9\.]\+`"
" Operators that are language-defined -- '=', '::', '=>' etc
syn match hsReservedOp "\%(=\(\s\|\w\|$\)\|::\|=>\|->\|<-\|\s|\s\)"

" Highlight function/value names in type signatures.  Looks ahead to find a ::
" after a name.  This allows whitespace before the name so that it can match
" in a 'where,' but it won't match local type annotations on random little
" things.
syn match hsFunctionList "^\s*\(\<\(where\>\|let\>\)\@![a-z][a-zA-Z0-9']*[[:space:]\n,]\+\)*[a-z][a-zA-Z0-9']*[[:space:]\n]*\ze::" contains=hsFunction,hsReservedOp
syn match hsFunction "\s*[a-z][a-zA-Z0-9']*\([[:space:]\n]*\(::\|,\)\)\@=" contained
" Also support the style where the first where binding is on the same line as
" the where keyword.
syn match hsFunction "\(\<\(where\|let\)\s\+\([a-z][a-zA-Z0-9']*\s*,\s*\)*\)\@<=[a-z][a-zA-Z0-9']*\(\s*\(,\s*[a-z][a-zA-Z0-9']*\s*\)*::\)\@="

syn region _hsParenTypeSyntax start="(" end=")" matchgroup=hsDelimiter contains=_hsParenTypeSyntax,@hsTypeSyntax contained
syn keyword hsTypeQuant forall contained
syn region hsTypeAnno start="::" end="\ze\%(,\|}\|)\|$\)" contains=_hsParenTypeSyntax,@hsTypeSyntax,hsTypeQuant

" FIXME Ignoring proc for now, also mdo and rec

" Give undefined a bit of special treatment
syn keyword hsScary undefined

" C Preprocessor stuff
syn match hsCPP '\(^\s*\)\@<=#\(warning\|pragma\|error\)\W\@='
syn match hsCPPCond '\(^\s*\)\@<=#\(if\|ifdef\|elif\)\W\@='
syn match hsCPPCond '\(^\s*\)\@<=#\(endif\|else\)\(\s*$\|\W\)\@='
syn match hsCPPInclude '\(^\s*\)\@<=#include\W\@='
syn match hsCPPDefine '\(^\s*\)\@<=#define\W\@='
syn match hsCPPDefined '\(^\s*.*\W\)\@<=defined(\w\+)'

" Copied from vim2hs. Cute conceals.
syntax match hsStructure
    \ "\\\ze[[:alpha:][:space:]_([]"
    \ display conceal cchar=λ

syntax match hsOperator
    \ "\s\.\_s"ms=s+1,me=e-1
    \ display conceal cchar=∘

setlocal conceallevel=2

if version >= 508 || !exists('did_hs_syntax_inits')
  if version < 508
    let did_hs_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  " CPP
  HiLink hsCPP PreProc
  HiLink hsCPPCond PreCondit
  HiLink hsCPPInclude Include
  HiLink hsCPPDefine Define
  HiLink hsCPPDefined PreProc

  " Comments
  HiLink hsLineComment Comment
  HiLink hsBlockComment Comment
  HiLink hsPragma SpecialComment
  HiLink hsHaddockComment SpecialComment
  HiLink hsHaddockSection SpecialComment

  HiLink hsKeyword Keyword
  HiLink hsTypeQuant Keyword
  HiLink hsConditional Conditional
  HiLink hsImport Include
  HiLink hsTypeDecls Keyword
  HiLink hsTypeVar Identifier
  HiLink hsTypeCons StorageClass
  HiLink hsDataConDecl hsDataCons
  HiLink hsPipeDataConDecl hsReservedOp

  HiLink hsFIXME Todo

  HiLink hsOperator Operator
  HiLink hsReservedOp Structure

  HiLink hsModuleQualifier StorageClass

  HiLink hsFunction Function
  HiLink hsDataCons Type

  " Literals
  HiLink hsSpecialChar SpecialChar
  HiLink hsFloat Float
  HiLink hsNumber Number
  HiLink hsCharacter Character
  HiLink hsString String

  HiLink hsDelimiter Delimiter

  HiLink hsScary Todo

  HiLink Conceal Operator

  delcommand HiLink
endif

let b:current_syntax = "haskell"
