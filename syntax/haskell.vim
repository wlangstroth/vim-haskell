" Vim syntax file
"
" Modification of vims Haskell syntax file:
"   - match types using regular expression
"   - highlight toplevel functions
"   - use "syntax keyword" instead of "syntax match" where appropriate
"   - functions and types in import and module declarations are matched
"   - removed hs_highlight_more_types (just not needed anymore)
"   - enable spell checking in comments and strings only
"   - FFI highlighting
"   - QuasiQuotation
"   - top level Template Haskell slices
"   - PackageImport
"
" TODO: find out which vim versions are still supported
"
" From Original file:
" ===================
"
" Language:         Haskell
" Maintainer:       Haskell Cafe mailinglist <haskell-cafe@haskell.org>
" Last Change:      2010 Feb 21
" Original Author:  John Williams <jrw@pobox.com>
"
" Thanks to Ryan Crumley for suggestions and John Meacham for
" pointing out bugs. Also thanks to Ian Lynagh and Donald Bruce Stewart
" for providing the inspiration for the inclusion of the handling
" of C preprocessor directives, and for pointing out a bug in the
" end-of-line comment handling.
"
" Options-assign a value to these variables to turn the option on:
"
" hs_highlight_delimiters - Highlight delimiter characters--users
"               with a light-colored background will
"               probably want to turn this on.
" hs_highlight_boolean - Treat True and False as keywords.
" hs_highlight_types - Treat names of primitive types as keywords.
" hs_highlight_debug - Highlight names of debugging functions.
" hs_allow_hash_operator - Don't highlight seemingly incorrect C
"              preprocessor directives but assume them to be
"              operators
"
"

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

"syntax sync fromstart "mmhhhh.... is this really ok to do so?
syn sync linebreaks=15 minlines=50 maxlines=500

syn match  hsSpecialChar    contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match  hsSpecialChar    contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match  hsSpecialCharError   contained "\\&\|'''\+"
syn region hsString     start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=hsSpecialChar,@Spell
syn match  hsCharacter      "[^a-zA-Z0-9_']'\([^\\]\|\\[^']\+\|\\'\)'"lc=1 contains=hsSpecialChar,hsSpecialCharError
syn match  hsCharacter      "^'\([^\\]\|\\[^']\+\|\\'\)'" contains=hsSpecialChar,hsSpecialCharError

" (Qualified) identifiers (no default highlighting)
syn match ConId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[A-Z][a-zA-Z0-9_']*\>"
syn match VarId "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=\<[a-z][a-zA-Z0-9_']*\>"

" Infix operators--most punctuation characters and any (qualified) identifier
" enclosed in `backquotes`. An operator starting with : is a constructor,
" others are variables (e.g. functions).
syn match hsVarSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[-!#$%&\*\+/<=>\?@\\^|~.][-!#$%&\*\+/<=>\?@\\^|~:.]*"
syn match hsConSym "\(\<[A-Z][a-zA-Z0-9_']*\.\)\=:[-!#$%&\*\+./<=>\?@\\^|~:]*"
syn match hsVarSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[a-z][a-zA-Z0-9_']*`"
syn match hsConSym "`\(\<[A-Z][a-zA-Z0-9_']*\.\)\=[A-Z][a-zA-Z0-9_']*`"

" Toplevel Template Haskell support
"syn match hsTHTopLevel "^[a-z]\(\(.\&[^=]\)\|\(\n[^a-zA-Z0-9]\)\)*"
syn match hsTHIDTopLevel "^[a-z]\S*"
syn match hsTHTopLevel "^\$(\?" nextgroup=hsTHTopLevelName
syn match hsTHTopLevelName "[a-z]\S*" contained

" Reserved symbols--cannot be overloaded.
syn match hsDelimiter  "(\|)\|\[\|\]\|,\|;\|_\|{\|}"

syn region hsInnerParen start="(" end=")" contained contains=hsInnerParen,hsConSym,hsType,hsVarSym
syn region hsInfixOpFunctionName start="^(" end=")\s*[^:`]\(\W\&\S\&[^'\"`()[\]{}@]\)\+"re=s
      \ contained keepend contains=hsInnerParen,hsHlInfixOp

syn match hsHlFunctionName "[a-z_]\(\S\&[^,\(\)\[\]]\)*" contained
syn match hsFunctionName "^[a-z_]\(\S\&[^,\(\)\[\]]\)*" contained contains=hsHlFunctionName
syn match hsHlInfixFunctionName "`[a-z_][^`]*`" contained
syn match hsInfixFunctionName "^\S[^=]*`[a-z_][^`]*`"me=e-1 contained contains=hsHlInfixFunctionName,hsType,hsConSym,hsVarSym,hsString,hsCharacter
syn match hsHlInfixOp "\(\W\&\S\&[^`(){}'[\]]\)\+" contained contains=hsString
syn match hsInfixOpFunctionName "^\(\(\w\|[[\]{}]\)\+\|\(\".*\"\)\|\('.*'\)\)\s*[^:]=*\(\W\&\S\&[^='\"`()[\]{}@]\)\+"
      \ contained contains=hsHlInfixOp,hsCharacter

syn match hsOpFunctionName        "(\(\W\&[^(),\"]\)\+)" contained
"syn region hsFunction start="^["'a-z_([{]" end="=\(\s\|\n\|\w\|[([]\)" keepend extend
syn region hsFunction start="^["'a-zA-Z_([{]\(\(.\&[^=]\)\|\(\n\s\)\)*=" end="\(\s\|\n\|\w\|[([]\)"
      \ contains=hsOpFunctionName,hsInfixOpFunctionName,hsInfixFunctionName,hsFunctionName,hsType,hsConSym,hsVarSym,hsString,hsCharacter

syn match hsTypeOp "::"
syn match hsDeclareFunction "^[a-z_(]\S*\(\s\|\n\)*::" contains=hsFunctionName,hsOpFunctionName,hsTypeOp

" hi hsTypeOp guibg=red
" hi hsInfixOpFunctionName guibg=yellow
" hi hsFunction guibg=green
" hi hsInfixFunctionName guibg=red
" hi hsDeclareFunction guibg=red

syn keyword hsStructure data family class where instance default deriving
syn keyword hsTypedef type newtype pattern

syn keyword hsInfix infix infixl infixr
syn keyword hsStatement  do case of let in
syn keyword hsConditional if then else

if exists("hs_highlight_types")
  " Primitive types from the standard prelude and libraries.
  syn match hsType "\<[A-Z]\(\S\&[^,.]\)*\>"
  syn match hsType "()"
endif

" Not real keywords, but close.
if exists("hs_highlight_boolean")
  " Boolean constants from the standard prelude.
  "syn keyword hsBoolean True False
endif

syn region  hsPackageString start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial contained
syn match   hsModuleName  excludenl "\([A-Z]\w*\.\?\)*" contained

syn match hsImport "\<import\>\s\+\(qualified\s\+\)\?\(\<\(\w\|\.\)*\>\)"
      \ contains=hsModuleName,hsImportLabel
      \ nextgroup=hsImportParams,hsImportIllegal skipwhite
syn keyword hsImportLabel import qualified contained

syn match hsImportIllegal "\w\+" contained

syn keyword hsAsLabel as contained
syn keyword hsHidingLabel hiding contained

syn match hsImportParams "as\s\+\(\w\+\)" contained
      \ contains=hsModuleName,hsAsLabel
      \ nextgroup=hsImportParams,hsImportIllegal skipwhite
syn match hsImportParams "hiding" contained
      \ contains=hsHidingLabel
      \ nextgroup=hsImportParams,hsImportIllegal skipwhite
syn region hsImportParams start="(" end=")" contained
      \ contains=hsBlockComment,hsLineComment, hsType,hsDelimTypeExport,hsHlFunctionName,hsOpFunctionName
      \ nextgroup=hsImportIllegal skipwhite

"hi hsImport guibg=red
"hi hsImportParams guibg=bg
"hi hsImportIllegal guibg=bg
"hi hsModuleName guibg=bg

" new module highlighting
syn region hsDelimTypeExport start="\<[A-Z]\(\S\&[^,.]\)*\>(" end=")" contained
      \ contains=hsType

syn keyword hsExportModuleLabel module contained
syn match hsExportModule "\<module\>\(\s\|\t\|\n\)*\([A-Z]\w*\.\?\)*" contained contains=hsExportModuleLabel,hsModuleName

syn keyword hsModuleStartLabel module contained
syn keyword hsModuleWhereLabel where contained

syn match hsModuleStart "^module\(\s\|\n\)*\(\<\(\w\|\.\)*\>\)\(\s\|\n\)*"
      \ contains=hsModuleStartLabel,hsModuleName
      \ nextgroup=hsModuleCommentA,hsModuleExports,hsModuleWhereLabel

syn region hsModuleCommentA start="{-" end="-}"
      \ contains=hsModuleCommentA,hsCommentTodo,@Spell contained
      \ nextgroup=hsModuleCommentA,hsModuleExports,hsModuleWhereLabel skipwhite skipnl

syn match hsModuleCommentA "--.*\n"
      \ contains=hsCommentTodo,@Spell contained
      \ nextgroup=hsModuleCommentA,hsModuleExports,hsModuleWhereLabel skipwhite skipnl

syn region hsModuleExports start="(" end=")" contained
      \ nextgroup=hsModuleCommentB,hsModuleWhereLabel skipwhite skipnl
      \ contains=hsBlockComment,hsLineComment,hsType,hsDelimTypeExport,hsHlFunctionName,hsOpFunctionName,hsExportModule

syn match hsModuleCommentB "--.*\n"
      \ contains=hsCommentTodo,@Spell contained
      \ nextgroup=hsModuleCommentB,hsModuleWhereLabel skipwhite skipnl

syn region hsModuleCommentB start="{-" end="-}"
      \ contains=hsModuleCommentB,hsCommentTodo,@Spell contained
      \ nextgroup=hsModuleCommentB,hsModuleWhereLabel skipwhite skipnl
" end module highlighting

" FFI support
syn keyword hsFFIForeign foreign contained
syn keyword hsFFIImportExport export contained
syn keyword hsFFICallConvention ccall stdcall contained
syn keyword hsFFISafety safe unsafe contained
syn region  hsFFIString      start=+"+  skip=+\\\\\|\\"+  end=+"+  contained contains=hsSpecialChar
syn match hsFFI excludenl "\<foreign\>\(.\&[^\"]\)*\"\(.\)*\"\(\s\|\n\)*\(.\)*::"
      \ keepend
      \ contains=hsFFIForeign,hsFFIImportExport,hsFFICallConvention,hsFFISafety,hsFFIString,hsOpFunctionName,hsHlFunctionName


syn match   hsNumber     "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
syn match   hsFloat      "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"

" Comments
syn keyword hsCommentTodo    TODO FIXME XXX TBD contained
syn match   hsLineComment      "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$" contains=hsCommentTodo,@Spell
syn region  hsBlockComment     start="{-"  end="-}" contains=hsBlockComment,hsCommentTodo,@Spell
syn region  hsPragma        start="{-#" end="#-}"

" QuasiQuotation
syn region hsQQ start="\[\$" end="|\]"me=e-2 keepend contains=hsQQVarID,hsQQContent nextgroup=hsQQEnd
syn region hsQQNew start="\[\(.\&[^|]\&\S\)*|" end="|\]"me=e-2 keepend contains=hsQQVarIDNew,hsQQContent nextgroup=hsQQEnd
syn match hsQQContent ".*" contained
syn match hsQQEnd "|\]" contained
syn match hsQQVarID "\[\$\(.\&[^|]\)*|" contained
syn match hsQQVarIDNew "\[\(.\&[^|]\)*|" contained

if exists("hs_highlight_debug")
  " Debugging functions from the standard prelude.
  syn keyword hsDebug undefined error trace
endif


" C Preprocessor directives. Shamelessly ripped from c.vim and trimmed
" First, see whether to flag directive-like lines or not
if (!exists("hs_allow_hash_operator"))
  syn match   cError      display "^\s*\(%:\|#\).*$"
endif
" Accept %: for # (C99)
syn region  cPreCondit  start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=cComment,cCppString,cCommentError
syn match   cPreCondit  display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
syn region  cCppOut     start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=cCppOut2
syn region  cCppOut2    contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=cCppSkip
syn region  cCppSkip    contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=cCppSkip
syn region  cIncluded   display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match   cIncluded   display contained "<[^>]*>"
syn match   cInclude    display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=cIncluded
syn cluster cPreProcGroup   contains=cPreCondit,cIncluded,cInclude,cDefine,cCppOut,cCppOut2,cCppSkip,cCommentStartError
syn region  cDefine     matchgroup=cPreCondit start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$"
syn region  cPreProc    matchgroup=cPreCondit start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend

syn region  cComment    matchgroup=cCommentStart start="/\*" end="\*/" contains=cCommentStartError,cSpaceError contained
syn match   cCommentError   display "\*/" contained
syn match   cCommentStartError display "/\*"me=e-1 contained
syn region  cCppString  start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial contained


" Types from the standard prelude.
syn match hsType        "\<\(Bool\|Maybe\|Either\|Ordering\)\>"
syn match hsType        "\<\(Char\|String\|Int\|Integer\|Float\|Double\|Rational\|IO\)\>"
syn match hsType        "\<\(ReadS\|ShowS\)\>"
syn match hsType        "\<\(FilePath\|IOError\)\>"

" Classes from the standard prelude
syn match hsType        "\<\(Eq\|Ord\|Enum\|Bounded\|Num\|Real\|Integral\|Fractional\|Floating\|RealFrac\|RealFloat\|Monad\|Functor\)\>"
syn match hsType        "\<\(Show\|Read\)\>"


" Constants from the standard prelude.
syn match hsBoolean     "\<\(True\|False\)\>"
syn match hsMaybe       "\<\(Nothing\|Just\)\>"
syn match hsConstant    "\<\(Left\|Right\)\>"
syn match hsOrdering    "\<\(LT\|EQ\|GT\)\>"


" Functions from the standard prelude.
syn match hsFunction    "\<\(compare\|max\|min\)\>"
syn match hsFunction    "\<\(succ\|pred\|toEnum\|fromEnum\|enumFrom\|enumFromThen\|enumFromTo\|enumFromThenTo\)\>"
syn match hsFunction    "\<\(minBound\|maxBound\)\>"
syn match hsFunction    "\<\(negate\|abs\|signum\|fromInteger\)\>"
syn match hsFunction    "\<toRational\>"
syn match hsFunction    "\<\(quot\|rem\|div\|mod\|quotRem\|divMod\|toInteger\)\>"
syn match hsFunction    "\<\(recip\|fromRational\)\>"
syn match hsFunction    "\<\(pi\|exp\|log\|sqrt\|logBase\|sin\|cos\|tan\|asin\|acos\|atan\|sinh\|cosh\|tanh\|asinh\|acosh\|atanh\)\>"
syn match hsFunction    "\<\(properFraction\|truncate\|round\|ceiling\|floor\)\>"
syn match hsFunction    "\<\(floatRadix\|floatDigits\|floatRange\|decodeFloat\|encodeFloat\|exponent\|significand\|scaleFloat\|isNaN\|isInfinite\|isDenormalized\|isIEEE\|isNegativeZero\|atan2\)\>"
syn match hsFunction    "\<\(return\|fail\)\>"
syn match hsFunction    "\<\(fmap\)\>"
syn match hsFunction    "\<\(mapM\|mapM_\|sequence\|sequence_\)\>"
syn match hsFunction    "\<\(maybe\|either\)\>"
syn match hsFunction    "\<\(not\|otherwise\)\>"
syn match hsFunction    "\<\(subtract\|even\|odd\|gcd\|lcm\)\>"
syn match hsFunction    "\<\(fromIntegral\|realToFrac\)\>"
syn match hsFunction    "\<\(fst\|snd\|curry\|uncurry\|id\|const\|flip\|until\)\>"
syn match hsFunction    "\<\(asTypeOf\|error\|undefined\)\>"
syn match hsFunction    "\<\(seq\)\>"
syn match hsFunction    "\<\(map\|filter\|concat\|concatMap\)\>"
syn match hsFunction    "\<\(head\|last\|tail\|init\|null\|length\)\>"
syn match hsFunction    "\<\(foldl\|foldl1\|scanl\|scanl1\|foldr\|foldr1\|scanr\|scanr1\)\>"
syn match hsFunction    "\<\(iterate\|repeat\|replicate\|cycle\)\>"
syn match hsFunction    "\<\(take\|drop\|splitAt\|takeWhile\|dropWhile\|span\|break\)\>"
syn match hsFunction    "\<\(lines\|words\|unlines\|unwords\|reverse\|and\|or\)\>"
syn match hsFunction    "\<\(any\|all\|elem\|notElem\|lookup\)\>"
syn match hsFunction    "\<\(sum\|product\|maximum\|minimum\)\>"
syn match hsFunction    "\<\(zip\|zip3\|zipWith\|zipWith3\|unzip\|unzip3\)\>"
syn match hsFunction    "\<\(readsPrec\|readList\)\>"
syn match hsFunction    "\<\(showsPrec\|show\|showList\)\>"
syn match hsFunction    "\<\(reads\|shows\|read\|lex\)\>"
syn match hsFunction    "\<\(showChar\|showString\|readParen\|showParen\)\>"
syn match hsFunction    "\<\(ioError\|userError\|catch\)\>"
syn match hsFunction    "\<\(putChar\|putStr\|putStrLn\|print\)\>"
syn match hsFunction    "\<\(getChar\|getLine\|getContents\|interact\)\>"
syn match hsFunction    "\<\(readFile\|writeFile\|appendFile\|readIO\|readLn\)\>"


if version >= 508 || !exists("did_hs_syntax_inits")
  if version < 508
    let did_hsSyntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink hsHlFunctionName              Function
  HiLink hsHlInfixFunctionName         Function
  HiLink hsHlInfixOp                   Function
  HiLink hsOpFunctionName              Function
  HiLink hsTypedef                     Typedef
  HiLink hsVarSym                      hsOperator
  HiLink hsConSym                      hsOperator
  "if exists("hs_highlight_delimiters")
    " Some people find this highlighting distracting.
    HiLink hsDelimiter                 Delimiter
  "endif

  HiLink hsModuleStartLabel            Structure
  HiLink hsExportModuleLabel           Keyword
  HiLink hsModuleWhereLabel            Structure
  HiLink hsModuleName                  Normal

  HiLink hsImportIllegal               Error
  HiLink hsAsLabel                     hsImportLabel
  HiLink hsHidingLabel                 hsImportLabel
  HiLink hsImportLabel                 Include
  HiLink hsImportMod                   Include
  HiLink hsPackageString               hsString

  HiLink hsOperator                    Operator

  HiLink hsInfix                       Keyword
  HiLink hsStructure                   Structure
  HiLink hsStatement                   Statement
  HiLink hsConditional                 Conditional

  HiLink hsSpecialCharError            Error
  HiLink hsSpecialChar                 SpecialChar
  HiLink hsString                      String
  HiLink hsFFIString                   String
  HiLink hsCharacter                   Character
  HiLink hsNumber                      Number
  HiLink hsFloat                       Float

  HiLink hsLiterateComment             hsComment
  HiLink hsBlockComment                hsComment
  HiLink hsLineComment                 hsComment
  HiLink hsModuleCommentA              hsComment
  HiLink hsModuleCommentB              hsComment
  HiLink hsComment                     Comment
  HiLink hsCommentTodo                 Todo
  HiLink hsPragma                      SpecialComment
  HiLink hsBoolean                     Boolean

  if exists("hs_highlight_types")
    HiLink hsDelimTypeExport           hsType
    HiLink hsType                      Type
  endif

  HiLink hsDebug                       Debug

  HiLink hsTypeOp                      hsOperator

  HiLink cCppString                    hsString
  HiLink cCommentStart                 hsComment
  HiLink cCommentError                 hsError
  HiLink cCommentStartError            hsError
  HiLink cInclude                      Include
  HiLink cPreProc                      PreProc
  HiLink cDefine                       Macro
  HiLink cIncluded                     hsString
  HiLink cError                        Error
  HiLink cPreCondit                    PreCondit
  HiLink cComment                      Comment
  HiLink cCppSkip                      cCppOut
  HiLink cCppOut2                      cCppOut
  HiLink cCppOut                       Comment

  HiLink hsFFIForeign                  Keyword
  HiLink hsFFIImportExport             Structure
  HiLink hsFFICallConvention           Keyword
  HiLink hsFFISafety                   Keyword

  HiLink hsFunction                    Function
  HiLink hsMaybe                       hsEnumConst
  HiLink hsOrdering                    hsEnumConst
  HiLink hsEnumConst                   Constant
  HiLink hsConstant                    Constant

  HiLink hsTHIDTopLevel                Macro
  HiLink hsTHTopLevelName              Macro

  HiLink hsQQVarID                     Keyword
  HiLink hsQQVarIDNew                  Keyword
  HiLink hsQQEnd                       Keyword
  HiLink hsQQContent                   String

  delcommand HiLink
endif

let b:current_syntax = "haskell"
