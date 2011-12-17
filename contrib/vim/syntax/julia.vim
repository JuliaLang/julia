" Vim syntax file
" Language:	julia
" Maintainer:	Carlo Baldassi <carlobaldassi@gmail.com>
" Last Change:	2011 dec 11

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syntax cluster juliaExpressions		contains=@juliaParItems,@juliaStringItems,@juliaKeywordItems,@juliaBlocksItems,@juliaTypesItems,@juliaConstItems,@juliaMacroItems,@juliaOperatorItems,@juliaNumberItems,@juliaCommentItems,@juliaErrorItems

syntax cluster juliaParItems		contains=juliaParBlock,juliaSqBraBlock,juliaCurBraBlock
syntax cluster juliaKeywordItems	contains=juliaKeyword,juliaTypedef
syntax cluster juliaBlocksItems		contains=juliaConditionalBlock,juliaRepeatBlock,juliaBeginBlock,juliaFunctionBlock,juliaMacroBlock,juliaQuoteBlock,juliaTypeBlock,juliaExceptionBlock,juliaLetBlock
syntax cluster juliaTypesItems		contains=juliaBuiltinType
syntax cluster juliaConstItems		contains=juliaConstNum,juliaConstBool,juliaConstIO,juliaConstLimits,juliaConstErrno,juliaConstGeneric
syntax cluster juliaMacroItems		contains=juliaMacro
syntax cluster juliaNumberItems		contains=juliaNumbers
syntax cluster juliaStringItems		contains=juliaChar,juliaString,juliaEString,juliaIString,juliaLString,juliabString,juliaShellString,juliaRegEx
syntax cluster juliaOperatorItems	contains=juliaArithOperator,juliaBitOperator,juliaBoolOperator,juliaCompOperator,juliaAssignOperator,juliaRangeOperator,juliaTypeOperator,juliaFuncOperator,juliaCTransOperator,juliaVarargOperator,juliaTernaryRegion
syntax cluster juliaCommentItems	contains=juliaCommentL
syntax cluster juliaErrorItems		contains=juliaErrorPar,juliaErrorEnd,juliaErrorElse

syntax match   juliaErrorPar		display "[])}]"
syntax match   juliaErrorEnd		display "\<end\>"
syntax match   juliaErrorElse		display "\<\(else\|elseif\)\>"
syntax match   juliaErrorCatch		display "\<catch\>"
syntax match   juliaErrorSemicol	display contained ";"

syntax match   juliaRangeEnd		display contained "\<end\>"

syntax region  juliaParBlock		matchgroup=juliaParDelim start="(" end=")" contains=@juliaExpressions
syntax region  juliaParBlockInRange	matchgroup=juliaParDelim contained start="(" end=")" contains=@juliaExpressions,juliaParBlockInRange,juliaRangeEnd
syntax region  juliaSqBraBlock		matchgroup=juliaParDelim start="\[" end="\]" contains=@juliaExpressions,juliaParBlockInRange,juliaRangeEnd
syntax region  juliaCurBraBlock		matchgroup=juliaParDelim start="{" end="}" contains=@juliaExpressions

syntax match   juliaKeyword		"\<\(return\|local\|break\|continue\|global\|module\|import\|export\|const\)\>"
syntax region  juliaConditionalBlock	matchgroup=juliaConditional start="\<if\>" end="\<end\>" contains=@juliaExpressions,juliaConditionalEIBlock,juliaConditionalEBlock fold
syntax region  juliaConditionalEIBlock	matchgroup=juliaConditional transparent contained start="\<elseif\>" end="\<\(end\|else\|elseif\)\>"me=s-1 contains=@juliaExpressions,juliaConditionalEIBlock,juliaConditionalEBlock
syntax region  juliaConditionalEBlock	matchgroup=juliaConditional transparent contained start="\<else\>" end="\<end\>"me=s-1 contains=@juliaExpressions
syntax region  juliaRepeatBlock		matchgroup=juliaRepeat start="\<\(while\|for\)\>" end="\<end\>" contains=@juliaExpressions fold
syntax region  juliaBeginBlock		matchgroup=juliaKeyword start="\<begin\>" end="\<end\>" contains=@juliaExpressions fold
syntax region  juliaFunctionBlock	matchgroup=juliaKeyword start="\<function\>" end="\<end\>" contains=@juliaExpressions fold
syntax region  juliaMacroBlock		matchgroup=juliaKeyword start="\<macro\>" end="\<end\>" contains=@juliaExpressions fold
syntax region  juliaQuoteBlock		matchgroup=juliaKeyword start="\<quote\>" end="\<end\>" contains=@juliaExpressions fold
syntax region  juliaTypeBlock		matchgroup=juliaKeyword start="\<type\>" end="\<end\>" contains=@juliaExpressions fold
syntax region  juliaExceptionBlock	matchgroup=juliaException start="\<try\>" end="\<end\>" contains=@juliaExpressions,juliaCatchBlock fold
syntax region  juliaCatchBlock		matchgroup=juliaException transparent contained start="\<catch\>" end="\<end\>"me=s-1 contains=@juliaExpressions
syntax region  juliaLetBlock		matchgroup=juliaKeyword start="\<let\>" end="\<end\>" contains=@juliaExpressions fold
syntax match   juliaTypedef		"\<\(abstract\|typealias\|bitstype\)\>"

syntax match   juliaBuiltinType		display "\<\(Uint\(\|8\|16\|32\|64\)\|Int\(\|8\|16\|32\|64\)\|Float\(\|32\|64\)\|Complex\(\|64\|128\)\|ComplexNum\|Bool\|Char\|Number\|Scalar\|Real\|Integer\|Array\|DArray\|AbstractArray\|AbstractVector\|AbstractMatrix\|StridedVector\|StridedMatrix\|VecOrMat\|StridedVecOrMat\|Range\|Range1\|SparseMatrixCSC\|Tuple\|NTuple\|Buffer\|Size\|Index\|Symbol\|Function\|Vector\|Matrix\|Union\|Type\|Any\|Complex\|None\|String\|Ptr\|Void\|Exception\|PtrInt\|Type\)\>"
syntax match   juliaConstNum		display "\<\(pi\|NaN\(32\)\?\|Inf\(32\)\?\)\>"
syntax match   juliaConstBool		display "\<\(true\|false\)\>"
syntax match   juliaConstIO		display "\<\(std\(out\|in\|err\)_stream\|STD\(OUT\|IN\|ERR\)\|sizeof_\(ios_t\|fd_set\)\)\>"
syntax match   juliaConstPtr		display "\<\(WORD_SIZE\|C_NULL\)\>"
syntax match   juliaConstLimits		display "\<\(MAX_\(TYPEUNION_\(LEN\|DEPTH\)\|TUPLE\(_DEPTH\|TYPE_LEN\)\)\)\>"
syntax match   juliaConstErrno		display "\<E\(2BIG\|ACCES\|ADDRINUSE\|ADDRNOTAVAIL\|ADV\|AFNOSUPPORT\|AGAIN\|ALREADY\|BADE\|BADFD\|BADF\|BADMSG\|BADR\|BADRQC\|BADSLT\|BFONT\|BUSY\|CANCELED\|CHILD\|CHRNG\|COMM\|CONNABORTED\|CONNREFUSED\|CONNRESET\|DEADLK\|DESTADDRREQ\|DOM\|DOTDOT\|DQUOT\|EXIST\|FAULT\|FBIG\|HOSTDOWN\|HOSTUNREACH\|HWPOISON\|IDRM\|ILSEQ\|INPROGRESS\|INTR\|INVAL\|IO\|ISCONN\|ISDIR\|ISNAM\|KEYEXPIRED\|KEYREJECTED\|KEYREVOKED\|L2HLT\|L2NSYNC\|L3HLT\|L3RST\|LIBACC\|LIBBAD\|LIBEXEC\|LIBMAX\|LIBSCN\|LNRNG\|LOOP\|MEDIUMTYPE\|MFILE\|MLINK\|MSGSIZE\|MULTIHOP\|NAMETOOLONG\|NAVAIL\|NETDOWN\|NETRESET\|NETUNREACH\|NFILE\|NOANO\|NOBUFS\|NOCSI\|NODATA\|NODEV\|NOENT\|NOEXEC\|NOKEY\|NOLCK\|NOLINK\|NOMEDIUM\|NOMEM\|NOMSG\|NONET\|NOPKG\|NOPROTOOPT\|NOSPC\|NOSR\|NOSTR\|NOSYS\|NOTBLK\|NOTCONN\|NOTDIR\|NOTEMPTY\|NOTNAM\|NOTRECOVERABLE\|NOTSOCK\|NOTTY\|NOTUNIQ\|NXIO\|OPNOTSUPP\|OVERFLOW\|OWNERDEAD\|PERM\|PFNOSUPPORT\|PIPE\|PROTO\|PROTONOSUPPORT\|PROTOTYPE\|RANGE\|REMCHG\|REMOTE\|REMOTEIO\|RESTART\|RFKILL\|ROFS\|SHUTDOWN\|SOCKTNOSUPPORT\|SPIPE\|SRCH\|SRMNT\|STALE\|STRPIPE\|TIMEDOUT\|TIME\|TOOMANYREFS\|TXTBSY\|UCLEAN\|UNATCH\|USERS\|XDEV\|XFULL\)\>"
syntax match   juliaConstGeneric	display "\<\(nothing\)\>"

syntax match   juliaMacro		display "@[_[:alpha:]][_[:alnum:]]*"

syntax match   juliaNumbers		display transparent "\<\d\|\.\d\|\<im\>" contains=juliaNumber,juliaFloat,juliaComplexUnit

syntax match   juliaNumber		display contained "\d\+\(\>\|im\>\|\ze[_[:alpha:]]\)" contains=juliaComplexUnit
"hex number
syntax match   juliaNumber		display contained "0x\x\+\(\>\|im\>\|\ze[_[:alpha:]]\)" contains=juliaComplexUnit
"floating point number, with dot, optional exponent
syntax match   juliaFloat		display contained "\d\+\.\d*\([eE][-+]\?\d\+\)\?\(\>\|im\>\|\ze[_[:alpha:]]\)" contains=juliaComplexUnit
"floating point number, starting with a dot, optional exponent
syntax match   juliaFloat		display contained "\.\d\+\([eE][-+]\?\d\+\)\?\(\>\|im\>\|\ze[_[:alpha:]]\)" contains=juliaComplexUnit
"floating point number, without dot, with exponent
syntax match   juliaFloat		display contained "\d\+[eE][-+]\?\d\+\(\>\|im\>\|\ze[_[:alpha:]]\)" contains=juliaComplexUnit

syntax match   juliaComplexUnit		display	contained "\<im\>"

syntax match   juliaArithOperator	"\(+\|-\|//\|%\|\.\?\(*\|/\|\\\|\^\)\)"
syntax match   juliaCompOperator	"[<>]"
syntax match   juliaBitOperator		"\(<<\|>>>\|>>\|\\&\||\|\~\)"
syntax match   juliaBoolOperator	"\(\\&\\&\|||\|!\)"
syntax match   juliaCompOperator	"\([<>]=\|!=\|==\)"
syntax match   juliaAssignOperator	"\([|\&*/\\%+-]\|<<\|>>>\|>>\)\?="
syntax match   juliaRangeOperator	":"
syntax match   juliaTypeOperator	"\(<:\|::\)"
syntax match   juliaFuncOperator	"->"
syntax match   juliaVarargOperator	"\.\{3\}"
syntax match   juliaCTransOperator	"'"
syntax region  juliaTernaryRegion	matchgroup=juliaTernaryOperator start="?" skip="::" end=":" contains=@juliaExpressions,juliaErrorSemicol

syntax match   juliaChar		display "'\\\?.'" contains=juliaSpecialChar
syntax match   juliaChar		display "'\\\o\{3\}'" contains=juliaOctalEscapeChar
syntax match   juliaChar		display "'\\x\x\{2\}'" contains=juliaHexEscapeChar
syntax match   juliaChar		display "'\\u\x\{1,4\}'" contains=juliaUniCharSmall
syntax match   juliaChar		display "'\\U\x\{1,8\}'" contains=juliaUniCharLarge

syntax region  juliaString		matchgroup=juliaStringDelim start=+"+ skip=+\(\\\\\)*\\"+ end=+"+ contains=@juliaStringVars,@juliaSpecialChars
syntax region  juliaEString		matchgroup=juliaStringDelim start=+E"+ skip=+\(\\\\\)*\\"+ end=+"+ contains=@juliaSpecialChars
syntax region  juliaIString		matchgroup=juliaStringDelim start=+I"+ skip=+\(\\\\\)*\\"+ end=+"+ contains=@juliaStringVars
syntax region  juliaLString		matchgroup=juliaStringDelim start=+L"+ skip=+\(\\\\\)*\\"+ end=+"+
syntax region  juliabString		matchgroup=juliaStringDelim start=+b"+ skip=+\(\\\\\)*\\"+ end=+"+ contains=@juliaSpecialChars

syntax region  juliaShellString		matchgroup=juliaStringDelim start=+`+ skip=+\(\\\\\)*\\`+ end=+`+ contains=@juliaStringVars,juliaSpecialChar

syntax cluster juliaStringVars		contains=juliaStringVarsPar,juliaStringVarsSqBra,juliaStringVarsCurBra,juliaStringVarsPla
syntax region  juliaStringVarsPar	contained matchgroup=juliaStringVarDelim start="$(" end=")" contains=@juliaExpressions
syntax region  juliaStringVarsSqBra	contained matchgroup=juliaStringVarDelim start="$\[" end="\]" contains=@juliaExpressions
syntax region  juliaStringVarsCurBra	contained matchgroup=juliaStringVarDelim start="${" end="}" contains=@juliaExpressions
syntax match   juliaStringVarsPla	contained "$[_[:alpha:]][_[:alnum:]]*"

" TODO improve RegEx
syntax region  juliaRegEx		matchgroup=juliaStringDelim start=+ri\?m\?s\?"+ skip=+\(\\\\\)*\\"+ end=+"+

syntax cluster juliaSpecialChars	contains=juliaSpecialChar,juliaOctalEscapeChar,juliaHexEscapeChar,juliaUniCharSmall,juliaUniCharLarge
syntax match   juliaSpecialChar		contained "\\."
syntax match   juliaOctalEscapeChar	contained "\\\o\{3\}"
syntax match   juliaHexEscapeChar	contained "\\x\x\{2\}"
syntax match   juliaUniCharSmall	contained "\\u\x\{1,4\}"
syntax match   juliaUniCharLarge	contained "\\U\x\{1,8\}"

syntax region  juliaCommentL		matchgroup=juliaCommentDelim start="#" end="$" keepend contains=@juliaCommentSpace
syntax cluster juliaCommentSpace	contains=juliaTodo
syntax keyword juliaTodo		contained TODO FIXME XXX


hi def link juliaParDelim		juliaNone

hi def link juliaKeyword		Keyword
hi def link juliaConditional		Conditional
hi def link juliaRepeat			Repeat
hi def link juliaException		Exception
hi def link juliaTypedef		Typedef
hi def link juliaBuiltinType		Type
hi def link juliaConstNum		Constant
hi def link juliaConstIO		Constant
hi def link juliaConstPtr		Constant
hi def link juliaConstLimits		Constant
hi def link juliaConstErrno		Constant
hi def link juliaConstGeneric		Constant
hi def link juliaConstBool		Boolean
hi def link juliaRangeEnd		Constant

hi def link juliaMacro			Macro

hi def link juliaNumber			Number
hi def link juliaFloat			Float
hi def link juliaComplexUnit		Constant

hi def link juliaChar			Character

hi def link juliaString			String
hi def link juliaEString		String
hi def link juliaIString		String
hi def link juliaLString		String
hi def link juliabString		String
hi def link juliaShellString		String
hi def link juliaStringDelim		String
hi def link juliaStringVarsPla		Identifier
hi def link juliaStringVarDelim		Delimiter

hi def link juliaRegEx			String

hi def link juliaSpecialChar		SpecialChar
hi def link juliaOctalEscapeChar	SpecialChar
hi def link juliaHexEscapeChar		SpecialChar
hi def link juliaUniCharSmall		SpecialChar
hi def link juliaUniCharLarge		SpecialChar

if exists("julia_highlight_operators")
  hi def link juliaOperator		Operator
else
  hi def link juliaOperator		juliaNone
endif
hi def link juliaArithOperator		juliaOperator
hi def link juliaBitOperator		juliaOperator
hi def link juliaBoolOperator		juliaOperator
hi def link juliaCompOperator		juliaOperator
hi def link juliaAssignOperator		juliaOperator
hi def link juliaRangeOperator		juliaOperator
hi def link juliaTypeOperator		juliaOperator
hi def link juliaFuncOperator		juliaOperator
hi def link juliaCTransOperator		juliaOperator
hi def link juliaVarargOperator		juliaOperator
hi def link juliaTernaryOperator	juliaOperator

hi def link juliaCommentL		Comment
hi def link juliaCommentDelim		Comment
hi def link juliaTodo			Todo

hi def link juliaErrorPar		juliaError
hi def link juliaErrorEnd		juliaError
hi def link juliaErrorElse		juliaError
hi def link juliaErrorCatch		juliaError
hi def link juliaErrorSemicol		juliaError

hi def link juliaError			Error

if exists("julia_minlines")
  let b:julia_minlines = julia_minlines
else
  let b:julia_minlines = 50
endif

syn sync fromstart
" exec "syn sync match juliaSyncBlock grouphere juliaParBlock /(/"

let b:current_syntax = "julia"
