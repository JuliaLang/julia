
#=
@enum(SyntaxKind,
    Call
)

# A type to multiplex token kinds from various libraries
struct Kind
    id::UInt32
end

Kind(k::TzTokens.Kind) = Kind(0x00010000 | UInt32(k))
Kind(k::SyntaxKind) = Kind(0x00020000 | UInt32(k))

_kind_namespace(k::Kind) = k.id >> 16
_kind_code(k::Kind)      = k.id & 0xffff

function Base.show(io::IO, k::Kind)
    ns = _kind_namespace(k)
    code = _kind_code(k)
    if ns == 1
        # Basic token kinds from Tokenize
        print(io, Kind, "(")
        show(io, Tokenize.Tokens.Kind(code))
        print(io, ")")
    elseif ns == 2
        # Syntax node kinds, defined here
        print(io, Kind, "(")
        show(io, SyntaxKind(code))
        print(io, ")")
    else
        print(io, typeof(Kind), "(", k.id, ")")
    end
end

function Base.:(==)(k1::Kind, k2::Kind)
    k1.id == k2.id
end
=#

using Tokenize.Tokens: Kind, isliteral, iskeyword, isoperator

kind(k::Kind) = k
kind(raw::TzTokens.RawToken) = TzTokens.exactkind(raw)

"""
    K"s"

The full kind of a string "s".  For example, K")" is the kind of the
right parenthesis token.
"""
macro K_str(str)
    name = Symbol(str)
    return :(Kinds.$name)
end

function _kind_str(k::Kind)
    if k in (K"Identifier", K"VarIdentifier")
        "I"
    elseif isliteral(k)
        "L"
    elseif k == K"Comment"
        "C"
    elseif k == K"Whitespace"
        "W"
    elseif k == K"NewlineWs"
        "N"
    elseif iskeyword(k)
        lowercase(string(k))
    elseif isoperator(k)
        string(TzTokens.UNICODE_OPS_REVERSE[k]) 
    elseif k == K"("
        "("
    elseif k == K"["
        "["
    elseif k == K"{"
        "{"
    elseif k == K")"
        ")"
    elseif k == K"]"
        "]"
    elseif k == K"}"
        "}"
    elseif k == K"@"
        "@"
    elseif k == K","
        ","
    elseif k == K";"
        ";"
    else
        lowercase(string(k))
    end
end

"""
A module to giving literal names to token kinds

Rules:
* Kinds which correspond to exactly one textural form are represented with that
  text.  This includes keywords like K"for" and operators like K"*".
* Kinds which represent many textural forms have UpperCamelCase names. This
  includes kinds like K"Identifier" and K"Comment".
"""
baremodule Kinds

import ..JuliaSyntax: JuliaSyntax, Kind

import Tokenize

macro _K(sym)
    :(Tokenize.Tokens.$sym)
#     :(Kind(Tokenize.Tokens.$sym))
end

const EndMarker   =  @_K ENDMARKER
const Error       =  @_K ERROR
const Comment     =  @_K COMMENT
const Whitespace  =  @_K WHITESPACE
const Identifier  =  @_K IDENTIFIER
const VarIdentifier  =  @_K VAR_IDENTIFIER
const var"@"      =  @_K AT_SIGN
const var","      =  @_K COMMA
const var";"      =  @_K SEMICOLON

const BEGIN_KEYWORDS   =  @_K begin_keywords
const Keyword          =  @_K KEYWORD
const var"abstract"    =  @_K ABSTRACT
const var"baremodule"  =  @_K BAREMODULE
const var"begin"       =  @_K BEGIN
const var"break"       =  @_K BREAK
const var"catch"       =  @_K CATCH
const var"const"       =  @_K CONST
const var"continue"    =  @_K CONTINUE
const var"do"          =  @_K DO
const var"else"        =  @_K ELSE
const var"elseif"      =  @_K ELSEIF
const var"end"         =  @_K END
const var"export"      =  @_K EXPORT
const var"finally"     =  @_K FINALLY
const var"for"         =  @_K FOR
const var"function"    =  @_K FUNCTION
const var"global"      =  @_K GLOBAL
const var"if"          =  @_K IF
const var"import"      =  @_K IMPORT
const var"importall"   =  @_K IMPORTALL
const var"let"         =  @_K LET
const var"local"       =  @_K LOCAL
const var"macro"       =  @_K MACRO
const var"module"      =  @_K MODULE
const var"mutable"     =  @_K MUTABLE
const var"new"         =  @_K NEW
const var"outer"       =  @_K OUTER
const var"primitive"   =  @_K PRIMITIVE
const var"quote"       =  @_K QUOTE
const var"return"      =  @_K RETURN
const var"struct"      =  @_K STRUCT
const var"try"         =  @_K TRY
const var"type"        =  @_K TYPE
const var"using"       =  @_K USING
const var"while"       =  @_K WHILE
const END_KEYWORDS     =  @_K end_keywords

const BEGIN_CSTPARSER     =  @_K begin_cstparser
const InvisibleBrackets  =  @_K INVISIBLE_BRACKETS
const Nothing             =  @_K NOTHING
const Ws                  =  @_K WS
const SemicolonWs        =  @_K SEMICOLON_WS
const NewlineWs          =  @_K NEWLINE_WS
const EmptyWs            =  @_K EMPTY_WS
const END_CSTPARSER       =  @_K end_cstparser

const BEGIN_LITERAL  =  @_K begin_literal
const Literal        =  @_K LITERAL
const Integer        =  @_K INTEGER
const BinInt         =  @_K BIN_INT
const HexInt         =  @_K HEX_INT
const OctInt         =  @_K OCT_INT
const Float          =  @_K FLOAT
const String         =  @_K STRING
const TripleString   =  @_K TRIPLE_STRING
const Char           =  @_K CHAR
const Cmd            =  @_K CMD
const TripleCmd      =  @_K TRIPLE_CMD
const var"true"      =  @_K TRUE
const var"false"     =  @_K FALSE
const END_LITERAL    =  @_K end_literal

const BEGIN_DELIMITERS  =  @_K begin_delimiters
const var"["            =  @_K LSQUARE
const var"]"            =  @_K RSQUARE
const var"{"            =  @_K LBRACE
const var"}"            =  @_K RBRACE
const var"("            =  @_K LPAREN
const var")"            =  @_K RPAREN
const END_DELIMITERS    =  @_K end_delimiters

const BEGIN_OPS  =  @_K begin_ops
const OP         =  @_K OP
const var"..."   =  @_K DDDOT

# Level 1
const BEGIN_ASSIGNMENTS  =  @_K begin_assignments
const var"="             =  @_K EQ
const var"+="            =  @_K PLUS_EQ
const var"-="            =  @_K MINUS_EQ
const var"*="            =  @_K STAR_EQ
const var"/="            =  @_K FWD_SLASH_EQ
const var"//="           =  @_K FWDFWD_SLASH_EQ
const var"|="            =  @_K OR_EQ
const var"^="            =  @_K CIRCUMFLEX_EQ
const var"÷="            =  @_K DIVISION_EQ
const var"%="            =  @_K REM_EQ
const var"<<="           =  @_K LBITSHIFT_EQ
const var">>="           =  @_K RBITSHIFT_EQ
const var">>>="          =  @_K UNSIGNED_BITSHIFT_EQ
const var"\="            =  @_K BACKSLASH_EQ
const var"&="            =  @_K AND_EQ
const var":="            =  @_K COLON_EQ
const var"~"             =  @_K APPROX
const var"$="            =  @_K EX_OR_EQ
const var"⊻="            =  @_K XOR_EQ
const END_ASSIGNMENTS    =  @_K end_assignments

const BEGIN_PAIRARROW  =  @_K begin_pairarrow
const var"=>"          =  @_K PAIR_ARROW
const END_PAIRARROW    =  @_K end_pairarrow

# Level 2
const BEGIN_CONDITIONAL  =  @_K begin_conditional
const var"?"             =  @_K CONDITIONAL
const END_CONDITIONAL    =  @_K end_conditional

# Level 3
const BEGIN_ARROW  =  @_K begin_arrow
const var"-->"     =  @_K RIGHT_ARROW
const var"<--"     =  @_K LEFT_ARROW
const var"<-->"    =  @_K DOUBLE_ARROW
const var"←"       =  @_K LEFTWARDS_ARROW
const var"→"       =  @_K RIGHTWARDS_ARROW
const var"↔"       =  @_K LEFT_RIGHT_ARROW
const var"↚"       =  @_K LEFTWARDS_ARROW_WITH_STROKE
const var"↛"       =  @_K RIGHTWARDS_ARROW_WITH_STROKE
const var"↞"       =  @_K LEFTWARDS_TWO_HEADED_ARROW
const var"↠"       =  @_K RIGHTWARDS_TWO_HEADED_ARROW
const var"↢"       =  @_K LEFTWARDS_ARROW_WITH_TAIL
const var"↣"       =  @_K RIGHTWARDS_ARROW_WITH_TAIL
const var"↤"       =  @_K LEFTWARDS_ARROW_FROM_BAR
const var"↦"       =  @_K RIGHTWARDS_ARROW_FROM_BAR
const var"↮"       =  @_K LEFT_RIGHT_ARROW_WITH_STROKE
const var"⇎"       =  @_K LEFT_RIGHT_DOUBLE_ARROW_WITH_STROKE
const var"⇍"       =  @_K LEFTWARDS_DOUBLE_ARROW_WITH_STROKE
const var"⇏"       =  @_K RIGHTWARDS_DOUBLE_ARROW_WITH_STROKE
const var"⇐"       =  @_K LEFTWARDS_DOUBLE_ARROW
const var"⇒"       =  @_K RIGHTWARDS_DOUBLE_ARROW
const var"⇔"       =  @_K LEFT_RIGHT_DOUBLE_ARROW
const var"⇴"       =  @_K RIGHT_ARROW_WITH_SMALL_CIRCLE
const var"⇶"       =  @_K THREE_RIGHTWARDS_ARROWS
const var"⇷"       =  @_K LEFTWARDS_ARROW_WITH_VERTICAL_STROKE
const var"⇸"       =  @_K RIGHTWARDS_ARROW_WITH_VERTICAL_STROKE
const var"⇹"       =  @_K LEFT_RIGHT_ARROW_WITH_VERTICAL_STROKE
const var"⇺"       =  @_K LEFTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE
const var"⇻"       =  @_K RIGHTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE
const var"⇼"       =  @_K LEFT_RIGHT_ARROW_WITH_DOUBLE_VERTICAL_STROKE
const var"⇽"       =  @_K LEFTWARDS_OPEN_HEADED_ARROW
const var"⇾"       =  @_K RIGHTWARDS_OPEN_HEADED_ARROW
const var"⇿"       =  @_K LEFT_RIGHT_OPEN_HEADED_ARROW
const var"⟵"       =  @_K LONG_LEFTWARDS_ARROW
const var"⟶"       =  @_K LONG_RIGHTWARDS_ARROW
const var"⟷"       =  @_K LONG_LEFT_RIGHT_ARROW
const var"⟹"       =  @_K LONG_RIGHTWARDS_DOUBLE_ARROW
const var"⟺"       =  @_K LONG_LEFT_RIGHT_DOUBLE_ARROW
const var"⟻"       =  @_K LONG_LEFTWARDS_ARROW_FROM_BAR
const var"⟼"       =  @_K LONG_RIGHTWARDS_ARROW_FROM_BAR
const var"⟽"       =  @_K LONG_LEFTWARDS_DOUBLE_ARROW_FROM_BAR
const var"⟾"       =  @_K LONG_RIGHTWARDS_DOUBLE_ARROW_FROM_BAR
const var"⟿"       =  @_K LONG_RIGHTWARDS_SQUIGGLE_ARROW
const var"⤀"       =  @_K RIGHTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE
const var"⤁"       =  @_K RIGHTWARDS_TWO_HEADED_ARROW_WITH_DOUBLE_VERTICAL_STROKE
const var"⤂"       =  @_K LEFTWARDS_DOUBLE_ARROW_WITH_VERTICAL_STROKE
const var"⤃"       =  @_K RIGHTWARDS_DOUBLE_ARROW_WITH_VERTICAL_STROKE
const var"⤄"       =  @_K LEFT_RIGHT_DOUBLE_ARROW_WITH_VERTICAL_STROKE
const var"⤅"       =  @_K RIGHTWARDS_TWO_HEADED_ARROW_FROM_BAR
const var"⤆"       =  @_K LEFTWARDS_DOUBLE_ARROW_FROM_BAR
const var"⤇"       =  @_K RIGHTWARDS_DOUBLE_ARROW_FROM_BAR
const var"⤌"       =  @_K LEFTWARDS_DOUBLE_DASH_ARROW
const var"⤍"       =  @_K RIGHTWARDS_DOUBLE_DASH_ARROW
const var"⤎"       =  @_K LEFTWARDS_TRIPLE_DASH_ARROW
const var"⤏"       =  @_K RIGHTWARDS_TRIPLE_DASH_ARROW
const var"⤐"       =  @_K RIGHTWARDS_TWO_HEADED_TRIPLE_DASH_ARROW
const var"⤑"       =  @_K RIGHTWARDS_ARROW_WITH_DOTTED_STEM
const var"⤔"       =  @_K RIGHTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE
const var"⤕"       =  @_K RIGHTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE
const var"⤖"       =  @_K RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL
const var"⤗"       =  @_K RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE
const var"⤘"       =  @_K RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE
const var"⤝"       =  @_K LEFTWARDS_ARROW_TO_BLACK_DIAMOND
const var"⤞"       =  @_K RIGHTWARDS_ARROW_TO_BLACK_DIAMOND
const var"⤟"       =  @_K LEFTWARDS_ARROW_FROM_BAR_TO_BLACK_DIAMOND
const var"⤠"       =  @_K RIGHTWARDS_ARROW_FROM_BAR_TO_BLACK_DIAMOND
const var"⥄"       =  @_K SHORT_RIGHTWARDS_ARROW_ABOVE_LEFTWARDS_ARROW
const var"⥅"       =  @_K RIGHTWARDS_ARROW_WITH_PLUS_BELOW
const var"⥆"       =  @_K LEFTWARDS_ARROW_WITH_PLUS_BELOW
const var"⥇"       =  @_K RIGHTWARDS_ARROW_THROUGH_X
const var"⥈"       =  @_K LEFT_RIGHT_ARROW_THROUGH_SMALL_CIRCLE
const var"⥊"       =  @_K LEFT_BARB_UP_RIGHT_BARB_DOWN_HARPOON
const var"⥋"       =  @_K LEFT_BARB_DOWN_RIGHT_BARB_UP_HARPOON
const var"⥎"       =  @_K LEFT_BARB_UP_RIGHT_BARB_UP_HARPOON
const var"⥐"       =  @_K LEFT_BARB_DOWN_RIGHT_BARB_DOWN_HARPOON
const var"⥒"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_UP_TO_BAR
const var"⥓"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_UP_TO_BAR
const var"⥖"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_DOWN_TO_BAR
const var"⥗"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_DOWN_TO_BAR
const var"⥚"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_UP_FROM_BAR
const var"⥛"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_UP_FROM_BAR
const var"⥞"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_DOWN_FROM_BAR
const var"⥟"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_DOWN_FROM_BAR
const var"⥢"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_DOWN
const var"⥤"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_DOWN
const var"⥦"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_UP
const var"⥧"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_DOWN_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_DOWN
const var"⥨"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_UP
const var"⥩"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_DOWN_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_DOWN
const var"⥪"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LONG_DASH
const var"⥫"       =  @_K LEFTWARDS_HARPOON_WITH_BARB_DOWN_BELOW_LONG_DASH
const var"⥬"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LONG_DASH
const var"⥭"       =  @_K RIGHTWARDS_HARPOON_WITH_BARB_DOWN_BELOW_LONG_DASH
const var"⥰"       =  @_K RIGHT_DOUBLE_ARROW_WITH_ROUNDED_HEAD
const var"⧴"       =  @_K RULE_DELAYED
const var"⬱"       =  @_K THREE_LEFTWARDS_ARROWS
const var"⬰"       =  @_K LEFT_ARROW_WITH_SMALL_CIRCLE
const var"⬲"       =  @_K LEFT_ARROW_WITH_CIRCLED_PLUS
const var"⬳"       =  @_K LONG_LEFTWARDS_SQUIGGLE_ARROW
const var"⬴"       =  @_K LEFTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE
const var"⬵"       =  @_K LEFTWARDS_TWO_HEADED_ARROW_WITH_DOUBLE_VERTICAL_STROKE
const var"⬶"       =  @_K LEFTWARDS_TWO_HEADED_ARROW_FROM_BAR
const var"⬷"       =  @_K LEFTWARDS_TWO_HEADED_TRIPLE_DASH_ARROW
const var"⬸"       =  @_K LEFTWARDS_ARROW_WITH_DOTTED_STEM
const var"⬹"       =  @_K LEFTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE
const var"⬺"       =  @_K LEFTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE
const var"⬻"       =  @_K LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL
const var"⬼"       =  @_K LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE
const var"⬽"       =  @_K LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE
const var"⬾"       =  @_K LEFTWARDS_ARROW_THROUGH_X
const var"⬿"       =  @_K WAVE_ARROW_POINTING_DIRECTLY_LEFT
const var"⭀"       =  @_K EQUALS_SIGN_ABOVE_LEFTWARDS_ARROW
const var"⭁"       =  @_K REVERSE_TILDE_OPERATOR_ABOVE_LEFTWARDS_ARROW
const var"⭂"       =  @_K LEFTWARDS_ARROW_ABOVE_REVERSE_ALMOST_EQUAL_TO
const var"⭃"       =  @_K RIGHTWARDS_ARROW_THROUGH_GREATER_THAN
const var"⭄"       =  @_K RIGHTWARDS_ARROW_THROUGH_SUPERSET
const var"⭇"       =  @_K REVERSE_TILDE_OPERATOR_ABOVE_RIGHTWARDS_ARROW
const var"⭈"       =  @_K RIGHTWARDS_ARROW_ABOVE_REVERSE_ALMOST_EQUAL_TO
const var"⭉"       =  @_K TILDE_OPERATOR_ABOVE_LEFTWARDS_ARROW
const var"⭊"       =  @_K LEFTWARDS_ARROW_ABOVE_ALMOST_EQUAL_TO
const var"⭋"       =  @_K LEFTWARDS_ARROW_ABOVE_REVERSE_TILDE_OPERATOR
const var"⭌"       =  @_K RIGHTWARDS_ARROW_ABOVE_REVERSE_TILDE_OPERATOR
const var"￩"       =  @_K HALFWIDTH_LEFTWARDS_ARROW
const var"￫"       =  @_K HALFWIDTH_RIGHTWARDS_ARROW
const var"↻"       =  @_K CIRCLE_ARROW_RIGHT
const var"⇜"       =  @_K LEFT_SQUIGGLE_ARROW
const var"⇝"       =  @_K RIGHT_SQUIGGLE_ARROW
const var"↜"       =  @_K LEFT_WAVE_ARROW
const var"↝"       =  @_K RIGHT_WAVE_ARROW
const var"↩"       =  @_K LEFTWARDS_ARROW_WITH_HOOK
const var"↪"       =  @_K RIGHTWARDS_ARROW_WITH_HOOK
const var"↫"       =  @_K LOOP_ARROW_LEFT
const var"↬"       =  @_K LOOP_ARROW_RIGHT
const var"↼"       =  @_K LEFT_HARPOON_UP
const var"↽"       =  @_K LEFT_HARPOON_DOWN
const var"⇀"       =  @_K RIGHT_HARPOON_UP
const var"⇁"       =  @_K RIGHT_HARPOON_DOWN
const var"⇄"       =  @_K RIGHT_LEFT_ARROWS
const var"⇆"       =  @_K LEFT_RIGHT_ARROWS
const var"⇇"       =  @_K LEFT_LEFT_ARROWS
const var"⇉"       =  @_K RIGHT_RIGHT_ARROWS
const var"⇋"       =  @_K LEFT_RIGHT_HARPOONS
const var"⇌"       =  @_K RIGHT_LEFT_HARPOONS
const var"⇚"       =  @_K L_LEFT_ARROW
const var"⇛"       =  @_K R_RIGHT_ARROW
const var"⇠"       =  @_K LEFT_DASH_ARROW
const var"⇢"       =  @_K RIGHT_DASH_ARROW
const var"↷"       =  @_K CURVE_ARROW_RIGHT
const var"↶"       =  @_K CURVE_ARROW_LEFT
const var"↺"       =  @_K CIRCLE_ARROW_LEFT
const END_ARROW    =  @_K end_arrow

# Level 4
const BEGIN_LAZYOR = @_K begin_lazyor
const var"||"      = @_K LAZY_OR
const END_LAZYOR   = @_K end_lazyor

# Level 5
const BEGIN_LAZYAND = @_K begin_lazyand
const var"&&"       = @_K LAZY_AND
const END_LAZYAND   = @_K end_lazyand

# Level 6
const BEGIN_COMPARISON = @_K begin_comparison
const var"<:"   =  @_K ISSUBTYPE
const var">:"   =  @_K ISSUPERTYPE
const var">"    =  @_K GREATER
const var"<"    =  @_K LESS
const var">="   =  @_K GREATER_EQ
const var"≥"    =  @_K GREATER_THAN_OR_EQUAL_TO
const var"<="   =  @_K LESS_EQ
const var"≤"    =  @_K LESS_THAN_OR_EQUAL_TO
const var"=="   =  @_K EQEQ
const var"==="  =  @_K EQEQEQ
const var"≡"    =  @_K IDENTICAL_TO
const var"!="   =  @_K NOT_EQ
const var"≠"    =  @_K NOT_EQUAL_TO
const var"!=="  =  @_K NOT_IS
const var"≢"    =  @_K NOT_IDENTICAL_TO
const var"∈"    =  @_K ELEMENT_OF
const var"in"   =  @_K IN
const var"isa"  =  @_K ISA
const var"∉"    =  @_K NOT_AN_ELEMENT_OF
const var"∋"    =  @_K CONTAINS_AS_MEMBER
const var"∌"    =  @_K DOES_NOT_CONTAIN_AS_MEMBER
const var"⊆"    =  @_K SUBSET_OF_OR_EQUAL_TO
const var"⊈"    =  @_K NEITHER_A_SUBSET_OF_NOR_EQUAL_TO
const var"⊂"    =  @_K SUBSET_OF
const var"⊄"    =  @_K NOT_A_SUBSET_OF
const var"⊊"    =  @_K SUBSET_OF_WITH_NOT_EQUAL_TO
const var"∝"    =  @_K PROPORTIONAL_TO
const var"∊"    =  @_K SMALL_ELEMENT_OF
const var"∍"    =  @_K SMALL_CONTAINS_AS_MEMBER
const var"∥"    =  @_K PARALLEL_TO
const var"∦"    =  @_K NOT_PARALLEL_TO
const var"∷"    =  @_K PROPORTION
const var"∺"    =  @_K GEOMETRIC_PROPORTION
const var"∻"    =  @_K HOMOTHETIC
const var"∽"    =  @_K REVERSED_TILDE
const var"∾"    =  @_K INVERTED_LAZY_S
const var"≁"    =  @_K NOT_TILDE
const var"≃"    =  @_K ASYMPTOTICALLY_EQUAL_TO
const var"≄"    =  @_K NOT_ASYMPTOTICALLY_EQUAL_TO
const var"≅"    =  @_K APPROXIMATELY_EQUAL_TO
const var"≆"    =  @_K APPROXIMATELY_BUT_NOT_ACTUALLY_EQUAL_TO
const var"≇"    =  @_K NEITHER_APPROXIMATELY_NOR_ACTUALLY_EQUAL_TO
const var"≈"    =  @_K ALMOST_EQUAL_TO
const var"≉"    =  @_K NOT_ALMOST_EQUAL_TO
const var"≊"    =  @_K ALMOST_EQUAL_OR_EQUAL_TO
const var"≋"    =  @_K TRIPLE_TILDE
const var"≌"    =  @_K ALL_EQUAL_TO
const var"≍"    =  @_K EQUIVALENT_TO
const var"≎"    =  @_K GEOMETRICALLY_EQUIVALENT_TO
const var"≐"    =  @_K APPROACHES_THE_LIMIT
const var"≑"    =  @_K GEOMETRICALLY_EQUAL_TO
const var"≒"    =  @_K APPROXIMATELY_EQUAL_TO_OR_THE_IMAGE_OF
const var"≓"    =  @_K IMAGE_OF_OR_APPROXIMATELY_EQUAL_TO
const var"≔"    =  @_K COLON_EQUALS
const var"≕"    =  @_K EQUALS_COLON
const var"≖"    =  @_K RING_IN_EQUAL_TO
const var"≗"    =  @_K RING_EQUAL_TO
const var"≘"    =  @_K CORRESPONDS_TO
const var"≙"    =  @_K ESTIMATES
const var"≚"    =  @_K EQUIANGULAR_TO
const var"≛"    =  @_K STAR_EQUALS
const var"≜"    =  @_K DELTA_EQUAL_TO
const var"≝"    =  @_K EQUAL_TO_BY_DEFINITION
const var"≞"    =  @_K MEASURED_BY
const var"≟"    =  @_K QUESTIONED_EQUAL_TO
const var"≣"    =  @_K STRICTLY_EQUIVALENT_TO
const var"≦"    =  @_K LESS_THAN_OVER_EQUAL_TO
const var"≧"    =  @_K GREATER_THAN_OVER_EQUAL_TO
const var"≨"    =  @_K LESS_THAN_BUT_NOT_EQUAL_TO
const var"≩"    =  @_K GREATER_THAN_BUT_NOT_EQUAL_TO
const var"≪"    =  @_K MUCH_LESS_THAN
const var"≫"    =  @_K MUCH_GREATER_THAN
const var"≬"    =  @_K BETWEEN
const var"≭"    =  @_K NOT_EQUIVALENT_TO
const var"≮"    =  @_K NOT_LESS_THAN
const var"≯"    =  @_K NOT_GREATER_THAN
const var"≰"    =  @_K NEITHER_LESS_THAN_NOR_EQUAL_TO
const var"≱"    =  @_K NEITHER_GREATER_THAN_NOR_EQUAL_TO
const var"≲"    =  @_K LESS_THAN_OR_EQUIVALENT_TO
const var"≳"    =  @_K GREATER_THAN_OR_EQUIVALENT_TO
const var"≴"    =  @_K NEITHER_LESS_THAN_NOR_EQUIVALENT_TO
const var"≵"    =  @_K NEITHER_GREATER_THAN_NOR_EQUIVALENT_TO
const var"≶"    =  @_K LESS_THAN_OR_GREATER_THAN
const var"≷"    =  @_K GREATER_THAN_OR_LESS_THAN
const var"≸"    =  @_K NEITHER_LESS_THAN_NOR_GREATER_THAN
const var"≹"    =  @_K NEITHER_GREATER_THAN_NOR_LESS_THAN
const var"≺"    =  @_K PRECEDES
const var"≻"    =  @_K SUCCEEDS
const var"≼"    =  @_K PRECEDES_OR_EQUAL_TO
const var"≽"    =  @_K SUCCEEDS_OR_EQUAL_TO
const var"≾"    =  @_K PRECEDES_OR_EQUIVALENT_TO
const var"≿"    =  @_K SUCCEEDS_OR_EQUIVALENT_TO
const var"⊀"    =  @_K DOES_NOT_PRECEDE
const var"⊁"    =  @_K DOES_NOT_SUCCEED
const var"⊃"    =  @_K SUPERSET_OF
const var"⊅"    =  @_K NOT_A_SUPERSET_OF
const var"⊇"    =  @_K SUPERSET_OF_OR_EQUAL_TO
const var"⊉"    =  @_K NEITHER_A_SUPERSET_OF_NOR_EQUAL_TO
const var"⊋"    =  @_K SUPERSET_OF_WITH_NOT_EQUAL_TO
const var"⊏"    =  @_K SQUARE_IMAGE_OF
const var"⊐"    =  @_K SQUARE_ORIGINAL_OF
const var"⊑"    =  @_K SQUARE_IMAGE_OF_OR_EQUAL_TO
const var"⊒"    =  @_K SQUARE_ORIGINAL_OF_OR_EQUAL_TO
const var"⊜"    =  @_K CIRCLED_EQUALS
const var"⊩"    =  @_K FORCES
const var"⊬"    =  @_K DOES_NOT_PROVE
const var"⊮"    =  @_K DOES_NOT_FORCE
const var"⊰"    =  @_K PRECEDES_UNDER_RELATION
const var"⊱"    =  @_K SUCCEEDS_UNDER_RELATION
const var"⊲"    =  @_K NORMAL_SUBGROUP_OF
const var"⊳"    =  @_K CONTAINS_AS_NORMAL_SUBGROUP
const var"⊴"    =  @_K NORMAL_SUBGROUP_OF_OR_EQUAL_TO
const var"⊵"    =  @_K CONTAINS_AS_NORMAL_SUBGROUP_OR_EQUAL_TO
const var"⊶"    =  @_K ORIGINAL_OF
const var"⊷"    =  @_K IMAGE_OF
const var"⋍"    =  @_K REVERSED_TILDE_EQUALS
const var"⋐"    =  @_K DOUBLE_SUBSET
const var"⋑"    =  @_K DOUBLE_SUPERSET
const var"⋕"    =  @_K EQUAL_AND_PARALLEL_TO
const var"⋖"    =  @_K LESS_THAN_WITH_DOT
const var"⋗"    =  @_K GREATER_THAN_WITH_DOT
const var"⋘"    =  @_K VERY_MUCH_LESS_THAN
const var"⋙"    =  @_K VERY_MUCH_GREATER_THAN
const var"⋚"    =  @_K LESS_THAN_EQUAL_TO_OR_GREATER_THAN
const var"⋛"    =  @_K GREATER_THAN_EQUAL_TO_OR_LESS_THAN
const var"⋜"    =  @_K EQUAL_TO_OR_LESS_THAN
const var"⋝"    =  @_K EQUAL_TO_OR_GREATER_THAN
const var"⋞"    =  @_K EQUAL_TO_OR_PRECEDES
const var"⋟"    =  @_K EQUAL_TO_OR_SUCCEEDS
const var"⋠"    =  @_K DOES_NOT_PRECEDE_OR_EQUAL
const var"⋡"    =  @_K DOES_NOT_SUCCEED_OR_EQUAL
const var"⋢"    =  @_K NOT_SQUARE_IMAGE_OF_OR_EQUAL_TO
const var"⋣"    =  @_K NOT_SQUARE_ORIGINAL_OF_OR_EQUAL_TO
const var"⋤"    =  @_K SQUARE_IMAGE_OF_OR_NOT_EQUAL_TO
const var"⋥"    =  @_K SQUARE_ORIGINAL_OF_OR_NOT_EQUAL_TO
const var"⋦"    =  @_K LESS_THAN_BUT_NOT_EQUIVALENT_TO
const var"⋧"    =  @_K GREATER_THAN_BUT_NOT_EQUIVALENT_TO
const var"⋨"    =  @_K PRECEDES_BUT_NOT_EQUIVALENT_TO
const var"⋩"    =  @_K SUCCEEDS_BUT_NOT_EQUIVALENT_TO
const var"⋪"    =  @_K NOT_NORMAL_SUBGROUP_OF
const var"⋫"    =  @_K DOES_NOT_CONTAIN_AS_NORMAL_SUBGROUP
const var"⋬"    =  @_K NOT_NORMAL_SUBGROUP_OF_OR_EQUAL_TO
const var"⋭"    =  @_K DOES_NOT_CONTAIN_AS_NORMAL_SUBGROUP_OR_EQUAL
const var"⋲"    =  @_K ELEMENT_OF_WITH_LONG_HORIZONTAL_STROKE
const var"⋳"    =  @_K ELEMENT_OF_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE
const var"⋴"    =  @_K SMALL_ELEMENT_OF_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE
const var"⋵"    =  @_K ELEMENT_OF_WITH_DOT_ABOVE
const var"⋶"    =  @_K ELEMENT_OF_WITH_OVERBAR
const var"⋷"    =  @_K SMALL_ELEMENT_OF_WITH_OVERBAR
const var"⋸"    =  @_K ELEMENT_OF_WITH_UNDERBAR
const var"⋹"    =  @_K ELEMENT_OF_WITH_TWO_HORIZONTAL_STROKES
const var"⋺"    =  @_K CONTAINS_WITH_LONG_HORIZONTAL_STROKE
const var"⋻"    =  @_K CONTAINS_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE
const var"⋼"    =  @_K SMALL_CONTAINS_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE
const var"⋽"    =  @_K CONTAINS_WITH_OVERBAR
const var"⋾"    =  @_K SMALL_CONTAINS_WITH_OVERBAR
const var"⋿"    =  @_K Z_NOTATION_BAG_MEMBERSHIP
const var"⟈"    =  @_K REVERSE_SOLIDUS_PRECEDING_SUBSET
const var"⟉"    =  @_K SUPERSET_PRECEDING_SOLIDUS
const var"⟒"    =  @_K ELEMENT_OF_OPENING_UPWARDS
const var"⦷"    =  @_K CIRCLED_PARALLEL
const var"⧀"    =  @_K CIRCLED_LESS_THAN
const var"⧁"    =  @_K CIRCLED_GREATER_THAN
const var"⧡"    =  @_K INCREASES_AS
const var"⧣"    =  @_K EQUALS_SIGN_AND_SLANTED_PARALLEL
const var"⧤"    =  @_K EQUALS_SIGN_AND_SLANTED_PARALLEL_WITH_TILDE_ABOVE
const var"⧥"    =  @_K IDENTICAL_TO_AND_SLANTED_PARALLEL
const var"⩦"    =  @_K EQUALS_SIGN_WITH_DOT_BELOW
const var"⩧"    =  @_K IDENTICAL_WITH_DOT_ABOVE
const var"⩪"    =  @_K TILDE_OPERATOR_WITH_DOT_ABOVE
const var"⩫"    =  @_K TILDE_OPERATOR_WITH_RISING_DOTS
const var"⩬"    =  @_K SIMILAR_MINUS_SIMILAR
const var"⩭"    =  @_K CONGRUENT_WITH_DOT_ABOVE
const var"⩮"    =  @_K EQUALS_WITH_ASTERISK
const var"⩯"    =  @_K ALMOST_EQUAL_TO_WITH_CIRCUMFLEX_ACCENT
const var"⩰"    =  @_K APPROXIMATELY_EQUAL_OR_EQUAL_TO
const var"⩱"    =  @_K EQUALS_SIGN_ABOVE_PLUS_SIGN
const var"⩲"    =  @_K PLUS_SIGN_ABOVE_EQUALS_SIGN
const var"⩳"    =  @_K EQUALS_SIGN_ABOVE_TILDE_OPERATOR
const var"⩴"    =  @_K DOUBLE_COLON_EQUAL
const var"⩵"    =  @_K TWO_CONSECUTIVE_EQUALS_SIGNS
const var"⩶"    =  @_K THREE_CONSECUTIVE_EQUALS_SIGNS
const var"⩷"    =  @_K EQUALS_SIGN_WITH_TWO_DOTS_ABOVE_AND_TWO_DOTS_BELOW
const var"⩸"    =  @_K EQUIVALENT_WITH_FOUR_DOTS_ABOVE
const var"⩹"    =  @_K LESS_THAN_WITH_CIRCLE_INSIDE
const var"⩺"    =  @_K GREATER_THAN_WITH_CIRCLE_INSIDE
const var"⩻"    =  @_K LESS_THAN_WITH_QUESTION_MARK_ABOVE
const var"⩼"    =  @_K GREATER_THAN_WITH_QUESTION_MARK_ABOVE
const var"⩽"    =  @_K LESS_THAN_OR_SLANTED_EQUAL_TO
const var"⩾"    =  @_K GREATER_THAN_OR_SLANTED_EQUAL_TO
const var"⩿"    =  @_K LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_INSIDE
const var"⪀"    =  @_K GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_INSIDE
const var"⪁"    =  @_K LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE
const var"⪂"    =  @_K GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE
const var"⪃"    =  @_K LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE_RIGHT
const var"⪄"    =  @_K GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE_LEFT
const var"⪅"    =  @_K LESS_THAN_OR_APPROXIMATE
const var"⪆"    =  @_K GREATER_THAN_OR_APPROXIMATE
const var"⪇"    =  @_K LESS_THAN_AND_SINGLE_LINE_NOT_EQUAL_TO
const var"⪈"    =  @_K GREATER_THAN_AND_SINGLE_LINE_NOT_EQUAL_TO
const var"⪉"    =  @_K LESS_THAN_AND_NOT_APPROXIMATE
const var"⪊"    =  @_K GREATER_THAN_AND_NOT_APPROXIMATE
const var"⪋"    =  @_K LESS_THAN_ABOVE_DOUBLE_LINE_EQUAL_ABOVE_GREATER_THAN
const var"⪌"    =  @_K GREATER_THAN_ABOVE_DOUBLE_LINE_EQUAL_ABOVE_LESS_THAN
const var"⪍"    =  @_K LESS_THAN_ABOVE_SIMILAR_OR_EQUAL
const var"⪎"    =  @_K GREATER_THAN_ABOVE_SIMILAR_OR_EQUAL
const var"⪏"    =  @_K LESS_THAN_ABOVE_SIMILAR_ABOVE_GREATER_THAN
const var"⪐"    =  @_K GREATER_THAN_ABOVE_SIMILAR_ABOVE_LESS_THAN
const var"⪑"    =  @_K LESS_THAN_ABOVE_GREATER_THAN_ABOVE_DOUBLE_LINE_EQUAL
const var"⪒"    =  @_K GREATER_THAN_ABOVE_LESS_THAN_ABOVE_DOUBLE_LINE_EQUAL
const var"⪓"    =  @_K LESS_THAN_ABOVE_SLANTED_EQUAL_ABOVE_GREATER_THAN_ABOVE_SLANTED_EQUAL
const var"⪔"    =  @_K GREATER_THAN_ABOVE_SLANTED_EQUAL_ABOVE_LESS_THAN_ABOVE_SLANTED_EQUAL
const var"⪕"    =  @_K SLANTED_EQUAL_TO_OR_LESS_THAN
const var"⪖"    =  @_K SLANTED_EQUAL_TO_OR_GREATER_THAN
const var"⪗"    =  @_K SLANTED_EQUAL_TO_OR_LESS_THAN_WITH_DOT_INSIDE
const var"⪘"    =  @_K SLANTED_EQUAL_TO_OR_GREATER_THAN_WITH_DOT_INSIDE
const var"⪙"    =  @_K DOUBLE_LINE_EQUAL_TO_OR_LESS_THAN
const var"⪚"    =  @_K DOUBLE_LINE_EQUAL_TO_OR_GREATER_THAN
const var"⪛"    =  @_K DOUBLE_LINE_SLANTED_EQUAL_TO_OR_LESS_THAN
const var"⪜"    =  @_K DOUBLE_LINE_SLANTED_EQUAL_TO_OR_GREATER_THAN
const var"⪝"    =  @_K SIMILAR_OR_LESS_THAN
const var"⪞"    =  @_K SIMILAR_OR_GREATER_THAN
const var"⪟"    =  @_K SIMILAR_ABOVE_LESS_THAN_ABOVE_EQUALS_SIGN
const var"⪠"    =  @_K SIMILAR_ABOVE_GREATER_THAN_ABOVE_EQUALS_SIGN
const var"⪡"    =  @_K DOUBLE_NESTED_LESS_THAN
const var"⪢"    =  @_K DOUBLE_NESTED_GREATER_THAN
const var"⪣"    =  @_K DOUBLE_NESTED_LESS_THAN_WITH_UNDERBAR
const var"⪤"    =  @_K GREATER_THAN_OVERLAPPING_LESS_THAN
const var"⪥"    =  @_K GREATER_THAN_BESIDE_LESS_THAN
const var"⪦"    =  @_K LESS_THAN_CLOSED_BY_CURVE
const var"⪧"    =  @_K GREATER_THAN_CLOSED_BY_CURVE
const var"⪨"    =  @_K LESS_THAN_CLOSED_BY_CURVE_ABOVE_SLANTED_EQUAL
const var"⪩"    =  @_K GREATER_THAN_CLOSED_BY_CURVE_ABOVE_SLANTED_EQUAL
const var"⪪"    =  @_K SMALLER_THAN
const var"⪫"    =  @_K LARGER_THAN
const var"⪬"    =  @_K SMALLER_THAN_OR_EQUAL_TO
const var"⪭"    =  @_K LARGER_THAN_OR_EQUAL_TO
const var"⪮"    =  @_K EQUALS_SIGN_WITH_BUMPY_ABOVE
const var"⪯"    =  @_K PRECEDES_ABOVE_SINGLE_LINE_EQUALS_SIGN
const var"⪰"    =  @_K SUCCEEDS_ABOVE_SINGLE_LINE_EQUALS_SIGN
const var"⪱"    =  @_K PRECEDES_ABOVE_SINGLE_LINE_NOT_EQUAL_TO
const var"⪲"    =  @_K SUCCEEDS_ABOVE_SINGLE_LINE_NOT_EQUAL_TO
const var"⪳"    =  @_K PRECEDES_ABOVE_EQUALS_SIGN
const var"⪴"    =  @_K SUCCEEDS_ABOVE_EQUALS_SIGN
const var"⪵"    =  @_K PRECEDES_ABOVE_NOT_EQUAL_TO
const var"⪶"    =  @_K SUCCEEDS_ABOVE_NOT_EQUAL_TO
const var"⪷"    =  @_K PRECEDES_ABOVE_ALMOST_EQUAL_TO
const var"⪸"    =  @_K SUCCEEDS_ABOVE_ALMOST_EQUAL_TO
const var"⪹"    =  @_K PRECEDES_ABOVE_NOT_ALMOST_EQUAL_TO
const var"⪺"    =  @_K SUCCEEDS_ABOVE_NOT_ALMOST_EQUAL_TO
const var"⪻"    =  @_K DOUBLE_PRECEDES
const var"⪼"    =  @_K DOUBLE_SUCCEEDS
const var"⪽"    =  @_K SUBSET_WITH_DOT
const var"⪾"    =  @_K SUPERSET_WITH_DOT
const var"⪿"    =  @_K SUBSET_WITH_PLUS_SIGN_BELOW
const var"⫀"    =  @_K SUPERSET_WITH_PLUS_SIGN_BELOW
const var"⫁"    =  @_K SUBSET_WITH_MULTIPLICATION_SIGN_BELOW
const var"⫂"    =  @_K SUPERSET_WITH_MULTIPLICATION_SIGN_BELOW
const var"⫃"    =  @_K SUBSET_OF_OR_EQUAL_TO_WITH_DOT_ABOVE
const var"⫄"    =  @_K SUPERSET_OF_OR_EQUAL_TO_WITH_DOT_ABOVE
const var"⫅"    =  @_K SUBSET_OF_ABOVE_EQUALS_SIGN
const var"⫆"    =  @_K SUPERSET_OF_ABOVE_EQUALS_SIGN
const var"⫇"    =  @_K SUBSET_OF_ABOVE_TILDE_OPERATOR
const var"⫈"    =  @_K SUPERSET_OF_ABOVE_TILDE_OPERATOR
const var"⫉"    =  @_K SUBSET_OF_ABOVE_ALMOST_EQUAL_TO
const var"⫊"    =  @_K SUPERSET_OF_ABOVE_ALMOST_EQUAL_TO
const var"⫋"    =  @_K SUBSET_OF_ABOVE_NOT_EQUAL_TO
const var"⫌"    =  @_K SUPERSET_OF_ABOVE_NOT_EQUAL_TO
const var"⫍"    =  @_K SQUARE_LEFT_OPEN_BOX_OPERATOR
const var"⫎"    =  @_K SQUARE_RIGHT_OPEN_BOX_OPERATOR
const var"⫏"    =  @_K CLOSED_SUBSET
const var"⫐"    =  @_K CLOSED_SUPERSET
const var"⫑"    =  @_K CLOSED_SUBSET_OR_EQUAL_TO
const var"⫒"    =  @_K CLOSED_SUPERSET_OR_EQUAL_TO
const var"⫓"    =  @_K SUBSET_ABOVE_SUPERSET
const var"⫔"    =  @_K SUPERSET_ABOVE_SUBSET
const var"⫕"    =  @_K SUBSET_ABOVE_SUBSET
const var"⫖"    =  @_K SUPERSET_ABOVE_SUPERSET
const var"⫗"    =  @_K SUPERSET_BESIDE_SUBSET
const var"⫘"    =  @_K SUPERSET_BESIDE_AND_JOINED_BY_DASH_WITH_SUBSET
const var"⫙"    =  @_K ELEMENT_OF_OPENING_DOWNWARDS
const var"⫷"    =  @_K TRIPLE_NESTED_LESS_THAN
const var"⫸"    =  @_K TRIPLE_NESTED_GREATER_THAN
const var"⫹"    =  @_K DOUBLE_LINE_SLANTED_LESS_THAN_OR_EQUAL_TO
const var"⫺"    =  @_K DOUBLE_LINE_SLANTED_GREATER_THAN_OR_EQUAL_TO
const var"⊢"    =  @_K RIGHT_TACK
const var"⊣"    =  @_K LEFT_TACK
const var"⟂"    =  @_K PERP
const END_COMPARISON = @_K end_comparison

# Level 7
const BEGIN_PIPE  =  @_K begin_pipe
const var"|>"     =  @_K LPIPE
const var"<|"     =  @_K RPIPE
const END_PIPE    =  @_K end_pipe

# Level 8
const BEGIN_COLON  =  @_K begin_colon
const var":"       =  @_K COLON
const var".."      =  @_K DDOT
const var"…"       =  @_K LDOTS
const var"⁝"       =  @_K TRICOLON
const var"⋮"       =  @_K VDOTS
const var"⋱"       =  @_K DDOTS
const var"⋰"       =  @_K ADOTS
const var"⋯"       =  @_K CDOTS
const END_COLON    =  @_K end_colon

# Level 9
const BEGIN_PLUS  =  @_K begin_plus
const var"$"   =  @_K EX_OR
const var"+"   =  @_K PLUS
const var"-"   =  @_K MINUS
const var"++"  =  @_K PLUSPLUS
const var"⊕"   =  @_K CIRCLED_PLUS
const var"⊖"   =  @_K CIRCLED_MINUS
const var"⊞"   =  @_K SQUARED_PLUS
const var"⊟"   =  @_K SQUARED_MINUS
const var"|"   =  @_K OR
const var"∪"   =  @_K UNION
const var"∨"   =  @_K LOGICAL_OR
const var"⊔"   =  @_K SQUARE_CUP
const var"±"   =  @_K PLUS_MINUS_SIGN
const var"∓"   =  @_K MINUS_OR_PLUS_SIGN
const var"∔"   =  @_K DOT_PLUS
const var"∸"   =  @_K DOT_MINUS
const var"≂"   =  @_K MINUS_TILDE
const var"≏"   =  @_K DIFFERENCE_BETWEEN
const var"⊎"   =  @_K MULTISET_UNION
const var"⊻"   =  @_K XOR
const var"⊽"   =  @_K NOR
const var"⋎"   =  @_K CURLY_LOGICAL_OR
const var"⋓"   =  @_K DOUBLE_UNION
const var"⧺"   =  @_K DOUBLE_PLUS
const var"⧻"   =  @_K TRIPLE_PLUS
const var"⨈"   =  @_K TWO_LOGICAL_OR_OPERATOR
const var"⨢"   =  @_K PLUS_SIGN_WITH_SMALL_CIRCLE_ABOVE
const var"⨣"   =  @_K PLUS_SIGN_WITH_CIRCUMFLEX_ACCENT_ABOVE
const var"⨤"   =  @_K PLUS_SIGN_WITH_TILDE_ABOVE
const var"⨥"   =  @_K PLUS_SIGN_WITH_DOT_BELOW
const var"⨦"   =  @_K PLUS_SIGN_WITH_TILDE_BELOW
const var"⨧"   =  @_K PLUS_SIGN_WITH_SUBSCRIPT_TWO
const var"⨨"   =  @_K PLUS_SIGN_WITH_BLACK_TRIANGLE
const var"⨩"   =  @_K MINUS_SIGN_WITH_COMMA_ABOVE
const var"⨪"   =  @_K MINUS_SIGN_WITH_DOT_BELOW
const var"⨫"   =  @_K MINUS_SIGN_WITH_FALLING_DOTS
const var"⨬"   =  @_K MINUS_SIGN_WITH_RISING_DOTS
const var"⨭"   =  @_K PLUS_SIGN_IN_LEFT_HALF_CIRCLE
const var"⨮"   =  @_K PLUS_SIGN_IN_RIGHT_HALF_CIRCLE
const var"⨹"   =  @_K PLUS_SIGN_IN_TRIANGLE
const var"⨺"   =  @_K MINUS_SIGN_IN_TRIANGLE
const var"⩁"   =  @_K UNION_WITH_MINUS_SIGN
const var"⩂"   =  @_K UNION_WITH_OVERBAR
const var"⩅"   =  @_K UNION_WITH_LOGICAL_OR
const var"⩊"   =  @_K UNION_BESIDE_AND_JOINED_WITH_UNION
const var"⩌"   =  @_K CLOSED_UNION_WITH_SERIFS
const var"⩏"   =  @_K DOUBLE_SQUARE_UNION
const var"⩐"   =  @_K CLOSED_UNION_WITH_SERIFS_AND_SMASH_PRODUCT
const var"⩒"   =  @_K LOGICAL_OR_WITH_DOT_ABOVE
const var"⩔"   =  @_K DOUBLE_LOGICAL_OR
const var"⩖"   =  @_K TWO_INTERSECTING_LOGICAL_OR
const var"⩗"   =  @_K SLOPING_LARGE_OR
const var"⩛"   =  @_K LOGICAL_OR_WITH_MIDDLE_STEM
const var"⩝"   =  @_K LOGICAL_OR_WITH_HORIZONTAL_DASH
const var"⩡"   =  @_K SMALL_VEE_WITH_UNDERBAR
const var"⩢"   =  @_K LOGICAL_OR_WITH_DOUBLE_OVERBAR
const var"⩣"   =  @_K LOGICAL_OR_WITH_DOUBLE_UNDERBAR
const var"¦"   =  @_K BROKEN_BAR
const END_PLUS    =  @_K end_plus

# Level 10
const BEGIN_BITSHIFTS  =  @_K begin_bitshifts
const var"<<"          =  @_K LBITSHIFT
const var">>"          =  @_K RBITSHIFT
const var">>>"         =  @_K UNSIGNED_BITSHIFT
const END_BITSHIFTS    =  @_K end_bitshifts

# Level 11
const BEGIN_TIMES  =  @_K begin_times
const var"*"       =  @_K STAR
const var"/"       =  @_K FWD_SLASH
const var"÷"       =  @_K DIVISION_SIGN
const var"%"       =  @_K REM
const var"⋅"       =  @_K UNICODE_DOT
const var"∘"       =  @_K RING_OPERATOR
const var"×"       =  @_K MULTIPLICATION_SIGN
const var"\\"      =  @_K BACKSLASH
const var"&"       =  @_K AND
const var"∩"       =  @_K INTERSECTION
const var"∧"       =  @_K LOGICAL_AND
const var"⊗"       =  @_K CIRCLED_TIMES
const var"⊘"       =  @_K CIRCLED_DIVISION_SLASH
const var"⊙"       =  @_K CIRCLED_DOT_OPERATOR
const var"⊚"       =  @_K CIRCLED_RING_OPERATOR
const var"⊛"       =  @_K CIRCLED_ASTERISK_OPERATOR
const var"⊠"       =  @_K SQUARED_TIMES
const var"⊡"       =  @_K SQUARED_DOT_OPERATOR
const var"⊓"       =  @_K SQUARE_CAP
const var"∗"       =  @_K ASTERISK_OPERATOR
const var"∙"       =  @_K BULLET_OPERATOR
const var"∤"       =  @_K DOES_NOT_DIVIDE
const var"⅋"       =  @_K TURNED_AMPERSAND
const var"≀"       =  @_K WREATH_PRODUCT
const var"⊼"       =  @_K NAND
const var"⋄"       =  @_K DIAMOND_OPERATOR
const var"⋆"       =  @_K STAR_OPERATOR
const var"⋇"       =  @_K DIVISION_TIMES
const var"⋉"       =  @_K LEFT_NORMAL_FACTOR_SEMIDIRECT_PRODUCT
const var"⋊"       =  @_K RIGHT_NORMAL_FACTOR_SEMIDIRECT_PRODUCT
const var"⋋"       =  @_K LEFT_SEMIDIRECT_PRODUCT
const var"⋌"       =  @_K RIGHT_SEMIDIRECT_PRODUCT
const var"⋏"       =  @_K CURLY_LOGICAL_AND
const var"⋒"       =  @_K DOUBLE_INTERSECTION
const var"⟑"       =  @_K AND_WITH_DOT
const var"⦸"       =  @_K CIRCLED_REVERSE_SOLIDUS
const var"⦼"       =  @_K CIRCLED_ANTICLOCKWISE_ROTATED_DIVISION_SIGN
const var"⦾"       =  @_K CIRCLED_WHITE_BULLET
const var"⦿"       =  @_K CIRCLED_BULLET
const var"⧶"       =  @_K SOLIDUS_WITH_OVERBAR
const var"⧷"       =  @_K REVERSE_SOLIDUS_WITH_HORIZONTAL_STROKE
const var"⨇"       =  @_K TWO_LOGICAL_AND_OPERATOR
const var"⨰"       =  @_K MULTIPLICATION_SIGN_WITH_DOT_ABOVE
const var"⨱"       =  @_K MULTIPLICATION_SIGN_WITH_UNDERBAR
const var"⨲"       =  @_K SEMIDIRECT_PRODUCT_WITH_BOTTOM_CLOSED
const var"⨳"       =  @_K SMASH_PRODUCT
const var"⨴"       =  @_K MULTIPLICATION_SIGN_IN_LEFT_HALF_CIRCLE
const var"⨵"       =  @_K MULTIPLICATION_SIGN_IN_RIGHT_HALF_CIRCLE
const var"⨶"       =  @_K CIRCLED_MULTIPLICATION_SIGN_WITH_CIRCUMFLEX_ACCENT
const var"⨷"       =  @_K MULTIPLICATION_SIGN_IN_DOUBLE_CIRCLE
const var"⨸"       =  @_K CIRCLED_DIVISION_SIGN
const var"⨻"       =  @_K MULTIPLICATION_SIGN_IN_TRIANGLE
const var"⨼"       =  @_K INTERIOR_PRODUCT
const var"⨽"       =  @_K RIGHTHAND_INTERIOR_PRODUCT
const var"⩀"       =  @_K INTERSECTION_WITH_DOT
const var"⩃"       =  @_K INTERSECTION_WITH_OVERBAR
const var"⩄"       =  @_K INTERSECTION_WITH_LOGICAL_AND
const var"⩋"       =  @_K INTERSECTION_BESIDE_AND_JOINED_WITH_INTERSECTION
const var"⩍"       =  @_K CLOSED_INTERSECTION_WITH_SERIFS
const var"⩎"       =  @_K DOUBLE_SQUARE_INTERSECTION
const var"⩑"       =  @_K LOGICAL_AND_WITH_DOT_ABOVE
const var"⩓"       =  @_K DOUBLE_LOGICAL_AND
const var"⩕"       =  @_K TWO_INTERSECTING_LOGICAL_AND
const var"⩘"       =  @_K SLOPING_LARGE_AND
const var"⩚"       =  @_K LOGICAL_AND_WITH_MIDDLE_STEM
const var"⩜"       =  @_K LOGICAL_AND_WITH_HORIZONTAL_DASH
const var"⩞"       =  @_K LOGICAL_AND_WITH_DOUBLE_OVERBAR
const var"⩟"       =  @_K LOGICAL_AND_WITH_UNDERBAR
const var"⩠"       =  @_K LOGICAL_AND_WITH_DOUBLE_UNDERBAR
const var"⫛"       =  @_K TRANSVERSAL_INTERSECTION
const var"⊍"       =  @_K MULTISET_MULTIPLICATION
const var"▷"       =  @_K WHITE_RIGHT_POINTING_TRIANGLE
const var"⨝"       =  @_K JOIN
const var"⟕"       =  @_K LEFT_OUTER_JOIN
const var"⟖"       =  @_K RIGHT_OUTER_JOIN
const var"⟗"       =  @_K FULL_OUTER_JOIN
const var"⌿"       =  @_K NOT_SLASH
const var"⨟"       =  @_K BB_SEMI
const END_TIMES    =  @_K end_times

# Level 12
const BEGIN_RATIONAL  =  @_K begin_rational
const var"//"         =  @_K FWDFWD_SLASH
const END_RATIONAL    =  @_K end_rational

# Level 13
const BEGIN_POWER  =  @_K begin_power
const var"^"       =  @_K CIRCUMFLEX_ACCENT
const var"↑"       =  @_K UPWARDS_ARROW
const var"↓"       =  @_K DOWNWARDS_ARROW
const var"⇵"       =  @_K DOWNWARDS_ARROW_LEFTWARDS_OF_UPWARDS_ARROW
const var"⟰"       =  @_K UPWARDS_QUADRUPLE_ARROW
const var"⟱"       =  @_K DOWNWARDS_QUADRUPLE_ARROW
const var"⤈"       =  @_K DOWNWARDS_ARROW_WITH_HORIZONTAL_STROKE
const var"⤉"       =  @_K UPWARDS_ARROW_WITH_HORIZONTAL_STROKE
const var"⤊"       =  @_K UPWARDS_TRIPLE_ARROW
const var"⤋"       =  @_K DOWNWARDS_TRIPLE_ARROW
const var"⤒"       =  @_K UPWARDS_ARROW_TO_BAR
const var"⤓"       =  @_K DOWNWARDS_ARROW_TO_BAR
const var"⥉"       =  @_K UPWARDS_TWO_HEADED_ARROW_FROM_SMALL_CIRCLE
const var"⥌"       =  @_K UP_BARB_RIGHT_DOWN_BARB_LEFT_HARPOON
const var"⥍"       =  @_K UP_BARB_LEFT_DOWN_BARB_RIGHT_HARPOON
const var"⥏"       =  @_K UP_BARB_RIGHT_DOWN_BARB_RIGHT_HARPOON
const var"⥑"       =  @_K UP_BARB_LEFT_DOWN_BARB_LEFT_HARPOON
const var"⥔"       =  @_K UPWARDS_HARPOON_WITH_BARB_RIGHT_TO_BAR
const var"⥕"       =  @_K DOWNWARDS_HARPOON_WITH_BARB_RIGHT_TO_BAR
const var"⥘"       =  @_K UPWARDS_HARPOON_WITH_BARB_LEFT_TO_BAR
const var"⥙"       =  @_K DOWNWARDS_HARPOON_WITH_BARB_LEFT_TO_BAR
const var"⥜"       =  @_K UPWARDS_HARPOON_WITH_BARB_RIGHT_FROM_BAR
const var"⥝"       =  @_K DOWNWARDS_HARPOON_WITH_BARB_RIGHT_FROM_BAR
const var"⥠"       =  @_K UPWARDS_HARPOON_WITH_BARB_LEFT_FROM_BAR
const var"⥡"       =  @_K DOWNWARDS_HARPOON_WITH_BARB_LEFT_FROM_BAR
const var"⥣"       =  @_K UPWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_UPWARDS_HARPOON_WITH_BARB_RIGHT
const var"⥥"       =  @_K DOWNWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_DOWNWARDS_HARPOON_WITH_BARB_RIGHT
const var"⥮"       =  @_K UPWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_DOWNWARDS_HARPOON_WITH_BARB_RIGHT
const var"⥯"       =  @_K DOWNWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_UPWARDS_HARPOON_WITH_BARB_RIGHT
const var"￪"       =  @_K HALFWIDTH_UPWARDS_ARROW
const var"￬"       =  @_K HALFWIDTH_DOWNWARDS_ARROW
const END_POWER    =  @_K end_power

# Level 14
const BEGIN_DECL  =  @_K begin_decl
const var"::"     =  @_K DECLARATION
const END_DECL    =  @_K end_decl

# Level 15
const BEGIN_WHERE  =  @_K begin_where
const var"where"   =  @_K WHERE
const END_WHERE    =  @_K end_where

# Level 16
const BEGIN_DOT  =  @_K begin_dot
const var"."     =  @_K DOT
const END_DOT    =  @_K end_dot

const var"!"   =  @_K NOT
const var"'"   =  @_K PRIME
const var".'"  =  @_K TRANSPOSE
const var"->"  =  @_K ANON_FUNC

const BEGIN_UNICODE_OPS  =  @_K begin_unicode_ops
const var"¬"             =  @_K NOT_SIGN
const var"√"             =  @_K SQUARE_ROOT
const var"∛"             =  @_K CUBE_ROOT
const var"∜"             =  @_K QUAD_ROOT
const END_UNICODE_OPS    =  @_K end_unicode_ops

const END_OPS = @_K end_ops

# Cute synonyms
const var" "      =  @_K WHITESPACE
const var"\n"      =  @_K NEWLINE_WS

# Our custom syntax tokens

const BEGIN_SYNTAX_KINDS = @_K begin_syntax_kinds
const toplevel = @_K TOPLEVEL
const call     = @_K CALL
const block    = @_K BLOCK
const END_SYNTAX_KINDS = @_K end_syntax_kinds

end

