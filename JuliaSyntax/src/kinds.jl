# Definition of Kind type - mapping from token string identifiers to
# enumeration values as used in @K_str
const _kind_names =
[
    "None"         # Placeholder; never emitted by lexer
    "EndMarker"    # EOF
    "Comment"
    "Whitespace"
    "NewlineWs"    # newline-containing whitespace
    "Identifier"
    "@"
    ","
    ";"

    "BEGIN_ERRORS"
        # Tokenization errors
        "ErrorEofMultiComment"
        "ErrorInvalidNumericConstant"
        "ErrorHexFloatMustContainP"
        "ErrorAmbiguousNumericConstant"
        "ErrorAmbiguousNumericDotMultiply"
        "ErrorInvalidInterpolationTerminator"
        "ErrorNumericOverflow"
        "ErrorInvalidEscapeSequence"
        "ErrorOverLongCharacter"
        "ErrorInvalidUTF8"
        "ErrorInvisibleChar"
        "ErrorUnknownCharacter"
        "ErrorBidiFormatting"
        # Generic error
        "error"
    "END_ERRORS"

    "BEGIN_KEYWORDS"
        "baremodule"
        "begin"
        "break"
        "const"
        "continue"
        "do"
        "export"
        "for"
        "function"
        "global"
        "if"
        "import"
        "let"
        "local"
        "macro"
        "module"
        "quote"
        "return"
        "struct"
        "try"
        "using"
        "while"
        "BEGIN_BLOCK_CONTINUATION_KEYWORDS"
            "catch"
            "finally"
            "else"
            "elseif"
            "end"
        "END_BLOCK_CONTINUATION_KEYWORDS"
        "BEGIN_CONTEXTUAL_KEYWORDS"
            # contextual keywords
            "abstract"
            "as"
            "doc"
            "mutable"
            "outer"
            "primitive"
            "public"
            "type"
            "var"
        "END_CONTEXTUAL_KEYWORDS"
    "END_KEYWORDS"

    "BEGIN_LITERAL"
        "Integer"
        "BinInt"
        "HexInt"
        "OctInt"
        "Float"
        "Float32"
        "String"
        "Char"
        "CmdString"
        "true"
        "false"
    "END_LITERAL"

    "BEGIN_DELIMITERS"
        "["
        "]"
        "{"
        "}"
        "("
        ")"
        "\""
        "\"\"\""
        "`"
        "```"
    "END_DELIMITERS"

    "BEGIN_OPS"
    "ErrorInvalidOperator"
    "Error**"

    "..."

    # Level 1
    "BEGIN_ASSIGNMENTS"
        "="
        "+="
        "-="   # Also used for "−="
        "*="
        "/="
        "//="
        "|="
        "^="
        "÷="
        "%="
        "<<="
        ">>="
        ">>>="
        "\\="
        "&="
        ":="
        "~"
        "\$="
        "⊻="
        "≔"
        "⩴"
        "≕"
    "END_ASSIGNMENTS"

    "BEGIN_PAIRARROW"
        "=>"
    "END_PAIRARROW"

    # Level 2
    "BEGIN_CONDITIONAL"
    "?"
    "END_CONDITIONAL"

    # Level 3
    "BEGIN_ARROW"
        "-->"
        "<--"
        "<-->"
        "←"
        "→"
        "↔"
        "↚"
        "↛"
        "↞"
        "↠"
        "↢"
        "↣"
        "↤"
        "↦"
        "↮"
        "⇎"
        "⇍"
        "⇏"
        "⇐"
        "⇒"
        "⇔"
        "⇴"
        "⇶"
        "⇷"
        "⇸"
        "⇹"
        "⇺"
        "⇻"
        "⇼"
        "⇽"
        "⇾"
        "⇿"
        "⟵"
        "⟶"
        "⟷"
        "⟹"
        "⟺"
        "⟻"
        "⟼"
        "⟽"
        "⟾"
        "⟿"
        "⤀"
        "⤁"
        "⤂"
        "⤃"
        "⤄"
        "⤅"
        "⤆"
        "⤇"
        "⤌"
        "⤍"
        "⤎"
        "⤏"
        "⤐"
        "⤑"
        "⤔"
        "⤕"
        "⤖"
        "⤗"
        "⤘"
        "⤝"
        "⤞"
        "⤟"
        "⤠"
        "⥄"
        "⥅"
        "⥆"
        "⥇"
        "⥈"
        "⥊"
        "⥋"
        "⥎"
        "⥐"
        "⥒"
        "⥓"
        "⥖"
        "⥗"
        "⥚"
        "⥛"
        "⥞"
        "⥟"
        "⥢"
        "⥤"
        "⥦"
        "⥧"
        "⥨"
        "⥩"
        "⥪"
        "⥫"
        "⥬"
        "⥭"
        "⥰"
        "⧴"
        "⬱"
        "⬰"
        "⬲"
        "⬳"
        "⬴"
        "⬵"
        "⬶"
        "⬷"
        "⬸"
        "⬹"
        "⬺"
        "⬻"
        "⬼"
        "⬽"
        "⬾"
        "⬿"
        "⭀"
        "⭁"
        "⭂"
        "⭃"
        "⥷"
        "⭄"
        "⥺"
        "⭇"
        "⭈"
        "⭉"
        "⭊"
        "⭋"
        "⭌"
        "￩"
        "￫"
        "⇜"
        "⇝"
        "↜"
        "↝"
        "↩"
        "↪"
        "↫"
        "↬"
        "↼"
        "↽"
        "⇀"
        "⇁"
        "⇄"
        "⇆"
        "⇇"
        "⇉"
        "⇋"
        "⇌"
        "⇚"
        "⇛"
        "⇠"
        "⇢"
        "↷"
        "↶"
        "↺"
        "↻"
    "END_ARROW"

    # Level 4
    "BEGIN_LAZYOR"
        "||"
    "END_LAZYOR"

    # Level 5
    "BEGIN_LAZYAND"
        "&&"
    "END_LAZYAND"

    # Level 6
    "BEGIN_COMPARISON"
        "<:"
        ">:"
        ">"
        "<"
        ">="
        "≥"
        "<="
        "≤"
        "=="
        "==="
        "≡"
        "!="
        "≠"
        "!=="
        "≢"
        "∈"
        "in"
        "isa"
        "∉"
        "∋"
        "∌"
        "⊆"
        "⊈"
        "⊂"
        "⊄"
        "⊊"
        "∝"
        "∊"
        "∍"
        "∥"
        "∦"
        "∷"
        "∺"
        "∻"
        "∽"
        "∾"
        "≁"
        "≃"
        "≂"
        "≄"
        "≅"
        "≆"
        "≇"
        "≈"
        "≉"
        "≊"
        "≋"
        "≌"
        "≍"
        "≎"
        "≐"
        "≑"
        "≒"
        "≓"
        "≖"
        "≗"
        "≘"
        "≙"
        "≚"
        "≛"
        "≜"
        "≝"
        "≞"
        "≟"
        "≣"
        "≦"
        "≧"
        "≨"
        "≩"
        "≪"
        "≫"
        "≬"
        "≭"
        "≮"
        "≯"
        "≰"
        "≱"
        "≲"
        "≳"
        "≴"
        "≵"
        "≶"
        "≷"
        "≸"
        "≹"
        "≺"
        "≻"
        "≼"
        "≽"
        "≾"
        "≿"
        "⊀"
        "⊁"
        "⊃"
        "⊅"
        "⊇"
        "⊉"
        "⊋"
        "⊏"
        "⊐"
        "⊑"
        "⊒"
        "⊜"
        "⊩"
        "⊬"
        "⊮"
        "⊰"
        "⊱"
        "⊲"
        "⊳"
        "⊴"
        "⊵"
        "⊶"
        "⊷"
        "⋍"
        "⋐"
        "⋑"
        "⋕"
        "⋖"
        "⋗"
        "⋘"
        "⋙"
        "⋚"
        "⋛"
        "⋜"
        "⋝"
        "⋞"
        "⋟"
        "⋠"
        "⋡"
        "⋢"
        "⋣"
        "⋤"
        "⋥"
        "⋦"
        "⋧"
        "⋨"
        "⋩"
        "⋪"
        "⋫"
        "⋬"
        "⋭"
        "⋲"
        "⋳"
        "⋴"
        "⋵"
        "⋶"
        "⋷"
        "⋸"
        "⋹"
        "⋺"
        "⋻"
        "⋼"
        "⋽"
        "⋾"
        "⋿"
        "⟈"
        "⟉"
        "⟒"
        "⦷"
        "⧀"
        "⧁"
        "⧡"
        "⧣"
        "⧤"
        "⧥"
        "⩦"
        "⩧"
        "⩪"
        "⩫"
        "⩬"
        "⩭"
        "⩮"
        "⩯"
        "⩰"
        "⩱"
        "⩲"
        "⩳"
        "⩵"
        "⩶"
        "⩷"
        "⩸"
        "⩹"
        "⩺"
        "⩻"
        "⩼"
        "⩽"
        "⩾"
        "⩿"
        "⪀"
        "⪁"
        "⪂"
        "⪃"
        "⪄"
        "⪅"
        "⪆"
        "⪇"
        "⪈"
        "⪉"
        "⪊"
        "⪋"
        "⪌"
        "⪍"
        "⪎"
        "⪏"
        "⪐"
        "⪑"
        "⪒"
        "⪓"
        "⪔"
        "⪕"
        "⪖"
        "⪗"
        "⪘"
        "⪙"
        "⪚"
        "⪛"
        "⪜"
        "⪝"
        "⪞"
        "⪟"
        "⪠"
        "⪡"
        "⪢"
        "⪣"
        "⪤"
        "⪥"
        "⪦"
        "⪧"
        "⪨"
        "⪩"
        "⪪"
        "⪫"
        "⪬"
        "⪭"
        "⪮"
        "⪯"
        "⪰"
        "⪱"
        "⪲"
        "⪳"
        "⪴"
        "⪵"
        "⪶"
        "⪷"
        "⪸"
        "⪹"
        "⪺"
        "⪻"
        "⪼"
        "⪽"
        "⪾"
        "⪿"
        "⫀"
        "⫁"
        "⫂"
        "⫃"
        "⫄"
        "⫅"
        "⫆"
        "⫇"
        "⫈"
        "⫉"
        "⫊"
        "⫋"
        "⫌"
        "⫍"
        "⫎"
        "⫏"
        "⫐"
        "⫑"
        "⫒"
        "⫓"
        "⫔"
        "⫕"
        "⫖"
        "⫗"
        "⫘"
        "⫙"
        "⫷"
        "⫸"
        "⫹"
        "⫺"
        "⊢"
        "⊣"
        "⟂"
        # ⫪,⫫ see https://github.com/JuliaLang/julia/issues/39350
        "⫪"
        "⫫"
    "END_COMPARISON"

    # Level 7
    "BEGIN_PIPE"
        "<|"
        "|>"
    "END_PIPE"

    # Level 8
    "BEGIN_COLON"
        ":"
        ".."
        "…"
        "⁝"
        "⋮"
        "⋱"
        "⋰"
        "⋯"
    "END_COLON"

    # Level 9
    "BEGIN_PLUS"
        "\$"
        "+"
        "-" # also used for "−"
        "++"
        "⊕"
        "⊖"
        "⊞"
        "⊟"
        "|"
        "∪"
        "∨"
        "⊔"
        "±"
        "∓"
        "∔"
        "∸"
        "≏"
        "⊎"
        "⊻"
        "⊽"
        "⋎"
        "⋓"
        "⟇"
        "⧺"
        "⧻"
        "⨈"
        "⨢"
        "⨣"
        "⨤"
        "⨥"
        "⨦"
        "⨧"
        "⨨"
        "⨩"
        "⨪"
        "⨫"
        "⨬"
        "⨭"
        "⨮"
        "⨹"
        "⨺"
        "⩁"
        "⩂"
        "⩅"
        "⩊"
        "⩌"
        "⩏"
        "⩐"
        "⩒"
        "⩔"
        "⩖"
        "⩗"
        "⩛"
        "⩝"
        "⩡"
        "⩢"
        "⩣"
        "¦"
    "END_PLUS"

    # Level 10
    "BEGIN_TIMES"
        "*"
        "/"
        "÷"
        "%"
        "⋅" # also used for lookalikes "·" and "·"
        "∘"
        "×"
        "\\"
        "&"
        "∩"
        "∧"
        "⊗"
        "⊘"
        "⊙"
        "⊚"
        "⊛"
        "⊠"
        "⊡"
        "⊓"
        "∗"
        "∙"
        "∤"
        "⅋"
        "≀"
        "⊼"
        "⋄"
        "⋆"
        "⋇"
        "⋉"
        "⋊"
        "⋋"
        "⋌"
        "⋏"
        "⋒"
        "⟑"
        "⦸"
        "⦼"
        "⦾"
        "⦿"
        "⧶"
        "⧷"
        "⨇"
        "⨰"
        "⨱"
        "⨲"
        "⨳"
        "⨴"
        "⨵"
        "⨶"
        "⨷"
        "⨸"
        "⨻"
        "⨼"
        "⨽"
        "⩀"
        "⩃"
        "⩄"
        "⩋"
        "⩍"
        "⩎"
        "⩑"
        "⩓"
        "⩕"
        "⩘"
        "⩚"
        "⩜"
        "⩞"
        "⩟"
        "⩠"
        "⫛"
        "⊍"
        "▷"
        "⨝"
        "⟕"
        "⟖"
        "⟗"
        "⌿"
        "⨟"
    "END_TIMES"

    # Level 11
    "BEGIN_RATIONAL"
        "//"
    "END_RATIONAL"

    # Level 12
    "BEGIN_BITSHIFTS"
        "<<"
        ">>"
        ">>>"
    "END_BITSHIFTS"

    # Level 13
    "BEGIN_POWER"
        "^"
        "↑"
        "↓"
        "⇵"
        "⟰"
        "⟱"
        "⤈"
        "⤉"
        "⤊"
        "⤋"
        "⤒"
        "⤓"
        "⥉"
        "⥌"
        "⥍"
        "⥏"
        "⥑"
        "⥔"
        "⥕"
        "⥘"
        "⥙"
        "⥜"
        "⥝"
        "⥠"
        "⥡"
        "⥣"
        "⥥"
        "⥮"
        "⥯"
        "￪"
        "￬"
    "END_POWER"

    # Level 14
    "BEGIN_DECL"
        "::"
    "END_DECL"

    # Level 15
    "BEGIN_WHERE"
        "where"
    "END_WHERE"

    # Level 16
    "BEGIN_DOT"
        "."
    "END_DOT"

    "!"
    "'"
    ".'"
    "->"

    "BEGIN_UNICODE_OPS"
        "¬"
        "√"
        "∛"
        "∜"
    "END_UNICODE_OPS"
    "END_OPS"

    # The following kinds are emitted by the parser. There's two types of these:

    # 1. Implied tokens which have a position but might have zero width in the
    #    source text.
    #
    # In some cases we want to generate parse tree nodes in a standard form,
    # but some of the leaf tokens are implied rather than existing in the
    # source text, or the lexed tokens need to be re-kinded to represent
    # special forms which only the parser can infer. These are "parser tokens".
    #
    # Some examples:
    #
    # Docstrings - the macro name is invisible
    #   "doc" foo() = 1   ==>  (macrocall (core @doc) . (= (call foo) 1))
    #
    # String macros - the macro name does not appear in the source text, so we
    # need a special kind of token to imply it.
    #
    # In these cases, we use some special kinds which can be emitted as zero
    # width tokens to keep the parse tree more uniform.
    "BEGIN_PARSER_TOKENS"

        "TOMBSTONE" # Empty placeholder for kind to be filled later

        # Macro names are modelled as a special kind of identifier because the
        # @ may not be attached to the macro name in the source (or may not be
        # associated with a token at all in the case of implied macro calls
        # like CORE_DOC_MACRO_NAME)
        "BEGIN_MACRO_NAMES"
            "MacroName"
            "StringMacroName"
            "CmdMacroName"
            "core_@cmd"
            "core_@int128_str"
            "core_@uint128_str"
            "core_@big_str"
        "END_MACRO_NAMES"
    "END_PARSER_TOKENS"

    # 2. Nonterminals which are exposed in the AST, but where the surface
    #    syntax doesn't have a token corresponding to the node type.
    "BEGIN_SYNTAX_KINDS"
        "block"
        "call"
        "dotcall"
        "comparison"
        "curly"
        "inert"          # QuoteNode; not quasiquote
        "juxtapose"      # Numeric juxtaposition like 2x
        "string"         # A string interior node (possibly containing interpolations)
        "cmdstring"      # A cmd string node (containing delimiters plus string)
        "char"           # A char string node (containing delims + char data)
        "macrocall"
        "parameters"     # the list after ; in f(; a=1)
        "toplevel"
        "tuple"
        "ref"
        "vect"
        "parens"
        "importpath"
        # Concatenation syntax
        "braces"
        "bracescat"
        "hcat"
        "vcat"
        "ncat"
        "typed_hcat"
        "typed_vcat"
        "typed_ncat"
        "row"
        "nrow"
        # Comprehensions
        "generator"
        "filter"
        "cartesian_iterator"
        "comprehension"
        "typed_comprehension"
        # Container for a single statement/atom plus any trivia and errors
        "wrapper"
    "END_SYNTAX_KINDS"
]

"""
    K"name"
    Kind(namestr)

`Kind` is a type tag for specifying the type of tokens and interior nodes of
a syntax tree. Abstractly, this tag is used to define our own *sum types* for
syntax tree nodes. We do this explicitly outside the Julia type system because
(a) Julia doesn't have sum types and (b) we want concrete data structures which
are unityped from the Julia compiler's point of view, for efficiency.

Naming rules:
* Kinds which correspond to exactly one textural form are represented with that
  text. This includes keywords like K"for" and operators like K"*".
* Kinds which represent many textural forms have UpperCamelCase names. This
  includes kinds like K"Identifier" and K"Comment".
* Kinds which exist merely as delimiters are all uppercase
"""
primitive type Kind 16 end

# The implementation of Kind here is basically similar to @enum. However we use
# the K_str macro to self-name these kinds with their literal representation,
# rather than needing to invent a new name for each.

let kind_int_type = :UInt16
    # Preprocess _kind_names to conflate category markers with the first/last
    # in the category.
    kindstr_to_int = Dict{String,UInt16}()
    i = 1
    while i <= length(_kind_names)
        kn = _kind_names[i]
        kind_int = i-1
        if startswith(kn, "BEGIN_")
            deleteat!(_kind_names, i)
        elseif startswith(kn, "END_")
            kind_int = i-2
            deleteat!(_kind_names, i)
        else
            i += 1
        end
        push!(kindstr_to_int, kn=>kind_int)
    end

    max_kind_int = length(_kind_names)-1

    @eval begin
        function Kind(x::Integer)
            if x < 0 || x > $max_kind_int
                throw(ArgumentError("Kind out of range: $x"))
            end
            return Base.bitcast(Kind, convert($kind_int_type, x))
        end

        Base.convert(::Type{String}, k::Kind) = _kind_names[1 + reinterpret($kind_int_type, k)]

        let kindstr_to_int=$kindstr_to_int
            function Base.convert(::Type{Kind}, s::AbstractString)
                i = get(kindstr_to_int, s) do
                    error("unknown Kind name $(repr(s))")
                end
                Kind(i)
            end
        end

        Base.string(x::Kind) = convert(String, x)
        Base.print(io::IO, x::Kind) = print(io, convert(String, x))

        Base.typemin(::Type{Kind}) = Kind(0)
        Base.typemax(::Type{Kind}) = Kind($max_kind_int)

        Base.:<(x::Kind, y::Kind) = reinterpret($kind_int_type, x) < reinterpret($kind_int_type, y)

        Base.instances(::Type{Kind}) = (Kind(i) for i in reinterpret($kind_int_type, typemin(Kind)):reinterpret($kind_int_type, typemax(Kind)))
    end
end

function Base.show(io::IO, k::Kind)
    print(io, "K\"$(convert(String, k))\"")
end

#-------------------------------------------------------------------------------

"""
    K"s"

The kind of a token or AST internal node with string "s".

For example
* K")" is the kind of the right parenthesis token
* K"block" is the kind of a block of code (eg, statements within a begin-end).
"""
macro K_str(s)
    convert(Kind, s)
end

"""
A set of kinds which can be used with the `in` operator.  For example

    k in KSet"+ - *"
"""
macro KSet_str(str)
    kinds = [convert(Kind, s) for s in split(str)]

    quote
        ($(kinds...),)
    end
end

"""
    kind(x)

Return the `Kind` of `x`.
"""
kind(k::Kind) = k

#-------------------------------------------------------------------------------
const _nonunique_kind_names = Set([
    K"Comment"
    K"Whitespace"
    K"NewlineWs"
    K"Identifier"

    K"ErrorEofMultiComment"
    K"ErrorInvalidNumericConstant"
    K"ErrorHexFloatMustContainP"
    K"ErrorAmbiguousNumericConstant"
    K"ErrorAmbiguousNumericDotMultiply"
    K"ErrorInvalidInterpolationTerminator"
    K"ErrorNumericOverflow"
    K"ErrorInvalidEscapeSequence"
    K"ErrorOverLongCharacter"
    K"ErrorInvalidUTF8"
    K"ErrorInvisibleChar"
    K"ErrorUnknownCharacter"
    K"ErrorBidiFormatting"
    K"ErrorInvalidOperator"

    K"Integer"
    K"BinInt"
    K"HexInt"
    K"OctInt"
    K"Float"
    K"Float32"
    K"String"
    K"Char"
    K"CmdString"

    K"MacroName"
    K"StringMacroName"
    K"CmdMacroName"
])

"""
Return the string representation of a token kind, or `nothing` if the kind
represents a class of tokens like K"Identifier".

When `unique=true` only return a string when the kind uniquely defines the
corresponding input token, otherwise return `nothing`.  When `unique=false`,
return the name of the kind.

TODO: Replace `untokenize()` with `Base.string()`?
"""
function untokenize(k::Kind; unique=true)
    if unique && k in _nonunique_kind_names
        return nothing
    else
        return convert(String, k)
    end
end

# Error kind => description
const _token_error_descriptions = Dict{Kind, String}(
    K"ErrorEofMultiComment" => "unterminated multi-line comment #= ... =#",
    K"ErrorInvalidNumericConstant" => "invalid numeric constant",
    K"ErrorHexFloatMustContainP" => "hex float literal must contain `p` or `P`",
    K"ErrorAmbiguousNumericConstant" => "ambiguous `.` syntax; add whitespace to clarify (eg `1.+2` might be `1.0+2` or `1 .+ 2`)",
    K"ErrorAmbiguousNumericDotMultiply" => "numeric constant cannot be implicitly multiplied because it ends with `.`",
    K"ErrorInvalidInterpolationTerminator" => "interpolated variable ends with invalid character; use `\$(...)` instead",
    K"ErrorNumericOverflow"=>"overflow in numeric literal",
    K"ErrorInvalidEscapeSequence"=>"invalid string escape sequence",
    K"ErrorOverLongCharacter"=>"character literal contains multiple characters",
    K"ErrorInvalidUTF8"=>"invalid UTF-8 sequence",
    K"ErrorInvisibleChar"=>"invisible character",
    K"ErrorUnknownCharacter"=>"unknown unicode character",
    K"ErrorBidiFormatting"=>"unbalanced bidirectional unicode formatting",
    K"ErrorInvalidOperator" => "invalid operator",
    K"Error**" => "use `x^y` instead of `x**y` for exponentiation, and `x...` instead of `**x` for splatting",
    K"error" => "unknown error token",
)

#-------------------------------------------------------------------------------
# Predicates
is_contextual_keyword(k::Kind) = K"BEGIN_CONTEXTUAL_KEYWORDS" <= k <= K"END_CONTEXTUAL_KEYWORDS"
is_error(k::Kind) = K"BEGIN_ERRORS" <= k <= K"END_ERRORS" || k == K"ErrorInvalidOperator" || k == K"Error**"
is_keyword(k::Kind) = K"BEGIN_KEYWORDS" <= k <= K"END_KEYWORDS"
is_block_continuation_keyword(k::Kind) = K"BEGIN_BLOCK_CONTINUATION_KEYWORDS" <= k <= K"END_BLOCK_CONTINUATION_KEYWORDS"
is_literal(k::Kind) = K"BEGIN_LITERAL" <= k <= K"END_LITERAL"
is_operator(k::Kind) = K"BEGIN_OPS" <= k <= K"END_OPS"
is_word_operator(k::Kind) = (k == K"in" || k == K"isa" || k == K"where")

is_contextual_keyword(k) = is_contextual_keyword(kind(k))
is_error(k) = is_error(kind(k))
is_keyword(k) = is_keyword(kind(k))
is_literal(k) = is_literal(kind(k))
is_operator(k) = is_operator(kind(k))
is_word_operator(k) = is_word_operator(kind(k))


# Predicates for operator precedence
# FIXME: Review how precedence depends on dottedness, eg
# https://github.com/JuliaLang/julia/pull/36725
is_prec_assignment(x)  = K"BEGIN_ASSIGNMENTS" <= kind(x) <= K"END_ASSIGNMENTS"
is_prec_pair(x)        = K"BEGIN_PAIRARROW"   <= kind(x) <= K"END_PAIRARROW"
is_prec_conditional(x) = K"BEGIN_CONDITIONAL" <= kind(x) <= K"END_CONDITIONAL"
is_prec_arrow(x)       = K"BEGIN_ARROW"       <= kind(x) <= K"END_ARROW"
is_prec_lazy_or(x)     = K"BEGIN_LAZYOR"      <= kind(x) <= K"END_LAZYOR"
is_prec_lazy_and(x)    = K"BEGIN_LAZYAND"     <= kind(x) <= K"END_LAZYAND"
is_prec_comparison(x)  = K"BEGIN_COMPARISON"  <= kind(x) <= K"END_COMPARISON"
is_prec_pipe(x)        = K"BEGIN_PIPE"        <= kind(x) <= K"END_PIPE"
is_prec_colon(x)       = K"BEGIN_COLON"       <= kind(x) <= K"END_COLON"
is_prec_plus(x)        = K"BEGIN_PLUS"        <= kind(x) <= K"END_PLUS"
is_prec_bitshift(x)    = K"BEGIN_BITSHIFTS"   <= kind(x) <= K"END_BITSHIFTS"
is_prec_times(x)       = K"BEGIN_TIMES"       <= kind(x) <= K"END_TIMES"
is_prec_rational(x)    = K"BEGIN_RATIONAL"    <= kind(x) <= K"END_RATIONAL"
is_prec_power(x)       = K"BEGIN_POWER"       <= kind(x) <= K"END_POWER"
is_prec_decl(x)        = K"BEGIN_DECL"        <= kind(x) <= K"END_DECL"
is_prec_where(x)       = K"BEGIN_WHERE"       <= kind(x) <= K"END_WHERE"
is_prec_dot(x)         = K"BEGIN_DOT"         <= kind(x) <= K"END_DOT"
is_prec_unicode_ops(x) = K"BEGIN_UNICODE_OPS" <= kind(x) <= K"END_UNICODE_OPS"
is_prec_pipe_lt(x)     = kind(x) == K"<|"
is_prec_pipe_gt(x)     = kind(x) == K"|>"
is_syntax_kind(x)      = K"BEGIN_SYNTAX_KINDS"<= kind(x) <= K"END_SYNTAX_KINDS"
is_macro_name(x)       = K"BEGIN_MACRO_NAMES" <= kind(x) <= K"END_MACRO_NAMES"

function is_number(x)
    kind(x) in (K"Integer", K"BinInt", K"HexInt", K"OctInt", K"Float", K"Float32")
end

function is_string_delim(x)
    kind(x) in (K"\"", K"\"\"\"")
end

function is_radical_op(x)
    kind(x) in (K"√", K"∛", K"∜")
end

"""
Return true if `x` has whitespace or comment kind
"""
function is_whitespace(x)
    k = kind(x)
    return k == K"Whitespace" || k == K"NewlineWs" || k == K"Comment"
end
