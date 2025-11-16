# Definition of Kind type - mapping from token string identifiers to
# enumeration values as used in @K_str

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

const _kind_str_to_int = Dict{String,UInt16}()
const _kind_int_to_str = Dict{UInt16,String}()
const _kind_modules = Dict{Int,Union{Symbol,Module}}(
    0=>nameof(@__MODULE__),
    1=>:JuliaLowering,
    2=>:JuliaSyntaxFormatter
)
# Number of bits reserved for kind id's belonging to a single module
const _kind_nbits = 10
const _kind_module_id_max = typemax(UInt16) >> _kind_nbits

function Kind(x::Integer)
    if x < 0 || x > typemax(UInt16)
        throw(ArgumentError("Kind out of range: $x"))
    end
    return Base.bitcast(Kind, convert(UInt16, x))
end

function Kind(s::AbstractString)
    i = get(_kind_str_to_int, s) do
        error("unknown Kind name $(repr(s))")
    end
    Kind(i)
end

Base.string(x::Kind) = get(_kind_int_to_str, reinterpret(UInt16, x), "<error: unknown kind>")
Base.print(io::IO, x::Kind) = print(io, string(x))

Base.isless(x::Kind, y::Kind) = reinterpret(UInt16, x) < reinterpret(UInt16, y)

function Base.show(io::IO, k::Kind)
    print(io, "K\"", k, "\"")
end

# Save the string representation rather than the bit pattern so that kinds
# can be serialized and deserialized across different JuliaSyntax versions.
function Base.write(io::IO, k::Kind)
    str = string(k)
    write(io, UInt8(sizeof(str))) + write(io, str)
end
function Base.read(io::IO, ::Type{Kind})
    len = read(io, UInt8)
    str = String(read(io, len))
    Kind(str)
end

function Base.parentmodule(k::Kind)
    mod_id = reinterpret(UInt16, k) >> _kind_nbits
    _kind_modules[mod_id]::Module
end

function _register_kinds!(kind_modules, int_to_kindstr, kind_str_to_int, mod, module_id, names)
    if module_id > _kind_module_id_max
        error("Kind module id $module_id is out of range")
    elseif length(names) >= 1 << _kind_nbits
        error("Too many kind names")
    elseif !haskey(kind_modules, module_id)
        kind_modules[module_id] = mod
    else
        m = kind_modules[module_id]
        if m == nameof(mod)
            # Ok: known kind module, but not loaded until now
            kind_modules[module_id] = mod
        elseif m == mod
            existing_kinds = Union{Nothing, Kind}[(i = get(kind_str_to_int, n, nothing);
                               isnothing(i) ? nothing : Kind(i)) for n in names]
            if any(isnothing, existing_kinds) ||
                    !issorted(existing_kinds) ||
                    any(k->parentmodule(k) != mod, existing_kinds)
                error("Error registering kinds for module $mod (register_kinds() called more than once inconsistently, or conflict with existing module kinds?)")
            else
                # Assume we're re-registering kinds as in top level vs `__init__`
                return
            end
        else
            error("Kind module ID $module_id already claimed by module $m")
        end
    end
    _register_kinds_names!(int_to_kindstr, kind_str_to_int, module_id, names)
end

# This function is separated from `_register_kinds!` to prevent sharing of the variable `i`
# here and in the closure in `_register_kinds!`, which causes boxing and bad inference.
function _register_kinds_names!(int_to_kindstr, kind_str_to_int, module_id, names)
    # Process names to conflate category BEGIN/END markers with the first/last
    # in the category.
    i = 0
    for name in names
        normal_kind = false
        if startswith(name, "BEGIN_")
            j = i
        elseif startswith(name, "END_")
            j = i - 1
        else
            normal_kind = true
            j = i
            i += 1
        end
        kind_int = (module_id << _kind_nbits) | j
        push!(kind_str_to_int, name=>kind_int)
        if normal_kind
            push!(int_to_kindstr, kind_int=>name)
        end
    end
end

"""
    register_kinds!(mod, module_id, names)

Register custom `Kind`s with the given `names`, belonging to a module `mod`.
`names` is an array of arbitrary strings.

In order for kinds to be represented by a small number of bits, some nontrivial
cooperation is required between modules using custom kinds:
* The integer `module_id` is globally unique for each `mod` which will be used
  together, and not larger than $_kind_module_id_max.
* No two modules register the same `name`. The semantics of a given `kind` name
  should be defined by the module which owns it.

To allow ranges of kinds to be delimited and quickly tested for, some special
names are allowed: `BEGIN_section` and `END_section` pairs are detected, and
alias the next and previous kind id's respectively so that kinds in `section`
can be tested with `BEGIN_section <= k <= END_section`.
"""
function register_kinds!(mod, module_id, names)
    _register_kinds!(_kind_modules, _kind_int_to_str, _kind_str_to_int, mod, module_id, names)
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
    Kind(s)
end

"""
A set of kinds which can be used with the `in` operator.  For example

    k in KSet"+ - *"
"""
macro KSet_str(str)
    kinds = [Kind(s) for s in split(str)]

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
# Kinds used by JuliaSyntax
register_kinds!(JuliaSyntax, 0, [
    # Whitespace
    "Comment"
    "Whitespace"
    "NewlineWs"    # newline-containing whitespace

    # Identifiers
    "BEGIN_IDENTIFIERS"
        "Identifier"
        "Placeholder" # Used for empty catch variables, and all-underscore identifiers in lowering
        # String and command macro names are modeled as a special kind of
        # identifier as they need to be mangled before lookup.
        "StrMacroName"
        "CmdMacroName"
    "END_IDENTIFIERS"

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
        "BEGIN_NUMBERS"
            "Bool"
            "Integer"
            "BinInt"
            "HexInt"
            "OctInt"
            "Float"
            "Float32"
        "END_NUMBERS"
        "String"
        "Char"
        "CmdString"
    "END_LITERAL"

    "BEGIN_DELIMITERS"
        # Punctuation
        "@"
        ","
        ";"

        # Paired delimiters
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
        "BEGIN_SYNTACTIC_ASSIGNMENTS"
        "="
        ".="
        "op="  # Updating assignment operator ( $= %= &= *= += -= //= /= <<= >>= >>>= \= ^= |= ÷= ⊻= )
        ".op="
        ":="
        "END_SYNTACTIC_ASSIGNMENTS"
        "~"
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
        "🢲"
    "END_ARROW"

    # Level 4
    "BEGIN_LAZYOR"
        "||"
        ".||"
    "END_LAZYOR"

    # Level 5
    "BEGIN_LAZYAND"
        "&&"
        ".&&"
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

    # 2. Nonterminals which are exposed in the AST, but where the surface
    #    syntax doesn't have a token corresponding to the node type.
    "BEGIN_SYNTAX_KINDS"
        "block"
        "call"
        "dotcall"
        "comparison"
        "curly"
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
        "iteration"
        "comprehension"
        "typed_comprehension"
        "macro_name"
        # Container for a single statement/atom plus any trivia and errors
        "wrapper"
    "END_SYNTAX_KINDS"

    # Special tokens
    "TOMBSTONE"    # Empty placeholder for kind to be filled later
    "None"         # Never emitted by lexer/parser
    "EndMarker"    # EOF

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
        "ErrorIdentifierStart"
        "ErrorUnknownCharacter"
        "ErrorBidiFormatting"
        # Generic error
        "error"
    "END_ERRORS"
])

#-------------------------------------------------------------------------------
const _nonunique_kind_names = Set([
    K"Comment"
    K"Whitespace"
    K"NewlineWs"
    K"Identifier"
    K"Placeholder"

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

    K"Bool"
    K"Integer"
    K"BinInt"
    K"HexInt"
    K"OctInt"
    K"Float"
    K"Float32"
    K"String"
    K"Char"
    K"CmdString"

    K"StrMacroName"
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
        return string(k)
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
    K"ErrorIdentifierStart" => "identifier cannot begin with character",
    K"ErrorUnknownCharacter"=>"unknown unicode character",
    K"ErrorBidiFormatting"=>"unbalanced bidirectional unicode formatting",
    K"ErrorInvalidOperator" => "invalid operator",
    K"Error**" => "use `x^y` instead of `x**y` for exponentiation, and `x...` instead of `**x` for splatting",
    K"error" => "unknown error token",
)

#-------------------------------------------------------------------------------
# Predicates
is_identifier(k::Kind) = K"BEGIN_IDENTIFIERS" <= k <= K"END_IDENTIFIERS"
is_contextual_keyword(k::Kind) = K"BEGIN_CONTEXTUAL_KEYWORDS" <= k <= K"END_CONTEXTUAL_KEYWORDS"
is_error(k::Kind) = K"BEGIN_ERRORS" <= k <= K"END_ERRORS" || k == K"ErrorInvalidOperator" || k == K"Error**"
is_keyword(k::Kind) = K"BEGIN_KEYWORDS" <= k <= K"END_KEYWORDS"
is_block_continuation_keyword(k::Kind) = K"BEGIN_BLOCK_CONTINUATION_KEYWORDS" <= k <= K"END_BLOCK_CONTINUATION_KEYWORDS"
is_literal(k::Kind) = K"BEGIN_LITERAL" <= k <= K"END_LITERAL"
is_number(k::Kind)  = K"BEGIN_NUMBERS" <= k <= K"END_NUMBERS"
is_operator(k::Kind) = K"BEGIN_OPS" <= k <= K"END_OPS"
is_word_operator(k::Kind) = (k == K"in" || k == K"isa" || k == K"where")

is_identifier(x) = is_identifier(kind(x))
is_contextual_keyword(x) = is_contextual_keyword(kind(x))
is_error(x) = is_error(kind(x))
is_keyword(x) = is_keyword(kind(x))
is_literal(x) = is_literal(kind(x))
is_number(x)  = is_number(kind(x))
is_operator(x) = is_operator(kind(x))
is_word_operator(x) = is_word_operator(kind(x))

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
is_syntactic_assignment(x) = K"BEGIN_SYNTACTIC_ASSIGNMENTS" <= kind(x) <= K"END_SYNTACTIC_ASSIGNMENTS"

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

function is_syntactic_operator(x)
    k = kind(x)
    # TODO: Do we need to disallow dotted and suffixed forms when this is used
    # in the parser? The lexer itself usually disallows such tokens, so it's
    # not clear whether we need to handle them. (Though note `.->` is a
    # token...)
    return k in KSet"&& || . ... ->" || is_syntactic_assignment(k)
end
