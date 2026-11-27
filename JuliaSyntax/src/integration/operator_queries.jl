# This file is a part of Julia. License is MIT: https://julialang.org/license

# The classification of names as operators that the runtime's operator queries
# answer with (jl_is_operator, jl_is_unary_operator,
# jl_is_unary_and_binary_operator, jl_is_syntactic_operator and
# jl_operator_precedence, which printing uses to decide how to show code). Their
# semantics are those of the flisp parser, which answers them until Base has
# started; here they are derived from this parser's tokenizer. Julia's Base
# hands them to the runtime with `register_operator_queries`.

# Map JuliaSyntax PrecedenceLevel values to the numeric precedence levels of
# the runtime's operator queries (those of the flisp parser: the order of its
# `prec-ops`). Note that flisp orders times < rational < bitshift
# while JuliaSyntax orders bitshift < times < rational, so this table is not
# the identity even where both scales have entries.
const _JSPREC_TO_FLISP = let m = zeros(Cint, Int(PREC_COMPOUND_ASSIGN) + 1)
    m[Int(PREC_ASSIGNMENT)     + 1] = 1
    m[Int(PREC_PAIRARROW)      + 1] = 2
    m[Int(PREC_CONDITIONAL)    + 1] = 3
    m[Int(PREC_ARROW)          + 1] = 4
    m[Int(PREC_LAZYOR)         + 1] = 5
    m[Int(PREC_LAZYAND)        + 1] = 6
    m[Int(PREC_COMPARISON)     + 1] = 7
    m[Int(PREC_PIPE_LT)        + 1] = 8
    m[Int(PREC_PIPE_GT)        + 1] = 9
    m[Int(PREC_COLON)          + 1] = 10
    m[Int(PREC_PLUS)           + 1] = 11
    m[Int(PREC_TIMES)          + 1] = 12
    m[Int(PREC_RATIONAL)       + 1] = 13
    m[Int(PREC_BITSHIFT)       + 1] = 14
    m[Int(PREC_POWER)          + 1] = 15
    m[Int(PREC_DECL)           + 1] = 16
    m[Int(PREC_WHERE)          + 1] = 0  # flisp: `where` is not in prec-ops
    m[Int(PREC_DOT)            + 1] = 17
    m[Int(PREC_QUOTE)          + 1] = 0
    m[Int(PREC_UNICODE_OPS)    + 1] = 0
    m[Int(PREC_COMPOUND_ASSIGN)+ 1] = 1
    m
end

"""
    OperatorInfo

The runtime's view of a name as an operator (see [`operator_info`](@ref)):
whether it is an operator, a unary operator, both a unary and a binary
operator, or a syntactic operator, and its precedence (`0` for none), on the
scale of the runtime's `jl_operator_precedence`.
"""
struct OperatorInfo
    isop::Bool
    unary::Bool
    unary_binary::Bool
    syntactic::Bool
    prec::Cint
end
const NOT_AN_OP = OperatorInfo(false, false, false, false, 0)

# Operators with their own token kind whose RawToken does not carry a usable
# `op_precedence`, or whose flisp answers differ from what the kind suggests.
function _fixed_kind_info(k, dotted::Bool)
    if k == K"="
        return OperatorInfo(true, false, false, true, 1)
    elseif k == K":="
        return OperatorInfo(true, false, false, true, 1)
    elseif k == K"~"
        # `~` is at assignment precedence; unary even when dotted, but
        # unary-and-binary only in its undotted form
        return OperatorInfo(true, true, !dotted, false, 1)
    elseif k == K"&&"
        return OperatorInfo(true, false, false, true, 6)
    elseif k == K"||"
        return OperatorInfo(true, false, false, true, 5)
    elseif k == K"<:" || k == K">:"
        return OperatorInfo(true, !dotted, false, false, 7)
    elseif k == K"::"
        return OperatorInfo(true, false, false, false, 16)
    elseif k == K"->"
        # syntactic, and not in the flisp prec-ops table
        return OperatorInfo(true, false, false, true, 0)
    elseif k == K"?"
        return OperatorInfo(true, false, false, false, 3)
    elseif k == K"'"
        return OperatorInfo(true, false, false, false, 0)
    elseif k == K"&"
        return OperatorInfo(true, false, !dotted, false, 12)
    elseif k == K"."
        return OperatorInfo(true, false, false, true, 17)
    elseif k == K"where"
        return OperatorInfo(false, false, false, false, 0)
    elseif k == K"in" || k == K"isa"
        # word operators: not `operator?`s in flisp, but they do have
        # comparison precedence
        return OperatorInfo(false, false, false, false, 7)
    end
    return nothing
end

function _classify_token(k, jsprec::Integer, dotted::Bool, base::String)
    fixed = _fixed_kind_info(k, dotted)
    fixed !== nothing && return fixed
    (is_operator(k) && !is_error(k)) || return NOT_AN_OP
    prec = 0 <= Int(jsprec) < length(_JSPREC_TO_FLISP) ?
        _JSPREC_TO_FLISP[Int(jsprec) + 1] : Cint(0)
    if Int(jsprec) == Int(PREC_COMPOUND_ASSIGN)
        return OperatorInfo(true, false, false, true, 1)
    end
    # n.b. by string, not kind: unicode lookalikes (e.g. − U+2212) share a
    # kind with their ascii canonical form but are not unary in flisp
    unary = base in ("+", "-", "!", "~", "¬", "√", "∛", "∜", "⋆", "±", "∓") # dotop allowed
    unary_binary = base in ("+", "-", "⋆", "±", "∓") ||
        (base in ("\$", "&", "~") && !dotted)
    return OperatorInfo(true, unary, unary_binary, false, prec)
end

# Operators that cannot take a `.` prefix
function _undottable(k)
    return k in KSet"-> . ? ' $ :: : where in isa := = Identifier"
end

# Operators that cannot take a suffix; mirrors the tokenizer, which attaches
# suffix characters only to non-syntactic symbolic operator tokens.
function _unsuffixable(k)
    return is_syntactic_operator(k) ||
        k in KSet"<: >: :: ' ? . $ where in isa && || Identifier"
end

function _strip_suffix(s::String)
    isempty(s) && return s, false
    i = lastindex(s)
    found = false
    while i >= firstindex(s) && Tokenize.isopsuffix(s[i])
        found = true
        i = i == firstindex(s) ? 0 : prevind(s, i)
    end
    i <= 0 && return "", found # all suffix characters
    return s[firstindex(s):i], found
end

# Tokenize `s` and return (kind, jsprec) if it consists of a single token
# spanning the whole string, else nothing.
function _single_token(s::String)
    found = nothing
    for t in Tokenize.tokenize(s)
        k = kind(t)
        k == K"EndMarker" && break
        found === nothing || return nothing # more than one token
        Int(t.endbyte) - Int(t.startbyte) + 1 == sizeof(s) || return nothing
        found = (k, Int(t.op_precedence))
    end
    return found
end

# Compound assignment operators tokenize as [op(PREC_COMPOUND_ASSIGN), =]
function _compound_assign(s::String)
    toks = collect(Tokenize.tokenize(s))
    length(toks) == 3 || return false # op, =, EndMarker
    kind(toks[3]) == K"EndMarker" || return false
    Int(toks[1].op_precedence) == Int(PREC_COMPOUND_ASSIGN) || return false
    return kind(toks[2]) == K"=" &&
        Int(toks[2].endbyte) + 1 == sizeof(s)
end

function _operator_info(s::String)
    isempty(s) && return NOT_AN_OP
    # Tokens built from `.` never tokenize as a unit; classify by string.
    if s == "."
        return OperatorInfo(true, false, false, true, 17)
    elseif s == ".."
        return OperatorInfo(true, false, false, false, 10)
    elseif s == "..."
        return OperatorInfo(true, false, false, true, 0)
    elseif s == ".="
        return OperatorInfo(true, false, false, true, 1)
    elseif s == ".'"
        return OperatorInfo(true, false, false, false, 0)
    end
    dotted = false
    if startswith(s, ".") && !startswith(s, "..")
        dotted = true
        s = s[2:end]
    end
    s, suffixed = _strip_suffix(s)
    isempty(s) && return NOT_AN_OP
    if !dotted && s == "🢲"
        # an identifier character (see jl_id_start_char), but still present
        # in the flisp precedence table; the dotted form is a normal operator
        return OperatorInfo(false, false, false, false, 4)
    end
    if _compound_assign(s)
        # `\$` cannot be dotted, including in its compound-assignment form
        (dotted && startswith(s, "\$")) && return NOT_AN_OP
        info = OperatorInfo(true, false, false, true, 1)
        k = nothing
    else
        tok = _single_token(s)
        tok === nothing && return NOT_AN_OP
        k, jsprec = tok
        info = _classify_token(k, jsprec, dotted, s)
    end
    if !info.isop
        # `in`/`isa` have a precedence despite not being operators, but
        # only in their plain form
        return (dotted || suffixed) ? NOT_AN_OP : info
    end
    if dotted && k !== nothing && _undottable(k)
        return NOT_AN_OP
    end
    if suffixed
        # assignment-level and colon-level operators (including `~` and the
        # unicode `:=`-family) do not take suffixes
        if k === nothing || _unsuffixable(k) || info.prec == 1 || info.prec == 10
            return NOT_AN_OP
        end
        info = OperatorInfo(true, false, false, false, info.prec)
    end
    return info
end

"""
    operator_info(name)

How the runtime's operator queries classify `name` (an operator name, like
`"+"`, `".+"` or `"+₁"`), as an [`OperatorInfo`](@ref).
"""
operator_info(name::AbstractString) = _operator_info(String(name))

# The entry points for the runtime (see `register_operator_queries`). Names are
# classified once: printing asks about the same few names over and over.
const _operator_info_cache = Dict{Symbol,OperatorInfo}()
const _operator_info_lock = ReentrantLock()

function _operator_info(name::Cstring)
    try
        sym = ccall(:jl_symbol, Ref{Symbol}, (Cstring,), name)
        return @lock _operator_info_lock get!(_operator_info_cache, sym) do
            _operator_info(String(sym))
        end
    catch
        return NOT_AN_OP # the runtime has no way to handle errors here
    end
end
_c_is_operator(name::Cstring) = Cint(_operator_info(name).isop)
_c_is_unary_operator(name::Cstring) = Cint(_operator_info(name).unary)
_c_is_unary_and_binary_operator(name::Cstring) = Cint(_operator_info(name).unary_binary)
_c_is_syntactic_operator(name::Cstring) = Cint(_operator_info(name).syntactic)
_c_operator_precedence(name::Cstring) = _operator_info(name).prec

"""
    register_operator_queries()

Make the runtime's operator queries (`jl_is_operator` etc.) answer with this
JuliaSyntax's classification of operators, rather than ask the runtime's own
frontend. This is for the copy of JuliaSyntax in Julia's Base, which does it
when it starts.
"""
function register_operator_queries()
    ccall(:jl_set_operator_queries, Cvoid,
          (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
          @cfunction(_c_is_operator, Cint, (Cstring,)),
          @cfunction(_c_is_unary_operator, Cint, (Cstring,)),
          @cfunction(_c_is_unary_and_binary_operator, Cint, (Cstring,)),
          @cfunction(_c_is_syntactic_operator, Cint, (Cstring,)),
          @cfunction(_c_operator_precedence, Cint, (Cstring,)))
    nothing
end
