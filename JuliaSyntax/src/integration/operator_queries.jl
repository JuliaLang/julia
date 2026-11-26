# This file is a part of Julia. License is MIT: https://julialang.org/license

# The classification of names as operators that Julia's printing uses to decide
# how to show code (Base.isoperator, Base.operator_precedence etc., and the
# runtime's jl_is_operator), derived from this parser's tokenizer. It follows
# the flisp parser, which classifies them while bootstrapping, except that
# precedences are this parser's `PrecedenceLevel`s.

"""
    OperatorInfo

How a name classifies as an operator (see [`operator_info`](@ref)): whether
it is an operator, a unary operator, both a unary and a binary operator, or a
syntactic operator, and its precedence as a binary operator (a
`PrecedenceLevel`, or `0` for none).
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
        return OperatorInfo(true, false, false, true, Cint(PREC_ASSIGNMENT))
    elseif k == K":="
        return OperatorInfo(true, false, false, true, Cint(PREC_ASSIGNMENT))
    elseif k == K"~"
        # `~` is at assignment precedence; unary even when dotted, but
        # unary-and-binary only in its undotted form
        return OperatorInfo(true, true, !dotted, false, Cint(PREC_ASSIGNMENT))
    elseif k == K"&&"
        return OperatorInfo(true, false, false, true, Cint(PREC_LAZYAND))
    elseif k == K"||"
        return OperatorInfo(true, false, false, true, Cint(PREC_LAZYOR))
    elseif k == K"<:" || k == K">:"
        return OperatorInfo(true, !dotted, false, false, Cint(PREC_COMPARISON))
    elseif k == K"::"
        return OperatorInfo(true, false, false, false, Cint(PREC_DECL))
    elseif k == K"->"
        # syntactic, and not a binary operator
        return OperatorInfo(true, false, false, true, 0)
    elseif k == K"?"
        return OperatorInfo(true, false, false, false, Cint(PREC_CONDITIONAL))
    elseif k == K"'"
        return OperatorInfo(true, false, false, false, 0)
    elseif k == K"&"
        return OperatorInfo(true, false, !dotted, false, Cint(PREC_TIMES))
    elseif k == K"."
        return OperatorInfo(true, false, false, true, Cint(PREC_DOT))
    elseif k == K"where"
        return OperatorInfo(false, false, false, false, 0)
    elseif k == K"in" || k == K"isa"
        # word operators: not `operator?`s in flisp, but they do have
        # comparison precedence
        return OperatorInfo(false, false, false, false, Cint(PREC_COMPARISON))
    end
    return nothing
end

function _classify_token(k, jsprec::PrecedenceLevel, dotted::Bool, base::String)
    fixed = _fixed_kind_info(k, dotted)
    fixed !== nothing && return fixed
    (is_operator(k) && !is_error(k)) || return NOT_AN_OP
    if jsprec == PREC_COMPOUND_ASSIGN
        return OperatorInfo(true, false, false, true, Cint(PREC_ASSIGNMENT))
    end
    prec = Cint(jsprec)
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
        found = (k, t.op_precedence)
    end
    return found
end

# Compound assignment operators tokenize as [op(PREC_COMPOUND_ASSIGN), =]
function _compound_assign(s::String)
    toks = collect(Tokenize.tokenize(s))
    length(toks) == 3 || return false # op, =, EndMarker
    kind(toks[3]) == K"EndMarker" || return false
    toks[1].op_precedence == PREC_COMPOUND_ASSIGN || return false
    return kind(toks[2]) == K"=" &&
        Int(toks[2].endbyte) + 1 == sizeof(s)
end

function _operator_info(s::String)
    isempty(s) && return NOT_AN_OP
    # Tokens built from `.` never tokenize as a unit; classify by string.
    if s == "."
        return OperatorInfo(true, false, false, true, Cint(PREC_DOT))
    elseif s == ".."
        return OperatorInfo(true, false, false, false, Cint(PREC_COLON))
    elseif s == "..."
        return OperatorInfo(true, false, false, true, 0)
    elseif s == ".="
        return OperatorInfo(true, false, false, true, Cint(PREC_ASSIGNMENT))
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
        return OperatorInfo(false, false, false, false, Cint(PREC_ARROW))
    end
    if _compound_assign(s)
        # `\$` cannot be dotted, including in its compound-assignment form
        (dotted && startswith(s, "\$")) && return NOT_AN_OP
        info = OperatorInfo(true, false, false, true, Cint(PREC_ASSIGNMENT))
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
        if k === nothing || _unsuffixable(k) || info.prec == Cint(PREC_ASSIGNMENT) || info.prec == Cint(PREC_COLON)
            return NOT_AN_OP
        end
        info = OperatorInfo(true, false, false, false, info.prec)
    end
    return info
end

"""
    operator_info(name)

How `name` (an operator name, like `:+`, `:.+` or `:+₁`) classifies as an
operator, as an [`OperatorInfo`](@ref).
"""
operator_info(name::AbstractString) = _operator_info(String(name))

# Names are classified once: printing asks about the same few over and over.
const _operator_info_cache = Dict{Symbol,OperatorInfo}()
const _operator_info_lock = ReentrantLock()

function operator_info(name::Symbol)
    @lock _operator_info_lock get!(_operator_info_cache, name) do
        _operator_info(String(name))
    end
end

# For the runtime's jl_is_operator, which its printing (jl_static_show) uses
function _c_is_operator(name::Cstring)
    try
        return Cint(operator_info(ccall(:jl_symbol, Ref{Symbol}, (Cstring,), name)).isop)
    catch
        return Cint(0) # the runtime has no way to handle errors here
    end
end

"""
    register_operator_queries()

Make the runtime's `jl_is_operator`, which its printing uses, answer with this
JuliaSyntax's classification of operators, rather than ask the runtime's own
frontend. This is for the copy of JuliaSyntax in Julia's Base, which does it
when it starts.
"""
function register_operator_queries()
    ccall(:jl_set_is_operator_query, Cvoid, (Ptr{Cvoid},),
          @cfunction(_c_is_operator, Cint, (Cstring,)))
    nothing
end
