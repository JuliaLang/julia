# This file is a part of Julia. License is MIT: https://julialang.org/license

# Flisp-compatible operator classification implemented on top of the
# JuliaSyntax tokenizer. This implements the semantics of the C entry points
# jl_is_operator, jl_is_unary_operator, jl_is_unary_and_binary_operator,
# jl_is_syntactic_operator and jl_operator_precedence, with answers
# bit-identical to the flisp reference frontend (enforced by
# test_ops_parity.jl, which sweeps all of unicode plus multi-character
# combinations against the flisp implementation).
#
# The includer must define `_JS` as the JuliaSyntax module before including
# this file (this allows the same file to be used from the JuliaFrontend
# package and, for parity testing, directly against Base.JuliaSyntax).

const _TZ = _JS.Tokenize

# Map JuliaSyntax PrecedenceLevel values to the numeric precedence levels
# returned by the flisp frontend (the order of `prec-ops` in
# src/julia-parser.scm). Note that flisp orders times < rational < bitshift
# while JuliaSyntax orders bitshift < times < rational, so this table is not
# the identity even where both scales have entries.
const _JSPREC_TO_FLISP = let m = zeros(Cint, Int(_JS.PREC_COMPOUND_ASSIGN) + 1)
    m[Int(_JS.PREC_ASSIGNMENT)     + 1] = 1
    m[Int(_JS.PREC_PAIRARROW)      + 1] = 2
    m[Int(_JS.PREC_CONDITIONAL)    + 1] = 3
    m[Int(_JS.PREC_ARROW)          + 1] = 4
    m[Int(_JS.PREC_LAZYOR)         + 1] = 5
    m[Int(_JS.PREC_LAZYAND)        + 1] = 6
    m[Int(_JS.PREC_COMPARISON)     + 1] = 7
    m[Int(_JS.PREC_PIPE_LT)        + 1] = 8
    m[Int(_JS.PREC_PIPE_GT)        + 1] = 9
    m[Int(_JS.PREC_COLON)          + 1] = 10
    m[Int(_JS.PREC_PLUS)           + 1] = 11
    m[Int(_JS.PREC_TIMES)          + 1] = 12
    m[Int(_JS.PREC_RATIONAL)       + 1] = 13
    m[Int(_JS.PREC_BITSHIFT)       + 1] = 14
    m[Int(_JS.PREC_POWER)          + 1] = 15
    m[Int(_JS.PREC_DECL)           + 1] = 16
    m[Int(_JS.PREC_WHERE)          + 1] = 0  # flisp: `where` is not in prec-ops
    m[Int(_JS.PREC_DOT)            + 1] = 17
    m[Int(_JS.PREC_QUOTE)          + 1] = 0
    m[Int(_JS.PREC_UNICODE_OPS)    + 1] = 0
    m[Int(_JS.PREC_COMPOUND_ASSIGN)+ 1] = 1
    m
end

struct OpInfo
    isop::Bool
    unary::Bool
    unary_binary::Bool
    syntactic::Bool
    prec::Cint
end
const NOT_AN_OP = OpInfo(false, false, false, false, 0)

# Operators with their own token kind whose RawToken does not carry a usable
# `op_precedence`, or whose flisp answers differ from what the kind suggests.
function _fixed_kind_info(k, dotted::Bool)
    if k == _JS.K"="
        return OpInfo(true, false, false, true, 1)
    elseif k == _JS.K":="
        return OpInfo(true, false, false, true, 1)
    elseif k == _JS.K"~"
        # `~` is at assignment precedence; unary even when dotted, but
        # unary-and-binary only in its undotted form
        return OpInfo(true, true, !dotted, false, 1)
    elseif k == _JS.K"&&"
        return OpInfo(true, false, false, true, 6)
    elseif k == _JS.K"||"
        return OpInfo(true, false, false, true, 5)
    elseif k == _JS.K"<:" || k == _JS.K">:"
        return OpInfo(true, !dotted, false, false, 7)
    elseif k == _JS.K"::"
        return OpInfo(true, false, false, false, 16)
    elseif k == _JS.K"->"
        # syntactic, and not in the flisp prec-ops table
        return OpInfo(true, false, false, true, 0)
    elseif k == _JS.K"?"
        return OpInfo(true, false, false, false, 3)
    elseif k == _JS.K"'"
        return OpInfo(true, false, false, false, 0)
    elseif k == _JS.K"&"
        return OpInfo(true, false, !dotted, false, 12)
    elseif k == _JS.K"."
        return OpInfo(true, false, false, true, 17)
    elseif k == _JS.K"where"
        return OpInfo(false, false, false, false, 0)
    elseif k == _JS.K"in" || k == _JS.K"isa"
        # word operators: not `operator?`s in flisp, but they do have
        # comparison precedence
        return OpInfo(false, false, false, false, 7)
    end
    return nothing
end

function _classify_token(k, jsprec::Integer, dotted::Bool, base::String)
    fixed = _fixed_kind_info(k, dotted)
    fixed !== nothing && return fixed
    (_JS.is_operator(k) && !_JS.is_error(k)) || return NOT_AN_OP
    prec = 0 <= Int(jsprec) < length(_JSPREC_TO_FLISP) ?
        _JSPREC_TO_FLISP[Int(jsprec) + 1] : Cint(0)
    if Int(jsprec) == Int(_JS.PREC_COMPOUND_ASSIGN)
        return OpInfo(true, false, false, true, 1)
    end
    # n.b. by string, not kind: unicode lookalikes (e.g. − U+2212) share a
    # kind with their ascii canonical form but are not unary in flisp
    unary = base in ("+", "-", "!", "~", "¬", "√", "∛", "∜", "⋆", "±", "∓") # dotop allowed
    unary_binary = base in ("+", "-", "⋆", "±", "∓") ||
        (base in ("\$", "&", "~") && !dotted)
    return OpInfo(true, unary, unary_binary, false, prec)
end

# Operators that cannot take a `.` prefix
function _undottable(k)
    return k in _JS.KSet"-> . ? ' $ :: : where in isa := = Identifier"
end

# Operators that cannot take a suffix; mirrors the tokenizer, which attaches
# suffix characters only to non-syntactic symbolic operator tokens.
function _unsuffixable(k)
    return _JS.is_syntactic_operator(k) ||
        k in _JS.KSet"<: >: :: ' ? . $ where in isa && || Identifier"
end

function _strip_suffix(s::String)
    isempty(s) && return s, false
    i = lastindex(s)
    found = false
    while i >= firstindex(s) && _TZ.isopsuffix(s[i])
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
    for t in _TZ.tokenize(s)
        k = _JS.kind(t)
        k == _JS.K"EndMarker" && break
        found === nothing || return nothing # more than one token
        Int(t.endbyte) - Int(t.startbyte) + 1 == sizeof(s) || return nothing
        found = (k, Int(t.op_precedence))
    end
    return found
end

# Compound assignment operators tokenize as [op(PREC_COMPOUND_ASSIGN), =]
function _compound_assign(s::String)
    toks = collect(_TZ.tokenize(s))
    length(toks) == 3 || return false # op, =, EndMarker
    _JS.kind(toks[3]) == _JS.K"EndMarker" || return false
    Int(toks[1].op_precedence) == Int(_JS.PREC_COMPOUND_ASSIGN) || return false
    return _JS.kind(toks[2]) == _JS.K"=" &&
        Int(toks[2].endbyte) + 1 == sizeof(s)
end

function _opinfo(s::String)
    isempty(s) && return NOT_AN_OP
    # Tokens built from `.` never tokenize as a unit; classify by string.
    if s == "."
        return OpInfo(true, false, false, true, 17)
    elseif s == ".."
        return OpInfo(true, false, false, false, 10)
    elseif s == "..."
        return OpInfo(true, false, false, true, 0)
    elseif s == ".="
        return OpInfo(true, false, false, true, 1)
    elseif s == ".'"
        return OpInfo(true, false, false, false, 0)
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
        return OpInfo(false, false, false, false, 4)
    end
    if _compound_assign(s)
        # `\$` cannot be dotted, including in its compound-assignment form
        (dotted && startswith(s, "\$")) && return NOT_AN_OP
        info = OpInfo(true, false, false, true, 1)
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
        info = OpInfo(true, false, false, false, info.prec)
    end
    return info
end

flisp_is_operator(s::String) = _opinfo(s).isop
flisp_is_unary_operator(s::String) = _opinfo(s).unary
flisp_is_unary_and_binary_operator(s::String) = _opinfo(s).unary_binary
flisp_is_syntactic_operator(s::String) = _opinfo(s).syntactic
flisp_operator_precedence(s::String) = _opinfo(s).prec
