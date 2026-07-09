# This file is a part of Julia. License is MIT: https://julialang.org/license

# Runtime support for `match` statements.
#
#     match x
#     case 1
#         "one"
#     case (a, b) if a > b
#         "decreasing pair"
#     case ::AbstractString
#         "string"
#     end
#
# A match statement checks its scrutinee against each `case` pattern in
# order, running the body of the first arm that matches. Falling through the
# end of the arms is a runtime error (`MatchError`). The match statement is
# an expression evaluating to the value of the selected arm's body; an arm
# with an empty body evaluates to the matched value itself. A `match`
# participates in the default `break` scope: `break` (or `break _ value`)
# exits the match.
#
# Pattern semantics:
#  * A bare identifier at the top level of a pattern is *resolved*: its value
#    is used as a matcher (see `pattern_match`). Values match by `isequal`;
#    functions act as predicates (`case ==(0)`); see below.
#  * Identifiers in argument position of tuple/call/etc. patterns are
#    captures, bound over the arm's guard and body.
#  * `$x` matches the value of `x` by `isequal` in any position.
#  * `::T` tests the type; `x::T` also captures.
#  * `(p1, p2)` matches `Tuple`s of matching length; `[p1, p2, r...]` matches
#    `AbstractVector`s; `k => v` matches `Pair`s; `(; a, b)` matches on
#    properties.
#  * `T(p1, p2)` destructures fields of `T` in order. Calls to non-type
#    functions are evaluated and their result used as a matcher, e.g.
#    `case ==(c)`; overload `Base.matcher` to customize call patterns.
#    A call pattern may contain one `rest...` slurp subpattern, delivered
#    to `matcher` implementations as a `MatchSlurp`.
#  * `p1 | p2` matches either alternative (which must bind the same names).
#
# The match statement itself (pattern compilation, arm dispatch, and the
# inline destructuring form `match pat = val`) is implemented in lowering;
# see `src/julia-syntax.scm`. This file provides the runtime matching
# protocol it targets, which is also the user extension point.

"""
    MatchError(value)

The exception thrown when no `case` arm of a `match` statement matches the
scrutinee `value`.

!!! compat "Julia 1.14"
    The `match` statement was added in Julia 1.14.
"""
struct MatchError <: Exception
    value::Any
end

#-------------------------------------------------------------------------------
# Matching protocol
#
# `pattern_match(m, value)` returns `nothing` if `m` does not match `value`,
# or a (possibly empty) tuple of captured values.

"""
    pattern_match(m, value) -> Union{Nothing, Tuple}

The `match` statement's pattern matching protocol: test `value` against the
matcher `m`, returning `nothing` on mismatch or a tuple of captured values
on success.

The default fallback compares `value` with `m` using [`isequal`](@ref).
Functions act as predicates: `pattern_match(f, v)` succeeds when
`f(v) === true`, which is what makes patterns like `case ==(0)` or
`case in(1:3)` work. Custom matcher objects may add methods to this
function; see also [`matcher`](@ref).
"""
pattern_match(@nospecialize(m), @nospecialize(v)) = isequal(m, v) ? () : nothing
pattern_match(f::Function, @nospecialize(v)) = f(v) === true ? () : nothing
# the method for Regex patterns is defined in regex.jl, which is loaded later

abstract type AbstractMatcher end

# A capture: matches anything, binding one value
struct MatchCapture <: AbstractMatcher end
pattern_match(::MatchCapture, @nospecialize(v)) = (v,)

# `_`: matches anything, binds nothing
struct MatchWildcard <: AbstractMatcher end
pattern_match(::MatchWildcard, @nospecialize(v)) = ()

# `::T`
struct TypeMatcher <: AbstractMatcher
    t
end
pattern_match(m::TypeMatcher, @nospecialize(v)) = v isa m.t ? () : nothing

# `x::T`
struct CaptureTyped <: AbstractMatcher
    t
end
pattern_match(m::CaptureTyped, @nospecialize(v)) = v isa m.t ? (v,) : nothing

# `$x`: match a value by isequal, regardless of its type
struct AsValue <: AbstractMatcher
    x
end
pattern_match(m::AsValue, @nospecialize(v)) = isequal(m.x, v) ? () : nothing

# `(p1, p2, ...)`: tuples of exactly matching length
struct TupleMatcher{PS<:Tuple} <: AbstractMatcher
    pats::PS
end
function pattern_match(m::TupleMatcher, @nospecialize(v))
    v isa Tuple || return nothing
    length(v) == length(m.pats) || return nothing
    caps = ()
    for i in 1:length(m.pats)
        r = pattern_match(m.pats[i], v[i])
        r === nothing && return nothing
        caps = (caps..., r...)
    end
    return caps
end

# `[p1, p2, ...]` / `[p1, rest...]`: AbstractVectors, with optional trailing slurp
struct VectMatcher{PS<:Tuple} <: AbstractMatcher
    pats::PS
    slurp::Bool
end
function pattern_match(m::VectMatcher, @nospecialize(v))
    v isa AbstractVector || return nothing
    n = length(m.pats)
    npos = m.slurp ? n - 1 : n
    m.slurp ? (length(v) >= npos || return nothing) :
              (length(v) == n || return nothing)
    caps = ()
    i0 = firstindex(v)
    for i in 1:npos
        r = pattern_match(m.pats[i], v[i0 + i - 1])
        r === nothing && return nothing
        caps = (caps..., r...)
    end
    if m.slurp
        r = pattern_match(m.pats[n], v[i0 + npos:end])
        r === nothing && return nothing
        caps = (caps..., r...)
    end
    return caps
end

# `k => v`
struct PairMatcher <: AbstractMatcher
    k
    v
end
function pattern_match(m::PairMatcher, @nospecialize(x))
    x isa Pair || return nothing
    r1 = pattern_match(m.k, x.first)
    r1 === nothing && return nothing
    r2 = pattern_match(m.v, x.second)
    r2 === nothing && return nothing
    return (r1..., r2...)
end

# `(; a, b)`: property destructuring
struct PropertyMatcher{PS<:Tuple} <: AbstractMatcher
    names::Array{Symbol,1}   # == Vector{Symbol}; the alias is defined later
    pats::PS
end
function pattern_match(m::PropertyMatcher, @nospecialize(v))
    caps = ()
    for i in 1:length(m.names)
        name = m.names[i]
        hasproperty(v, name) || return nothing
        r = pattern_match(m.pats[i], getproperty(v, name))
        r === nothing && return nothing
        caps = (caps..., r...)
    end
    return caps
end

# `p1 | p2`: alternation. All alternatives bind the same names; `perms[i]`
# maps the canonical capture order onto alternative `i`'s capture order.
struct OrMatcher{AS<:Tuple,PS<:Tuple} <: AbstractMatcher
    alts::AS
    perms::PS
end
function pattern_match(m::OrMatcher, @nospecialize(v))
    for i in 1:length(m.alts)
        r = pattern_match(m.alts[i], v)
        r === nothing && continue
        perm = m.perms[i]
        return ntuple(j -> r[perm[j]], length(perm))
    end
    return nothing
end

# `T(p1, p2)`: structural destructuring of the fields of `T`
struct StructMatcher{PS<:Tuple} <: AbstractMatcher
    t
    pats::PS
end
function pattern_match(m::StructMatcher, @nospecialize(v))
    v isa m.t || return nothing
    nfields(v) == length(m.pats) || return nothing
    caps = ()
    for i in 1:length(m.pats)
        r = pattern_match(m.pats[i], getfield(v, i))
        r === nothing && return nothing
        caps = (caps..., r...)
    end
    return caps
end

"""
    MatchSlurp(pat)

The compiled form of a `rest...` subpattern in a call pattern
`f(p1, rest..., p2)` of a `match` statement. [`matcher`](@ref)
implementations receive it in the pattern list and decide how to slurp;
`pat` is the compiled pattern for the slurped collection (usually a
capture).
"""
struct MatchSlurp{M}
    pat::M
end

function _matcher_has_captures(@nospecialize(m))
    (m isa MatchCapture || m isa CaptureTyped) && return true
    if m isa TupleMatcher || m isa VectMatcher || m isa StructMatcher
        return any(_matcher_has_captures, m.pats)
    elseif m isa PairMatcher
        return _matcher_has_captures(m.k) || _matcher_has_captures(m.v)
    elseif m isa PropertyMatcher
        return any(_matcher_has_captures, m.pats)
    elseif m isa OrMatcher
        return any(_matcher_has_captures, m.alts)
    elseif m isa MatchSlurp
        return _matcher_has_captures(m.pat)
    end
    return false
end

"""
    matcher(f, patterns...)

Construct the matcher for a call pattern `f(patterns...)` in a `match`
statement. For a type `T`, the default destructures instances of `T`,
matching `patterns` against its fields in order. For other callables the
call is evaluated (the patterns must not contain captures) and its result
used as the matcher, so that e.g. `case ==(c)` and `case startswith("f")`
work as predicates.

A call pattern may contain one `rest...` slurp subpattern, which is passed
in `patterns` as a [`MatchSlurp`](@ref).

Overload this function to give call patterns of specific functions custom
matching behavior (\"active patterns\").
"""
function matcher(::Type{T}, pats...) where {T}
    if any(p -> p isa MatchSlurp, pats)
        throw(ArgumentError("slurp patterns are not supported when destructuring struct fields"))
    end
    return StructMatcher(T, pats)
end
function matcher(@nospecialize(f), pats...)
    if any(_matcher_has_captures, pats)
        throw(ArgumentError("cannot destructure through a call to `$f` in a match pattern; define a `Base.matcher` method for it"))
    end
    return f(pats...)
end
