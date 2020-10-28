# This file is a part of Julia. License is MIT: https://julialang.org/license

module Order


import ..@__MODULE__, ..parentmodule
const Base = parentmodule(@__MODULE__)
import .Base:
    AbstractVector, @propagate_inbounds, isless, identity, getindex,
    +, -, !, &, <, |

## notions of element ordering ##

export # not exported by Base
    Ordering, Forward, Reverse,
    By, Lt, Perm,
    ReverseOrdering, ForwardOrdering,
    DirectOrdering,
    lt, ord, ordtype

"""
        Base.Order.Ordering

Abstract type which represents a total order on some set of elements.

Use [`Base.Order.lt`](@ref) to compare two elements according to the ordering.
"""
abstract type Ordering end

struct ForwardOrdering <: Ordering end

"""
    ReverseOrdering(fwd::Ordering=Forward)

A wrapper which reverses an ordering.

For a given `Ordering` `o`, the following holds for all  `a`, `b`:

    lt(ReverseOrdering(o), a, b) == lt(o, b, a)
"""
struct ReverseOrdering{Fwd<:Ordering} <: Ordering
    fwd::Fwd
end

ReverseOrdering(rev::ReverseOrdering) = rev.fwd
ReverseOrdering(fwd::Fwd) where {Fwd} = ReverseOrdering{Fwd}(fwd)
ReverseOrdering() = ReverseOrdering(ForwardOrdering())

const DirectOrdering = Union{ForwardOrdering,ReverseOrdering{ForwardOrdering}}

"""
    Base.Order.Forward

Default ordering according to [`isless`](@ref).
"""
const Forward = ForwardOrdering()

"""
    Base.Order.Reverse

Reverse ordering according to [`isless`](@ref).
"""
const Reverse = ReverseOrdering()

"""
    By(by, order::Ordering=Forward)

`Ordering` which applies `order` to elements after they have been transformed
by the function `by`.
"""
struct By{T, O} <: Ordering
    by::T
    order::O
end

# backwards compatibility with VERSION < v"1.5-"
By(by) = By(by, Forward)

"""
    Lt(lt)

`Ordering` which calls `lt(a, b)` to compare elements. `lt` should
obey the same rules as implementations of [`isless`](@ref).
"""
struct Lt{T} <: Ordering
    lt::T
end

"""
    Perm(order::Ordering, data::AbstractVector)

`Ordering` on the indices of `data` where `i` is less than `j` if `data[i]` is
less than `data[j]` according to `order`. In the case that `data[i]` and
`data[j]` are equal, `i` and `j` are compared by numeric value.
"""
struct Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    order::O
    data::V
end

ReverseOrdering(by::By) = By(by.by, ReverseOrdering(by.order))
ReverseOrdering(perm::Perm) = Perm(ReverseOrdering(perm.order), perm.data)

"""
    lt(o::Ordering, a, b)

Test whether `a` is less than `b` according to the ordering `o`.
"""
lt(o::ForwardOrdering,       a, b) = isless(a,b)
lt(o::ReverseOrdering,       a, b) = lt(o.fwd,b,a)
lt(o::By,                    a, b) = lt(o.order,o.by(a),o.by(b))
lt(o::Lt,                    a, b) = o.lt(a,b)

@propagate_inbounds function lt(p::Perm, a::Integer, b::Integer)
    da = p.data[a]
    db = p.data[b]
    lt(p.order, da, db) | (!lt(p.order, db, da) & (a < b))
end

_ord(lt::typeof(isless), by::typeof(identity), order::Ordering) = order
_ord(lt::typeof(isless), by,                   order::Ordering) = By(by, order)

function _ord(lt, by, order::Ordering)
    if order === Forward
        return Lt((x, y) -> lt(by(x), by(y)))
    elseif order === Reverse
        return Lt((x, y) -> lt(by(y), by(x)))
    else
        error("Passing both lt= and order= arguments is ambiguous; please pass order=Forward or order=Reverse (or leave default)")
    end
end

"""
    ord(lt, by, rev::Union{Bool, Nothing}, order::Ordering=Forward)

Construct an [`Ordering`](@ref) object from the same arguments used by
[`sort!`](@ref).
Elements are first transformed by the function `by` (which may be
[`identity`](@ref)) and are then compared according to either the function `lt`
or an existing ordering `order`. `lt` should be [`isless`](@ref) or a function
which obeys similar rules. Finally, the resulting order is reversed if
`rev=true`.

Passing an `lt` other than `isless` along with an `order` other than
[`Base.Order.Forward`](@ref) or [`Base.Order.Reverse`](@ref) is not permitted,
otherwise all options are independent and can be used together in all possible
combinations.
"""
ord(lt, by, rev::Nothing, order::Ordering=Forward) = _ord(lt, by, order)

function ord(lt, by, rev::Bool, order::Ordering=Forward)
    o = _ord(lt, by, order)
    return rev ? ReverseOrdering(o) : o
end


# This function is not in use anywhere in Base but we observed
# use in sorting-related packages (#34719). It's probably best to move
# this functionality to those packages in the future; let's remind/force
# ourselves to deprecate this in v2.0.
# The following clause means `if VERSION < v"2.0-"` but it also works during
# bootstrap. For the same reason, we need to write `Int32` instead of `Cint`.
if ccall(:jl_ver_major, Int32, ()) < 2
    ordtype(o::ReverseOrdering, vs::AbstractArray) = ordtype(o.fwd, vs)
    ordtype(o::Perm,            vs::AbstractArray) = ordtype(o.order, o.data)
    # TODO: here, we really want the return type of o.by, without calling it
    ordtype(o::By,              vs::AbstractArray) = try typeof(o.by(vs[1])) catch; Any end
    ordtype(o::Ordering,        vs::AbstractArray) = eltype(vs)
end

end
