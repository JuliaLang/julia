# This file is a part of Julia. License is MIT: https://julialang.org/license

module Order


import ..@__MODULE__, ..parentmodule
const Base = parentmodule(@__MODULE__)
import .Base:
    AbstractVector, @propagate_inbounds, isless, identity, getindex, reverse,
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

Abstract type which represents a strict weak order on some set of elements. See
[`sort!`](@ref) for more.

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

"""
    reverse(o::Base.Ordering)

reverses ordering specified by `o`.

"""
reverse(o::Ordering) = ReverseOrdering(o)

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

`Ordering` that calls `lt(a, b)` to compare elements. `lt` must
obey the same rules as the `lt` parameter of [`sort!`](@ref).
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
    lt(o::Ordering, a, b) -> Bool

Test whether `a` is less than `b` according to the ordering `o`.
""" # No see-also because the prepared ordering system is experimental.
function lt end

"""
    lt_prepared(o::Ordering, a, b)

Test whether `a` is less than `b` according to the ordering `o`, assuming both `a` and `b`
have been prepared with `prepare`.

`lt_prepared(o, prepare(o, a), prepare(o, b))` is equivalent to `lt(o, a, b)`.

!!! warning
    Comparing a prepared element `prepare(o1, x)` under a different ordering `o2`
    is undefined behavior and may, for example, result in segfaults.

See also `lt_prepared_1`, `lt_prepared_2`.
"""
function lt_prepared end

"""
    lt_prepared_1(o::Ordering, a, b)

Test whether `a` is less than `b` according to the ordering `o`, assuming `a` has been
prepared with `prepare`.

`lt_prepared_1(o, prepare(o, a), b)` is equivalent to `lt(o, a, b)`.

!!! warning
    Comparing a prepared element `prepare(o1, x)` under a different ordering `o2`
    is undefined behavior and may, for example, result in segfaults.

See also `lt`, `lt_prepared`.
"""
@propagate_inbounds lt_prepared_1(o::Ordering, a, b) = lt_prepared(o, a, prepare(o, b))

"""
    lt_prepared_2(o::Ordering, a, b)

Test whether `a` is less than `b` according to the ordering `o`, assuming `b` has been
prepared with `prepare`.

!!! warning
    Comparing a prepared element `prepare(o1, x)` under a different ordering `o2`
    is undefined behavior and may, for example, result in segfaults.

See also `lt`, `lt_prepared`.
"""
@propagate_inbounds lt_prepared_2(o::Ordering, a, b) = lt_prepared(o, prepare(o, a), b)

"""
    prepare(o::Ordering, x)

Prepare an element `x` for efficient comparison with `lt_prepared`.

`lt(o::MyOrdering, a, b)` and `lt_prepared(o, prepare(o, a), prepare(o, b))` are
equivalent. They must have indistinguishable behavior and have the same performance
characteristics.

If you define `prepare` on a custom `Ordering`, you should also define `lt_prepared` and
should not define `lt` for that order.

!!! warning
    Comparing a prepared element `prepare(o1, x)` under a different ordering `o2`
    is undefined behavior and may, for example, result in segfaults.
"""
function prepare end

# Fallbacks
@propagate_inbounds lt_prepared(o::Ordering, a, b) = lt(o, a, b) # TODO: remove this in Julia 2.0
prepare(o::Ordering, x) = x
# Not defining this because it would cause a stack overflow for invalid `Ordering`s:
# lt(o::Ordering, a, b) = lt_prepared(o, prepare(o, a), prepare(o, b))

# Forward
lt(o::ForwardOrdering, a, b) = isless(a, b)

# Reverse
prepare(o::ReverseOrdering, x) = prepare(o.fwd, x)
lt_prepared(o::ReverseOrdering, a, b) = lt_prepared(o.fwd, b, a)
lt(o::ReverseOrdering, a, b) = lt_prepared(o, prepare(o, a), prepare(o, b))

# By
prepare(o::By, x) = prepare(o.order, o.by(x))
lt_prepared(o::By, a, b) = lt_prepared(o.order, a, b)
lt(o::By, a, b) = lt_prepared(o, prepare(o, a), prepare(o, b))

# Perm
@propagate_inbounds prepare(o::Perm, i) = (prepare(o.order, o.data[i]), i)
lt_prepared(p::Perm, (da, a), (db, b)) =
    (lt_prepared(p.order, da, db)::Bool) | (!(lt_prepared(p.order, db, da)::Bool) & (a < b))
@propagate_inbounds lt(o::Perm, a, b) = lt_prepared(o, prepare(o, a), prepare(o, b))

## Lt
lt(o::Lt, a, b) = o.lt(a, b)


_ord(lt::typeof(isless), by, order::Ordering)                         = _by(by, order)
_ord(lt::typeof(isless), by, order::ForwardOrdering)                  = _by(by, order)  # disambiguation
_ord(lt::typeof(isless), by, order::ReverseOrdering{ForwardOrdering}) = _by(by, order)  # disambiguation
_ord(lt,                 by, order::ForwardOrdering)                  = _by(by, Lt(lt))
_ord(lt,                 by, order::ReverseOrdering{ForwardOrdering}) = reverse(_by(by, Lt(lt)))
_ord(lt,                 by, order::Ordering) = error("Passing both lt= and order= arguments is ambiguous; please pass order=Forward or order=Reverse (or leave default)")
_by(by, order::Ordering) = By(by, order)
_by(::typeof(identity), order::Ordering) = order

"""
    ord(lt, by, rev::Union{Bool, Nothing}, order::Ordering=Forward)

Construct an [`Ordering`](@ref) object from the same arguments used by
[`sort!`](@ref).
Elements are first transformed by the function `by` (which may be
[`identity`](@ref)) and are then compared according to either the function `lt`
or an existing ordering `order`. `lt` should be [`isless`](@ref) or a function
that obeys the same rules as the `lt` parameter of [`sort!`](@ref). Finally,
the resulting order is reversed if `rev=true`.

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
