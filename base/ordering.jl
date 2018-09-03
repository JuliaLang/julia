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

abstract type Ordering end

struct ForwardOrdering <: Ordering end
struct ReverseOrdering{Fwd<:Ordering} <: Ordering
    fwd::Fwd
end

ReverseOrdering(rev::ReverseOrdering) = rev.fwd
ReverseOrdering(fwd::Fwd) where {Fwd} = ReverseOrdering{Fwd}(fwd)

const DirectOrdering = Union{ForwardOrdering,ReverseOrdering{ForwardOrdering}}

const Forward = ForwardOrdering()
const Reverse = ReverseOrdering(Forward)

struct By{T} <: Ordering
    by::T
end

struct Lt{T} <: Ordering
    lt::T
end

struct Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    order::O
    data::V
end

lt(o::ForwardOrdering,       a, b) = isless(a,b)
lt(o::ReverseOrdering,       a, b) = lt(o.fwd,b,a)
lt(o::By,                    a, b) = isless(o.by(a),o.by(b))
lt(o::Lt,                    a, b) = o.lt(a,b)

@propagate_inbounds function lt(p::Perm, a::Integer, b::Integer)
    da = p.data[a]
    db = p.data[b]
    lt(p.order, da, db) | (!lt(p.order, db, da) & (a < b))
end

ordtype(o::ReverseOrdering, vs::AbstractArray) = ordtype(o.fwd, vs)
ordtype(o::Perm,            vs::AbstractArray) = ordtype(o.order, o.data)
# TODO: here, we really want the return type of o.by, without calling it
ordtype(o::By,              vs::AbstractArray) = try typeof(o.by(vs[1])) catch; Any end
ordtype(o::Ordering,        vs::AbstractArray) = eltype(vs)

_ord(lt::typeof(isless), by::typeof(identity), order::Ordering) = order
_ord(lt::typeof(isless), by,                   order::Ordering) = By(by)
_ord(lt,                 by::typeof(identity), order::Ordering) = Lt(lt)
_ord(lt,                 by,                   order::Ordering) = Lt((x,y)->lt(by(x),by(y)))

ord(lt, by, rev::Nothing, order::Ordering=Forward) = _ord(lt, by, order)

function ord(lt, by, rev::Bool, order::Ordering=Forward)
    o = _ord(lt, by, order)
    return rev ? ReverseOrdering(o) : o
end

end
