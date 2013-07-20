module Order

## notions of element ordering ##

export # not exported by Base
    Ordering, Forward, By, Lt, Perm,
    ReverseOrdering, ForwardOrdering,
    lt, ord
    # Reverse, # TODO: clashes with Reverse iterator

abstract Ordering

immutable ForwardOrdering <: Ordering end
immutable ReverseOrdering{Fwd<:Ordering} <: Ordering
    fwd::Fwd
end

ReverseOrdering(rev::ReverseOrdering) = rev.fwd
ReverseOrdering{Fwd}(fwd::Fwd) = ReverseOrdering{Fwd}(fwd)

immutable By <: Ordering
    by::Function
end
immutable Lt <: Ordering
    lt::Function
end

const Forward = ForwardOrdering()
const Reverse = ReverseOrdering(Forward)

lt(o::ForwardOrdering, a, b) = isless(a,b)
lt(o::ReverseOrdering, a, b) = lt(o.fwd,b,a)
lt(o::By,              a, b) = isless(o.by(a),o.by(b))
lt(o::Lt,              a, b) = o.lt(a,b)

immutable Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    order::O
    data::V
end
Perm{O<:Ordering,V<:AbstractVector}(o::O,v::V) = Perm{O,V}(o,v)

lt(p::Perm, a, b) = lt(p.order, p.data[a], p.data[b])


function ord(lt::Function, by::Function, rev::Bool, order::Ordering=Forward)
    o = (lt===isless) & (by===identity) ? order  :
        (lt===isless) & (by!==identity) ? By(by) :
        (lt!==isless) & (by===identity) ? Lt(lt) :
                                          Lt((x,y)->lt(by(x),by(y)))
    rev ? ReverseOrdering(o) : o
end

end