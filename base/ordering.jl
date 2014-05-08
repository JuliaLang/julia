module Order

## notions of element ordering ##

export # not exported by Base
    Ordering, Forward, Reverse, Lexicographic,
    By, Lt, Perm,
    ReverseOrdering, ForwardOrdering, LexicographicOrdering,
    DirectOrdering,
    lt, uint_mapping, ord, ordtype

abstract Ordering

immutable ForwardOrdering <: Ordering end
immutable ReverseOrdering{Fwd<:Ordering} <: Ordering
    fwd::Fwd
end

ReverseOrdering(rev::ReverseOrdering) = rev.fwd
ReverseOrdering{Fwd}(fwd::Fwd) = ReverseOrdering{Fwd}(fwd)

typealias DirectOrdering Union(ForwardOrdering,ReverseOrdering{ForwardOrdering})

const Forward = ForwardOrdering()
const Reverse = ReverseOrdering(Forward)

immutable LexicographicOrdering <: Ordering end
const Lexicographic = LexicographicOrdering()

immutable By <: Ordering
    by::Function
end

immutable Lt <: Ordering
    lt::Function
end

immutable Perm{O<:Ordering,V<:AbstractVector} <: Ordering
    order::O
    data::V
end
Perm{O<:Ordering,V<:AbstractVector}(o::O,v::V) = Perm{O,V}(o,v)

lt(o::ForwardOrdering,       a, b) = isless(a,b)
lt(o::ReverseOrdering,       a, b) = lt(o.fwd,b,a)
lt(o::By,                    a, b) = isless(o.by(a),o.by(b))
lt(o::Lt,                    a, b) = o.lt(a,b)
lt(o::LexicographicOrdering, a, b) = lexcmp(a,b) < 0

function lt(p::Perm, a::Int, b::Int)
    lt(p.order, p.data[a], p.data[b]) ? true :
    lt(p.order, p.data[b], p.data[a]) ? false : a < b
end
function lt(p::Perm{LexicographicOrdering}, a::Int, b::Int)
    c = lexcmp(p.data[a], p.data[b])
    c != 0 ? c < 0 : a < b
end

# Map a bits-type to an unsigned int, maintaining sort order
uint_mapping(::ForwardOrdering, x::Unsigned) = x
uint_mapping(::ForwardOrdering, x::Int8)     = uint8  (x $ typemin(Int8))
uint_mapping(::ForwardOrdering, x::Int16)    = uint16 (x $ typemin(Int16))
uint_mapping(::ForwardOrdering, x::Int32)    = uint32 (x $ typemin(Int32))
uint_mapping(::ForwardOrdering, x::Int64)    = uint64 (x $ typemin(Int64))
uint_mapping(::ForwardOrdering, x::Int128)   = uint128(x $ typemin(Int128))
uint_mapping(::ForwardOrdering, x::Float32)  = (y = reinterpret(Int32, x); uint32(y < 0 ? ~y : (y $ typemin(Int32))))
uint_mapping(::ForwardOrdering, x::Float64)  = (y = reinterpret(Int64, x); uint64(y < 0 ? ~y : (y $ typemin(Int64))))

uint_mapping{Fwd}(::ReverseOrdering{Fwd}, x) = ~uint_mapping(Fwd, x)
#uint_mapping{T<:Real}(::ReverseOrdering{ForwardOrdering}, x::T) = ~uint_mapping(Forward, x)  ## Manually inlined
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Unsigned) = ~x
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Int8)     = ~uint8  (x $ typemin(Int8))
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Int16)    = ~uint16 (x $ typemin(Int16))
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Int32)    = ~uint32 (x $ typemin(Int32))
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Int64)    = ~uint64 (x $ typemin(Int64))
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Int128)   = ~uint128(x $ typemin(Int128))
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Float32)  = (y = reinterpret(Int32, x); uint32(y < 0 ? y : ~(y $ typemin(Int32))))
uint_mapping(::ReverseOrdering{ForwardOrdering}, x::Float64)  = (y = reinterpret(Int64, x); uint64(y < 0 ? y : ~(y $ typemin(Int64))))

uint_mapping(o::By,   x     ) = uint_mapping(Forward, o.by(x))
uint_mapping(o::Perm, i::Int) = uint_mapping(o.order, o.data[i])
uint_mapping(o::Lt,   x     ) = error("uint_mapping does not work with general Lt Orderings")

ordtype(o::ReverseOrdering, vs::AbstractArray) = ordtype(o.fwd, vs)
ordtype(o::Perm,            vs::AbstractArray) = ordtype(o.order, o.data)
# TODO: here, we really want the return type of o.by, without calling it
ordtype(o::By,              vs::AbstractArray) = try typeof(o.by(vs[1])) catch Any end
ordtype(o::Ordering,        vs::AbstractArray) = eltype(vs)

function ord(lt::Function, by::Function, rev::Bool, order::Ordering=Forward)
    order == Forward || order == Lexicographic ||
        Base.warn_once("the `order` keyword is deprecated, use `lt`, `by` and `rev` instead")
    o = (lt===isless) & (by===identity) ? order  :
        (lt===isless) & (by!==identity) ? By(by) :
        (lt!==isless) & (by===identity) ? Lt(lt) :
                                          Lt((x,y)->lt(by(x),by(y)))
    rev ? ReverseOrdering(o) : o
end

end
