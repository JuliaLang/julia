# This file is a part of Julia. License is MIT: https://julialang.org/license

#  Floating point optimizations
module Float
using ..Sort
using ...Order
using ..Base: @inbounds, AbstractVector, Vector, last, firstindex, lastindex, Missing, Type, reinterpret

import Core.Intrinsics: slt_int
import ..StandardSortingAlgorithms: sort!, UIntMappable, uint_map, uint_unmap
import ...Order: lt, DirectOrdering

const Floats = Union{Float32,Float64}
const FPSortable = Union{ # Mixed Float32 and Float64 are not allowed.
    AbstractVector{Union{Float32, Missing}},
    AbstractVector{Union{Float64, Missing}},
    AbstractVector{Float32},
    AbstractVector{Float64},
    AbstractVector{Missing}}

struct Left <: Ordering end
struct Right <: Ordering end

left(::DirectOrdering) = Left()
right(::DirectOrdering) = Right()

left(o::Perm) = Perm(left(o.order), o.data)
right(o::Perm) = Perm(right(o.order), o.data)

lt(::Left, x::T, y::T) where {T<:Floats} = slt_int(y, x)
lt(::Right, x::T, y::T) where {T<:Floats} = slt_int(x, y)

uint_map(x::Float32, ::Left) = ~reinterpret(UInt32, x)
uint_unmap(::Type{Float32}, u::UInt32, ::Left) = reinterpret(Float32, ~u)
uint_map(x::Float32, ::Right) = reinterpret(UInt32, x)
uint_unmap(::Type{Float32}, u::UInt32, ::Right) = reinterpret(Float32, u)
UIntMappable(::Type{Float32}, ::Union{Left, Right}) = UInt32

uint_map(x::Float64, ::Left) = ~reinterpret(UInt64, x)
uint_unmap(::Type{Float64}, u::UInt64, ::Left) = reinterpret(Float64, ~u)
uint_map(x::Float64, ::Right) = reinterpret(UInt64, x)
uint_unmap(::Type{Float64}, u::UInt64, ::Right) = reinterpret(Float64, u)
UIntMappable(::Type{Float64}, ::Union{Left, Right}) = UInt64

isnan(o::DirectOrdering, x::Floats) = (x!=x)
isnan(o::DirectOrdering, x::Missing) = false
isnan(o::Perm, i::Integer) = isnan(o.order,o.data[i])

ismissing(o::DirectOrdering, x::Floats) = false
ismissing(o::DirectOrdering, x::Missing) = true
ismissing(o::Perm, i::Integer) = ismissing(o.order,o.data[i])

allowsmissing(::AbstractVector{T}, ::DirectOrdering) where {T} = T >: Missing
allowsmissing(::AbstractVector{<:Integer},
              ::Perm{<:DirectOrdering,<:AbstractVector{T}}) where {T} =
    T >: Missing

function specials2left!(testf::Function, v::AbstractVector, o::Ordering,
                        lo::Integer=firstindex(v), hi::Integer=lastindex(v))
    i = lo
    @inbounds while i <= hi && testf(o,v[i])
        i += 1
    end
    j = i + 1
    @inbounds while j <= hi
        if testf(o,v[j])
            v[i], v[j] = v[j], v[i]
            i += 1
        end
        j += 1
    end
    return i, hi
end
function specials2right!(testf::Function, v::AbstractVector, o::Ordering,
                         lo::Integer=firstindex(v), hi::Integer=lastindex(v))
    i = hi
    @inbounds while lo <= i && testf(o,v[i])
        i -= 1
    end
    j = i - 1
    @inbounds while lo <= j
        if testf(o,v[j])
            v[i], v[j] = v[j], v[i]
            i -= 1
        end
        j -= 1
    end
    return lo, i
end

function specials2left!(v::AbstractVector, a::Algorithm, o::Ordering)
    lo, hi = firstindex(v), lastindex(v)
    if allowsmissing(v, o)
        i, _ = specials2left!((v, o) -> ismissing(v, o) || isnan(v, o), v, o, lo, hi)
        sort!(v, lo, i-1, a, o)
        return i, hi
    else
        return specials2left!(isnan, v, o, lo, hi)
    end
end
function specials2right!(v::AbstractVector, a::Algorithm, o::Ordering)
    lo, hi = firstindex(v), lastindex(v)
    if allowsmissing(v, o)
        _, i = specials2right!((v, o) -> ismissing(v, o) || isnan(v, o), v, o, lo, hi)
        sort!(v, i+1, hi, a, o)
        return lo, i
    else
        return specials2right!(isnan, v, o, lo, hi)
    end
end

specials2end!(v::AbstractVector, a::Algorithm, o::ForwardOrdering) =
    specials2right!(v, a, o)
specials2end!(v::AbstractVector, a::Algorithm, o::ReverseOrdering) =
    specials2left!(v, a, o)
specials2end!(v::AbstractVector{<:Integer}, a::Algorithm, o::Perm{<:ForwardOrdering}) =
    specials2right!(v, a, o)
specials2end!(v::AbstractVector{<:Integer}, a::Algorithm, o::Perm{<:ReverseOrdering}) =
    specials2left!(v, a, o)

issignleft(o::ForwardOrdering, x::Floats) = lt(o, x, zero(x))
issignleft(o::ReverseOrdering, x::Floats) = lt(o, x, -zero(x))
issignleft(o::Perm, i::Integer) = issignleft(o.order, o.data[i])

function fpsort!(v::AbstractVector, a::Algorithm, o::Ordering,
        t::Union{AbstractVector, Nothing}=nothing)
    # fpsort!'s optimizations speed up comparisons, of which there are O(nlogn).
    # The overhead is O(n). For n < 10, it's not worth it.
    length(v) < 10 && return sort!(v, firstindex(v), lastindex(v), SMALL_ALGORITHM, o, t)

    i, j = lo, hi = specials2end!(v,a,o)
    @inbounds while true
        while i <= j &&  issignleft(o,v[i]); i += 1; end
        while i <= j && !issignleft(o,v[j]); j -= 1; end
        i <= j || break
        v[i], v[j] = v[j], v[i]
        i += 1; j -= 1
    end
    sort!(v, lo, j,  a, left(o), t)
    sort!(v, i,  hi, a, right(o), t)
    return v
end


fpsort!(v::AbstractVector, a::Sort.PartialQuickSort, o::Ordering) =
    sort!(v, firstindex(v), lastindex(v), a, o)

function sort!(v::FPSortable, a::Algorithm, o::DirectOrdering,
        t::Union{FPSortable, Nothing}=nothing)
    fpsort!(v, a, o, t)
end
function sort!(v::AbstractVector{<:Union{Signed, Unsigned}}, a::Algorithm,
        o::Perm{<:DirectOrdering,<:FPSortable}, t::Union{AbstractVector, Nothing}=nothing)
    fpsort!(v, a, o, t)
end

end # module Float
