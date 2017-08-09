# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    ConjArray(array)

A lazy-view wrapper of an `AbstractArray`, taking the elementwise complex conjugate. This
type is usually constructed (and unwrapped) via the [`conj`](@ref) function (or related
[`ctranspose`](@ref)), but currently this is the default behavior for `RowVector` only. For
other arrays, the `ConjArray` constructor can be used directly.

# Examples
```jldoctest
julia> [1+im, 1-im]'
1×2 RowVector{Complex{Int64},ConjArray{Complex{Int64},1,Array{Complex{Int64},1}}}:
 1-1im  1+1im

julia> ConjArray([1+im 0; 0 1-im])
2×2 ConjArray{Complex{Int64},2,Array{Complex{Int64},2}}:
 1-1im  0+0im
 0+0im  1+1im
```
"""
struct ConjArray{T,N,A<:AbstractArray} <: AbstractArray{T,N}
    parent::A
end

@inline ConjArray(a::AbstractArray{T,N}) where {T,N} = ConjArray{conj_type(T),N,typeof(a)}(a)

const ConjVector{T,V<:AbstractVector} = ConjArray{T,1,V}
@inline ConjVector(v::AbstractVector{T}) where {T} = ConjArray{conj_type(T),1,typeof(v)}(v)

const ConjMatrix{T,M<:AbstractMatrix} = ConjArray{T,2,M}
@inline ConjMatrix(m::AbstractMatrix{T}) where {T} = ConjArray{conj_type(T),2,typeof(m)}(m)

# This type can cause the element type to change under conjugation - e.g. an array of complex arrays.
@inline conj_type(x) = conj_type(typeof(x))
@inline conj_type(::Type{T}) where {T} = promote_op(conj, T)

@inline parent(c::ConjArray) = c.parent
@inline parent_type(c::ConjArray) = parent_type(typeof(c))
@inline parent_type(::Type{ConjArray{T,N,A}}) where {T,N,A} = A

@inline size(a::ConjArray) = size(a.parent)
IndexStyle(::CA) where {CA<:ConjArray} = IndexStyle(parent_type(CA))
IndexStyle(::Type{CA}) where {CA<:ConjArray} = IndexStyle(parent_type(CA))

@propagate_inbounds getindex(a::ConjArray{T,N}, i::Int) where {T,N} = conj(getindex(a.parent, i))
@propagate_inbounds getindex(a::ConjArray{T,N}, i::Vararg{Int,N}) where {T,N} = conj(getindex(a.parent, i...))
@propagate_inbounds setindex!(a::ConjArray{T,N}, v, i::Int) where {T,N} = setindex!(a.parent, conj(v), i)
@propagate_inbounds setindex!(a::ConjArray{T,N}, v, i::Vararg{Int,N}) where {T,N} = setindex!(a.parent, conj(v), i...)

@inline similar(a::ConjArray, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(a), T, dims)

# Currently, this is default behavior for RowVector only
@inline conj(a::ConjArray) = parent(a)

# Helper functions, currently used by RowVector
@inline _conj(a::AbstractArray) = ConjArray(a)
@inline _conj(a::AbstractArray{T}) where {T<:Real} = a
@inline _conj(a::ConjArray) = parent(a)
@inline _conj(a::ConjArray{T}) where {T<:Real} = parent(a)
