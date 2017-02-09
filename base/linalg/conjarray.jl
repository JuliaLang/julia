# This file is a part of Julia. License is MIT: http://julialang.org/license

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
immutable ConjArray{T, N, A <: AbstractArray} <: AbstractArray{T, N}
    parent::A
end

@inline ConjArray{T,N}(a::AbstractArray{T,N}) = ConjArray{conj_type(T), N, typeof(a)}(a)

typealias ConjVector{T, V <: AbstractVector} ConjArray{T, 1, V}
@inline ConjVector{T}(v::AbstractVector{T}) = ConjArray{conj_type(T), 1, typeof(v)}(v)

typealias ConjMatrix{T, M <: AbstractMatrix} ConjArray{T, 2, M}
@inline ConjMatrix{T}(m::AbstractMatrix{T}) = ConjArray{conj_type(T), 2, typeof(m)}(m)

# This type can cause the element type to change under conjugation - e.g. an array of complex arrays.
@inline conj_type(x) = conj_type(typeof(x))
@inline conj_type{T}(::Type{T}) = promote_op(conj, T)

@inline parent(c::ConjArray) = c.parent
@inline parent_type(c::ConjArray) = parent_type(typeof(c))
@inline parent_type{T,N,A}(::Type{ConjArray{T,N,A}}) = A

@inline size(a::ConjArray) = size(a.parent)
linearindexing{CA <: ConjArray}(::CA) = linearindexing(parent_type(CA))
linearindexing{CA <: ConjArray}(::Type{CA}) = linearindexing(parent_type(CA))

@propagate_inbounds getindex{T,N}(a::ConjArray{T,N}, i::Int) = conj(getindex(a.parent, i))
@propagate_inbounds getindex{T,N}(a::ConjArray{T,N}, i::Vararg{Int,N}) = conj(getindex(a.parent, i...))
@propagate_inbounds setindex!{T,N}(a::ConjArray{T,N}, v, i::Int) = setindex!(a.parent, conj(v), i)
@propagate_inbounds setindex!{T,N}(a::ConjArray{T,N}, v, i::Vararg{Int,N}) = setindex!(a.parent, conj(v), i...)

@inline similar{T,N}(a::ConjArray, ::Type{T}, dims::Dims{N}) = similar(parent(a), T, dims)

# Currently, this is default behavior for RowVector only
@inline conj(a::ConjArray) = parent(a)

# Helper functions, currently used by RowVector
@inline _conj(a::AbstractArray) = ConjArray(a)
@inline _conj{T<:Real}(a::AbstractArray{T}) = a
@inline _conj(a::ConjArray) = parent(a)
@inline _conj{T<:Real}(a::ConjArray{T}) = parent(a)
