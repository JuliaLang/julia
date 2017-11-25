# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    RowVector(vector)

A lazy-view wrapper of an [`AbstractVector`](@ref), which turns a length-`n` vector into a `1×n`
shaped row vector and represents the transpose of a vector (the elements are also transposed
recursively). This type is usually constructed (and unwrapped) via the [`transpose`](@ref)
function or `.'` operator (or related [`adjoint`](@ref) or `'` operator).

By convention, a vector can be multiplied by a matrix on its left (`A * v`) whereas a row
vector can be multiplied by a matrix on its right (such that `v.' * A = (A.' * v).'`). It
differs from a `1×n`-sized matrix by the facts that its transpose returns a vector and the
inner product `v1.' * v2` returns a scalar, but will otherwise behave similarly.
"""
struct RowVector{T,V<:AbstractVector{T}} <: AbstractMatrix{T}
    vec::V
end

parent(rowvec::RowVector) = rowvec.vec
vec(rowvec::RowVector) = rowvec.vec

# Constructors that take a vector
@inline RowVector(vec::AbstractVector{T}) where {T} = RowVector{T,typeof(vec)}(vec)
@inline RowVector{T}(vec::AbstractVector{T}) where {T} = RowVector{T,typeof(vec)}(vec)

# Conversion of underlying storage
function convert(::Type{RowVector{T,V}}, rowvec::RowVector) where {T,V<:AbstractVector{T}}
    RowVector{T,V}(convert(V,parent(rowvec)))
end

# similar tries to maintain the RowVector wrapper and the parent type
@inline similar(rowvec::RowVector) = RowVector(similar(parent(rowvec)))
@inline similar(rowvec::RowVector, ::Type{T}) where {T} = RowVector(similar(parent(rowvec), T))

# Resizing similar currently loses its RowVector property.
@inline similar(rowvec::RowVector, ::Type{T}, dims::Dims{N}) where {T,N} = similar(parent(rowvec), T, dims)

# Basic methods
"""
transpose(v::AbstractVector)

The transposition operator (`.'`).

# Examples
```jldoctest
julia> v = [1,2,3]
3-element Array{Int64,1}:
1
2
3

julia> transpose(v)
1×3 RowVector{Int64,Array{Int64,1}}:
1  2  3
```
"""
@inline transpose(vec::AbstractVector) = RowVector(vec)
@inline transpose(rowvec::RowVector) = parent(rowvec)

# AbstractArray interface
@inline length(rowvec::RowVector) =  length(parent(rowvec))
@inline size(rowvec::RowVector) = (1, length(parent(rowvec)))
@inline size(rowvec::RowVector, d) = ifelse(d==2, length(parent(rowvec)), 1)
@inline indices(rowvec::RowVector) = (Base.OneTo(1), indices(parent(rowvec))[1])
@inline indices(rowvec::RowVector, d) = ifelse(d == 2, indices(parent(rowvec))[1], Base.OneTo(1))
IndexStyle(::RowVector) = IndexLinear()
IndexStyle(::Type{<:RowVector}) = IndexLinear()

@propagate_inbounds getindex(rowvec::RowVector, i::Int) = parent(rowvec)[i]
@propagate_inbounds setindex!(rowvec::RowVector, v, i::Int) = setindex!(parent(rowvec), v, i)

# Keep a RowVector where appropriate
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, i::Int) = parent(rowvec)[i]
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, inds::AbstractArray{Int}) = RowVector(parent(rowvec)[inds])
@propagate_inbounds getindex(rowvec::RowVector, ::Colon, ::Colon) = RowVector(parent(rowvec)[:])

# helper function for below
@inline to_vec(rowvec::RowVector) = parent(rowvec)
@inline to_vec(x::Number) = x
@inline to_vecs(rowvecs...) = (map(to_vec, rowvecs)...,)

# map: Preserve the RowVector by un-wrapping and re-wrapping
@inline map(f, rowvecs::RowVector...) = RowVector(map(f, to_vecs(rowvecs...)...))

# broacast (other combinations default to higher-dimensional array)
# (in future, should use broadcast infrastructure to manage this?)
@inline function broadcast(f, rowvecs::Union{Number,RowVector}...)
    RowVector(broadcast(f, to_vecs(rowvecs...)...))
end

# Horizontal concatenation #
@inline hcat(X::RowVector...) = transpose(vcat(map(transpose, X)...))
@inline hcat(X::Union{RowVector,Number}...) = transpose(vcat(map(transpose, X)...))

@inline function typed_hcat(::Type{T}, X::RowVector...) where {T}
    transpose(typed_vcat(T, map(transpose, X)...))
end
@inline function typed_hcat(::Type{T}, X::Union{RowVector,Number}...) where {T}
    transpose(typed_vcat(T, map(transpose, X)...))
end
