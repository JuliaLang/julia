# This file is a part of Julia. License is MIT: https://julialang.org/license

# TODO: remove this type stub between 0.7 and 1.0

"""
    ConjArray(array)

A lazy-view wrapper of an `AbstractArray`, taking the elementwise complex conjugate. This
type is usually constructed (and unwrapped) via the [`conj`](@ref) function (or related
[`adjoint`](@ref)), but currently this is the default behavior for `RowVector` only. For
other arrays, the `ConjArray` constructor can be used directly.

# Examples
```jldoctest
julia> ConjArray([1+im 0; 0 1-im])
2×2 ConjArray{Complex{Int64},2,Array{Complex{Int64},2}}:
 1-1im  0+0im
 0+0im  1+1im
```
"""
struct ConjArray{T,N,A<:AbstractArray} <: AbstractArray{T,N}
    parent::A
end
const ConjVector{T,V<:AbstractVector} = ConjArray{T,1,V}
const ConjMatrix{T,M<:AbstractMatrix} = ConjArray{T,2,M}
