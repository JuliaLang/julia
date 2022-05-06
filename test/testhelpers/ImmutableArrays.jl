# This file is a part of Julia. License is MIT: https://julialang.org/license

# ImmutableArrays (arrays that implement getindex but not setindex!)

# This test file defines an array wrapper that is immutable. It can be used to
# test the action of methods on immutable arrays.

module ImmutableArrays

export ImmutableArray

"An immutable wrapper type for arrays."
struct ImmutableArray{T,N,A<:AbstractArray} <: AbstractArray{T,N}
    data::A
end

ImmutableArray(data::AbstractArray{T,N}) where {T,N} = ImmutableArray{T,N,typeof(data)}(data)

# Minimal AbstractArray interface
Base.size(A::ImmutableArray) = size(A.data)
Base.size(A::ImmutableArray, d) = size(A.data, d)
Base.getindex(A::ImmutableArray, i...) = getindex(A.data, i...)

# The immutable array remains immutable after conversion to AbstractArray
AbstractArray{T}(A::ImmutableArray) where {T} = ImmutableArray(AbstractArray{T}(A.data))
AbstractArray{T,N}(A::ImmutableArray{S,N}) where {S,T,N} = ImmutableArray(AbstractArray{T,N}(A.data))

end
