# This file is a part of Julia. License is MIT: https://julialang.org/license

# SimpleImmutableArrays (arrays that implement getindex but not setindex!)

# This test file defines an array wrapper that is immutable. It can be used to
# test the action of methods on immutable arrays.

module SimpleImmutableArrays

export SimpleImmutableArray

"An immutable wrapper type for arrays."
struct SimpleImmutableArray{T,N,A<:AbstractArray} <: AbstractArray{T,N}
    data::A
end

SimpleImmutableArray(data::AbstractArray{T,N}) where {T,N} = SimpleImmutableArray{T,N,typeof(data)}(data)

# Minimal AbstractArray interface
Base.size(A::SimpleImmutableArray) = size(A.data)
Base.size(A::SimpleImmutableArray, d) = size(A.data, d)
Base.getindex(A::SimpleImmutableArray, i...) = getindex(A.data, i...)

# The immutable array remains immutable after conversion to AbstractArray
AbstractArray{T}(A::SimpleImmutableArray) where {T} = SimpleImmutableArray(AbstractArray{T}(A.data))
AbstractArray{T,N}(A::SimpleImmutableArray{S,N}) where {S,T,N} = SimpleImmutableArray(AbstractArray{T,N}(A.data))

end