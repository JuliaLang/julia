# This file is a part of Julia. License is MIT: https://julialang.org/license

## Tests for the abstract array interfaces and operations with arrays
## with different indexing rules

# A custom linear fast array type with 24 elements that doesn't rely upon Array storage
mutable struct T24Linear{T,N,dims} <: AbstractArray{T,N}
    v1::T;  v2::T;  v3::T;  v4::T;  v5::T;  v6::T;  v7::T;  v8::T
    v9::T;  v10::T; v11::T; v12::T; v13::T; v14::T; v15::T; v16::T
    v17::T; v18::T; v19::T; v20::T; v21::T; v22::T; v23::T; v24::T
    T24Linear{T,N,d}() where {T,N,d} =
        (prod(d) == 24 || throw(DimensionMismatch("T24Linear must have 24 elements")); new())
    function T24Linear{T,N,d}(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,
                              v13,v14,v15,v16,v17,v18,v19,v20,v21,v22,v23,v24) where {T,N,d}
        prod(d) == 24 || throw(DimensionMismatch("T24Linear must have 24 elements"))
        new(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20,v21,v22,v23,v24)
    end
end

T24Linear(::Type{T}, dims::Int...) where T = T24Linear(T, dims)
T24Linear(::Type{T}, dims::NTuple{N,Int}) where {T,N} = T24Linear{T,N,dims}()

T24Linear(     X::AbstractArray{T,N}) where {T,N  } = T24Linear{T,N}(X)
T24Linear{T  }(X::AbstractArray{_,N}) where {T,N,_} = T24Linear{T,N}(X)
T24Linear{T,N}(X::AbstractArray     ) where {T,N  } = T24Linear{T,N,size(X)}(X...)

Base.size(::T24Linear{T,N,dims}) where {T,N,dims} = dims
import Base: IndexLinear
Base.IndexStyle(::Type{A}) where {A<:T24Linear} = IndexLinear()
Base.getindex(A::T24Linear, i::Int) = getfield(A, i)
Base.setindex!(A::T24Linear{T}, v, i::Int) where {T} = setfield!(A, i, convert(T, v))

# A custom linear slow sparse-like array that relies upon Dict for its storage
struct TSlow{T,N} <: AbstractArray{T,N}
    data::Dict{NTuple{N,Int}, T}
    dims::NTuple{N,Int}
end
TSlow(::Type{T}, dims::Int...) where {T} = TSlow(T, dims)
TSlow(::Type{T}, dims::NTuple{N,Int}) where {T,N} = TSlow{T,N}(Dict{NTuple{N,Int}, T}(), dims)

TSlow{T,N}(X::TSlow{T,N})         where {T,N  } = X
TSlow(     X::AbstractArray{T,N}) where {T,N  } = TSlow{T,N}(X)
TSlow{T  }(X::AbstractArray{_,N}) where {T,N,_} = TSlow{T,N}(X)
TSlow{T,N}(X::AbstractArray     ) where {T,N  } = begin
    A = TSlow(T, size(X))
    for I in CartesianIndices(X)
        A[Tuple(I)...] = X[Tuple(I)...]
    end
    A
end

Base.size(A::TSlow) = A.dims
Base.similar(A::TSlow, ::Type{T}, dims::Dims) where {T} = TSlow(T, dims)
Base.IndexStyle(::Type{A}) where {A<:TSlow} = IndexCartesian()
Base.getindex(A::TSlow{T,N}, i::Vararg{Int,N}) where {T,N} = get(A.data, i, zero(T))
Base.setindex!(A::TSlow{T,N}, v, i::Vararg{Int,N}) where {T,N} = (A.data[i] = v)
