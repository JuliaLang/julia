# This file is a part of Julia. License is MIT: https://julialang.org/license

# SizedArrays

# This test file defines an array wrapper with statical size. It can be used to
# test the action of LinearAlgebra with non-number eltype.

module SizedArrays

import Base: +, *, ==

export SizedArray

struct SizedArray{SZ,T,N,A<:AbstractArray} <: AbstractArray{T,N}
    data::A
    function SizedArray{SZ}(data::AbstractArray{T,N}) where {SZ,T,N}
        SZ == size(data) || throw(ArgumentError("size mismatch!"))
        new{SZ,T,N,typeof(data)}(data)
    end
    function SizedArray{SZ,T,N,A}(data::AbstractArray{T,N}) where {SZ,T,N,A}
        SZ == size(data) || throw(ArgumentError("size mismatch!"))
        new{SZ,T,N,A}(A(data))
    end
end
Base.convert(::Type{SizedArray{SZ,T,N,A}}, data::AbstractArray) where {SZ,T,N,A} = SizedArray{SZ,T,N,A}(data)

# Minimal AbstractArray interface
Base.size(a::SizedArray) = size(typeof(a))
Base.size(::Type{<:SizedArray{SZ}}) where {SZ} = SZ
Base.getindex(A::SizedArray, i...) = getindex(A.data, i...)
Base.zero(::Type{T}) where T <: SizedArray = SizedArray{size(T)}(zeros(eltype(T), size(T)))
+(S1::SizedArray{SZ}, S2::SizedArray{SZ}) where {SZ} = SizedArray{SZ}(S1.data + S2.data)
==(S1::SizedArray{SZ}, S2::SizedArray{SZ}) where {SZ} = S1.data == S2.data
function *(S1::SizedArray, S2::SizedArray)
    0 < ndims(S1) < 3 && 0 < ndims(S2) < 3 && size(S1, 2) == size(S2, 1) || throw(ArgumentError("size mismatch!"))
    data = S1.data * S2.data
    SZ = ndims(data) == 1 ? (size(S1, 1), ) : (size(S1, 1), size(S2, 2))
    SizedArray{SZ}(data)
end
end
