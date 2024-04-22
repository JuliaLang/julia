# This file is a part of Julia. License is MIT: https://julialang.org/license

# SizedArrays

# This test file defines an array wrapper with statical size. It can be used to
# test the action of LinearAlgebra with non-number eltype.

module SizedArrays

import Base: +, *, ==

using LinearAlgebra
import LinearAlgebra: mul!

export SizedArray

struct SOneTo{N} <: AbstractUnitRange{Int} end
SOneTo(N) = SOneTo{N}()
Base.length(::SOneTo{N}) where {N} = N
Base.size(r::SOneTo) = (length(r),)
Base.axes(r::SOneTo) = (r,)
Base.first(::SOneTo) = 1
Base.last(r::SOneTo) = length(r)
Base.show(io::IO, r::SOneTo) = print(io, "SOneTo(", length(r), ")")

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
SizedMatrix{SZ,T,A<:AbstractArray} = SizedArray{SZ,T,2,A}
SizedVector{SZ,T,A<:AbstractArray} = SizedArray{SZ,T,1,A}
Base.convert(::Type{SizedArray{SZ,T,N,A}}, data::AbstractArray) where {SZ,T,N,A} = SizedArray{SZ,T,N,A}(data)

# Minimal AbstractArray interface
Base.size(a::SizedArray) = size(typeof(a))
Base.size(::Type{<:SizedArray{SZ}}) where {SZ} = SZ
Base.axes(a::SizedArray) = map(SOneTo, size(a))
Base.getindex(A::SizedArray, i...) = getindex(A.data, i...)
Base.setindex!(A::SizedArray, v, i...) = setindex!(A.data, v, i...)
Base.zero(::Type{T}) where T <: SizedArray = SizedArray{size(T)}(zeros(eltype(T), size(T)))
Base.parent(S::SizedArray) = S.data
+(S1::SizedArray{SZ}, S2::SizedArray{SZ}) where {SZ} = SizedArray{SZ}(S1.data + S2.data)
==(S1::SizedArray{SZ}, S2::SizedArray{SZ}) where {SZ} = S1.data == S2.data

homogenize_shape(t::Tuple) = (_homogenize_shape(first(t)), homogenize_shape(Base.tail(t))...)
homogenize_shape(::Tuple{}) = ()
_homogenize_shape(x::Integer) = x
_homogenize_shape(x::AbstractUnitRange) = length(x)
const Dims = Union{Integer, Base.OneTo, SOneTo}
function Base.similar(::Type{A}, shape::Tuple{Dims, Vararg{Dims}}) where {A<:AbstractArray}
    similar(A, homogenize_shape(shape))
end
function Base.similar(::Type{A}, shape::Tuple{SOneTo, Vararg{SOneTo}}) where {A<:AbstractArray}
    R = similar(A, length.(shape))
    SizedArray{length.(shape)}(R)
end

const SizedMatrixLike = Union{SizedMatrix, Transpose{<:Any, <:SizedMatrix}, Adjoint{<:Any, <:SizedMatrix}}

_data(S::SizedArray) = S.data
_data(T::Transpose{<:Any, <:SizedArray}) = transpose(_data(parent(T)))
_data(T::Adjoint{<:Any, <:SizedArray}) = adjoint(_data(parent(T)))

function *(S1::SizedMatrixLike, S2::SizedMatrixLike)
    0 < ndims(S1) < 3 && 0 < ndims(S2) < 3 && size(S1, 2) == size(S2, 1) || throw(ArgumentError("size mismatch!"))
    data = _data(S1) * _data(S2)
    SZ = ndims(data) == 1 ? (size(S1, 1), ) : (size(S1, 1), size(S2, 2))
    SizedArray{SZ}(data)
end

# deliberately wide method definitions to test for method ambiguties in LinearAlgebra
*(S1::SizedMatrixLike, M::AbstractMatrix) = _data(S1) * M
mul!(dest::AbstractMatrix, S1::SizedMatrix, M::AbstractMatrix, α::Number, β::Number) =
    mul!(dest, _data(S1), M, α, β)
mul!(dest::AbstractMatrix, M::AbstractMatrix, S2::SizedMatrix, α::Number, β::Number) =
    mul!(dest, M, _data(S2), α, β)
mul!(dest::AbstractMatrix, S1::SizedMatrix, S2::SizedMatrix, α::Number, β::Number) =
    mul!(dest, _data(S1), _data(S2), α, β)
mul!(dest::AbstractVector, M::AbstractMatrix, v::SizedVector, α::Number, β::Number) =
    mul!(dest, M, _data(v), α, β)

end
