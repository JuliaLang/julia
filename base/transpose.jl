# import Base: *, /, \, parent, call, transpose, ctranspose, size, getindex

immutable MatrixTranspose{C,T,A} <: AbstractArray{T,2}
    data::A # A <: AbstractMatrix{T}
end
call{C,T}(::Type{MatrixTranspose{C}}, A::AbstractArray{T,2}) = MatrixTranspose{C,T,typeof(A)}(A)
parent(T::MatrixTranspose) = T.data

transpose{T}(A::AbstractArray{T,2}) = MatrixTranspose{false}(A)
ctranspose{T}(A::AbstractArray{T,2}) = MatrixTranspose{true}(A)

transpose(T::MatrixTranspose{false}) = T.data
ctranspose(T::MatrixTranspose{true}) = T.data
transpose(T::MatrixTranspose{true}) = ConjugateArray(T.data)
ctranspose(T::MatrixTranspose{false}) = ConjugateArray(T.data)

size(T::MatrixTranspose) = (size(T.data, 2), size(T.data, 1))
getindex(T::MatrixTranspose{false}, i::Int, j::Int) = transpose(T.data[j, i])
getindex(T::MatrixTranspose{true}, i::Int, j::Int) = ctranspose(T.data[j, i])

## Vector transposes
immutable Covector{C,T,A} # <: AbstractMatrix{T} ??? Definitely not Vector.
    data::A # A <: AbstractVector{T}
end
call{C,T}(::Type{Covector{C}}, A::AbstractArray{T,1}) = Covector{C,T,typeof(A)}(A)
parent(T::Covector) = T.data
length(T::Covector) = length(T.data)

transpose{T}(A::AbstractArray{T,1}) = Covector{false}(A)
ctranspose{T}(A::AbstractArray{T,1}) = Covector{true}(A)

transpose(T::Covector{false}) = T.data
ctranspose(T::Covector{true}) = T.data
transpose(T::Covector{true}) = ConjugateArray(T.data)
ctranspose(T::Covector{false}) = ConjugateArray(T.data)

getindex(c::Covector{true}, i::Int) = ctranspose(c.data[i])
getindex(c::Covector{false}, i::Int) = transpose(c.data[i])

## Undefined for arbitrary N
transpose(A::AbstractArray) = error("transpose of 0- or 3+ dimensional arrays is undefined")
ctranspose(A::AbstractArray) = error("ctranspose of 0- or 3+ dimensional arrays is undefined")

# allow un-transposing any array:
untranspose(T::MatrixTranspose) = T.data
untranspose(T::Covector) = T.data
untranspose(A::AbstractArray) = A

# Lazy conjugates are required for transposes of ctransposes (and vice versa)
immutable ConjugateArray{T,N,A} <: AbstractArray{T,N}
    data::A # <: AbstractArray{T,N}
end
ConjugateArray{T,N}(A::AbstractArray{T,N}) = ConjugateArray{T,N,typeof(A)}(A)
lazyconj(A::AbstractArray) = ConjugateArray(A)
lazyconj(x) = conj(x)

size(C::ConjugateArray) = size(C.data)
linearindexing{C<:ConjugateArray}(::Type{C}) = linearindexing(C.data)
@inline getindex(C::ConjugateArray, I::Int...) = lazyconj(getindex(C.data, I...))
@inline unsafe_getindex(C::ConjugateArray, I::Int...) = lazyconj(unsafe_getindex(C.data, I...))
