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

transpose(T::MatrixTranspose{true}) = error("cannot take transpose of a ctranspose for now")
ctranspose(T::MatrixTranspose{false}) = error("cannot take ctranspose of a transpose for now")

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

transpose(T::Covector{true}) = error("cannot take transpose of a ctranspose for now")
ctranspose(T::Covector{false}) = error("cannot take ctranspose of a transpose for now")

## Undefined for arbitrary N
transpose(A::AbstractArray) = error("transpose of 0- or 3+ dimensional arrays is undefined")
ctranspose(A::AbstractArray) = error("ctranspose of 0- or 3+ dimensional arrays is undefined")

# allow un-transposing any array:
untranspose(T::MatrixTranspose) = T.data
untranspose(T::Covector) = T.data
untranspose(A::AbstractArray) = A
