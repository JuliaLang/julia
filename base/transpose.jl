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

transpose(T::MatrixTranspose{false}) = error("cannot take transpose of a ctranspose for now")
ctranspose(T::MatrixTranspose{true}) = error("cannot take ctranspose of a transpose for now")

size(T::MatrixTranspose) = (size(T.data, 2), size(T.data, 1))
getindex(T::MatrixTranspose{false}, i::Int, j::Int) = transpose(T.data[j, i])
getindex(T::MatrixTranspose{true}, i::Int, j::Int) = ctranspose(T.data[j, i])

## Vector transposes
immutable Covector{C,T,A} # <: ???
    data::A # A <: AbstractVector{T}
end
call{C,T}(::Type{Covector{C}}, A::AbstractArray{T,1}) = Covector{C,T,typeof(A)}(A)
parent(T::Covector) = T.data

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

## There is a huge combinatorial explosion here. There are 4 mostly-orthoganol
# dimensions:
#
# * Mutation:  (Mutating, non-mutating). This can be a simple fallback
# * Shape:     (Mat*Mat; Vec*Mat; Mat*Vec). Plus some Vec*Vec with combinations of:
# * Transpose: (A, Aᵀ, Aᴴ) × (B, Bᵀ, Bᴴ).
# * Implementation: (BLAS, Strided, generic)
#
# The key to staying sane is keeping the number of combinations on any one
# name relatively limited.

# Convenience aliases
#
# # At the top level, we pick off dot-products and errors, and then punt to mutation
# *(::AbstractVector, ::AbstractVector) = throw(ArgumentError("cannot multiply two vectors. Did you mean `.*` or `dot`?"))
# *(::Covector, ::Covector) = throw(ArgumentError("cannot multiply two covectors. Did you mean `.*` or `dot`?"))
# *(v::Covector{true}, w::Vector) = dot(v, w)
# *(v::Covector{false}, w::Vector) = dot(conj(v), w) # TODO: don't conjugate twice!
# function *(A::Union{AbstractVecOrMat,Covector}, B::Union{AbstractVecOrMat,Covector})
#     mul!(similar(parent(B), promote_type(arithtype(eltype(A)), arithtype(eltype(B))), mul_shape(A,B)), A, B)
# end
#
# # Now mul! has dispatches algorithm to use, either BLAS or generic, based upon the element and container type
# typealias BlasVec{T<:Union{BlasFloat,BlasInt},C,A<:StridedVector} Union{StridedVector{T}, Covector{C,T,A}}
# typealias BlasMat{T<:Union{BlasFloat,BlasInt},C,A<:StridedMatrix} Union{StridedMatrix{T}, MatrixTranspose{C,T,A}}
#
# #
#
#
#
#
#
#
#
#
#
#
#
#
#
















#
