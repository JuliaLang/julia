# This file is a part of Julia. License is MIT: https://julialang.org/license

# Multiplication #

# inner product -> dot product specializations
@inline *(rowvec::RowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(parent(rowvec), vec)
#@inline *(rowvec::ConjRowVector{T}, vec::AbstractVector{T}) where {T<:Real} = dot(rowvec', vec)
#@inline *(rowvec::ConjRowVector, vec::AbstractVector) = dot(rowvec', vec)

# Generic behavior
@inline function *(rowvec::RowVector, vec::AbstractVector)
    if length(rowvec) != length(vec)
        throw(DimensionMismatch("A has dimensions $(size(rowvec)) but B has dimensions $(size(vec))"))
    end
    sum(@inbounds(return rowvec[i]*vec[i]) for i = 1:length(vec))
end
@inline *(rowvec::RowVector, mat::AbstractMatrix) = transpose(mat.' * transpose(rowvec))
*(::RowVector, ::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline *(vec::AbstractVector, rowvec::RowVector) = vec .* rowvec
*(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))

# Transposed forms
A_mul_Bt(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline A_mul_Bt(rowvec::RowVector, mat::AbstractMatrix) = transpose(mat * transpose(rowvec))
@inline A_mul_Bt(rowvec1::RowVector, rowvec2::RowVector) = rowvec1*transpose(rowvec2)
A_mul_Bt(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline A_mul_Bt(vec1::AbstractVector, vec2::AbstractVector) = vec1 * transpose(vec2)
@inline A_mul_Bt(mat::AbstractMatrix, rowvec::RowVector) = mat * transpose(rowvec)

@inline At_mul_Bt(rowvec::RowVector, vec::AbstractVector) = transpose(rowvec) * transpose(vec)
@inline At_mul_Bt(vec::AbstractVector, mat::AbstractMatrix) = transpose(mat * vec)
At_mul_Bt(rowvec1::RowVector, rowvec2::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline At_mul_Bt(vec::AbstractVector, rowvec::RowVector) = transpose(vec)*transpose(rowvec)
At_mul_Bt(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch(
    "Cannot multiply two transposed vectors"))
@inline At_mul_Bt(mat::AbstractMatrix, rowvec::RowVector) = mat.' * transpose(rowvec)

At_mul_B(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline At_mul_B(vec::AbstractVector, mat::AbstractMatrix) = transpose(At_mul_B(mat,vec))
@inline At_mul_B(rowvec1::RowVector, rowvec2::RowVector) = transpose(rowvec1) * rowvec2
At_mul_B(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch(
    "Cannot multiply two transposed vectors"))
@inline At_mul_B(vec1::AbstractVector{T}, vec2::AbstractVector{T}) where {T<:Real} =
    reduce(+, map(At_mul_B, vec1, vec2)) # Seems to be overloaded...
@inline At_mul_B(vec1::AbstractVector, vec2::AbstractVector) = transpose(vec1) * vec2

# Conjugated forms
A_mul_Bc(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline A_mul_Bc(rowvec::RowVector, mat::AbstractMatrix) = adjoint(mat * adjoint(rowvec))
@inline A_mul_Bc(rowvec1::RowVector, rowvec2::RowVector) = rowvec1 * adjoint(rowvec2)
A_mul_Bc(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline A_mul_Bc(vec1::AbstractVector, vec2::AbstractVector) = vec1 * adjoint(vec2)
@inline A_mul_Bc(mat::AbstractMatrix, rowvec::RowVector) = mat * adjoint(rowvec)

@inline Ac_mul_Bc(rowvec::RowVector, vec::AbstractVector) = adjoint(rowvec) * adjoint(vec)
@inline Ac_mul_Bc(vec::AbstractVector, mat::AbstractMatrix) = adjoint(mat * vec)
Ac_mul_Bc(rowvec1::RowVector, rowvec2::RowVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline Ac_mul_Bc(vec::AbstractVector, rowvec::RowVector) = adjoint(vec)*adjoint(rowvec)
Ac_mul_Bc(vec::AbstractVector, rowvec::AbstractVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline Ac_mul_Bc(mat::AbstractMatrix, rowvec::RowVector) = mat' * adjoint(rowvec)

Ac_mul_B(::RowVector, ::AbstractVector) = throw(DimensionMismatch("Cannot multiply two vectors"))
@inline Ac_mul_B(vec::AbstractVector, mat::AbstractMatrix) = adjoint(Ac_mul_B(mat,vec))
@inline Ac_mul_B(rowvec1::RowVector, rowvec2::RowVector) = adjoint(rowvec1) * rowvec2
Ac_mul_B(vec::AbstractVector, rowvec::RowVector) = throw(DimensionMismatch("Cannot multiply two transposed vectors"))
@inline Ac_mul_B(vec1::AbstractVector, vec2::AbstractVector) = adjoint(vec1)*vec2

# Pseudo-inverse
pinv(v::RowVector, tol::Real=0) = pinv(v', tol)'

# Left Division #

\(rowvec1::RowVector, rowvec2::RowVector) = pinv(rowvec1) * rowvec2
\(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
At_ldiv_B(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))
Ac_ldiv_B(mat::AbstractMatrix, rowvec::RowVector) = throw(DimensionMismatch("Cannot left-divide transposed vector by matrix"))

# Right Division #

@inline /(rowvec::RowVector, mat::AbstractMatrix) = transpose(transpose(mat) \ transpose(rowvec))
@inline A_rdiv_Bt(rowvec::RowVector, mat::AbstractMatrix) = transpose(mat \ transpose(rowvec))
@inline A_rdiv_Bc(rowvec::RowVector, mat::AbstractMatrix) = adjoint(mat  \ adjoint(rowvec))
