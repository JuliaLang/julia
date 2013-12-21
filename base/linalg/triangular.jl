## Triangular
type Triangular{T<:Number} <: AbstractMatrix{T}
    UL::Matrix{T}
    uplo::Char
    unitdiag::Char
end
function Triangular{T<:Number}(A::Matrix{T}, uplo::Symbol, unitdiag::Bool)
    if size(A, 1) != size(A, 2) throw(DimensionMismatch("matrix must be square")) end
    return Triangular(A, string(uplo)[1], unitdiag ? 'U' : 'N')
end
Triangular(A::Matrix, uplo::Symbol) = Triangular(A, uplo, all(diag(A) .== 1) ? true : false)
function Triangular(A::Matrix)
    if istriu(A) return Triangular(A, :U) end
    if istril(A) return Triangular(A, :L) end
    throw(ArgumentError("matrix is not triangular"))
end

size(A::Triangular, args...) = size(A.UL, args...)
full(A::Triangular) = (istril(A) ? tril! : triu!)(A.UL)

getindex{T}(A::Triangular{T}, i::Integer, j::Integer) = i == j ? A.UL[i,j] : ((A.uplo == 'U') == (i < j) ? getindex(A.UL, i, j) : zero(T))

istril(A::Triangular) = A.uplo == 'L'
istriu(A::Triangular) = A.uplo == 'U'

# Vector multiplication
*{T<:BlasFloat}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'N', A.unitdiag, A.UL, b)
Ac_mul_B{T<:BlasComplex}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'C', A.unitdiag, A.UL, b)
At_mul_B{T<:BlasReal}(A::Triangular{T}, b::Vector{T}) = BLAS.trmv(A.uplo, 'T', A.unitdiag, A.UL, b)

# Matrix multiplication
*{T<:BlasFloat}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'N', A.unitdiag, one(T), A.UL, B)
*{T<:BlasFloat}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'N', B.unitdiag, one(T), A, B.UL)
Ac_mul_B{T<:BlasComplex}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'C', A.unitdiag, one(T), A.UL, B)
Ac_mul_B{T<:BlasReal}(A::Triangular{T}, B::StridedMatrix{T}) = BLAS.trmm('L', A.uplo, 'T', A.unitdiag, one(T), A.UL, B)
A_mul_Bc{T<:BlasComplex}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'C', B.unitdiag, one(T), B.UL, A)
A_mul_Bc{T<:BlasReal}(A::StridedMatrix{T}, B::Triangular{T}) = BLAS.trmm('R', B.uplo, 'T', B.unitdiag, one(T), B.UL, A)

\{T<:BlasFloat}(A::Triangular{T}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(A.uplo, 'N', A.unitdiag, A.UL, copy(B))
Ac_ldiv_B{T<:BlasReal}(A::Triangular{T}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(A.uplo, 'T', A.unitdiag, A.UL, copy(B))
Ac_ldiv_B{T<:BlasComplex}(A::Triangular{T}, B::StridedVecOrMat{T}) = LAPACK.trtrs!(A.uplo, 'C', A.unitdiag, A.UL, copy(B))

/{T<:BlasFloat}(A::StridedVecOrMat{T}, B::Triangular{T}) = BLAS.trsm!('R', B.uplo, 'N', B.unitdiag, one(T), B.UL, copy(A))
A_rdiv_Bc{T<:BlasReal}(A::StridedVecOrMat{T}, B::Triangular{T}) = BLAS.trsm!('R', B.uplo, 'T', B.unitdiag, one(T), B.UL, copy(A))
A_rdiv_Bc{T<:BlasComplex}(A::StridedVecOrMat{T}, B::Triangular{T}) = BLAS.trsm!('R', B.uplo, 'C', B.unitdiag, one(T), B.UL, copy(A))

det(A::Triangular) = prod(diag(A.UL))

inv{T<:BlasFloat}(A::Triangular{T}) = LAPACK.trtri!(A.uplo, A.unitdiag, copy(A.UL))
inv(A::Triangular) = inv(Triangular(float(A.UL), A.uplo, A.unitdiag))
