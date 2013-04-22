# Rectangular Full Packed Matrices

type SymmetricRFP{T<:BlasFloat} <: AbstractMatrix{T}
    data::Vector{T}
    transr::Char
    uplo::Char
end

function Ac_mul_A_RFP{T<:BlasFloat}(A::Matrix{T})
    n = size(A, 2)
    C = LAPACK.sfrk!('N', 'U', 'T', 1.0, A, 0.0, Array(T, div(n*(n+1),2)))
    return SymmetricRFP(C, 'N', 'U')
end

type TriangularRFP{T<:BlasFloat} <: AbstractMatrix{T}
    data::Vector{T}
    transr::Char
    uplo::Char
end
TriangularRFP(A::Matrix) = TriangularRFP(trttf!('N', 'U', A)[1], 'N', 'U')

function full(A::TriangularRFP)
    B = LAPACK.tfttr!(A.transr, A.uplo, A.data)[1]
    if A.uplo == 'U' 
        return triu!(B)
    else
        return tril!(B)
    end
end

type CholeskyDenseRFP{T<:BlasFloat} <: Factorization{T}
    data::Vector{T}
    transr::Char
    uplo::Char
end

function chol(A::SymmetricRFP)
    C, info = LAPACK.pftrf!(A.transr, A.uplo, copy(A.data))
    return CholeskyDenseRFP(C, A.transr, A.uplo)
end

# Least squares
\(A::CholeskyDenseRFP, B::VecOrMat) = LAPACK.pftrs!(A.transr, A.uplo, A.data, copy(B))

function inv(A::CholeskyDenseRFP)
    B, info = LAPACK.pftri!(A.transr, A.uplo, copy(A.data))
    if info > 0 throw(LAPACK.SingularException(info)) end
    return B
end
