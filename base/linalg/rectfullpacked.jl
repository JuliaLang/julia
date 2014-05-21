# Rectangular Full Packed Matrices

type SymmetricRFP{T<:BlasFloat} <: StructuredMatrix{T}
    data::Vector{T}
    transr::Char
    uplo::Char
end

function Ac_mul_A_RFP{T<:BlasFloat}(A::Matrix{T})
    n = size(A, 2)
    C = LAPACK.sfrk!('N', 'U', 'T', 1.0, A, 0.0, Array(T, div(n*(n+1),2)))
    SymmetricRFP(C, 'N', 'U')
end

type TriangularRFP{T<:BlasFloat} <: StructuredMatrix{T}
    data::Vector{T}
    transr::Char
    uplo::Char
end
TriangularRFP(A::Matrix) = TriangularRFP(trttf!('N', 'U', A), 'N', 'U')

full(A::TriangularRFP) = (A.uplo=='U' ? triu! : tril!)(LAPACK.tfttr!(A.transr, A.uplo, A.data))

type CholeskyDenseRFP{T<:BlasFloat} <: Factorization{T}
    data::Vector{T}
    transr::Char
    uplo::Char
end

cholfact!{T<:BlasFloat}(A::SymmetricRFP{T}) = CholeskyDenseRFP(LAPACK.pftrf!(A.transr, A.uplo, copy(A.data)), A.transr, A.uplo)
cholfact{T<:BlasFloat}(A::SymmetricRFP{T}) = cholfact!(copy(A))

copy(A::SymmetricRFP) = SymmetricRFP(copy(A.data), A.transr, A.uplo)

# Least squares
\(A::CholeskyDenseRFP, B::VecOrMat) = LAPACK.pftrs!(A.transr, A.uplo, A.data, copy(B))

inv(A::CholeskyDenseRFP)=LAPACK.pftri!(A.transr, A.uplo, copy(A.data))
