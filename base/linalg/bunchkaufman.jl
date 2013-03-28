## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

type BunchKaufman{T<:BlasFloat} <: Factorization{T}
    LD::Matrix{T}
    ipiv::Vector{BlasInt}
    uplo::Char
    function BunchKaufman(A::Matrix{T}, uplo::Char)
        LD, ipiv = LAPACK.sytrf!(uplo , copy(A))
        new(LD, ipiv, uplo)
    end
end
BunchKaufman{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Char) = BunchKaufman{T}(A, uplo)
BunchKaufman{T<:Real}(A::StridedMatrix{T}, uplo::Char) = BunchKaufman(float64(A), uplo)
BunchKaufman{T<:Number}(A::StridedMatrix{T}) = BunchKaufman(A, 'U')

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman,d::Integer) = size(B.LD,d)

function inv(B::BunchKaufman)
    symmetrize_conj!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
end

\{T<:BlasFloat}(B::BunchKaufman{T}, R::StridedVecOrMat{T}) =
    LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, copy(R))
