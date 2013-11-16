## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

type BunchKaufman{T<:BlasFloat} <: Factorization{T}
    LD::Matrix{T}
    ipiv::Vector{BlasInt}
    uplo::Char
    symmetric::Bool
end

function bkfact!{T<:BlasReal}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issym(A))
    symmetric || error("The Bunch-Kaufman decomposition is only valid for symmetric matrices")
    LD, ipiv = LAPACK.sytrf!(string(uplo)[1] , A)
    BunchKaufman(LD, ipiv, string(uplo)[1], symmetric)
end
function bkfact!{T<:BlasComplex}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issym(A))
    LD, ipiv = (symmetric ? LAPACK.sytrf! : LAPACK.hetrf!)(string(uplo)[1] , A)
    BunchKaufman(LD, ipiv, string(uplo)[1], symmetric)
end
bkfact!(A::StridedMatrix, args...) = bkfact!(float(A), args...)
bkfact{T<:BlasFloat}(A::StridedMatrix{T}, args...) = bkfact!(copy(A), args...)
bkfact(A::StridedMatrix, args...) = bkfact(float(A), args...)

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman,d::Integer) = size(B.LD,d)
issym(B::BunchKaufman) = B.symmetric
ishermitian(B::BunchKaufman) = !B.symmetric

inv{T<:BlasReal}(B::BunchKaufman{T})=symmetrize_conj!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)

function inv{T<:BlasComplex}(B::BunchKaufman{T})
    if issym(B)
        symmetrize!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
    else
        symmetrize_conj!(LAPACK.hetri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
    end
end

\{T<:BlasReal}(B::BunchKaufman{T}, R::StridedVecOrMat{T}) = LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, copy(R))
function \{T<:BlasComplex}(B::BunchKaufman{T}, R::StridedVecOrMat{T})
    (issym(B) ? LAPACK.sytrs! : LAPACK.hetrs!)(B.uplo, B.LD, B.ipiv, copy(R))
end
