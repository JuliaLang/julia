# This file is a part of Julia. License is MIT: http://julialang.org/license

## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

immutable BunchKaufman{T,S<:AbstractMatrix} <: Factorization{T}
    LD::S
    ipiv::Vector{BlasInt}
    uplo::Char
    symmetric::Bool
    BunchKaufman(LD::AbstractMatrix{T}, ipiv::Vector{BlasInt}, uplo::Char, symmetric::Bool) = new(LD, ipiv, uplo, symmetric)
end
BunchKaufman{T}(LD::AbstractMatrix{T}, ipiv::Vector{BlasInt}, uplo::Char, symmetric::Bool) = BunchKaufman{T,typeof(LD)}(LD, ipiv, uplo, symmetric)

function bkfact!{T<:BlasReal}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issym(A))
    if !symmetric
        throw(ArgumentError("Bunch-Kaufman decomposition is only valid for symmetric matrices"))
    end
    LD, ipiv = LAPACK.sytrf!(char_uplo(uplo) , A)
    BunchKaufman(LD, ipiv, char_uplo(uplo), symmetric)
end
function bkfact!{T<:BlasComplex}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issym(A))
    LD, ipiv = (symmetric ? LAPACK.sytrf! : LAPACK.hetrf!)(char_uplo(uplo) , A)
    BunchKaufman(LD, ipiv, char_uplo(uplo), symmetric)
end
bkfact{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issym(A)) = bkfact!(copy(A), uplo, symmetric)
bkfact{T}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issym(A)) = bkfact!(convert(Matrix{promote_type(Float32,typeof(sqrt(one(T))))},A),uplo,symmetric)

convert{T}(::Type{BunchKaufman{T}},B::BunchKaufman) = BunchKaufman(convert(Matrix{T},B.LD),B.ipiv,B.uplo,B.symmetric)
convert{T}(::Type{Factorization{T}}, B::BunchKaufman) = convert(BunchKaufman{T}, B)

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman,d::Integer) = size(B.LD,d)
issym(B::BunchKaufman) = B.symmetric
ishermitian(B::BunchKaufman) = !B.symmetric

inv{T<:BlasReal}(B::BunchKaufman{T}) = copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)

function inv{T<:BlasComplex}(B::BunchKaufman{T})
    if issym(B)
        copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
    else
        copytri!(LAPACK.hetri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    end
end

A_ldiv_B!{T<:BlasReal}(B::BunchKaufman{T}, R::StridedVecOrMat{T}) = LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
function A_ldiv_B!{T<:BlasComplex}(B::BunchKaufman{T}, R::StridedVecOrMat{T})
    (issym(B) ? LAPACK.sytrs! : LAPACK.hetrs!)(B.uplo, B.LD, B.ipiv, R)
end

function det(F::BunchKaufman)
    M = F.LD
    p = F.ipiv
    n = size(F.LD, 1)

    i = 1
    d = one(eltype(F))
    while i <= n
        if p[i] > 0
            # 1x1 pivot case
            d *= M[i,i]
            i += 1
        else
            # 2x2 pivot case. Make sure not to square before the subtraction by scaling with the off-diagonal element. This is safe because the off diagonal is always large for 2x2 pivots.
            if F.uplo == 'U'
                d *= M[i, i + 1]*(M[i,i]/M[i, i + 1]*M[i + 1, i + 1] - (issym(F) ? M[i, i + 1] : conj(M[i, i + 1])))
            else
                d *= M[i + 1,i]*(M[i, i]/M[i + 1, i]*M[i + 1, i + 1] - (issym(F) ? M[i + 1, i] : conj(M[i + 1, i])))
            end
            i += 2
        end
    end
    return d
end

## reconstruct the original matrix
## TODO: understand the procedure described at
## http://www.nag.com/numeric/FL/nagdoc_fl22/pdf/F07/f07mdf.pdf
