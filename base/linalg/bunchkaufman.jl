# This file is a part of Julia. License is MIT: http://julialang.org/license

## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

immutable BunchKaufman{T,S<:AbstractMatrix} <: Factorization{T}
    LD::S
    ipiv::Vector{BlasInt}
    uplo::Char
    symmetric::Bool
    rook::Bool
    info::BlasInt
end
BunchKaufman{T}(A::AbstractMatrix{T}, ipiv::Vector{BlasInt}, uplo::Char, symmetric::Bool,
    rook::Bool, info::BlasInt) =
        BunchKaufman{T,typeof(A)}(A, ipiv, uplo, symmetric, rook, info)

function bkfact!{T<:BlasReal}(A::StridedMatrix{T}, uplo::Symbol = :U,
    symmetric::Bool = issymmetric(A), rook::Bool = false)

    if !symmetric
        throw(ArgumentError("Bunch-Kaufman decomposition is only valid for symmetric matrices"))
    end
    if rook
        LD, ipiv, info = LAPACK.sytrf_rook!(char_uplo(uplo), A)
    else
        LD, ipiv, info = LAPACK.sytrf!(char_uplo(uplo), A)
    end
    BunchKaufman(LD, ipiv, char_uplo(uplo), symmetric, rook, info)
end
function bkfact!{T<:BlasComplex}(A::StridedMatrix{T}, uplo::Symbol=:U,
    symmetric::Bool=issymmetric(A), rook::Bool=false)

    if rook
        if symmetric
            LD, ipiv, info = LAPACK.sytrf_rook!(char_uplo(uplo), A)
        else
            LD, ipiv, info = LAPACK.hetrf_rook!(char_uplo(uplo), A)
        end
    else
        if symmetric
            LD, ipiv, info = LAPACK.sytrf!(char_uplo(uplo),  A)
        else
            LD, ipiv, info = LAPACK.hetrf!(char_uplo(uplo), A)
        end
    end
    BunchKaufman(LD, ipiv, char_uplo(uplo), symmetric, rook, info)
end
bkfact{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issymmetric(A),
    rook::Bool=false) =
        bkfact!(copy(A), uplo, symmetric, rook)
bkfact{T}(A::StridedMatrix{T}, uplo::Symbol=:U, symmetric::Bool=issymmetric(A),
    rook::Bool=false) =
        bkfact!(convert(Matrix{promote_type(Float32, typeof(sqrt(one(T))))}, A),
                uplo, symmetric, rook)

convert{T}(::Type{BunchKaufman{T}}, B::BunchKaufman{T}) = B
convert{T}(::Type{BunchKaufman{T}}, B::BunchKaufman) =
    BunchKaufman(convert(Matrix{T}, B.LD), B.ipiv, B.uplo, B.symmetric, B.rook, B.info)
convert{T}(::Type{Factorization{T}}, B::BunchKaufman) = convert(BunchKaufman{T}, B)

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman, d::Integer) = size(B.LD, d)
issymmetric(B::BunchKaufman) = B.symmetric
ishermitian(B::BunchKaufman) = !B.symmetric

function inv{T<:BlasReal}(B::BunchKaufman{T})
    if B.info > 0
        throw(SingularException(B.info))
    end

    if B.rook
        copytri!(LAPACK.sytri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    else
        copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    end
end

function inv{T<:BlasComplex}(B::BunchKaufman{T})
    if B.info > 0
        throw(SingularException(B.info))
    end

    if issymmetric(B)
        if B.rook
            copytri!(LAPACK.sytri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
        else
            copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo)
        end
    else
        if B.rook
            copytri!(LAPACK.hetri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
        else
            copytri!(LAPACK.hetri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
        end
    end
end

function A_ldiv_B!{T<:BlasReal}(B::BunchKaufman{T}, R::StridedVecOrMat{T})
    if B.info > 0
        throw(SingularException(B.info))
    end

    if B.rook
        LAPACK.sytrs_rook!(B.uplo, B.LD, B.ipiv, R)
    else
        LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
    end
end
function A_ldiv_B!{T<:BlasComplex}(B::BunchKaufman{T}, R::StridedVecOrMat{T})
    if B.info > 0
        throw(SingularException(B.info))
    end

    if B.rook
        if issymmetric(B)
            LAPACK.sytrs_rook!(B.uplo, B.LD, B.ipiv, R)
        else
            LAPACK.hetrs_rook!(B.uplo, B.LD, B.ipiv, R)
        end
    else
        if issymmetric(B)
            LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
        else
            LAPACK.hetrs!(B.uplo, B.LD, B.ipiv, R)
        end
    end
end

function det(F::BunchKaufman)
    if F.info > 0
        return zero(eltype(F))
    end

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
            # 2x2 pivot case. Make sure not to square before the subtraction by scaling
            # with the off-diagonal element. This is safe because the off diagonal is
            # always large for 2x2 pivots.
            if F.uplo == 'U'
                d *= M[i, i + 1]*(M[i,i]/M[i, i + 1]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i, i + 1] : conj(M[i, i + 1])))
            else
                d *= M[i + 1,i]*(M[i, i]/M[i + 1, i]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i + 1, i] : conj(M[i + 1, i])))
            end
            i += 2
        end
    end
    return d
end

## reconstruct the original matrix
## TODO: understand the procedure described at
## http://www.nag.com/numeric/FL/nagdoc_fl22/pdf/F07/f07mdf.pdf
