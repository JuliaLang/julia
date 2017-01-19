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

"""
    bkfact!(A, uplo::Symbol=:U, symmetric::Bool=issymmetric(A), rook::Bool=false) -> BunchKaufman

`bkfact!` is the same as [`bkfact`](@ref), but saves space by overwriting the
input `A`, instead of creating a copy.
"""
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

"""
    bkfact(A, uplo::Symbol=:U, symmetric::Bool=issymmetric(A), rook::Bool=false) -> BunchKaufman

Compute the Bunch-Kaufman [^Bunch1977] factorization of a symmetric or Hermitian
matrix `A` and return a `BunchKaufman` object.
`uplo` indicates which triangle of matrix `A` to reference.
If `symmetric` is `true`, `A` is assumed to be symmetric. If `symmetric` is `false`,
`A` is assumed to be Hermitian. If `rook` is `true`, rook pivoting is used. If
`rook` is false, rook pivoting is not used.
The following functions are available for
`BunchKaufman` objects: [`size`](@ref), `\\`, [`inv`](@ref), [`issymmetric`](@ref), [`ishermitian`](@ref).

[^Bunch1977]: J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. [url](http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0).

"""
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
convert{T}(::Type{Factorization{T}}, B::BunchKaufman{T}) = B
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

function logabsdet(F::BunchKaufman)
    M = F.LD
    p = F.ipiv
    n = size(F.LD, 1)

    if F.info > 0
        return eltype(F)(-Inf), zero(eltype(F))
    end
    s = one(real(eltype(F)))
    i = 1
    abs_det = zero(real(eltype(F)))
    while i <= n
        if p[i] > 0
            elm = M[i,i]
            s *= sign(elm)
            abs_det += log(abs(elm))
            i += 1
        else
            # 2x2 pivot case. Make sure not to square before the subtraction by scaling
            # with the off-diagonal element. This is safe because the off diagonal is
            # always large for 2x2 pivots.
            if F.uplo == 'U'
                elm = M[i, i + 1]*(M[i,i]/M[i, i + 1]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i, i + 1] : conj(M[i, i + 1])))
                s *= sign(elm)
                abs_det += log(abs(elm))
            else
                elm = M[i + 1,i]*(M[i, i]/M[i + 1, i]*M[i + 1, i + 1] -
                    (issymmetric(F) ? M[i + 1, i] : conj(M[i + 1, i])))
                s *= sign(elm)
                abs_det += log(abs(elm))
            end
            i += 2
        end
    end
    return abs_det, s
end

## reconstruct the original matrix
## TODO: understand the procedure described at
## http://www.nag.com/numeric/FL/nagdoc_fl22/pdf/F07/f07mdf.pdf
