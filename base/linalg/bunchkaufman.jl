# This file is a part of Julia. License is MIT: https://julialang.org/license

## Create an extractor that extracts the modified original matrix, e.g.
## LD for BunchKaufman, UL for CholeskyDense, LU for LUDense and
## define size methods for Factorization types using it.

struct BunchKaufman{T,S<:AbstractMatrix} <: Factorization{T}
    LD::S
    ipiv::Vector{BlasInt}
    uplo::Char
    symmetric::Bool
    rook::Bool
    info::BlasInt
end
BunchKaufman(A::AbstractMatrix{T}, ipiv::Vector{BlasInt}, uplo::Char, symmetric::Bool,
             rook::Bool, info::BlasInt) where {T} =
        BunchKaufman{T,typeof(A)}(A, ipiv, uplo, symmetric, rook, info)

"""
    bkfact!(A, uplo::Symbol=:U, symmetric::Bool=issymmetric(A), rook::Bool=false) -> BunchKaufman

`bkfact!` is the same as [`bkfact`](@ref), but saves space by overwriting the
input `A`, instead of creating a copy.
"""
function bkfact!(A::RealHermSymComplexSym{T,S} where {T<:BlasReal,S<:StridedMatrix}, rook::Bool = false)
    LD, ipiv, info = rook ? LAPACK.sytrf_rook!(A.uplo, A.data) : LAPACK.sytrf!(A.uplo, A.data)
    BunchKaufman(LD, ipiv, A.uplo, true, rook, info)
end
function bkfact!(A::Hermitian{T,S} where {T<:BlasComplex,S<:StridedMatrix{T}}, rook::Bool = false)
    LD, ipiv, info = rook ? LAPACK.hetrf_rook!(A.uplo, A.data) : LAPACK.hetrf!(A.uplo, A.data)
    BunchKaufman(LD, ipiv, A.uplo, false, rook, info)
end
function bkfact!(A::StridedMatrix{<:BlasFloat}, rook::Bool = false)
    if ishermitian(A)
        return bkfact!(Hermitian(A), rook)
    elseif issymmetric(A)
        return bkfact!(Symmetric(A), rook)
    else
        throw(ArgumentError("Bunch-Kaufman decomposition is only valid for symmetric or Hermitian matrices"))
    end
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

[^Bunch1977]: J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. [url](http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0/).

"""
bkfact(A::AbstractMatrix{T}, rook::Bool=false) where {T} =
    bkfact!(copy_oftype(A, typeof(sqrt(one(T)))), rook)

convert(::Type{BunchKaufman{T}}, B::BunchKaufman{T}) where {T} = B
convert(::Type{BunchKaufman{T}}, B::BunchKaufman) where {T} =
    BunchKaufman(convert(Matrix{T}, B.LD), B.ipiv, B.uplo, B.symmetric, B.rook, B.info)
convert(::Type{Factorization{T}}, B::BunchKaufman{T}) where {T} = B
convert(::Type{Factorization{T}}, B::BunchKaufman) where {T} = convert(BunchKaufman{T}, B)

size(B::BunchKaufman) = size(B.LD)
size(B::BunchKaufman, d::Integer) = size(B.LD, d)
issymmetric(B::BunchKaufman) = B.symmetric
ishermitian(B::BunchKaufman) = !B.symmetric

issuccess(B::BunchKaufman) = B.info == 0

function Base.show(io::IO, mime::MIME{Symbol("text/plain")}, B::BunchKaufman)
    println(io, summary(B))
    print(io, "successful: $(issuccess(B))")
end

function inv(B::BunchKaufman{<:BlasReal})
    if !issuccess(B)
        throw(SingularException(B.info))
    end

    if B.rook
        copytri!(LAPACK.sytri_rook!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    else
        copytri!(LAPACK.sytri!(B.uplo, copy(B.LD), B.ipiv), B.uplo, true)
    end
end

function inv(B::BunchKaufman{<:BlasComplex})
    if !issuccess(B)
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

function A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where T<:BlasReal
    if !issuccess(B)
        throw(SingularException(B.info))
    end

    if B.rook
        LAPACK.sytrs_rook!(B.uplo, B.LD, B.ipiv, R)
    else
        LAPACK.sytrs!(B.uplo, B.LD, B.ipiv, R)
    end
end
function A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{T}) where T<:BlasComplex
    if !issuccess(B)
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
# There is no fallback solver for Bunch-Kaufman so we'll have to promote to same element type
function A_ldiv_B!(B::BunchKaufman{T}, R::StridedVecOrMat{S}) where {T,S}
    TS = promote_type(T,S)
    return A_ldiv_B!(convert(BunchKaufman{TS}, B), convert(AbstractArray{TS}, R))
end

function logabsdet(F::BunchKaufman)
    M = F.LD
    p = F.ipiv
    n = size(F.LD, 1)

    if !issuccess(F)
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
