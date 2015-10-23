# This file is a part of Julia. License is MIT: http://julialang.org/license

##########################
# Cholesky Factorization #
##########################
immutable Cholesky{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    uplo::Char
end
Cholesky{T}(A::AbstractMatrix{T}, uplo::Symbol) = Cholesky{T,typeof(A)}(A, char_uplo(uplo))
Cholesky{T}(A::AbstractMatrix{T}, uplo::Char) = Cholesky{T,typeof(A)}(A, uplo)

immutable CholeskyPivoted{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    uplo::Char
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt
end
function CholeskyPivoted{T}(A::AbstractMatrix{T}, uplo::Char, piv::Vector{BlasInt},
                            rank::BlasInt, tol::Real, info::BlasInt)
    CholeskyPivoted{T,typeof(A)}(A, uplo, piv, rank, tol, info)
end

function chol!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{UpperTriangular})
    C, info = LAPACK.potrf!('U', A)
    return @assertposdef UpperTriangular(C) info
end
function chol!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{LowerTriangular})
    C, info = LAPACK.potrf!('L', A)
    return @assertposdef LowerTriangular(C) info
end
chol!(A::StridedMatrix) = chol!(A, UpperTriangular)

function chol!{T}(A::AbstractMatrix{T}, ::Type{UpperTriangular})
    n = chksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[i,k]'A[i,k]
            end
            Akk = chol!(A[k,k], UpperTriangular)
            A[k,k] = Akk
            AkkInv = inv(Akk')
            for j = k + 1:n
                for i = 1:k - 1
                    A[k,j] -= A[i,k]'A[i,j]
                end
                A[k,j] = AkkInv*A[k,j]
            end
        end
    end
    return UpperTriangular(A)
end
function chol!{T}(A::AbstractMatrix{T}, ::Type{LowerTriangular})
    n = chksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[k,i]*A[k,i]'
            end
            Akk = chol!(A[k,k], LowerTriangular)
            A[k,k] = Akk
            AkkInv = inv(Akk)
            for j = 1:k
                for i = k + 1:n
                    if j == 1
                        A[i,k] = A[i,k]*AkkInv'
                    end
                    if j < k
                        A[i,k] -= A[i,j]*A[k,j]'*AkkInv'
                    end
                end
            end
        end
     end
    return LowerTriangular(A)
end

doc"""
    chol(A::AbstractMatrix) -> U

Compute the Cholesky factorization of a positive definite matrix `A` and return the UpperTriangular matrix `U` such that `A = U'U`.
"""
function chol{T}(A::AbstractMatrix{T})
    S = promote_type(typeof(chol(one(T))), Float32)
    chol!(copy_oftype(A, S))
end
function chol!(x::Number, uplo)
    rx = real(x)
    if rx != abs(x)
        throw(ArgumentError("x must be positive semidefinite"))
    end
    rxr = sqrt(rx)
    convert(promote_type(typeof(x), typeof(rxr)), rxr)
end
doc"""
    chol(x::Number) -> y

Compute the square root of a non-negative number `x`.
"""
chol(x::Number, args...) = chol!(x, nothing)

cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol, ::Type{Val{false}}) =
    _cholfact!(A, Val{false}, uplo)
cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) =
    _cholfact!(A, Val{true}, uplo; tol = tol)
cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol = :U) =
    _cholfact!(A, Val{false}, uplo)

function _cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Val{false}}, uplo::Symbol=:U)
    if uplo == :U
        return Cholesky(chol!(A, UpperTriangular).data, uplo)
    else
        return Cholesky(chol!(A, LowerTriangular).data, uplo)
    end
end
function _cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Val{true}}, uplo::Symbol=:U; tol=0.0)
    uplochar = char_uplo(uplo)
    A, piv, rank, info = LAPACK.pstrf!(uplochar, A, tol)
    return CholeskyPivoted{T,StridedMatrix{T}}(A, uplochar, piv, rank, tol, info)
end
function cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{false}})
    if uplo == :U
        Cholesky(chol!(A, UpperTriangular).data, 'U')
    else
        Cholesky(chol!(A, LowerTriangular).data, 'L')
    end
end
cholfact!(A::StridedMatrix, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) = throw(ArgumentError("generic pivoted Cholesky fectorization is not implemented yet"))
cholfact!(A::StridedMatrix, uplo::Symbol = :U) = cholfact!(A, uplo, Val{false})

cholfact{T}(A::StridedMatrix{T}, uplo::Symbol, ::Type{Val{false}}) =
    cholfact!(copy_oftype(A, promote_type(typeof(chol(one(T))),Float32)), uplo, Val{false})
cholfact{T}(A::StridedMatrix{T}, uplo::Symbol, ::Type{Val{true}}; tol = 0.0) =
    cholfact!(copy_oftype(A, promote_type(typeof(chol(one(T))),Float32)), uplo, Val{true}; tol = tol)
cholfact{T}(A::StridedMatrix{T}, uplo::Symbol = :U) = cholfact(A, uplo, Val{false})

function cholfact(x::Number, uplo::Symbol=:U)
    xf = fill(chol(x), 1, 1)
    Cholesky(xf, uplo)
end

function convert{Tnew,Told,S}(::Type{Cholesky{Tnew}}, C::Cholesky{Told,S})
    Cnew = convert(AbstractMatrix{Tnew}, C.factors)
    Cholesky{Tnew, typeof(Cnew)}(Cnew, C.uplo)
end
function convert{T,S}(::Type{Cholesky{T,S}}, C::Cholesky)
    Cnew = convert(AbstractMatrix{T}, C.factors)
    Cholesky{T, typeof(Cnew)}(Cnew, C.uplo)
end
convert{T}(::Type{Factorization{T}}, C::Cholesky) = convert(Cholesky{T}, C)
convert{T}(::Type{CholeskyPivoted{T}},C::CholeskyPivoted) =
    CholeskyPivoted(AbstractMatrix{T}(C.factors),C.uplo,C.piv,C.rank,C.tol,C.info)
convert{T}(::Type{Factorization{T}}, C::CholeskyPivoted) = convert(CholeskyPivoted{T}, C)

full{T,S}(C::Cholesky{T,S}) = C.uplo == 'U' ? C[:U]'C[:U] : C[:L]*C[:L]'
function full(F::CholeskyPivoted)
    ip = invperm(F[:p])
    return (F[:L] * F[:U])[ip,ip]
end

copy(C::Cholesky) = Cholesky(copy(C.factors), C.uplo)
copy(C::CholeskyPivoted) = CholeskyPivoted(copy(C.factors), C.uplo, C.piv, C.rank, C.tol, C.info)

size(C::Union{Cholesky, CholeskyPivoted}) = size(C.factors)
size(C::Union{Cholesky, CholeskyPivoted}, d::Integer) = size(C.factors, d)

function getindex{T,S}(C::Cholesky{T,S}, d::Symbol)
    d == :U && return UpperTriangular(symbol(C.uplo) == d ? C.factors : C.factors')
    d == :L && return LowerTriangular(symbol(C.uplo) == d ? C.factors : C.factors')
    d == :UL && return symbol(C.uplo) == :U ? UpperTriangular(C.factors) : LowerTriangular(C.factors)
    throw(KeyError(d))
end
function getindex{T<:BlasFloat}(C::CholeskyPivoted{T}, d::Symbol)
    d == :U && return UpperTriangular(symbol(C.uplo) == d ? C.factors : C.factors')
    d == :L && return LowerTriangular(symbol(C.uplo) == d ? C.factors : C.factors')
    d == :p && return C.piv
    if d == :P
        n = size(C, 1)
        P = zeros(T, n, n)
        for i = 1:n
            P[C.piv[i],i] = one(T)
        end
        return P
    end
    throw(KeyError(d))
end

show{T,S<:AbstractMatrix}(io::IO, C::Cholesky{T,S}) =
    (println("$(typeof(C)) with factor:");show(io,C[:UL]))

A_ldiv_B!{T<:BlasFloat,S<:AbstractMatrix}(C::Cholesky{T,S}, B::StridedVecOrMat{T}) =
    LAPACK.potrs!(C.uplo, C.factors, B)

function A_ldiv_B!{T,S<:AbstractMatrix}(C::Cholesky{T,S}, B::StridedVecOrMat)
    if C.uplo == 'L'
        return Ac_ldiv_B!(LowerTriangular(C.factors), A_ldiv_B!(LowerTriangular(C.factors), B))
    else
        return A_ldiv_B!(UpperTriangular(C.factors), Ac_ldiv_B!(UpperTriangular(C.factors), B))
    end
end

function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedVector{T})
    chkfullrank(C)
    ipermute!(LAPACK.potrs!(C.uplo, C.factors, permute!(B, C.piv)), C.piv)
end
function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedMatrix{T})
    chkfullrank(C)
    n = size(C, 1)
    for i=1:size(B, 2)
        permute!(sub(B, 1:n, i), C.piv)
    end
    LAPACK.potrs!(C.uplo, C.factors, B)
    for i=1:size(B, 2)
        ipermute!(sub(B, 1:n, i), C.piv)
    end
    B
end

function A_ldiv_B!(C::CholeskyPivoted, B::StridedVector)
    if C.uplo == 'L'
        Ac_ldiv_B!(LowerTriangular(C.factors), A_ldiv_B!(LowerTriangular(C.factors), B[C.piv]))[invperm(C.piv)]
    else
        A_ldiv_B!(UpperTriangular(C.factors), Ac_ldiv_B!(UpperTriangular(C.factors), B[C.piv]))[invperm(C.piv)]
    end
end

function A_ldiv_B!(C::CholeskyPivoted, B::StridedMatrix)
    if C.uplo == 'L'
        Ac_ldiv_B!(LowerTriangular(C.factors), A_ldiv_B!(LowerTriangular(C.factors), B[C.piv,:]))[invperm(C.piv),:]
    else
        A_ldiv_B!(UpperTriangular(C.factors), Ac_ldiv_B!(UpperTriangular(C.factors), B[C.piv,:]))[invperm(C.piv),:]
    end
end

function det(C::Cholesky)
    dd = one(eltype(C))
    for i in 1:size(C.factors,1) dd *= abs2(C.factors[i,i]) end
    dd
end

det(C::CholeskyPivoted) = C.rank < size(C.factors, 1) ? real(zero(eltype(C))) : prod(abs2(diag(C.factors)))

function logdet(C::Cholesky)
    dd = zero(eltype(C))
    for i in 1:size(C.factors,1) dd += log(C.factors[i,i]) end
    dd + dd # instead of 2.0dd which can change the type
end

inv!{T<:BlasFloat,S<:StridedMatrix}(C::Cholesky{T,S}) =
    copytri!(LAPACK.potri!(C.uplo, C.factors), C.uplo, true)

inv{T<:BlasFloat,S<:StridedMatrix}(C::Cholesky{T,S}) =
    inv!(copy(C))

function inv(C::CholeskyPivoted)
    chkfullrank(C)
    ipiv = invperm(C.piv)
    copytri!(LAPACK.potri!(C.uplo, copy(C.factors)), C.uplo, true)[ipiv, ipiv]
end

chkfullrank(C::CholeskyPivoted) = C.rank < size(C.factors, 1) && throw(RankDeficientException(C.info))

rank(C::CholeskyPivoted) = C.rank
