##########################
# Cholesky Factorization #
##########################
immutable Cholesky{T,S<:AbstractMatrix,UpLo} <: Factorization{T}
    UL::S
    Cholesky(UL::AbstractMatrix{T}) = new(UL)
end
Cholesky{T}(UL::AbstractMatrix{T},UpLo::Symbol) = Cholesky{T,typeof(UL),UpLo}(UL)

immutable CholeskyPivoted{T,S<:AbstractMatrix} <: Factorization{T}
    UL::S
    uplo::Char
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt
    CholeskyPivoted(UL::AbstractMatrix{T}, uplo::Char, piv::Vector{BlasInt}, rank::BlasInt, tol::Real, info::BlasInt) = new(UL, uplo, piv, rank, tol, info)
end
CholeskyPivoted{T}(UL::AbstractMatrix{T}, uplo::Char, piv::Vector{BlasInt}, rank::BlasInt, tol::Real, info::BlasInt) = CholeskyPivoted{T,typeof(UL)}(UL, uplo, piv, rank, tol, info)

function chol!{T<:BlasFloat}(A::StridedMatrix{T})
    C, info = LAPACK.potrf!('U', A)
    return @assertposdef UpperTriangular(C) info
end
function chol!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol)
    C, info = LAPACK.potrf!(char_uplo(uplo), A)
    return @assertposdef uplo == :U ? UpperTriangular(C) : LowerTriangular(C) info
end

function chol!{T}(A::AbstractMatrix{T})
    n = chksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[i,k]'A[i,k]
            end
            A[k,k] = chol!(A[k,k], uplo)
            AkkInv = inv(A[k,k])
            for j = k + 1:n
                for i = 1:k - 1
                    A[k,j] -= A[i,k]'A[i,j]
                end
                A[k,j] = A[k,k]'\A[k,j]
            end
        end
    end
    return UpperTriangular(A)
end
function chol!{T}(A::AbstractMatrix{T}, uplo::Symbol)
    n = chksquare(A)
    @inbounds begin
        if uplo == :L
            for k = 1:n
                for i = 1:k - 1
                    A[k,k] -= A[k,i]*A[k,i]'
                end
                A[k,k] = chol!(A[k,k], uplo)
                AkkInv = inv(A[k,k]')
                for j = 1:k
                    for i = k + 1:n
                        j == 1 && (A[i,k] = A[i,k]*AkkInv)
                        j < k && (A[i,k] -= A[i,j]*A[k,j]'*AkkInv)
                    end
                end
            end
        elseif uplo == :U
            for k = 1:n
                for i = 1:k - 1
                    A[k,k] -= A[i,k]'A[i,k]
                end
                A[k,k] = chol!(A[k,k], uplo)
                AkkInv = inv(A[k,k])
                for j = k + 1:n
                    for i = 1:k - 1
                        A[k,j] -= A[i,k]'A[i,j]
                    end
                    A[k,j] = A[k,k]'\A[k,j]
                end
            end
        else
            throw(ArgumentError("uplo must be either :U or :L but was $(uplo)"))
        end
    end
    return uplo == :U ? UpperTriangular(A) : LowerTriangular(A)
end

cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol=:U, pivot::Union(Type{Val{false}}, Type{Val{true}})=Val{false}; tol=0.0) =
    _cholfact!(A, pivot, uplo, tol=tol)
function _cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Val{false}}, uplo::Symbol=:U; tol=0.0)
    uplochar = char_uplo(uplo)
    return Cholesky(chol!(A, uplo).data, uplo)
end
function _cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Val{true}}, uplo::Symbol=:U; tol=0.0)
    uplochar = char_uplo(uplo)
    A, piv, rank, info = LAPACK.pstrf!(uplochar, A, tol)
    return CholeskyPivoted{T,StridedMatrix{T}}(A, uplochar, piv, rank, tol, info)
end
cholfact!(A::AbstractMatrix, uplo::Symbol=:U) = Cholesky(chol!(A, uplo).data, uplo)

function cholfact!{T<:BlasFloat,S,UpLo}(C::Cholesky{T,S,UpLo})
    _, info = LAPACK.potrf!(char_uplo(UpLo), C.UL)
    info[1]>0 && throw(PosDefException(info[1]))
    C
end

cholfact{T<:BlasFloat}(A::StridedMatrix{T}, uplo::Symbol=:U, pivot::Union(Type{Val{false}}, Type{Val{true}})=Val{false}; tol=0.0) =
    cholfact!(copy(A), uplo, pivot, tol=tol)


copy_oftype{T}(A::StridedMatrix{T}, ::Type{T}) = copy(A)
copy_oftype{T,S}(A::StridedMatrix{T}, ::Type{S}) = convert(AbstractMatrix{S}, A)
cholfact{T}(A::StridedMatrix{T}, uplo::Symbol=:U, pivot::Union(Type{Val{false}}, Type{Val{true}})=Val{false}; tol=0.0) =
    _cholfact(copy_oftype(A, promote_type(typeof(chol(one(T))),Float32)), pivot, uplo, tol=tol)
_cholfact{T<:BlasFloat}(A::StridedMatrix{T}, pivot::Type{Val{true}}, uplo::Symbol=:U; tol=0.0) =
    cholfact!(A, uplo, pivot, tol = tol)
_cholfact{T<:BlasFloat}(A::StridedMatrix{T}, pivot::Type{Val{false}}, uplo::Symbol=:U; tol=0.0) =
    cholfact!(A, uplo, pivot, tol = tol)

_cholfact{T}(A::StridedMatrix{T}, ::Type{Val{false}}, uplo::Symbol=:U; tol=0.0) =
    cholfact!(A, uplo)
_cholfact{T}(A::StridedMatrix{T}, ::Type{Val{true}}, uplo::Symbol=:U; tol=0.0) =
    throw(ArgumentError("pivot only supported for Float32, Float64, Complex{Float32} and Complex{Float64}"))


function cholfact(x::Number, uplo::Symbol=:U)
    xf = fill(chol!(x, uplo), 1, 1)
    Cholesky(xf, uplo)
end

chol{T}(A::AbstractMatrix{T}, uplo::Symbol=:U) = (S = promote_type(typeof(chol(one(T))),Float32); S != T ? chol!(convert(AbstractMatrix{S}, A), uplo) : chol!(copy(A), uplo))
function chol!(x::Number, uplo::Symbol=:U)
    rx = real(x)
    rx == abs(x) || throw(DomainError())
    rxr = sqrt(rx)
    convert(promote_type(typeof(x), typeof(rxr)), rxr)
end
chol(x::Number, uplo::Symbol=:U) = chol!(x, uplo)

function convert{Tnew,Told,S,UpLo}(::Type{Cholesky{Tnew}},C::Cholesky{Told,S,UpLo})
    Cnew = convert(AbstractMatrix{Tnew}, C.UL)
    Cholesky{Tnew, typeof(Cnew), UpLo}(Cnew)
end
function convert{T,S,UpLo}(::Type{Cholesky{T,S,UpLo}},C::Cholesky)
    Cnew = convert(AbstractMatrix{T}, C.UL)
    Cholesky{T, typeof(Cnew), UpLo}(Cnew)
end
convert{T}(::Type{Factorization{T}}, C::Cholesky) = convert(Cholesky{T}, C)
convert{T}(::Type{CholeskyPivoted{T}},C::CholeskyPivoted) = CholeskyPivoted(convert(AbstractMatrix{T},C.UL),C.uplo,C.piv,C.rank,C.tol,C.info)
convert{T}(::Type{Factorization{T}}, C::CholeskyPivoted) = convert(CholeskyPivoted{T}, C)

full{T,S}(C::Cholesky{T,S,:U}) = C[:U]'C[:U]
full{T,S}(C::Cholesky{T,S,:L}) = C[:L]*C[:L]'

size(C::Union(Cholesky, CholeskyPivoted)) = size(C.UL)
size(C::Union(Cholesky, CholeskyPivoted), d::Integer) = size(C.UL,d)

function getindex{T,S,UpLo}(C::Cholesky{T,S,UpLo}, d::Symbol)
    d == :U && return UpperTriangular(UpLo == d ? C.UL : C.UL')
    d == :L && return LowerTriangular(UpLo == d ? C.UL : C.UL')
    d == :UL && return UpLo == :U ? UpperTriangular(C.UL) : LowerTriangular(C.UL)
    throw(KeyError(d))
end
function getindex{T<:BlasFloat}(C::CholeskyPivoted{T}, d::Symbol)
    d == :U && return UpperTriangular(symbol(C.uplo) == d ? C.UL : C.UL')
    d == :L && return LowerTriangular(symbol(C.uplo) == d ? C.UL : C.UL')
    d == :p && return C.piv
    if d == :P
        n = size(C, 1)
        P = zeros(T, n, n)
        for i=1:n
            P[C.piv[i],i] = one(T)
        end
        return P
    end
    throw(KeyError(d))
end

show{T,S<:AbstractMatrix,UpLo}(io::IO, C::Cholesky{T,S,UpLo}) = (println("$(typeof(C)) with factor:");show(io,C[UpLo]))

A_ldiv_B!{T<:BlasFloat,S<:AbstractMatrix}(C::Cholesky{T,S,:U}, B::StridedVecOrMat{T}) = LAPACK.potrs!('U', C.UL, B)
A_ldiv_B!{T<:BlasFloat,S<:AbstractMatrix}(C::Cholesky{T,S,:L}, B::StridedVecOrMat{T}) = LAPACK.potrs!('L', C.UL, B)
A_ldiv_B!{T,S<:AbstractMatrix}(C::Cholesky{T,S,:L}, B::StridedVecOrMat) = Ac_ldiv_B!(LowerTriangular(C.UL), A_ldiv_B!(LowerTriangular(C.UL), B))
A_ldiv_B!{T,S<:AbstractMatrix}(C::Cholesky{T,S,:U}, B::StridedVecOrMat) = A_ldiv_B!(UpperTriangular(C.UL), Ac_ldiv_B!(UpperTriangular(C.UL), B))

function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedVector{T})
    chkfullrank(C)
    ipermute!(LAPACK.potrs!(C.uplo, C.UL, permute!(B, C.piv)), C.piv)
end
function A_ldiv_B!{T<:BlasFloat}(C::CholeskyPivoted{T}, B::StridedMatrix{T})
    chkfullrank(C)
    n = size(C, 1)
    for i=1:size(B, 2)
        permute!(sub(B, 1:n, i), C.piv)
    end
    LAPACK.potrs!(C.uplo, C.UL, B)
    for i=1:size(B, 2)
        ipermute!(sub(B, 1:n, i), C.piv)
    end
    B
end
A_ldiv_B!(C::CholeskyPivoted, B::StridedVector) = C.uplo=='L' ? Ac_ldiv_B!(LowerTriangular(C.UL), A_ldiv_B!(LowerTriangular(C.UL), B[C.piv]))[invperm(C.piv)] : A_ldiv_B!(UpperTriangular(C.UL), Ac_ldiv_B!(UpperTriangular(C.UL), B[C.piv]))[invperm(C.piv)]
A_ldiv_B!(C::CholeskyPivoted, B::StridedMatrix) = C.uplo=='L' ? Ac_ldiv_B!(LowerTriangular(C.UL), A_ldiv_B!(LowerTriangular(C.UL), B[C.piv,:]))[invperm(C.piv),:] : A_ldiv_B!(UpperTriangular(C.UL), Ac_ldiv_B!(UpperTriangular(C.UL), B[C.piv,:]))[invperm(C.piv),:]

function det{T,S,UpLo}(C::Cholesky{T,S,UpLo})
    dd = one(T)
    for i in 1:size(C.UL,1) dd *= abs2(C.UL[i,i]) end
    dd
end

det{T}(C::CholeskyPivoted{T}) = C.rank<size(C.UL,1) ? real(zero(T)) : prod(abs2(diag(C.UL)))

function logdet{T,S,UpLo}(C::Cholesky{T,S,UpLo})
    dd = zero(T)
    for i in 1:size(C.UL,1) dd += log(C.UL[i,i]) end
    dd + dd # instead of 2.0dd which can change the type
end

inv{T<:BlasFloat,S<:AbstractMatrix}(C::Cholesky{T,S,:U}) = copytri!(LAPACK.potri!('U', copy(C.UL)), 'U', true)
inv{T<:BlasFloat,S<:AbstractMatrix}(C::Cholesky{T,S,:L}) = copytri!(LAPACK.potri!('L', copy(C.UL)), 'L', true)

function inv(C::CholeskyPivoted)
    chkfullrank(C)
    ipiv = invperm(C.piv)
    copytri!(LAPACK.potri!(C.uplo, copy(C.UL)), C.uplo, true)[ipiv, ipiv]
end

chkfullrank(C::CholeskyPivoted) = C.rank<size(C.UL, 1) && throw(RankDeficientException(C.info))

rank(C::CholeskyPivoted) = C.rank
