##########################
# Cholesky Factorization #
##########################
immutable Cholesky{T,S<:AbstractMatrix,UpLo} <: Factorization{T}
    UL::S
end
immutable CholeskyPivoted{T} <: Factorization{T}
    UL::Matrix{T}
    uplo::Char
    piv::Vector{BlasInt}
    rank::BlasInt
    tol::Real
    info::BlasInt
end

for (uplos, uploc) in ((:(:U), 'U'), (:(:L), 'L'))
    @eval begin
        function chol!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Token{$uplos}})
            C, info = LAPACK.potrf!($uploc, A)
            return @assertposdef Triangular{eltype(C),typeof(C),$uplos,false}(C) info
        end
    end
end

function chol!{T}(A::AbstractMatrix{T}, ::Type{Token{:L}})
    n = chksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[k,i]*A[k,i]'
            end
            A[k,k] = chol!(A[k,k], Token{:L})
            AkkInv = inv(A[k,k]')
            for j = 1:k
                for i = k + 1:n
                    j == 1 && (A[i,k] = A[i,k]*AkkInv)
                    j < k && (A[i,k] -= A[i,j]*A[k,j]'*AkkInv)
                end
            end
        end
    end
    return Triangular{T,typeof(A),:L,false}(A)
end
function chol!{T}(A::AbstractMatrix{T}, ::Type{Token{:U}})
    n = chksquare(A)
    @inbounds begin
        for k = 1:n
            for i = 1:k - 1
                A[k,k] -= A[i,k]'A[i,k]
            end
            A[k,k] = chol!(A[k,k], Token{:U})
            AkkInv = inv(A[k,k])
            for j = k + 1:n
                for i = 1:k - 1
                    A[k,j] -= A[i,k]'A[i,j]
                end
                A[k,j] = A[k,k]'\A[k,j]
            end
        end
    end
    return Triangular{T,typeof(A),:U,false}(A)
end
chol!(A::AbstractMatrix) = chol!(A, Token{:U})

cholfact!{T<:BlasFloat,uplo}(A::StridedMatrix{T}, ::Type{Token{uplo}}) = Cholesky{T,typeof(A),uplo}(chol!(A, Token{uplo}).data)
function cholfact!{T<:BlasFloat,uplo}(A::StridedMatrix{T}, ::Type{Token{uplo}}, ::Type{Token{:pivot}}; tol = 0.0)
    uplochar = char_uplo(Token{uplo})
    A, piv, rank, info = LAPACK.pstrf!(uplochar, A, tol)
    return CholeskyPivoted{T}(A, uplochar, piv, rank, tol, info)
end
cholfact!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Token{:pivot}}; tol = 0.0) = cholfact!(A, Token{:U}, Token{:pivot}, tol = tol)

cholfact!{UpLo}(A::AbstractMatrix, ::Type{Token{UpLo}}) = Cholesky{eltype(A),typeof(A),UpLo}(chol!(A, Token{UpLo}).data)
cholfact!(A::AbstractMatrix) = cholfact!(A, Token{:U})

function cholfact!{T<:BlasFloat,S,UpLo}(C::Cholesky{T,S,UpLo})
    _, info = LAPACK.potrf!(char_uplo(UpLo), C.UL)
    info[1]>0 && throw(PosDefException(info[1]))
    C
end

cholfact{T<:BlasFloat}(A::StridedMatrix{T}, args...; kwargs...) = cholfact!(copy(A), args...; kwargs...)
function cholfact{T}(A::AbstractMatrix{T}, args...; kwargs...)
    S = promote_type(typeof(chol(one(T))),Float32)
    # S <: BlasFloat && return cholfact!(convert(AbstractMatrix{S}, A), args...; kwargs...)
    # pivot && throw(ArgumentError("pivot only supported for Float32, Float64, Complex{Float32} and Complex{Float64}"))
    S != T && return cholfact!(convert(AbstractMatrix{S}, A), args..., kwargs...)
    return cholfact!(copy(A), args...; kwargs...)
end
function cholfact(x::Number, ::Type{Token{:U}})
    xf = fill(chol!(x), 1, 1)
    Cholesky{:U, eltype(xf), typeof(xf)}(xf)
end
function cholfact(x::Number, ::Type{Token{:L}})
    xf = fill(chol!(x), 1, 1)
    Cholesky{:L, eltype(xf), typeof(xf)}(xf)
end

chol(A::AbstractMatrix, args...) = chol!(copy(A), args...)
function chol!(x::Number, args...)
    rx = real(x)
    rx == abs(x) || throw(DomainError())
    rxr = sqrt(rx)
    convert(promote_type(typeof(x), typeof(rxr)), rxr)
end
chol(x::Number, args...) = chol!(x, args...)

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
    d == :U && return Triangular(UpLo == d ? C.UL : C.UL', Token{:U})
    d == :L && return Triangular(UpLo == d ? C.UL : C.UL', Token{:L})
    d == :UL && return Triangular(C.UL, Token{UpLo})
    throw(KeyError(d))
end
function getindex{T<:BlasFloat}(C::CholeskyPivoted{T}, d::Symbol)
    d == :U && return Triangular(symbol(C.uplo) == d ? C.UL : C.UL', Token{:U})
    d == :L && return Triangular(symbol(C.uplo) == d ? C.UL : C.UL', Token{:L})
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
A_ldiv_B!{T,S<:AbstractMatrix}(C::Cholesky{T,S,:L}, B::StridedVecOrMat) = Ac_ldiv_B!(Triangular(C.UL, Token{:L}), A_ldiv_B!(Triangular(C.UL, Token{:L}), B))
A_ldiv_B!{T,S<:AbstractMatrix}(C::Cholesky{T,S,:U}, B::StridedVecOrMat) = A_ldiv_B!(Triangular(C.UL, Token{:U}), Ac_ldiv_B!(Triangular(C.UL, Token{:U}), B))

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
A_ldiv_B!(C::CholeskyPivoted, B::StridedVector) = C.uplo == 'L' ? Ac_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), A_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), B[C.piv]))[invperm(C.piv)] : A_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), Ac_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), B[C.piv]))[invperm(C.piv)]
A_ldiv_B!(C::CholeskyPivoted, B::StridedMatrix) = C.uplo=='L' ? Ac_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), A_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), B[C.piv,:]))[invperm(C.piv),:] : A_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), Ac_ldiv_B!(Triangular(C.UL, Token{symbol(C.uplo)}), B[C.piv,:]))[invperm(C.piv),:]

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
