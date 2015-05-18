# This file is a part of Julia. License is MIT: http://julialang.org/license

# QR and Hessenberg Factorizations

immutable QR{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    QR(factors::AbstractMatrix{T}, τ::Vector{T}) = new(factors, τ)
end
QR{T}(factors::AbstractMatrix{T}, τ::Vector{T}) = QR{T,typeof(factors)}(factors, τ)
# Note. For QRCompactWY factorization without pivoting, the WY representation based method introduced in LAPACK 3.4
immutable QRCompactWY{S,M<:AbstractMatrix} <: Factorization{S}
    factors::M
    T::Matrix{S}
    QRCompactWY(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) = new(factors, T)
end
QRCompactWY{S}(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) = QRCompactWY{S,typeof(factors)}(factors, T)

immutable QRPivoted{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    jpvt::Vector{BlasInt}
    QRPivoted(factors::AbstractMatrix{T}, τ::Vector{T}, jpvt::Vector{BlasInt}) = new(factors, τ, jpvt)
end
QRPivoted{T}(factors::AbstractMatrix{T}, τ::Vector{T}, jpvt::Vector{BlasInt}) = QRPivoted{T,typeof(factors)}(factors, τ, jpvt)

function qrfact!{T}(A::AbstractMatrix{T}, pivot::Union(Type{Val{false}}, Type{Val{true}})=Val{false})
    pivot==Val{true} && warn("pivoting only implemented for Float32, Float64, Complex64 and Complex128")
    m, n = size(A)
    τ = zeros(T, min(m,n))
    @inbounds begin
        for k = 1:min(m-1+!(T<:Real),n)
            τk = elementaryLeft!(A, k, k)
            τ[k] = τk
            for j = k+1:n
                vAj = A[k,j]
                for i = k+1:m
                    vAj += conj(A[i,k])*A[i,j]
                end
                vAj = conj(τk)*vAj
                A[k,j] -= vAj
                for i = k+1:m
                    A[i,j] -= A[i,k]*vAj
                end
            end
        end
    end
    QR(A, τ)
end
qrfact!{T<:BlasFloat}(A::StridedMatrix{T}, pivot::Type{Val{false}} = Val{false}) = QRCompactWY(LAPACK.geqrt!(A, min(minimum(size(A)), 36))...)
qrfact!{T<:BlasFloat}(A::StridedMatrix{T}, pivot::Type{Val{true}}) = QRPivoted(LAPACK.geqp3!(A)...)
qrfact{T<:BlasFloat}(A::StridedMatrix{T}, pivot::Union(Type{Val{false}}, Type{Val{true}})=Val{false}) = qrfact!(copy(A), pivot)
copy_oftype{T}(A::StridedMatrix{T}, ::Type{T}) = copy(A)
copy_oftype{T,S}(A::StridedMatrix{T}, ::Type{S}) = convert(AbstractMatrix{S}, A)
qrfact{T}(A::StridedMatrix{T}, pivot::Union(Type{Val{false}}, Type{Val{true}})=Val{false}) = qrfact!(copy_oftype(A, typeof(one(T)/norm(one(T)))), pivot)
qrfact(x::Number) = qrfact(fill(x,1,1))

qr(A::Union(Number, AbstractMatrix), pivot::Union(Type{Val{false}}, Type{Val{true}})=Val{false}; thin::Bool=true) =
    _qr(A, pivot, thin=thin)
function _qr(A::Union(Number, AbstractMatrix), ::Type{Val{false}}; thin::Bool=true)
    F = qrfact(A, Val{false})
    full(getq(F), thin=thin), F[:R]::Matrix{eltype(F)}
end
function _qr(A::Union(Number, AbstractMatrix), ::Type{Val{true}}; thin::Bool=true)
    F = qrfact(A, Val{true})
    full(getq(F), thin=thin), F[:R]::Matrix{eltype(F)}, F[:p]::Vector{BlasInt}
end

convert{T}(::Type{QR{T}},A::QR) = QR(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ))
convert{T}(::Type{Factorization{T}}, A::QR) = convert(QR{T}, A)
convert{T}(::Type{QRCompactWY{T}},A::QRCompactWY) = QRCompactWY(convert(AbstractMatrix{T}, A.factors), convert(AbstractMatrix{T}, A.T))
convert{T}(::Type{Factorization{T}}, A::QRCompactWY) = convert(QRCompactWY{T}, A)
convert{T}(::Type{QRPivoted{T}},A::QRPivoted) = QRPivoted(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ), A.jpvt)
convert{T}(::Type{Factorization{T}}, A::QRPivoted) = convert(QRPivoted{T}, A)

function getindex(A::QR, d::Symbol)
    m, n = size(A)
    d == :R && return triu!(A.factors[1:min(m,n), 1:n])
    d == :Q && return getq(A)
    throw(KeyError(d))
end
function getindex(A::QRCompactWY, d::Symbol)
    m, n = size(A)
    d == :R && return triu!(A.factors[1:min(m,n), 1:n])
    d == :Q && return getq(A)
    throw(KeyError(d))
end
function getindex{T}(A::QRPivoted{T}, d::Symbol)
    m, n = size(A)
    d == :R && return triu!(A.factors[1:min(m,n), 1:n])
    d == :Q && return getq(A)
    d == :p && return A.jpvt
    if d == :P
        p = A[:p]
        n = length(p)
        P = zeros(T, n, n)
        for i in 1:n
            P[p[i],i] = one(T)
        end
        return P
    end
    throw(KeyError(d))
end

# Type-stable interface to get Q
getq(A::QRCompactWY) = QRCompactWYQ(A.factors,A.T)
getq(A::Union(QR, QRPivoted)) = QRPackedQ(A.factors,A.τ)

immutable QRPackedQ{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    factors::S
    τ::Vector{T}
    QRPackedQ(factors::AbstractMatrix{T}, τ::Vector{T}) = new(factors, τ)
end
QRPackedQ{T}(factors::AbstractMatrix{T}, τ::Vector{T}) = QRPackedQ{T,typeof(factors)}(factors, τ)

immutable QRPackedWYQ{S,M<:AbstractMatrix} <: AbstractMatrix{S}
    factors::M
    T::Matrix{S}
    QRPackedWYQ(factors::AbstractMatrix{S}, T::Matrix{S}) = new(factors, T)
end
QRPackedWYQ{S}(factors::AbstractMatrix{S}, T::Matrix{S}) = QRPackedWYQ{S,typeof(factors)}(factors, T)

immutable QRCompactWYQ{S, M<:AbstractMatrix} <: AbstractMatrix{S}
    factors::M
    T::Matrix{S}
    QRCompactWYQ(factors::AbstractMatrix{S}, T::Matrix{S}) = new(factors, T)
end
QRCompactWYQ{S}(factors::AbstractMatrix{S}, T::Matrix{S}) = QRCompactWYQ{S,typeof(factors)}(factors, T)

convert{T}(::Type{QRPackedQ{T}}, Q::QRPackedQ) = QRPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
convert{T}(::Type{AbstractMatrix{T}}, Q::QRPackedQ) = convert(QRPackedQ{T}, Q)
convert{S}(::Type{QRCompactWYQ{S}}, Q::QRCompactWYQ) = QRCompactWYQ(convert(AbstractMatrix{S}, Q.factors), convert(AbstractMatrix{S}, Q.T))
convert{S}(::Type{AbstractMatrix{S}}, Q::QRCompactWYQ) = convert(QRCompactWYQ{S}, Q)

size(A::Union(QR,QRCompactWY,QRPivoted), dim::Integer) = size(A.factors, dim)
size(A::Union(QR,QRCompactWY,QRPivoted)) = size(A.factors)
size(A::Union(QRPackedQ,QRCompactWYQ), dim::Integer) = 0 < dim ? (dim <= 2 ? size(A.factors, 1) : 1) : throw(BoundsError())
size(A::Union(QRPackedQ,QRCompactWYQ)) = size(A, 1), size(A, 2)

full{T}(A::Union(QRPackedQ{T},QRCompactWYQ{T}); thin::Bool=true) = A_mul_B!(A, thin ? eye(T, size(A.factors,1), minimum(size(A.factors))) : eye(T, size(A.factors,1)))

## Multiplication by Q
### QB
A_mul_B!{T<:BlasFloat}(A::QRCompactWYQ{T}, B::StridedVecOrMat{T}) = LAPACK.gemqrt!('L','N',A.factors,A.T,B)
A_mul_B!{T<:BlasFloat}(A::QRPackedQ{T}, B::StridedVecOrMat{T}) = LAPACK.ormqr!('L','N',A.factors,A.τ,B)
function A_mul_B!{T}(A::QRPackedQ{T}, B::AbstractVecOrMat{T})
    mA, nA = size(A.factors)
    mB, nB = size(B,1), size(B,2)
    mA == mB || throw(DimensionMismatch())
    Afactors = A.factors
    @inbounds begin
        for k = min(mA,nA):-1:1
            for j = 1:nB
                vBj = B[k,j]
                for i = k+1:mB
                    vBj += conj(Afactors[i,k])*B[i,j]
                end
                vBj = A.τ[k]*vBj
                B[k,j] -= vBj
                for i = k+1:mB
                    B[i,j] -= Afactors[i,k]*vBj
                end
            end
        end
    end
    B
end

function (*){TA,Tb}(A::Union(QRPackedQ{TA},QRCompactWYQ{TA}), b::StridedVector{Tb})
    TAb = promote_type(TA, Tb)
    Anew = convert(AbstractMatrix{TAb}, A)
    if size(A.factors, 1) == length(b)
        bnew = copy_oftype(b, TAb)
    elseif size(A.factors, 2) == length(b)
        bnew = [b; zeros(TAb, size(A.factors, 1) - length(b))]
    else
        throw(DimensionMismatch("vector must have length either $(size(A.factors, 1)) or $(size(A.factors, 2))"))
    end
    A_mul_B!(Anew, bnew)
end
function (*){TA,TB}(A::Union(QRPackedQ{TA},QRCompactWYQ{TA}), B::StridedMatrix{TB})
    TAB = promote_type(TA, TB)
    Anew = convert(AbstractMatrix{TAB}, A)
    if size(A.factors, 1) == size(B, 1)
        Bnew = copy_oftype(B, TAB)
    elseif size(A.factors, 2) == size(B, 1)
        Bnew = [B; zeros(TAB, size(A.factors, 1) - size(B,1), size(B, 2))]
    else
        throw(DimensionMismatch("first dimension of matrix must have size either $(size(A.factors, 1)) or $(size(A.factors, 2))"))
    end
    A_mul_B!(Anew, Bnew)
end

### QcB
Ac_mul_B!{T<:BlasReal}(A::QRCompactWYQ{T}, B::StridedVecOrMat{T}) = LAPACK.gemqrt!('L','T',A.factors,A.T,B)
Ac_mul_B!{T<:BlasComplex}(A::QRCompactWYQ{T}, B::StridedVecOrMat{T}) = LAPACK.gemqrt!('L','C',A.factors,A.T,B)
Ac_mul_B!{T<:BlasReal}(A::QRPackedQ{T}, B::StridedVecOrMat{T}) = LAPACK.ormqr!('L','T',A.factors,A.τ,B)
Ac_mul_B!{T<:BlasComplex}(A::QRPackedQ{T}, B::StridedVecOrMat{T}) = LAPACK.ormqr!('L','C',A.factors,A.τ,B)
function Ac_mul_B!{T}(A::QRPackedQ{T}, B::AbstractVecOrMat{T})
    mA, nA = size(A.factors)
    mB, nB = size(B,1), size(B,2)
    mA == mB || throw(DimensionMismatch())
    Afactors = A.factors
    @inbounds begin
        for k = 1:min(mA,nA)
            for j = 1:nB
                vBj = B[k,j]
                for i = k+1:mB
                    vBj += conj(Afactors[i,k])*B[i,j]
                end
                vBj = conj(A.τ[k])*vBj
                B[k,j] -= vBj
                for i = k+1:mB
                    B[i,j] -= Afactors[i,k]*vBj
                end
            end
        end
    end
    B
end
function Ac_mul_B{TQ<:Number,TB<:Number,N}(Q::Union(QRPackedQ{TQ},QRCompactWYQ{TQ}), B::StridedArray{TB,N})
    TQB = promote_type(TQ,TB)
    return Ac_mul_B!(convert(AbstractMatrix{TQB}, Q), copy_oftype(B, TQB))
end

### AQ
A_mul_B!{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) = LAPACK.gemqrt!('R','N', B.factors, B.T, A)
A_mul_B!(A::StridedVecOrMat, B::QRPackedQ) = LAPACK.ormqr!('R', 'N', B.factors, B.τ, A)
function A_mul_B!{T}(A::StridedMatrix{T},Q::QRPackedQ{T})
    mQ, nQ = size(Q.factors)
    mA, nA = size(A,1), size(A,2)
    nA == mQ || throw(DimensionMismatch())
    Qfactors = Q.factors
    @inbounds begin
        for k = 1:min(mQ,nQ)
            for i = 1:mA
                vAi = A[i,k]
                for j = k+1:mQ
                    vAi += A[i,j]*Qfactors[j,k]
                end
                vAi = vAi*Q.τ[k]
                A[i,k] -= vAi
                for j = k+1:nA
                    A[i,j] -= vAi*conj(Qfactors[j,k])
                end
            end
        end
    end
    A
end

function (*){TA,TQ,N}(A::StridedArray{TA,N}, Q::Union(QRPackedQ{TQ},QRCompactWYQ{TQ}))
    TAQ = promote_type(TA, TQ)
    return A_mul_B!(copy_oftype(A, TAQ), convert(AbstractMatrix{TAQ}, Q))
end

### AQc
A_mul_Bc!{T<:BlasReal}(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) = LAPACK.gemqrt!('R','T',B.factors,B.T,A)
A_mul_Bc!{T<:BlasComplex}(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) = LAPACK.gemqrt!('R','C',B.factors,B.T,A)
A_mul_Bc!{T<:BlasReal}(A::StridedVecOrMat{T}, B::QRPackedQ{T}) = LAPACK.ormqr!('R','T',B.factors,B.τ,A)
A_mul_Bc!{T<:BlasComplex}(A::StridedVecOrMat{T}, B::QRPackedQ{T}) = LAPACK.ormqr!('R','C',B.factors,B.τ,A)
function A_mul_Bc!{T}(A::AbstractMatrix{T},Q::QRPackedQ{T})
    mQ, nQ = size(Q.factors)
    mA, nA = size(A,1), size(A,2)
    nA == mQ || throw(DimensionMismatch())
    Qfactors = Q.factors
    @inbounds begin
        for k = min(mQ,nQ):-1:1
            for i = 1:mA
                vAi = A[i,k]
                for j = k+1:mQ
                    vAi += A[i,j]*Qfactors[j,k]
                end
                vAi = vAi*conj(Q.τ[k])
                A[i,k] -= vAi
                for j = k+1:nA
                    A[i,j] -= vAi*conj(Qfactors[j,k])
                end
            end
        end
    end
    A
end
A_mul_Bc(A::AbstractTriangular, B::Union(QRCompactWYQ,QRPackedQ)) = A_mul_Bc(full(A), B)
function A_mul_Bc{TA,TB}(A::AbstractArray{TA}, B::Union(QRCompactWYQ{TB},QRPackedQ{TB}))
    TAB = promote_type(TA,TB)
    BB = convert(AbstractMatrix{TAB}, B)
    if size(A,2) == size(B.factors, 1)
        return A_mul_Bc!(copy_oftype(A, TAB), BB)
    elseif size(A,2) == size(B.factors,2)
        return A_mul_Bc!([A zeros(TAB, size(A, 1), size(B.factors, 1) - size(B.factors, 2))], BB)
    else
        throw(DimensionMismatch())
    end
end

A_ldiv_B!{T<:BlasFloat}(A::QRCompactWY{T}, b::StridedVector{T}) = (A_ldiv_B!(UpperTriangular(A[:R]), sub(Ac_mul_B!(A[:Q], b), 1:size(A, 2))); b)
A_ldiv_B!{T<:BlasFloat}(A::QRCompactWY{T}, B::StridedMatrix{T}) = (A_ldiv_B!(UpperTriangular(A[:R]), sub(Ac_mul_B!(A[:Q], B), 1:size(A, 2), 1:size(B, 2))); B)

# Julia implementation similarly to xgelsy
function A_ldiv_B!{T<:BlasFloat}(A::QRPivoted{T}, B::StridedMatrix{T}, rcond::Real)
    mA, nA = size(A.factors)
    nr = min(mA,nA)
    nrhs = size(B, 2)
    if nr == 0 return zeros(T, 0, nrhs), 0 end
    ar = abs(A.factors[1])
    if ar == 0 return zeros(T, nr, nrhs), 0 end
    rnk = 1
    xmin = ones(T, 1)
    xmax = ones(T, 1)
    tmin = tmax = ar
    while rnk < nr
        tmin, smin, cmin = LAPACK.laic1!(2, xmin, tmin, sub(A.factors, 1:rnk, rnk + 1), A.factors[rnk + 1, rnk + 1])
        tmax, smax, cmax = LAPACK.laic1!(1, xmax, tmax, sub(A.factors, 1:rnk, rnk + 1), A.factors[rnk + 1, rnk + 1])
        tmax*rcond > tmin && break
        push!(xmin, cmin)
        push!(xmax, cmax)
        for i = 1:rnk
            xmin[i] *= smin
            xmax[i] *= smax
        end
        rnk += 1
    end
    C, τ = LAPACK.tzrzf!(A.factors[1:rnk,:])
    A_ldiv_B!(UpperTriangular(C[1:rnk,1:rnk]),sub(Ac_mul_B!(getq(A),sub(B, 1:mA, 1:nrhs)),1:rnk,1:nrhs))
    B[rnk+1:end,:] = zero(T)
    LAPACK.ormrz!('L', iseltype(B, Complex) ? 'C' : 'T', C, τ, sub(B,1:nA,1:nrhs))
    B[1:nA,:] = sub(B, 1:nA, :)[invperm(A[:p]::Vector{BlasInt}),:]
    return B, rnk
end
A_ldiv_B!{T<:BlasFloat}(A::QRPivoted{T}, B::StridedVector{T}) = vec(A_ldiv_B!(A,reshape(B,length(B),1)))
A_ldiv_B!{T<:BlasFloat}(A::QRPivoted{T}, B::StridedVecOrMat{T}) = A_ldiv_B!(A, B, maximum(size(A))*eps(real(float(one(eltype(B))))))[1]
function A_ldiv_B!{T}(A::QR{T},B::StridedMatrix{T})
    m, n = size(A)
    minmn = min(m,n)
    mB, nB = size(B)
    Ac_mul_B!(A[:Q],sub(B,1:m,1:nB)) # Reconsider when arrayviews are merged.
    R = A[:R]
    @inbounds begin
        if n > m # minimum norm solution
            τ = zeros(T,m)
            for k = m:-1:1 # Trapezoid to triangular by elementary operation
                τ[k] = elementaryRightTrapezoid!(R,k)
                for i = 1:k-1
                    vRi = R[i,k]
                    for j = m+1:n
                        vRi += R[i,j]*R[k,j]
                    end
                    vRi *= τ[k]
                    R[i,k] -= vRi
                    for j = m+1:n
                        R[i,j] -= vRi*R[k,j]
                    end
                end
            end
        end
        for k = 1:nB # solve triangular system. When array views are implemented, consider exporting    to function.
            for i = minmn:-1:1
                for j = i+1:minmn
                    B[i,k] -= R[i,j]*B[j,k]
                end
                B[i,k] /= R[i,i]
            end
        end
        if n > m # Apply elementary transformation to solution
            B[m+1:mB,1:nB] = zero(T)
            for j = 1:nB
                for k = 1:m
                    vBj = B[k,j]
                    for i = m+1:n
                        vBj += B[i,j]*conj(R[k,i])
                    end
                    vBj *= τ[k]
                    B[k,j] -= vBj
                    for i = m+1:n
                        B[i,j] -= R[k,i]*vBj
                    end
                end
            end
        end
    end
    return B
end
A_ldiv_B!(A::QR, B::StridedVector) = A_ldiv_B!(A, reshape(B, length(B), 1))[:]
function A_ldiv_B!(A::QRPivoted, b::StridedVector)
    A_ldiv_B!(QR(A.factors,A.τ), b)
    b[1:size(A.factors, 2)] = sub(b, 1:size(A.factors, 2))[invperm(A.jpvt)]
    b
end
function A_ldiv_B!(A::QRPivoted, B::StridedMatrix)
    A_ldiv_B!(QR(A.factors, A.τ), B)
    B[1:size(A.factors, 2),:] = sub(B, 1:size(A.factors, 2), :)[invperm(A.jpvt)]
    B
end
function \{TA,Tb}(A::Union(QR{TA},QRCompactWY{TA},QRPivoted{TA}), b::StridedVector{Tb})
    S = promote_type(TA,Tb)
    m,n = size(A)
    m == length(b) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has length $(length(b))"))
    AA = convert(Factorization{S}, A)
    if n > m
        x = A_ldiv_B!(AA, [b; zeros(S, n - m)])
    else
        x = A_ldiv_B!(AA, copy_oftype(b, S))
    end
    return length(x) > n ? x[1:n] : x
end
function \{TA,TB}(A::Union(QR{TA},QRCompactWY{TA},QRPivoted{TA}),B::StridedMatrix{TB})
    S = promote_type(TA,TB)
    m,n = size(A)
    m == size(B,1) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has $(size(B,1)) rows"))
    AA = convert(Factorization{S}, A)
    if n > m
        X = A_ldiv_B!(AA, [B; zeros(S, n - m, size(B, 2))])
    else
        X = A_ldiv_B!(AA, copy_oftype(B, S))
    end
    return size(X, 1) > n ? X[1:n,:] : X
end

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

# FIXME! Should add balancing option through xgebal
immutable Hessenberg{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    Hessenberg(factors::AbstractMatrix{T}, τ::Vector{T}) = new(factors, τ)
end
Hessenberg{T}(factors::AbstractMatrix{T}, τ::Vector{T}) = Hessenberg{T,typeof(factors)}(factors, τ)

Hessenberg(A::StridedMatrix) = Hessenberg(LAPACK.gehrd!(A)...)

hessfact!{T<:BlasFloat}(A::StridedMatrix{T}) = Hessenberg(A)
hessfact{T<:BlasFloat}(A::StridedMatrix{T}) = hessfact!(copy(A))
function hessfact{T}(A::StridedMatrix{T})
    S = promote_type(Float32, typeof(one(T)/norm(one(T))))
    return hessfact!(copy_oftype(A, S))
end

immutable HessenbergQ{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    factors::S
    τ::Vector{T}
    HessenbergQ(factors::AbstractMatrix{T}, τ::Vector{T}) = new(factors, τ)
end
HessenbergQ{T}(factors::AbstractMatrix{T}, τ::Vector{T}) = HessenbergQ{T,typeof(factors)}(factors, τ)
HessenbergQ(A::Hessenberg) = HessenbergQ(A.factors, A.τ)
size(A::HessenbergQ, args...) = size(A.factors, args...)

function getindex(A::Hessenberg, d::Symbol)
    d == :Q && return HessenbergQ(A)
    d == :H && return triu(A.factors, -1)
    throw(KeyError(d))
end

# Also printing of QRQs
getindex(A::Union(QRPackedQ,QRCompactWYQ,HessenbergQ), i::Integer, j::Integer) = (x = zeros(eltype(A), size(A, 1)); x[i] = 1; y = zeros(eltype(A), size(A, 2)); y[j] = 1; dot(x, A*y))

## reconstruct the original matrix
full(F::QR) = F[:Q] * F[:R]
full(F::QRCompactWY) = F[:Q] * F[:R]
full(F::QRPivoted) = (F[:Q] * F[:R])[:,invperm(F[:p])]

full(A::HessenbergQ) = LAPACK.orghr!(1, size(A.factors, 1), copy(A.factors), A.τ)
full(F::Hessenberg) = (fq = full(F[:Q]); (fq * F[:H]) * fq')
