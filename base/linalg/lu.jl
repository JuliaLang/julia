####################
# LU Factorization #
####################
immutable LU{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    ipiv::Vector{BlasInt}
    info::BlasInt
    LU(factors::AbstractMatrix{T}, ipiv::Vector{BlasInt}, info::BlasInt) = new(factors, ipiv, info)
end
LU{T}(factors::AbstractMatrix{T}, ipiv::Vector{BlasInt}, info::BlasInt) = LU{T,typeof(factors)}(factors, ipiv, info)

# StridedMatrix
function lufact!{T<:BlasFloat}(A::StridedMatrix{T}; pivot = true)
    !pivot && return generic_lufact!(A, pivot=pivot)
    lpt = LAPACK.getrf!(A)
    return LU{T,typeof(A)}(lpt[1], lpt[2], lpt[3])
end
lufact!(A::StridedMatrix; pivot = true) = generic_lufact!(A, pivot=pivot)
function generic_lufact!{T}(A::StridedMatrix{T}; pivot = true)
    m, n = size(A)
    minmn = min(m,n)
    info = 0
    ipiv = Array(BlasInt, minmn)
    @inbounds begin
        for k = 1:minmn
            # find index max
            kp = k
            if pivot
                amax = real(zero(T))
                for i = k:m
                    absi = abs(A[i,k])
                    if absi > amax
                        kp = i
                        amax = absi
                    end
                end
            end
            ipiv[k] = kp
            if A[kp,k] != 0
                if k != kp
                    # Interchange
                    for i = 1:n
                        tmp = A[k,i]
                        A[k,i] = A[kp,i]
                        A[kp,i] = tmp
                    end
                end
                # Scale first column
                Akkinv = inv(A[k,k])
                for i = k+1:m
                    A[i,k] *= Akkinv
                end
            elseif info == 0
                info = k
            end
            # Update the rest
            for j = k+1:n
                for i = k+1:m
                    A[i,j] -= A[i,k]*A[k,j]
                end
            end
        end
    end
    LU{T,typeof(A)}(A, ipiv, convert(BlasInt, info))
end
lufact{T<:BlasFloat}(A::AbstractMatrix{T}; pivot = true) = lufact!(copy(A), pivot=pivot)
lufact{T}(A::AbstractMatrix{T}; pivot = true) = (S = typeof(zero(T)/one(T)); S != T ? lufact!(convert(AbstractMatrix{S}, A), pivot=pivot) : lufact!(copy(A), pivot=pivot))
lufact(x::Number) = LU(fill(x, 1, 1), BlasInt[1], x == 0 ? one(BlasInt) : zero(BlasInt))
lufact(F::LU) = F

lu(x::Number) = (one(x), x, 1)
function lu(A::AbstractMatrix; pivot = true)
    F = lufact(A, pivot = pivot)
    F[:L], F[:U], F[:p]
end

function convert{T}(::Type{LU{T}}, F::LU)
    M = convert(AbstractMatrix{T}, F.factors)
    LU{T,typeof(M)}(M, F.ipiv, F.info)
end
convert{T,S}(::Type{LU{T,S}}, F::LU) = LU{T,S}(convert(S, F.factors), F.ipiv, F.info)
convert{T}(::Type{Factorization{T}}, F::LU) = convert(LU{T}, F)


size(A::LU) = size(A.factors)
size(A::LU,n) = size(A.factors,n)

function ipiv2perm{T}(v::AbstractVector{T}, maxi::Integer)
    p = T[1:maxi]
    @inbounds for i in 1:length(v)
        p[i], p[v[i]] = p[v[i]], p[i]
    end
    return p
end

function getindex{T,S<:StridedMatrix}(A::LU{T,S}, d::Symbol)
    m, n = size(A)
    if d == :L
        L = tril!(A.factors[1:m, 1:min(m,n)])
        for i = 1:min(m,n); L[i,i] = one(T); end
        return L
    end
    d == :U && return triu!(A.factors[1:min(m,n), 1:n])
    d == :p && return ipiv2perm(A.ipiv, m)
    if d == :P
        p = A[:p]
        P = zeros(T, m, m)
        for i in 1:m
            P[i,p[i]] = one(T)
        end
        return P
    end
    throw(KeyError(d))
end

A_ldiv_B!{T<:BlasFloat, S<:StridedMatrix}(A::LU{T, S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('N', A.factors, A.ipiv, B) A.info
A_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, b::StridedVector) = A_ldiv_B!(Triangular(A.factors, :U, false), A_ldiv_B!(Triangular(A.factors, :L, true), b[ipiv2perm(A.ipiv, length(b))]))
A_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, B::StridedMatrix) = A_ldiv_B!(Triangular(A.factors, :U, false), A_ldiv_B!(Triangular(A.factors, :L, true), B[ipiv2perm(A.ipiv, size(B, 1)),:]))
At_ldiv_B{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('T', A.factors, A.ipiv, copy(B)) A.info
Ac_ldiv_B{T<:BlasComplex,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('C', A.factors, A.ipiv, copy(B)) A.info
At_ldiv_Bt{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('T', A.factors, A.ipiv, transpose(B)) A.info
Ac_ldiv_Bc{T<:BlasComplex,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('C', A.factors, A.ipiv, ctranspose(B)) A.info

function det{T,S}(A::LU{T,S})
    n = chksquare(A)
    A.info > 0 && return zero(typeof(A.factors[1]))
    return prod(diag(A.factors)) * (bool(sum(A.ipiv .!= 1:n) % 2) ? -one(T) : one(T))
end

function logdet2{T<:Real,S}(A::LU{T,S})  # return log(abs(det)) and sign(det)
    n = chksquare(A)
    dg = diag(A.factors)
    s = (bool(sum(A.ipiv .!= 1:n) % 2) ? -one(T) : one(T)) * prod(sign(dg))
    sum(log(abs(dg))), s
end

function logdet{T<:Real,S}(A::LU{T,S})
    d,s = logdet2(A)
    s>=0 || error("DomainError: determinant is negative")
    d
end

function logdet{T<:Complex,S}(A::LU{T,S})
    n = chksquare(A)
    s = sum(log(diag(A.factors))) + (bool(sum(A.ipiv .!= 1:n) % 2) ? complex(0,pi) : 0)
    r, a = reim(s)
    a = pi-mod(pi-a,2pi) #Take principal branch with argument (-pi,pi]
    complex(r,a)
end

inv{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}) = @assertnonsingular LAPACK.getri!(copy(A.factors), A.ipiv) A.info

cond{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}, p::Number) = inv(LAPACK.gecon!(p == 1 ? '1' : 'I', A.factors, norm((A[:L]*A[:U])[A[:p],:], p)))
cond(A::LU, p::Number) = norm(A[:L]*A[:U],p)*norm(inv(A),p)

# Tridiagonal

# See dgttrf.f
function lufact!{T}(A::Tridiagonal{T}; pivot = true)
    n = size(A, 1)
    info = 0
    ipiv = Array(BlasInt, n)
    dl = A.dl
    d = A.d
    du = A.du
    du2 = A.du2

    @inbounds begin
        for i = 1:n
            ipiv[i] = i
        end
        for i = 1:n-2
            # pivot or not?
            if !pivot || abs(d[i]) >= abs(dl[i])
                # No interchange
                if d[i] != 0
                    fact = dl[i]/d[i]
                    dl[i] = fact
                    d[i+1] -= fact*du[i]
                    du2[i] = 0
                end
            else
                # Interchange
                fact = d[i]/dl[i]
                d[i] = dl[i]
                dl[i] = fact
                tmp = du[i]
                du[i] = d[i+1]
                d[i+1] = tmp - fact*d[i+1]
                du2[i] = du[i+1]
                du[i+1] = -fact*du[i+1]
                ipiv[i] = i+1
            end
        end
        if n > 1
            i = n-1
            if !pivot || abs(d[i]) >= abs(dl[i])
                if d[i] != 0
                    fact = dl[i]/d[i]
                    dl[i] = fact
                    d[i+1] -= fact*du[i]
                end
            else
                fact = d[i]/dl[i]
                d[i] = dl[i]
                dl[i] = fact
                tmp = du[i]
                du[i] = d[i+1]
                d[i+1] = tmp - fact*d[i+1]
                ipiv[i] = i+1
            end
        end
        # check for a zero on the diagonal of U
        for i = 1:n
            if d[i] == 0
                info = i
                break
            end
        end
    end
    LU{T,Tridiagonal{T}}(A, ipiv, convert(BlasInt, info))
end

factorize(A::Tridiagonal) = lufact(A)

# See dgtts2.f
function A_ldiv_B!{T}(A::LU{T,Tridiagonal{T}}, B::AbstractVecOrMat)
    n = size(A,1)
    n == size(B,1) || throw(DimensionMismatch())
    nrhs = size(B,2)
    dl = A.factors.dl
    d = A.factors.d
    du = A.factors.du
    du2 = A.factors.du2
    ipiv = A.ipiv
    @inbounds begin
        for j = 1:nrhs
            for i = 1:n-1
                ip = ipiv[i]
                tmp = B[i+1-ip+i,j] - dl[i]*B[ip,j]
                B[i,j] = B[ip,j]
                B[i+1,j] = tmp
            end
            B[n,j] /= d[n]
            if n > 1
                B[n-1,j] = (B[n-1,j] - du[n-1]*B[n,j])/d[n-1]
            end
            for i = n-2:-1:1
                B[i,j] = (B[i,j] - du[i]*B[i+1,j] - du2[i]*B[i+2,j])/d[i]
            end
        end
    end
    return B
end

function At_ldiv_B!{T}(A::LU{T,Tridiagonal{T}}, B::AbstractVecOrMat)
    n = size(A,1)
    n == size(B,1) || throw(DimentionsMismatch(""))
    nrhs = size(B,2)
    dl = A.factors.dl
    d = A.factors.d
    du = A.factors.du
    du2 = A.factors.du2
    ipiv = A.ipiv
    @inbounds begin
        for j = 1:nrhs
            B[1,j] /= d[1]
            if n > 1
                B[2,j] = (B[2,j] - du[1]*B[1,j])/d[2]
            end
            for i = 3:n
                B[i,j] = (B[i,j] - du[i-1]*B[i-1,j] - du2[i-2]*B[i-2,j])/d[i]
            end
            for i = n-1:-1:1
                if ipiv[i] == i
                    B[i,j] = B[i,j] - dl[i]*B[i+1,j]
                else
                    tmp = B[i+1,j]
                    B[i+1,j] = B[i,j] - dl[i]*tmp
                    B[i,j] = tmp
                end
            end
        end
    end
    return B
end

# Ac_ldiv_B!{T<:Real}(A::LU{T,Tridiagonal{T}}, B::AbstractVecOrMat) = At_ldiv_B!(A,B)
function Ac_ldiv_B!{T}(A::LU{T,Tridiagonal{T}}, B::AbstractVecOrMat)
    n = size(A,1)
    n == size(B,1) || throw(DimentionsMismatch(""))
    nrhs = size(B,2)
    dl = A.factors.dl
    d = A.factors.d
    du = A.factors.du
    du2 = A.factors.du2
    ipiv = A.ipiv
    @inbounds begin
        for j = 1:nrhs
            B[1,j] /= conj(d[1])
            if n > 1
                B[2,j] = (B[2,j] - conj(du[1])*B[1,j])/conj(d[2])
            end
            for i = 3:n
                B[i,j] = (B[i,j] - conj(du[i-1])*B[i-1,j] - conj(du2[i-2])*B[i-2,j])/conj(d[i])
            end
            for i = n-1:-1:1
                if ipiv[i] == i
                    B[i,j] = B[i,j] - conj(dl[i])*B[i+1,j]
                else
                    tmp = B[i+1,j]
                    B[i+1,j] = B[i,j] - conj(dl[i])*tmp
                    B[i,j] = tmp
                end
            end
        end
    end
    return B
end

/(B::AbstractMatrix,A::LU) = At_ldiv_Bt(A,B).'

