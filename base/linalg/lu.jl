# This file is a part of Julia. License is MIT: http://julialang.org/license

####################
# LU Factorization #
####################
immutable LU{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    ipiv::Vector{BlasInt}
    info::BlasInt
    LU{T,S}(factors::AbstractMatrix{T}, ipiv::Vector{BlasInt}, info::BlasInt) where {T,S} = new(factors, ipiv, info)
end
LU(factors::AbstractMatrix{T}, ipiv::Vector{BlasInt}, info::BlasInt) where T = LU{T,typeof(factors)}(factors, ipiv, info)

# StridedMatrix
function lufact!{T<:BlasFloat}(A::StridedMatrix{T}, pivot::Union{Type{Val{false}}, Type{Val{true}}} = Val{true})
    if pivot === Val{false}
        return generic_lufact!(A, pivot)
    end
    lpt = LAPACK.getrf!(A)
    return LU{T,typeof(A)}(lpt[1], lpt[2], lpt[3])
end

"""
    lufact!(A, pivot=Val{true}) -> LU

`lufact!` is the same as [`lufact`](@ref), but saves space by overwriting the
input `A`, instead of creating a copy. An [`InexactError`](@ref)
exception is thrown if the factorization produces a number not representable by the
element type of `A`, e.g. for integer types.
"""
lufact!(A::StridedMatrix, pivot::Union{Type{Val{false}}, Type{Val{true}}} = Val{true}) = generic_lufact!(A, pivot)
function generic_lufact!{T,Pivot}(A::StridedMatrix{T}, ::Type{Val{Pivot}} = Val{true})
    m, n = size(A)
    minmn = min(m,n)
    info = 0
    ipiv = Array{BlasInt}(minmn)
    @inbounds begin
        for k = 1:minmn
            # find index max
            kp = k
            if Pivot
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

# floating point types doesn't have to be promoted for LU, but should default to pivoting
lufact{T<:AbstractFloat}(A::Union{AbstractMatrix{T},AbstractMatrix{Complex{T}}}, pivot::Union{Type{Val{false}}, Type{Val{true}}} = Val{true}) = lufact!(copy(A), pivot)

# for all other types we must promote to a type which is stable under division
"""
    lufact(A [,pivot=Val{true}]) -> F::LU

Compute the LU factorization of `A`.

In most cases, if `A` is a subtype `S` of `AbstractMatrix{T}` with an element
type `T` supporting `+`, `-`, `*` and `/`, the return type is `LU{T,S{T}}`. If
pivoting is chosen (default) the element type should also support `abs` and
`<`.

The individual components of the factorization `F` can be accessed by indexing:

| Component | Description                         |
|:----------|:------------------------------------|
| `F[:L]`   | `L` (lower triangular) part of `LU` |
| `F[:U]`   | `U` (upper triangular) part of `LU` |
| `F[:p]`   | (right) permutation `Vector`        |
| `F[:P]`   | (right) permutation `Matrix`        |

The relationship between `F` and `A` is

`F[:L]*F[:U] == A[F[:p], :]`

`F` further supports the following functions:

| Supported function               | `LU` | `LU{T,Tridiagonal{T}}` |
|:---------------------------------|:-----|:-----------------------|
| [`/`](@ref)                      | ✓    |                        |
| [`\\`](@ref)                     | ✓    | ✓                      |
| [`cond`](@ref)                   | ✓    |                        |
| [`det`](@ref)                    | ✓    | ✓                      |
| [`logdet`](@ref)                 | ✓    | ✓                      |
| [`logabsdet`](@ref)              | ✓    | ✓                      |
| [`size`](@ref)                   | ✓    | ✓                      |

# Example

```jldoctest
julia> A = [4 3; 6 3]
2×2 Array{Int64,2}:
 4  3
 6  3

julia> F = lufact(A)
Base.LinAlg.LU{Float64,Array{Float64,2}} with factors L and U:
[1.0 0.0; 1.5 1.0]
[4.0 3.0; 0.0 -1.5]

julia> F[:L] * F[:U] == A[F[:p], :]
true
```
"""
function lufact{T}(A::AbstractMatrix{T}, pivot::Union{Type{Val{false}}, Type{Val{true}}})
    S = typeof(zero(T)/one(T))
    AA = similar(A, S, size(A))
    copy!(AA, A)
    lufact!(AA, pivot)
end
# We can't assume an ordered field so we first try without pivoting
function lufact{T}(A::AbstractMatrix{T})
    S = typeof(zero(T)/one(T))
    AA = similar(A, S, size(A))
    copy!(AA, A)
    F = lufact!(AA, Val{false})
    if F.info == 0
        return F
    else
        AA = similar(A, S, size(A))
        copy!(AA, A)
        return lufact!(AA, Val{true})
    end
end

lufact(x::Number) = LU(fill(x, 1, 1), BlasInt[1], x == 0 ? one(BlasInt) : zero(BlasInt))
lufact(F::LU) = F

lu(x::Number) = (one(x), x, 1)

"""
    lu(A, pivot=Val{true}) -> L, U, p

Compute the LU factorization of `A`, such that `A[p,:] = L*U`.
By default, pivoting is used. This can be overridden by passing
`Val{false}` for the second argument.

See also [`lufact`](@ref).

# Example

```jldoctest
julia> A = [4. 3.; 6. 3.]
2×2 Array{Float64,2}:
 4.0  3.0
 6.0  3.0

julia> L, U, p = lu(A)
([1.0 0.0; 0.666667 1.0], [6.0 3.0; 0.0 1.0], [2, 1])

julia> A[p, :] == L * U
true
```
"""
function lu(A::AbstractMatrix, pivot::Union{Type{Val{false}}, Type{Val{true}}} = Val{true})
    F = lufact(A, pivot)
    F[:L], F[:U], F[:p]
end

function convert{T}(::Type{LU{T}}, F::LU)
    M = convert(AbstractMatrix{T}, F.factors)
    LU{T,typeof(M)}(M, F.ipiv, F.info)
end
convert{T,S}(::Type{LU{T,S}}, F::LU) = LU{T,S}(convert(S, F.factors), F.ipiv, F.info)
convert{T}(::Type{Factorization{T}}, F::LU{T}) = F
convert{T}(::Type{Factorization{T}}, F::LU) = convert(LU{T}, F)


size(A::LU) = size(A.factors)
size(A::LU,n) = size(A.factors,n)

function ipiv2perm{T}(v::AbstractVector{T}, maxi::Integer)
    p = T[1:maxi;]
    @inbounds for i in 1:length(v)
        p[i], p[v[i]] = p[v[i]], p[i]
    end
    return p
end

function getindex{T,S<:StridedMatrix}(F::LU{T,S}, d::Symbol)
    m, n = size(F)
    if d == :L
        L = tril!(F.factors[1:m, 1:min(m,n)])
        for i = 1:min(m,n); L[i,i] = one(T); end
        return L
    elseif d == :U
        return triu!(F.factors[1:min(m,n), 1:n])
    elseif d == :p
        return ipiv2perm(F.ipiv, m)
    elseif d == :P
        return eye(T, m)[:,invperm(F[:p])]
    else
        throw(KeyError(d))
    end
end

function show(io::IO, C::LU)
    println(io, "$(typeof(C)) with factors L and U:")
    show(io, C[:L])
    println(io)
    show(io, C[:U])
end

A_ldiv_B!{T<:BlasFloat, S<:StridedMatrix}(A::LU{T, S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('N', A.factors, A.ipiv, B) A.info
A_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, b::StridedVector) = A_ldiv_B!(UpperTriangular(A.factors), A_ldiv_B!(UnitLowerTriangular(A.factors), b[ipiv2perm(A.ipiv, length(b))]))
A_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, B::StridedMatrix) = A_ldiv_B!(UpperTriangular(A.factors), A_ldiv_B!(UnitLowerTriangular(A.factors), B[ipiv2perm(A.ipiv, size(B, 1)),:]))

At_ldiv_B!{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('T', A.factors, A.ipiv, B) A.info
At_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, b::StridedVector) = At_ldiv_B!(UnitLowerTriangular(A.factors), At_ldiv_B!(UpperTriangular(A.factors), b))[invperm(ipiv2perm(A.ipiv, length(b)))]
At_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, B::StridedMatrix) = At_ldiv_B!(UnitLowerTriangular(A.factors), At_ldiv_B!(UpperTriangular(A.factors), B))[invperm(ipiv2perm(A.ipiv, size(B,1))),:]

Ac_ldiv_B!{T<:Real,S<:StridedMatrix}(F::LU{T,S}, B::StridedVecOrMat{T}) = At_ldiv_B!(F, B)
Ac_ldiv_B!{T<:BlasComplex,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('C', A.factors, A.ipiv, B) A.info
Ac_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, b::StridedVector) = Ac_ldiv_B!(UnitLowerTriangular(A.factors), Ac_ldiv_B!(UpperTriangular(A.factors), b))[invperm(ipiv2perm(A.ipiv, length(b)))]
Ac_ldiv_B!{T,S<:StridedMatrix}(A::LU{T,S}, B::StridedMatrix) = Ac_ldiv_B!(UnitLowerTriangular(A.factors), Ac_ldiv_B!(UpperTriangular(A.factors), B))[invperm(ipiv2perm(A.ipiv, size(B,1))),:]

At_ldiv_Bt{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('T', A.factors, A.ipiv, transpose(B)) A.info
At_ldiv_Bt(A::LU, B::StridedVecOrMat) = At_ldiv_B(A, transpose(B))

Ac_ldiv_Bc{T<:BlasComplex,S<:StridedMatrix}(A::LU{T,S}, B::StridedVecOrMat{T}) = @assertnonsingular LAPACK.getrs!('C', A.factors, A.ipiv, ctranspose(B)) A.info
Ac_ldiv_Bc(A::LU, B::StridedVecOrMat) = Ac_ldiv_B(A, ctranspose(B))

function det{T}(A::LU{T})
    n = checksquare(A)
    A.info > 0 && return zero(T)
    P = one(T)
    c = 0
    @inbounds for i = 1:n
        P *= A.factors[i,i]
        if A.ipiv[i] != i
                c += 1
        end
    end
    s = (isodd(c) ? -one(T) : one(T))
    return P * s
end

function logabsdet{T}(A::LU{T})  # return log(abs(det)) and sign(det)
    n = checksquare(A)
    A.info > 0 && return log(zero(real(T))), log(one(T))
    c = 0
    P = one(T)
    abs_det = zero(real(T))
    @inbounds for i = 1:n
        dg_ii = A.factors[i,i]
        P *= sign(dg_ii)
        if A.ipiv[i] != i
            c += 1
        end
        abs_det += log(abs(dg_ii))
    end
    s = ifelse(isodd(c), -one(real(T)), one(real(T))) * P
    abs_det, s
end

inv!{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}) = @assertnonsingular LAPACK.getri!(A.factors, A.ipiv) A.info
inv{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}) = inv!(LU(copy(A.factors), copy(A.ipiv), copy(A.info)))

cond{T<:BlasFloat,S<:StridedMatrix}(A::LU{T,S}, p::Number) = inv(LAPACK.gecon!(p == 1 ? '1' : 'I', A.factors, norm((A[:L]*A[:U])[A[:p],:], p)))
cond(A::LU, p::Number) = norm(A[:L]*A[:U],p)*norm(inv(A),p)

# Tridiagonal

# See dgttrf.f
function lufact!{T}(A::Tridiagonal{T}, pivot::Union{Type{Val{false}}, Type{Val{true}}} = Val{true})
    n = size(A, 1)
    info = 0
    ipiv = Array{BlasInt}(n)
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
            if pivot === Val{false} || abs(d[i]) >= abs(dl[i])
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
            if pivot === Val{false} || abs(d[i]) >= abs(dl[i])
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

function getindex{T}(F::Base.LinAlg.LU{T,Tridiagonal{T}}, d::Symbol)
    m, n = size(F)
    if d == :L
        L = Array(Bidiagonal(ones(T, n), F.factors.dl, false))
        for i = 2:n
            tmp = L[F.ipiv[i], 1:i - 1]
            L[F.ipiv[i], 1:i - 1] = L[i, 1:i - 1]
            L[i, 1:i - 1] = tmp
        end
        return L
    elseif d == :U
        U = Array(Bidiagonal(F.factors.d, F.factors.du, true))
        for i = 1:n - 2
            U[i,i + 2] = F.factors.du2[i]
        end
        return U
    elseif d == :p
        return ipiv2perm(F.ipiv, m)
    elseif d == :P
        return eye(T, m)[:,invperm(F[:p])]
    end
    throw(KeyError(d))
end

# See dgtts2.f
function A_ldiv_B!{T}(A::LU{T,Tridiagonal{T}}, B::AbstractVecOrMat)
    n = size(A,1)
    if n != size(B,1)
        throw(DimensionMismatch("matrix has dimensions ($n,$n) but right hand side has $(size(B,1)) rows"))
    end
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
    if n != size(B,1)
        throw(DimensionMismatch("matrix has dimensions ($n,$n) but right hand side has $(size(B,1)) rows"))
    end
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
    if n != size(B,1)
        throw(DimensionMismatch("matrix has dimensions ($n,$n) but right hand side has $(size(B,1)) rows"))
    end
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

# Conversions
convert(::Type{AbstractMatrix}, F::LU) = (F[:L] * F[:U])[invperm(F[:p]),:]
convert(::Type{AbstractArray}, F::LU) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::LU) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::LU) = convert(Matrix, F)
full(F::LU) = convert(AbstractArray, F)

function convert{T}(::Type{Tridiagonal}, F::Base.LinAlg.LU{T,Tridiagonal{T}})
    n = size(F, 1)

    dl     = copy(F.factors.dl)
    d      = copy(F.factors.d)
    du     = copy(F.factors.du)
    du2    = copy(F.factors.du2)

    for i = n - 1:-1:1
        li         = dl[i]
        dl[i]      = li*d[i]
        d[i + 1]  += li*du[i]
        if i < n - 1
            du[i + 1] += li*du2[i]
        end

        if F.ipiv[i] != i
            tmp   = dl[i]
            dl[i] = d[i]
            d[i]  = tmp

            tmp      = d[i + 1]
            d[i + 1] = du[i]
            du[i]    = tmp

            if i < n - 1
                tmp       = du[i + 1]
                du[i + 1] = du2[i]
                du2[i]    = tmp
            end
        end
    end
    return Tridiagonal(dl, d, du)
end
convert{T}(::Type{AbstractMatrix}, F::Base.LinAlg.LU{T,Tridiagonal{T}}) = convert(Tridiagonal, F)
convert{T}(::Type{AbstractArray}, F::Base.LinAlg.LU{T,Tridiagonal{T}}) = convert(AbstractMatrix, F)
convert{T}(::Type{Matrix}, F::Base.LinAlg.LU{T,Tridiagonal{T}}) = convert(Array, convert(AbstractArray, F))
convert{T}(::Type{Array}, F::Base.LinAlg.LU{T,Tridiagonal{T}}) = convert(Matrix, F)
full{T}(F::Base.LinAlg.LU{T,Tridiagonal{T}}) = convert(AbstractArray, F)
