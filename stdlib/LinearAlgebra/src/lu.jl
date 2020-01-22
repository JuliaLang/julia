# This file is a part of Julia. License is MIT: https://julialang.org/license

####################
# LU Factorization #
####################
"""
    LU <: Factorization

Matrix factorization type of the `LU` factorization of a square matrix `A`. This
is the return type of [`lu`](@ref), the corresponding matrix factorization function.

The individual components of the factorization `F::LU` can be accessed via [`getproperty`](@ref):

| Component | Description                              |
|:----------|:-----------------------------------------|
| `F.L`     | `L` (unit lower triangular) part of `LU` |
| `F.U`     | `U` (upper triangular) part of `LU`      |
| `F.p`     | (right) permutation `Vector`             |
| `F.P`     | (right) permutation `Matrix`             |

Iterating the factorization produces the components `F.L`, `F.U`, and `F.p`.

# Examples
```jldoctest
julia> A = [4 3; 6 3]
2×2 Array{Int64,2}:
 4  3
 6  3

julia> F = lu(A)
LU{Float64,Array{Float64,2}}
L factor:
2×2 Array{Float64,2}:
 1.0       0.0
 0.666667  1.0
U factor:
2×2 Array{Float64,2}:
 6.0  3.0
 0.0  1.0

julia> F.L * F.U == A[F.p, :]
true

julia> l, u, p = lu(A); # destructuring via iteration

julia> l == F.L && u == F.U && p == F.p
true
```
"""
struct LU{T,S<:AbstractMatrix{T}} <: Factorization{T}
    factors::S
    ipiv::Vector{BlasInt}
    info::BlasInt

    function LU{T,S}(factors, ipiv, info) where {T,S<:AbstractMatrix{T}}
        require_one_based_indexing(factors)
        new{T,S}(factors, ipiv, info)
    end
end
function LU(factors::AbstractMatrix{T}, ipiv::Vector{BlasInt}, info::BlasInt) where {T}
    LU{T,typeof(factors)}(factors, ipiv, info)
end
function LU{T}(factors::AbstractMatrix, ipiv::AbstractVector{<:Integer}, info::Integer) where {T}
    LU(convert(AbstractMatrix{T}, factors),
       convert(Vector{BlasInt}, ipiv),
       BlasInt(info))
end

# iteration for destructuring into components
Base.iterate(S::LU) = (S.L, Val(:U))
Base.iterate(S::LU, ::Val{:U}) = (S.U, Val(:p))
Base.iterate(S::LU, ::Val{:p}) = (S.p, Val(:done))
Base.iterate(S::LU, ::Val{:done}) = nothing

adjoint(F::LU) = Adjoint(F)
transpose(F::LU) = Transpose(F)

# StridedMatrix
function lu!(A::StridedMatrix{T}, pivot::Union{Val{false}, Val{true}} = Val(true);
             check::Bool = true) where T<:BlasFloat
    if pivot === Val(false)
        return generic_lufact!(A, pivot; check = check)
    end
    lpt = LAPACK.getrf!(A)
    check && checknonsingular(lpt[3])
    return LU{T,typeof(A)}(lpt[1], lpt[2], lpt[3])
end
function lu!(A::HermOrSym, pivot::Union{Val{false}, Val{true}} = Val(true); check::Bool = true)
    copytri!(A.data, A.uplo, isa(A, Hermitian))
    lu!(A.data, pivot; check = check)
end

"""
    lu!(A, pivot=Val(true); check = true) -> LU

`lu!` is the same as [`lu`](@ref), but saves space by overwriting the
input `A`, instead of creating a copy. An [`InexactError`](@ref)
exception is thrown if the factorization produces a number not representable by the
element type of `A`, e.g. for integer types.

# Examples
```jldoctest
julia> A = [4. 3.; 6. 3.]
2×2 Array{Float64,2}:
 4.0  3.0
 6.0  3.0

julia> F = lu!(A)
LU{Float64,Array{Float64,2}}
L factor:
2×2 Array{Float64,2}:
 1.0       0.0
 0.666667  1.0
U factor:
2×2 Array{Float64,2}:
 6.0  3.0
 0.0  1.0

julia> iA = [4 3; 6 3]
2×2 Array{Int64,2}:
 4  3
 6  3

julia> lu!(iA)
ERROR: InexactError: Int64(0.6666666666666666)
Stacktrace:
[...]
```
"""
lu!(A::StridedMatrix, pivot::Union{Val{false}, Val{true}} = Val(true); check::Bool = true) =
    generic_lufact!(A, pivot; check = check)
function generic_lufact!(A::StridedMatrix{T}, ::Val{Pivot} = Val(true);
                         check::Bool = true) where {T,Pivot}
    m, n = size(A)
    minmn = min(m,n)
    info = 0
    ipiv = Vector{BlasInt}(undef, minmn)
    @inbounds begin
        for k = 1:minmn
            # find index max
            kp = k
            if Pivot
                amax = abs(zero(T))
                for i = k:m
                    absi = abs(A[i,k])
                    if absi > amax
                        kp = i
                        amax = absi
                    end
                end
            end
            ipiv[k] = kp
            if !iszero(A[kp,k])
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
    check && checknonsingular(info, Val{Pivot}())
    return LU{T,typeof(A)}(A, ipiv, convert(BlasInt, info))
end

function lutype(T::Type)
    # In generic_lufact!, the elements of the lower part of the matrix are
    # obtained using the division of two matrix elements. Hence their type can
    # be different (e.g. the division of two types with the same unit is a type
    # without unit).
    # The elements of the upper part are obtained by U - U * L
    # where U is an upper part element and L is a lower part element.
    # Therefore, the types LT, UT should be invariant under the map:
    # (LT, UT) -> begin
    #     L = oneunit(UT) / oneunit(UT)
    #     U = oneunit(UT) - oneunit(UT) * L
    #     typeof(L), typeof(U)
    # end
    # The following should handle most cases
    UT = typeof(oneunit(T) - oneunit(T) * (oneunit(T) / (oneunit(T) + zero(T))))
    LT = typeof(oneunit(UT) / oneunit(UT))
    S = promote_type(T, LT, UT)
end

# for all other types we must promote to a type which is stable under division
"""
    lu(A, pivot=Val(true); check = true) -> F::LU

Compute the LU factorization of `A`.

When `check = true`, an error is thrown if the decomposition fails.
When `check = false`, responsibility for checking the decomposition's
validity (via [`issuccess`](@ref)) lies with the user.

In most cases, if `A` is a subtype `S` of `AbstractMatrix{T}` with an element
type `T` supporting `+`, `-`, `*` and `/`, the return type is `LU{T,S{T}}`. If
pivoting is chosen (default) the element type should also support [`abs`](@ref) and
[`<`](@ref).

The individual components of the factorization `F` can be accessed via [`getproperty`](@ref):

| Component | Description                         |
|:----------|:------------------------------------|
| `F.L`     | `L` (lower triangular) part of `LU` |
| `F.U`     | `U` (upper triangular) part of `LU` |
| `F.p`     | (right) permutation `Vector`        |
| `F.P`     | (right) permutation `Matrix`        |

Iterating the factorization produces the components `F.L`, `F.U`, and `F.p`.

The relationship between `F` and `A` is

`F.L*F.U == A[F.p, :]`

`F` further supports the following functions:

| Supported function               | `LU` | `LU{T,Tridiagonal{T}}` |
|:---------------------------------|:-----|:-----------------------|
| [`/`](@ref)                      | ✓    |                        |
| [`\\`](@ref)                     | ✓    | ✓                      |
| [`inv`](@ref)                    | ✓    | ✓                      |
| [`det`](@ref)                    | ✓    | ✓                      |
| [`logdet`](@ref)                 | ✓    | ✓                      |
| [`logabsdet`](@ref)              | ✓    | ✓                      |
| [`size`](@ref)                   | ✓    | ✓                      |

# Examples
```jldoctest
julia> A = [4 3; 6 3]
2×2 Array{Int64,2}:
 4  3
 6  3

julia> F = lu(A)
LU{Float64,Array{Float64,2}}
L factor:
2×2 Array{Float64,2}:
 1.0       0.0
 0.666667  1.0
U factor:
2×2 Array{Float64,2}:
 6.0  3.0
 0.0  1.0

julia> F.L * F.U == A[F.p, :]
true

julia> l, u, p = lu(A); # destructuring via iteration

julia> l == F.L && u == F.U && p == F.p
true
```
"""
function lu(A::AbstractMatrix{T}, pivot::Union{Val{false}, Val{true}}=Val(true);
            check::Bool = true) where T
    S = lutype(T)
    lu!(copy_oftype(A, S), pivot; check = check)
end

lu(S::LU) = S
function lu(x::Number; check::Bool=true)
    info = x == 0 ? one(BlasInt) : zero(BlasInt)
    check && checknonsingular(info)
    return LU(fill(x, 1, 1), BlasInt[1], info)
end

function LU{T}(F::LU) where T
    M = convert(AbstractMatrix{T}, F.factors)
    LU{T,typeof(M)}(M, F.ipiv, F.info)
end
LU{T,S}(F::LU) where {T,S} = LU{T,S}(convert(S, F.factors), F.ipiv, F.info)
Factorization{T}(F::LU{T}) where {T} = F
Factorization{T}(F::LU) where {T} = LU{T}(F)

copy(A::LU{T,S}) where {T,S} = LU{T,S}(copy(A.factors), copy(A.ipiv), A.info)

size(A::LU)    = size(getfield(A, :factors))
size(A::LU, i) = size(getfield(A, :factors), i)

function ipiv2perm(v::AbstractVector{T}, maxi::Integer) where T
    require_one_based_indexing(v)
    p = T[1:maxi;]
    @inbounds for i in 1:length(v)
        p[i], p[v[i]] = p[v[i]], p[i]
    end
    return p
end

function getproperty(F::LU{T,<:StridedMatrix}, d::Symbol) where T
    m, n = size(F)
    if d === :L
        L = tril!(getfield(F, :factors)[1:m, 1:min(m,n)])
        for i = 1:min(m,n); L[i,i] = one(T); end
        return L
    elseif d === :U
        return triu!(getfield(F, :factors)[1:min(m,n), 1:n])
    elseif d === :p
        return ipiv2perm(getfield(F, :ipiv), m)
    elseif d === :P
        return Matrix{T}(I, m, m)[:,invperm(F.p)]
    else
        getfield(F, d)
    end
end

Base.propertynames(F::LU, private::Bool=false) =
    (:L, :U, :p, :P, (private ? fieldnames(typeof(F)) : ())...)

issuccess(F::LU) = F.info == 0

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::LU)
    if issuccess(F)
        summary(io, F); println(io)
        println(io, "L factor:")
        show(io, mime, F.L)
        println(io, "\nU factor:")
        show(io, mime, F.U)
    else
        print(io, "Failed factorization of type $(typeof(F))")
    end
end

_apply_ipiv_rows!(A::LU, B::StridedVecOrMat) = _ipiv_rows!(A, 1 : length(A.ipiv), B)
_apply_inverse_ipiv_rows!(A::LU, B::StridedVecOrMat) = _ipiv_rows!(A, length(A.ipiv) : -1 : 1, B)

function _ipiv_rows!(A::LU, order::OrdinalRange, B::StridedVecOrMat)
    for i = order
        if i != A.ipiv[i]
            _swap_rows!(B, i, A.ipiv[i])
        end
    end
    B
end

function _swap_rows!(B::StridedVector, i::Integer, j::Integer)
    B[i], B[j] = B[j], B[i]
    B
end

function _swap_rows!(B::StridedMatrix, i::Integer, j::Integer)
    for col = 1 : size(B, 2)
        B[i,col], B[j,col] = B[j,col], B[i,col]
    end
    B
end

_apply_ipiv_cols!(A::LU, B::StridedVecOrMat) = _ipiv_cols!(A, 1 : length(A.ipiv), B)
_apply_inverse_ipiv_cols!(A::LU, B::StridedVecOrMat) = _ipiv_cols!(A, length(A.ipiv) : -1 : 1, B)

function _ipiv_cols!(A::LU, order::OrdinalRange, B::StridedVecOrMat)
    for i = order
        if i != A.ipiv[i]
            _swap_cols!(B, i, A.ipiv[i])
        end
    end
    B
end

function _swap_cols!(B::StridedVector, i::Integer, j::Integer)
    _swap_rows!(B, i, j)
end

function _swap_cols!(B::StridedMatrix, i::Integer, j::Integer)
    for row = 1 : size(B, 1)
        B[row,i], B[row,j] = B[row,j], B[row,i]
    end
    B
end

function rdiv!(A::StridedVecOrMat, B::LU{<:Any,<:StridedMatrix})
    rdiv!(rdiv!(A, UpperTriangular(B.factors)), UnitLowerTriangular(B.factors))
    _apply_inverse_ipiv_cols!(B, A)
end

ldiv!(A::LU{T,<:StridedMatrix}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.getrs!('N', A.factors, A.ipiv, B)

function ldiv!(A::LU{<:Any,<:StridedMatrix}, B::StridedVecOrMat)
    _apply_ipiv_rows!(A, B)
    ldiv!(UpperTriangular(A.factors), ldiv!(UnitLowerTriangular(A.factors), B))
end

ldiv!(transA::Transpose{T,<:LU{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
    (A = transA.parent; LAPACK.getrs!('T', A.factors, A.ipiv, B))

function ldiv!(transA::Transpose{<:Any,<:LU{<:Any,<:StridedMatrix}}, B::StridedVecOrMat)
    A = transA.parent
    ldiv!(transpose(UnitLowerTriangular(A.factors)), ldiv!(transpose(UpperTriangular(A.factors)), B))
    _apply_inverse_ipiv_rows!(A, B)
end

ldiv!(adjF::Adjoint{T,<:LU{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:Real} =
    (F = adjF.parent; ldiv!(transpose(F), B))
ldiv!(adjA::Adjoint{T,<:LU{T,<:StridedMatrix}}, B::StridedVecOrMat{T}) where {T<:BlasComplex} =
    (A = adjA.parent; LAPACK.getrs!('C', A.factors, A.ipiv, B))

function ldiv!(adjA::Adjoint{<:Any,<:LU{<:Any,<:StridedMatrix}}, B::StridedVecOrMat)
    A = adjA.parent
    ldiv!(adjoint(UnitLowerTriangular(A.factors)), ldiv!(adjoint(UpperTriangular(A.factors)), B))
    _apply_inverse_ipiv_rows!(A, B)
end

(\)(A::Adjoint{<:Any,<:LU}, B::Adjoint{<:Any,<:StridedVecOrMat}) = A \ copy(B)
(\)(A::Transpose{<:Any,<:LU}, B::Transpose{<:Any,<:StridedVecOrMat}) = A \ copy(B)
(\)(A::Adjoint{T,<:LU{T,<:StridedMatrix}}, B::Adjoint{T,<:StridedVecOrMat{T}}) where {T<:BlasComplex} =
    LAPACK.getrs!('C', A.parent.factors, A.parent.ipiv, copy(B))
(\)(A::Transpose{T,<:LU{T,<:StridedMatrix}}, B::Transpose{T,<:StridedVecOrMat{T}}) where {T<:BlasFloat} =
    LAPACK.getrs!('T', A.parent.factors, A.parent.ipiv, copy(B))

function (/)(A::AbstractMatrix, F::Adjoint{<:Any,<:LU})
    T = promote_type(eltype(A), eltype(F))
    return adjoint(ldiv!(F.parent, copy_oftype(adjoint(A), T)))
end
# To avoid ambiguities with definitions in adjtrans.jl and factorizations.jl
(/)(adjA::Adjoint{<:Any,<:AbstractVector}, F::Adjoint{<:Any,<:LU}) = adjoint(F.parent \ adjA.parent)
(/)(adjA::Adjoint{<:Any,<:AbstractMatrix}, F::Adjoint{<:Any,<:LU}) = adjoint(F.parent \ adjA.parent)
function (/)(trA::Transpose{<:Any,<:AbstractVector}, F::Adjoint{<:Any,<:LU})
    T = promote_type(eltype(trA), eltype(F))
    return adjoint(ldiv!(F.parent, convert(AbstractVector{T}, conj(trA.parent))))
end
function (/)(trA::Transpose{<:Any,<:AbstractMatrix}, F::Adjoint{<:Any,<:LU})
    T = promote_type(eltype(trA), eltype(F))
    return adjoint(ldiv!(F.parent, convert(AbstractMatrix{T}, conj(trA.parent))))
end

function det(F::LU{T}) where T
    n = checksquare(F)
    issuccess(F) || return zero(T)
    P = one(T)
    c = 0
    @inbounds for i = 1:n
        P *= F.factors[i,i]
        if F.ipiv[i] != i
            c += 1
        end
    end
    s = (isodd(c) ? -one(T) : one(T))
    return P * s
end

function logabsdet(F::LU{T}) where T  # return log(abs(det)) and sign(det)
    n = checksquare(F)
    issuccess(F) || return log(zero(real(T))), log(one(T))
    c = 0
    P = one(T)
    abs_det = zero(real(T))
    @inbounds for i = 1:n
        dg_ii = F.factors[i,i]
        P *= sign(dg_ii)
        if F.ipiv[i] != i
            c += 1
        end
        abs_det += log(abs(dg_ii))
    end
    s = ifelse(isodd(c), -one(real(T)), one(real(T))) * P
    abs_det, s
end

inv!(A::LU{<:BlasFloat,<:StridedMatrix}) =
    LAPACK.getri!(A.factors, A.ipiv)
inv!(A::LU{T,<:StridedMatrix}) where {T} =
    ldiv!(A.factors, copy(A), Matrix{T}(I, size(A, 1), size(A, 1)))
inv(A::LU{<:BlasFloat,<:StridedMatrix}) = inv!(copy(A))

# Tridiagonal

# See dgttrf.f
function lu!(A::Tridiagonal{T,V}, pivot::Union{Val{false}, Val{true}} = Val(true);
             check::Bool = true) where {T,V}
    n = size(A, 1)
    info = 0
    ipiv = Vector{BlasInt}(undef, n)
    dl = A.dl
    d = A.d
    du = A.du
    du2 = fill!(similar(d, n-2), 0)::V

    @inbounds begin
        for i = 1:n
            ipiv[i] = i
        end
        for i = 1:n-2
            # pivot or not?
            if pivot === Val(false) || abs(d[i]) >= abs(dl[i])
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
            if pivot === Val(false) || abs(d[i]) >= abs(dl[i])
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
    B = Tridiagonal{T,V}(dl, d, du, du2)
    check && checknonsingular(info, pivot)
    return LU{T,Tridiagonal{T,V}}(B, ipiv, convert(BlasInt, info))
end

factorize(A::Tridiagonal) = lu(A)

function getproperty(F::LU{T,Tridiagonal{T,V}}, d::Symbol) where {T,V}
    m, n = size(F)
    if d === :L
        dl = getfield(getfield(F, :factors), :dl)
        L = Array(Bidiagonal(fill!(similar(dl, n), one(T)), dl, d))
        for i = 2:n
            tmp = L[getfield(F, :ipiv)[i], 1:i - 1]
            L[getfield(F, :ipiv)[i], 1:i - 1] = L[i, 1:i - 1]
            L[i, 1:i - 1] = tmp
        end
        return L
    elseif d === :U
        U = Array(Bidiagonal(getfield(getfield(F, :factors), :d), getfield(getfield(F, :factors), :du), d))
        for i = 1:n - 2
            U[i,i + 2] = getfield(getfield(F, :factors), :du2)[i]
        end
        return U
    elseif d === :p
        return ipiv2perm(getfield(F, :ipiv), m)
    elseif d === :P
        return Matrix{T}(I, m, m)[:,invperm(F.p)]
    end
    return getfield(F, d)
end

# See dgtts2.f
function ldiv!(A::LU{T,Tridiagonal{T,V}}, B::AbstractVecOrMat) where {T,V}
    require_one_based_indexing(B)
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

function ldiv!(transA::Transpose{<:Any,<:LU{T,Tridiagonal{T,V}}}, B::AbstractVecOrMat) where {T,V}
    require_one_based_indexing(B)
    A = transA.parent
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

# Ac_ldiv_B!(A::LU{T,Tridiagonal{T}}, B::AbstractVecOrMat) where {T<:Real} = At_ldiv_B!(A,B)
function ldiv!(adjA::Adjoint{<:Any,LU{T,Tridiagonal{T,V}}}, B::AbstractVecOrMat) where {T,V}
    require_one_based_indexing(B)
    A = adjA.parent
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

rdiv!(B::AbstractMatrix, A::LU) = transpose(ldiv!(transpose(A), transpose(B)))
rdiv!(B::AbstractMatrix, A::Transpose{<:Any,<:LU}) = transpose(ldiv!(A.parent, transpose(B)))
rdiv!(B::AbstractMatrix, A::Adjoint{<:Any,<:LU}) = adjoint(ldiv!(A.parent, adjoint(B)))

# Conversions
AbstractMatrix(F::LU) = (F.L * F.U)[invperm(F.p),:]
AbstractArray(F::LU) = AbstractMatrix(F)
Matrix(F::LU) = Array(AbstractArray(F))
Array(F::LU) = Matrix(F)

function Tridiagonal(F::LU{T,Tridiagonal{T,V}}) where {T,V}
    n = size(F, 1)

    dl  = copy(F.factors.dl)
    d   = copy(F.factors.d)
    du  = copy(F.factors.du)
    du2 = copy(F.factors.du2)

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
AbstractMatrix(F::LU{T,Tridiagonal{T,V}}) where {T,V} = Tridiagonal(F)
AbstractArray(F::LU{T,Tridiagonal{T,V}}) where {T,V} = AbstractMatrix(F)
Matrix(F::LU{T,Tridiagonal{T,V}}) where {T,V} = Array(AbstractArray(F))
Array(F::LU{T,Tridiagonal{T,V}}) where {T,V} = Matrix(F)
