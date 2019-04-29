# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Hessenberg{T,TH,S<:AbstractMatrix{TH},V<:Number} <: Factorization{T}
    factors::S
    τ::Vector{TH}
    μ::V # represents a shifted Hessenberg matrix H+μI

    function Hessenberg{T,TH,S,V}(factors, τ, μ::V=zero(V)) where {T,TH,S<:AbstractMatrix{TH},V<:Number}
        require_one_based_indexing(factors, τ)
        new{T,TH,S,V}(factors, τ, μ)
    end
end
Hessenberg(factors::AbstractMatrix{T}, τ::Vector{T}, μ::V=false) where {T,V<:Number} = Hessenberg{promote_type(T,V),T,typeof(factors),V}(factors, τ, μ)
Hessenberg(F::Hessenberg, μ::Number=F.μ) = Hessenberg(F.factors, F.τ, μ)

Hessenberg(A::StridedMatrix, μ::Number=false) = Hessenberg(LAPACK.gehrd!(A)..., μ)

copy(F::Hessenberg) = Hessenberg(copy(F.factors), copy(F.τ), F.μ)
size(F::Hessenberg, d) = size(F.factors, d)
size(F::Hessenberg) = size(F.factors)

adjoint(F::Hessenberg) = Adjoint(F)

# iteration for destructuring into components
Base.iterate(S::Hessenberg) = (S.Q, Val(:H))
Base.iterate(S::Hessenberg, ::Val{:H}) = (S.H, Val(:done))
Base.iterate(S::Hessenberg, ::Val{:done}) = nothing

"""
    hessenberg!(A) -> Hessenberg

`hessenberg!` is the same as [`hessenberg`](@ref), but saves space by overwriting
the input `A`, instead of creating a copy.
"""
hessenberg!(A::StridedMatrix{<:BlasFloat}) = Hessenberg(A)

hessenberg(A::StridedMatrix{<:BlasFloat}) = hessenberg!(copy(A))

"""
    hessenberg(A) -> Hessenberg

Compute the Hessenberg decomposition of `A` and return a `Hessenberg` object. If `F` is the
factorization object, the unitary matrix can be accessed with `F.Q` and the Hessenberg
matrix with `F.H`. When `Q` is extracted, the resulting type is the `HessenbergQ` object,
and may be converted to a regular matrix with [`convert(Array, _)`](@ref)
 (or `Array(_)` for short).

Note that the shifted factorization `A+μI = Q (H+μI) Q'` can be
constructed efficiently by `F + μ*I` using the [`UniformScaling`](@ref)
object [`I`](@ref), which creates a new `Hessenberg` with the same `Q` and `H`
(shared storage) and a modified shift.   The shift of a given `F` is
obtained by `F.μ`.  This is useful because multiple shifted solves `(F + μ*I) \\ b`
(for different `μ` and/or `b`) can be performed efficiently once `F` is created.

Iterating the decomposition produces the factors `F.Q` and `F.H` (not including
the shift `F.μ`).

# Examples
```jldoctest
julia> A = [4. 9. 7.; 4. 4. 1.; 4. 3. 2.]
3×3 Array{Float64,2}:
 4.0  9.0  7.0
 4.0  4.0  1.0
 4.0  3.0  2.0

julia> F = hessenberg(A);

julia> F.Q * F.H * F.Q'
3×3 Array{Float64,2}:
 4.0  9.0  7.0
 4.0  4.0  1.0
 4.0  3.0  2.0

julia> q, h = F; # destructuring via iteration

julia> q == F.Q && h == F.H
true
```
"""
hessenberg(A::StridedMatrix{T}) where T =
    hessenberg!(copy_oftype(A, eigtype(T)))


function show(io::IO, mime::MIME"text/plain", F::Hessenberg)
    summary(io, F); println(io)
    if iszero(F.μ)
        println(io, "Factorization QHQ': ")
    else
        println(io, "Factorization Q(H+μI)Q' for shift μ = ", F.μ, ':')
    end
    println(io, "Q factor:")
    show(io, mime, F.Q)
    println(io, "\nH factor:")
    show(io, mime, F.H)
end

struct HessenbergQ{T,S<:AbstractMatrix} <: AbstractQ{T}
    factors::S
    τ::Vector{T}
    function HessenbergQ{T,S}(factors, τ) where {T,S<:AbstractMatrix}
        require_one_based_indexing(factors)
        new(factors, τ)
    end
end
HessenbergQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = HessenbergQ{T,typeof(factors)}(factors, τ)
HessenbergQ(A::Hessenberg) = HessenbergQ(A.factors, A.τ)

function getproperty(F::Hessenberg, d::Symbol)
    d == :Q && return HessenbergQ(F)
    d == :H && return triu(getfield(F, :factors), -1)
    return getfield(F, d)
end

Base.propertynames(F::Hessenberg, private::Bool=false) =
    (:Q, :H, :μ, (private ? (:factors, :τ) : ())...)

## reconstruct the original matrix
Matrix{T}(Q::HessenbergQ) where {T} = convert(Matrix{T}, LAPACK.orghr!(1, size(Q.factors, 1), copy(Q.factors), Q.τ))
AbstractArray(F::Hessenberg) = AbstractMatrix(F)
Matrix(F::Hessenberg) = Array(AbstractArray(F))
Array(F::Hessenberg) = Matrix(F)
function AbstractMatrix(F::Hessenberg)
    Q = F.Q
    A = rmul!(lmul!(Q, F.H), Q')
    if iszero(F.μ)
        return A
    elseif typeof(zero(eltype(A))+F.μ) <: eltype(A) # can shift A in-place
        for i = 1:size(A,1)
            @inbounds A[i,i] += F.μ
        end
        return A
    else
        return A + F.μ*I # allocate another matrix, e.g. if A is real and μ is complex
    end
end

lmul!(Q::HessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('L', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
rmul!(X::StridedMatrix{T}, Q::HessenbergQ{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('R', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
lmul!(adjQ::Adjoint{<:Any,<:HessenbergQ{T}}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    (Q = adjQ.parent; LAPACK.ormhr!('L', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))
rmul!(X::StridedMatrix{T}, adjQ::Adjoint{<:Any,<:HessenbergQ{T}}) where {T<:BlasFloat} =
    (Q = adjQ.parent; LAPACK.ormhr!('R', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))

lmul!(Q::HessenbergQ{T}, X::Adjoint{T,<:StridedVecOrMat{T}}) where {T<:BlasFloat} = rmul!(X', Q')'
rmul!(X::Adjoint{T,<:StridedMatrix{T}}, Q::HessenbergQ{T}) where {T<:BlasFloat} = lmul!(Q', X')'
lmul!(adjQ::Adjoint{<:Any,<:HessenbergQ{T}}, X::Adjoint{T,<:StridedVecOrMat{T}}) where {T<:BlasFloat}  = rmul!(X', adjQ')'
rmul!(X::Adjoint{T,<:StridedMatrix{T}}, adjQ::Adjoint{<:Any,<:HessenbergQ{T}}) where {T<:BlasFloat} = lmul!(adjQ', X')'

# multiply x by the entries of M in the upper-k triangle, which contains
# the entries of the upper-Hessenberg matrix H for k=-1
function rmul_triu!(M::AbstractMatrix, x, k::Integer=0)
    require_one_based_indexing(M)
    m, n = size(M)
    for j = 1:n, i = 1:min(j-k,m)
        @inbounds M[i,j] *= x
    end
    return M
end
function lmul_triu!(x, M::AbstractMatrix, k::Integer=0)
    require_one_based_indexing(M)
    m, n = size(M)
    for j = 1:n, i = 1:min(j-k,m)
        @inbounds M[i,j] = x * M[i,j]
    end
    return M
end

# multiply Hessenberg by scalar
rmul!(F::Hessenberg{<:Any,T}, x::T) where {T<:Number} = Hessenberg(rmul_triu!(F.factors, x, -1), F.τ, F.μ*x)
lmul!(x::T, F::Hessenberg{<:Any,T}) where {T<:Number} = Hessenberg(lmul_triu!(x, F.factors, -1), F.τ, x*F.μ)
*(F::Hessenberg{<:Any,T}, x::T) where {T<:Number} = rmul!(copy(F), x)
*(x::T, F::Hessenberg{<:Any,T}) where {T<:Number} = lmul!(x, copy(F))
function (*)(F::Hessenberg{<:Any,T}, x::S) where {T,S<:Number}
    TS = typeof(zero(T) * x)
    if TS === T
        return rmul!(copy(F), convert(T, x))
    else
        return rmul!(Hessenberg(Matrix{TS}(F.factors), Vector{TS}(F.τ), F.μ), convert(TS, x))
    end
end
function (*)(x::S, F::Hessenberg{<:Any,T}) where {T,S<:Number}
    TS = typeof(zero(T) * x)
    if TS === T
        return lmul!(convert(T, x), copy(F))
    else
        return lmul!(convert(TS, x), Hessenberg(Matrix{TS}(F.factors), Vector{TS}(F.τ), F.μ))
    end
end
-(F::Hessenberg{<:Any,T}) where {T} = F * -one(T)

# shift Hessenberg by λI
+(F::Hessenberg, J::UniformScaling) = Hessenberg(F, F.μ + J.λ)
+(J::UniformScaling, F::Hessenberg) = Hessenberg(F, J.λ + F.μ)
-(F::Hessenberg, J::UniformScaling) = Hessenberg(F, F.μ - J.λ)
-(J::UniformScaling, F::Hessenberg) = Hessenberg(-F, J.λ - F.μ)

# Solving (H+µI)x = b: we can do this in O(m²) time and O(m) memory
# (in-place in x) by the RQ algorithm from:
#
#    G. Henry, "The shifted Hessenberg system solve computation," Tech. Rep. 94–163,
#    Center for Appl. Math., Cornell University (1994).
#
# as reviewed in
#
#    C. Beattie et al., "A note on shifted Hessenberg systems and frequency
#    response computation," ACM Trans. Math. Soft. 38, pp. 12:6–12:16 (2011)
#
# (Note, however, that there is apparently a typo in Algorithm 1 of the
#  Beattie paper: the Givens rotation uses u(k), not H(k,k) - σ.)
#
# Essentially, it works by doing a Givens RQ factorization of H+µI from
# right to left, and doing backsubstitution *simultaneously*.

# solve (H+μI)X = B, storing result in B
function ldiv_H!(F::Hessenberg, B::AbstractVecOrMat)
    m = size(F,1)
    m != size(B,1) && throw(DimensionMismatch("wrong right-hand-side # rows != $m"))
    require_one_based_indexing(B)
    n = size(B,2)
    H = F.factors
    μ = F.μ
    u = Vector{typeof(zero(eltype(H))+μ)}(undef, m) # for last rotated col of H-μI
    copyto!(u, 1, H, m*(m-1)+1, m) # u .= H[:,m]
    u[m] += μ
    X = B # not a copy, just rename to match paper
    cs = Vector{Tuple{real(eltype(u)),eltype(u)}}(undef, length(u)) # store Givens rotations
    @inbounds for k = m:-1:2
        c, s, ρ = givensAlgorithm(u[k], H[k,k-1])
        cs[k] = (c, s)
        for i = 1:n
            X[k,i] /= ρ
            t₁ = s * X[k,i]; t₂ = c * X[k,i]
            @simd for j = 1:k-2
                X[j,i] -= u[j]*t₂ + H[j,k-1]*t₁
            end
            X[k-1,i] -= u[k-1]*t₂ + (H[k-1,k-1] + μ) * t₁
        end
        @simd for j = 1:k-2
            u[j] = H[j,k-1]*c - u[j]*s'
        end
        u[k-1] = (H[k-1,k-1] + μ) * c - u[k-1]*s'
    end
    for i = 1:n
        τ₁ = X[1,i] / u[1]
        @inbounds for j = 2:m
            τ₂ = X[j,i]
            c, s = cs[j]
            X[j-1,i] = c*τ₁ + s*τ₂
            τ₁ = c*τ₂ - s'τ₁
        end
        X[m,i] = τ₁
    end
    return X
end

# solve X(H+μI) = B, storing result in B
#
# Note: this can be derived from the Henry (1994) algorithm
# by transformation to F(Hᵀ+µI)F FXᵀ = FBᵀ, where
# F is the permutation matrix that reverses the order
# of rows/cols.  Essentially, we take the ldiv_H! algorithm,
# swap indices of H and X to transpose, and reverse the
# order of the H indices (or the order of the loops).
function rdiv_H!(B::AbstractMatrix, F::Hessenberg)
    m = size(F,1)
    m != size(B,2) && throw(DimensionMismatch("wrong right-hand-side # cols != $m"))
    require_one_based_indexing(B)
    n = size(B,1)
    H = F.factors
    μ = F.μ
    u = Vector{typeof(zero(eltype(H))+μ)}(undef, m) # for last rotated row of H-μI
    u .= @view H[1,:]
    u[1] += μ
    X = B # not a copy, just rename to match paper
    cs = Vector{Tuple{real(eltype(u)),eltype(u)}}(undef, length(u)) # store Givens rotations
    @inbounds for k = 1:m-1
        c, s, ρ = givensAlgorithm(u[k], H[k+1,k])
        cs[k] = (c, s)
        for i = 1:n
            X[i,k] /= ρ
            t₁ = s * X[i,k]; t₂ = c * X[i,k]
            @simd for j = k+2:m
                X[i,j] -= u[j]*t₂ + H[k+1,j]*t₁
            end
            X[i,k+1] -= u[k+1]*t₂ + (H[k+1,k+1] + μ) * t₁
        end
        @simd for j = k+2:m
            u[j] = H[k+1,j]*c - u[j]*s'
        end
        u[k+1] = (H[k+1,k+1] + μ) * c - u[k+1]*s'
    end
    for i = 1:n
        τ₁ = X[i,m] / u[m]
        @inbounds for j = m-1:-1:1
            τ₂ = X[i,j]
            c, s = cs[j]
            X[i,j+1] = c*τ₁ + s*τ₂
            τ₁ = c*τ₂ - s'τ₁
        end
        X[i,1] = τ₁
    end
    return X
end

function ldiv!(F::Hessenberg, B::AbstractVecOrMat)
    Q = F.Q
    return lmul!(Q, ldiv_H!(F, lmul!(Q', B)))
end

function rdiv!(B::AbstractMatrix, F::Hessenberg)
    Q = F.Q
    return rmul!(rdiv_H!(rmul!(B, Q), F), Q')
end

# handle case of real H and complex μ — we need to work around the
# fact that we can't multiple a real F.Q by a complex matrix directly in LAPACK
function ldiv!(F::Hessenberg{<:Complex,<:Real}, B::AbstractVecOrMat{<:Complex})
    Q = F.Q
    Br = lmul!(Q', real(B))
    Bi = lmul!(Q', imag(B))
    ldiv_H!(F, B .= Complex.(Br,Bi))
    Br = lmul!(Q, real(B))
    Bi = lmul!(Q, imag(B))
    return B .= Complex.(Br,Bi)
end
function rdiv!(B::AbstractVecOrMat{<:Complex}, F::Hessenberg{<:Complex,<:Real})
    Q = F.Q
    Br = rmul!(real(B), Q)
    Bi = rmul!(imag(B), Q)
    rdiv_H!(B .= Complex.(Br,Bi), F)
    Br = rmul!(real(B), Q')
    Bi = rmul!(imag(B), Q')
    return B .= Complex.(Br,Bi)
end

ldiv!(F::Adjoint{<:Any,<:Hessenberg}, B::AbstractVecOrMat) = rdiv!(B', F')'
rdiv!(B::AbstractMatrix, F::Adjoint{<:Any,<:Hessenberg}) = ldiv!(F', B')'

# Hessenberg-matrix determinant formula for H+μI based on:
#
#    N. D. Cahill, J. R. D’Errico, D. A. Narayan, and J. Y. Narayan, "Fibonacci determinants,"
#    College Math. J. 33, pp. 221-225 (2003).
#
# as reviewed in Theorem 2.1 of:
#
#    K. Kaygisiz and A. Sahin, "Determinant and permanent of Hessenberg matrix and generalized Lucas polynomials,"
#    arXiv:1111.4067 (2011).
#
# Cost is O(m²) with O(m) storage.
function det(F::Hessenberg)
    H = F.factors
    m = size(H,1)
    μ = F.μ
    m == 0 && return one(zero(eltype(H)) + μ)
    determinant = H[1,1] + μ
    prevdeterminant = one(determinant)
    m == 1 && return determinant
    prods = Vector{typeof(determinant)}(undef, m-1) # temporary storage for partial products
    @inbounds for n = 2:m
        prods[n-1] = prevdeterminant
        prevdeterminant = determinant
        determinant *= H[n,n] + μ
        h = H[n,n-1]
        @simd for r = n-1:-2:2
            determinant -= H[r,n] * (prods[r] *= h) - H[r-1,n] * (prods[r-1] *= h)
        end
        if iseven(n)
            determinant -= H[1,n] * (prods[1] *= h)
        end
    end
    return determinant
end

# O(m²) log-determinant based on first doing Givens RQ to put H+μI into upper-triangular form and then
# taking the product of the diagonal entries.   The trick is that we only need O(m) temporary storage,
# because we don't need to store the whole Givens-rotated matrix, only the most recent column.
# We do RQ (column rotations) rather than QR (row rotations) for more consecutive memory access.
# (We could also use it for det instead of the Cahill algorithm above.  Cahill is slightly faster
#  for very small matrices where you are likely to use det, and also uses only ± and * so it can
#  be applied to Hessenberg matrices over other number fields.)
function logabsdet(F::Hessenberg)
    H = F.factors
    m = size(H,1)
    μ = F.μ
    P = one(zero(eltype(H)) + μ)
    logdeterminant = zero(real(P))
    m == 0 && return (logdeterminant, P)
    g = Vector{typeof(P)}(undef, m) # below, g is the k-th col of Givens-rotated H+μI matrix
    copyto!(g, 1, H, m*(m-1)+1, m) # g .= H[:,m]
    g[m] += μ
    @inbounds for k = m:-1:2
        c, s, ρ = givensAlgorithm(g[k], H[k,k-1])
        logdeterminant += log(abs(ρ))
        P *= sign(ρ)
        g[k-1] = c*(H[k-1,k-1] + μ) - s'*g[k-1]
        @simd for j = 1:k-2
            g[j] = c*H[j,k-1] - s'*g[j]
        end
    end
    logdeterminant += log(abs(g[1]))
    P *= sign(g[1])
    return (logdeterminant, P)
end
function logdet(F::Hessenberg)
    d,s = logabsdet(F)
    return d + log(s)
end