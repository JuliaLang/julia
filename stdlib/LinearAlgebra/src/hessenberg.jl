# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Hessenberg{T,S<:AbstractMatrix{T},V<:Number} <: Factorization{T}
    factors::S
    τ::Vector{T}
    μ::V # represents a shifted Hessenberg matrix H-μI

    function Hessenberg{T,S,V}(factors, τ, μ::V=zero(V)) where {T,S<:AbstractMatrix{T},V<:Number}
        require_one_based_indexing(factors, τ)
        new{T,S}(factors, τ, μ)
    end
end
Hessenberg(factors::AbstractMatrix{T}, τ::Vector{T}, μ::V=false) where {T,V<:Number} = Hessenberg{T,typeof(factors),V}(factors, τ, μ)
function Hessenberg{T}(factors::AbstractMatrix, τ::AbstractVector, μ::V=false) where {T,V<:Number}
    Hessenberg(convert(AbstractMatrix{T}, factors), convert(Vector{T}, v), μ)
end
Hessenberg(F::Hessenberg, μ::Number=F.μ) = Hessenberg(F.factors, F.τ, μ)
Hessenberg{T}(F::Hessenberg{T}) where {T} = copy(F) # copying simplifies promotion logic below
Hessenberg{T}(F::Hessenberg) where {T} = Hessenberg{T}(F.factors, F.τ, F.μ)

Hessenberg(A::StridedMatrix, μ::Number=false) = Hessenberg(LAPACK.gehrd!(A)..., μ)

copy(F::Hessenberg) = Hessenberg(copy(F.factors), copy(F.τ), F.μ)
size(F::Hessenberg, d) = size(F.factors, d)
size(F::Hessenberg) = size(F.factors)

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

Note that the shifted factorization `A-μI = Q (H - μI) Q'` can be
constructed efficiently by `F - μ*I` using the [`UniformScaling`](@ref)
object [`I`](@ref), which creates a new `Hessenberg` with the same `Q` and `H`
(shared storage) and a modified shift.   The shift of a given `F` is
obtained by `F.μ`.

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
    (:Q, :H, (private ? fieldnames(typeof(F)) : ())...)

## reconstruct the original matrix
Matrix{T}(Q::HessenbergQ) where {T} = convert(Matrix{T}, LAPACK.orghr!(1, size(Q.factors, 1), copy(Q.factors), Q.τ))
AbstractArray(F::Hessenberg) = AbstractMatrix(F)
Matrix(F::Hessenberg) = Array(AbstractArray(F))
Array(F::Hessenberg) = Matrix(F)
function AbstractMatrix(F::Hessenberg{T,S,V}) where {T,S,V}
    fq = Array(F.Q)
    A = rmul!(lmul!(fq, F.H), fq')
    if iszero(F.μ)
        return A
    elseif promote_type(T,V) <: T # can shift A in-place
        for i = 1:size(A,1)
            @inbounds A[i,i] -= F.μ
        end
        return A
    else
        return A - F.μ*I # allocate another matrix, e.g. if A is real and μ is complex
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
rmul!(F::Hessenberg{T}, x::T) where {T} = Hessenberg(rmul_triu!(F.factors, x, -1), F.τ, F.μ*x)
lmul!(x::T, F::Hessenberg{T}) where {T} = Hessenberg(lmul_triu!(x, F.factors, -1), F.τ, x*F.μ)
(*)(F::Hessenberg{T}, x::T) where {T} = rmul!(copy(F), x)
(*)(x::T, F::Hessenberg{T}) where {T} = lmul!(x, copy(F))
function (*)(F::Hessenberg{T}, x::S) where {T,S}
    TS = typeof(zero(T) * x)
    rmul!(Hessenberg{TS}(F), convert(TS, x))
end
function (*)(x::S, F::Hessenberg{T}) where {T,S}
    TS = typeof(zero(T) * x)
    return lmul!(convert(TS, x), Hessenberg{TS}(F))
end
(-)(F::Hessenberg{T}) where {T} = F * -one(T)

# shift Hessenberg by λI
(+)(F::Hessenberg, J::UniformScaling) = Hessenberg(F, F.μ + J.λ)
(+)(J::UniformScaling, F::Hessenberg) = Hessenberg(F, J.λ + F.μ)
(-)(F::Hessenberg, J::UniformScaling) = Hessenberg(F, F.μ - J.λ)
(-)(J::UniformScaling, F::Hessenberg{T}) where {T} = Hessenberg(-F, J.λ - F.μ)

# Solving (H-µI)x = b: we can do this in O(n²) time and O(n) memory
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
# Essentially, it works by doing a Givens RQ factorization of H-µI from
# right to left, and doing backsubstitution *simultaneously*.

# solve (H-μ)X = B, storing result in B
function ldiv_H!(F::Hessenberg, B::AbstractVecOrMat)
    m = size(F,1)
    m != size(B,1) && throw(DimensionMismatch("wrong right-hand-side length != $m"))
    require_one_based_indexing(B)
    n = size(B,2)
    H = F.factors
    μ = F.μ
    u = Vector{typeof(zero(eltype(H))-μ)}(undef, m) # for last rotated col of H-μI
    copyto!(u, 1, H, m*(m-1)+1, m) # u .= H[:,m]
    u[m] -= μ
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
            X[k-1,i] -= u[k-1]*t₂ + (H[k-1,k-1] - μ) * t₁
        end
        @simd for j = 1:k-2
            u[j] = H[j,k-1]*c - u[j]*s'
        end
        u[k-1] = (H[k-1,k-1] - μ) * c - u[k-1]*s'
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

function ldiv!(F::Hessenberg, B::AbstractVecOrMat)
    Q = F.Q
    return lmul!(Q, ldiv_H!(F, lmul!(Q', B)))
end
