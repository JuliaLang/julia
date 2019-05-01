# This file is a part of Julia. License is MIT: https://julialang.org/license

######################################################################################
# Upper-Hessenberg matrices H+μI, analogous to the UpperTriangular type
# but including a shift μ.

"""
    UpperHessenberg(A::AbstractMatrix, μ=0)

Construct an `UpperHessenberg` view of the matrix `A+μI`, where `μ` is an
optional diagonal shift.  (Entries of `A` below the first subdiagonal are ignored.)

Given an `UpperHessenberg` matrix `H`, subsequent shifts `H+λ*I` are also
performed efficiently by constructing a new `UpperHessenberg` that shares
same underlying data array.  Efficient algorithms are also implemented for
`H \\ b`, `det(H)`, and similar, so shifted solves `(H+λ*I) \\ b` can be performed
for multiple shifts `λ` without making a copy of the matrix.

See also the [`hessenberg`](@ref) function to factor any matrix into a similar
upper-Hessenberg matrix.

# Examples
```jldoctest
julia> A = [1 2 3 4; 5 6 7 8; 9 10 11 12; 13 14 15 16]
4×4 Array{Int64,2}:
  1   2   3   4
  5   6   7   8
  9  10  11  12
 13  14  15  16

julia> UpperHessenberg(A, 100)
4×4 UpperHessenberg{Int64,Array{Int64,2},Int64}:
 101    2    3    4
   5  106    7    8
   ⋅   10  111   12
   ⋅    ⋅   15  116
```
"""
struct UpperHessenberg{T,S<:AbstractMatrix,V<:Number} <: AbstractMatrix{T}
    data::S
    μ::V # represents a shifted Hessenberg matrix H+μI

    function UpperHessenberg{T,S,V}(data, μ) where {T,S<:AbstractMatrix,V<:Number}
        require_one_based_indexing(data)
        new{T,S,V}(data, μ)
    end
end
UpperHessenberg(H::UpperHessenberg) = H
UpperHessenberg(H::UpperHessenberg, μ::Number) = UpperHessenberg(H.data, H.μ + μ)
UpperHessenberg{T}(A::S, μ::V=false) where {T,S<:AbstractMatrix,V<:Number} =
    UpperHessenberg{T,S,V}(A, μ)
UpperHessenberg{T}(H::UpperHessenberg, μ::Number=false) where {T} =
    UpperHessenberg{T}(H.data, H.μ+μ)
UpperHessenberg(A::AbstractMatrix{T}, μ::Number=false) where {T} =
    UpperHessenberg{typeof(zero(T) + μ)}(A, μ)
Matrix(H::UpperHessenberg{T}) where {T} = Matrix{T}(H)
Array(H::UpperHessenberg) = Matrix(H)
size(H::UpperHessenberg, d) = size(H.data, d)
size(H::UpperHessenberg) = size(H.data)
parent(H::UpperHessenberg) = H.data

# similar behaves like UpperTriangular
similar(H::UpperHessenberg, ::Type{T}) where {T} = UpperHessenberg(similar(H.data, T))
similar(H::UpperHessenberg, ::Type{T}, dims::Dims{N}) where {T,N} = similar(H.data, T, dims)

copy(H::UpperHessenberg{T}) where {T} = UpperHessenberg{T}(copy(H.data), H.μ)
real(H::UpperHessenberg{<:Real}) = H
real(H::UpperHessenberg{<:Complex}) = UpperHessenberg(triu!(real(H.data),-1), real(H.μ))
imag(H::UpperHessenberg) = UpperHessenberg(triu!(imag(H.data),-1), imag(H.μ))

function Matrix{T}(H::UpperHessenberg) where T
    m,n = size(H)
    B = triu!(copyto!(Matrix{T}(undef, m, n), H.data), -1)
    if !iszero(H.μ)
        for i = 1:min(m,n)
            @inbounds B[i,i] += H.μ
        end
    end
    return B
end

getindex(H::UpperHessenberg{T}, i::Integer, j::Integer) where {T} =
    i < j ? convert(T, H.data[i,j]) :
    i == j ? convert(T, H.data[i,i] + H.μ) :
    i == j+1 ? convert(T, H.data[i,j]) : zero(T)

function setindex!(A::UpperHessenberg, x, i::Integer, j::Integer)
    if i > j
        x == 0 || throw(ArgumentError("cannot set index in the lower triangular part " *
            "($i, $j) of an UpperHessenberg matrix to a nonzero value ($x)"))
    elseif i == j
        A.data[i,j] = x - A.μ
    else
        A.data[i,j] = x
    end
    return x
end

function Base.replace_in_print_matrix(A::UpperHessenberg, i::Integer, j::Integer, s::AbstractString)
    return i <= j+1 ? s : Base.replace_with_centered_mark(s)
end

Base.copy(A::Adjoint{<:Any,<:UpperHessenberg}) = adjoint!(copy(A.parent))
Base.copy(A::Transpose{<:Any,<:UpperHessenberg}) = transpose!(copy(A.parent))

-(A::UpperHessenberg) = UpperHessenberg(-A.data, -A.μ)
rmul!(H::UpperHessenberg, x::Number) = UpperHessenberg(rmul!(H.data, x), H.μ*x)
lmul!(x::Number, H::UpperHessenberg) = UpperHessenberg(lmul!(x, H.data), x*H.μ)

# (future: we could also have specialized routines for UpperHessenberg * UpperTriangular)

fillstored!(H::UpperHessenberg, x) = (H′ = UpperHessenberg(fillband!(H.data, x, -1, size(H,2)-1)); H′)

+(A::UpperHessenberg, B::UpperHessenberg) = UpperHessenberg(A.data+B.data, A.μ + B.μ)
-(A::UpperHessenberg, B::UpperHessenberg) = UpperHessenberg(A.data-B.data, A.μ - B.μ)
# (future: we could also have specialized routines for UpperHessenberg ± UpperTriangular)

# shift Hessenberg by λI
+(H::UpperHessenberg, J::UniformScaling) = UpperHessenberg(H, H.μ + J.λ)
+(J::UniformScaling, H::UpperHessenberg) = UpperHessenberg(H, J.λ + H.μ)
-(H::UpperHessenberg, J::UniformScaling) = UpperHessenberg(H, H.μ - J.λ)
-(J::UniformScaling, H::UpperHessenberg) = UpperHessenberg(-H.data, J.λ - H.μ)

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
function ldiv!(F::UpperHessenberg, B::AbstractVecOrMat)
    checksquare(F)
    m = size(F,1)
    m != size(B,1) && throw(DimensionMismatch("wrong right-hand-side # rows != $m"))
    require_one_based_indexing(B)
    n = size(B,2)
    H = F.data
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
function rdiv!(B::AbstractMatrix, F::UpperHessenberg)
    checksquare(F)
    m = size(F,1)
    m != size(B,2) && throw(DimensionMismatch("wrong right-hand-side # cols != $m"))
    require_one_based_indexing(B)
    n = size(B,1)
    H = F.data
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
function det(F::UpperHessenberg)
    checksquare(F)
    H = F.data
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
function logabsdet(F::UpperHessenberg)
    checksquare(F)
    H = F.data
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

######################################################################################
# Hessenberg factorizations Q(H+μI)Q' of A+μI:

"""
    Hessenberg <: Factorization

A `Hessenberg` object represents the Hessenberg factorization `QHQ'` of a square
matrix, and is produced by the [`hessenberg`](@ref) function.
"""
struct Hessenberg{T,TH,S<:UpperHessenberg{T,<:AbstractMatrix{TH}},W<:AbstractVector} <: Factorization{T}
    H::S # upper triangle is H, lower triangle is Q data (reflectors)
    τ::W # more Q (reflector) data

    function Hessenberg{T,TH,S,W}(H, τ) where {T,TH,S<:UpperHessenberg{T,<:AbstractMatrix{TH}},W<:AbstractVector}
        require_one_based_indexing(τ)
        new{T,TH,S,W}(H, τ)
    end
end
function Hessenberg(factors::AbstractMatrix{T}, τ::AbstractVector{T}, μ::V=false) where {T,V<:Number}
    H = UpperHessenberg(factors, μ)
    return Hessenberg{eltype(H),T,typeof(H),typeof(τ)}(H, τ)
end
Hessenberg(F::Hessenberg) = F
Hessenberg(F::Hessenberg, μ::Number) = Hessenberg(F.H.data, F.τ, μ)

copy(F::Hessenberg) = Hessenberg(copy(F.H.data), copy(F.τ), F.H.μ)
size(F::Hessenberg, d) = size(F.H, d)
size(F::Hessenberg) = size(F.H)

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
hessenberg!(A::StridedMatrix{<:BlasFloat}) = Hessenberg(LAPACK.gehrd!(A)...)

hessenberg(A::StridedMatrix{<:BlasFloat}) = hessenberg!(copy(A))

"""
    hessenberg(A) -> Hessenberg

Compute the Hessenberg decomposition of `A` and return a `Hessenberg` object. If `F` is the
factorization object, the unitary matrix can be accessed with `F.Q` (of type `LinearAlgebra.HessenbergQ`)
and the Hessenberg matrix with `F.H` (of type [`UpperHessenberg`](@ref)), either of
which may be converted to a regular matrix with `Matrix(F.H)` or `Matrix(F.Q)`.

Note that the shifted factorization `A+μI = Q (H+μI) Q'` can be
constructed efficiently by `F + μ*I` using the [`UniformScaling`](@ref)
object [`I`](@ref), which creates a new `Hessenberg` object with shared storage
and a modified shift.   The shift of a given `F` is obtained by `F.H.μ`.
This is useful because multiple shifted solves `(F + μ*I) \\ b`
(for different `μ` and/or `b`) can be performed efficiently once `F` is created.

Iterating the decomposition produces the factors `F.Q` and `F.H`.

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

hessenberg(H::UpperHessenberg) = H

function show(io::IO, mime::MIME"text/plain", F::Hessenberg)
    summary(io, F)
    println(io, "\nQ factor:")
    show(io, mime, F.Q)
    println(io, "\nH factor:")
    show(io, mime, F.H)
end

"""
    HessenbergQ <: AbstractQ

Given a [`Hessenberg`](@ref) factorization object `F`, `F.Q` returns
a `HessenbergQ` object, which is an implicit representation of the unitary
matrix `Q` in the Hessenberg factorization `QHQ'` represented by `F`.
This `F.Q` object can be efficiently multiplied by matrices or vectors,
and can be converted to an ordinary matrix type with `Matrix(F.Q)`.
"""
struct HessenbergQ{T,S<:AbstractMatrix,W<:AbstractVector} <: AbstractQ{T}
    factors::S
    τ::W
    function HessenbergQ{T,S,W}(factors, τ) where {T,S<:AbstractMatrix,W<:AbstractVector}
        new(factors, τ)
    end
end
HessenbergQ(factors::AbstractMatrix{T}, τ::AbstractVector) where {T} = HessenbergQ{T,typeof(factors),typeof(τ)}(factors, τ)
HessenbergQ(F::Hessenberg) = HessenbergQ(F.H.data, F.τ)

function getproperty(F::Hessenberg, d::Symbol)
    d == :Q && return HessenbergQ(F)
    return getfield(F, d)
end

Base.propertynames(F::Hessenberg, private::Bool=false) =
    (:Q, :H, (private ? (:τ,) : ())...)

# HessenbergQ from LAPACK/BLAS (as opposed to Julia libraries like GenericLinearAlgebra)
const BlasHessenbergQ{T} = HessenbergQ{T,<:StridedMatrix{T},<:StridedVector{T}} where {T<:BlasFloat}

## reconstruct the original matrix
Matrix{T}(Q::BlasHessenbergQ) where {T} = convert(Matrix{T}, LAPACK.orghr!(1, size(Q.factors, 1), copy(Q.factors), Q.τ))
AbstractArray(F::Hessenberg) = AbstractMatrix(F)
Matrix(F::Hessenberg) = Array(AbstractArray(F))
Array(F::Hessenberg) = Matrix(F)
function AbstractMatrix(F::Hessenberg)
    Q = F.Q
    A = rmul!(lmul!(Q, triu(F.H.data,-1)), Q') # don't include μ for efficiency if Q is real and μ is complex
    if iszero(F.H.μ)
        return A
    elseif typeof(zero(eltype(A))+F.H.μ) <: eltype(A) # can shift A in-place
        for i = 1:size(A,1)
            @inbounds A[i,i] += F.H.μ
        end
        return A
    else
        return A + F.H.μ*I # allocate another matrix, e.g. if A is real and μ is complex
    end
end

lmul!(Q::BlasHessenbergQ{T}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('L', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
rmul!(X::StridedMatrix{T}, Q::BlasHessenbergQ{T}) where {T<:BlasFloat} =
    LAPACK.ormhr!('R', 'N', 1, size(Q.factors, 1), Q.factors, Q.τ, X)
lmul!(adjQ::Adjoint{<:Any,<:BlasHessenbergQ{T}}, X::StridedVecOrMat{T}) where {T<:BlasFloat} =
    (Q = adjQ.parent; LAPACK.ormhr!('L', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))
rmul!(X::StridedMatrix{T}, adjQ::Adjoint{<:Any,<:BlasHessenbergQ{T}}) where {T<:BlasFloat} =
    (Q = adjQ.parent; LAPACK.ormhr!('R', ifelse(T<:Real, 'T', 'C'), 1, size(Q.factors, 1), Q.factors, Q.τ, X))

lmul!(Q::HessenbergQ{T}, X::Adjoint{T,<:StridedVecOrMat{T}}) where {T} = rmul!(X', Q')'
rmul!(X::Adjoint{T,<:StridedMatrix{T}}, Q::HessenbergQ{T}) where {T} = lmul!(Q', X')'
lmul!(adjQ::Adjoint{<:Any,<:HessenbergQ{T}}, X::Adjoint{T,<:StridedVecOrMat{T}}) where {T}  = rmul!(X', adjQ')'
rmul!(X::Adjoint{T,<:StridedMatrix{T}}, adjQ::Adjoint{<:Any,<:HessenbergQ{T}}) where {T} = lmul!(adjQ', X')'

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

# multiply Hessenberg by scalar (but don't modify lower triangle of F.H.data)
rmul!(F::Hessenberg{<:Any,T}, x::T) where {T<:Number} = Hessenberg(rmul_triu!(F.H.data, x, -1), F.τ, F.H.μ*x)
lmul!(x::T, F::Hessenberg{<:Any,T}) where {T<:Number} = Hessenberg(lmul_triu!(x, F.H.data, -1), F.τ, x*F.H.μ)
function (*)(F::Hessenberg{<:Any,T}, x::S) where {T,S<:Number}
    TS = typeof(zero(T) * x)
    if TS === T
        return rmul!(copy(F), convert(T, x))
    else
        return rmul!(Hessenberg(Matrix{TS}(F.H.data), Vector{TS}(F.τ), F.H.μ), convert(TS, x))
    end
end
function (*)(x::S, F::Hessenberg{<:Any,T}) where {T,S<:Number}
    TS = typeof(zero(T) * x)
    if TS === T
        return lmul!(convert(T, x), copy(F))
    else
        return lmul!(convert(TS, x), Hessenberg(Matrix{TS}(F.H.data), Vector{TS}(F.τ), F.H.μ))
    end
end
-(F::Hessenberg) = F * -one(eltype(F.H.data))

# shift Hessenberg by λI
+(F::Hessenberg, J::UniformScaling) = Hessenberg(F, F.H.μ + J.λ)
+(J::UniformScaling, F::Hessenberg) = Hessenberg(F, J.λ + F.H.μ)
-(F::Hessenberg, J::UniformScaling) = Hessenberg(F, F.H.μ - J.λ)
-(J::UniformScaling, F::Hessenberg) = Hessenberg(-F, J.λ - F.H.μ)

function ldiv!(F::Hessenberg, B::AbstractVecOrMat)
    Q = F.Q
    return lmul!(Q, ldiv!(F.H, lmul!(Q', B)))
end

function rdiv!(B::AbstractMatrix, F::Hessenberg)
    Q = F.Q
    return rmul!(rdiv!(rmul!(B, Q), F.H), Q')
end

# handle case of real H and complex μ — we need to work around the
# fact that we can't multiple a real F.Q by a complex matrix directly in LAPACK
function ldiv!(F::Hessenberg{<:Complex,<:Real}, B::AbstractVecOrMat{<:Complex})
    Q = F.Q
    Br = lmul!(Q', real(B))
    Bi = lmul!(Q', imag(B))
    ldiv!(F.H, B .= Complex.(Br,Bi))
    Br = lmul!(Q, real(B))
    Bi = lmul!(Q, imag(B))
    return B .= Complex.(Br,Bi)
end
function rdiv!(B::AbstractVecOrMat{<:Complex}, F::Hessenberg{<:Complex,<:Real})
    Q = F.Q
    Br = rmul!(real(B), Q)
    Bi = rmul!(imag(B), Q)
    rdiv!(B .= Complex.(Br,Bi), F.H)
    Br = rmul!(real(B), Q')
    Bi = rmul!(imag(B), Q')
    return B .= Complex.(Br,Bi)
end

ldiv!(F::Adjoint{<:Any,<:Hessenberg}, B::AbstractVecOrMat) = rdiv!(B', F')'
rdiv!(B::AbstractMatrix, F::Adjoint{<:Any,<:Hessenberg}) = ldiv!(F', B')'

det(F::Hessenberg) = det(F.H)
logabsdet(F::Hessenberg) = logabsdet(F.H)
function logdet(F::Hessenberg)
    d,s = logabsdet(F)
    return d + log(s)
end