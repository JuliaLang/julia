# This file is a part of Julia. License is MIT: https://julialang.org/license

# LQ Factorizations
"""
    LQ <: Factorization

Matrix factorization type of the `LQ` factorization of a matrix `A`. The `LQ`
decomposition is the [`QR`](@ref) decomposition of `transpose(A)`. This is the return
type of [`lq`](@ref), the corresponding matrix factorization function.

If `S::LQ` is the factorization object, the lower triangular component can be
obtained via `S.L`, and the orthogonal/unitary component via `S.Q`, such that
`A ≈ S.L*S.Q`.

Iterating the decomposition produces the components `S.L` and `S.Q`.

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Matrix{Float64}:
  5.0   7.0
 -2.0  -4.0

julia> S = lq(A)
LQ{Float64, Matrix{Float64}, Vector{Float64}}
L factor:
2×2 Matrix{Float64}:
 -8.60233   0.0
  4.41741  -0.697486
Q factor: 2×2 LinearAlgebra.LQPackedQ{Float64, Matrix{Float64}, Vector{Float64}}

julia> S.L * S.Q
2×2 Matrix{Float64}:
  5.0   7.0
 -2.0  -4.0

julia> l, q = S; # destructuring via iteration

julia> l == S.L &&  q == S.Q
true
```
"""
struct LQ{T,S<:AbstractMatrix{T},C<:AbstractVector{T}} <: Factorization{T}
    factors::S
    τ::C

    function LQ{T,S,C}(factors, τ) where {T,S<:AbstractMatrix{T},C<:AbstractVector{T}}
        require_one_based_indexing(factors)
        new{T,S,C}(factors, τ)
    end
end
LQ(factors::AbstractMatrix{T}, τ::AbstractVector{T}) where {T} =
    LQ{T,typeof(factors),typeof(τ)}(factors, τ)
LQ{T}(factors::AbstractMatrix, τ::AbstractVector) where {T} =
    LQ(convert(AbstractMatrix{T}, factors), convert(AbstractVector{T}, τ))
# backwards-compatible constructors (remove with Julia 2.0)
@deprecate(LQ{T,S}(factors::AbstractMatrix{T}, τ::AbstractVector{T}) where {T,S},
           LQ{T,S,typeof(τ)}(factors, τ), false)

# iteration for destructuring into components
Base.iterate(S::LQ) = (S.L, Val(:Q))
Base.iterate(S::LQ, ::Val{:Q}) = (S.Q, Val(:done))
Base.iterate(S::LQ, ::Val{:done}) = nothing

"""
    lq!(A) -> LQ

Compute the [`LQ`](@ref) factorization of `A`, using the input
matrix as a workspace. See also [`lq`](@ref).
"""
lq!(A::StridedMatrix{<:BlasFloat}) = LQ(LAPACK.gelqf!(A)...)

"""
    lq(A) -> S::LQ

Compute the LQ decomposition of `A`. The decomposition's lower triangular
component can be obtained from the [`LQ`](@ref) object `S` via `S.L`, and the
orthogonal/unitary component via `S.Q`, such that `A ≈ S.L*S.Q`.

Iterating the decomposition produces the components `S.L` and `S.Q`.

The LQ decomposition is the QR decomposition of `transpose(A)`, and it is useful
in order to compute the minimum-norm solution `lq(A) \\ b` to an underdetermined
system of equations (`A` has more columns than rows, but has full row rank).

# Examples
```jldoctest
julia> A = [5. 7.; -2. -4.]
2×2 Matrix{Float64}:
  5.0   7.0
 -2.0  -4.0

julia> S = lq(A)
LQ{Float64, Matrix{Float64}, Vector{Float64}}
L factor:
2×2 Matrix{Float64}:
 -8.60233   0.0
  4.41741  -0.697486
Q factor: 2×2 LinearAlgebra.LQPackedQ{Float64, Matrix{Float64}, Vector{Float64}}

julia> S.L * S.Q
2×2 Matrix{Float64}:
  5.0   7.0
 -2.0  -4.0

julia> l, q = S; # destructuring via iteration

julia> l == S.L &&  q == S.Q
true
```
"""
lq(A::AbstractMatrix{T}) where {T} = lq!(copy_similar(A, lq_eltype(T)))
lq(x::Number) = lq!(fill(convert(lq_eltype(typeof(x)), x), 1, 1))

lq_eltype(::Type{T}) where {T} = typeof(zero(T) / sqrt(abs2(one(T))))

copy(A::LQ) = LQ(copy(A.factors), copy(A.τ))

LQ{T}(A::LQ) where {T} = LQ(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ))
Factorization{T}(A::LQ) where {T} = LQ{T}(A)

AbstractMatrix(A::LQ) = A.L*A.Q
AbstractArray(A::LQ) = AbstractMatrix(A)
Matrix(A::LQ) = Array(AbstractArray(A))
Array(A::LQ) = Matrix(A)

transpose(F::LQ{<:Real}) = F'
transpose(::LQ) =
    throw(ArgumentError("transpose of LQ decomposition is not supported, consider using adjoint"))

Base.copy(F::AdjointFactorization{T,<:LQ{T}}) where {T} =
    QR{T,typeof(F.parent.factors),typeof(F.parent.τ)}(copy(adjoint(F.parent.factors)), copy(F.parent.τ))

function getproperty(F::LQ, d::Symbol)
    m, n = size(F)
    if d === :L
        return tril!(getfield(F, :factors)[1:m, 1:min(m,n)])
    elseif d === :Q
        return LQPackedQ(getfield(F, :factors), getfield(F, :τ))
    else
        return getfield(F, d)
    end
end

Base.propertynames(F::LQ, private::Bool=false) =
    (:L, :Q, (private ? fieldnames(typeof(F)) : ())...)

# getindex(A::LQPackedQ, i::Integer, j::Integer) =
#     lmul!(A, setindex!(zeros(eltype(A), size(A, 2)), 1, j))[i]

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::LQ)
    summary(io, F); println(io)
    println(io, "L factor:")
    show(io, mime, F.L)
    print(io, "\nQ factor: ")
    show(io, mime, F.Q)
end

size(F::LQ, dim::Integer) = size(getfield(F, :factors), dim)
size(F::LQ)               = size(getfield(F, :factors))

## Multiplication by LQ
function lmul!(A::LQ, B::AbstractVecOrMat)
    lmul!(LowerTriangular(A.L), view(lmul!(A.Q, B), 1:size(A,1), axes(B,2)))
    return B
end
function *(A::LQ{TA}, B::AbstractVecOrMat{TB}) where {TA,TB}
    TAB = promote_type(TA, TB)
    _cut_B(lmul!(convert(Factorization{TAB}, A), copy_similar(B, TAB)), 1:size(A,1))
end

# With a real lhs and complex rhs with the same precision, we can reinterpret
# the complex rhs as a real rhs with twice the number of columns
function (\)(F::LQ{T}, B::VecOrMat{Complex{T}}) where T<:BlasReal
    require_one_based_indexing(B)
    X = zeros(T, size(F,2), 2*size(B,2))
    X[1:size(B,1), 1:size(B,2)] .= real.(B)
    X[1:size(B,1), size(B,2)+1:size(X,2)] .= imag.(B)
    ldiv!(F, X)
    return reshape(copy(reinterpret(Complex{T}, copy(transpose(reshape(X, div(length(X), 2), 2))))),
                           isa(B, AbstractVector) ? (size(F,2),) : (size(F,2), size(B,2)))
end


function ldiv!(A::LQ, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    m, n = size(A)
    m ≤ n || throw(DimensionMismatch("LQ solver does not support overdetermined systems (more rows than columns)"))

    ldiv!(LowerTriangular(A.L), view(B, 1:size(A,1), axes(B,2)))
    return lmul!(adjoint(A.Q), B)
end

function ldiv!(Fadj::AdjointFactorization{<:Any,<:LQ}, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    m, n = size(Fadj)
    m >= n || throw(DimensionMismatch("solver does not support underdetermined systems (more columns than rows)"))

    F = parent(Fadj)
    lmul!(F.Q, B)
    ldiv!(UpperTriangular(adjoint(F.L)), view(B, 1:size(F,1), axes(B,2)))
    return B
end
