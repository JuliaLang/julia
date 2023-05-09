# This file is a part of Julia. License is MIT: https://julialang.org/license

# QR Factorization
"""
    QR <: Factorization

A QR matrix factorization stored in a packed format, typically obtained from
[`qr`](@ref). If ``A`` is an `m`×`n` matrix, then

```math
A = Q R
```

where ``Q`` is an orthogonal/unitary matrix and ``R`` is upper triangular.
The matrix ``Q`` is stored as a sequence of Householder reflectors ``v_i``
and coefficients ``\\tau_i`` where:

```math
Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T).
```

Iterating the decomposition produces the components `Q` and `R`.

The object has two fields:

* `factors` is an `m`×`n` matrix.

  - The upper triangular part contains the elements of ``R``, that is `R =
    triu(F.factors)` for a `QR` object `F`.

  - The subdiagonal part contains the reflectors ``v_i`` stored in a packed format where
    ``v_i`` is the ``i``th column of the matrix `V = I + tril(F.factors, -1)`.

* `τ` is a vector  of length `min(m,n)` containing the coefficients ``\tau_i``.
"""
struct QR{T,S<:AbstractMatrix{T},C<:AbstractVector{T}} <: Factorization{T}
    factors::S
    τ::C

    function QR{T,S,C}(factors, τ) where {T,S<:AbstractMatrix{T},C<:AbstractVector{T}}
        require_one_based_indexing(factors)
        new{T,S,C}(factors, τ)
    end
end
QR(factors::AbstractMatrix{T}, τ::AbstractVector{T}) where {T} =
    QR{T,typeof(factors),typeof(τ)}(factors, τ)
QR{T}(factors::AbstractMatrix, τ::AbstractVector) where {T} =
    QR(convert(AbstractMatrix{T}, factors), convert(AbstractVector{T}, τ))
# backwards-compatible constructors (remove with Julia 2.0)
@deprecate(QR{T,S}(factors::AbstractMatrix{T}, τ::AbstractVector{T}) where {T,S},
           QR{T,S,typeof(τ)}(factors, τ), false)

# iteration for destructuring into components
Base.iterate(S::QR) = (S.Q, Val(:R))
Base.iterate(S::QR, ::Val{:R}) = (S.R, Val(:done))
Base.iterate(S::QR, ::Val{:done}) = nothing

# Note. For QRCompactWY factorization without pivoting, the WY representation based method introduced in LAPACK 3.4
"""
    QRCompactWY <: Factorization

A QR matrix factorization stored in a compact blocked format, typically obtained from
[`qr`](@ref). If ``A`` is an `m`×`n` matrix, then

```math
A = Q R
```

where ``Q`` is an orthogonal/unitary matrix and ``R`` is upper triangular. It is similar
to the [`QR`](@ref) format except that the orthogonal/unitary matrix ``Q`` is stored in
*Compact WY* format [^Schreiber1989].  For the block size ``n_b``, it is stored as
a `m`×`n` lower trapezoidal matrix ``V`` and a matrix ``T = (T_1 \\; T_2 \\; ... \\;
T_{b-1} \\; T_b')`` composed of ``b = \\lceil \\min(m,n) / n_b \\rceil`` upper triangular
matrices ``T_j`` of size ``n_b``×``n_b`` (``j = 1, ..., b-1``) and an upper trapezoidal
``n_b``×``\\min(m,n) - (b-1) n_b`` matrix ``T_b'`` (``j=b``) whose upper square part
denoted with ``T_b`` satisfying

```math
Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T)
= \\prod_{j=1}^{b} (I - V_j T_j V_j^T)
```

such that ``v_i`` is the ``i``th column of ``V``, ``\\tau_i`` is the ``i``th element
of `[diag(T_1); diag(T_2); …; diag(T_b)]`, and ``(V_1 \\; V_2 \\; ... \\; V_b)``
is the left `m`×`min(m, n)` block of ``V``.  When constructed using [`qr`](@ref),
the block size is given by ``n_b = \\min(m, n, 36)``.

Iterating the decomposition produces the components `Q` and `R`.

The object has two fields:

* `factors`, as in the [`QR`](@ref) type, is an `m`×`n` matrix.

  - The upper triangular part contains the elements of ``R``, that is `R =
    triu(F.factors)` for a `QR` object `F`.

  - The subdiagonal part contains the reflectors ``v_i`` stored in a packed format such
    that `V = I + tril(F.factors, -1)`.

* `T` is a ``n_b``-by-``\\min(m,n)`` matrix as described above. The subdiagonal elements
  for each triangular matrix ``T_j`` are ignored.

!!! note

    This format should not to be confused with the older *WY* representation
    [^Bischof1987].


[^Bischof1987]: C Bischof and C Van Loan, "The WY representation for products of Householder matrices", SIAM J Sci Stat Comput 8 (1987), s2-s13. [doi:10.1137/0908009](https://doi.org/10.1137/0908009)

[^Schreiber1989]: R Schreiber and C Van Loan, "A storage-efficient WY representation for products of Householder transformations", SIAM J Sci Stat Comput 10 (1989), 53-57. [doi:10.1137/0910005](https://doi.org/10.1137/0910005)
"""
struct QRCompactWY{S,M<:AbstractMatrix{S},C<:AbstractMatrix{S}} <: Factorization{S}
    factors::M
    T::C

    function QRCompactWY{S,M,C}(factors, T) where {S,M<:AbstractMatrix{S},C<:AbstractMatrix{S}}
        require_one_based_indexing(factors)
        new{S,M,C}(factors, T)
    end
end
QRCompactWY(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where {S} =
    QRCompactWY{S,typeof(factors),typeof(T)}(factors, T)
QRCompactWY{S}(factors::AbstractMatrix, T::AbstractMatrix) where {S} =
    QRCompactWY(convert(AbstractMatrix{S}, factors), convert(AbstractMatrix{S}, T))
# backwards-compatible constructors (remove with Julia 2.0)
@deprecate(QRCompactWY{S,M}(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where {S,M},
           QRCompactWY{S,M,typeof(T)}(factors, T), false)

# iteration for destructuring into components
Base.iterate(S::QRCompactWY) = (S.Q, Val(:R))
Base.iterate(S::QRCompactWY, ::Val{:R}) = (S.R, Val(:done))
Base.iterate(S::QRCompactWY, ::Val{:done}) = nothing

# returns upper triangular views of all non-undef values of `qr(A).T`:
#
# julia> sparse(qr(A).T .== qr(A).T)
# 36×100 SparseMatrixCSC{Bool, Int64} with 1767 stored entries:
# ⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
# ⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
# ⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿
# ⠀⠀⠀⠀⠀⠂⠛⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿
# ⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⢀⠐⠙⢿⣿⣿⣿⣿
# ⠀⠀⠐⠀⠀⠀⠀⠀⠀⢀⢙⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠁⠀⡀⠀⠙⢿⣿⣿
# ⠀⠀⠐⠀⠀⠀⠀⠀⠀⠀⠄⠀⠙⢿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀⡀⠀⠀⢀⠀⠀⠙⢿
# ⠀⡀⠀⠀⠀⠀⠀⠀⠂⠒⠒⠀⠀⠀⠙⢿⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⣿⣿⠀⠀⠀⠀⠀⠀⠀⢀⠀⠀⠀⡀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⣈⡀⠀⠀⠀⠀⠀⠀⠙⢿⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢿⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠂⠀⢀⠀
#
function _triuppers_qr(T)
    blocksize, cols = size(T)
    return Iterators.map(0:div(cols - 1, blocksize)) do i
        n = min(blocksize, cols - i * blocksize)
        return UpperTriangular(view(T, 1:n, (1:n) .+ i * blocksize))
    end
end

function Base.hash(F::QRCompactWY, h::UInt)
    return hash(F.factors, foldr(hash, _triuppers_qr(F.T); init=hash(QRCompactWY, h)))
end
function Base.:(==)(A::QRCompactWY, B::QRCompactWY)
    return A.factors == B.factors && all(splat(==), zip(_triuppers_qr.((A.T, B.T))...))
end
function Base.isequal(A::QRCompactWY, B::QRCompactWY)
    return isequal(A.factors, B.factors) && all(zip(_triuppers_qr.((A.T, B.T))...)) do (a, b)
        isequal(a, b)::Bool
    end
end

"""
    QRPivoted <: Factorization

A QR matrix factorization with column pivoting in a packed format, typically obtained from
[`qr`](@ref). If ``A`` is an `m`×`n` matrix, then

```math
A P = Q R
```

where ``P`` is a permutation matrix, ``Q`` is an orthogonal/unitary matrix and ``R`` is
upper triangular. The matrix ``Q`` is stored as a sequence of Householder reflectors:

```math
Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T).
```

Iterating the decomposition produces the components `Q`, `R`, and `p`.

The object has three fields:

* `factors` is an `m`×`n` matrix.

  - The upper triangular part contains the elements of ``R``, that is `R =
    triu(F.factors)` for a `QR` object `F`.

  - The subdiagonal part contains the reflectors ``v_i`` stored in a packed format where
    ``v_i`` is the ``i``th column of the matrix `V = I + tril(F.factors, -1)`.

* `τ` is a vector of length `min(m,n)` containing the coefficients ``\tau_i``.

* `jpvt` is an integer vector of length `n` corresponding to the permutation ``P``.
"""
struct QRPivoted{T,S<:AbstractMatrix{T},C<:AbstractVector{T},P<:AbstractVector{<:Integer}} <: Factorization{T}
    factors::S
    τ::C
    jpvt::P

    function QRPivoted{T,S,C,P}(factors, τ, jpvt) where {T,S<:AbstractMatrix{T},C<:AbstractVector{T},P<:AbstractVector{<:Integer}}
        require_one_based_indexing(factors, τ, jpvt)
        new{T,S,C,P}(factors, τ, jpvt)
    end
end
QRPivoted(factors::AbstractMatrix{T}, τ::AbstractVector{T},
          jpvt::AbstractVector{<:Integer}) where {T} =
    QRPivoted{T,typeof(factors),typeof(τ),typeof(jpvt)}(factors, τ, jpvt)
QRPivoted{T}(factors::AbstractMatrix, τ::AbstractVector,
             jpvt::AbstractVector{<:Integer}) where {T} =
    QRPivoted(convert(AbstractMatrix{T}, factors), convert(AbstractVector{T}, τ), jpvt)
# backwards-compatible constructors (remove with Julia 2.0)
@deprecate(QRPivoted{T,S}(factors::AbstractMatrix{T}, τ::AbstractVector{T},
                          jpvt::AbstractVector{<:Integer}) where {T,S},
           QRPivoted{T,S,typeof(τ),typeof(jpvt)}(factors, τ, jpvt), false)

# iteration for destructuring into components
Base.iterate(S::QRPivoted) = (S.Q, Val(:R))
Base.iterate(S::QRPivoted, ::Val{:R}) = (S.R, Val(:p))
Base.iterate(S::QRPivoted, ::Val{:p}) = (S.p, Val(:done))
Base.iterate(S::QRPivoted, ::Val{:done}) = nothing

function qrfactUnblocked!(A::AbstractMatrix{T}) where {T}
    require_one_based_indexing(A)
    m, n = size(A)
    τ = zeros(T, min(m,n))
    for k = 1:min(m - 1 + !(T<:Real), n)
        x = view(A, k:m, k)
        τk = reflector!(x)
        τ[k] = τk
        reflectorApply!(x, τk, view(A, k:m, k + 1:n))
    end
    QR(A, τ)
end

# Find index for columns with largest two norm
function indmaxcolumn(A::AbstractMatrix)
    mm = norm(view(A, :, 1))
    ii = 1
    for i = 2:size(A, 2)
        mi = norm(view(A, :, i))
        if abs(mi) > mm
            mm = mi
            ii = i
        end
    end
    return ii
end

function qrfactPivotedUnblocked!(A::AbstractMatrix)
    m, n = size(A)
    piv = Vector(UnitRange{BlasInt}(1,n))
    τ = Vector{eltype(A)}(undef, min(m,n))
    for j = 1:min(m,n)

        # Find column with maximum norm in trailing submatrix
        jm = indmaxcolumn(view(A, j:m, j:n)) + j - 1

        if jm != j
            # Flip elements in pivoting vector
            tmpp = piv[jm]
            piv[jm] = piv[j]
            piv[j] = tmpp

            # Update matrix with
            for i = 1:m
                tmp = A[i,jm]
                A[i,jm] = A[i,j]
                A[i,j] = tmp
            end
        end

        # Compute reflector of columns j
        x = view(A, j:m, j)
        τj = reflector!(x)
        τ[j] = τj

        # Update trailing submatrix with reflector
        reflectorApply!(x, τj, view(A, j:m, j+1:n))
    end
    return QRPivoted{eltype(A), typeof(A), typeof(τ), typeof(piv)}(A, τ, piv)
end

# LAPACK version
qr!(A::StridedMatrix{<:BlasFloat}, ::NoPivot; blocksize=36) =
    QRCompactWY(LAPACK.geqrt!(A, min(min(size(A)...), blocksize))...)
qr!(A::StridedMatrix{<:BlasFloat}, ::ColumnNorm) = QRPivoted(LAPACK.geqp3!(A)...)

# Generic fallbacks

"""
    qr!(A, pivot = NoPivot(); blocksize)

`qr!` is the same as [`qr`](@ref) when `A` is a subtype of [`AbstractMatrix`](@ref),
but saves space by overwriting the input `A`, instead of creating a copy.
An [`InexactError`](@ref) exception is thrown if the factorization produces a number not
representable by the element type of `A`, e.g. for integer types.

!!! compat "Julia 1.4"
    The `blocksize` keyword argument requires Julia 1.4 or later.

# Examples
```jldoctest
julia> a = [1. 2.; 3. 4.]
2×2 Matrix{Float64}:
 1.0  2.0
 3.0  4.0

julia> qr!(a)
LinearAlgebra.QRCompactWY{Float64, Matrix{Float64}, Matrix{Float64}}
Q factor:
2×2 LinearAlgebra.QRCompactWYQ{Float64, Matrix{Float64}, Matrix{Float64}}
R factor:
2×2 Matrix{Float64}:
 -3.16228  -4.42719
  0.0      -0.632456

julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> qr!(a)
ERROR: InexactError: Int64(3.1622776601683795)
Stacktrace:
[...]
```
"""
qr!(A::AbstractMatrix, ::NoPivot) = qrfactUnblocked!(A)
qr!(A::AbstractMatrix, ::ColumnNorm) = qrfactPivotedUnblocked!(A)
qr!(A::AbstractMatrix) = qr!(A, NoPivot())
# TODO: Remove in Julia v2.0
@deprecate qr!(A::AbstractMatrix, ::Val{true})  qr!(A, ColumnNorm())
@deprecate qr!(A::AbstractMatrix, ::Val{false}) qr!(A, NoPivot())

_qreltype(::Type{T}) where T = typeof(zero(T)/sqrt(abs2(one(T))))

"""
    qr(A, pivot = NoPivot(); blocksize) -> F

Compute the QR factorization of the matrix `A`: an orthogonal (or unitary if `A` is
complex-valued) matrix `Q`, and an upper triangular matrix `R` such that

```math
A = Q R
```

The returned object `F` stores the factorization in a packed format:

 - if `pivot == ColumnNorm()` then `F` is a [`QRPivoted`](@ref) object,

 - otherwise if the element type of `A` is a BLAS type ([`Float32`](@ref), [`Float64`](@ref),
   `ComplexF32` or `ComplexF64`), then `F` is a [`QRCompactWY`](@ref) object,

 - otherwise `F` is a [`QR`](@ref) object.

The individual components of the decomposition `F` can be retrieved via property accessors:

 - `F.Q`: the orthogonal/unitary matrix `Q`
 - `F.R`: the upper triangular matrix `R`
 - `F.p`: the permutation vector of the pivot ([`QRPivoted`](@ref) only)
 - `F.P`: the permutation matrix of the pivot ([`QRPivoted`](@ref) only)

Iterating the decomposition produces the components `Q`, `R`, and if extant `p`.

The following functions are available for the `QR` objects: [`inv`](@ref), [`size`](@ref),
and [`\\`](@ref). When `A` is rectangular, `\\` will return a least squares
solution and if the solution is not unique, the one with smallest norm is returned. When
`A` is not full rank, factorization with (column) pivoting is required to obtain a minimum
norm solution.

Multiplication with respect to either full/square or non-full/square `Q` is allowed, i.e. both `F.Q*F.R`
and `F.Q*A` are supported. A `Q` matrix can be converted into a regular matrix with
[`Matrix`](@ref). This operation returns the "thin" Q factor, i.e., if `A` is `m`×`n` with `m>=n`, then
`Matrix(F.Q)` yields an `m`×`n` matrix with orthonormal columns.  To retrieve the "full" Q factor, an
`m`×`m` orthogonal matrix, use `F.Q*I`. If `m<=n`, then `Matrix(F.Q)` yields an `m`×`m`
orthogonal matrix.

The block size for QR decomposition can be specified by keyword argument
`blocksize :: Integer` when `pivot == NoPivot()` and `A isa StridedMatrix{<:BlasFloat}`.
It is ignored when `blocksize > minimum(size(A))`. See [`QRCompactWY`](@ref).

!!! compat "Julia 1.4"
    The `blocksize` keyword argument requires Julia 1.4 or later.

# Examples
```jldoctest
julia> A = [3.0 -6.0; 4.0 -8.0; 0.0 1.0]
3×2 Matrix{Float64}:
 3.0  -6.0
 4.0  -8.0
 0.0   1.0

julia> F = qr(A)
LinearAlgebra.QRCompactWY{Float64, Matrix{Float64}, Matrix{Float64}}
Q factor:
3×3 LinearAlgebra.QRCompactWYQ{Float64, Matrix{Float64}, Matrix{Float64}}
R factor:
2×2 Matrix{Float64}:
 -5.0  10.0
  0.0  -1.0

julia> F.Q * F.R == A
true
```

!!! note
    `qr` returns multiple types because LAPACK uses several representations
    that minimize the memory storage requirements of products of Householder
    elementary reflectors, so that the `Q` and `R` matrices can be stored
    compactly rather as two separate dense matrices.
"""
function qr(A::AbstractMatrix{T}, arg...; kwargs...) where T
    require_one_based_indexing(A)
    AA = copy_similar(A, _qreltype(T))
    return qr!(AA, arg...; kwargs...)
end
# TODO: remove in Julia v2.0
@deprecate qr(A::AbstractMatrix, ::Val{false}; kwargs...) qr(A, NoPivot(); kwargs...)
@deprecate qr(A::AbstractMatrix, ::Val{true}; kwargs...)  qr(A, ColumnNorm(); kwargs...)

qr(x::Number) = qr(fill(x,1,1))
function qr(v::AbstractVector)
    require_one_based_indexing(v)
    qr(reshape(v, (length(v), 1)))
end

# Conversions
QR{T}(A::QR) where {T} = QR(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ))
Factorization{T}(A::QR{T}) where {T} = A
Factorization{T}(A::QR) where {T} = QR{T}(A)
QRCompactWY{T}(A::QRCompactWY) where {T} = QRCompactWY(convert(AbstractMatrix{T}, A.factors), convert(AbstractMatrix{T}, A.T))
Factorization{T}(A::QRCompactWY{T}) where {T} = A
Factorization{T}(A::QRCompactWY) where {T} = QRCompactWY{T}(A)
AbstractMatrix(F::Union{QR,QRCompactWY}) = F.Q * F.R
AbstractArray(F::Union{QR,QRCompactWY}) = AbstractMatrix(F)
Matrix(F::Union{QR,QRCompactWY}) = Array(AbstractArray(F))
Array(F::Union{QR,QRCompactWY}) = Matrix(F)
QRPivoted{T}(A::QRPivoted) where {T} = QRPivoted(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ), A.jpvt)
Factorization{T}(A::QRPivoted{T}) where {T} = A
Factorization{T}(A::QRPivoted) where {T} = QRPivoted{T}(A)
AbstractMatrix(F::QRPivoted) = (F.Q * F.R)[:,invperm(F.p)]
AbstractArray(F::QRPivoted) = AbstractMatrix(F)
Matrix(F::QRPivoted) = Array(AbstractArray(F))
Array(F::QRPivoted) = Matrix(F)

function show(io::IO, mime::MIME{Symbol("text/plain")}, F::Union{QR, QRCompactWY, QRPivoted})
    summary(io, F); println(io)
    println(io, "Q factor:")
    show(io, mime, F.Q)
    println(io, "\nR factor:")
    show(io, mime, F.R)
    if F isa QRPivoted
        println(io, "\npermutation:")
        show(io, mime, F.p)
    end
end

function getproperty(F::QR, d::Symbol)
    m, n = size(F)
    if d === :R
        return triu!(getfield(F, :factors)[1:min(m,n), 1:n])
    elseif d === :Q
        return QRPackedQ(getfield(F, :factors), F.τ)
    else
        getfield(F, d)
    end
end
function getproperty(F::QRCompactWY, d::Symbol)
    m, n = size(F)
    if d === :R
        return triu!(getfield(F, :factors)[1:min(m,n), 1:n])
    elseif d === :Q
        return QRCompactWYQ(getfield(F, :factors), F.T)
    else
        getfield(F, d)
    end
end
Base.propertynames(F::Union{QR,QRCompactWY}, private::Bool=false) =
    (:R, :Q, (private ? fieldnames(typeof(F)) : ())...)

function getproperty(F::QRPivoted{T}, d::Symbol) where T
    m, n = size(F)
    if d === :R
        return triu!(getfield(F, :factors)[1:min(m,n), 1:n])
    elseif d === :Q
        return QRPackedQ(getfield(F, :factors), F.τ)
    elseif d === :p
        return getfield(F, :jpvt)
    elseif d === :P
        p = F.p
        n = length(p)
        P = zeros(T, n, n)
        for i in 1:n
            P[p[i],i] = one(T)
        end
        return P
    else
        getfield(F, d)
    end
end
Base.propertynames(F::QRPivoted, private::Bool=false) =
    (:R, :Q, :p, :P, (private ? fieldnames(typeof(F)) : ())...)

transpose(F::Union{QR{<:Real},QRPivoted{<:Real},QRCompactWY{<:Real}}) = F'
transpose(::Union{QR,QRPivoted,QRCompactWY}) =
    throw(ArgumentError("transpose of QR decomposition is not supported, consider using adjoint"))

size(F::Union{QR,QRCompactWY,QRPivoted}) = size(getfield(F, :factors))
size(F::Union{QR,QRCompactWY,QRPivoted}, dim::Integer) = size(getfield(F, :factors), dim)


function ldiv!(A::QRCompactWY{T}, b::AbstractVector{T}) where {T}
    require_one_based_indexing(b)
    m, n = size(A)
    ldiv!(UpperTriangular(view(A.factors, 1:min(m,n), 1:n)), view(lmul!(adjoint(A.Q), b), 1:size(A, 2)))
    return b
end
function ldiv!(A::QRCompactWY{T}, B::AbstractMatrix{T}) where {T}
    require_one_based_indexing(B)
    m, n = size(A)
    ldiv!(UpperTriangular(view(A.factors, 1:min(m,n), 1:n)), view(lmul!(adjoint(A.Q), B), 1:size(A, 2), 1:size(B, 2)))
    return B
end

# Julia implementation similar to xgelsy
function ldiv!(A::QRPivoted{T,<:StridedMatrix}, B::AbstractMatrix{T}, rcond::Real) where {T<:BlasFloat}
    require_one_based_indexing(B)
    m, n = size(A)

    if m > size(B, 1) || n > size(B, 1)
        throw(DimensionMismatch("B has leading dimension $(size(B, 1)) but needs at least $(max(m, n))"))
    end

    if length(A.factors) == 0 || length(B) == 0
        return B, 0
    end

    @inbounds begin
        smin = smax = abs(A.factors[1])

        if smax == 0
            return fill!(B, 0), 0
        end

        mn = min(m, n)

        # allocate temporary work space
        tmp  = Vector{T}(undef, 2mn)
        wmin = view(tmp, 1:mn)
        wmax = view(tmp, mn+1:2mn)

        rnk = 1
        wmin[1] = 1
        wmax[1] = 1

        while rnk < mn
            i = rnk + 1

            smin, s1, c1 = LAPACK.laic1!(2, view(wmin, 1:rnk), smin, view(A.factors, 1:rnk, i), A.factors[i,i])
            smax, s2, c2 = LAPACK.laic1!(1, view(wmax, 1:rnk), smax, view(A.factors, 1:rnk, i), A.factors[i,i])

            if smax*rcond > smin
                break
            end

            for j in 1:rnk
                wmin[j] *= s1
                wmax[j] *= s2
            end
            wmin[i] = c1
            wmax[i] = c2

            rnk += 1
        end

        if rnk < n
            C, τ = LAPACK.tzrzf!(A.factors[1:rnk, :])
            work = vec(C)
        else
            C, τ = A.factors, A.τ
            work = resize!(tmp, n)
        end

        lmul!(adjoint(A.Q), view(B, 1:m, :))
        ldiv!(UpperTriangular(view(C, 1:rnk, 1:rnk)), view(B, 1:rnk, :))

        if rnk < n
            B[rnk+1:n,:] .= zero(T)
            LAPACK.ormrz!('L', T <: Complex ? 'C' : 'T', C, τ, view(B, 1:n, :))
        end

        for j in axes(B, 2)
            for i in 1:n
                work[A.p[i]] = B[i,j]
            end
            for i in 1:n
                B[i,j] = work[i]
            end
        end
    end

    return B, rnk
end

ldiv!(A::QRPivoted{T,<:StridedMatrix}, B::AbstractVector{T}) where {T<:BlasFloat} =
    vec(ldiv!(A, reshape(B, length(B), 1)))
ldiv!(A::QRPivoted{T,<:StridedMatrix}, B::AbstractMatrix{T}) where {T<:BlasFloat} =
    ldiv!(A, B, min(size(A)...)*eps(real(T)))[1]

function _wide_qr_ldiv!(A::QR{T}, B::AbstractMatrix{T}) where T
    m, n = size(A)
    minmn = min(m,n)
    mB, nB = size(B)
    lmul!(adjoint(A.Q), view(B, 1:m, :))
    R = A.R # makes a copy, used as a buffer below
    @inbounds begin
        if n > m # minimum norm solution
            τ = zeros(T,m)
            for k = m:-1:1 # Trapezoid to triangular by elementary operation
                x = view(R, k, [k; m + 1:n])
                τk = reflector!(x)
                τ[k] = conj(τk)
                for i = 1:k - 1
                    vRi = R[i,k]
                    for j = m + 1:n
                        vRi += R[i,j]*x[j - m + 1]'
                    end
                    vRi *= τk
                    R[i,k] -= vRi
                    for j = m + 1:n
                        R[i,j] -= vRi*x[j - m + 1]
                    end
                end
            end
        end
        ldiv!(UpperTriangular(view(R, :, 1:minmn)), view(B, 1:minmn, :))
        if n > m # Apply elementary transformation to solution
            B[m + 1:mB,1:nB] .= zero(T)
            for j = 1:nB
                for k = 1:m
                    vBj = B[k,j]'
                    for i = m + 1:n
                        vBj += B[i,j]'*R[k,i]'
                    end
                    vBj *= τ[k]
                    B[k,j] -= vBj'
                    for i = m + 1:n
                        B[i,j] -= R[k,i]'*vBj'
                    end
                end
            end
        end
    end
    return B
end


function ldiv!(A::QR{T}, B::AbstractMatrix{T}) where T
    m, n = size(A)
    m < n && return _wide_qr_ldiv!(A, B)

    lmul!(adjoint(A.Q), view(B, 1:m, :))
    R = A.factors
    ldiv!(UpperTriangular(view(R,1:n,:)), view(B, 1:n, :))
    return B
end
function ldiv!(A::QR, B::AbstractVector)
    ldiv!(A, reshape(B, length(B), 1))
    return B
end

function ldiv!(A::QRPivoted, b::AbstractVector)
    ldiv!(QR(A.factors,A.τ), b)
    b[1:size(A.factors, 2)] = view(b, 1:size(A.factors, 2))[invperm(A.jpvt)]
    b
end
function ldiv!(A::QRPivoted, B::AbstractMatrix)
    ldiv!(QR(A.factors, A.τ), B)
    B[1:size(A.factors, 2),:] = view(B, 1:size(A.factors, 2), :)[invperm(A.jpvt),:]
    B
end

function _apply_permutation!(F::QRPivoted, B::AbstractVecOrMat)
    # Apply permutation but only to the top part of the solution vector since
    # it's padded with zeros for underdetermined problems
    B[1:length(F.p), :] = B[F.p, :]
    return B
end
_apply_permutation!(::Factorization, B::AbstractVecOrMat) = B

function ldiv!(Fadj::AdjointFactorization{<:Any,<:Union{QR,QRCompactWY,QRPivoted}}, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    m, n = size(Fadj)

    # We don't allow solutions overdetermined systems
    if m > n
        throw(DimensionMismatch("overdetermined systems are not supported"))
    end
    if n != size(B, 1)
        throw(DimensionMismatch("inputs should have the same number of rows"))
    end
    F = parent(Fadj)

    B = _apply_permutation!(F, B)

    # For underdetermined system, the triangular solve should only be applied to the top
    # part of B that contains the rhs. For square problems, the view corresponds to B itself
    ldiv!(LowerTriangular(adjoint(F.R)), view(B, 1:size(F.R, 2), :))
    lmul!(F.Q, B)

    return B
end

# With a real lhs and complex rhs with the same precision, we can reinterpret the complex
# rhs as a real rhs with twice the number of columns.

# convenience methods to compute the return size correctly for vectors and matrices
_ret_size(A::Factorization, b::AbstractVector) = (max(size(A, 2), length(b)),)
_ret_size(A::Factorization, B::AbstractMatrix) = (max(size(A, 2), size(B, 1)), size(B, 2))

function (\)(A::Union{QR{T},QRCompactWY{T},QRPivoted{T}}, BIn::VecOrMat{Complex{T}}) where T<:BlasReal
    require_one_based_indexing(BIn)
    m, n = size(A)
    m == size(BIn, 1) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has $(size(BIn,1)) rows"))

# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      ->       |y1|y2|y3|y4|     ->      |x2|y2|     ->    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    B = reshape(copy(transpose(reinterpret(T, reshape(BIn, (1, length(BIn)))))), size(BIn, 1), 2*size(BIn, 2))

    X = _zeros(T, B, n)
    X[1:size(B, 1), :] = B

    ldiv!(A, X)

# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      <-       |y1|y2|y3|y4|     <-      |x2|y2|     <-    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    XX = reshape(collect(reinterpret(Complex{T}, copy(transpose(reshape(X, div(length(X), 2), 2))))), _ret_size(A, BIn))
    return _cut_B(XX, 1:n)
end

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

# FIXME! Should add balancing option through xgebal
