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
struct QR{T,S<:AbstractMatrix{T}} <: Factorization{T}
    factors::S
    τ::Vector{T}

    function QR{T,S}(factors, τ) where {T,S<:AbstractMatrix{T}}
        require_one_based_indexing(factors)
        new{T,S}(factors, τ)
    end
end
QR(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = QR{T,typeof(factors)}(factors, τ)
function QR{T}(factors::AbstractMatrix, τ::AbstractVector) where {T}
    QR(convert(AbstractMatrix{T}, factors), convert(Vector{T}, τ))
end

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
struct QRCompactWY{S,M<:AbstractMatrix{S}} <: Factorization{S}
    factors::M
    T::Matrix{S}

    function QRCompactWY{S,M}(factors, T) where {S,M<:AbstractMatrix{S}}
        require_one_based_indexing(factors)
        new{S,M}(factors, T)
    end
end
QRCompactWY(factors::AbstractMatrix{S}, T::Matrix{S}) where {S} = QRCompactWY{S,typeof(factors)}(factors, T)
function QRCompactWY{S}(factors::AbstractMatrix, T::AbstractMatrix) where {S}
    QRCompactWY(convert(AbstractMatrix{S}, factors), convert(Matrix{S}, T))
end

# iteration for destructuring into components
Base.iterate(S::QRCompactWY) = (S.Q, Val(:R))
Base.iterate(S::QRCompactWY, ::Val{:R}) = (S.R, Val(:done))
Base.iterate(S::QRCompactWY, ::Val{:done}) = nothing

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
struct QRPivoted{T,S<:AbstractMatrix{T}} <: Factorization{T}
    factors::S
    τ::Vector{T}
    jpvt::Vector{BlasInt}

    function QRPivoted{T,S}(factors, τ, jpvt) where {T,S<:AbstractMatrix{T}}
        require_one_based_indexing(factors, τ, jpvt)
        new{T,S}(factors, τ, jpvt)
    end
end
QRPivoted(factors::AbstractMatrix{T}, τ::Vector{T}, jpvt::Vector{BlasInt}) where {T} =
    QRPivoted{T,typeof(factors)}(factors, τ, jpvt)
function QRPivoted{T}(factors::AbstractMatrix, τ::AbstractVector, jpvt::AbstractVector) where {T}
    QRPivoted(convert(AbstractMatrix{T}, factors),
              convert(Vector{T}, τ),
              convert(Vector{BlasInt}, jpvt))
end

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
function indmaxcolumn(A::StridedMatrix)
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

function qrfactPivotedUnblocked!(A::StridedMatrix)
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
        τj = LinearAlgebra.reflector!(x)
        τ[j] = τj

        # Update trailing submatrix with reflector
        LinearAlgebra.reflectorApply!(x, τj, view(A, j:m, j+1:n))
    end
    return LinearAlgebra.QRPivoted{eltype(A), typeof(A)}(A, τ, piv)
end

# LAPACK version
qr!(A::StridedMatrix{<:BlasFloat}, ::Val{false} = Val(false); blocksize=36) =
    QRCompactWY(LAPACK.geqrt!(A, min(min(size(A)...), blocksize))...)
qr!(A::StridedMatrix{<:BlasFloat}, ::Val{true}) = QRPivoted(LAPACK.geqp3!(A)...)

# Generic fallbacks

"""
    qr!(A, pivot=Val(false); blocksize)

`qr!` is the same as [`qr`](@ref) when `A` is a subtype of
[`StridedMatrix`](@ref), but saves space by overwriting the input `A`, instead of creating a copy.
An [`InexactError`](@ref) exception is thrown if the factorization produces a number not
representable by the element type of `A`, e.g. for integer types.

!!! compat "Julia 1.4"
    The `blocksize` keyword argument requires Julia 1.4 or later.

# Examples
```jldoctest
julia> a = [1. 2.; 3. 4.]
2×2 Array{Float64,2}:
 1.0  2.0
 3.0  4.0

julia> qr!(a)
LinearAlgebra.QRCompactWY{Float64,Array{Float64,2}}
Q factor:
2×2 LinearAlgebra.QRCompactWYQ{Float64,Array{Float64,2}}:
 -0.316228  -0.948683
 -0.948683   0.316228
R factor:
2×2 Array{Float64,2}:
 -3.16228  -4.42719
  0.0      -0.632456

julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> qr!(a)
ERROR: InexactError: Int64(-3.1622776601683795)
Stacktrace:
[...]
```
"""
qr!(A::StridedMatrix, ::Val{false}) = qrfactUnblocked!(A)
qr!(A::StridedMatrix, ::Val{true}) = qrfactPivotedUnblocked!(A)
qr!(A::StridedMatrix) = qr!(A, Val(false))

_qreltype(::Type{T}) where T = typeof(zero(T)/sqrt(abs2(one(T))))

"""
    qr(A, pivot=Val(false); blocksize) -> F

Compute the QR factorization of the matrix `A`: an orthogonal (or unitary if `A` is
complex-valued) matrix `Q`, and an upper triangular matrix `R` such that

```math
A = Q R
```

The returned object `F` stores the factorization in a packed format:

 - if `pivot == Val(true)` then `F` is a [`QRPivoted`](@ref) object,

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
[`Matrix`](@ref).  This operation returns the "thin" Q factor, i.e., if `A` is `m`×`n` with `m>=n`, then
`Matrix(F.Q)` yields an `m`×`n` matrix with orthonormal columns.  To retrieve the "full" Q factor, an
`m`×`m` orthogonal matrix, use `F.Q*Matrix(I,m,m)`.  If `m<=n`, then `Matrix(F.Q)` yields an `m`×`m`
orthogonal matrix.

The block size for QR decomposition can be specified by keyword argument
`blocksize :: Integer` when `pivot == Val(false)` and `A isa StridedMatrix{<:BlasFloat}`.
It is ignored when `blocksize > minimum(size(A))`.  See [`QRCompactWY`](@ref).

!!! compat "Julia 1.4"
    The `blocksize` keyword argument requires Julia 1.4 or later.

# Examples
```jldoctest
julia> A = [3.0 -6.0; 4.0 -8.0; 0.0 1.0]
3×2 Array{Float64,2}:
 3.0  -6.0
 4.0  -8.0
 0.0   1.0

julia> F = qr(A)
LinearAlgebra.QRCompactWY{Float64,Array{Float64,2}}
Q factor:
3×3 LinearAlgebra.QRCompactWYQ{Float64,Array{Float64,2}}:
 -0.6   0.0   0.8
 -0.8   0.0  -0.6
  0.0  -1.0   0.0
R factor:
2×2 Array{Float64,2}:
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
    AA = similar(A, _qreltype(T), size(A))
    copyto!(AA, A)
    return qr!(AA, arg...; kwargs...)
end
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

abstract type AbstractQ{T} <: AbstractMatrix{T} end

inv(Q::AbstractQ) = Q'

"""
    QRPackedQ <: AbstractMatrix

The orthogonal/unitary ``Q`` matrix of a QR factorization stored in [`QR`](@ref) or
[`QRPivoted`](@ref) format.
"""
struct QRPackedQ{T,S<:AbstractMatrix{T}} <: AbstractQ{T}
    factors::S
    τ::Vector{T}

    function QRPackedQ{T,S}(factors, τ) where {T,S<:AbstractMatrix{T}}
        require_one_based_indexing(factors)
        new{T,S}(factors, τ)
    end
end
QRPackedQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = QRPackedQ{T,typeof(factors)}(factors, τ)
function QRPackedQ{T}(factors::AbstractMatrix, τ::AbstractVector) where {T}
    QRPackedQ(convert(AbstractMatrix{T}, factors), convert(Vector{T}, τ))
end

"""
    QRCompactWYQ <: AbstractMatrix

The orthogonal/unitary ``Q`` matrix of a QR factorization stored in [`QRCompactWY`](@ref)
format.
"""
struct QRCompactWYQ{S, M<:AbstractMatrix{S}} <: AbstractQ{S}
    factors::M
    T::Matrix{S}

    function QRCompactWYQ{S,M}(factors, T) where {S,M<:AbstractMatrix{S}}
        require_one_based_indexing(factors)
        new{S,M}(factors, T)
    end
end
QRCompactWYQ(factors::AbstractMatrix{S}, T::Matrix{S}) where {S} = QRCompactWYQ{S,typeof(factors)}(factors, T)
function QRCompactWYQ{S}(factors::AbstractMatrix, T::AbstractMatrix) where {S}
    QRCompactWYQ(convert(AbstractMatrix{S}, factors), convert(Matrix{S}, T))
end

QRPackedQ{T}(Q::QRPackedQ) where {T} = QRPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
AbstractMatrix{T}(Q::QRPackedQ{T}) where {T} = Q
AbstractMatrix{T}(Q::QRPackedQ) where {T} = QRPackedQ{T}(Q)
QRCompactWYQ{S}(Q::QRCompactWYQ) where {S} = QRCompactWYQ(convert(AbstractMatrix{S}, Q.factors), convert(AbstractMatrix{S}, Q.T))
AbstractMatrix{S}(Q::QRCompactWYQ{S}) where {S} = Q
AbstractMatrix{S}(Q::QRCompactWYQ) where {S} = QRCompactWYQ{S}(Q)
Matrix{T}(Q::AbstractQ{S}) where {T,S} = Matrix{T}(lmul!(Q, Matrix{S}(I, size(Q, 1), min(size(Q.factors)...))))
Matrix(Q::AbstractQ{T}) where {T} = Matrix{T}(Q)
Array{T}(Q::AbstractQ) where {T} = Matrix{T}(Q)
Array(Q::AbstractQ) = Matrix(Q)

size(F::Union{QR,QRCompactWY,QRPivoted}, dim::Integer) = size(getfield(F, :factors), dim)
size(F::Union{QR,QRCompactWY,QRPivoted}) = size(getfield(F, :factors))
size(Q::AbstractQ, dim::Integer) = size(getfield(Q, :factors), dim == 2 ? 1 : dim)
size(Q::AbstractQ) = size(Q, 1), size(Q, 2)

function getindex(Q::AbstractQ, i::Integer, j::Integer)
    x = zeros(eltype(Q), size(Q, 1))
    x[i] = 1
    y = zeros(eltype(Q), size(Q, 2))
    y[j] = 1
    return dot(x, lmul!(Q, y))
end

## Multiplication by Q
### QB
lmul!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix} =
    LAPACK.gemqrt!('L','N',A.factors,A.T,B)
lmul!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix} =
    LAPACK.ormqr!('L','N',A.factors,A.τ,B)
function lmul!(A::QRPackedQ, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    mA, nA = size(A.factors)
    mB, nB = size(B,1), size(B,2)
    if mA != mB
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but B has dimensions ($mB, $nB)"))
    end
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

function (*)(A::AbstractQ, b::StridedVector)
    TAb = promote_type(eltype(A), eltype(b))
    Anew = convert(AbstractMatrix{TAb}, A)
    if size(A.factors, 1) == length(b)
        bnew = copy_oftype(b, TAb)
    elseif size(A.factors, 2) == length(b)
        bnew = [b; zeros(TAb, size(A.factors, 1) - length(b))]
    else
        throw(DimensionMismatch("vector must have length either $(size(A.factors, 1)) or $(size(A.factors, 2))"))
    end
    lmul!(Anew, bnew)
end
function (*)(A::AbstractQ, B::StridedMatrix)
    TAB = promote_type(eltype(A), eltype(B))
    Anew = convert(AbstractMatrix{TAB}, A)
    if size(A.factors, 1) == size(B, 1)
        Bnew = copy_oftype(B, TAB)
    elseif size(A.factors, 2) == size(B, 1)
        Bnew = [B; zeros(TAB, size(A.factors, 1) - size(B,1), size(B, 2))]
    else
        throw(DimensionMismatch("first dimension of matrix must have size either $(size(A.factors, 1)) or $(size(A.factors, 2))"))
    end
    lmul!(Anew, Bnew)
end

### QcB
lmul!(adjA::Adjoint{<:Any,<:QRCompactWYQ{T,S}}, B::StridedVecOrMat{T}) where {T<:BlasReal,S<:StridedMatrix} =
    (A = adjA.parent; LAPACK.gemqrt!('L','T',A.factors,A.T,B))
lmul!(adjA::Adjoint{<:Any,<:QRCompactWYQ{T,S}}, B::StridedVecOrMat{T}) where {T<:BlasComplex,S<:StridedMatrix} =
    (A = adjA.parent; LAPACK.gemqrt!('L','C',A.factors,A.T,B))
lmul!(adjA::Adjoint{<:Any,<:QRPackedQ{T,S}}, B::StridedVecOrMat{T}) where {T<:BlasReal,S<:StridedMatrix} =
    (A = adjA.parent; LAPACK.ormqr!('L','T',A.factors,A.τ,B))
lmul!(adjA::Adjoint{<:Any,<:QRPackedQ{T,S}}, B::StridedVecOrMat{T}) where {T<:BlasComplex,S<:StridedMatrix} =
    (A = adjA.parent; LAPACK.ormqr!('L','C',A.factors,A.τ,B))
function lmul!(adjA::Adjoint{<:Any,<:QRPackedQ}, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    A = adjA.parent
    mA, nA = size(A.factors)
    mB, nB = size(B,1), size(B,2)
    if mA != mB
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but B has dimensions ($mB, $nB)"))
    end
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
function *(adjQ::Adjoint{<:Any,<:AbstractQ}, B::StridedVecOrMat)
    Q = adjQ.parent
    TQB = promote_type(eltype(Q), eltype(B))
    return lmul!(adjoint(convert(AbstractMatrix{TQB}, Q)), copy_oftype(B, TQB))
end

### QBc/QcBc
function *(Q::AbstractQ, adjB::Adjoint{<:Any,<:StridedVecOrMat})
    B = adjB.parent
    TQB = promote_type(eltype(Q), eltype(B))
    Bc = similar(B, TQB, (size(B, 2), size(B, 1)))
    adjoint!(Bc, B)
    return lmul!(convert(AbstractMatrix{TQB}, Q), Bc)
end
function *(adjQ::Adjoint{<:Any,<:AbstractQ}, adjB::Adjoint{<:Any,<:StridedVecOrMat})
    Q, B = adjQ.parent, adjB.parent
    TQB = promote_type(eltype(Q), eltype(B))
    Bc = similar(B, TQB, (size(B, 2), size(B, 1)))
    adjoint!(Bc, B)
    return lmul!(adjoint(convert(AbstractMatrix{TQB}, Q)), Bc)
end

### AQ
rmul!(A::StridedVecOrMat{T}, B::QRCompactWYQ{T,S}) where {T<:BlasFloat,S<:StridedMatrix} =
    LAPACK.gemqrt!('R','N', B.factors, B.T, A)
rmul!(A::StridedVecOrMat{T}, B::QRPackedQ{T,S}) where {T<:BlasFloat,S<:StridedMatrix} =
    LAPACK.ormqr!('R', 'N', B.factors, B.τ, A)
function rmul!(A::StridedMatrix,Q::QRPackedQ)
    mQ, nQ = size(Q.factors)
    mA, nA = size(A,1), size(A,2)
    if nA != mQ
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but matrix Q has dimensions ($mQ, $nQ)"))
    end
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

function (*)(A::StridedMatrix, Q::AbstractQ)
    TAQ = promote_type(eltype(A), eltype(Q))

    return rmul!(copy_oftype(A, TAQ), convert(AbstractMatrix{TAQ}, Q))
end

### AQc
rmul!(A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:QRCompactWYQ{T}}) where {T<:BlasReal} =
    (B = adjB.parent; LAPACK.gemqrt!('R','T',B.factors,B.T,A))
rmul!(A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:QRCompactWYQ{T}}) where {T<:BlasComplex} =
    (B = adjB.parent; LAPACK.gemqrt!('R','C',B.factors,B.T,A))
rmul!(A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:QRPackedQ{T}}) where {T<:BlasReal} =
    (B = adjB.parent; LAPACK.ormqr!('R','T',B.factors,B.τ,A))
rmul!(A::StridedVecOrMat{T}, adjB::Adjoint{<:Any,<:QRPackedQ{T}}) where {T<:BlasComplex} =
    (B = adjB.parent; LAPACK.ormqr!('R','C',B.factors,B.τ,A))
function rmul!(A::StridedMatrix, adjQ::Adjoint{<:Any,<:QRPackedQ})
    Q = adjQ.parent
    mQ, nQ = size(Q.factors)
    mA, nA = size(A,1), size(A,2)
    if nA != mQ
        throw(DimensionMismatch("matrix A has dimensions ($mA,$nA) but matrix Q has dimensions ($mQ, $nQ)"))
    end
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
function *(A::StridedMatrix, adjB::Adjoint{<:Any,<:AbstractQ})
    B = adjB.parent
    TAB = promote_type(eltype(A),eltype(B))
    BB = convert(AbstractMatrix{TAB}, B)
    if size(A,2) == size(B.factors, 1)
        AA = similar(A, TAB, size(A))
        copyto!(AA, A)
        return rmul!(AA, adjoint(BB))
    elseif size(A,2) == size(B.factors,2)
        return rmul!([A zeros(TAB, size(A, 1), size(B.factors, 1) - size(B.factors, 2))], adjoint(BB))
    else
        throw(DimensionMismatch("matrix A has dimensions $(size(A)) but matrix B has dimensions $(size(B))"))
    end
end
*(u::AdjointAbsVec, A::Adjoint{<:Any,<:AbstractQ}) = adjoint(A.parent * u.parent)


### AcQ/AcQc
function *(adjA::Adjoint{<:Any,<:StridedVecOrMat}, Q::AbstractQ)
    A = adjA.parent
    TAQ = promote_type(eltype(A), eltype(Q))
    Ac = similar(A, TAQ, (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    return rmul!(Ac, convert(AbstractMatrix{TAQ}, Q))
end
function *(adjA::Adjoint{<:Any,<:StridedVecOrMat}, adjQ::Adjoint{<:Any,<:AbstractQ})
    A, Q = adjA.parent, adjQ.parent
    TAQ = promote_type(eltype(A), eltype(Q))
    Ac = similar(A, TAQ, (size(A, 2), size(A, 1)))
    adjoint!(Ac, A)
    return rmul!(Ac, adjoint(convert(AbstractMatrix{TAQ}, Q)))
end

### mul!
mul!(C::StridedVecOrMat{T}, Q::AbstractQ{T}, B::StridedVecOrMat{T}) where {T} = lmul!(Q, copyto!(C, B))
mul!(C::StridedVecOrMat{T}, A::StridedVecOrMat{T}, Q::AbstractQ{T}) where {T} = rmul!(copyto!(C, A), Q)
mul!(C::StridedVecOrMat{T}, adjQ::Adjoint{<:Any,<:AbstractQ{T}}, B::StridedVecOrMat{T}) where {T} = lmul!(adjQ, copyto!(C, B))
mul!(C::StridedVecOrMat{T}, A::StridedVecOrMat{T}, adjQ::Adjoint{<:Any,<:AbstractQ{T}}) where {T} = rmul!(copyto!(C, A), adjQ)

ldiv!(A::QRCompactWY{T}, b::StridedVector{T}) where {T<:BlasFloat} =
    (ldiv!(UpperTriangular(A.R), view(lmul!(adjoint(A.Q), b), 1:size(A, 2))); b)
ldiv!(A::QRCompactWY{T}, B::StridedMatrix{T}) where {T<:BlasFloat} =
    (ldiv!(UpperTriangular(A.R), view(lmul!(adjoint(A.Q), B), 1:size(A, 2), 1:size(B, 2))); B)

# Julia implementation similar to xgelsy
function ldiv!(A::QRPivoted{T}, B::StridedMatrix{T}, rcond::Real) where T<:BlasFloat
    mA, nA = size(A.factors)
    nr = min(mA,nA)
    nrhs = size(B, 2)
    if nr == 0
        return B, 0
    end
    ar = abs(A.factors[1])
    if ar == 0
        B[1:nA, :] .= 0
        return B, 0
    end
    rnk = 1
    xmin = T[1]
    xmax = T[1]
    tmin = tmax = ar
    while rnk < nr
        tmin, smin, cmin = LAPACK.laic1!(2, xmin, tmin, view(A.factors, 1:rnk, rnk + 1), A.factors[rnk + 1, rnk + 1])
        tmax, smax, cmax = LAPACK.laic1!(1, xmax, tmax, view(A.factors, 1:rnk, rnk + 1), A.factors[rnk + 1, rnk + 1])
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
    ldiv!(UpperTriangular(C[1:rnk,1:rnk]),view(lmul!(adjoint(A.Q), view(B, 1:mA, 1:nrhs)), 1:rnk, 1:nrhs))
    B[rnk+1:end,:] .= zero(T)
    LAPACK.ormrz!('L', eltype(B)<:Complex ? 'C' : 'T', C, τ, view(B,1:nA,1:nrhs))
    B[1:nA,:] = view(B, 1:nA, :)[invperm(A.p),:]
    return B, rnk
end
ldiv!(A::QRPivoted{T}, B::StridedVector{T}) where {T<:BlasFloat} =
    vec(ldiv!(A,reshape(B,length(B),1)))
ldiv!(A::QRPivoted{T}, B::StridedVecOrMat{T}) where {T<:BlasFloat} =
    ldiv!(A, B, min(size(A)...)*eps(real(float(one(eltype(B))))))[1]
function ldiv!(A::QR{T}, B::StridedMatrix{T}) where T
    m, n = size(A)
    minmn = min(m,n)
    mB, nB = size(B)
    lmul!(adjoint(A.Q), view(B, 1:m, :))
    R = A.R
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
        LinearAlgebra.ldiv!(UpperTriangular(view(R, :, 1:minmn)), view(B, 1:minmn, :))
        if n > m # Apply elementary transformation to solution
            B[m + 1:mB,1:nB] .= zero(T)
            for j = 1:nB
                for k = 1:m
                    vBj = B[k,j]
                    for i = m + 1:n
                        vBj += B[i,j]*R[k,i]'
                    end
                    vBj *= τ[k]
                    B[k,j] -= vBj
                    for i = m + 1:n
                        B[i,j] -= R[k,i]*vBj
                    end
                end
            end
        end
    end
    return B
end
ldiv!(A::QR, B::StridedVector) = ldiv!(A, reshape(B, length(B), 1))[:]
function ldiv!(A::QRPivoted, b::StridedVector)
    ldiv!(QR(A.factors,A.τ), b)
    b[1:size(A.factors, 2)] = view(b, 1:size(A.factors, 2))[invperm(A.jpvt)]
    b
end
function ldiv!(A::QRPivoted, B::StridedMatrix)
    ldiv!(QR(A.factors, A.τ), B)
    B[1:size(A.factors, 2),:] = view(B, 1:size(A.factors, 2), :)[invperm(A.jpvt),:]
    B
end

# convenience methods
## return only the solution of a least squares problem while avoiding promoting
## vectors to matrices.
_cut_B(x::AbstractVector, r::UnitRange) = length(x)  > length(r) ? x[r]   : x
_cut_B(X::AbstractMatrix, r::UnitRange) = size(X, 1) > length(r) ? X[r,:] : X

## append right hand side with zeros if necessary
_zeros(::Type{T}, b::AbstractVector, n::Integer) where {T} = zeros(T, max(length(b), n))
_zeros(::Type{T}, B::AbstractMatrix, n::Integer) where {T} = zeros(T, max(size(B, 1), n), size(B, 2))

function (\)(A::Union{QR{TA},QRCompactWY{TA},QRPivoted{TA}}, B::AbstractVecOrMat{TB}) where {TA,TB}
    require_one_based_indexing(B)
    S = promote_type(TA,TB)
    m, n = size(A)
    m == size(B,1) || throw(DimensionMismatch("Both inputs should have the same number of rows"))

    AA = Factorization{S}(A)

    X = _zeros(S, B, n)
    X[1:size(B, 1), :] = B
    ldiv!(AA, X)
    return _cut_B(X, 1:n)
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


det(Q::QRPackedQ) = _det_tau(Q.τ)

det(Q::QRCompactWYQ) =
    prod(i -> _det_tau(_diagview(Q.T[:, i:min(i + size(Q.T, 1), size(Q.T, 2))])),
         1:size(Q.T, 1):size(Q.T, 2))

_diagview(A) = @view A[diagind(A)]

# Compute `det` from the number of Householder reflections.  Handle
# the case `Q.τ` contains zeros.
_det_tau(τs::AbstractVector{<:Real}) =
    isodd(count(!iszero, τs)) ? -one(eltype(τs)) : one(eltype(τs))

# In complex case, we need to compute the non-unit eigenvalue `λ = 1 - c*τ`
# (where `c = v'v`) of each Householder reflector.  As we know that the
# reflector must have the determinant of 1, it must satisfy `abs2(λ) == 1`.
# Combining this with the constraint `c > 0`, it turns out that the eigenvalue
# (hence the determinant) can be computed as `λ = -sign(τ)^2`.
# See: https://github.com/JuliaLang/julia/pull/32887#issuecomment-521935716
_det_tau(τs) = prod(τ -> iszero(τ) ? one(τ) : -sign(τ)^2, τs)
