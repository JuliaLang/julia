# This file is a part of Julia. License is MIT: https://julialang.org/license

# QR and Hessenberg Factorizations
"""
    QR <: Factorization

A QR matrix factorization stored in a packed format, typically obtained from
[`qrfact`](@ref). If ``A`` is an `m`×`n` matrix, then

```math
A = Q R
```

where ``Q`` is an orthogonal/unitary matrix and ``R`` is upper triangular.
The matrix ``Q`` is stored as a sequence of Householder reflectors ``v_i``
and coefficients ``\\tau_i`` where:

```math
Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T).
```

The object has two fields:

* `factors` is an `m`×`n` matrix.

  - The upper triangular part contains the elements of ``R``, that is `R =
    triu(F.factors)` for a `QR` object `F`.

  - The subdiagonal part contains the reflectors ``v_i`` stored in a packed format where
    ``v_i`` is the ``i``th column of the matrix `V = I + tril(F.factors, -1)`.

* `τ` is a vector  of length `min(m,n)` containing the coefficients ``\tau_i``.

"""
struct QR{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    QR{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end
QR(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = QR{T,typeof(factors)}(factors, τ)

# Note. For QRCompactWY factorization without pivoting, the WY representation based method introduced in LAPACK 3.4
"""
    QRCompactWY <: Factorization

A QR matrix factorization stored in a compact blocked format, typically obtained from
[`qrfact`](@ref). If ``A`` is an `m`×`n` matrix, then

```math
A = Q R
```

where ``Q`` is an orthogonal/unitary matrix and ``R`` is upper triangular. It is similar
to the [`QR`](@ref) format except that the orthogonal/unitary matrix ``Q`` is stored in
*Compact WY* format [^Schreiber1989], as a lower trapezoidal matrix ``V`` and an upper
triangular matrix ``T`` where

```math
Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T) = I - V T V^T
```

such that ``v_i`` is the ``i``th column of ``V``, and ``\tau_i`` is the ``i``th diagonal
element of ``T``.

The object has two fields:

* `factors`, as in the [`QR`](@ref) type, is an `m`×`n` matrix.

  - The upper triangular part contains the elements of ``R``, that is `R =
    triu(F.factors)` for a `QR` object `F`.

  - The subdiagonal part contains the reflectors ``v_i`` stored in a packed format such
    that `V = I + tril(F.factors, -1)`.

* `T` is a square matrix with `min(m,n)` columns, whose upper triangular part gives the
  matrix ``T`` above (the subdiagonal elements are ignored).

!!! note

    This format should not to be confused with the older *WY* representation
    [^Bischof1987].


[^Bischof1987]: C Bischof and C Van Loan, "The WY representation for products of Householder matrices", SIAM J Sci Stat Comput 8 (1987), s2-s13. [doi:10.1137/0908009](https://doi.org/10.1137/0908009)

[^Schreiber1989]: R Schreiber and C Van Loan, "A storage-efficient WY representation for products of Householder transformations", SIAM J Sci Stat Comput 10 (1989), 53-57. [doi:10.1137/0910005](https://doi.org/10.1137/0910005)
"""
struct QRCompactWY{S,M<:AbstractMatrix} <: Factorization{S}
    factors::M
    T::Matrix{S}
    QRCompactWY{S,M}(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where {S,M<:AbstractMatrix} = new(factors, T)
end
QRCompactWY(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where {S} = QRCompactWY{S,typeof(factors)}(factors, T)

"""
    QRPivoted <: Factorization

A QR matrix factorization with column pivoting in a packed format, typically obtained from
[`qrfact`](@ref). If ``A`` is an `m`×`n` matrix, then

```math
A P = Q R
```

where ``P`` is a permutation matrix, ``Q`` is an orthogonal/unitary matrix and ``R`` is
upper triangular. The matrix ``Q`` is stored as a sequence of Householder reflectors:

```math
Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T).
```

The object has three fields:

* `factors` is an `m`×`n` matrix.

  - The upper triangular part contains the elements of ``R``, that is `R =
    triu(F.factors)` for a `QR` object `F`.

  - The subdiagonal part contains the reflectors ``v_i`` stored in a packed format where
    ``v_i`` is the ``i``th column of the matrix `V = I + tril(F.factors, -1)`.

* `τ` is a vector of length `min(m,n)` containing the coefficients ``\tau_i``.

* `jpvt` is an integer vector of length `n` corresponding to the permutation ``P``.
"""
struct QRPivoted{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    jpvt::Vector{BlasInt}
    QRPivoted{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}, jpvt::Vector{BlasInt}) where {T,S<:AbstractMatrix} =
        new(factors, τ, jpvt)
end
QRPivoted(factors::AbstractMatrix{T}, τ::Vector{T}, jpvt::Vector{BlasInt}) where {T} =
    QRPivoted{T,typeof(factors)}(factors, τ, jpvt)

function qrfactUnblocked!(A::AbstractMatrix{T}) where {T}
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
qrfact!(A::StridedMatrix{<:BlasFloat}, ::Val{false}) = QRCompactWY(LAPACK.geqrt!(A, min(min(size(A)...), 36))...)
qrfact!(A::StridedMatrix{<:BlasFloat}, ::Val{true}) = QRPivoted(LAPACK.geqp3!(A)...)
qrfact!(A::StridedMatrix{<:BlasFloat}) = qrfact!(A, Val(false))

# Generic fallbacks

"""
    qrfact!(A, pivot=Val(false))

`qrfact!` is the same as [`qrfact`](@ref) when `A` is a subtype of
`StridedMatrix`, but saves space by overwriting the input `A`, instead of creating a copy.
An [`InexactError`](@ref) exception is thrown if the factorization produces a number not
representable by the element type of `A`, e.g. for integer types.

# Examples
```jldoctest
julia> a = [1. 2.; 3. 4.]
2×2 Array{Float64,2}:
 1.0  2.0
 3.0  4.0

julia> qrfact!(a)
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

julia> qrfact!(a)
ERROR: InexactError: Int64(Int64, -3.1622776601683795)
Stacktrace:
[...]
```
"""
qrfact!(A::StridedMatrix, ::Val{false}) = qrfactUnblocked!(A)
qrfact!(A::StridedMatrix, ::Val{true}) = qrfactPivotedUnblocked!(A)
qrfact!(A::StridedMatrix) = qrfact!(A, Val(false))

_qreltype(::Type{T}) where T = typeof(zero(T)/sqrt(abs2(one(T))))

"""
    qrfact(A, pivot=Val(false)) -> F

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

The individual components of the factorization `F` can be accessed by indexing with a symbol:

 - `F.Q`: the orthogonal/unitary matrix `Q`
 - `F.R`: the upper triangular matrix `R`
 - `F.p`: the permutation vector of the pivot ([`QRPivoted`](@ref) only)
 - `F.P`: the permutation matrix of the pivot ([`QRPivoted`](@ref) only)

The following functions are available for the `QR` objects: [`inv`](@ref), [`size`](@ref),
and [`\\`](@ref). When `A` is rectangular, `\\` will return a least squares
solution and if the solution is not unique, the one with smallest norm is returned.

Multiplication with respect to either full/square or non-full/square `Q` is allowed, i.e. both `F.Q*F.R`
and `F.Q*A` are supported. A `Q` matrix can be converted into a regular matrix with
[`Matrix`](@ref).

# Examples
```jldoctest
julia> A = [3.0 -6.0; 4.0 -8.0; 0.0 1.0]
3×2 Array{Float64,2}:
 3.0  -6.0
 4.0  -8.0
 0.0   1.0

julia> F = qrfact(A)
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
    `qrfact` returns multiple types because LAPACK uses several representations
    that minimize the memory storage requirements of products of Householder
    elementary reflectors, so that the `Q` and `R` matrices can be stored
    compactly rather as two separate dense matrices.
"""
function qrfact(A::AbstractMatrix{T}, arg) where T
    AA = similar(A, _qreltype(T), size(A))
    copyto!(AA, A)
    return qrfact!(AA, arg)
end
function qrfact(A::AbstractMatrix{T}) where T
    AA = similar(A, _qreltype(T), size(A))
    copyto!(AA, A)
    return qrfact!(AA)
end
qrfact(x::Number) = qrfact(fill(x,1,1))

"""
    qr(A, pivot=Val(false); full::Bool = false) -> Q, R, [p]

Compute the (pivoted) QR factorization of `A` such that either `A = Q*R` or `A[:,p] = Q*R`.
Also see [`qrfact`](@ref).
The default is to compute a "thin" factorization. Note that `R` is not
extended with zeros when a full/square orthogonal factor `Q` is requested (via `full = true`).
"""
function qr(A::Union{Number,AbstractMatrix}, pivot::Union{Val{false},Val{true}} = Val(false);
            full::Bool = false, thin::Union{Bool,Nothing} = nothing)
    # DEPRECATION TODO: remove deprecated thin argument and associated logic after 0.7
    if thin != nothing
        Base.depwarn(string("the `thin` keyword argument in `qr(A, pivot; thin = $(thin))` has ",
            "been deprecated in favor of `full`, which has the opposite meaning, ",
            "e.g. `qr(A, pivot; full = $(!thin))`."), :qr)
        full::Bool = !thin
    end
    return _qr(A, pivot, full = full)
end
function _qr(A::Union{Number,AbstractMatrix}, ::Val{false}; full::Bool = false)
    F = qrfact(A, Val(false))
    Q, R = F.Q, F.R
    sQf1 = size(Q.factors, 1)
    return (!full ? Array(Q) : lmul!(Q, Matrix{eltype(Q)}(I, sQf1, sQf1))), R
end
function _qr(A::Union{Number, AbstractMatrix}, ::Val{true}; full::Bool = false)
    F = qrfact(A, Val(true))
    Q, R, p = F.Q, F.R, F.p
    sQf1 = size(Q.factors, 1)
    return (!full ? Array(Q) : lmul!(Q, Matrix{eltype(Q)}(I, sQf1, sQf1))), R, p
end

"""
    qr(v::AbstractVector) -> w, r

Computes the polar decomposition of a vector.
Returns `w`, a unit vector in the direction of `v`, and
`r`, the norm of `v`.

See also [`normalize`](@ref), [`normalize!`](@ref),
and [`LinearAlgebra.qr!`](@ref).

# Examples
```jldoctest
julia> v = [1; 2]
2-element Array{Int64,1}:
 1
 2

julia> w, r = qr(v)
([0.447214, 0.894427], 2.23606797749979)

julia> w*r == v
true
```
"""
function qr(v::AbstractVector)
    nrm = norm(v)
    if !isempty(v)
        vv = copy_oftype(v, typeof(v[1]/nrm))
        return __normalize!(vv, nrm), nrm
    else
        T = typeof(zero(eltype(v))/nrm)
        return T[], oneunit(T)
    end
end

"""
    LinearAlgebra.qr!(v::AbstractVector) -> w, r

Computes the polar decomposition of a vector. Instead of returning a new vector
as `qr(v::AbstractVector)`, this function mutates the input vector `v` in place.
Returns `w`, a unit vector in the direction of `v` (this is a mutation of `v`),
and `r`, the norm of `v`.

See also [`normalize`](@ref), [`normalize!`](@ref),
and [`qr`](@ref).

# Examples
```jldoctest
julia> v = [1.; 2.]
2-element Array{Float64,1}:
 1.0
 2.0

julia> w, r = LinearAlgebra.qr!(v)
([0.447214, 0.894427], 2.23606797749979)

julia> w === v
true
```
"""
function qr!(v::AbstractVector)
    nrm = norm(v)
    __normalize!(v, nrm), nrm
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
    println(io, summary(F))
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
    if d == :R
        return triu!(getfield(F, :factors)[1:min(m,n), 1:n])
    elseif d == :Q
        return QRPackedQ(getfield(F, :factors), F.τ)
    else
        getfield(F, d)
    end
end
function getproperty(F::QRCompactWY, d::Symbol)
    m, n = size(F)
    if d == :R
        return triu!(getfield(F, :factors)[1:min(m,n), 1:n])
    elseif d == :Q
        return QRCompactWYQ(getfield(F, :factors), F.T)
    else
        getfield(F, d)
    end
end
Base.propertynames(F::Union{QR,QRCompactWY}, private::Bool=false) =
    (:R, :Q, (private ? fieldnames(typeof(F)) : ())...)

function getproperty(F::QRPivoted{T}, d::Symbol) where T
    m, n = size(F)
    if d == :R
        return triu!(getfield(F, :factors)[1:min(m,n), 1:n])
    elseif d == :Q
        return QRPackedQ(getfield(F, :factors), F.τ)
    elseif d == :p
        return getfield(F, :jpvt)
    elseif d == :P
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

"""
    QRPackedQ <: AbstractMatrix

The orthogonal/unitary ``Q`` matrix of a QR factorization stored in [`QR`](@ref) or
[`QRPivoted`](@ref) format.
"""
struct QRPackedQ{T,S<:AbstractMatrix} <: AbstractQ{T}
    factors::S
    τ::Vector{T}
    QRPackedQ{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end
QRPackedQ(factors::AbstractMatrix{T}, τ::Vector{T}) where {T} = QRPackedQ{T,typeof(factors)}(factors, τ)

"""
    QRCompactWYQ <: AbstractMatrix

The orthogonal/unitary ``Q`` matrix of a QR factorization stored in [`QRCompactWY`](@ref)
format.
"""
struct QRCompactWYQ{S, M<:AbstractMatrix} <: AbstractQ{S}
    factors::M
    T::Matrix{S}
    QRCompactWYQ{S,M}(factors::AbstractMatrix{S}, T::Matrix{S}) where {S,M<:AbstractMatrix} = new(factors, T)
end
QRCompactWYQ(factors::AbstractMatrix{S}, T::Matrix{S}) where {S} = QRCompactWYQ{S,typeof(factors)}(factors, T)

QRPackedQ{T}(Q::QRPackedQ) where {T} = QRPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
AbstractMatrix{T}(Q::QRPackedQ{T}) where {T} = Q
AbstractMatrix{T}(Q::QRPackedQ) where {T} = QRPackedQ{T}(Q)
QRCompactWYQ{S}(Q::QRCompactWYQ) where {S} = QRCompactWYQ(convert(AbstractMatrix{S}, Q.factors), convert(AbstractMatrix{S}, Q.T))
AbstractMatrix{S}(Q::QRCompactWYQ{S}) where {S} = Q
AbstractMatrix{S}(Q::QRCompactWYQ) where {S} = QRCompactWYQ{S}(Q)
Matrix(A::AbstractQ{T}) where {T} = lmul!(A, Matrix{T}(I, size(A.factors, 1), min(size(A.factors)...)))
Array(A::AbstractQ) = Matrix(A)

size(A::Union{QR,QRCompactWY,QRPivoted}, dim::Integer) = size(getfield(A, :factors), dim)
size(A::Union{QR,QRCompactWY,QRPivoted}) = size(getfield(A, :factors))
size(A::AbstractQ, dim::Integer) = 0 < dim ? (dim <= 2 ? size(getfield(A, :factors), 1) : 1) : throw(BoundsError())
size(A::AbstractQ) = size(A, 1), size(A, 2)


function getindex(A::AbstractQ, i::Integer, j::Integer)
    x = zeros(eltype(A), size(A, 1))
    x[i] = 1
    y = zeros(eltype(A), size(A, 2))
    y[j] = 1
    return dot(x, lmul!(A, y))
end

## Multiplication by Q
### QB
lmul!(A::QRCompactWYQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix} =
    LAPACK.gemqrt!('L','N',A.factors,A.T,B)
lmul!(A::QRPackedQ{T,S}, B::StridedVecOrMat{T}) where {T<:BlasFloat, S<:StridedMatrix} =
    LAPACK.ormqr!('L','N',A.factors,A.τ,B)
function lmul!(A::QRPackedQ, B::AbstractVecOrMat)
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
        B[1:nA, :] = 0
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
    B[rnk+1:end,:] = zero(T)
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
            B[m + 1:mB,1:nB] = zero(T)
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
    S = promote_type(TA,TB)
    m, n = size(A)
    m == size(B,1) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has $(size(B,1)) rows"))

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
