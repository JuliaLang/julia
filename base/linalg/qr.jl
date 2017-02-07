# This file is a part of Julia. License is MIT: http://julialang.org/license

# QR and Hessenberg Factorizations

immutable QR{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    QR{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end
QR(factors::AbstractMatrix{T}, τ::Vector{T}) where T = QR{T,typeof(factors)}(factors, τ)
# Note. For QRCompactWY factorization without pivoting, the WY representation based method introduced in LAPACK 3.4
immutable QRCompactWY{S,M<:AbstractMatrix} <: Factorization{S}
    factors::M
    T::Matrix{S}
    QRCompactWY{S,M}(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where {S,M<:AbstractMatrix} = new(factors, T)
end
QRCompactWY(factors::AbstractMatrix{S}, T::AbstractMatrix{S}) where S = QRCompactWY{S,typeof(factors)}(factors, T)

immutable QRPivoted{T,S<:AbstractMatrix} <: Factorization{T}
    factors::S
    τ::Vector{T}
    jpvt::Vector{BlasInt}
    QRPivoted{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}, jpvt::Vector{BlasInt}) where {T,S<:AbstractMatrix} =
        new(factors, τ, jpvt)
end
QRPivoted(factors::AbstractMatrix{T}, τ::Vector{T}, jpvt::Vector{BlasInt}) where T =
    QRPivoted{T,typeof(factors)}(factors, τ, jpvt)

function qrfactUnblocked!{T}(A::AbstractMatrix{T})
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
    piv = collect(UnitRange{BlasInt}(1,n))
    τ = Array{eltype(A)}(min(m,n))
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
        τj = LinAlg.reflector!(x)
        τ[j] = τj

        # Update trailing submatrix with reflector
        LinAlg.reflectorApply!(x, τj, view(A, j:m, j+1:n))
    end
    return LinAlg.QRPivoted{eltype(A), typeof(A)}(A, τ, piv)
end

# LAPACK version
qrfact!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Val{false}}) = QRCompactWY(LAPACK.geqrt!(A, min(minimum(size(A)), 36))...)
qrfact!{T<:BlasFloat}(A::StridedMatrix{T}, ::Type{Val{true}}) = QRPivoted(LAPACK.geqp3!(A)...)
qrfact!{T<:BlasFloat}(A::StridedMatrix{T}) = qrfact!(A, Val{false})

# Generic fallbacks

"""
    qrfact!(A, pivot=Val{false})

`qrfact!` is the same as [`qrfact`](@ref) when `A` is a subtype of
`StridedMatrix`, but saves space by overwriting the input `A`, instead of creating a copy.
An [`InexactError`](@ref) exception is thrown if the factorization produces a number not
representable by the element type of `A`, e.g. for integer types.
"""
qrfact!(A::StridedMatrix, ::Type{Val{false}}) = qrfactUnblocked!(A)
qrfact!(A::StridedMatrix, ::Type{Val{true}}) = qrfactPivotedUnblocked!(A)
qrfact!(A::StridedMatrix) = qrfact!(A, Val{false})

"""
    qrfact(A, pivot=Val{false}) -> F

Computes the QR factorization of `A`. The return type of `F` depends on the element type of
`A` and whether pivoting is specified (with `pivot==Val{true}`).

| Return type   | `eltype(A)`     | `pivot`      | Relationship between `F` and `A` |
|:--------------|:----------------|:-------------|:---------------------------------|
| `QR`          | not `BlasFloat` | either       | `A==F[:Q]*F[:R]`                 |
| `QRCompactWY` | `BlasFloat`     | `Val{false}` | `A==F[:Q]*F[:R]`                 |
| `QRPivoted`   | `BlasFloat`     | `Val{true}`  | `A[:,F[:p]]==F[:Q]*F[:R]`        |

`BlasFloat` refers to any of: `Float32`, `Float64`, `Complex64` or `Complex128`.

The individual components of the factorization `F` can be accessed by indexing:

| Component | Description                               | `QR`            | `QRCompactWY`      | `QRPivoted`     |
|:----------|:------------------------------------------|:----------------|:-------------------|:----------------|
| `F[:Q]`   | `Q` (orthogonal/unitary) part of `QR`     | ✓ (`QRPackedQ`) | ✓ (`QRCompactWYQ`) | ✓ (`QRPackedQ`) |
| `F[:R]`   | `R` (upper right triangular) part of `QR` | ✓               | ✓                  | ✓               |
| `F[:p]`   | pivot `Vector`                            |                 |                    | ✓               |
| `F[:P]`   | (pivot) permutation `Matrix`              |                 |                    | ✓               |

The following functions are available for the `QR` objects: [`size`](@ref)
and [`\\`](@ref). When `A` is rectangular, `\\` will return a least squares
solution and if the solution is not unique, the one with smallest norm is returned.

Multiplication with respect to either thin or full `Q` is allowed, i.e. both `F[:Q]*F[:R]`
and `F[:Q]*A` are supported. A `Q` matrix can be converted into a regular matrix with
[`full`](@ref) which has a named argument `thin`.

```jldoctest
julia> A = [3.0 -6.0; 4.0 -8.0; 0.0 1.0]
3×2 Array{Float64,2}:
 3.0  -6.0
 4.0  -8.0
 0.0   1.0

julia> F = qrfact(A)
Base.LinAlg.QRCompactWY{Float64,Array{Float64,2}} with factors Q and R:
[-0.6 0.0 0.8; -0.8 0.0 -0.6; 0.0 -1.0 0.0]
[-5.0 10.0; 0.0 -1.0]

julia> F[:Q] * F[:R] == A
true
```

!!! note
    `qrfact` returns multiple types because LAPACK uses several representations
    that minimize the memory storage requirements of products of Householder
    elementary reflectors, so that the `Q` and `R` matrices can be stored
    compactly rather as two separate dense matrices.

    The data contained in `QR` or `QRPivoted` can be used to construct the
    `QRPackedQ` type, which is a compact representation of the rotation matrix:

    ```math
    Q = \\prod_{i=1}^{\\min(m,n)} (I - \\tau_i v_i v_i^T)
    ```

    where ``\\tau_i`` is the scale factor and ``v_i`` is the projection vector
    associated with the ``i^{th}`` Householder elementary reflector.

    The data contained in `QRCompactWY` can be used to construct the
    `QRCompactWYQ` type, which is a compact representation of the rotation
    matrix

    ```math
    Q = I + Y T Y^T
    ```

    where `Y` is ``m \\times r`` lower trapezoidal and `T` is ``r \\times r``
    upper triangular. The *compact WY* representation [^Schreiber1989] is not
    to be confused with the older, *WY* representation [^Bischof1987]. (The
    LAPACK documentation uses `V` in lieu of `Y`.)

    [^Bischof1987]: C Bischof and C Van Loan, "The WY representation for products of Householder matrices", SIAM J Sci Stat Comput 8 (1987), s2-s13. [doi:10.1137/0908009](http://dx.doi.org/10.1137/0908009)

    [^Schreiber1989]: R Schreiber and C Van Loan, "A storage-efficient WY representation for products of Householder transformations", SIAM J Sci Stat Comput 10 (1989), 53-57. [doi:10.1137/0910005](http://dx.doi.org/10.1137/0910005)
"""
function qrfact{T}(A::AbstractMatrix{T}, arg)
    AA = similar(A, typeof(zero(T)/norm(one(T))), size(A))
    copy!(AA, A)
    return qrfact!(AA, arg)
end
function qrfact{T}(A::AbstractMatrix{T})
    AA = similar(A, typeof(zero(T)/norm(one(T))), size(A))
    copy!(AA, A)
    return qrfact!(AA)
end
qrfact(x::Number) = qrfact(fill(x,1,1))

"""
    qr(A, pivot=Val{false}; thin::Bool=true) -> Q, R, [p]

Compute the (pivoted) QR factorization of `A` such that either `A = Q*R` or `A[:,p] = Q*R`.
Also see [`qrfact`](@ref).
The default is to compute a thin factorization. Note that `R` is not
extended with zeros when the full `Q` is requested.
"""
qr(A::Union{Number, AbstractMatrix}, pivot::Union{Type{Val{false}}, Type{Val{true}}}=Val{false}; thin::Bool=true) =
    _qr(A, pivot, thin=thin)
function _qr(A::Union{Number, AbstractMatrix}, ::Type{Val{false}}; thin::Bool=true)
    F = qrfact(A, Val{false})
    full(getq(F), thin=thin), F[:R]::Matrix{eltype(F)}
end
function _qr(A::Union{Number, AbstractMatrix}, ::Type{Val{true}}; thin::Bool=true)
    F = qrfact(A, Val{true})
    full(getq(F), thin=thin), F[:R]::Matrix{eltype(F)}, F[:p]::Vector{BlasInt}
end

"""
    qr(v::AbstractVector) -> w, r

Computes the polar decomposition of a vector.
Returns `w`, a unit vector in the direction of `v`, and
`r`, the norm of `v`.

See also [`normalize`](@ref), [`normalize!`](@ref),
and [`LinAlg.qr!`](@ref).

# Example

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
    LinAlg.qr!(v::AbstractVector) -> w, r

Computes the polar decomposition of a vector. Instead of returning a new vector
as `qr(v::AbstractVector)`, this function mutates the input vector `v` in place.
Returns `w`, a unit vector in the direction of `v` (this is a mutation of `v`),
and `r`, the norm of `v`.

See also [`normalize`](@ref), [`normalize!`](@ref),
and [`qr`](@ref).
"""
function qr!(v::AbstractVector)
    nrm = norm(v)
    __normalize!(v, nrm), nrm
end

# Conversions
convert{T}(::Type{QR{T}}, A::QR) = QR(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ))
convert{T}(::Type{Factorization{T}}, A::QR{T}) = A
convert{T}(::Type{Factorization{T}}, A::QR) = convert(QR{T}, A)
convert{T}(::Type{QRCompactWY{T}}, A::QRCompactWY) = QRCompactWY(convert(AbstractMatrix{T}, A.factors), convert(AbstractMatrix{T}, A.T))
convert{T}(::Type{Factorization{T}}, A::QRCompactWY{T}) = A
convert{T}(::Type{Factorization{T}}, A::QRCompactWY) = convert(QRCompactWY{T}, A)
convert(::Type{AbstractMatrix}, F::Union{QR,QRCompactWY}) = F[:Q] * F[:R]
convert(::Type{AbstractArray}, F::Union{QR,QRCompactWY}) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::Union{QR,QRCompactWY}) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::Union{QR,QRCompactWY}) = convert(Matrix, F)
full(F::Union{QR,QRCompactWY}) = convert(AbstractArray, F)
convert{T}(::Type{QRPivoted{T}}, A::QRPivoted) = QRPivoted(convert(AbstractMatrix{T}, A.factors), convert(Vector{T}, A.τ), A.jpvt)
convert{T}(::Type{Factorization{T}}, A::QRPivoted{T}) = A
convert{T}(::Type{Factorization{T}}, A::QRPivoted) = convert(QRPivoted{T}, A)
convert(::Type{AbstractMatrix}, F::QRPivoted) = (F[:Q] * F[:R])[:,invperm(F[:p])]
convert(::Type{AbstractArray}, F::QRPivoted) = convert(AbstractMatrix, F)
convert(::Type{Matrix}, F::QRPivoted) = convert(Array, convert(AbstractArray, F))
convert(::Type{Array}, F::QRPivoted) = convert(Matrix, F)
full(F::QRPivoted) = convert(AbstractArray, F)

function show(io::IO, F::Union{QR, QRCompactWY, QRPivoted})
    println(io, "$(typeof(F)) with factors Q and R:")
    show(io, F[:Q])
    println(io)
    show(io, F[:R])
end

function getindex(A::QR, d::Symbol)
    m, n = size(A)
    if d == :R
        return triu!(A.factors[1:min(m,n), 1:n])
    elseif d == :Q
        return getq(A)
    else
        throw(KeyError(d))
    end
end
function getindex(A::QRCompactWY, d::Symbol)
    m, n = size(A)
    if d == :R
        return triu!(A.factors[1:min(m,n), 1:n])
    elseif d == :Q
        return getq(A)
    else
        throw(KeyError(d))
    end
end
function getindex{T}(A::QRPivoted{T}, d::Symbol)
    m, n = size(A)
    if d == :R
        return triu!(A.factors[1:min(m,n), 1:n])
    elseif d == :Q
        return getq(A)
    elseif d == :p
        return A.jpvt
    elseif d == :P
        p = A[:p]
        n = length(p)
        P = zeros(T, n, n)
        for i in 1:n
            P[p[i],i] = one(T)
        end
        return P
    else
        throw(KeyError(d))
    end
end

# Type-stable interface to get Q
getq(A::QRCompactWY) = QRCompactWYQ(A.factors,A.T)
getq(A::Union{QR, QRPivoted}) = QRPackedQ(A.factors,A.τ)

immutable QRPackedQ{T,S<:AbstractMatrix} <: AbstractMatrix{T}
    factors::S
    τ::Vector{T}
    QRPackedQ{T,S}(factors::AbstractMatrix{T}, τ::Vector{T}) where {T,S<:AbstractMatrix} = new(factors, τ)
end
QRPackedQ(factors::AbstractMatrix{T}, τ::Vector{T}) where T = QRPackedQ{T,typeof(factors)}(factors, τ)

immutable QRCompactWYQ{S, M<:AbstractMatrix} <: AbstractMatrix{S}
    factors::M
    T::Matrix{S}
    QRCompactWYQ{S,M}(factors::AbstractMatrix{S}, T::Matrix{S}) where {S,M<:AbstractMatrix} = new(factors, T)
end
QRCompactWYQ(factors::AbstractMatrix{S}, T::Matrix{S}) where S = QRCompactWYQ{S,typeof(factors)}(factors, T)

convert{T}(::Type{QRPackedQ{T}}, Q::QRPackedQ) = QRPackedQ(convert(AbstractMatrix{T}, Q.factors), convert(Vector{T}, Q.τ))
convert{T}(::Type{AbstractMatrix{T}}, Q::QRPackedQ{T}) = Q
convert{T}(::Type{AbstractMatrix{T}}, Q::QRPackedQ) = convert(QRPackedQ{T}, Q)
convert{S}(::Type{QRCompactWYQ{S}}, Q::QRCompactWYQ) = QRCompactWYQ(convert(AbstractMatrix{S}, Q.factors), convert(AbstractMatrix{S}, Q.T))
convert{S}(::Type{AbstractMatrix{S}}, Q::QRCompactWYQ{S}) = Q
convert{S}(::Type{AbstractMatrix{S}}, Q::QRCompactWYQ) = convert(QRCompactWYQ{S}, Q)
convert{T}(::Type{Matrix}, A::Union{QRPackedQ{T},QRCompactWYQ{T}}) = A_mul_B!(A, eye(T, size(A.factors, 1), minimum(size(A.factors))))
convert(::Type{Array}, A::Union{QRPackedQ,QRCompactWYQ}) = convert(Matrix, A)

"""
    full(A::Union{QRPackedQ,QRCompactWYQ}; thin::Bool=true) -> Matrix

Converts an orthogonal or unitary matrix stored as a `QRCompactWYQ` object, i.e. in the
compact WY format [^Bischof1987], or in the `QRPackedQ` format, to a dense matrix.

Optionally takes a `thin` Boolean argument, which if `true` omits the columns that span the
rows of `R` in the QR factorization that are zero. The resulting matrix is the `Q` in a thin
QR factorization (sometimes called the reduced QR factorization). If `false`, returns a `Q`
that spans all rows of `R` in its corresponding QR factorization.
"""
function full{T}(A::Union{QRPackedQ{T},QRCompactWYQ{T}}; thin::Bool = true)
    if thin
        convert(Array, A)
    else
        A_mul_B!(A, eye(T, size(A.factors, 1)))
    end
end

size(A::Union{QR,QRCompactWY,QRPivoted}, dim::Integer) = size(A.factors, dim)
size(A::Union{QR,QRCompactWY,QRPivoted}) = size(A.factors)
size(A::Union{QRPackedQ,QRCompactWYQ}, dim::Integer) = 0 < dim ? (dim <= 2 ? size(A.factors, 1) : 1) : throw(BoundsError())
size(A::Union{QRPackedQ,QRCompactWYQ}) = size(A, 1), size(A, 2)


function getindex(A::Union{QRPackedQ,QRCompactWYQ}, i::Integer, j::Integer)
    x = zeros(eltype(A), size(A, 1))
    x[i] = 1
    y = zeros(eltype(A), size(A, 2))
    y[j] = 1
    return dot(x, A_mul_B!(A, y))
end

## Multiplication by Q
### QB
A_mul_B!{T<:BlasFloat}(A::QRCompactWYQ{T}, B::StridedVecOrMat{T}) = LAPACK.gemqrt!('L','N',A.factors,A.T,B)
A_mul_B!{T<:BlasFloat}(A::QRPackedQ{T}, B::StridedVecOrMat{T}) = LAPACK.ormqr!('L','N',A.factors,A.τ,B)
function A_mul_B!(A::QRPackedQ, B::AbstractVecOrMat)
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

function (*)(A::Union{QRPackedQ,QRCompactWYQ}, b::StridedVector)
    TAb = promote_type(eltype(A), eltype(b))
    Anew = convert(AbstractMatrix{TAb}, A)
    if size(A.factors, 1) == length(b)
        bnew = copy_oftype(b, TAb)
    elseif size(A.factors, 2) == length(b)
        bnew = [b; zeros(TAb, size(A.factors, 1) - length(b))]
    else
        throw(DimensionMismatch("vector must have length either $(size(A.factors, 1)) or $(size(A.factors, 2))"))
    end
    A_mul_B!(Anew, bnew)
end
function (*)(A::Union{QRPackedQ,QRCompactWYQ}, B::StridedMatrix)
    TAB = promote_type(eltype(A), eltype(B))
    Anew = convert(AbstractMatrix{TAB}, A)
    if size(A.factors, 1) == size(B, 1)
        Bnew = copy_oftype(B, TAB)
    elseif size(A.factors, 2) == size(B, 1)
        Bnew = [B; zeros(TAB, size(A.factors, 1) - size(B,1), size(B, 2))]
    else
        throw(DimensionMismatch("first dimension of matrix must have size either $(size(A.factors, 1)) or $(size(A.factors, 2))"))
    end
    A_mul_B!(Anew, Bnew)
end

### QcB
Ac_mul_B!{T<:BlasReal}(A::QRCompactWYQ{T}, B::StridedVecOrMat{T}) = LAPACK.gemqrt!('L','T',A.factors,A.T,B)
Ac_mul_B!{T<:BlasComplex}(A::QRCompactWYQ{T}, B::StridedVecOrMat{T}) = LAPACK.gemqrt!('L','C',A.factors,A.T,B)
Ac_mul_B!{T<:BlasReal}(A::QRPackedQ{T}, B::StridedVecOrMat{T}) = LAPACK.ormqr!('L','T',A.factors,A.τ,B)
Ac_mul_B!{T<:BlasComplex}(A::QRPackedQ{T}, B::StridedVecOrMat{T}) = LAPACK.ormqr!('L','C',A.factors,A.τ,B)
function Ac_mul_B!(A::QRPackedQ, B::AbstractVecOrMat)
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
function Ac_mul_B(Q::Union{QRPackedQ,QRCompactWYQ}, B::StridedVecOrMat)
    TQB = promote_type(eltype(Q), eltype(B))
    return Ac_mul_B!(convert(AbstractMatrix{TQB}, Q), copy_oftype(B, TQB))
end

### QBc/QcBc
for (f1, f2) in ((:A_mul_Bc, :A_mul_B!),
                 (:Ac_mul_Bc, :Ac_mul_B!))
    @eval begin
        function ($f1)(Q::Union{QRPackedQ,QRCompactWYQ}, B::StridedVecOrMat)
            TQB = promote_type(eltype(Q), eltype(B))
            Bc = similar(B, TQB, (size(B, 2), size(B, 1)))
            ctranspose!(Bc, B)
            return ($f2)(convert(AbstractMatrix{TQB}, Q), Bc)
        end
    end
end

### AQ
A_mul_B!{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) = LAPACK.gemqrt!('R','N', B.factors, B.T, A)
A_mul_B!{T<:BlasFloat}(A::StridedVecOrMat{T}, B::QRPackedQ{T}) = LAPACK.ormqr!('R', 'N', B.factors, B.τ, A)
function A_mul_B!(A::StridedMatrix,Q::QRPackedQ)
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

function (*)(A::StridedMatrix, Q::Union{QRPackedQ,QRCompactWYQ})
    TAQ = promote_type(eltype(A), eltype(Q))
    return A_mul_B!(copy_oftype(A, TAQ), convert(AbstractMatrix{TAQ}, Q))
end

### AQc
A_mul_Bc!{T<:BlasReal}(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) = LAPACK.gemqrt!('R','T',B.factors,B.T,A)
A_mul_Bc!{T<:BlasComplex}(A::StridedVecOrMat{T}, B::QRCompactWYQ{T}) = LAPACK.gemqrt!('R','C',B.factors,B.T,A)
A_mul_Bc!{T<:BlasReal}(A::StridedVecOrMat{T}, B::QRPackedQ{T}) = LAPACK.ormqr!('R','T',B.factors,B.τ,A)
A_mul_Bc!{T<:BlasComplex}(A::StridedVecOrMat{T}, B::QRPackedQ{T}) = LAPACK.ormqr!('R','C',B.factors,B.τ,A)
function A_mul_Bc!(A::AbstractMatrix,Q::QRPackedQ)
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
function A_mul_Bc(A::AbstractMatrix, B::Union{QRCompactWYQ,QRPackedQ})
    TAB = promote_type(eltype(A),eltype(B))
    BB = convert(AbstractMatrix{TAB}, B)
    if size(A,2) == size(B.factors, 1)
        AA = similar(A, TAB, size(A))
        copy!(AA, A)
        return A_mul_Bc!(AA, BB)
    elseif size(A,2) == size(B.factors,2)
        return A_mul_Bc!([A zeros(TAB, size(A, 1), size(B.factors, 1) - size(B.factors, 2))], BB)
    else
        throw(DimensionMismatch("matrix A has dimensions $(size(A)) but matrix B has dimensions $(size(B))"))
    end
end
@inline A_mul_Bc(rowvec::RowVector, B::Union{LinAlg.QRCompactWYQ,LinAlg.QRPackedQ}) = ctranspose(B*ctranspose(rowvec))


### AcQ/AcQc
for (f1, f2) in ((:Ac_mul_B, :A_mul_B!),
                 (:Ac_mul_Bc, :A_mul_Bc!))
    @eval begin
        function ($f1)(A::StridedVecOrMat, Q::Union{QRPackedQ,QRCompactWYQ})
            TAQ = promote_type(eltype(A), eltype(Q))
            Ac = similar(A, TAQ, (size(A, 2), size(A, 1)))
            ctranspose!(Ac, A)
            return ($f2)(Ac, convert(AbstractMatrix{TAQ}, Q))
        end
    end
end

A_ldiv_B!{T<:BlasFloat}(A::QRCompactWY{T}, b::StridedVector{T}) = (A_ldiv_B!(UpperTriangular(A[:R]), view(Ac_mul_B!(A[:Q], b), 1:size(A, 2))); b)
A_ldiv_B!{T<:BlasFloat}(A::QRCompactWY{T}, B::StridedMatrix{T}) = (A_ldiv_B!(UpperTriangular(A[:R]), view(Ac_mul_B!(A[:Q], B), 1:size(A, 2), 1:size(B, 2))); B)

# Julia implementation similarly to xgelsy
function A_ldiv_B!{T<:BlasFloat}(A::QRPivoted{T}, B::StridedMatrix{T}, rcond::Real)
    mA, nA = size(A.factors)
    nr = min(mA,nA)
    nrhs = size(B, 2)
    if nr == 0 return zeros(T, 0, nrhs), 0 end
    ar = abs(A.factors[1])
    if ar == 0 return zeros(T, nr, nrhs), 0 end
    rnk = 1
    xmin = ones(T, 1)
    xmax = ones(T, 1)
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
    A_ldiv_B!(UpperTriangular(C[1:rnk,1:rnk]),view(Ac_mul_B!(getq(A),view(B, 1:mA, 1:nrhs)),1:rnk,1:nrhs))
    B[rnk+1:end,:] = zero(T)
    LAPACK.ormrz!('L', eltype(B)<:Complex ? 'C' : 'T', C, τ, view(B,1:nA,1:nrhs))
    B[1:nA,:] = view(B, 1:nA, :)[invperm(A[:p]::Vector{BlasInt}),:]
    return B, rnk
end
A_ldiv_B!{T<:BlasFloat}(A::QRPivoted{T}, B::StridedVector{T}) = vec(A_ldiv_B!(A,reshape(B,length(B),1)))
A_ldiv_B!{T<:BlasFloat}(A::QRPivoted{T}, B::StridedVecOrMat{T}) = A_ldiv_B!(A, B, maximum(size(A))*eps(real(float(one(eltype(B))))))[1]
function A_ldiv_B!{T}(A::QR{T}, B::StridedMatrix{T})
    m, n = size(A)
    minmn = min(m,n)
    mB, nB = size(B)
    Ac_mul_B!(A[:Q], view(B, 1:m, :))
    R = A[:R]
    @inbounds begin
        if n > m # minimum norm solution
            τ = zeros(T,m)
            for k = m:-1:1 # Trapezoid to triangular by elementary operation
                x = view(R, k, [k; m + 1:n])
                τk = reflector!(x)
                τ[k] = τk'
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
        Base.A_ldiv_B!(UpperTriangular(view(R, :, 1:minmn)), view(B, 1:minmn, :))
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
A_ldiv_B!(A::QR, B::StridedVector) = A_ldiv_B!(A, reshape(B, length(B), 1))[:]
function A_ldiv_B!(A::QRPivoted, b::StridedVector)
    A_ldiv_B!(QR(A.factors,A.τ), b)
    b[1:size(A.factors, 2)] = view(b, 1:size(A.factors, 2))[invperm(A.jpvt)]
    b
end
function A_ldiv_B!(A::QRPivoted, B::StridedMatrix)
    A_ldiv_B!(QR(A.factors, A.τ), B)
    B[1:size(A.factors, 2),:] = view(B, 1:size(A.factors, 2), :)[invperm(A.jpvt),:]
    B
end

# convenience methods
## return only the solution of a least squares problem while avoiding promoting
## vectors to matrices.
_cut_B(x::AbstractVector, r::UnitRange) = length(x)  > length(r) ? x[r]   : x
_cut_B(X::AbstractMatrix, r::UnitRange) = size(X, 1) > length(r) ? X[r,:] : X

## append right hand side with zeros if necessary
function _append_zeros(b::AbstractVector, T::Type, n)
    if n > length(b)
        x = zeros(T, n)
        return copy!(x, b)
    else
        return copy_oftype(b, T)
    end
end
function _append_zeros(B::AbstractMatrix, T::Type, n)
    if n > size(B, 1)
        X = zeros(T, (n, size(B, 2)))
        X[1:size(B,1), :] = B
        return X
    else
        return copy_oftype(B, T)
    end
end

function (\){TA,TB}(A::Union{QR{TA},QRCompactWY{TA},QRPivoted{TA}}, B::AbstractVecOrMat{TB})
    S = promote_type(TA,TB)
    m, n = size(A)
    m == size(B,1) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has $(size(B,1)) rows"))

    AA = convert(Factorization{S}, A)

    X = A_ldiv_B!(AA, _append_zeros(B, S, n))

    return _cut_B(X, 1:n)
end

# With a real lhs and complex rhs with the same precision, we can reinterpret the complex
# rhs as a real rhs with twice the number of columns.

# convenience methods to compute the return size correctly for vectors and matrices
_ret_size(A::Factorization, b::AbstractVector) = (max(size(A, 2), length(b)),)
_ret_size(A::Factorization, B::AbstractMatrix) = (max(size(A, 2), size(B, 1)), size(B, 2))

function (\){T<:BlasReal}(A::Union{QR{T},QRCompactWY{T},QRPivoted{T}}, BIn::VecOrMat{Complex{T}})
    m, n = size(A)
    m == size(BIn, 1) || throw(DimensionMismatch("left hand side has $m rows, but right hand side has $(size(BIn,1)) rows"))

# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      ->       |y1|y2|y3|y4|     ->      |x2|y2|     ->    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    B = reshape(transpose(reinterpret(T, BIn, (2, length(BIn)))), size(BIn, 1), 2*size(BIn, 2))

    X = A_ldiv_B!(A, _append_zeros(B, T, n))

# |z1|z3|  reinterpret  |x1|x2|x3|x4|  transpose  |x1|y1|  reshape  |x1|y1|x3|y3|
# |z2|z4|      <-       |y1|y2|y3|y4|     <-      |x2|y2|     <-    |x2|y2|x4|y4|
#                                                 |x3|y3|
#                                                 |x4|y4|
    XX = reinterpret(Complex{T}, transpose(reshape(X, div(length(X), 2), 2)), _ret_size(A, BIn))
    return _cut_B(XX, 1:n)
end

##TODO:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations

# FIXME! Should add balancing option through xgebal
