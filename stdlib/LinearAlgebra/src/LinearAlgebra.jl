# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Linear algebra module. Provides array arithmetic,
matrix factorizations and other linear algebra related
functionality.
"""
module LinearAlgebra

import Base: \, /, //, *, ^, +, -, ==
import Base: USE_BLAS64, abs, acos, acosh, acot, acoth, acsc, acsch, adjoint, asec, asech,
    asin, asinh, atan, atanh, axes, big, broadcast, cbrt, ceil, cis, collect, conj, convert,
    copy, copyto!, copymutable, cos, cosh, cot, coth, csc, csch, eltype, exp, fill!, floor,
    getindex, hcat, getproperty, imag, inv, invpermuterows!, isapprox, isequal, isone, iszero,
    IndexStyle, kron, kron!, length, log, map, ndims, one, oneunit, parent, permutecols!,
    permutedims, permuterows!, power_by_squaring, promote_rule, real, sec, sech, setindex!,
    show, similar, sin, sincos, sinh, size, sqrt, strides, stride, tan, tanh, transpose, trunc,
    typed_hcat, vec, view, zero
using Base: IndexLinear, promote_eltype, promote_op, print_matrix,
    @propagate_inbounds, reduce, typed_hvcat, typed_vcat, require_one_based_indexing,
    splat, BitInteger
using Base.Broadcast: Broadcasted, broadcasted
using Base.PermutedDimsArrays: CommutativeOps
using OpenBLAS_jll
using libblastrampoline_jll
import Libdl

export
# Modules
    BLAS,
    LAPACK,

# Types
    Adjoint,
    Bidiagonal,
    BunchKaufman,
    Cholesky,
    CholeskyPivoted,
    ColumnNorm,
    Diagonal,
    Eigen,
    Factorization,
    GeneralizedEigen,
    GeneralizedSVD,
    GeneralizedSchur,
    Hermitian,
    Hessenberg,
    LDLt,
    LQ,
    LU,
    LowerTriangular,
    NoPivot,
    QR,
    QRPivoted,
    RowMaximum,
    RowNonZero,
    SVD,
    Schur,
    SymTridiagonal,
    Symmetric,
    Transpose,
    Tridiagonal,
    UniformScaling,
    UnitLowerTriangular,
    UnitUpperTriangular,
    UpperHessenberg,
    UpperTriangular,


# Functions
    adjoint!,
    adjoint,
    axpby!,
    axpy!,
    bunchkaufman!,
    bunchkaufman,
    cholesky!,
    cholesky,
    cond,
    condskeel,
    copy_adjoint!,
    copy_transpose!,
    copyto!,
    copytrito!,
    cross,
    det,
    diag,
    diagind,
    diagm,
    dot,
    eigen!,
    eigen,
    eigmax,
    eigmin,
    eigvals!,
    eigvals,
    eigvecs,
    factorize,
    givens,
    hermitianpart!,
    hermitianpart,
    hessenberg!,
    hessenberg,
    isdiag,
    ishermitian,
    isposdef!,
    isposdef,
    issuccess,
    issymmetric,
    istril,
    istriu,
    kron!,
    kron,
    ldiv!,
    ldlt!,
    ldlt,
    lmul!,
    logabsdet,
    logdet,
    lowrankdowndate!,
    lowrankdowndate,
    lowrankupdate!,
    lowrankupdate,
    lq!,
    lq,
    lu!,
    lu,
    lyap,
    mul!,
    norm,
    normalize!,
    normalize,
    nullspace,
    opnorm,
    ordschur!,
    ordschur,
    pinv,
    qr!,
    qr,
    rank,
    rdiv!,
    reflect!,
    rmul!,
    rotate!,
    schur!,
    schur,
    svd!,
    svd,
    svdvals!,
    svdvals,
    sylvester,
    tr,
    transpose!,
    transpose,
    tril!,
    tril,
    triu!,
    triu,


# Operators
    \,
    /,

# Constants
    I

# not exported, but public names
public AbstractTriangular,
        Givens,
        checksquare,
        hermitian,
        hermitian_type,
        isbanded,
        peakflops,
        symmetric,
        symmetric_type

const BlasFloat = Union{Float64,Float32,ComplexF64,ComplexF32}
const BlasReal = Union{Float64,Float32}
const BlasComplex = Union{ComplexF64,ComplexF32}

if USE_BLAS64
    const BlasInt = Int64
else
    const BlasInt = Int32
end


abstract type Algorithm end
struct DivideAndConquer <: Algorithm end
struct QRIteration <: Algorithm end
struct RobustRepresentations <: Algorithm end

# Pivoting strategies for matrix factorization algorithms.
abstract type PivotingStrategy end

"""
    NoPivot

Pivoting is not performed. This is the default strategy for [`cholesky`](@ref) and
[`qr`](@ref) factorizations. Note, however, that other matrix factorizations such as the LU
factorization may fail without pivoting, and may also be numerically unstable for
floating-point matrices in the face of roundoff error. In such cases, this pivot strategy
is mainly useful for pedagogical purposes.
"""
struct NoPivot <: PivotingStrategy end

"""
    RowNonZero

First non-zero element in the remaining rows is chosen as the pivot element.

Beware that for floating-point matrices, the resulting LU algorithm is numerically unstable
— this strategy is mainly useful for comparison to hand calculations (which typically use
this strategy) or for other algebraic types (e.g. rational numbers) not susceptible to
roundoff errors. Otherwise, the default `RowMaximum` pivoting strategy should be generally
preferred in Gaussian elimination.

Note that the [element type](@ref eltype) of the matrix must admit an [`iszero`](@ref)
method.
"""
struct RowNonZero <: PivotingStrategy end

"""
    RowMaximum

A row (and potentially also column) pivot is chosen based on a maximum property.
This is the default strategy for LU factorization and for pivoted Cholesky factorization
(though [`NoPivot`] is the default for [`cholesky`](@ref)).

In the LU case, the maximum-magnitude element within the current column in the remaining
rows is chosen as the pivot element. This is sometimes referred to as the "partial
pivoting" algorithm. In this case, the [element type](@ref eltype) of the matrix must admit
an [`abs`](@ref) method, whose result type must admit a [`<`](@ref) method.

In the Cholesky case, the maximal element among the remaining diagonal elements is
chosen as the pivot element. This is sometimes referred to as the "diagonal pivoting"
algorithm, and leads to _complete pivoting_ (i.e., of both rows and columns by the same
permutation). In this case, the (real part of the) [element type](@ref eltype) of the
matrix must admit a [`<`](@ref) method.
"""
struct RowMaximum <: PivotingStrategy end

"""
    ColumnNorm

The column with the maximum norm is used for subsequent computation. This is used for
pivoted QR factorization.

Note that the [element type](@ref eltype) of the matrix must admit [`norm`](@ref) and
[`abs`](@ref) methods, whose respective result types must admit a [`<`](@ref) method.
"""
struct ColumnNorm <: PivotingStrategy end

using Base: DimOrInd

# Check that stride of matrix/vector is 1
# Writing like this to avoid splatting penalty when called with multiple arguments,
# see PR 16416
"""
    stride1(A) -> Int

Return the distance between successive array elements
in dimension 1 in units of element size.

# Examples
```jldoctest
julia> A = [1,2,3,4]
4-element Vector{Int64}:
 1
 2
 3
 4

julia> LinearAlgebra.stride1(A)
1

julia> B = view(A, 2:2:4)
2-element view(::Vector{Int64}, 2:2:4) with eltype Int64:
 2
 4

julia> LinearAlgebra.stride1(B)
2
```
"""
stride1(x) = stride(x,1)
stride1(x::Array) = 1
stride1(x::DenseArray) = stride(x, 1)::Int

@inline chkstride1(A...) = _chkstride1(true, A...)
@noinline _chkstride1(ok::Bool) = ok || error("matrix does not have contiguous columns")
@inline _chkstride1(ok::Bool, A, B...) = _chkstride1(ok & (stride1(A) == 1), B...)

# Subtypes of StridedArrays that satisfy certain properties on their strides
# Similar to Base.RangeIndex, but only include range types where the step is statically known to be non-zero
const IncreasingRangeIndex = Union{BitInteger, AbstractUnitRange{<:BitInteger}}
const NonConstRangeIndex = Union{IncreasingRangeIndex, StepRange{<:BitInteger, <:BitInteger}}
# StridedArray subtypes for which _fullstride2(::T) === true is known from the type
DenseOrStridedReshapedReinterpreted{T,N} =
    Union{DenseArray{T,N}, Base.StridedReshapedArray{T,N}, Base.StridedReinterpretArray{T,N}}
# Similar to Base.StridedSubArray, except with a NonConstRangeIndex instead of a RangeIndex
StridedSubArrayStandard{T,N,A<:DenseOrStridedReshapedReinterpreted,
    I<:Tuple{Vararg{Union{NonConstRangeIndex, Base.ReshapedUnitRange, Base.AbstractCartesianIndex}}}} = Base.StridedSubArray{T,N,A,I}
StridedArrayStdSubArray{T,N} = Union{DenseOrStridedReshapedReinterpreted{T,N},StridedSubArrayStandard{T,N}}
# Similar to Base.StridedSubArray, except with a IncreasingRangeIndex instead of a RangeIndex
StridedSubArrayIncr{T,N,A<:DenseOrStridedReshapedReinterpreted,
    I<:Tuple{Vararg{Union{IncreasingRangeIndex, Base.ReshapedUnitRange, Base.AbstractCartesianIndex}}}} = Base.StridedSubArray{T,N,A,I}
StridedArrayStdSubArrayIncr{T,N} = Union{DenseOrStridedReshapedReinterpreted{T,N},StridedSubArrayIncr{T,N}}
# These subarrays have a stride of 1 along the first dimension
StridedSubArrayAUR{T,N,A<:DenseOrStridedReshapedReinterpreted,
    I<:Tuple{AbstractUnitRange{<:BitInteger}}} = Base.StridedSubArray{T,N,A,I}
StridedArrayStride1{T,N} = Union{DenseOrStridedReshapedReinterpreted{T,N},StridedSubArrayIncr{T,N}}
# StridedMatrixStride1 may typically be forwarded to LAPACK methods
StridedMatrixStride1{T} = StridedArrayStride1{T,2}

"""
    LinearAlgebra.checksquare(A)

Check that a matrix is square, then return its common dimension.
For multiple arguments, return a vector.

# Examples
```jldoctest
julia> A = fill(1, (4,4)); B = fill(1, (5,5));

julia> LinearAlgebra.checksquare(A, B)
2-element Vector{Int64}:
 4
 5
```
"""
function checksquare(A)
    m,n = size(A)
    m == n || throw(DimensionMismatch(lazy"matrix is not square: dimensions are $(size(A))"))
    m
end

function checksquare(A...)
    sizes = Int[]
    for a in A
        size(a,1)==size(a,2) || throw(DimensionMismatch(lazy"matrix is not square: dimensions are $(size(a))"))
        push!(sizes, size(a,1))
    end
    return sizes
end

function char_uplo(uplo::Symbol)
    if uplo === :U
        return 'U'
    elseif uplo === :L
        return 'L'
    else
        throw_uplo()
    end
end

function sym_uplo(uplo::Char)
    if uplo == 'U'
        return :U
    elseif uplo == 'L'
        return :L
    else
        throw_uplo()
    end
end

@noinline throw_uplo() = throw(ArgumentError("uplo argument must be either :U (upper) or :L (lower)"))

"""
    ldiv!(Y, A, B) -> Y

Compute `A \\ B` in-place and store the result in `Y`, returning the result.

The argument `A` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholesky`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lu!`](@ref)),
and performance-critical situations requiring `ldiv!` usually also require fine-grained
control over the factorization of `A`.

!!! note
    Certain structured matrix types, such as `Diagonal` and `UpperTriangular`, are permitted, as
    these are already in a factorized form

# Examples
```jldoctest
julia> A = [1 2.2 4; 3.1 0.2 3; 4 1 2];

julia> X = [1; 2.5; 3];

julia> Y = zero(X);

julia> ldiv!(Y, qr(A), X);

julia> Y
3-element Vector{Float64}:
  0.7128099173553719
 -0.051652892561983674
  0.10020661157024757

julia> A\\X
3-element Vector{Float64}:
  0.7128099173553719
 -0.05165289256198333
  0.10020661157024785
```
"""
ldiv!(Y, A, B)

"""
    ldiv!(A, B)

Compute `A \\ B` in-place and overwriting `B` to store the result.

The argument `A` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholesky`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lu!`](@ref)),
and performance-critical situations requiring `ldiv!` usually also require fine-grained
control over the factorization of `A`.

!!! note
    Certain structured matrix types, such as `Diagonal` and `UpperTriangular`, are permitted, as
    these are already in a factorized form

# Examples
```jldoctest
julia> A = [1 2.2 4; 3.1 0.2 3; 4 1 2];

julia> X = [1; 2.5; 3];

julia> Y = copy(X);

julia> ldiv!(qr(A), X);

julia> X
3-element Vector{Float64}:
  0.7128099173553719
 -0.051652892561983674
  0.10020661157024757

julia> A\\Y
3-element Vector{Float64}:
  0.7128099173553719
 -0.05165289256198333
  0.10020661157024785
```
"""
ldiv!(A, B)


"""
    rdiv!(A, B)

Compute `A / B` in-place and overwriting `A` to store the result.

The argument `B` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholesky`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lu!`](@ref)),
and performance-critical situations requiring `rdiv!` usually also require fine-grained
control over the factorization of `B`.

!!! note
    Certain structured matrix types, such as `Diagonal` and `UpperTriangular`, are permitted, as
    these are already in a factorized form
"""
rdiv!(A, B)

"""
    copy_oftype(A, T)

Creates a copy of `A` with eltype `T`. No assertions about mutability of the result are
made. When `eltype(A) == T`, then this calls `copy(A)` which may be overloaded for custom
array types. Otherwise, this calls `convert(AbstractArray{T}, A)`.
"""
copy_oftype(A::AbstractArray{T}, ::Type{T}) where {T} = copy(A)
copy_oftype(A::AbstractArray{T,N}, ::Type{S}) where {T,N,S} = convert(AbstractArray{S,N}, A)

"""
    copymutable_oftype(A, T)

Copy `A` to a mutable array with eltype `T` based on `similar(A, T)`.

The resulting matrix typically has similar algebraic structure as `A`. For
example, supplying a tridiagonal matrix results in another tridiagonal matrix.
In general, the type of the output corresponds to that of `similar(A, T)`.

In LinearAlgebra, mutable copies (of some desired eltype) are created to be passed
to in-place algorithms (such as `ldiv!`, `rdiv!`, `lu!` and so on). If the specific
algorithm is known to preserve the algebraic structure, use `copymutable_oftype`.
If the algorithm is known to return a dense matrix (or some wrapper backed by a dense
matrix), then use `copy_similar`.

See also: `Base.copymutable`, `copy_similar`.
"""
copymutable_oftype(A::AbstractArray, ::Type{S}) where {S} = copyto!(similar(A, S), A)

"""
    copy_similar(A, T)

Copy `A` to a mutable array with eltype `T` based on `similar(A, T, size(A))`.

Compared to `copymutable_oftype`, the result can be more flexible. In general, the type
of the output corresponds to that of the three-argument method `similar(A, T, size(A))`.

See also: `copymutable_oftype`.
"""
copy_similar(A::AbstractArray, ::Type{T}) where {T} = copyto!(similar(A, T, size(A)), A)

"""
    BandIndex(band, index)

Represent a Cartesian index as a linear index along a band.
This type is primarily meant to index into a specific band without branches,
so, for best performance, `band` should be a compile-time constant.
"""
struct BandIndex
    band :: Int
    index :: Int
end
function _cartinds(b::BandIndex)
    (; band, index) = b
    bandg0 = max(band,0)
    row = index - band + bandg0
    col = index + bandg0
    CartesianIndex(row, col)
end
function Base.to_indices(A, inds, t::Tuple{BandIndex, Vararg{Any}})
    to_indices(A, inds, (_cartinds(first(t)), Base.tail(t)...))
end
function Base.checkbounds(::Type{Bool}, A::AbstractMatrix, b::BandIndex)
    checkbounds(Bool, A, _cartinds(b))
end
function Base.checkbounds(A::Broadcasted, b::BandIndex)
    checkbounds(A, _cartinds(b))
end

include("adjtrans.jl")
include("transpose.jl")

include("exceptions.jl")
include("generic.jl")

include("blas.jl")
include("matmul.jl")
include("lapack.jl")

include("dense.jl")
include("tridiag.jl")
include("triangular.jl")

include("factorization.jl")
include("eigen.jl")
include("svd.jl")
include("symmetric.jl")
include("cholesky.jl")
include("lu.jl")
include("bunchkaufman.jl")
include("diagonal.jl")
include("symmetriceigen.jl")
include("bidiag.jl")
include("uniformscaling.jl")
include("qr.jl")
include("lq.jl")
include("hessenberg.jl")
include("abstractq.jl")
include("givens.jl")
include("special.jl")
include("bitarray.jl")
include("ldlt.jl")
include("schur.jl")
include("structuredbroadcast.jl")
include("deprecated.jl")

const ⋅ = dot
const × = cross
export ⋅, ×

# Separate the char corresponding to the wrapper from that corresponding to the uplo
# In most cases, the former may be constant-propagated, while the latter usually can't be.
# This improves type-inference in wrap for Symmetric/Hermitian matrices
# A WrapperChar is equivalent to `isuppertri ? uppercase(wrapperchar) : lowercase(wrapperchar)`
struct WrapperChar <: AbstractChar
    wrapperchar :: Char
    isuppertri :: Bool
end
function Base.Char(w::WrapperChar)
    T = w.wrapperchar
    if T ∈ ('N', 'T', 'C') # known cases where isuppertri is true
        T
    else
        _isuppertri(w) ? uppercase(T) : lowercase(T)
    end
end
Base.codepoint(w::WrapperChar) = codepoint(Char(w))
WrapperChar(n::UInt32) = WrapperChar(Char(n))
WrapperChar(c::Char) = WrapperChar(c, isuppercase(c))
# We extract the wrapperchar so that the result may be constant-propagated
# This doesn't return a value of the same type on purpose
Base.uppercase(w::WrapperChar) = uppercase(w.wrapperchar)
Base.lowercase(w::WrapperChar) = lowercase(w.wrapperchar)
_isuppertri(w::WrapperChar) = w.isuppertri
_isuppertri(x::AbstractChar) = isuppercase(x) # compatibility with earlier Char-based implementation
_uplosym(x) = _isuppertri(x) ? (:U) : (:L)

wrapper_char(::AbstractArray) = 'N'
wrapper_char(::Adjoint) = 'C'
wrapper_char(::Adjoint{<:Real}) = 'T'
wrapper_char(::Transpose) = 'T'
wrapper_char(A::Hermitian) =  WrapperChar('H', A.uplo == 'U')
wrapper_char(A::Hermitian{<:Real}) = WrapperChar('S', A.uplo == 'U')
wrapper_char(A::Symmetric) = WrapperChar('S', A.uplo == 'U')

wrapper_char_NTC(A::AbstractArray) = uppercase(wrapper_char(A)) == 'N'
wrapper_char_NTC(A::Union{StridedArray, Adjoint, Transpose}) = true
wrapper_char_NTC(A::Union{Symmetric, Hermitian}) = false

Base.@constprop :aggressive function wrap(A::AbstractVecOrMat, tA::AbstractChar)
    # merge the result of this before return, so that we can type-assert the return such
    # that even if the tmerge is inaccurate, inference can still identify that the
    # `_generic_matmatmul` signature still matches and doesn't require missing backedges
    tA_uc = uppercase(tA)
    B = if tA_uc == 'N'
        A
    elseif tA_uc == 'T'
        transpose(A)
    elseif tA_uc == 'C'
        adjoint(A)
    elseif tA_uc == 'H'
        Hermitian(A, _uplosym(tA))
    elseif tA_uc == 'S'
        Symmetric(A, _uplosym(tA))
    end
    return B::AbstractVecOrMat
end

_unwrap(A::AbstractVecOrMat) = A

## convenience methods
## return only the solution of a least squares problem while avoiding promoting
## vectors to matrices.
_cut_B(x::AbstractVector, r::UnitRange) = length(x)  > length(r) ? x[r]   : x
_cut_B(X::AbstractMatrix, r::UnitRange) = size(X, 1) > length(r) ? X[r,:] : X

# SymTridiagonal ev can be the same length as dv, but the last element is
# ignored. However, some methods can fail if they read the entire ev
# rather than just the meaningful elements. This is a helper function
# for getting only the meaningful elements of ev. See #41089
_evview(S::SymTridiagonal) = @view S.ev[begin:begin + length(S.dv) - 2]

## append right hand side with zeros if necessary
_zeros(::Type{T}, b::AbstractVector, n::Integer) where {T} = zeros(T, max(length(b), n))
_zeros(::Type{T}, B::AbstractMatrix, n::Integer) where {T} = zeros(T, max(size(B, 1), n), size(B, 2))

# append a zero element / drop the last element
_pushzero(A) = (B = similar(A, length(A)+1); @inbounds B[begin:end-1] .= A; @inbounds B[end] = zero(eltype(B)); B)
_droplast!(A) = deleteat!(A, lastindex(A))

# destination type for matmul
matprod_dest(A::StructuredMatrix, B::StructuredMatrix, TS) = similar(B, TS, size(B))
matprod_dest(A, B::StructuredMatrix, TS) = similar(A, TS, size(A))
matprod_dest(A::StructuredMatrix, B, TS) = similar(B, TS, size(B))
# diagonal is special, as it does not change the structure of the other matrix
# we call similar without a size to preserve the type of the matrix wherever possible
# reroute through _matprod_dest_diag to allow speicalizing on the type of the StructuredMatrix
# without defining methods for both the orderings
matprod_dest(A::StructuredMatrix, B::Diagonal, TS) = _matprod_dest_diag(A, TS)
matprod_dest(A::Diagonal, B::StructuredMatrix, TS) = _matprod_dest_diag(B, TS)
matprod_dest(A::Diagonal, B::Diagonal, TS) = _matprod_dest_diag(B, TS)
_matprod_dest_diag(A, TS) = similar(A, TS)
function _matprod_dest_diag(A::SymTridiagonal, TS)
    n = size(A, 1)
    Tridiagonal(similar(A, TS, n-1), similar(A, TS, n), similar(A, TS, n-1))
end

# Special handling for adj/trans vec
matprod_dest(A::Diagonal, B::AdjOrTransAbsVec, TS) = similar(B, TS)

# General fallback definition for handling under- and overdetermined system as well as square problems
# While this definition is pretty general, it does e.g. promote to common element type of lhs and rhs
# which is required by LAPACK but not SuiteSparse which allows real-complex solves in some cases. Hence,
# we restrict this method to only the LAPACK factorizations in LinearAlgebra.
# The definition is put here since it explicitly references all the Factorization structs so it has
# to be located after all the files that define the structs.
const LAPACKFactorizations{T,S} = Union{
    BunchKaufman{T,S},
    Cholesky{T,S},
    LQ{T,S},
    LU{T,S},
    QR{T,S},
    QRCompactWY{T,S},
    QRPivoted{T,S},
    SVD{T,<:Real,S}}

(\)(F::LAPACKFactorizations, B::AbstractVecOrMat) = ldiv(F, B)
(\)(F::AdjointFactorization{<:Any,<:LAPACKFactorizations}, B::AbstractVecOrMat) = ldiv(F, B)
(\)(F::TransposeFactorization{<:Any,<:LU}, B::AbstractVecOrMat) = ldiv(F, B)

function ldiv(F::Factorization, B::AbstractVecOrMat)
    require_one_based_indexing(B)
    m, n = size(F)
    if m != size(B, 1)
        throw(DimensionMismatch("arguments must have the same number of rows"))
    end

    TFB = typeof(oneunit(eltype(B)) / oneunit(eltype(F)))
    FF = Factorization{TFB}(F)

    # For wide problem we (often) compute a minimum norm solution. The solution
    # is larger than the right hand side so we use size(F, 2).
    BB = _zeros(TFB, B, n)

    if n > size(B, 1)
        # Underdetermined
        copyto!(view(BB, 1:m, :), B)
    else
        copyto!(BB, B)
    end

    ldiv!(FF, BB)

    # For tall problems, we compute a least squares solution so only part
    # of the rhs should be returned from \ while ldiv! uses (and returns)
    # the complete rhs
    return _cut_B(BB, 1:n)
end
# disambiguate
(\)(F::LAPACKFactorizations{T}, B::VecOrMat{Complex{T}}) where {T<:BlasReal} =
    @invoke \(F::Factorization{T}, B::VecOrMat{Complex{T}})
(\)(F::AdjointFactorization{T,<:LAPACKFactorizations}, B::VecOrMat{Complex{T}}) where {T<:BlasReal} =
    ldiv(F, B)
(\)(F::TransposeFactorization{T,<:LU}, B::VecOrMat{Complex{T}}) where {T<:BlasReal} =
    ldiv(F, B)

"""
    LinearAlgebra.peakflops(n::Integer=4096; eltype::DataType=Float64, ntrials::Integer=3, parallel::Bool=false)

`peakflops` computes the peak flop rate of the computer by using double precision
[`gemm!`](@ref LinearAlgebra.BLAS.gemm!). By default, if no arguments are specified, it
multiplies two `Float64` matrices of size `n x n`, where `n = 4096`. If the underlying BLAS is using
multiple threads, higher flop rates are realized. The number of BLAS threads can be set with
[`BLAS.set_num_threads(n)`](@ref).

If the keyword argument `eltype` is provided, `peakflops` will construct matrices with elements
of type `eltype` for calculating the peak flop rate.

By default, `peakflops` will use the best timing from 3 trials. If the `ntrials` keyword argument
is provided, `peakflops` will use those many trials for picking the best timing.

If the keyword argument `parallel` is set to `true`, `peakflops` is run in parallel on all
the worker processors. The flop rate of the entire parallel computer is returned. When
running in parallel, only 1 BLAS thread is used. The argument `n` still refers to the size
of the problem that is solved on each processor.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1. In Julia 1.0 it is available from
    the standard library `InteractiveUtils`.
"""
function peakflops(n::Integer=4096; eltype::DataType=Float64, ntrials::Integer=3, parallel::Bool=false)
    t = zeros(Float64, ntrials)
    for i=1:ntrials
        a = ones(eltype,n,n)
        t[i] = @elapsed a2 = a*a
        @assert a2[1,1] == n
    end

    if parallel
        let Distributed = Base.require(Base.PkgId(
                Base.UUID((0x8ba89e20_285c_5b6f, 0x9357_94700520ee1b)), "Distributed"))
            nworkers = @invokelatest Distributed.nworkers()
            results = @invokelatest Distributed.pmap(peakflops, fill(n, nworkers))
            return sum(results)
        end
    else
        return 2*Float64(n)^3 / minimum(t)
    end
end


function versioninfo(io::IO=stdout)
    indent = "  "
    config = BLAS.get_config()
    build_flags = join(string.(config.build_flags), ", ")
    println(io, "BLAS: ", BLAS.libblastrampoline, " (", build_flags, ")")
    for lib in config.loaded_libs
        interface = uppercase(string(lib.interface))
        println(io, indent, "--> ", lib.libname, " (", interface, ")")
    end
    println(io, "Threading:")
    println(io, indent, "Threads.threadpoolsize() = ", Threads.threadpoolsize())
    println(io, indent, "Threads.maxthreadid() = ", Base.Threads.maxthreadid())
    println(io, indent, "LinearAlgebra.BLAS.get_num_threads() = ", BLAS.get_num_threads())
    println(io, "Relevant environment variables:")
    env_var_names = [
        "JULIA_NUM_THREADS",
        "MKL_DYNAMIC",
        "MKL_NUM_THREADS",
         # OpenBLAS has a hierarchy of environment variables for setting the
         # number of threads, see
         # https://github.com/xianyi/OpenBLAS/blob/c43ec53bdd00d9423fc609d7b7ecb35e7bf41b85/README.md#setting-the-number-of-threads-using-environment-variables
        ("OPENBLAS_NUM_THREADS", "GOTO_NUM_THREADS", "OMP_NUM_THREADS"),
    ]
    printed_at_least_one_env_var = false
    print_var(io, indent, name) = println(io, indent, name, " = ", ENV[name])
    for name in env_var_names
        if name isa Tuple
            # If `name` is a Tuple, then find the first environment which is
            # defined, and disregard the following ones.
            for nm in name
                if haskey(ENV, nm)
                    print_var(io, indent, nm)
                    printed_at_least_one_env_var = true
                    break
                end
            end
        else
            if haskey(ENV, name)
                print_var(io, indent, name)
                printed_at_least_one_env_var = true
            end
        end
    end
    if !printed_at_least_one_env_var
        println(io, indent, "[none]")
    end
    return nothing
end

function __init__()
    try
        verbose = parse(Bool, get(ENV, "LBT_VERBOSE", "false"))
        BLAS.lbt_forward(OpenBLAS_jll.libopenblas_path; clear=true, verbose)
        BLAS.check()
    catch ex
        Base.showerror_nostdio(ex, "WARNING: Error during initialization of module LinearAlgebra")
    end
    # register a hook to disable BLAS threading
    Base.at_disable_library_threading(() -> BLAS.set_num_threads(1))

    # https://github.com/xianyi/OpenBLAS/blob/c43ec53bdd00d9423fc609d7b7ecb35e7bf41b85/README.md#setting-the-number-of-threads-using-environment-variables
    if !haskey(ENV, "OPENBLAS_NUM_THREADS") && !haskey(ENV, "GOTO_NUM_THREADS") && !haskey(ENV, "OMP_NUM_THREADS")
        @static if Sys.isapple() && Base.BinaryPlatforms.arch(Base.BinaryPlatforms.HostPlatform()) == "aarch64"
            BLAS.set_num_threads(max(1, Sys.CPU_THREADS))
        else
            BLAS.set_num_threads(max(1, Sys.CPU_THREADS ÷ 2))
        end
    end
end

end # module LinearAlgebra
