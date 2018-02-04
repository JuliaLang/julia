# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

"""
Linear algebra module. Provides array arithmetic,
matrix factorizations and other linear algebra related
functionality.
"""
module LinearAlgebra

import Base: \, /, *, ^, +, -, ==
import Base: USE_BLAS64, abs, acos, acosh, acot, acoth, acsc, acsch, adjoint, asec, asech,
    asin, asinh, atan, atanh, axes, big, broadcast, ceil, conj, convert, copy, copyto!, cos,
    cosh, cot, coth, csc, csch, eltype, exp, findmax, findmin, fill!, floor, getindex, hcat,
    getproperty, imag, inv, isapprox, isone, IndexStyle, kron, length, log, map, ndims,
    oneunit, parent, power_by_squaring, print_matrix, promote_rule, real, round, sec, sech,
    setindex!, show, similar, sin, sincos, sinh, size, size_to_strides, sqrt, StridedReinterpretArray,
    StridedReshapedArray, ReshapedArray, ReinterpretArray, strides, stride, tan, tanh, transpose, trunc, typed_hcat, vec,
    MemoryLayout, UnknownLayout
using Base: hvcat_fill, iszero, IndexLinear, _length, promote_op, promote_typeof,
    @propagate_inbounds, @pure, reduce, typed_vcat, AbstractCartesianIndex, RangeIndex, Slice
# We use `_length` because of non-1 indices; releases after julia 0.5
# can go back to `length`. `_length(A)` is equivalent to `length(linearindices(A))`.

export
# Modules
    LAPACK,
    BLAS,

# Types
    Adjoint,
    Transpose,
    SymTridiagonal,
    Tridiagonal,
    Bidiagonal,
    Factorization,
    BunchKaufman,
    Cholesky,
    CholeskyPivoted,
    Eigen,
    GeneralizedEigen,
    GeneralizedSVD,
    GeneralizedSchur,
    Hessenberg,
    LU,
    LDLt,
    QR,
    QRPivoted,
    LQ,
    Schur,
    SVD,
    Hermitian,
    Symmetric,
    LowerTriangular,
    UpperTriangular,
    UnitLowerTriangular,
    UnitUpperTriangular,
    Diagonal,
    UniformScaling,

# Functions
    axpy!,
    axpby!,
    bkfact,
    bkfact!,
    chol,
    cholfact,
    cholfact!,
    cond,
    condskeel,
    copyto!,
    copy_transpose!,
    cross,
    adjoint,
    adjoint!,
    det,
    diag,
    diagind,
    diagm,
    diff,
    dot,
    eig,
    eigfact,
    eigfact!,
    eigmax,
    eigmin,
    eigvals,
    eigvals!,
    eigvecs,
    factorize,
    givens,
    hessfact,
    hessfact!,
    isdiag,
    ishermitian,
    isposdef,
    isposdef!,
    issuccess,
    issymmetric,
    istril,
    istriu,
    kron,
    ldiv!,
    ldltfact!,
    ldltfact,
    linreg,
    logabsdet,
    logdet,
    lu,
    lufact,
    lufact!,
    lyap,
    mul!,
    mul1!,
    mul2!,
    norm,
    normalize,
    normalize!,
    nullspace,
    ordschur!,
    ordschur,
    peakflops,
    pinv,
    qr,
    qrfact!,
    qrfact,
    lq,
    lqfact!,
    lqfact,
    rank,
    rdiv!,
    schur,
    schurfact!,
    schurfact,
    svd,
    svdfact!,
    svdfact,
    svdvals!,
    svdvals,
    sylvester,
    trace,
    transpose,
    transpose!,
    transpose_type,
    tril,
    triu,
    tril!,
    triu!,
    vecdot,
    vecnorm,

# Operators
    \,
    /,

# Constants
    I

const BlasFloat = Union{Float64,Float32,ComplexF64,ComplexF32}
const BlasReal = Union{Float64,Float32}
const BlasComplex = Union{ComplexF64,ComplexF32}

if USE_BLAS64
    const BlasInt = Int64
else
    const BlasInt = Int32
end

## Traits for memory layouts ##
abstract type AbstractStridedLayout{T} <: MemoryLayout{T} end
abstract type DenseColumns{T} <: AbstractStridedLayout{T} end
struct DenseColumnMajor{T} <: DenseColumns{T} end
struct DenseColumnsStridedRows{T} <: DenseColumns{T} end
abstract type DenseRows{T} <: AbstractStridedLayout{T} end
struct DenseRowMajor{T} <: DenseRows{T} end
struct DenseRowsStridedColumns{T} <: DenseRows{T} end
struct StridedLayout{T} <: AbstractStridedLayout{T} end

"""
    AbstractStridedLayout{T}

is an abstract type whose subtypes are returned by `MemoryLayout(A)`
if a matrix or vector `A` have storage laid out at regular offsets in memory,
and which can therefore be passed to external C and Fortran functions expecting
this memory layout.
"""
AbstractStridedLayout

"""
    DenseColumnMajor{T}()

is returned by `MemoryLayout(A)` if a vector or matrix `A` has storage in memory
equivalent to an `Array`, so that `stride(A,1) == 1` and `stride(A,2) == size(A,1)`.
Arrays with `DenseColumnMajor` must conform to the `DenseArray` interface.
"""
DenseColumnMajor

"""
    DenseColumnsStridedRows{T}()

is returned by `MemoryLayout(A)` if a vector or matrix `A` has storage in memory
as a column major matrix. In other words, the columns are stored in memory with
offsets of one, while the rows are stored with offsets given by `stride(A,2)`.
Arrays with `DenseColumnsStridedRows` must conform to the `DenseArray` interface.
"""
DenseColumnsStridedRows

"""
    DenseRowMajor{T}()

is returned by `MemoryLayout(A)` if a vector or matrix `A` has storage in memory
equivalent to the transpose of an `Array`, so that `stride(A,1) == size(A,1)` and
`stride(A,2) == 1`. Arrays with `DenseRowMajor` must conform to the
`DenseArray` interface.
"""
DenseRowMajor

"""
    DenseRowsStridedColumns{T}()

is returned by `MemoryLayout(A)` if a matrix `A` has storage in memory
as a row major matrix. In other words, the rows are stored in memory with
offsets of one, while the columns are stored with offsets given by `stride(A,1)`.
`Array`s with `DenseRowsStridedColumns` must conform to the `DenseArray` interface,
and `transpose(A)` should return a matrix whose layout is `DenseColumnsStridedRows{T}()`.
"""
DenseRowsStridedColumns

"""
    StridedLayout{T}()

is returned by `MemoryLayout(A)` if a vector or matrix `A` has storage laid out at regular
offsets in memory. In other words, the columns are stored with offsets given
by `stride(A,1)` and for matrices the rows are stored in memory with offsets
of `stride(A,2)`. `Array`s with `StridedLayout` must conform to the `DenseArray` interface.
"""
StridedLayout

MemoryLayout(A::Vector{T}) where T = DenseColumnMajor{T}()
MemoryLayout(A::Matrix{T}) where T = DenseColumnMajor{T}()
MemoryLayout(A::DenseArray{T}) where T = StridedLayout{T}()

MemoryLayout(A::SubArray) = submemorylayout(MemoryLayout(parent(A)), parentindices(A))
submemorylayout(::MemoryLayout{T}, _) where T = UnknownLayout{T}()
submemorylayout(::DenseColumns{T}, ::Tuple{I}) where {T,I<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseColumnMajor{T}()
submemorylayout(::AbstractStridedLayout{T}, ::Tuple{I}) where {T,I<:Union{RangeIndex,AbstractCartesianIndex}} =
    StridedLayout{T}()
submemorylayout(::DenseColumns{T}, ::Tuple{I,Int}) where {T,I<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseColumnMajor{T}()
submemorylayout(::DenseColumns{T}, ::Tuple{I,Int}) where {T,I<:Slice} =
    DenseColumnMajor{T}()
submemorylayout(::DenseRows{T}, ::Tuple{Int,I}) where {T,I<:Union{AbstractUnitRange{Int},Int,AbstractCartesianIndex}} =
    DenseColumnMajor{T}()
submemorylayout(::DenseRows{T}, ::Tuple{Int,I}) where {T,I<:Slice} =
    DenseColumnMajor{T}()
submemorylayout(::DenseColumnMajor{T}, ::Tuple{I1,I2}) where {T,I1<:Slice,I2<:AbstractUnitRange{Int}} =
    DenseColumnMajor{T}()
submemorylayout(::DenseColumnMajor{T}, ::Tuple{I1,I2}) where {T,I1<:AbstractUnitRange{Int},I2<:AbstractUnitRange{Int}} =
    DenseColumnsStridedRows{T}()
submemorylayout(::DenseColumns{T}, ::Tuple{I1,I2}) where {T,I1<:AbstractUnitRange{Int},I2<:AbstractUnitRange{Int}} =
    DenseColumnsStridedRows{T}()
submemorylayout(::DenseRows{T}, ::Tuple{I1,I2}) where {T,I1<:AbstractUnitRange{Int},I2<:Slice} =
    DenseRowMajor{T}()
submemorylayout(::DenseRows{T}, ::Tuple{I1,I2}) where {T,I1<:AbstractUnitRange{Int},I2<:AbstractUnitRange{Int}} =
    DenseRowsStridedColumns{T}()
submemorylayout(::AbstractStridedLayout{T}, ::Tuple{I1,I2}) where {T,I1<:Union{RangeIndex,AbstractCartesianIndex},I2<:Union{RangeIndex,AbstractCartesianIndex}} =
    StridedLayout{T}()

MemoryLayout(A::ReshapedArray) = reshapedmemorylayout(MemoryLayout(parent(A)))
reshapedmemorylayout(::MemoryLayout{T}) where T = UnknownLayout{T}()
reshapedmemorylayout(::DenseColumnMajor{T}) where T = DenseColumnMajor{T}()

MemoryLayout(A::ReinterpretArray{V}) where V = reinterpretedmemorylayout(MemoryLayout(parent(A)), V)
reinterpretedmemorylayout(::MemoryLayout{T}, ::Type{V}) where {T,V} = UnknownLayout{V}()
reinterpretedmemorylayout(::DenseColumnMajor{T}, ::Type{V}) where {T,V} = DenseColumnMajor{V}()

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
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> LinearAlgebra.stride1(A)
1

julia> B = view(A, 2:2:4)
2-element view(::Array{Int64,1}, 2:2:4) with eltype Int64:
 2
 4

julia> LinearAlgebra.stride1(B)
2
```
"""
stride1(x) = stride(x,1)
stride1(x::AbstractArray) = _stride1(x, MemoryLayout(x))
_stride1(x, _) = stride(x, 1)::Int
_stride1(x, ::DenseColumns) = 1

@inline chkstride1(A...) = _chkstride1(true, A...)
@noinline _chkstride1(ok::Bool) = ok || error("matrix does not have contiguous columns")
@inline _chkstride1(ok::Bool, A, B...) = _chkstride1(ok & (stride1(A) == 1), B...)

"""
    LinearAlgebra.checksquare(A)

Check that a matrix is square, then return its common dimension.
For multiple arguments, return a vector.

# Examples
```jldoctest
julia> A = fill(1, (4,4)); B = fill(1, (5,5));

julia> LinearAlgebra.checksquare(A, B)
2-element Array{Int64,1}:
 4
 5
```
"""
function checksquare(A)
    m,n = size(A)
    m == n || throw(DimensionMismatch("matrix is not square: dimensions are $(size(A))"))
    m
end

function checksquare(A...)
    sizes = Int[]
    for a in A
        size(a,1)==size(a,2) || throw(DimensionMismatch("matrix is not square: dimensions are $(size(a))"))
        push!(sizes, size(a,1))
    end
    return sizes
end

function char_uplo(uplo::Symbol)
    if uplo == :U
        'U'
    elseif uplo == :L
        'L'
    else
        throw(ArgumentError("uplo argument must be either :U (upper) or :L (lower)"))
    end
end

"""
    ldiv!(Y, A, B) -> Y

Compute `A \\ B` in-place and store the result in `Y`, returning the result.

The argument `A` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholfact`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lufact!`](@ref)),
and performance-critical situations requiring `ldiv!` usually also require fine-grained
control over the factorization of `A`.
"""
ldiv!(Y, A, B)

"""
    ldiv!(A, B)

Compute `A \\ B` in-place and overwriting `B` to store the result.

The argument `A` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholfact`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lufact!`](@ref)),
and performance-critical situations requiring `ldiv!` usually also require fine-grained
control over the factorization of `A`.
"""
ldiv!(A, B)


"""
    rdiv!(A, B)

Compute `A / B` in-place and overwriting `A` to store the result.

The argument `B` should *not* be a matrix.  Rather, instead of matrices it should be a
factorization object (e.g. produced by [`factorize`](@ref) or [`cholfact`](@ref)).
The reason for this is that factorization itself is both expensive and typically allocates memory
(although it can also be done in-place via, e.g., [`lufact!`](@ref)),
and performance-critical situations requiring `rdiv!` usually also require fine-grained
control over the factorization of `B`.
"""
rdiv!(A, B)

copy_oftype(A::AbstractArray{T}, ::Type{T}) where {T} = copy(A)
copy_oftype(A::AbstractArray{T,N}, ::Type{S}) where {T,N,S} = convert(AbstractArray{S,N}, A)

include("adjtrans.jl")
include("transpose.jl")
include("conjarray.jl")
include("rowvector.jl")

include("exceptions.jl")
include("generic.jl")

include("blas.jl")
include("matmul.jl")
include("lapack.jl")

include("dense.jl")
include("tridiag.jl")
include("triangular.jl")

include("factorization.jl")
include("qr.jl")
include("hessenberg.jl")
include("lq.jl")
include("eigen.jl")
include("svd.jl")
include("symmetric.jl")
include("cholesky.jl")
include("lu.jl")
include("bunchkaufman.jl")
include("diagonal.jl")
include("bidiag.jl")
include("uniformscaling.jl")
include("givens.jl")
include("special.jl")
include("bitarray.jl")
include("ldlt.jl")
include("schur.jl")
include("deprecated.jl")

const ⋅ = dot
const × = cross
export ⋅, ×


function versioninfo(io::IO=STDOUT)
    if Base.libblas_name == "libopenblas" || BLAS.vendor() == :openblas || BLAS.vendor() == :openblas64
        openblas_config = BLAS.openblas_get_config()
        println(io, "BLAS: libopenblas (", openblas_config, ")")
    else
        println(io, "BLAS: ",Base.libblas_name)
    end
    println(io, "LAPACK: ",Base.liblapack_name)
end

function __init__()
    try
        BLAS.check()
        if BLAS.vendor() == :mkl
            ccall((:MKL_Set_Interface_Layer, Base.libblas_name), Cvoid, (Cint,), USE_BLAS64 ? 1 : 0)
        end
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module LinearAlgebra")
    end
end

end # module LinearAlgebra
