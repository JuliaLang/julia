# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Linear algebra module. Provides array arithmetic,
matrix factorizations and other linear algebra related
functionality.
"""
module LinearAlgebra

import Base: \, /, *, ^, +, -, ==
import Base: USE_BLAS64, abs, acos, acosh, acot, acoth, acsc, acsch, adjoint, asec, asech,
    asin, asinh, atan, atanh, axes, big, broadcast, ceil, cis, conj, convert, copy, copyto!, cos,
    cosh, cot, coth, csc, csch, eltype, exp, fill!, floor, getindex, hcat,
    getproperty, imag, inv, isapprox, isequal, isone, iszero, IndexStyle, kron, kron!, length, log, map, ndims,
    one, oneunit, parent, power_by_squaring, print_matrix, promote_rule, real, round, sec, sech,
    setindex!, show, similar, sin, sincos, sinh, size, sqrt,
    strides, stride, tan, tanh, transpose, trunc, typed_hcat, vec, zero
using Base: IndexLinear, promote_eltype, promote_op, promote_typeof,
    @propagate_inbounds, @pure, reduce, typed_hvcat, typed_vcat, require_one_based_indexing,
    splat
using Base.Broadcast: Broadcasted, broadcasted
import Libdl

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
    ColumnNorm,
    Eigen,
    GeneralizedEigen,
    GeneralizedSVD,
    GeneralizedSchur,
    Hessenberg,
    LU,
    LDLt,
    NoPivot,
    QR,
    QRPivoted,
    LQ,
    Schur,
    SVD,
    Hermitian,
    RowMaximum,
    Symmetric,
    LowerTriangular,
    UpperTriangular,
    UnitLowerTriangular,
    UnitUpperTriangular,
    UpperHessenberg,
    Diagonal,
    UniformScaling,

# Functions
    axpy!,
    axpby!,
    bunchkaufman,
    bunchkaufman!,
    cholesky,
    cholesky!,
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
    dot,
    eigen,
    eigen!,
    eigmax,
    eigmin,
    eigvals,
    eigvals!,
    eigvecs,
    factorize,
    givens,
    hessenberg,
    hessenberg!,
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
    ldlt!,
    ldlt,
    logabsdet,
    logdet,
    lowrankdowndate,
    lowrankdowndate!,
    lowrankupdate,
    lowrankupdate!,
    lu,
    lu!,
    lyap,
    mul!,
    lmul!,
    rmul!,
    norm,
    normalize,
    normalize!,
    nullspace,
    ordschur!,
    ordschur,
    pinv,
    qr,
    qr!,
    lq,
    lq!,
    opnorm,
    rank,
    rdiv!,
    reflect!,
    rotate!,
    schur,
    schur!,
    svd,
    svd!,
    svdvals!,
    svdvals,
    sylvester,
    tr,
    transpose,
    transpose!,
    tril,
    triu,
    tril!,
    triu!,

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


abstract type Algorithm end
struct DivideAndConquer <: Algorithm end
struct QRIteration <: Algorithm end

abstract type PivotingStrategy end
struct NoPivot <: PivotingStrategy end
struct RowMaximum <: PivotingStrategy end
struct ColumnNorm <: PivotingStrategy end

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
"""
rdiv!(A, B)



"""
    copy_oftype(A, T)

Copy `A` to a mutable array with eltype `T` based on `similar(A, T)`.

The resulting matrix typically has similar algebraic structure as `A`. For
example, supplying a tridiagonal matrix results in another tridiagonal matrix.
In general, the type of the output corresponds to that of `similar(A, T)`.

There are three often used methods in LinearAlgebra to create a mutable copy
of an array with a given eltype. These copies can be passed to in-place
algorithms (such as ldiv!, rdiv!, lu! and so on). Which one to use in practice
depends on what is known (or assumed) about the structure of the array in that
algorithm.

See also: `copy_similar`, `copy_to_array`.
"""
copy_oftype(A::AbstractArray, ::Type{T}) where {T} = copyto!(similar(A,T), A)

"""
    copy_similar(A, T)

Copy `A` to a mutable array with eltype `T` based on `similar(A, T, size(A))`.

Compared to `copy_oftype`, the result can be more flexible. For example,
supplying a tridiagonal matrix results in a sparse array. In general, the type
of the output corresponds to that of the three-argument method `similar(A, T, size(s))`.

See also: `copy_oftype`, `copy_to_array`.
"""
copy_similar(A::AbstractArray, ::Type{T}) where {T} = copyto!(similar(A, T, size(A)), A)

"""
    copy_to_array(A, T)

Copy `A` to a regular dense `Array` with element type `T`.

The resulting array is mutable. It can be used, for example, to pass the data of
`A` to an efficient in-place method for a matrix factorization such as `lu!`, in
cases where a more specific implementation of `lu!` (or `lu`) is not available.

See also: `copy_oftype`, `copy_similar`.
"""
copy_to_array(A::AbstractArray, ::Type{T}) where {T} = copyto!(Array{T}(undef, size(A)...), A)

# The three copy functions above return mutable arrays with eltype T.
# To only ensure a certain eltype, and if a mutable copy is not needed, it is
# more efficient to use:
# convert(AbstractArray{T}, A)


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
include("qr.jl")
include("lq.jl")
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
include("hessenberg.jl")
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

## convenience methods
## return only the solution of a least squares problem while avoiding promoting
## vectors to matrices.
_cut_B(x::AbstractVector, r::UnitRange) = length(x)  > length(r) ? x[r]   : x
_cut_B(X::AbstractMatrix, r::UnitRange) = size(X, 1) > length(r) ? X[r,:] : X

# SymTridiagonal ev can be the same length as dv, but the last element is
# ignored. However, some methods can fail if they read the entired ev
# rather than just the meaningful elements. This is a helper function
# for getting only the meaningful elements of ev. See #41089
_evview(S::SymTridiagonal) = @view S.ev[begin:length(S.dv) - 1]

## append right hand side with zeros if necessary
_zeros(::Type{T}, b::AbstractVector, n::Integer) where {T} = zeros(T, max(length(b), n))
_zeros(::Type{T}, B::AbstractMatrix, n::Integer) where {T} = zeros(T, max(size(B, 1), n), size(B, 2))

# General fallback definition for handling under- and overdetermined system as well as square problems
# While this definition is pretty general, it does e.g. promote to common element type of lhs and rhs
# which is required by LAPACK but not SuiteSpase which allows real-complex solves in some cases. Hence,
# we restrict this method to only the LAPACK factorizations in LinearAlgebra.
# The definition is put here since it explicitly references all the Factorizion structs so it has
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
function (\)(F::Union{<:LAPACKFactorizations,Adjoint{<:Any,<:LAPACKFactorizations}}, B::AbstractVecOrMat)
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
    invoke(\, Tuple{Factorization{T}, VecOrMat{Complex{T}}}, F, B)

"""
    LinearAlgebra.peakflops(n::Integer=2000; parallel::Bool=false)

`peakflops` computes the peak flop rate of the computer by using double precision
[`gemm!`](@ref LinearAlgebra.BLAS.gemm!). By default, if no arguments are specified, it
multiplies a matrix of size `n x n`, where `n = 2000`. If the underlying BLAS is using
multiple threads, higher flop rates are realized. The number of BLAS threads can be set with
[`BLAS.set_num_threads(n)`](@ref).

If the keyword argument `parallel` is set to `true`, `peakflops` is run in parallel on all
the worker processors. The flop rate of the entire parallel computer is returned. When
running in parallel, only 1 BLAS thread is used. The argument `n` still refers to the size
of the problem that is solved on each processor.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1. In Julia 1.0 it is available from
    the standard library `InteractiveUtils`.
"""
function peakflops(n::Integer=2000; parallel::Bool=false)
    a = fill(1.,100,100)
    t = @elapsed a2 = a*a
    a = fill(1.,n,n)
    t = @elapsed a2 = a*a
    @assert a2[1,1] == n
    if parallel
        let Distributed = Base.require(Base.PkgId(
                Base.UUID((0x8ba89e20_285c_5b6f, 0x9357_94700520ee1b)), "Distributed"))
            return sum(Distributed.pmap(peakflops, fill(n, Distributed.nworkers())))
        end
    else
        return 2*Float64(n)^3 / t
    end
end


function versioninfo(io::IO=stdout)
    config = BLAS.get_config()
    println(io, "BLAS: $(BLAS.libblastrampoline) ($(join(string.(config.build_flags), ", ")))")
    for lib in config.loaded_libs
        println(io, " --> $(lib.libname) ($(uppercase(string(lib.interface))))")
    end
    return nothing
end

function find_library_path(name)
    shlib_ext = string(".", Libdl.dlext)
    if !endswith(name, shlib_ext)
        name_ext = string(name, shlib_ext)
    end

    # On windows, we look in `bin` and never in `lib`
    @static if Sys.iswindows()
        path = joinpath(Sys.BINDIR, name_ext)
        isfile(path) && return path
    else
        # On other platforms, we check `lib/julia` first, and if that doesn't exist, `lib`.
        path = joinpath(Sys.BINDIR, Base.LIBDIR, "julia", name_ext)
        isfile(path) && return path

        path = joinpath(Sys.BINDIR, Base.LIBDIR, name_ext)
        isfile(path) && return path
    end

    # If we can't find it by absolute path, we'll try just passing this straight through to `dlopen()`
    return name
end

function __init__()
    try
        libblas_path = find_library_path(Base.libblas_name)
        liblapack_path = find_library_path(Base.liblapack_name)
        # We manually `dlopen()` these libraries here, so that we search with `libjulia-internal`'s
        # `RPATH` and not `libblastrampoline's`.  Once it's been opened, when LBT tries to open it,
        # it will find the library already loaded.
        libblas_path = Libdl.dlpath(Libdl.dlopen(libblas_path))
        BLAS.lbt_forward(libblas_path; clear=true)
        if liblapack_path != libblas_path
            liblapack_path = Libdl.dlpath(Libdl.dlopen(liblapack_path))
            BLAS.lbt_forward(liblapack_path)
        end
        BLAS.check()
    catch ex
        Base.showerror_nostdio(ex, "WARNING: Error during initialization of module LinearAlgebra")
    end
    # register a hook to disable BLAS threading
    Base.at_disable_library_threading(() -> BLAS.set_num_threads(1))
end

end # module LinearAlgebra
