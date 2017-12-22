# This file is a part of Julia. License is MIT: https://julialang.org/license

# shims to maintain existence of names in Base module in A_mul_B deprecation process
function Ac_ldiv_Bt end
function At_ldiv_Bt end
function A_ldiv_Bt end
function At_ldiv_B end
function Ac_ldiv_Bc end
function A_ldiv_Bc end
function Ac_ldiv_B end
function At_rdiv_Bt end
function A_rdiv_Bt end
function At_rdiv_B end
function Ac_rdiv_Bc end
function A_rdiv_Bc end
function Ac_rdiv_B end
function At_mul_Bt end
function A_mul_Bt end
function At_mul_B end
function Ac_mul_Bc end
function A_mul_Bc end
function Ac_mul_B end

"""
Linear algebra module. Provides array arithmetic,
matrix factorizations and other linear algebra related
functionality.
"""
module LinAlg

import Base: \, /, *, ^, +, -, ==
import Base: A_mul_Bt, At_ldiv_Bt, A_rdiv_Bc, At_ldiv_B, Ac_mul_Bc, A_mul_Bc, Ac_mul_B,
    Ac_ldiv_B, Ac_ldiv_Bc, At_mul_Bt, A_rdiv_Bt, At_mul_B
import Base: USE_BLAS64, abs, acos, acosh, acot, acoth, acsc, acsch, adjoint, asec, asech,
    asin, asinh, atan, atanh, axes, big, broadcast, ceil, conj, convert, copy, copyto!, cos,
    cosh, cot, coth, csc, csch, eltype, exp, findmax, findmin, fill!, floor, getindex, hcat,
    getproperty, imag, inv, isapprox, isone, IndexStyle, kron, length, log, map, ndims,
    oneunit, parent, power_by_squaring, print_matrix, promote_rule, real, round, sec, sech,
    setindex!, show, similar, sin, sincos, sinh, size, sqrt, tan, tanh, transpose, trunc,
    typed_hcat, vec
using Base: hvcat_fill, iszero, IndexLinear, _length, promote_op, promote_typeof,
    @propagate_inbounds, @pure, reduce, typed_vcat
# We use `_length` because of non-1 indices; releases after julia 0.5
# can go back to `length`. `_length(A)` is equivalent to `length(linearindices(A))`.

export
# Modules
    LAPACK,
    BLAS,

# Types
    Adjoint,
    Transpose,
    RowVector,
    ConjArray,
    ConjVector,
    ConjMatrix,
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
    ldltfact!,
    ldltfact,
    linreg,
    logabsdet,
    logdet,
    lu,
    lufact,
    lufact!,
    lyap,
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
    scale!,
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
    A_ldiv_B!,
    A_ldiv_Bc,
    A_ldiv_Bt,
    A_mul_B!,
    A_mul_Bc,
    A_mul_Bc!,
    A_mul_Bt,
    A_mul_Bt!,
    A_rdiv_Bc,
    A_rdiv_Bt,
    Ac_ldiv_B,
    Ac_ldiv_Bc,
    Ac_ldiv_B!,
    Ac_mul_B,
    Ac_mul_B!,
    Ac_mul_Bc,
    Ac_mul_Bc!,
    Ac_rdiv_B,
    Ac_rdiv_Bc,
    At_ldiv_B,
    At_ldiv_Bt,
    At_ldiv_B!,
    At_mul_B,
    At_mul_B!,
    At_mul_Bt,
    At_mul_Bt!,
    At_rdiv_B,
    At_rdiv_Bt,

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

# Check that stride of matrix/vector is 1
# Writing like this to avoid splatting penalty when called with multiple arguments,
# see PR 16416
@inline chkstride1(A...) = _chkstride1(true, A...)
@noinline _chkstride1(ok::Bool) = ok || error("matrix does not have contiguous columns")
@inline _chkstride1(ok::Bool, A, B...) = _chkstride1(ok & (stride(A, 1) == 1), B...)

"""
    LinAlg.checksquare(A)

Check that a matrix is square, then return its common dimension.
For multiple arguments, return a vector.

# Examples
```jldoctest
julia> A = ones(4,4); B = zeros(5,5);

julia> LinAlg.checksquare(A, B)
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

# shims to maintain existence of names in LinAlg module in A_mul_B deprecation process
function A_mul_B! end
function Ac_mul_B! end
function Ac_mul_B! end
function At_mul_B! end
function A_ldiv_B! end
function At_ldiv_B! end
function Ac_ldiv_B! end
function A_rdiv_B! end
function A_rdiv_Bc! end

copy_oftype(A::AbstractArray{T}, ::Type{T}) where {T} = copy(A)
copy_oftype(A::AbstractArray{T,N}, ::Type{S}) where {T,N,S} = convert(AbstractArray{S,N}, A)

include("adjtrans.jl")
include("conjarray.jl")
include("transpose.jl")
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


function __init__()
    try
        BLAS.check()
        if BLAS.vendor() == :mkl
            ccall((:MKL_Set_Interface_Layer, Base.libblas_name), Cvoid, (Cint,), USE_BLAS64 ? 1 : 0)
        end
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module LinAlg")
    end
end

end # module LinAlg
