# This file is a part of Julia. License is MIT: http://julialang.org/license

module LinAlg

import Base: \, /, *, ^, +, -, ==
import Base: A_mul_Bt, At_ldiv_Bt, A_rdiv_Bc, At_ldiv_B, Ac_mul_Bc, A_mul_Bc, Ac_mul_B,
    Ac_ldiv_B, Ac_ldiv_Bc, At_mul_Bt, A_rdiv_Bt, At_mul_B
import Base: USE_BLAS64, abs, big, broadcast, ceil, conj, convert, copy, copy!,
    ctranspose, eltype, eye, findmax, findmin, fill!, floor, full, getindex,
    hcat, imag, indices, inv, isapprox, kron, length, linearindexing, map,
    ndims, oneunit, parent, power_by_squaring, print_matrix, promote_rule, real, round,
    setindex!, show, similar, size, transpose, trunc, typed_hcat
using Base: promote_op, _length, iszero, @pure, @propagate_inbounds, LinearFast,
    reduce, hvcat_fill, typed_vcat, promote_typeof
# We use `_length` because of non-1 indices; releases after julia 0.5
# can go back to `length`. `_length(A)` is equivalent to `length(linearindices(A))`.

export
# Modules
    LAPACK,
    BLAS,

# Types
    RowVector,
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
    bkfact,
    bkfact!,
    chol,
    cholfact,
    cholfact!,
    cond,
    condskeel,
    copy!,
    copy_transpose!,
    cross,
    ctranspose,
    ctranspose!,
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
    eigs,
    eigvals,
    eigvals!,
    eigvecs,
    expm,
    eye,
    factorize,
    givens,
    gradient,
    hessfact,
    hessfact!,
    isdiag,
    ishermitian,
    isposdef,
    isposdef!,
    issymmetric,
    istril,
    istriu,
    kron,
    ldltfact!,
    ldltfact,
    linreg,
    logabsdet,
    logdet,
    logm,
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
    sqrtm,
    svd,
    svdfact!,
    svdfact,
    svds,
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

typealias BlasFloat Union{Float64,Float32,Complex128,Complex64}
typealias BlasReal Union{Float64,Float32}
typealias BlasComplex Union{Complex128,Complex64}

if USE_BLAS64
    typealias BlasInt Int64
else
    typealias BlasInt Int32
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

# Example

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

copy_oftype{T,N}(A::AbstractArray{T,N}, ::Type{T}) = copy(A)
copy_oftype{T,N,S}(A::AbstractArray{T,N}, ::Type{S}) = convert(AbstractArray{S,N}, A)

include("transpose.jl")
include("rowvector.jl")

include("exceptions.jl")
include("generic.jl")

include("blas.jl")
import .BLAS: gemv! # consider renaming gemv! in matmul
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
include("schur.jl")
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

include("arpack.jl")
include("arnoldi.jl")

function __init__()
    try
        BLAS.check()
        if BLAS.vendor() == :mkl
            ccall((:MKL_Set_Interface_Layer, Base.libblas_name), Void, (Cint,), USE_BLAS64 ? 1 : 0)
        end
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module LinAlg")
    end
end

end # module LinAlg
