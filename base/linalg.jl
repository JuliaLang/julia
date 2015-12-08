# This file is a part of Julia. License is MIT: http://julialang.org/license

module LinAlg

import Base: \, /, *, ^, +, -, ==, ./, .*
import Base: A_mul_Bt, At_ldiv_Bt, A_rdiv_Bc, At_ldiv_B, Ac_mul_Bc, A_mul_Bc, Ac_mul_B,
    Ac_ldiv_B, Ac_ldiv_Bc, At_mul_Bt, A_rdiv_Bt, At_mul_B
import Base: USE_BLAS64, abs, big, ceil, conj, convert, copy, copy!, copy_transpose!,
    ctranspose, ctranspose!, eltype, eye, findmax, findmin, fill!, floor, full, getindex,
    imag, inv, isapprox, kron, ndims, power_by_squaring, print_matrix, promote_rule, real,
    round, setindex!, show, similar, size, transpose, transpose!, trunc, unsafe_getindex,
    unsafe_setindex!
using Base: promote_op, MulFun

export
# Modules
    LAPACK,
    BLAS,

# Types
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
    cross,
    ctranspose,
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
    issym,
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
    rank,
    scale,
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
    Ac_mul_B,
    Ac_mul_B!,
    Ac_mul_Bc,
    Ac_mul_Bc!,
    Ac_rdiv_B,
    Ac_rdiv_Bc,
    At_ldiv_B,
    At_ldiv_Bt,
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
function chkstride1(A...)
    for a in A
        stride(a,1)== 1 || error("matrix does not have contiguous columns")
    end
end

# Check that matrix is square
function chksquare(A)
    m,n = size(A)
    m == n || throw(DimensionMismatch("matrix is not square"))
    m
end

function chksquare(A...)
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

include("linalg/exceptions.jl")
include("linalg/generic.jl")

include("linalg/blas.jl")
import .BLAS: gemv! # consider renaming gemv! in matmul
include("linalg/matmul.jl")
include("linalg/lapack.jl")

include("linalg/dense.jl")
include("linalg/tridiag.jl")
include("linalg/triangular.jl")

include("linalg/factorization.jl")
include("linalg/qr.jl")
include("linalg/eigen.jl")
include("linalg/svd.jl")
include("linalg/schur.jl")
include("linalg/cholesky.jl")
include("linalg/lu.jl")

include("linalg/bunchkaufman.jl")
include("linalg/symmetric.jl")
include("linalg/diagonal.jl")
include("linalg/bidiag.jl")
include("linalg/uniformscaling.jl")
include("linalg/givens.jl")
include("linalg/special.jl")
include("linalg/bitarray.jl")
include("linalg/ldlt.jl")

include("linalg/arpack.jl")
include("linalg/arnoldi.jl")

function __init__()
    try
        Base.check_blas()
        if Base.blas_vendor() == :mkl
            ccall((:MKL_Set_Interface_Layer, Base.libblas_name), Void, (Cint,), USE_BLAS64 ? 1 : 0)
        end
    catch ex
        Base.showerror_nostdio(ex,
            "WARNING: Error during initialization of module LinAlg")
    end
end

end # module LinAlg
