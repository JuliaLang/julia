# This file is a part of Julia. License is MIT: http://julialang.org/license

module LinAlg

importall Base
import Base: USE_BLAS64, size, copy, copy_transpose!, power_by_squaring, print_matrix, transpose!

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
    sqrtm,
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
    logdet,
    lu,
    lufact,
    lufact!,
    lyap,
    norm,
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

typealias BlasFloat Union(Float64,Float32,Complex128,Complex64)
typealias BlasReal Union(Float64,Float32)
typealias BlasComplex Union(Complex128,Complex64)

if USE_BLAS64
    typealias BlasInt Int64
    blas_int(x) = Int64(x)
else
    typealias BlasInt Int32
    blas_int(x) = Int32(x)
end

#Check that stride of matrix/vector is 1
function chkstride1(A::StridedVecOrMat...)
    for a in A
        stride(a,1)== 1 || error("matrix does not have contiguous columns")
    end
end

#Check that matrix is square
function chksquare(A::AbstractMatrix)
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
    length(A)==1 ? sizes[1] : sizes
end

#Check that upper/lower (for special matrices) is correctly specified
macro chkuplo()
   :((uplo=='U' || uplo=='L') || throw(ArgumentError("""invalid uplo = $uplo

Valid choices are 'U' (upper) or 'L' (lower).""")))
end

const CHARU = 'U'
const CHARL = 'L'
char_uplo(uplo::Symbol) = uplo == :U ? CHARU : (uplo == :L ? CHARL : throw(ArgumentError("uplo argument must be either :U or :L")))

copy_oftype{T,N}(A::AbstractArray{T,N}, ::Type{T}) = copy(A)
copy_oftype{T,N,S}(A::AbstractArray{T,N}, ::Type{S}) = convert(AbstractArray{S,N}, A)

include("linalg/exceptions.jl")
include("linalg/generic.jl")

include("linalg/blas.jl")
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
include("linalg/rectfullpacked.jl")
include("linalg/givens.jl")
include("linalg/special.jl")
include("linalg/bitarray.jl")
include("linalg/ldlt.jl")

include("linalg/arpack.jl")
include("linalg/arnoldi.jl")

function __init__()
    Base.check_blas()
    if Base.blas_vendor() == :mkl
        ccall((:MKL_Set_Interface_Layer, Base.libblas_name), Void, (Cint,), USE_BLAS64 ? 1 : 0)
    end
end

end # module LinAlg
