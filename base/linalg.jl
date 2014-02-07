module LinAlg

importall Base
import Base: USE_BLAS64, size, copy, copy_transpose!, power_by_squaring, print_matrix

export 
# Modules
    LAPACK,
    BLAS,

# Types
    SymTridiagonal,
    Tridiagonal,
    Bidiagonal,
    Woodbury,
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
    LUTridiagonal,
    LDLTTridiagonal,
    QR,
    QRPivoted,
    Schur,
    SVD,
    Hermitian,
    Symmetric,
    Triangular,
    Diagonal,

# Functions
    axpy!,
    bkfact,
    bkfact!,
    check_blas,
    chol,
    cholfact,
    cholfact!,
    cholpfact,
    cholpfact!,
    cond,
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
    eigvecs,
    expm,
    sqrtm,
    eye,
    factorize,
    givens,
    gradient,
    hessfact,
    hessfact!,
    ishermitian,
    isposdef,
    isposdef!,
    issym,
    istril,
    istriu,
    kron,
    ldltd!,
    ldltd,
    linreg,
    logdet,
    lu,
    lufact,
    lufact!,
    norm,
    normfro,
    null,
    peakflops,
    pinv,
    qr,
    qrfact!,
    qrfact,
    qrp,
    qrpfact!,
    qrpfact,
    rank,
    rref,
    scale,
    scale!,
    schur,
    schurfact!,
    schurfact,
    solve,
    svd,
    svdfact!,
    svdfact,
    svdvals!,
    svdvals,
    trace,
    transpose,
    tril,
    triu,
    tril!,
    triu!,

# Operators
    \,
    /,
    A_ldiv_B!,
    A_ldiv_Bc,
    A_ldiv_Bt,
    A_mul_B,
    A_mul_B!,
    A_mul_Bc,
    A_mul_Bc!,
    A_mul_Bt,
    A_mul_Bt!,
    A_rdiv_Bc,
    A_rdiv_Bt,
    Ac_ldiv_B,
    Ac_ldiv_Bc,
    Ac_mul_b_RFP,
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
    At_rdiv_Bt

typealias BlasFloat Union(Float64,Float32,Complex128,Complex64)
typealias BlasReal Union(Float64,Float32)
typealias BlasComplex Union(Complex128,Complex64)
typealias BlasChar Char

if USE_BLAS64
    typealias BlasInt Int64
    blas_int(x) = int64(x)
else
    typealias BlasInt Int32
    blas_int(x) = int32(x)
end

#Check that stride of matrix/vector is 1
function chkstride1(A::StridedVecOrMat...)
    for a in A 
        stride(a,1)== 1 || error("Matrix does not have contiguous columns")
    end  
end

#Check that matrix is square
function chksquare(A...)
    sizes=Int[]
    for a in A 
        size(a,1)==size(a,2) || throw(DimensionMismatch("Matrix is not square: dimensions are $(size(a))"))
        push!(sizes, size(a,1))
    end
    length(A)==1 ? sizes[1] : sizes
end

#Check that upper/lower (for special matrices) is correctly specified
macro chkuplo()
   :((uplo=='U' || uplo=='L') || throw(ArgumentError("""invalid uplo = $uplo
            
Valid choices are 'U' (upper) or 'L' (lower).""")))
end

include("linalg/exceptions.jl")
include("linalg/generic.jl")

include("linalg/blas.jl")
include("linalg/matmul.jl")
include("linalg/lapack.jl")

include("linalg/dense.jl")
include("linalg/factorization.jl")

include("linalg/bunchkaufman.jl")
include("linalg/triangular.jl")
include("linalg/symmetric.jl")
include("linalg/woodbury.jl")
include("linalg/tridiag.jl")
include("linalg/diagonal.jl")
include("linalg/bidiag.jl")
include("linalg/rectfullpacked.jl")
include("linalg/givens.jl")
include("linalg/special.jl")
include("linalg/bitarray.jl")

include("linalg/sparse.jl")
include("linalg/umfpack.jl")
include("linalg/cholmod.jl")

include("linalg/arpack.jl")
include("linalg/arnoldi.jl")

function init()
    if Base.blas_vendor() == :mkl
        ccall((:MKL_Set_Interface_Layer, Base.libblas_name), Void, (Cint,), USE_BLAS64 ? 1 : 0)
    end
end

end # module LinAlg
