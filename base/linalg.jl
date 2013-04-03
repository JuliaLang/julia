module LinAlg

importall Base
import Base.USE_LIB64, Base.size, Base.copy, Base.copy_transpose!, Base.power_by_squaring

export 
# Types
    BunchKaufman,
    SymTridiagonal,
    Tridiagonal,
    Bidiagonal,
    Woodbury,
    Factorization,
    BunchKaufman,
    Cholesky,
    CholeskyPivoted,
    Eigen,
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
    Triangular,
    Diagonal,

# Functions
    check_openblas,
    chol,
    cholfact,
    cholfact!,
    cholp,
    cholpfact,
    cholpfact!,
    cond,
    copy!,
    cross,
    ctranspose,
    det,
    diag,
    diagm,
    diagmm,
    diagmm!,
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
    factors,
    hess,
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
    pinv,
    qr,
    qrfact!,
    qrfact,
    qrp,
    qrpfact!,
    qrpfact,
    qmulQR,
    qTmulQR,
    randsym,
    rank,
    rref,
    scale!,
    schur,
    schurfact!,
    schurfact,
    solve,
    svd,
    svdfact!,
    svdfact,
    svds,
    svdvals!,
    svdvals,
    symmetrize!,
    trace,
    transpose,
    tril,
    triu,
    tril!,
    triu!,

# Operators
    \,
    /,
    A_ldiv_Bc,
    A_ldiv_Bt,
    A_mul_B,
    A_mul_Bc,
    A_mul_Bt,
    A_rdiv_Bc,
    A_rdiv_Bt,
    Ac_ldiv_B,
    Ac_ldiv_Bc,
    Ac_mul_b_RFP,
    Ac_mul_B,
    Ac_mul_Bc,
    Ac_rdiv_B,
    Ac_rdiv_Bc,
    At_ldiv_B,
    At_ldiv_Bt,
    At_mul_B,
    At_mul_Bt,
    At_rdiv_B,
    At_rdiv_Bt


typealias BlasFloat Union(Float64,Float32,Complex128,Complex64)
typealias BlasChar Char

function check_openblas()
    libblas = dlopen( Base.libblas_name )
    if dlsym_e( libblas, :openblas_get_config ) != C_NULL
        openblas_config = bytestring( ccall((:openblas_get_config, Base.libblas_name), Ptr{Uint8}, () ))
        openblas64 = ismatch(r".*USE64BITINT.*", openblas_config)
        if Base.USE_LIB64 != openblas64
            println("OpenBLAS is incorrectly configured. Quitting.")
            quit()
        end
    end
end

if USE_LIB64
    typealias BlasInt Int64
    blas_int(x) = int64(x)
else
    typealias BlasInt Int32
    blas_int(x) = int32(x)
end

include("linalg/generic.jl")

include("linalg/blas.jl")
include("linalg/matmul.jl")
include("linalg/lapack.jl")

include("linalg/dense.jl")
include("linalg/factorization.jl")

include("linalg/bunchkaufman.jl")
include("linalg/triangular.jl")
include("linalg/hermitian.jl")
include("linalg/woodbury.jl")
include("linalg/tridiag.jl")
include("linalg/bidiag.jl")
include("linalg/diagonal.jl")
include("linalg/rectfullpacked.jl")

include("linalg/bitarray.jl")

include("linalg/sparse.jl")
include("linalg/umfpack.jl")
include("linalg/cholmod.jl")

include("linalg/arpack.jl")
include("linalg/arnoldi.jl")

end # module LinAlg
