# CBLAS Storage Orders
#   enum CBLAS_ORDER {CblasRowMajor=101, CblasColMajor=102 };
#   enum CBLAS_TRANSPOSE {CblasNoTrans=111, CblasTrans=112, CblasConjTrans=113, AtlasConj=114};
#   enum CBLAS_UPLO  {CblasUpper=121, CblasLower=122};
#   enum CBLAS_DIAG  {CblasNonUnit=131, CblasUnit=132};
#   enum CBLAS_SIDE  {CblasLeft=141, CblasRight=142};

libBLAS = dlopen("libBLAS")
libLAPACK = dlopen("libLAPACK")

# void cblas_dcopy(const int N, const double *X, const int incX,
#                 double *Y, const int incY);

function copy (X::Vector{Float64})
    n = length(X)
    Y = zeros(Float64, n)
    ccall(dlsym(libBLAS, "cblas_dcopy"),
          Float64,
          (Int32, Pointer{Float64}, Int32, Pointer{Float64}, Int32),
          n, X, 1, Y, 1)
    return Y
end

function copy (X::Matrix{Float64})
    m = size(X,1)
    n = size(X,2)
    Y = zeros(Float64, m, n)
    ccall(dlsym(libBLAS, "cblas_dcopy"),
          Float64,
          (Int32, Pointer{Float64}, Int32, Pointer{Float64}, Int32),
          m*n, X, 1, Y, 1)
    return Y
end

function copy (X::Vector{Float32})
    n = length(X)
    Y = zeros(Float32, n)
    ccall(dlsym(libBLAS, "cblas_scopy"),
          Float32,
          (Int32, Pointer{Float32}, Int32, Pointer{Float32}, Int32),
          n, X, 1, Y, 1)
    return Y
end

function copy (X::Matrix{Float32})
    m = size(X,1)
    n = size(X,2)
    Y = zeros(Float32, m, n)
    ccall(dlsym(libBLAS, "cblas_scopy"),
          Float32,
          (Int32, Pointer{Float32}, Int32, Pointer{Float32}, Int32),
          m*n, X, 1, Y, 1)
    return Y
end


# double cblas_ddot(const int N, const double *X, const int incX,
#                   const double *Y, const int incY);

function dot (x::Vector{Float64}, y::Vector{Float64})
    ccall(dlsym(libBLAS, "cblas_ddot"),
          Float64, 
          (Int32, Pointer{Float64}, Int32, Pointer{Float64}, Int32), 
          length(x), x, 1, y, 1)
end

function dot (x::Vector{Float32}, y::Vector{Float32})
    ccall(dlsym(libBLAS, "cblas_sdot"),
          Float32, 
          (Int32, Pointer{Float32}, Int32, Pointer{Float32}, Int32), 
          length(x), x, 1, y, 1)
end


# double cblas_dnrm2(const int N, const double *X, const int incX);

function norm (x::Vector{Float64})
    ccall(dlsym(libBLAS, "cblas_dnrm2"),
          Float64,
          (Int32, Pointer{Float64}, Int32),
          length(x), x, 1)
end

function norm (x::Vector{Float32})
    ccall(dlsym(libBLAS, "cblas_snrm2"),
          Float32,
          (Int32, Pointer{Float32}, Int32),
          length(x), x, 1)
end

# void cblas_dgemm(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TransA,
#                  const enum CBLAS_TRANSPOSE TransB, const int M, const int N,
#                  const int K, const double alpha, const double *A,
#                  const int lda, const double *B, const int ldb,
#                  const double beta, double *C, const int ldc);

function * (A::Matrix{Float64}, B::Matrix{Float64})
    m = size(A, 1)
    n = size(B, 2)
    k = size(A, 2)

    assert (k == size(B,1))
    C = zeros(Float64, m, n)

    ccall(dlsym(libBLAS, "cblas_dgemm"),
          Int32,
          (Int32, Int32, Int32, Int32, Int32, Int32, 
           Float64, Pointer{Float64}, Int32, 
           Pointer{Float64}, Int32, 
           Float64, Pointer{Float64}, Int32),
          102, 111, 111, m, n, k, 1.0, A, m, B, k, 0.0, C, m)

    return C
end

function * (A::Matrix{Float32}, B::Matrix{Float32})
    m = size(A, 1)
    n = size(B, 2)
    k = size(A, 2)

    assert (k == size(B,1))
    C = zeros(Float32, m, n)

    ccall(dlsym(libBLAS, "cblas_sgemm"),
          Int32,
          (Int32, Int32, Int32, Int32, Int32, Int32, 
           Float32, Pointer{Float32}, Int32, 
           Pointer{Float32}, Int32, 
           Float32, Pointer{Float32}, Int32),
          102, 111, 111, m, n, k, 1.0, A, m, B, k, 0.0, C, m)

    return C
end

# int dpotrf_(char *uplo, __CLPK_integer *n, __CLPK_doublereal *a, __CLPK_integer *
#             lda, __CLPK_integer *info);

function chol (A::Matrix{Float64})
    info = [0]
    n = size(A, 1)
    R = triu(A)
    ccall(dlsym(libLAPACK, "dpotrf_"),
          Int32,
          (Pointer{Uint8}, Pointer{Int32}, Pointer{Float64}, Pointer{Int32}, Pointer{Int32}),
          "U", n, R, n, info)
    if info[1] > 0; error("Matrix not Positive Definite"); end
    return R
end

function chol (A::Matrix{Float32})
    info = [0]
    n = size(A, 1)
    R = triu(A)
    ccall(dlsym(libLAPACK, "spotrf_"),
          Int32,
          (Pointer{Uint8}, Pointer{Int32}, Pointer{Float32}, Pointer{Int32}, Pointer{Int32}),
          "U", n, R, n, info)
    if info[1] > 0; error("Matrix not Positive Definite"); end
    return R
end

# int dgesv_(__CLPK_integer *n, __CLPK_integer *nrhs, __CLPK_doublereal *a, __CLPK_integer 
#            *lda, __CLPK_integer *ipiv, __CLPK_doublereal *b, __CLPK_integer *ldb, __CLPK_integer *info);

function \ (A::Matrix{Float64}, B::Matrix{Float64})
    info = [0]
    n = size(A, 1)
    nrhs = size(B, 2)
    ipiv = ones(Int32, n)
    X = copy(B)
   ccall(dlsym(libLAPACK, "dgesv_"),
         Int32,
         (Pointer{Int32}, Pointer{Int32}, Pointer{Float64}, Pointer{Int32}, Pointer{Int32}, 
          Pointer{Float64}, Pointer{Int32}, Pointer{Int32}),
         n, nrhs, A, n, ipiv, X, n, info)
    if info[1] > 0; error("U is singular"); end
    return X
end

function \ (A::Matrix{Float32}, B::Matrix{Float32})
    info = [0]
    n = size(A, 1)
    nrhs = size(B, 2)
    ipiv = ones(Int32, n)
    X = copy(B)
   ccall(dlsym(libLAPACK, "sgesv_"),
         Int32,
         (Pointer{Int32}, Pointer{Int32}, Pointer{Float32}, Pointer{Int32}, Pointer{Int32}, 
          Pointer{Float32}, Pointer{Int32}, Pointer{Int32}),
         n, nrhs, A, n, ipiv, X, n, info)
    if info[1] > 0; error("U is singular"); end
    return X
end

