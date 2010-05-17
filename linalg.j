libBLAS = dlopen("libBLAS")
libLAPACK = dlopen("libLAPACK")

typealias Ptr Pointer

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY) 

function copy (X::Vector{Float64})
    n = length(X)
    Y = zeros(Float64, n)
    ccall(dlsym(libBLAS, "dcopy_"),
          Void,
          (Ptr{Int32}, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32}),
          n, X, 1, Y, 1)
    return Y
end

function copy (X::Matrix{Float64})
    m = size(X,1)
    n = size(X,2)
    Y = zeros(Float64, m, n)
    ccall(dlsym(libBLAS, "dcopy_"),
          Void,
          (Ptr{Int32}, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32}),
          m*n, X, 1, Y, 1)
    return Y
end

function copy (X::Vector{Float32})
    n = length(X)
    Y = zeros(Float32, n)
    ccall(dlsym(libBLAS, "scopy_"),
          Void,
          (Ptr{Int32}, Ptr{Float32}, Ptr{Int32}, Ptr{Float32}, Ptr{Int32}),
          n, X, 1, Y, 1)
    return Y
end

function copy (X::Matrix{Float32})
    m = size(X,1)
    n = size(X,2)
    Y = zeros(Float32, m, n)
    ccall(dlsym(libBLAS, "scopy_"),
          Void,
          (Ptr{Int32}, Ptr{Float32}, Ptr{Int32}, Ptr{Float32}, Ptr{Int32}),
          m*n, X, 1, Y, 1)
    return Y
end

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)

function dot (x::Vector{Float64}, y::Vector{Float64})
    ccall(dlsym(libBLAS, "ddot_"),
          Float64, 
          (Ptr{Int32}, Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32}), 
          length(x), x, 1, y, 1)
end

function dot (x::Vector{Float32}, y::Vector{Float32})
    ccall(dlsym(libBLAS, "sdot_"),
          Float32, 
          (Ptr{Int32}, Ptr{Float32}, Ptr{Int32}, Ptr{Float32}, Ptr{Int32}), 
          length(x), x, 1, y, 1)
end


# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)

function norm (x::Vector{Float64})
    ccall(dlsym(libBLAS, "dnrm2_"),
          Float64,
          (Ptr{Int32}, Ptr{Float64}, Ptr{Int32}),
          length(x), x, 1)
end

function norm (x::Vector{Float32})
    ccall(dlsym(libBLAS, "snrm2_"),
          Float32,
          (Int32, Ptr{Float32}, Int32),
          length(x), x, 1)
end

#       SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)

function * (A::Matrix{Float64}, B::Matrix{Float64})
    m = size(A, 1)
    n = size(B, 2)
    k = size(A, 2)

    assert (k == size(B,1))
    C = zeros(Float64, m, n)

    ccall(dlsym(libBLAS, "dgemm_"),
          Void,
          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, 
           Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, 
           Ptr{Float64}, Ptr{Int32}, 
           Ptr{Float64}, Ptr{Float64}, Ptr{Int32}),
          "N", "N", m, n, k, 1.0, A, m, B, k, 0.0, C, m)

    return C
end

function * (A::Matrix{Float32}, B::Matrix{Float32})
    m = size(A, 1)
    n = size(B, 2)
    k = size(A, 2)

    assert (k == size(B,1))
    C = zeros(Float32, m, n)

    ccall(dlsym(libBLAS, "cblas_sgemm"),
          Void,
          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, 
           Ptr{Float32}, Ptr{Float32}, Ptr{Int32}, 
           Ptr{Float32}, Ptr{Int32}, 
           Ptr{Float32}, Ptr{Float32}, Ptr{Int32}),
          "N", "N", m, n, k, 1.0, A, m, B, k, 0.0, C, m)

    return C
end

# SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * )

function chol (A::Matrix{Float64})
    info = [0]
    n = size(A, 1)
    R = triu(A)
    ccall(dlsym(libLAPACK, "dpotrf_"),
          Int32,
          (Ptr{Uint8}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
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
          (Ptr{Uint8}, Ptr{Int32}, Ptr{Float32}, Ptr{Int32}, Ptr{Int32}),
          "U", n, R, n, info)
    if info[1] > 0; error("Matrix not Positive Definite"); end
    return R
end

#       SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )

function \ (A::Matrix{Float64}, B::Matrix{Float64})
    info = [0]
    n = size(A, 1)
    nrhs = size(B, 2)
    ipiv = ones(Int32, n)
    X = copy(B)
   ccall(dlsym(libLAPACK, "dgesv_"),
         Int32,
         (Ptr{Int32}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}, 
          Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
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
         (Ptr{Int32}, Ptr{Int32}, Ptr{Float32}, Ptr{Int32}, Ptr{Int32}, 
          Ptr{Float32}, Ptr{Int32}, Ptr{Int32}),
         n, nrhs, A, n, ipiv, X, n, info)
    if info[1] > 0; error("U is singular"); end
    return X
end

