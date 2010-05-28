libLAPACK = dlopen("libLAPACK")

# SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * )

function jl_gen_chol(fname, eltype)
    eval(`function chol (A::Matrix{$eltype})
         info = [0]
         n = size(A, 1)
         R = triu(A)
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               "U", n, R, n, info)
         if info[1] > 0; error("Matrix not Positive Definite"); end
         return R
         end
         )
end

jl_gen_chol("dpotrf_", Float64)
jl_gen_chol("spotrf_", Float32)

# SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, M, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * )

function jl_gen_lu(fname, eltype)
    eval(`function lu (A::Matrix{$eltype})
         info = [0]
         m = size(A, 1)
         n = size(A, 2)
         LU = copy(A)
         ipiv = Array(Int32, min(m,n))
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
               m, n, LU, m, ipiv, info)
         if info[1] > 0; error("Matrix is singular"); end
         P = 1:m
         for i=1:m; t = P[i]; P[i] = P[ipiv[i]]; P[ipiv[i]] = t ; end
         return (tril(LU, -1) + eye(m,n), triu(LU), P)
         end
         )
end

jl_gen_lu("dgetrf_", Float64)
jl_gen_lu("sgetrf_", Float32)

# SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LWORK, M, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            JPVT( * )
#       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )

# SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
# *
# *     .. Scalar Arguments ..
#       INTEGER            INFO, K, LDA, LWORK, M, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )

function jl_gen_qr(fname, fname2, eltype)
    eval(`function qr (A::Matrix{$eltype})
         info = [0]
         m = size(A, 1)
         n = size(A, 2)
         QR = copy(A)
         jpvt = zeros(Int32, n)
         k = min(m,n)
         tau = Array(Float64, k)

         # Workspace query for QR factorization
         work = [0.0]
         lwork = -1
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               m, n, QR, m, jpvt, tau, work, lwork, info)

         if info[1] == 0
            lwork = int32(work[1])
            work = Array(Float64, lwork)
         else
             error("Error in QR factorization")
         end

         # QR factorization
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               m, n, QR, m, jpvt, tau, work, lwork, info)

         if info[1] > 0; error("Matrix is singular"); end
         
         R = triu(QR)
         
         # Workspace query to form Q
         lwork2 = -1
         ccall(dlsym(libLAPACK, $fname2),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               m, k, k, QR, m, tau, work, lwork2, info)

         if info[1] == 0
            lwork2 = int32(work[1])
            if lwork2 > lwork; work = Array(Float64, lwork2); end
         else
             error("Error in QR factorization")
         end

         # Form Q
         ccall(dlsym(libLAPACK, $fname2),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               m, k, k, QR, m, tau, work, lwork2, info)
         
         return (QR[:, 1:k], R[1:k, :], jpvt)
         end
         )
end

jl_gen_qr("dgeqp3_", "dorgqr_", Float64)
jl_gen_qr("sgeqp3_", "sorgqr_", Float32)

# SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )

function jl_gen_mldivide(fname, eltype)
    eval(`function \ (A::Matrix{$eltype}, B::Matrix{$eltype})
        info = [0]
         n = size(A, 1)
         nrhs = size(B, 2)
         ipiv = Array(Int32, n)
         X = copy(B)
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}, 
                Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               n, nrhs, A, n, ipiv, X, n, info)
         if info[1] > 0; error("U is singular"); end
         return X
         end
         )
end

jl_gen_mldivide("dgesv_", Float64)
jl_gen_mldivide("sgesv_", Float32)
