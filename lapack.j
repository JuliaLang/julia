libLAPACK = dlopen("libLAPACK")

# SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * )

for (fname, eltype) = (("dpotrf_", Float64), ("spotrf_", Float32))
    eval(`function chol (A::Matrix{$eltype})
         info = [0]
         n = size(A, 1)
         R = triu(A)

         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Char}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               "U", n, R, n, info)

         if info[1] == 0; return R; end
         if info[1] > 0; error("Matrix not Positive Definite"); end
         error("Error in CHOL")
         end
         )
end

# SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, M, N
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * )

for (fname, eltype) = (("dgetrf_", Float64), ("sgetrf_", Float32))
    eval(`function lu (A::Matrix{$eltype})
         info = [0]
         m = size(A, 1)
         n = size(A, 2)
         LU = copy(A)
         ipiv = Array(Int32, min(m,n))

         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
               m, n, LU, m, ipiv, info)

         if info[1] > 0; error("Matrix is singular"); end
         P = 1:m
         for i=1:m; t = P[i]; P[i] = P[ipiv[i]]; P[ipiv[i]] = t ; end

         if info[1] == 0; return (tril(LU, -1) + eye(m,n), triu(LU), P); end
         error("Error in LU")
         end
         )
end

# SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LWORK, M, N
# *     .. Array Arguments ..
#       INTEGER            JPVT( * )
#       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )

# SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, K, LDA, LWORK, M, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )

for (fname, fname2, eltype) = (("dgeqp3_", "dorgqr_", Float64),
                               ("sgeqp3_", "sorgqr_", Float32))
    eval(`function qr (A::Matrix{$eltype})
         info = [0]
         m = size(A, 1)
         n = size(A, 2)
         QR = copy(A)
         jpvt = zeros(Int32, n)
         k = min(m,n)
         tau = Array($eltype, k)

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
            work = Array($eltype, lwork)
         else
             error("Error in QR factorization")
         end

         # Compute QR factorization
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
            if lwork2 > lwork; work = Array($eltype, lwork2); end
         else
             error("Error in QR factorization")
         end

         # Compute Q
         ccall(dlsym(libLAPACK, $fname2),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               m, k, k, QR, m, tau, work, lwork2, info)
         
         if info[1] == 0; return (QR[:, 1:k], R[1:k, :], jpvt); end
         error("Error in QR");
         end
         )
end

# SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )

for (fname, eltype) = (("dgesv_", Float64), ("sgesv_", Float32))
    eval(`function \ (A::Matrix{$eltype}, B::VectorOrMatrix{$eltype})
        info = [0]
         n = size(A, 1)
         if isa(B, Vector); nrhs = 1; else nrhs = size(B, 2); end
         ipiv = Array(Int32, n)
         X = copy(B)

         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}, 
                Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               n, nrhs, copy(A), n, ipiv, X, n, info)

         if info[1] == 0; return X; end
         if info[1] > 0; error("U is singular"); end
         end
         )
end

#       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )

for (fname, eltype) = (("dsyev_", Float64), ("ssyev_", Float32))
    eval(`function eig(A::Matrix{$eltype})

         if !issymmetric(A); error("Matrix must be symmetric"); end

         jobz = "V"
         uplo = "U"
         n = size(A, 1)
         EV = copy(A)
         W = Array($eltype, n)
         info = [0]

         # Workspace query
         work = [0.0]
         lwork = -1
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               jobz, uplo, n, EV, n, W, work, lwork, info)

         if info[1] == 0
             lwork = int32(work[1])
             work = Array($eltype, lwork)
         else
             error("Error in $fname")
         end

         # Compute eigenvalues, eigenvectors
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               jobz, uplo, n, EV, n, W, work, lwork, info)

         if info[1] == 0; return (EV, W); end
         error("Error in EIG");
         end
         )
end

# SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBU, JOBVT
#       INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
#      $                   VT( LDVT, * ), WORK( * )

for (fname, eltype) = (("dgesvd_", Float64), ("sgesvd_", Float32))
    eval(`function svd(A::Matrix{$eltype})
         jobu = "A"
         jobvt = "A"
         m = size(A, 1)
         n = size(A, 2)
         k = min(m,n)
         X = copy(A)
         S = Array($eltype, k)
         U = Array($eltype, m, m)
         VT = Array($eltype, n, n)
         info = [0]

         # Workspace query
         work = [0.0]
         lwork = -1
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               jobu, jobvt, m, n, X, m, S, U, m, VT, n, work, lwork, info)

         if info[1] == 0
             lwork = int32(work[1])
             work = Array($eltype, lwork)
         else
             error("Error in $fname")
         end

         # Compute SVD
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               jobu, jobvt, m, n, X, m, S, U, m, VT, n, work, lwork, info)

         SIGMA = zeros($eltype, m, n)
         for i=1:k; SIGMA[i,i] = S[i]; end

         if info[1] == 0; return (U,SIGMA,VT); end
         error("Error in SVD");
         end
         )
end
