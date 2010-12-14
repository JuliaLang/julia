libLAPACK = dlopen("libLAPACK")

# SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * )

for (fname, eltype) = (("dpotrf_", Float64), ("spotrf_", Float32))
    eval(quote
         function chol(A::DenseMatrix{$eltype})
         info = [0]
         n = size(A, 1)
         R = triu(A)

         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               "U", n, R, n, info)

         if info[1] == 0; return R; end
         if info[1] > 0; error("Matrix not Positive Definite"); end
         error("Error in CHOL")

         end end
         )
end

# SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, M, N
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * )

for (fname, eltype) = (("dgetrf_", Float64), ("sgetrf_", Float32))
    eval(quote
         function lu(A::DenseMatrix{$eltype})
         info = [0]
         m, n = size(A)
         LU = copy(A)
         ipiv = Array(Int32, min(m,n))

         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
               m, n, LU, m, ipiv, info)

         if info[1] > 0; error("Matrix is singular"); end
         P = linspace(1,m)
         for i=1:m; t = P[i]; P[i] = P[ipiv[i]]; P[ipiv[i]] = t ; end

         if info[1] == 0; return (tril(LU, -1) + eye(m,n), triu(LU), P); end
         error("Error in LU")

         end end
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
    eval(quote
         function qr(A::DenseMatrix{$eltype})
         info = [0]
         m, n = size(A)
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

         if info[1] == 0; lwork = int32(work[1]); work = Array($eltype, lwork);
         else error("Error in ", $fname); end

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

         if info[1] == 0; lwork = int32(work[1]); work = Array($eltype, lwork);
         else error("Error in ", $fname2); end

         # Compute Q
         ccall(dlsym(libLAPACK, $fname2),
               Void,
               (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               m, k, k, QR, m, tau, work, lwork2, info)
         
         if info[1] == 0; return (QR[:, 1:k], R[1:k, :], jpvt); end
         error("Error in ", $fname);

         end end
         )
end

#       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )

for (fname, eltype) = (("dsyev_", Float64), ("ssyev_", Float32))
    eval(quote
         function eig(A::DenseMatrix{$eltype})

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

         if info[1] == 0; lwork = int32(work[1]); work = Array($eltype, lwork);
         else error("Error in ", $fname); end

         # Compute eigenvalues, eigenvectors
         ccall(dlsym(libLAPACK, $fname),
               Void,
               (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               jobz, uplo, n, EV, n, W, work, lwork, info)

         if info[1] == 0; return (EV, W); end
         error("Error in EIG");

         end end
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
    eval(quote
         function svd(A::DenseMatrix{$eltype})
         jobu = "A"
         jobvt = "A"
         m, n = size(A)
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

         if info[1] == 0; lwork = int32(work[1]); work = Array($eltype, lwork);
         else error("Error in ", $fname); end

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

         end end
         )
end

# SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )

#      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO)
# *     .. Scalar Arguments ..
#       CHARACTER          TRANS
#       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS

#      SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          DIAG, TRANS, UPLO
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )

for (fname_lu, fname_chol, fname_lsq, fname_tri, eltype) = 
    (("dgesv_", "dposv_", "dgels_", "dtrtrs_", Float64),
     ("sgesv_", "sposv_", "sgels_", "strtrs_", Float32))

    eval(quote
        function \(A::DenseMatrix{$eltype}, B::DenseVecOrMat{$eltype})
        info = [0]
        m = size(A, 1)
        n = size(A, 2)
        if isa(B, Vector); nrhs = 1; else nrhs = size(B, 2); end
        Acopy = copy(A)
        X = copy(B)

        if m == n # Square
            case = :general
            if isuppertriangular(A); case = :upper_triangular; end
            if islowertriangular(A); case = :lower_triangular; end

            if case == :general # General
                ipiv = Array(Int32, n)

                # Check for SPD matrix
                if issymmetric(Acopy) && all([ Acopy[i,i] > 0 | i=1:n ])
                      case = :spd
                end

                if case == :spd
                      uplo = "U"
                      ccall(dlsym(libLAPACK, $fname_chol),
                            Void,
                            (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                             Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                            uplo, n, nrhs, Acopy, n, X, n, info)

                      if info[1] != 0
                         Acopy = copy(A)
                         case = :general
                      end
                end

                if case == :general
                      info[1] = 0
                      ccall(dlsym(libLAPACK, $fname_lu),
                            Void,
                            (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32},
                             Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                            n, nrhs, Acopy, n, ipiv, X, n, info)
                end

            else # Triangular
                uplo = "U"
                if case == :lower_triangular; uplo = "L"; end
                ccall(dlsym(libLAPACK, $fname_tri),
                      Void,
                      (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                      uplo, "N", "N", n, nrhs, Acopy, n, X, n, info)
            end

        else # Rectangular

            # Workspace query
            lwork = -1
            work = [0.0]
            ccall(dlsym(libLAPACK, $fname_lsq),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                   Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  "N", m, n, nrhs, Acopy, m, X, max(m,n), work, lwork, info)

            if info[1] == 0; lwork = int32(work[1]); work = Array($eltype, lwork);
            else error("Error in ", $fname_lsq); end

            ccall(dlsym(libLAPACK, $fname_lsq),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                   Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  "N", m, n, nrhs, Acopy, m, X, max(m,n), work, lwork, info)

         end # if m == n...

         if info[1] == 0; return X; end
         error("Error in solving A*X = B")

    end end # quote function \ ...
         )
end

