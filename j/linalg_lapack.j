libLAPACK = libBLAS

# SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * )

macro lapack_chol(fname, eltype)
    quote
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
        end
    end
end

@lapack_chol "dpotrf_" Float64
@lapack_chol "spotrf_" Float32

# SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, M, N
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * )

lu(A::DenseMatrix) = lu(A, false)

macro lapack_lu(fname, eltype)
    quote
        function lu(A::DenseMatrix{$eltype}, economy::Bool)
            info = [0]
            m, n = size(A)
            LU = A
            if !economy
                LU = copy(A)
            end
            ipiv = Array(Int32, min(m,n))
            
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                   Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  m, n, LU, m, ipiv, info)
            
            if info[1] > 0; error("Matrix is singular"); end
            P = linspace(1, m)
            for i=1:min(m,n); t = P[i]; P[i] = P[ipiv[i]]; P[ipiv[i]] = t ; end
            
            if info[1] == 0
                if economy
                    return (LU, P)
                else
                    L = tril(LU, -1) + eye(m,n)
                    U = m <= n ? triu(LU) : triu(LU)[1:n, :]
                    return (L, U, P)
                end
            end
            error("Error in LU")
        end

    end
end

@lapack_lu "dgetrf_" Float64
@lapack_lu "sgetrf_" Float32

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

macro lapack_qr(fname, fname2, eltype)
    quote 
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
                  m, k, k, QR, m, tau, work, lwork, info)
            
            if info[1] == 0; return (QR[:, 1:k], R[1:k, :], jpvt); end
            error("Error in ", $fname);
        end
    end
end

@lapack_qr "dgeqp3_" "dorgqr_" Float64
@lapack_qr "sgeqp3_" "sorgqr_" Float32

#       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )

macro lapack_eig(fname, eltype)
    quote 
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
            
            if info[1] == 0; return (diagm(W), EV); end
            error("Error in EIG");
        end
    end
end

@lapack_eig "dsyev_" Float64
@lapack_eig "ssyev_" Float32

# SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBU, JOBVT
#       INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
#      $                   VT( LDVT, * ), WORK( * )

macro lapack_svd(fname, eltype)
    quote 
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
        end
    end
end

@lapack_svd "dgesvd_" Float64
@lapack_svd "sgesvd_" Float32

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

macro lapack_backslash(fname_lu, fname_chol, fname_lsq, fname_tri, eltype)
    quote 
        function \(A::DenseMatrix{$eltype}, B::DenseVecOrMat{$eltype})
            info = [0]
            m = size(A, 1)
            n = size(A, 2)
            mrhs = size(B, 1)
            if m != mrhs; error("Number of rows of arguments do not match"); end
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
                Y = Array($eltype, max(m,n), nrhs)
                Y[1:size(X,1), 1:size(X,2)] = X

                ccall(dlsym(libLAPACK, $fname_lsq),
                      Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                       Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                      "N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork, info)
                
                if info[1] == 0
                    lwork = int32(work[1])
                    work = Array($eltype, lwork)
                else
                    error("Error in ", $fname_lsq)
                end
                
                ccall(dlsym(libLAPACK, $fname_lsq),
                      Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, 
                       Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                      "N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork, info)
                
                X = Y
            end # if m == n...
                
            if info[1] == 0; return X; end
            error("Error in solving A*X = B")
            
        end # function...
    end # quote...
end # macro...

@lapack_backslash "dgesv_" "dposv_" "dgels_" "dtrtrs_" Float64
@lapack_backslash "sgesv_" "sposv_" "sgels_" "strtrs_" Float32

## BIDIAG ##

bidiag(A) = bidiag(A, max(1,div(min(size(A)),2)), 2)

function bidiag(A::Array{Float64,2}, nb, nargout)
    (m,n) = size(A)
    s=min(m,n)
    w=max(m,n)
    D=zeros(s)
    E=zeros(s-1)
    tauq=zeros(s); taup=zeros(s)
    A0 = A = copy(A)

    k = 1-nb
    for k=1:nb:s-nb
        Dblk = zeros(nb)
        Eblk = zeros(nb)
        tauqblk = zeros(nb)
        taupblk = zeros(nb)
        X = zeros(m-k+1, nb)
        Y = zeros(n-k+1, nb)

        ccall(dlsym(libLAPACK, :dlabrd_), Void,
              (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
               Ptr{Float64},
               Ptr{Int32}, Ptr{Float64}, Ptr{Int32}),
              m-k+1, n-k+1, nb, A, m-k+1, Dblk, Eblk, tauqblk, taupblk,
              X, m-k+1, Y, n-k+1)
        b = k+nb-1  # end of block

        # accumulate reduced leading portion
        A0[k:b, k:end] = A[1:nb, :]
        A0[k:end, k:b] = A[:, 1:nb]

        D[k:b]    = Dblk
        E[k:b]    = Eblk
        tauq[k:b] = tauqblk
        taup[k:b] = taupblk

        # Update the trailing submatrix A[k+nb:m,k+nb:n], 
        # using an update of the form  A := A - V*Y' - X*U'
        l = nb+1
        A = A[l:end,l:end]-A[l:end,1:nb]*Y[l:end,:]'-X[l:end,:]*A[1:nb,l:end]
        A0[k+nb:m, k+nb:n] = A
    end

    k = k+nb

    tauqblk = zeros(s-k+1)
    taupblk = zeros(s-k+1)

    ccall(dlsym(libLAPACK,:dgebd2_), Void,
          (Ptr{Int32}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
           Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
           Ptr{Float64}, Ptr{Int32}),
          m-k+1, n-k+1, A, m-k+1, zeros(s-k+1), zeros(s-k),
          tauqblk, taupblk, zeros(w-k+1), 0)
    A0[k:m,k:n] = A
    tauq[k:end] = tauqblk
    taup[k:end] = taupblk

    for j=k:s-1
        l = j-k+1
        D[j] = A[l,l]
        if m >= n
            E[j] = A[l,l+1]
        else
            E[j] = A[l+1,l]
        end
    end
    D[s] = A[s-k+1,s-k+1]

    if nargout == 2
        return (D, E)
    end

    if m >= n
        Q = copy(A0)
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "Q", m, n, n, Q, m, tauq, zeros(s), s, 0)
        PT = A0
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "P", n, n, n, PT, m, taup, zeros(s), s, 0)
        PT = PT[1:n,:]
    else
        Q = copy(A0)
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "Q", m, m, n, Q, m, tauq, zeros(s), s, 0)
        Q = Q[:,1:m]
        PT = A0
        ccall(dlsym(libLAPACK,:dorgbr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64},
               Ptr{Int32}, Ptr{Int32}),
              "P", m, n, m, PT, m, taup, zeros(s), s, 0)
    end

    return (Q, D, E, PT)
end

blksvd(A) = blksvd(A, 1)

function blksvd(A::Array{Float64,2}, nargout)
    (m, n) = size(A)
    nb = 3

    (Q, D, E, PT) = bidiag(A, nb, 4)

    if m >= n
        ccall(dlsym(libLAPACK,:dbdsqr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}),
              "U",
              n, n, m, 0, D, E, PT, n, Q, m, zeros(1,1), 1, zeros(4*n,1), 0)
    else
        ccall(dlsym(libLAPACK,:dbdsqr_), Void,
              (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Int32}),
              "L",
              m, n, m, 0, D, E, PT, m, Q, m, zeros(1,1), 1, zeros(4*m,1), 0)
    end

    S = D
    if nargout == 1
        return S
    end
    U = Q
    VT = PT
    V = VT'
    S = diagm(S)
    return (U, S, V)
end
