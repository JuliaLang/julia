libLAPACK = libBLAS

macro jl_lapack_potrf_macro(fname, eltype)
    quote
        # SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function jl_lapack_potrf(uplo, n, A::Matrix{$eltype}, lda)
            info = [int32(0)]
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  uplo, int32(n), A, int32(lda), info)
            return info[1]
        end
    end
end

@jl_lapack_potrf_macro :dpotrf_ Float64
@jl_lapack_potrf_macro :spotrf_ Float32
@jl_lapack_potrf_macro :zpotrf_ Complex128
@jl_lapack_potrf_macro :cpotrf_ Complex64

function chol(A::Matrix)
    n = int32(size(A, 1))
    R = triu(A)

    info = jl_lapack_potrf("U", n, R, n)

    if info == 0; return R; end
    if info  > 0; error("Matrix not Positive Definite"); end
    error("Error in CHOL")
end

macro jl_lapack_getrf_macro(fname, eltype)
    quote
        # SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, M, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * )
        function jl_lapack_getrf(m, n, A::Matrix{$eltype}, lda, ipiv)
            info = [int32(0)]
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                   Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), A, int32(lda), ipiv, info)
            return info[1]
        end
    end
end

@jl_lapack_getrf_macro :dgetrf_ Float64
@jl_lapack_getrf_macro :sgetrf_ Float32
@jl_lapack_getrf_macro :zgetrf_ Complex128
@jl_lapack_getrf_macro :cgetrf_ Complex64

lu(A::Matrix) = lu(A, false)

function lu(A::Matrix, economy::Bool)
    m, n = size(A)
    LU = A
    if !economy
        LU = copy(A)
    end
    ipiv = Array(Int32, min(m,n))

    info = jl_lapack_getrf(m, n, LU, m, ipiv)

    if info > 0; error("Matrix is singular"); end
    P = linspace(1, m)
    for i=1:min(m,n); t = P[i]; P[i] = P[ipiv[i]]; P[ipiv[i]] = t ; end

    if info == 0
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


macro jl_lapack_qr_macro(real_geqp3, complex_geqp3, orgqr, ungqr, eltype, celtype)
    quote
        # SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       INTEGER            JPVT( * )
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function jl_lapack_geqp3(m, n, A::Matrix{$eltype}, lda, jpvt, tau, work, lwork)
            info = [int32(0)]
            ccall(dlsym(libLAPACK, $real_geqp3),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), A, int32(lda), jpvt, tau, work, int32(lwork), info)
            return info[1]
        end

        # SUBROUTINE ZGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, RWORK, INFO )
        #*      .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        #*      .. Array Arguments ..
        #       INTEGER            JPVT( * )
        #       DOUBLE PRECISION   RWORK( * )
        #       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
        function jl_lapack_geqp3(m, n, A::Matrix{$celtype}, lda, jpvt, tau, work, lwork, rwork)
            info = [int32(0)]
            ccall(dlsym(libLAPACK, $complex_geqp3),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$celtype}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$celtype}, Ptr{$celtype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(m), int32(n), A, int32(lda), jpvt, tau, work, int32(lwork), rwork, info)
            return info[1]
        end

        # SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, K, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function jl_lapack_orgqr(m, n, k, A::Matrix{$eltype}, lda, tau, work, lwork)
            info = [int32(0)]
            ccall(dlsym(libLAPACK, $orgqr),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                   Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), int32(k), A, int32(lda), tau, work, int32(lwork), info)
            return info[1]
        end

        # SUBROUTINE ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        #*     .. Scalar Arguments ..
        #      INTEGER            INFO, K, LDA, LWORK, M, N
        #*     .. Array Arguments ..
        #      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
        function jl_lapack_ungqr(m, n, k, A::Matrix{$celtype}, lda, tau, work, lwork)
            info = [int32(0)]
            ccall(dlsym(libLAPACK, $ungqr),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$celtype},
                   Ptr{Int32}, Ptr{$celtype}, Ptr{$celtype}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), int32(k), A, int32(lda), tau, work, int32(lwork), info)
            return info[1]
        end
    end
end

@jl_lapack_qr_macro :dgeqp3_ :zgeqp3_ :dorgqr_ :zungqr_ Float64 Complex128
@jl_lapack_qr_macro :sgeqp3_ :cgeqp3_ :sorgqr_ :cungqr_ Float32 Complex64

function qr{T}(A::Matrix{T})
    m, n = size(A)
    QR = copy(A)
    jpvt = zeros(Int32, n)
    k = min(m,n)
    tau = Array(T, k)
    if iscomplex(A)
        rwork = zeros(typeof(real(A[1])), long(2*n))
    end

    # Workspace query for QR factorization
    work = zeros(T,1)
    lwork = int32(-1)
    if iscomplex(A)
        info = jl_lapack_geqp3(m, n, QR, m, jpvt, tau, work, lwork, rwork)
    else
        info = jl_lapack_geqp3(m, n, QR, m, jpvt, tau, work, lwork)
    end

    if info == 0; lwork = real(work[1]); work = Array(T, long(lwork))
    else error("Error in LAPACK geqp3"); end

    # Compute QR factorization
    if iscomplex(A)
        info = jl_lapack_geqp3(m, n, QR, m, jpvt, tau, work, lwork, rwork)
    else
        info = jl_lapack_geqp3(m, n, QR, m, jpvt, tau, work, lwork)
    end

    if info > 0; error("Matrix is singular"); end

    R = triu(QR)

    # Workspace query to form Q
    lwork2 = int32(-1)
    if iscomplex(A)
        info = jl_lapack_ungqr(m, k, k, QR, m, tau, work, lwork2)
    else
        info = jl_lapack_orgqr(m, k, k, QR, m, tau, work, lwork2)
    end

    if info == 0; lwork2 = real(work[1]); work = Array(T, long(lwork2))
    else error("Error in LAPACK orgqr/ungqr"); end

    # Compute Q
    if iscomplex(A)
        info = jl_lapack_ungqr(m, k, k, QR, m, tau, work, lwork2)
    else
        info = jl_lapack_orgqr(m, k, k, QR, m, tau, work, lwork2)
    end

    if info == 0; return (QR[:, 1:k], R[1:k, :], jpvt); end
    error("Error in LAPACK orgqr/ungqr");
end


#       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )

macro lapack_eig(fname, eltype)
    quote
        function eig(A::Matrix{$eltype})
            if !issymmetric(A); error("Matrix must be symmetric"); end

            jobz = "V"
            uplo = "U"
            n = int32(size(A, 1))
            EV = copy(A)
            W = Array($eltype, long(n))
            info = [int32(0)]

            # Workspace query
            work = [0.0]
            lwork = int32(-1)
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  jobz, uplo, n, EV, n, W, work, lwork, info)

            if info[1] == 0; lwork = int32(work[1]); work = Array($eltype, long(lwork));
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

@lapack_eig :dsyev_ Float64
@lapack_eig :ssyev_ Float32

#      SUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO )
#*     .. Scalar Arguments ..
#      CHARACTER          JOBZ, UPLO
#      INTEGER            INFO, LDA, LWORK, N
#*     ..
#*     .. Array Arguments ..
#      DOUBLE PRECISION   RWORK( * ), W( * )
#      COMPLEX*16         A( LDA, * ), WORK( * )
macro lapack_eig_complex(fname, eltype, eltype2)
    quote
        function eig(A::Matrix{$eltype})
            if !ishermitian(A); error("Matrix must be Hermitian"); end

            jobz = "V"
            uplo = "U"
            n = int32(size(A, 1))
            EV = copy(A)
            W = Array($eltype2, long(n))
            info = [int32(0)]
            rwork = Array($eltype2, long(max(3n-2, 1)))

            # Workspace query
            work = zeros($eltype, 1)
            lwork = int32(-1)
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype2}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype2}, Ptr{Int32}),
                  jobz, uplo, n, EV, n, W, work, lwork, rwork, info)

            if info[1] == 0; lwork = int32(real(work[1])); work = Array($eltype, long(lwork));
            else error("Error in ", $fname); end

            # Compute eigenvalues, eigenvectors
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype2}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype2}, Ptr{Int32}),
                  jobz, uplo, n, EV, n, W, work, lwork, rwork, info)

            if info[1] == 0; return (diagm(W), EV); end
            error("Error in EIG");
        end
    end
end

@lapack_eig_complex :zheev_ Complex128 Float64
@lapack_eig_complex :cheev_ Complex64 Float32

# SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBU, JOBVT
#       INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
#      $                   VT( LDVT, * ), WORK( * )

macro lapack_svd(fname, eltype)
    quote
        function svd(A::Matrix{$eltype})
            jobu = "A"
            jobvt = "A"
            m, n = size(A)
            m = int32(m); n = int32(n)
            k = min(m,n)
            X = copy(A)
            S = Array($eltype, long(k))
            U = Array($eltype, long(m), long(m))
            VT = Array($eltype, long(n), long(n))
            info = [int32(0)]

            # Workspace query
            work = zeros($eltype, 1)
            lwork = int32(-1)
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  jobu, jobvt, m, n, X, m, S, U, m, VT, n, work, lwork, info)

            if info[1] == 0; lwork = int32(work[1]); work = Array($eltype, long(lwork));
            else error("Error in ", $fname); end

            # Compute SVD
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  jobu, jobvt, m, n, X, m, S, U, m, VT, n, work, lwork, info)

            SIGMA = zeros($eltype, long(m), long(n))
            for i=1:k; SIGMA[i,i] = S[i]; end

            if info[1] == 0; return (U,SIGMA,VT); end
            error("Error in SVD");
        end
    end
end

@lapack_svd :dgesvd_ Float64
@lapack_svd :sgesvd_ Float32

# SUBROUTINE ZGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
#     $                   WORK, LWORK, RWORK, INFO )
#*     .. Scalar Arguments ..
#      CHARACTER          JOBU, JOBVT
#      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
#*     ..
#*     .. Array Arguments ..
#      DOUBLE PRECISION   RWORK( * ), S( * )
#      COMPLEX*16         A( LDA, * ), U( LDU, * ), VT( LDVT, * ),
#     $                   WORK( * )

macro lapack_svd_complex(fname, eltype, eltype2)
    quote
        function svd(A::Matrix{$eltype})
            jobu = "A"
            jobvt = "A"
            m, n = size(A)
            m = int32(m); n = int32(n)
            k = min(m,n)
            X = copy(A)
            S = Array($eltype2, long(k))
            U = Array($eltype, long(m), long(m))
            VT = Array($eltype, long(n), long(n))
            info = [int32(0)]
            rwork = Array($eltype2, long(5*min(m,n)))

            # Workspace query
            work = zeros($eltype, 1)
            lwork = int32(-1)
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype2}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype2}, Ptr{Int32}),
                  jobu, jobvt, m, n, X, m, S, U, m, VT, n, work, lwork, rwork, info)

            if info[1] == 0; lwork = int32(real(work[1])); work = Array($eltype, long(lwork));
            else error("Error in ", $fname); end

            # Compute SVD
            ccall(dlsym(libLAPACK, $fname),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype2}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype2}, Ptr{Int32}),
                  jobu, jobvt, m, n, X, m, S, U, m, VT, n, work, lwork, rwork, info)

            SIGMA = zeros($eltype, long(m), long(n))
            for i=1:k; SIGMA[i,i] = S[i]; end

            if info[1] == 0; return (U,SIGMA,VT); end
            error("Error in SVD");
        end
    end
end

@lapack_svd_complex :zgesvd_ Complex128 Float64
@lapack_svd_complex :cgesvd_ Complex64 Float32

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
        function \(A::Matrix{$eltype}, B::VecOrMat{$eltype})
            info = [int32(0)]
            m = int32(size(A, 1))
            n = int32(size(A, 2))
            mrhs = int32(size(B, 1))
            if m != mrhs; error("Number of rows of arguments do not match"); end
            if isa(B, Vector); nrhs = int32(1); else nrhs=int32(size(B, 2)); end
            Acopy = copy(A)
            X = copy(B)

            if m == n # Square
                case = :general
                if isuppertriangular(A); case = :upper_triangular; end
                if islowertriangular(A); case = :lower_triangular; end

                if case == :general # General
                    ipiv = Array(Int32, long(n))

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
                Y = Array($eltype, long(max(m,n)), long(nrhs))
                Y[1:size(X,1), 1:nrhs] = X

                ccall(dlsym(libLAPACK, $fname_lsq),
                      Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                       Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                      "N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork, info)

                if info[1] == 0
                    lwork = int32(work[1])
                    work = Array($eltype, long(lwork))
                else
                    error("Error in ", $fname_lsq)
                end

                ccall(dlsym(libLAPACK, $fname_lsq),
                      Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                       Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                      "N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork, info)

                ##if B is a vector, format answer as vector
                if isa(B, Vector)
                    X = zeros($eltype, size(Y,1))
                    for i = 1:size(Y,1); X[i] = Y[i,1]; end
                else
                    X = Y
                end
            end # if m == n...

            if info[1] == 0; return X; end
            error("Error in solving A*X = B")

        end # function...
    end # quote...
end # macro...

@lapack_backslash :dgesv_ :dposv_ :dgels_ :dtrtrs_ Float64
@lapack_backslash :sgesv_ :sposv_ :sgels_ :strtrs_ Float32
@lapack_backslash :zgesv_ :zposv_ :zgels_ :ztrtrs_ Complex128
@lapack_backslash :cgesv_ :cposv_ :cgels_ :ctrtrs_ Complex64

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
