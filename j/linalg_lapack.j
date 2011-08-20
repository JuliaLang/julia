libLAPACK = libBLAS

macro jl_lapack_potrf_macro(potrf, eltype)
    quote

        # SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function jl_lapack_potrf(uplo, n, A::DenseMat{$eltype}, lda)
            info = Array(Int32, 1)
            a = pointer(A)
            ccall(dlsym(libLAPACK, $potrf),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  uplo, int32(n), a, int32(lda), info)
            return info[1]
        end

    end
end

@jl_lapack_potrf_macro :dpotrf_ Float64
@jl_lapack_potrf_macro :spotrf_ Float32
@jl_lapack_potrf_macro :zpotrf_ Complex128
@jl_lapack_potrf_macro :cpotrf_ Complex64

#does not check that input matrix is symmetric/hermitian
#(uses upper triangular half)
#Possible TODO: "economy mode"
function chol{T<:Union(Float32,Float64,Complex64,Complex128)}(A::DenseMat{T})
    if stride(A,1) != 1; error("chol: matrix columns must have contiguous elements"); end
    n = int32(size(A, 1))
    if isa(A, Matrix)
        R = triu(A)
    else
        R = triu(A[:,:])
    end
    info = jl_lapack_potrf("U", n, R, stride(A,2))

    if info == 0; return R; end
    if info  > 0; error("Matrix not Positive Definite"); end
    error("Error in CHOL")
end

macro jl_lapack_getrf_macro(getrf, eltype)
    quote

        # SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, M, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * )
        function jl_lapack_getrf(m, n, A::AbstractMatrix{$eltype}, lda, ipiv)
            info = Array(Int32, 1)
            a = pointer(A)
            ccall(dlsym(libLAPACK, $getrf),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                   Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), a, int32(lda), ipiv, info)
            return info[1]
        end

    end
end

@jl_lapack_getrf_macro :dgetrf_ Float64
@jl_lapack_getrf_macro :sgetrf_ Float32
@jl_lapack_getrf_macro :zgetrf_ Complex128
@jl_lapack_getrf_macro :cgetrf_ Complex64

lu(A::Matrix) = lu(A, false)
lu{T}(A::SubArray{T,2}) = lu(A,false)
function lu{T<:Union(Float32,Float64,Complex64,Complex128)}(A::DenseMat{T},
                                                            economy::Bool)
    if stride(A,1) != 1; error("lu: matrix columns must have contiguous elements"); end
    m, n = size(A)
    LU = A
    if !economy
        if isa(A, Matrix)
            LU = copy(A)
        else
            LU = A[:,:]
        end
    end
    ipiv = Array(Int32, min(m,n))

    info = jl_lapack_getrf(m, n, LU, stride(LU,2), ipiv)

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
        function jl_lapack_geqp3(m, n, A::DenseMat{$eltype}, lda, jpvt, tau, work, lwork)
            info = Array(Int32, 1)
            a = pointer(A)
            ccall(dlsym(libLAPACK, $real_geqp3),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), a, int32(lda), jpvt, tau, work, int32(lwork), info)
            return info[1]
        end

        # SUBROUTINE ZGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, RWORK, INFO )
        #*      .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        #*      .. Array Arguments ..
        #       INTEGER            JPVT( * )
        #       DOUBLE PRECISION   RWORK( * )
        #       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
        function jl_lapack_geqp3(m, n, A::DenseMat{$eltype}, lda, jpvt, tau, work, lwork, rwork)
            info = Array(Int32, 1)
            a = pointer(A)
            ccall(dlsym(libLAPACK, $complex_geqp3),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$celtype}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$celtype}, Ptr{$celtype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(m), int32(n), a, int32(lda), jpvt, tau, work, int32(lwork), rwork, info)
            return info[1]
        end

        # SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, K, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function jl_lapack_orgqr(m, n, k, A::DenseMat{$eltype}, lda, tau, work, lwork)
            info = Array(Int32, 1)
            a = pointer(A)
            ccall(dlsym(libLAPACK, $orgqr),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype},
                   Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), int32(k), a, int32(lda), tau, work, int32(lwork), info)
            return info[1]
        end

        # SUBROUTINE ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        #*     .. Scalar Arguments ..
        #      INTEGER            INFO, K, LDA, LWORK, M, N
        #*     .. Array Arguments ..
        #      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
        function jl_lapack_ungqr(m, n, k, A::DenseMat{$eltype}, lda, tau, work, lwork)
            info = Array(Int32, 1)
            a = pointer(A)
            ccall(dlsym(libLAPACK, $ungqr),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$celtype},
                   Ptr{Int32}, Ptr{$celtype}, Ptr{$celtype}, Ptr{Int32}, Ptr{Int32}),
                  int32(m), int32(n), int32(k), a, int32(lda), tau, work, int32(lwork), info)
            return info[1]
        end

    end
end

@jl_lapack_qr_macro :dgeqp3_ :zgeqp3_ :dorgqr_ :zungqr_ Float64 Complex128
@jl_lapack_qr_macro :sgeqp3_ :cgeqp3_ :sorgqr_ :cungqr_ Float32 Complex64

#possible TODO: economy mode?
function qr{T<:Union(Float32,Float64,Complex64,Complex128)}(A::DenseMat{T})
    m, n = size(A)
    if isa(A, Matrix)
        QR = copy(A)
    else
        QR = A[:,:]
    end
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
        info = jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork, rwork)
    else
        info = jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork)
    end

    if info == 0; lwork = real(work[1]); work = Array(T, long(lwork))
    else error("Error in LAPACK geqp3"); end

    # Compute QR factorization
    if iscomplex(A)
        info = jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork, rwork)
    else
        info = jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork)
    end

    if info > 0; error("Matrix is singular"); end

    R = triu(QR)

    # Workspace query to form Q
    lwork2 = int32(-1)
    if iscomplex(A)
        info = jl_lapack_ungqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    else
        info = jl_lapack_orgqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    end

    if info == 0; lwork2 = real(work[1]); work = Array(T, long(lwork2))
    else error("Error in LAPACK orgqr/ungqr"); end

    # Compute Q
    if iscomplex(A)
        info = jl_lapack_ungqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    else
        info = jl_lapack_orgqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    end

    if info == 0; return (QR[:, 1:k], R[1:k, :], jpvt); end
    error("Error in LAPACK orgqr/ungqr");
end

macro jl_lapack_eig_macro(syev, heev, real_geev, complex_geev, eltype, celtype)
    quote

        #       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBZ, UPLO
        #       INTEGER            INFO, LDA, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
        function jl_lapack_syev(jobz, uplo, n, A::Matrix{$eltype}, lda, W, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(libLAPACK, $syev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  jobz, uplo, int32(n), A, int32(lda), W, work, int32(lwork), info)
            return info[1]
        end

        #      SUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOBZ, UPLO
        #      INTEGER            INFO, LDA, LWORK, N
        #*     .. Array Arguments ..
        #      DOUBLE PRECISION   RWORK( * ), W( * )
        #      COMPLEX*16         A( LDA, * ), WORK( * )
        function jl_lapack_heev(jobz, uplo, n, A::Matrix{$celtype}, lda, W, work, lwork, rwork)
            info = Array(Int32, 1)
            ccall(dlsym(libLAPACK, $heev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$celtype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$celtype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  jobz, uplo, int32(n), A, int32(lda), W, work, int32(lwork), rwork, info)
            return info[1]
        end

        #      SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
        #      $                  LDVR, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBVL, JOBVR
        #       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
        #      $                   WI( * ), WORK( * ), WR( * )
        function jl_lapack_geev(jobvl, jobvr, n, A::Matrix{$eltype}, lda, WR, WI, VL, ldvl, 
                                VR, ldvr, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(libLAPACK, $real_geev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, 
                   Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  jobvl, jobvr, int32(n), A, int32(lda), WR, WI, VL, int32(ldvl),
                  VR, int32(ldvr), work, int32(lwork), info)
            return info[1]
        end

        #      SUBROUTINE ZGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR,
        #      $                  WORK, LWORK, RWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBVL, JOBVR
        #       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   RWORK( * )
        #       COMPLEX*16         A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
        #      $                   W( * ), WORK( * )
        function jl_lapack_geev(jobvl, jobvr, n, A::Matrix{$celtype}, lda, W, VL, ldvl, 
                                VR, ldvr, work, lwork, rwork)
            info = Array(Int32, 1)
            ccall(dlsym(libLAPACK, $complex_geev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$celtype}, Ptr{Int32},
                   Ptr{$celtype}, Ptr{$celtype}, Ptr{Int32}, 
                   Ptr{$celtype}, Ptr{Int32}, Ptr{$celtype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  jobvl, jobvr, int32(n), A, int32(lda), W, VL, int32(ldvl), 
                  VR, int32(ldvr), work, int32(lwork), rwork, info)
            return info[1]
        end

    end
end

@jl_lapack_eig_macro :dsyev_ :zheev_ :dgeev_ :zgeev_ Float64 Complex128
@jl_lapack_eig_macro :ssyev_ :cheev_ :sgeev_ :cgeev_ Float32 Complex64

function eig{T}(A::Matrix{T})
    m, n = size(A)
    if m != n; error("Input must be square"); end

    if ishermitian(A)

        jobz = "V"
        uplo = "U"
        EV = copy(A)
        W = Array(T, n)
        if iscomplex(A)
            rwork = Array(typeof(real(A[1])), long(max(3*n-2, 1)))
        end

        # Workspace query
        work = zeros(T,1)
        lwork = -1

        if iscomplex(A)
            info = jl_lapack_heev(jobz, uplo, n, EV, stride(A,2), W, work, lwork, rwork)
        else
            info = jl_lapack_syev(jobz, uplo, n, EV, stride(A,2), W, work, lwork)
        end

        if info == 0; lwork = real(work[1]); work = Array(T, long(lwork));
        else error("Error in LAPACK syev/heev"); end

        # Compute eigenvalues, eigenvectors
        if iscomplex(A)
            info = jl_lapack_heev(jobz, uplo, n, EV, stride(A,2), W, work, lwork, rwork)
        else
            info = jl_lapack_syev(jobz, uplo, n, EV, stride(A,2), W, work, lwork)
        end

        if info == 0; return (diagm(W), EV); end
        error("Error in LAPACK syev/heev");

    else # Non-symmetric case

        jobvl = "N"
        jobvr = "V"
        Acopy = copy(A)
        VL = Array(T, 1) # Not referenced, since we are only computing right eigenvectors
        VR = Array(T, n, n)
        if iscomplex(A)
            W = Array(T, n)
            rwork = Array(typeof(real(A[1])), 2*n)
        else
            WR = zeros(T, n)
            WI = zeros(T, n)
        end

        # Workspace query
        work = zeros(T,1)
        lwork = -1

        if iscomplex(A)
            info = jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(A,2), W, VL, n, VR, n, work, lwork, rwork)
        else
            info = jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(A,2), WR, WI, VL, n, VR, n, work, lwork)
        end

        if info == 0; lwork = real(work[1]); work = Array(T, long(lwork));
        else error("Error in LAPACK geev"); end

        # Compute eigenvalues, eigenvectors
        if iscomplex(A)
            info = jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(A,2), W, VL, n, VR, n, work, lwork, rwork)
        else
            info = jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(A,2), WR, WI, VL, n, VR, n, work, lwork)
        end
        
        if info != 0; error("Error in LAPACK geev"); end

        if iscomplex(A)
            return (diagm(W), VR)
        else
            evec = complex(zeros(T, n, n), zeros(T, n, n))
            for j=1:n
                if WI[j] == 0.0
                    evec[:,j] = VR[:,j]
                else
                    evec[:,j]   = VR[:,j] + im*VR[:,j+1]
                    evec[:,j+1] = VR[:,j] - im*VR[:,j+1]
                    j += 1
                end
            end
            return (diagm(complex(WR, WI)), evec)
        end

    end # symmetric / non-symmetric case

end

macro jl_lapack_gesvd_macro(real_gesvd, complex_gesvd, eltype, celtype)
    quote

        # SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBU, JOBVT
        #       INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
        #      $                   VT( LDVT, * ), WORK( * )
        function jl_lapack_gesvd(jobu, jobvt, m, n, A::Matrix{$eltype}, lda, S, U, ldu, 
                                 VT, ldvt, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(libLAPACK, $real_gesvd),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  jobu, jobvt, int32(m), int32(n), A, int32(lda), S, U, int32(ldu), 
                  VT, int32(ldvt), work, int32(lwork), info)
            return info[1]
        end

        # SUBROUTINE ZGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
        #     $                   WORK, LWORK, RWORK, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOBU, JOBVT
        #      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
        #*     .. Array Arguments ..
        #      DOUBLE PRECISION   RWORK( * ), S( * )
        #      COMPLEX*16         A( LDA, * ), U( LDU, * ), VT( LDVT, * ),
        #     $                   WORK( * )
        function jl_lapack_gesvd(jobu, jobvt, m, n, A::Matrix{$celtype}, lda, S, U, ldu, 
                                 VT, ldvt, work, lwork, rwork)
            info = Array(Int32, 1)
            ccall(dlsym(libLAPACK, $complex_gesvd),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$celtype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{$celtype}, Ptr{Int32}, Ptr{$celtype}, Ptr{Int32},
                   Ptr{$celtype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  jobu, jobvt, int32(m), int32(n), A, int32(lda), S, U, int32(ldu), 
                  VT, int32(ldvt), work, int32(lwork), rwork, info)
            return info[1]
        end

    end
end

@jl_lapack_gesvd_macro :dgesvd_ :zgesvd_ Float64 Complex128
@jl_lapack_gesvd_macro :sgesvd_ :cgesvd_ Float32 Complex64

function svd{T}(A::Matrix{T})
    jobu = "A"
    jobvt = "A"
    m, n = size(A)
    k = min(m,n)
    X = copy(A)
    S = Array(typeof(real(A[1])), k)
    U = Array(T, m, m)
    VT = Array(T, n, n)
    if iscomplex(A)
        rwork = Array(typeof(real(A[1])), 5*min(m,n))
    end

    # Workspace query
    work = zeros(T, 1)
    lwork = -1
    if iscomplex(A)
        info = jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(A,2), S, U, m, VT, n, work, lwork, rwork)
    else
        info = jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(A,2), S, U, m, VT, n, work, lwork)
    end

    if info == 0; lwork = real(work[1]); work = Array(T, long(lwork));
    else error("Error in LAPACK gesvd"); end

    # Compute SVD
    if iscomplex(A)
        info = jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(A,2), S, U, m, VT, n, work, lwork, rwork)
    else
        info = jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(A,2), S, U, m, VT, n, work, lwork)
    end

    SIGMA = zeros(T, m, n)
    for i=1:k; SIGMA[i,i] = S[i]; end

    if info == 0; return (U, SIGMA, VT); end
    error("Error in LAPACK gesvd");
end

macro jl_lapack_backslash_macro(gesv, posv, gels, trtrs, eltype)
    quote

        # SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        # *     ..
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function jl_lapack_gesv(n, nrhs, A::AbstractMatrix{$eltype}, lda, ipiv, B, ldb)
            info = Array(Int32, 1)
            a = pointer(A)
            b = pointer(B)
            ccall(dlsym(libLAPACK, $gesv),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  int32(n), int32(nrhs), a, int32(lda), ipiv, b, int32(ldb), info)
            return info[1]
        end

        #     SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function jl_lapack_posv(uplo, n, nrhs, A::AbstractMatrix{$eltype}, lda, B, ldb)
            info = Array(Int32, 1)
            a = pointer(A)
            b = pointer(B)
            ccall(dlsym(libLAPACK, $posv),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  uplo, int32(n), int32(nrhs), a, int32(lda), b, int32(ldb), info)
            return info[1]
        end

        #      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO)
        # *     .. Scalar Arguments ..
        #       CHARACTER          TRANS
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
        function jl_lapack_gels(trans, m, n, nrhs, A::AbstractMatrix{$eltype}, lda, B, ldb, work, lwork)
            info = Array(Int32, 1)
            a = pointer(A)
            b = pointer(B)
            ccall(dlsym(libLAPACK, $gels),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  trans, int32(m), int32(n), int32(nrhs), a, int32(lda), 
                  b, int32(ldb), work, int32(lwork), info)
            return info[1]
        end

        #      SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          DIAG, TRANS, UPLO
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function jl_lapack_trtrs(uplo, trans, diag, n, nrhs, A::AbstractMatrix{$eltype}, lda, B, ldb)
            info = Array(Int32, 1)
            a = pointer(A)
            b = pointer(B)
            ccall(dlsym(libLAPACK, $trtrs),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
                  uplo, trans, diag, int32(n), int32(nrhs), a, int32(lda), b, int32(ldb), info)
            return info[1]
        end

    end
end

@jl_lapack_backslash_macro :dgesv_ :dposv_ :dgels_ :dtrtrs_ Float64
@jl_lapack_backslash_macro :sgesv_ :sposv_ :sgels_ :strtrs_ Float32
@jl_lapack_backslash_macro :zgesv_ :zposv_ :zgels_ :ztrtrs_ Complex128
@jl_lapack_backslash_macro :cgesv_ :cposv_ :cgels_ :ctrtrs_ Complex64

function (\){T}(A::Matrix{T}, B::VecOrMat{T})
    m, n = size(A)
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
            if ishermitian(Acopy) && all([ Acopy[i,i] > 0 | i=1:n ])
                case = :spd
            end

            if case == :spd
                info = jl_lapack_posv("U", n, nrhs, Acopy, n, X, n)
                if info != 0
                    Acopy = copy(A)
                    case = :general
                end
            end

            if case == :general
                info = jl_lapack_gesv(n, nrhs, Acopy, n, ipiv, X, n)
            end

        else # Triangular
            uplo = "U"
            if case == :lower_triangular; uplo = "L"; end

            info = jl_lapack_trtrs(uplo, "N", "N", n, nrhs, Acopy, n, X, n)
        end

    else # Rectangular

        # Workspace query
        lwork = -1
        work = zeros(T,1)
        Y = Array(T, max(m,n), nrhs)
        Y[1:size(X,1), 1:nrhs] = X

        info = jl_lapack_dgels("N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork)

        if info == 0
            lwork = real(work[1])
            work = Array(T, long(lwork))
        else
            error("Error in LAPACK gels")
        end

        info = jl_lapack_dgels("N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork)

        ##if B is a vector, format answer as vector
        if isa(B, Vector)
            X = zeros(T, size(Y,1))
            for i = 1:size(Y,1); X[i] = Y[i,1]; end
        else
            X = Y
        end
    end # Square / Rectangular

    if info == 0; return X; end
    error("Error in LAPACK solving A*X = B")

end


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
