
for (potrf, elty) in ((:dpotrf_,:Float64), (:spotrf_,:Float32),
                      (:zpotrf_,:Complex128), (:cpotrf_,:Complex64))
    @eval begin
        # SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function _jl_lapack_potrf(uplo, n, A::StridedMatrix{$elty}, lda)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $string(potrf)),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  uplo, &n, A, &lda, info)
            return info[1]
        end

    end
end

# chol() does not check that input matrix is symmetric/hermitian
# It simply uses upper triangular half

chol{T<:Integer}(x::StridedMatrix{T}) = chol(float64(x))

function chol{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    R = chol!(copy(A))
end

function chol!{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    if stride(A,1) != 1; error("chol: matrix columns must have contiguous elements"); end
    n = int32(size(A, 1))
    info = _jl_lapack_potrf("U", n, A, stride(A,2))

    if info == 0; 
        # Zero out the lower triangular part of the result
        for j=1:n
            for i=(j+1):n
                A[i,j] = 0
            end
        end
        return A 
    end

    if info  > 0; error("matrix not positive definite"); end
    error("error in CHOL")
end

for (getrf, elty) in ((:dgetrf_,:Float64), (:sgetrf_,:Float32),
                      (:zgetrf_,:Complex128), (:cgetrf_,:Complex64))
    @eval begin
        # SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, M, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * )
        function _jl_lapack_getrf(m, n, A::StridedMatrix{$elty}, lda, ipiv)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $string(getrf)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, A, &lda, ipiv, info)
            return info[1]
        end
    end
end

lu{T<:Integer}(x::StridedMatrix{T}) = lu(float64(x))

function lu{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    (LU, P) = lu!(copy(A))
    m, n = size(A)

    L = m >= n ? tril(LU, -1) + eye(m,n) : tril(LU, -1)[:, 1:m] + eye(m,m)
    U = m <= n ? triu(LU) : triu(LU)[1:n, :]
    return (L, U, P)
end

function lu!{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    if stride(A,1) != 1; error("lu: matrix columns must have contiguous elements"); end
    m, n = size(A)
    ipiv = Array(Int32, min(m,n))

    info = _jl_lapack_getrf(m, n, A, stride(A,2), ipiv)

    #if info > 0; error("matrix is singular"); end
    P = [1:m]
    for i=1:min(m,n)
        t = P[i]
        P[i] = P[ipiv[i]]
        P[ipiv[i]] = t
    end

    if info >= 0; return (A, P); end
    error("error in LU")
end


for (real_geqp3, complex_geqp3, orgqr, ungqr, elty, celty) in
    (("dgeqp3_","zgeqp3_","dorgqr_","zungqr_",:Float64,:Complex128),
     ("sgeqp3_","cgeqp3_","sorgqr_","cungqr_",:Float32,:Complex64))
    @eval begin

        # SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       INTEGER            JPVT( * )
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function _jl_lapack_geqp3(m, n, A::StridedMatrix{$elty}, lda, jpvt, tau, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $real_geqp3),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, A, &lda, jpvt, tau, work, &lwork, info)
            return info[1]
        end

        # SUBROUTINE ZGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, RWORK, INFO )
        #*      .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        #*      .. Array Arguments ..
        #       INTEGER            JPVT( * )
        #       DOUBLE PRECISION   RWORK( * )
        #       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
        function _jl_lapack_geqp3(m, n, A::StridedMatrix{$celty}, lda, jpvt, tau, work, lwork, rwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $complex_geqp3),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$celty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$celty}, Ptr{$celty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &m, &n, A, &lda, jpvt, tau, work, &lwork, rwork, info)
            return info[1]
        end

        # SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, K, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function _jl_lapack_orgqr(m, n, k, A::StridedMatrix{$elty}, lda, tau, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $orgqr),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, &k, A, &lda, tau, work, &lwork, info)
            return info[1]
        end

        # SUBROUTINE ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        #*     .. Scalar Arguments ..
        #      INTEGER            INFO, K, LDA, LWORK, M, N
        #*     .. Array Arguments ..
        #      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
        function _jl_lapack_ungqr(m, n, k, A::StridedMatrix{$celty}, lda, tau, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $ungqr),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$celty},
                   Ptr{Int32}, Ptr{$celty}, Ptr{$celty}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, &k, A, &lda, tau, work, &lwork, info)
            return info[1]
        end

    end
end

#possible TODO: economy mode?

qr{T<:Integer}(x::StridedMatrix{T}) = qr(float64(x))

function qr{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    m, n = size(A)
    QR = copy(A)
    jpvt = zeros(Int32, n)
    k = min(m,n)
    tau = Array(T, k)
    if iscomplex(A)
        rwork = zeros(typeof(real(A[1])), int(2*n))
    end

    # Workspace query for QR factorization
    work = zeros(T,1)
    lwork = int32(-1)
    if iscomplex(A)
        info = _jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork, rwork)
    else
        info = _jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork)
    end

    if info == 0; lwork = real(work[1]); work = Array(T, int(lwork))
    else error("error in LAPACK geqp3"); end

    # Compute QR factorization
    if iscomplex(A)
        info = _jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork, rwork)
    else
        info = _jl_lapack_geqp3(m, n, QR, stride(QR,2), jpvt, tau, work, lwork)
    end

    #if info > 0; error("matrix is singular"); end

    R = triu(QR)

    # Workspace query to form Q
    lwork2 = int32(-1)
    if iscomplex(A)
        info = _jl_lapack_ungqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    else
        info = _jl_lapack_orgqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    end

    if info == 0; lwork2 = real(work[1]); work = Array(T, int(lwork2))
    else error("error in LAPACK orgqr/ungqr"); end

    # Compute Q
    if iscomplex(A)
        info = _jl_lapack_ungqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    else
        info = _jl_lapack_orgqr(m, k, k, QR, stride(QR,2), tau, work, lwork2)
    end

    if info >= 0; return (QR[:, 1:k], R[1:k, :], jpvt); end
    error("error in LAPACK orgqr/ungqr");
end

for (syev, heev, real_geev, complex_geev, elty, celty) in
    (("dsyev_","zheev_","dgeev_","zgeev_",:Float64,:Complex128),
     ("ssyev_","cheev_","sgeev_","cgeev_",:Float32,:Complex64))
    @eval begin

        #       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBZ, UPLO
        #       INTEGER            INFO, LDA, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
        function _jl_lapack_syev(jobz, uplo, n, A::StridedMatrix{$elty}, lda, W, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $syev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  jobz, uplo, &n, A, &lda, W, work, &lwork, info)
            return info[1]
        end

        #      SUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOBZ, UPLO
        #      INTEGER            INFO, LDA, LWORK, N
        #*     .. Array Arguments ..
        #      DOUBLE PRECISION   RWORK( * ), W( * )
        #      COMPLEX*16         A( LDA, * ), WORK( * )
        function _jl_lapack_heev(jobz, uplo, n, A::StridedMatrix{$celty}, lda, W, work, lwork, rwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $heev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$celty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$celty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  jobz, uplo, &n, A, &lda, W, work, &lwork, rwork, info)
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
        function _jl_lapack_geev(jobvl, jobvr, n, A::StridedMatrix{$elty}, lda, WR, WI, VL, ldvl, 
                                VR, ldvr, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $real_geev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, 
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  jobvl, jobvr, &n, A, &lda, WR, WI, VL, &ldvl,
                  VR, &ldvr, work, &lwork, info)
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
        function _jl_lapack_geev(jobvl, jobvr, n, A::StridedMatrix{$celty}, lda, W, VL, ldvl, 
                                VR, ldvr, work, lwork, rwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $complex_geev),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$celty}, Ptr{Int32},
                   Ptr{$celty}, Ptr{$celty}, Ptr{Int32}, 
                   Ptr{$celty}, Ptr{Int32}, Ptr{$celty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  jobvl, jobvr, &n, A, &lda, W, VL, &ldvl, 
                  VR, &ldvr, work, &lwork, rwork, info)
            return info[1]
        end

    end
end

eig{T<:Integer}(x::StridedMatrix{T}) = eig(float64(x))

function eig{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T})
    m, n = size(A)
    if m != n; error("matrix argument must be square"); end

    if ishermitian(A)

        jobz = "V"
        uplo = "U"
        EV = copy(A)
        W = Array(typeof(real(A[1])), n)
        if iscomplex(A)
            rwork = Array(typeof(real(A[1])), int(max(3*n-2, 1)))
        end

        # Workspace query
        work = zeros(T,1)
        lwork = -1

        if iscomplex(A)
            info = _jl_lapack_heev(jobz, uplo, n, EV, stride(EV,2), W, work, lwork, rwork)
        else
            info = _jl_lapack_syev(jobz, uplo, n, EV, stride(EV,2), W, work, lwork)
        end

        if info == 0; lwork = real(work[1]); work = Array(T, int(lwork));
        else error("error in LAPACK syev/heev"); end

        # Compute eigenvalues, eigenvectors
        if iscomplex(A)
            info = _jl_lapack_heev(jobz, uplo, n, EV, stride(EV,2), W, work, lwork, rwork)
        else
            info = _jl_lapack_syev(jobz, uplo, n, EV, stride(EV,2), W, work, lwork)
        end

        if info == 0; return (W, EV); end
        error("error in LAPACK syev/heev");

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
            info = _jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(Acopy,2), W, VL, n, VR, n, work, lwork, rwork)
        else
            info = _jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(Acopy,2), WR, WI, VL, n, VR, n, work, lwork)
        end

        if info == 0; lwork = real(work[1]); work = Array(T, int(lwork));
        else error("error in LAPACK geev"); end

        # Compute eigenvalues, eigenvectors
        if iscomplex(A)
            info = _jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(Acopy,2), W, VL, n, VR, n, work, lwork, rwork)
        else
            info = _jl_lapack_geev(jobvl, jobvr, n, Acopy, stride(Acopy,2), WR, WI, VL, n, VR, n, work, lwork)
        end
        
        if info != 0; error("error in LAPACK geev"); end

        if iscomplex(A)
            return (W, VR)
        else
            evec = complex(zeros(T, n, n), zeros(T, n, n))
            j = 1
            while j <= n
                if WI[j] == 0.0
                    evec[:,j] = VR[:,j]
                else
                    evec[:,j]   = VR[:,j] + im*VR[:,j+1]
                    evec[:,j+1] = VR[:,j] - im*VR[:,j+1]
                    j += 1
                end
                j += 1
            end
            return (complex(WR, WI), evec)
        end

    end # symmetric / non-symmetric case

end

function trideig(d::Vector{Float64}, e::Vector{Float64})
    dcopy = copy(d)
    ecopy = copy(e)
    ccall(dlsym(_jl_liblapack, :dsteqr_),
          Void,
          (Ptr{Uint8},Ptr{Int32},Ptr{Float64},
           Ptr{Float64},Ptr{Float64},Ptr{Int32},
           Ptr{Float64},Ptr{Int32}),
          "N", &numel(d), dcopy, ecopy,
          &0.0, &numel(d), &0.0, &0)
    return dcopy
end


for (real_gesvd, complex_gesvd, elty, celty) in
    (("dgesvd_","zgesvd_",:Float64,:Complex128),
     ("sgesvd_","cgesvd_",:Float32,:Complex64))
    @eval begin

        # SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBU, JOBVT
        #       INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
        #      $                   VT( LDVT, * ), WORK( * )
        function _jl_lapack_gesvd(jobu, jobvt, m, n, A::StridedMatrix{$elty}, lda, S, U, ldu, 
                                 VT, ldvt, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $real_gesvd),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  jobu, jobvt, &m, &n, A, &lda, S, U, &ldu, 
                  VT, &ldvt, work, &lwork, info)
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
        function _jl_lapack_gesvd(jobu, jobvt, m, n, A::StridedMatrix{$celty}, lda, S, U, ldu, 
                                 VT, ldvt, work, lwork, rwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $complex_gesvd),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$celty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$celty}, Ptr{Int32}, Ptr{$celty}, Ptr{Int32},
                   Ptr{$celty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  jobu, jobvt, &m, &n, A, &lda, S, U, &ldu, 
                  VT, &ldvt, work, &lwork, rwork, info)
            return info[1]
        end

    end
end

svd(A) = svd(A,true)
svdvals(A) = svd(A,false)[2]

svd{T<:Integer}(x::StridedMatrix{T},vecs) = svd(float64(x),vecs)

function svd{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},vecs::Bool)
    m, n = size(A)
    k = min(m,n)
    X = copy(A)
    S = Array(typeof(real(A[1])), k)
    if vecs
        jobu = jobvt = "A"
        U = Array(T, m, m)
        VT = Array(T, n, n)
    else
        jobu = jobvt = "N"
        U = Array(T, 0, 0)
        VT = Array(T, 0, 0)
    end
    if iscomplex(A)
        rwork = Array(typeof(real(A[1])), 5*min(m,n))
    end

    # Workspace query
    work = zeros(T, 1)
    lwork = -1
    if iscomplex(A)
        info = _jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(X,2), S, U, m, VT, n, work, lwork, rwork)
    else
        info = _jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(X,2), S, U, m, VT, n, work, lwork)
    end

    if info == 0; lwork = real(work[1]); work = Array(T, int(lwork));
    else error("error in LAPACK gesvd"); end

    # Compute SVD
    if iscomplex(A)
        info = _jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(X,2), S, U, m, VT, n, work, lwork, rwork)
    else
        info = _jl_lapack_gesvd(jobu, jobvt, m, n, X, stride(X,2), S, U, m, VT, n, work, lwork)
    end

    if info != 0; error("error in LAPACK gesvd"); end
    return (U, S, VT)
end

for (gesv, posv, gels, trtrs, elty) in (("dgesv_","dposv_","dgels_","dtrtrs_",:Float64),
                                        ("sgesv_","sposv_","sgels_","strtrs_",:Float32),
                                        ("zgesv_","zposv_","zgels_","ztrtrs_",:Complex128),
                                        ("cgesv_","cposv_","cgels_","ctrtrs_",:Complex64))
    @eval begin
        # SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        # *     ..
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_gesv(n, nrhs, A::StridedMatrix{$elty}, lda, ipiv, B, ldb)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $gesv),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, A, &lda, ipiv, B, &ldb, info)
            return info[1]
        end

        #     SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_posv(uplo, n, nrhs, A::StridedMatrix{$elty}, lda, B, ldb)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $posv),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  uplo, &n, &nrhs, A, &lda, B, &ldb, info)
            return info[1]
        end

        #      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO)
        # *     .. Scalar Arguments ..
        #       CHARACTER          TRANS
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
        function _jl_lapack_gels(trans, m, n, nrhs, A::StridedMatrix{$elty}, lda, B, ldb, work, lwork)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $gels),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  trans, &m, &n, &nrhs, A, &lda, 
                  B, &ldb, work, &lwork, info)
            return info[1]
        end

        #      SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          DIAG, TRANS, UPLO
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_trtrs(uplo, trans, diag, n, nrhs, A::StridedMatrix{$elty}, lda, B, ldb)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $trtrs),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  uplo, trans, diag, &n, &nrhs, A, &lda, B, &ldb, info)
            return info[1]
        end

    end
end

function (\){T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T}, B::VecOrMat{T})
    m, n = size(A)
    mrhs = size(B, 1)
    if m != mrhs; error("number of rows of arguments do not match"); end
    if isa(B, Vector); nrhs = 1; else nrhs = size(B, 2); end
    Acopy = copy(A)
    X = copy(B)

    if m == n # Square
        case = :general
        if istriu(A); case = :upper_triangular; end
        if istril(A); case = :lower_triangular; end

        if case == :general # General
            ipiv = Array(Int32, n)

            # Check for SPD matrix
            if ishermitian(Acopy) && all([ Acopy[i,i] > 0 for i=1:n ])
                case = :spd
            end

            if case == :spd
                info = _jl_lapack_posv("U", n, nrhs, Acopy, n, X, n)
                if info != 0
                    Acopy = copy(A)
                    case = :general
                end
            end

            if case == :general
                info = _jl_lapack_gesv(n, nrhs, Acopy, n, ipiv, X, n)
            end

        else # Triangular
            uplo = "U"
            if case == :lower_triangular; uplo = "L"; end

            info = _jl_lapack_trtrs(uplo, "N", "N", n, nrhs, Acopy, n, X, n)
        end

    else # Rectangular

        # Workspace query
        lwork = -1
        work = zeros(T,1)
        Y = Array(T, max(m,n), nrhs)
        Y[1:size(X,1), 1:nrhs] = X

        info = _jl_lapack_gels("N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork)

        if info == 0
            lwork = real(work[1])
            work = Array(T, int(lwork))
        else
            error("error in LAPACK gels")
        end

        info = _jl_lapack_gels("N", m, n, nrhs, Acopy, m, Y, max(m,n), work, lwork)

        X = Y[1:n, :]

        ##if B is a vector, format answer as vector
        if isa(B, Vector); X = reshape(X, (n,)); end

    end # Square / Rectangular

    if info == 0; return X; end
    error("error in LAPACK solving A*X = B")

end

(\){T1<:Real, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(float64(A), float64(B))

(/){T1<:Real, T2<:Real}(A::StridedVecOrMat{T1}, B::StridedVecOrMat{T2}) = (B' \ A')'
