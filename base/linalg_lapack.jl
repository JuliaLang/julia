
type LapackException <: Exception
    info::Int32
end

# Decompositions
for (gbtrf, geqrf, geqp3, getrf, potrf, elty) in
    ((:dgbtrf_,:dgeqrf_,:dgeqp3_,:dgetrf_,:dpotrf_,:Float64),
     (:sgbtrf_,:sgeqrf_,:sgeqp3_,:sgetrf_,:spotrf_,:Float32),
     (:zgbtrf_,:zgeqrf_,:zgeqp3_,:zgetrf_,:zpotrf_,:Complex128),
     (:cgbtrf_,:cgeqrf_,:cgeqp3_,:cgetrf_,:cpotrf_,:Complex64))
    @eval begin
        # SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function _jl_lapack_potrf(uplo, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_potrf: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            m    = int32(size(A, 1))
            n    = int32(size(A, 2))
            lda  = int32(stride(A, 2))
            ccall(dlsym(_jl_liblapack, $string(potrf)),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  uplo, &n, A, &lda, info)
## _jl_lapack_potrf is unusual in that it does not return the
## factorization, which overwrites A, but instead returns the error
## code. A symmetric (Hermitian) matrix can fail to be positive
## definite and you don't know if it is until you try the
## decompostion.

## Alternatively, A could be returned and the call to _jl_lapack_potrf
## could be wrapped in a try/catch block
            return info[1]
        end
        # SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, M, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * )
        function _jl_lapack_getrf(A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_potrf: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            m    = int32(size(A, 1))
            n    = int32(size(A, 2))
            lda  = int32(stride(A, 2))
            ipiv = Array(Int32, n)
            ccall(dlsym(_jl_liblapack, $string(getrf)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, A, &lda, ipiv, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            A, ipiv
        end
        # SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function _jl_lapack_geqrf(A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_potrf: matrix columns must have contiguous elements");
            end
            info  = Array(Int32, 1)
            m     = int32(size(A, 1))
            n     = int32(size(A, 2))
            lda   = int32(stride(A, 2))
            tau   = Array($elty, n)
            lwork = int32(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(dlsym(_jl_liblapack, $string(geqrf)),
                      Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &m, &n, A, &lda, tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, tau
        end
        # SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       INTEGER            JPVT( * )
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function _jl_lapack_geqp3(A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_geqp3: matrix columns must have contiguous elements");
            end
            info  = Array(Int32, 1)
            m     = int32(size(A, 1))
            n     = int32(size(A, 2))
            lda   = int32(stride(A, 2))
            jpvt  = Array(Int32, n)
            tau   = Array($elty, n)
            lwork = int32(-1)
            work  = Array($elty, (1,))
            Rtyp  = typeof(real(work[1]))
            cmplx = iscomplex(A)
            if cmplx rwork = Array(Rtyp, 2n) end
            for i in 1:2
                if cmplx
                    ccall(dlsym(_jl_liblapack, $string(geqp3)),
                          Void,
                          (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Rtyp}, Ptr{Int32}),
                          &m, &n, A, &lda, jpvt, tau, work, &lwork, rwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(geqp3)),
                          Void,
                          (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}),
                          &m, &n, A, &lda, jpvt, tau, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            A, tau, jpvt
        end
        # SUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, KL, KU, LDAB, M, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   AB( LDAB, * )
        function _jl_lapack_gbtrf(kl::Integer, ku::Integer, AB::StridedMatrix{$elty})
            if stride(AB,1) != 1
                error("_jl_lapack_gbtrf: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            m, n = size(AB)
            mnmn = min(m, n)
            ldab = stride(AB, 2)
            ipiv = Array(Int32, mnmn)
            ccall(dlsym(_jl_liblapack, $string(gbtrf)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, &kl, &ku, AB, &ldab, ipiv, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            AB, ipiv
        end
    end
end

# extractors
for (orgqr, elty) in
    ((:dorgqr_,:Float64),
     (:sorgqr_,:Float32),
     (:zungqr_,:Complex128),
     (:cungqr_,:Complex64))
    @eval begin
        # SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, K, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function _jl_lapack_orgqr(A::StridedMatrix{$elty}, tau)
            if stride(A,1) != 1
                error("_jl_lapack_orgqr: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            m, n = size(A)
            lda   = stride(A, 2)
            lwork = -1
            work  = Array($elty, (1,))
            for i in 1:2
                ccall(dlsym(_jl_liblapack, $string(orgqr)),
                      Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                       Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &m, &n, &n, A, &lda, tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A
        end
    end
end

# chol() does not check that input matrix is symmetric/hermitian
# It simply uses upper triangular half
chol{T<:Integer}(x::StridedMatrix{T}) = chol(float64(x))

function chol{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    R = chol!(copy(A))
end

## chol! overwrites A with either the upper or lower triangular
## Cholesky factor (default is upper)
chol!{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T}) = chol!(A, "U")
function chol!{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T}, uplo::String)
    info = _jl_lapack_potrf(uplo, A)
    if info != 0 error("chol: matrix is not positive definite, error $info") end
    uppercase(uplo)[1] == 'U' ? triu(A) : tril(A)
end


lu{T<:Integer}(x::StridedMatrix{T}) = lu(float64(x))

## LU decomposition returning L and U separately and P as a permutation
function lu{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    LU, ipiv = _jl_lapack_getrf(copy(A))
    m, n = size(A)

    L = m >= n ? tril(LU, -1) + eye(m,n) : tril(LU, -1)[:, 1:m] + eye(m,m)
    U = m <= n ? triu(LU) : triu(LU)[1:n, :]
    P = [1:m]
    for i=1:min(m,n)
        t = P[i]
        P[i] = P[ipiv[i]]
        P[ipiv[i]] = t
    end
    L, U, P
end

## lu! overwrites A with the components of the LU decomposition
## Note that returned value includes the pivot indices, not the permutation
function lu!{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    _jl_lapack_getrf(A)
end

## QR decomposition without column pivots
qr{T<:Integer}(x::StridedMatrix{T}) = qr(float64(x))

function qr{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    aa, tau = _jl_lapack_geqrf(copy(A))
    R = triu(aa[1:min(size(A)),:])
    _jl_lapack_orgqr(aa, tau), R
end

## QR decomposition with column pivots
qrp{T<:Integer}(x::StridedMatrix{T}) = qrp(float64(x))

function qrp{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    aa, tau, jpvt = _jl_lapack_geqp3(copy(A))
    R = triu(aa[1:min(size(A)),:])
    _jl_lapack_orgqr(aa, tau), R, jpvt
end


# eigenvalue-eigenvector, symmetric (Hermitian) or general cases
for (syev, geev, elty) in
    ((:dsyev_,:dgeev_,:Float64),
     (:ssyev_,:sgeev_,:Float32),
     (:zheev_,:zgeev_,:Complex128),
     (:cheev_,:cgeev_,:Complex64))
    @eval begin
        #       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBZ, UPLO
        #       INTEGER            INFO, LDA, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
        function _jl_lapack_syev(jobz::String, uplo::String, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_syev: matrix columns must have contiguous elements");
            end
            m, n  = size(A)
            if m != n
                error("_jl_lapack_syev: symmetric or Hermitian matrices must be square")
            end
            lda   = stride(A, 2)
            W     = Array($elty, n)
            lwork = -1
            work  = Array($elty, (1,))
            info  = Array(Int32, 1)
            Rtyp  = typeof(real(work[1]))
            for i in 1:2
                if iscomplex(A)
                    rwork = Array(Rtyp, max(1, 2n-1))
                    ccall(dlsym(_jl_liblapack, $string(syev)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Rtyp}, Ptr{Int32}),
                          jobz, uplo, &n, A, &lda, W, work, &lwork, rwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(syev)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}),
                          jobz, uplo, &n, A, &lda, W, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            uppercase(jobz)[1] == 'V' ? (W, A) : W
        end
        
        #      SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
        #      $                  LDVR, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBVL, JOBVR
        #       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
        #      $                   WI( * ), WORK( * ), WR( * )
        function _jl_lapack_geev(jobvl::String, jobvr::String, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_geev: matrix columns must have contiguous elements");
            end
            m, n  = size(A)
            if m != n
                error("_jl_lapack_geev: matrix for eigen-decomposition must be square")
            end
            lda   = stride(A, 2)
            lvecs = uppercase(jobvl)[1] == 'V'
            rvecs = uppercase(jobvr)[1] == 'V'
            VL    = Array($elty, lvecs ? (n, n) : (n, 0))
            VR    = Array($elty, rvecs ? (n, n) : (n, 0))            
            n     = n
            lwork = -1
            work  = Array($elty, 1)
            info  = Array(Int32, 1)
            Rtyp  = typeof(real(work[1]))
            cmplx = iscomplex(A)
            if cmplx
                W     = Array($elty, n)
                rwork = Array(Rtyp, 2n)
            else
                WR    = Array($elty, n)
                WI    = Array($elty, n)
            end
            for i = 1:2
                if cmplx
                    ccall(dlsym(_jl_liblapack, $string(geev)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, 
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Rtyp}, Ptr{Int32}),
                          jobvl, jobvr, &n, A, &lda, W, VL, &n, VR, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(geev)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Int32}),
                          jobvl, jobvr, &n, A, &lda, WR, WI, VL, &n,
                          VR, &n, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            cmplx ? (VL, W, VR) : (VL, WR, WI, VR)
        end
    end
end

eig{T<:Integer}(x::StridedMatrix{T}) = eig(float64(x))

function eig{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T})
    if ishermitian(A) return _jl_lapack_syev("V","U",copy(A)) end
                                        # Only compute right eigenvectors
    if iscomplex(A) return _jl_lapack_geev("N","V",copy(A))[2:3] end
    VL, WR, WI, VR = _jl_lapack_geev("N","V",copy(A))
    if all(WI .== 0.) return WR, VR end
    n = size(A, 2)
    evec = complex(zeros(T, n, n))
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
    complex(WR, WI), evec
end

function trideig(d::Vector{Float64}, e::Vector{Float64})
    dcopy = copy(d)
    ecopy = copy(e)
    ccall(dlsym(_jl_liblapack, :dsteqr_),
          Void,
          (Ptr{Uint8}, Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Float64},
          Ptr{Int32}, Ptr{Float64}, Ptr{Int32}),
          "N", &numel(d), dcopy, ecopy, &0.0, &numel(d), &0.0, &0)
    dcopy
end

# singular value decomposition - two forms, the gesdd form is usually faster
for (gesvd, gesdd, elty) in
    ((:dgesvd_,:dgesdd_,:Float64),
     (:sgesvd_,:sgesdd_,:Float32),
     (:zgesvd_,:zgesdd_,:Complex128),
     (:cgesvd_,:cgesdd_,:Complex64))
    @eval begin
        #    SUBROUTINE DGESDD( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK,
        #                   LWORK, IWORK, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOBZ
        #      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
        #*     ..
        #*     .. Array Arguments ..
        #      INTEGER            IWORK( * )
        #      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
        #                        VT( LDVT, * ), WORK( * )
        function _jl_lapack_gesdd(jobz::String, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_gesdd: matrix columns must have contiguous elements");
            end
            m, n   = size(A)
            minmn  = min(m, n)
            job    = uppercase(jobz)[1]
            if job == 'A'
                U  = Array($elty, (m, m))
                VT = Array($elty, (n, n))
            elseif job == 'S'
                U  = Array($elty, (m, minmn))
                VT = Array($elty, (n, minmn))
            elseif job == 'O'
                U  = Array($elty, m >= n ? (m, 1) : (m, m))
                VT = Array($elty, m >= n ? (n, n) : (n, 1))
            else
                U  = Array($elty, (m, 1))
                VT = Array($elty, (n, 1))
            end
            lda    = stride(A, 2)
            lwork  = -1
            work   = Array($elty, 1)
            iwork  = Array(Int32, 8minmn)
            info   = Array(Int32, 1)
            Rtyp   = typeof(real(work[1]))
            S      = Array(Rtyp, minmn)
            cmplx  = iscomplex(A)
            if cmplx rwork = Array(Rtyp, job == 'N' ? 5minmn : minmn*max(5minmn+7,2max(m,n)+2minmn+1)) end
            for i = 1:2
                if iscomplex(A)
                    ccall(dlsym(_jl_liblapack, $string(gesdd)),
                          Void,
                          (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Rtyp}, Ptr{Int32}, Ptr{Int32}),
                          jobz, &m, &n, A, &lda, S, U, &m, VT, &n,
                          work, &lwork, rwork, iwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(gesdd)),
                          Void,
                          (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}, Ptr{Int32}),
                          jobz, &m, &n, A, &lda, S, U, &m, VT, &n,
                          work, &lwork, iwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            if job == 'O' if m >= n return (A, S, VT) else return (U, S, A) end end
            return (U, S, VT)
        end

        # SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBU, JOBVT
        #       INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
        #      $                   VT( LDVT, * ), WORK( * )
        function _jl_lapack_gesvd(jobu, jobvt, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_gesvd: matrix columns must have contiguous elements");
            end
            m, n   = size(A)
            minmn  = min(m, n)          # can't map to int32 b/c of U and VT
            j1     = uppercase(jobu)[1]
            U      = Array($elty, j1 == 'A'? (m, m):(j1 == 'S'? (m, minmn) : (m, 0)))
            j2     = uppercase(jobvt)[1]
            VT     = Array($elty, j2 == 'A'? (n, n):(j2 == 'S'? (n, minmn) : (n, 0)))
            lda    = stride(A, 2)
            lwork  = -1
            work   = Array($elty, 1)
            info   = Array(Int32, 1)
            Rtyp   = typeof(real(work[1]))
            S      = Array(Rtyp, minmn)
            cmplx  = iscomplex(A)
            if cmplx rwork = Array(Rtyp, 5minmn) end
            for i in 1:2
                if cmplx
                    ccall(dlsym(_jl_liblapack, $string(gesvd)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Rtyp}, Ptr{Int32}),
                          jobu, jobvt, &m, &n, A, &lda, S, U, &m, VT, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(gesvd)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Int32}),
                          jobu, jobvt, &m, &n, A, &lda, S, U, &m, VT, &n,
                          work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            if j1 == 'O' return A, S, VT end
            if j2 == 'O' return U, S, A end
            U, S, VT
        end
    end
end

function sdd{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},vecs::String)
    _jl_lapack_gesdd(vecs, copy(A))
end
sdd(A) = sdd(A, "A")
sdd{T<:Integer}(x::StridedMatrix{T},vecs) = sdd(float64(x),vecs)

svd(A) = svd(A,true)
svdvals(A) = svd(A,false)[2]

svd{T<:Integer}(x::StridedMatrix{T},vecs) = svd(float64(x),vecs)

function svd{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},vecs::Bool)
    _jl_lapack_gesvd(vecs ? "A" : "N", vecs ? "A" : "N", copy(A))
end

# direct solvers
for (gesv, posv, gels, trtrs, elty) in
    (("dgesv_","dposv_","dgels_","dtrtrs_",:Float64),
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
        function _jl_lapack_gesv(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_gesv: matrix columns must have contiguous elements");
            end
            m, n    = size(A)
            k       = size(B, 1)
            if (m != n || k != m) error("_jl_lapack_gesv: dimension mismatch") end
            nrhs    = size(B, 2)
            lda     = stride(A, 2)
            ldb     = isa(B, Vector) ? m : stride(B, 2)
            ipiv    = Array(Int32, n)
            info    = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $gesv),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, A, &lda, ipiv, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            A, ipiv, B
        end

        #     SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_posv(uplo::String, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_posv: matrix columns must have contiguous elements");
            end
            m, n    = size(A)
            k       = size(B, 1)
            if (m != n || k != m) error("_jl_lapack_posv: dimension mismatch") end
            nrhs    = size(B, 2)
            lda     = stride(A, 2)
            ldb     = isa(B, Vector) ? m : stride(B, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $posv),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  uplo, &n, &nrhs, A, &lda, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            A, B
        end

        #      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO)
        # *     .. Scalar Arguments ..
        #       CHARACTER          TRANS
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
        function _jl_lapack_gels(trans, A::StridedMatrix{$elty},
                                 B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_gels: matrix columns must have contiguous elements");
            end
            m, n    = size(A)
            vecb    = isa(B, Vector)
            k       = size(B, 1)
            if k != m error("_jl_lapack_gels: dimension mismatch") end
            nrhs    = size(B, 2)
            lda     = stride(A, 2)
            ldb     = vecb ? m : stride(B, 2)
            info    = Array(Int32, 1)
            work    = Array($elty, 1)
            lwork   = -1
            for i in 1:2
                ccall(dlsym(_jl_liblapack, $gels),
                      Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      trans, &m, &n, &nrhs, A, &lda, B, &ldb, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            R       = triu(A[1:n,1:n])
            X       = vecb ? B[1:n] : B[1:n,:]
            RSS     = vecb ? sum(B[(n+1):m].^2) : [sum(B[(n+1):m, i].^2) for i=1:n]
            R, X, RSS
        end

        #      SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          DIAG, TRANS, UPLO
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_trtrs(uplo::String, trans::String, diag::String,
                                  A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_trtrs: matrix columns must have contiguous elements");
            end
            m, n    = size(A)
            k       = size(B, 1)
            if (m != n || k != m) error("_jl_lapack_trtrs: dimension mismatch") end
            nrhs    = size(B, 2)
            lda     = stride(A, 2)
            ldb     = isa(B, Vector) ? m : stride(B, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $trtrs),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  uplo, trans, diag, &n, &nrhs, A, &lda, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end

function (\){T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T}, B::StridedVecOrMat{T})
    Acopy = copy(A)
    m, n  = size(Acopy)
    X     = copy(B)

    if m == n # Square
        if istriu(A) return _jl_lapack_trtrs("U", "N", "N", Acopy, X) end
        if istril(A) return _jl_lapack_trtrs("L", "N", "N", Acopy, X) end
                                        # Check for SPD matrix
        if ishermitian(Acopy) && all([ Acopy[i,i] > 0 for i=1:n ]) 
            try
                _jl_lapack_posv("U", Acopy, X)
                return X
            catch e
                if !isa(e, LapackException) throw(e) end
                Acopy[:] = A
                X[:] = B
            end
        end
        return _jl_lapack_gesv(Acopy, X)[3]
    end
    _jl_lapack_gels("N", Acopy, X)[2]
end

(\){T1<:Real, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(float64(A), float64(B))

# TODO: use *gels transpose argument
(/)(A::StridedVecOrMat, B::StridedVecOrMat) = (B' \ A')'
