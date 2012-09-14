typealias LapackChar Char

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
        function _jl_lapack_potrf(uplo::LapackChar, A::StridedMatrix{$elty})
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
                  &uplo, &n, A, &lda, info)
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
        function _jl_lapack_syev(jobz::LapackChar, uplo::LapackChar, A::StridedMatrix{$elty})
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
                          &jobz, &uplo, &n, A, &lda, W, work, &lwork, rwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(syev)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}),
                          &jobz, &uplo, &n, A, &lda, W, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            jobz == 'V' ? (W, A) : W
        end
        
        #      SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
        #      $                  LDVR, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBVL, JOBVR
        #       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
        #      $                   WI( * ), WORK( * ), WR( * )
        function _jl_lapack_geev(jobvl::LapackChar, jobvr::LapackChar, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_geev: matrix columns must have contiguous elements");
            end
            m, n  = size(A)
            if m != n
                error("_jl_lapack_geev: matrix for eigen-decomposition must be square")
            end
            lda   = stride(A, 2)
            lvecs = jobvl == 'V'
            rvecs = jobvr == 'V'
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
                          &jobvl, &jobvr, &n, A, &lda, W, VL, &n, VR, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(geev)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Int32}),
                          &jobvl, &jobvr, &n, A, &lda, WR, WI, VL, &n,
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
    if ishermitian(A) return _jl_lapack_syev('V','U',copy(A)) end
                                        # Only compute right eigenvectors
    if iscomplex(A) return _jl_lapack_geev('N','V',copy(A))[2:3] end
    VL, WR, WI, VR = _jl_lapack_geev('N','V',copy(A))
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
          &uint8('N'), &numel(d), dcopy, ecopy, &0.0, &numel(d), &0.0, &0)
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
        function _jl_lapack_gesdd(job::LapackChar, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_gesdd: matrix columns must have contiguous elements");
            end
            m, n   = size(A)
            minmn  = min(m, n)
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
                          &job, &m, &n, A, &lda, S, U, &m, VT, &n,
                          work, &lwork, rwork, iwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(gesdd)),
                          Void,
                          (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}, Ptr{Int32}),
                          &job, &m, &n, A, &lda, S, U, &m, VT, &n,
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
        function _jl_lapack_gesvd(jobu::LapackChar, jobvt::LapackChar, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_gesvd: matrix columns must have contiguous elements");
            end
            m, n   = size(A)
            minmn  = min(m, n)          # can't map to int32 b/c of U and VT
            U      = Array($elty, jobu  == 'A'? (m, m):(jobu  == 'S'? (m, minmn) : (m, 0)))
            VT     = Array($elty, jobvt == 'A'? (n, n):(jobvt == 'S'? (n, minmn) : (n, 0)))
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
                          &jobu, &jobvt, &m, &n, A, &lda, S, U, &m, VT, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(dlsym(_jl_liblapack, $string(gesvd)),
                          Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Int32}),
                          &jobu, &jobvt, &m, &n, A, &lda, S, U, &m, VT, &n,
                          work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            if jobu  == 'O' return A, S, VT end
            if jobvt == 'O' return U, S, A end
            U, S, VT
        end
    end
end

function sdd{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},vecs)
    _jl_lapack_gesdd(vecs, copy(A))
end
sdd{T<:Integer}(x::StridedMatrix{T},vecs) = sdd(float64(x),vecs)
sdd(A) = sdd(A, 'A')

svd(A) = svd(A,true)
svdvals(A) = svd(A,false)[2]

svd{T<:Integer}(x::StridedMatrix{T},vecs) = svd(float64(x),vecs)

function svd{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},vecs::Bool)
    _jl_lapack_gesvd(vecs ? 'A' : 'N', vecs ? 'A' : 'N', copy(A))
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
        function _jl_lapack_posv(uplo::LapackChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
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
                  &uplo, &n, &nrhs, A, &lda, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            A, B
        end

        #      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO)
        # *     .. Scalar Arguments ..
        #       CHARACTER          TRANS
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
        function _jl_lapack_gels(trans::LapackChar, A::StridedMatrix{$elty},
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
                      &trans, &m, &n, &nrhs, A, &lda, B, &ldb, work, &lwork, info)
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
        function _jl_lapack_trtrs(uplo::LapackChar, trans::LapackChar, diag::LapackChar,
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
                  &uplo, &trans, &diag, &n, &nrhs, A, &lda, B, &ldb, info)
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
        if istriu(A) return _jl_lapack_trtrs('U', 'N', 'N', Acopy, X) end
        if istril(A) return _jl_lapack_trtrs('L', 'N', 'N', Acopy, X) end
                                        # Check for SPD matrix
        if ishermitian(Acopy) && all([ Acopy[i,i] > 0 for i=1:n ]) 
            try
                _jl_lapack_posv('U', Acopy, X)
                return X
            catch e
                if !isa(e, LapackException) throw(e) end
                Acopy[:] = A
                X[:] = B
            end
        end
        return _jl_lapack_gesv(Acopy, X)[3]
    end
    _jl_lapack_gels('N', Acopy, X)[2]
end

(\){T1<:Real, T2<:Real}(A::StridedMatrix{T1}, B::StridedVecOrMat{T2}) = (\)(float64(A), float64(B))

# TODO: use *gels transpose argument
(/)(A::StridedVecOrMat, B::StridedVecOrMat) = (B' \ A')'

## Balancing and back-transforming
for (gebal, gebak, elty) in
    (("dgebal_","dgebak_",:Float64),
     ("sgebal_","sgebak_",:Float32),
     ("zgebal_","zgebak_",:Complex128),
     ("cgebal_","cgebak_",:Complex64))
    
    @eval begin
        #     SUBROUTINE DGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOB
        #      INTEGER            IHI, ILP, INFO, LDA, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), SCALE( * )
        function _jl_lapack_gebal!(job::LapackChar, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_gebal!: matrix columns must have contiguous elements")
            end
            m, n    = size(A)
            if m != n error("_jl_lapack_gebal!: Matrix A must be square") end
            lda     = stride(A, 2)
            info    = Array(Int32, 1)
            ihi     = Array(Int32, 1)
            ilo     = Array(Int32, 1)
            scale   = Array($elty, n)
            ccall(dlsym(Base._jl_liblapack, $gebal),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &job, &n, A, &lda, ilo, ihi, scale, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            ilo[1], ihi[1], scale
        end
        #     SUBROUTINE DGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOB, SIDE
        #      INTEGER            IHI, ILP, INFO, LDV, M, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   SCALE( * ), V( LDV, * )
        function _jl_lapack_gebak!(job::LapackChar, side::LapackChar,
                        ilo::Int32, ihi::Int32, scale::Vector{$elty},
                        V::StridedMatrix{$elty})
            if stride(V,1) != 1
                error("_jl_lapack_gebak!: matrix columns must have contiguous elements")
            end
            m, n    = size(V)
            if m != n error("_jl_lapack_gebal!: Matrix V must be square") end
            ldv     = stride(V, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(Base._jl_liblapack, $gebak),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &job, &side, &m, &ilo, &ihi, scale, &n, V, &ldv, info)
            if info[1] != 0 throw(Base.LapackException(info[1])) end
            V
        end
    end
end

## Destructive matrix exponential using algorithm from Higham, 2008,
## "Functions of Matrices: Theory and Computation", SIAM
function expm!{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T})
    m, n = size(A)
    if m != n error("expm!: Matrix A must be square") end
    if m < 2 return exp(A) end
    ilo, ihi, scale = _jl_lapack_gebal!('B', A)    # modifies A
    nA   = norm(A, 1)
    I    = convert(Array{T,2}, eye(n))
    ## For sufficiently small nA, use lower order Padé-Approximations
    if (nA <= 2.1)
        if nA > 0.95
            C = [17643225600.,8821612800.,2075673600.,302702400.,
                    30270240.,   2162160.,    110880.,     3960.,
                          90.,         1.]
        elseif nA > 0.25
            C = [17297280.,8648640.,1995840.,277200.,
                    25200.,   1512.,     56.,     1.]
        elseif nA > 0.015
            C = [30240.,15120.,3360.,
                   420.,   30.,   1.]
        else
            C = [120.,60.,12.,1.]
        end
        A2 = A * A
        P  = copy(I)
        U  = C[2] * P
        V  = C[1] * P
        for k in 1:(div(size(C, 1), 2) - 1)
            k2 = 2 * k
            P *= A2
            U += C[k2 + 2] * P
            V += C[k2 + 1] * P
        end
        U  = A * U
        X  = (V - U)\(V + U)
    else
        s  = log2(nA/5.4)               # power of 2 later reversed by squaring
        if s > 0
            si = iceil(s)
            A /= 2^si
        end
        CC = [64764752532480000.,32382376266240000.,7771770303897600.,
               1187353796428800.,  129060195264000.,  10559470521600.,
                   670442572800.,      33522128640.,      1323241920.,
                       40840800.,           960960.,           16380.,
                            182.,                1.]
        A2 = A * A
        A4 = A2 * A2
        A6 = A2 * A4
        U  = A * (A6 * (CC[14]*A6 + CC[12]*A4 + CC[10]*A2) +
                  CC[8]*A6 + CC[6]*A4 + CC[4]*A2 + CC[2]*I)
        V  = A6 * (CC[13]*A6 + CC[11]*A4 + CC[9]*A2) +
                  CC[7]*A6 + CC[5]*A4 + CC[3]*A2 + CC[1]*I
        X  = (V-U)\(V+U)
                         
        if s > 0            # squaring to reverse dividing by power of 2
            for t in 1:si X *= X end
        end
    end
                                        # Undo the balancing
    doscale = false                     # check if rescaling is needed
    for i = ilo:ihi
        if scale[i] != 1.
            doscale = true
            break
        end
    end
    if doscale
        for j = ilo:ihi
            scj = scale[j]
            if scj != 1.                # is this overkill?
                for i = ilo:ihi
                    X[i,j] *= scale[i]/scj
                end
            else
                for i = ilo:ihi
                    X[i,j] *= scale[i]
                end
            end
        end
    end
    if ilo > 1       # apply lower permutations in reverse order
        for j in (ilo-1):1:-1 rcswap!(j, int(scale[j]), X) end
    end
    if ihi < n       # apply upper permutations in forward order
        for j in (ihi+1):n    rcswap!(j, int(scale[j]), X) end
    end
    X
end

## Swap rows j and jp and columns j and jp in X
function rcswap!{T<:Number}(j::Int, jp::Int, X::StridedMatrix{T})
    for k in 1:size(X, 2)
        tmp     = X[k,j]
        X[k,j]  = X[k,jp]
        X[k,jp] = tmp
        tmp     = X[j,k]
        X[j,k]  = X[jp,k]
        X[jp,k] = tmp
    end
end

# Matrix exponential
expm{T<:Union(Float32,Float64,Complex64,Complex128)}(A::StridedMatrix{T}) = expm!(copy(A))
expm{T<:Integer}(A::StridedMatrix{T}) = expm!(float(A))


#### Tridiagonal matrix routines ####
function \{T<:LapackScalar}(M::Tridiagonal{T}, rhs::StridedVecOrMat{T})
    if stride(rhs, 1) == 1
        x = copy(rhs)
        Mc = copy(M)
        Mlu, x = _jl_lapack_gtsv(Mc, x)
        return x
    end
    solve(M, rhs)  # use the Julia "fallback"
end

eig(M::Tridiagonal) = _jl_lapack_stev('V', copy(M))

# Decompositions
for (gttrf, pttrf, elty) in
    ((:dgttrf_,:dpttrf_,:Float64),
     (:sgttrf_,:spttrf_,:Float32),
     (:zgttrf_,:zpttrf_,:Complex128),
     (:cgttrf_,:cpttrf_,:Complex64))
    @eval begin
        function _jl_lapack_gttrf(M::Tridiagonal{$elty})
            info = Array(Int32, 1)
            n    = int32(length(M.d))
            ipiv = Array(Int32, n)
            ccall(dlsym(_jl_liblapack, $string(gttrf)),
                  Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, M.dl, M.d, M.du, M.dutmp, ipiv, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            M, ipiv
        end
        function _jl_lapack_pttrf(D::Vector{$elty}, E::Vector{$elty})
            info = Array(Int32, 1)
            n    = int32(length(D))
            if length(E) != n-1
                error("subdiagonal must be one element shorter than diagonal")
            end
            ccall(dlsym(_jl_liblapack, $string(pttrf)),
                  Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                  &n, D, E, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            D, E
        end
    end
end
# Direct solvers
for (gtsv, ptsv, elty) in
    ((:dgtsv_,:dptsv_,:Float64),
     (:sgtsv_,:sptsv,:Float32),
     (:zgtsv_,:zptsv,:Complex128),
     (:cgtsv_,:cptsv,:Complex64))
    @eval begin
        function _jl_lapack_gtsv(M::Tridiagonal{$elty}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_gtsv: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            n    = int32(length(M.d))
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(gtsv)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, M.dl, M.d, M.du, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            M, B
        end
        function _jl_lapack_ptsv(M::Tridiagonal{$elty}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_ptsv: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            n    = int32(length(M.d))
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(ptsv)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, M.d, M.dl, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            M, B
        end
    end
end
# Solvers using decompositions
for (gttrs, pttrs, elty) in
    ((:dgttrs_,:dpttrs_,:Float64),
     (:sgttrs_,:spttrs,:Float32),
     (:zgttrs_,:zpttrs,:Complex128),
     (:cgttrs_,:cpttrs,:Complex64))
    @eval begin
        function _jl_lapack_gttrs(trans::LapackChar, M::Tridiagonal{$elty}, ipiv::Vector{Int32}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_gttrs: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            n    = int32(length(M.d))
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(gttrs)),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &trans, &n, &nrhs, M.dl, M.d, M.du, M.dutmp, ipiv, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
        function _jl_lapack_pttrs(D::Vector{$elty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            if stride(B,1) != 1
                error("_jl_lapack_pttrs: matrix columns must have contiguous elements");
            end
            info = Array(Int32, 1)
            n    = int32(length(D))
            if length(E) != n-1
                error("subdiagonal must be one element shorter than diagonal")
            end
            nrhs = int32(size(B, 2))
            ldb  = int32(stride(B, 2))
            ccall(dlsym(_jl_liblapack, $string(pttrs)),
                  Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, &nrhs, D, E, B, &ldb, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end
# Eigenvalue-eigenvector (symmetric only)
for (stev, elty) in
    ((:dstev_,:Float64),
     (:sstev_,:Float32),
     (:zstev_,:Complex128),
     (:cstev_,:Complex64))
    @eval begin
        function _jl_lapack_stev(Z::Array, M::Tridiagonal{$elty})
            n    = int32(length(M.d))
            if isempty(Z)
                job = 'N'
                ldz = 1
                work = Array($elty, 0)
                Ztmp = work
            else
                if stride(Z,1) != 1
                    error("_jl_lapack_stev: eigenvector matrix columns must have contiguous elements");
                end
                if size(Z, 1) != n
                    error("_jl_lapack_stev: eigenvector matrix columns are not of the correct size")
                end
                Ztmp = Z
                job = 'V'
                ldz  = int32(stride(Z, 2))
                work = Array($elty, max(1, 2*n-2))
            end
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $string(stev)),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32},
                   Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &job, &n, M.d, M.dl, Ztmp, &ldz, work, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            M.d
        end
    end
end
function _jl_lapack_stev(job::LapackChar, M::Tridiagonal)
    if job == 'N' || job == 'n'
        Z = []
    elseif job == 'V' || job == 'v'
        n = length(M.d)
        Z = Array(eltype(M), n, n)
    else
        error("Job type not recognized")
    end
    D = _jl_lapack_stev(Z, M)
    return D, Z
end
