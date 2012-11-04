## The Lapack module of interfaces to Lapack subroutines
module Lapack
import Base.*

typealias LapackChar Char
type LapackException <: Exception
    info::Int32
end
type LapackDimMisMatch <: Exception
    name::ASCIIString
end

function chkstride1(A::StridedVecOrMat...)
    for a in A
        if stride(a,1) != 1 error("Lapack: Matrix must have contiguous columns") end
    end
end

function chksquare(A::Matrix...)
    for a in A
        m, n = size(a)
        if m != n error("Lapack: Matrix must be square") end
    end
end

# (GB) general banded matrices, LU decomposition and solver
for (gbtrf, gbtrs, elty) in
    ((:dgbtrf_,:dgbtrs_,:Float64),
     (:sgbtrf_,:sgbtrs_,:Float32),
     (:zgbtrf_,:zgbtrs_,:Complex128),
     (:cgbtrf_,:cgbtrs_,:Complex64))
    @eval begin
        # SUBROUTINE DGBTRF( M, N, KL, KU, AB, LDAB, IPIV, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, KL, KU, LDAB, M, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   AB( LDAB, * )
        function gbtrf!(kl::Integer, ku::Integer, m::Integer, AB::StridedMatrix{$elty})
            chkstride1(AB)
            info = Array(Int32, 1)
            n    = size(AB, 2)
            mnmn = min(m, n)
            ipiv = Array(Int32, mnmn)
            ccall(dlsym(Base.liblapack, $(string(gbtrf))), Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, &kl, &ku, AB, &stride(AB,2), ipiv, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            AB, ipiv
        end
        # SUBROUTINE DGBTRS( TRANS, N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO)
        # *     .. Scalar Arguments ..
        #       CHARACTER          TRANS
        #       INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   AB( LDAB, * ), B( LDB, * )
        function gbtrs!(trans::LapackChar, kl::Integer, ku::Integer, m::Integer,
                        AB::StridedMatrix{$elty}, ipiv::Vector{Int32},
                        B::StridedVecOrMat{$elty})
            chkstride1(AB, B)
            info = Array(Int32, 1)
            n    = size(AB,2)
            if m != n || m != size(B,1) throw(LapackDimMisMatch("gbtrs!")) end
            ccall(dlsym(Base.liblapack, $(string(gbtrs))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  &trans, &n, &kl, &ku, &size(B,2), AB, &stride(AB,2), ipiv,
                  B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end

## (GE) general matrices: balancing and back-transforming
for (gebal, gebak, elty) in
    ((:dgebal_, :dgebak_, :Float64),
     (:sgebal_, :sgebak_, :Float32),
     (:zgebal_, :zgebak_, :Complex128),
     (:cgebal_, :cgebak_, :Complex64))
    @eval begin
        #     SUBROUTINE DGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOB
        #      INTEGER            IHI, ILP, INFO, LDA, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), SCALE( * )
        function gebal!(job::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            n       = size(A, 2)
            info    = Array(Int32, 1)
            ihi     = Array(Int32, 1)
            ilo     = Array(Int32, 1)
            scale   = Array($elty, n)
            ccall(dlsym(Base.liblapack, $(string(gebal))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &job, &n, A, &stride(A,2), ilo, ihi, scale, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            ilo[1], ihi[1], scale
        end
        #     SUBROUTINE DGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOB, SIDE
        #      INTEGER            IHI, ILP, INFO, LDV, M, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   SCALE( * ), V( LDV, * )
        function gebak!(job::LapackChar, side::LapackChar,
                        ilo::Int32, ihi::Int32, scale::Vector{$elty},
                        V::StridedMatrix{$elty})
            chkstride1(V)
            chksquare(V)
            info    = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(gebak))), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &job, &side, &size(V,1), &ilo, &ihi, scale, &n, V, &stride(V,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            V
        end
    end
end

# (GE) general matrices, direct decompositions
# gebrd - reduction to bidiagonal form Q'*A*P=B where Q and P are orthogonal
# gelqf - unpivoted LQ decomposition
# geqlf - unpivoted QL decomposition
# geqrf - unpivoted QR decomposition
# gegp3 - pivoted QR decomposition
# gerqf - unpivoted RQ decomposition
# getrf - LU decomposition
for (gebrd, gelqf, geqlf, geqrf, geqp3, gerqf, getrf, elty) in
    ((:dgebrd_,:dgelqf_,:dgeqlf_,:dgeqrf_,:dgeqp3_,:dgerqf_,:dgetrf_,:Float64),
     (:sgebrd_,:sgelqf_,:sgeqlf_,:sgeqrf_,:sgeqp3_,:sgerqf_,:sgetrf_,:Float32),
     (:zgebrd_,:zgelqf_,:zgeqlf_,:zgeqrf_,:zgeqp3_,:zgerqf_,:zgetrf_,:Complex128),
     (:cgebrd_,:cgelqf_,:cgeqlf_,:cgeqrf_,:cgeqp3_,:cgerqf_,:cgetrf_,:Complex64))
    @eval begin
        # SUBROUTINE DGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,
        #                    INFO )
        # .. Scalar Arguments ..
        # INTEGER            INFO, LDA, LWORK, M, N
        # .. Array Arguments ..
        #  DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
        #           TAUQ( * ), WORK( * )
        function gebrd!(A::StridedMatrix{$elty})
            chkstride1(A)
            m, n  = size(A)
            k     = min(m, n)
            d     = Array($elty, k)
            s     = Array($elty, k)
            tauq  = Array($elty, k)
            taup  = Array($elty, k)
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(gebrd))), Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                       Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &m, &n, AA, &stride(A,2), d, s, tauq, taup, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
        end
        # SUBROUTINE DGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function gelqf!(A::StridedMatrix{$elty})
            chkstride1(A)
            info  = Array(Int32, 1)
            m     = int32(size(A, 1))
            n     = int32(size(A, 2))
            lda   = int32(stride(A, 2))
            tau   = Array($elty, n)
            lwork = int32(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(dlsym(Base.liblapack, $(string(gelqf))), Void,
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
        # SUBROUTINE DGEQLF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function geqlf!(A::StridedMatrix{$elty})
            chkstride1(A)
            info  = Array(Int32, 1)
            m     = int32(size(A, 1))
            n     = int32(size(A, 2))
            lda   = int32(stride(A, 2))
            tau   = Array($elty, n)
            lwork = int32(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(dlsym(Base.liblapack, $(string(geqlf))), Void,
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
        function geqp3!(A::StridedMatrix{$elty})
            chkstride1(A)
            m, n  = size(A)
            jpvt  = zeros(Int32, n)
            tau   = Array($elty, n)
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            Rtyp  = typeof(real(A[1]))
            cmplx = iscomplex(A)
            if cmplx rwork = Array(Rtyp, 2n) end
            for i in 1:2
                if cmplx
                    ccall(dlsym(Base.liblapack, $(string(geqp3))), Void,
                          (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Rtyp}, Ptr{Int32}),
                          &m, &n, A, &stride(A,2), jpvt, tau, work, &lwork, rwork, info)
                else
                    ccall(dlsym(Base.liblapack, $(string(geqp3))), Void,
                          (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}),
                          &m, &n, A, &stride(A,2), jpvt, tau, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            A, tau, jpvt
        end
        ## Several variants of geqrf! could be defined.
        ## geqrfp! - positive elements on diagonal of R
        ## geqrt!  - compact WY representation of Q (blocked algorithm)
        ## geqrt3! - recursive algorithm producing compact WY representation of Q
        # SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function geqrf!(A::StridedMatrix{$elty})
            chkstride1(A)
            m, n  = size(A)
            tau   = Array($elty, n)
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(dlsym(Base.liblapack, $(string(geqrf))), Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &m, &n, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, tau
        end
        # SUBROUTINE DGERQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function gerqf!(A::StridedMatrix{$elty})
            chkstride1(A)
            info  = Array(Int32, 1)
            m, n  = size(A)
            tau   = Array($elty, n)
            lwork = int32(-1)
            work  = Array($elty, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(dlsym(Base.liblapack, $(string(gerqf))), Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &m, &n, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, tau
        end
        # SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, M, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * )
        function getrf!(A::StridedMatrix{$elty})
            chkstride1(A)
            info = Array(Int32, 1)
            m, n = size(A)
            lda  = stride(A, 2)
            ipiv = Array(Int32, min(m,n))
            ccall(dlsym(Base.liblapack, $(string(getrf))), Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                  &m, &n, A, &lda, ipiv, info)
            if info[1] < 0 throw(LapackException(info[1])) end
            A, ipiv, info[1]
        end
    end
end

## (GE) general matrices, solvers with factorization, solver and inverse
for (gels, gesv, getrs, getri, elty) in
    ((:dgels_,:dgesv_,:dgetrs_,:dgetri_,:Float64),
     (:sgels_,:sgesv_,:sgetrs_,:sgetri_,:Float32),
     (:zgels_,:zgesv_,:zgetrs_,:zgetri_,:Complex128),
     (:cgels_,:cgesv_,:cgetrs_,:cgetri_,:Complex64))
    @eval begin
        #      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,INFO)
        # *     .. Scalar Arguments ..
        #       CHARACTER          TRANS
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
        function gels!(trans::LapackChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            btrn  = trans == 'T'
            m, n  = size(A)
            if size(B,1) != (btrn ? n : m)  throw(LapackDimMisMatch("gels!")) end
            info  = Array(Int32, 1)
            work  = Array($elty, 1)
            lwork = int32(-1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(gels))), Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &(btrn?'T':'N'), &m, &n, &size(B,2), A, &stride(A,2),
                      B, &stride(B,2), work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            k   = min(m, n)
            F   = m < n ? tril(A[1:k, 1:k]) : triu(A[1:k, 1:k])
            F, isa(B, Vector) ? B[1:k] : B[1:k,:], [sum(B[(k+1):size(B,1), i].^2) for i=1:size(B,2)]
        end
        # SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        # *     ..
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function gesv!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            chksquare(A)
            n     = size(A,1)
            if size(B,1) != n throw(LapackDimMisMatch("gesv!")) end
            ipiv    = Array(Int32, n)
            info    = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(gesv))), Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &n, &size(B,2), A, &stride(A,2), ipiv, B, &stride(B,2), info)
            if info[1] < 0 throw(LapackException(info[1])) end
            B, A, ipiv, info[1]
        end
        #     SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          TRANS
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      INTEGER            IPIV( * )
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function getrs!(trans::LapackChar, A::StridedMatrix{$elty}, ipiv::Vector{Int32}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            m, n    = size(A)
            if m != n || size(B, 1) != m error("getrs!: dimension mismatch") end
            nrhs    = size(B, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(getrs))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &trans, &n, &size(B,2), A, &stride(A,2), ipiv, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
        #     SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
        #*     .. Scalar Arguments ..
        #      INTEGER            INFO, LDA, LWORK, N
        #*     .. Array Arguments ..
        #      INTEGER            IPIV( * )
        #      DOUBLE PRECISION   A( LDA, * ), WORK( * )
        function getri!(A::StridedMatrix{$elty}, ipiv::Vector{Int32})
            chkstride1(A)
            m, n    = size(A)
            if m != n || n != numel(ipiv) error("getri!: dimension mismatch") end
            lda     = stride(A, 2)
            info    = Array(Int32, 1)
            lwork   = -1
            work    = Array($elty, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(getri))), Void,
                      (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &n, A, &lda, ipiv, work, &lwork, info)
                if info[1] != 0 error("getri!: error $(info[1])") end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            A
        end
    end
end
for (gelsd, elty) in ((:dgelsd_, Float64),
                      (:sgelsd_, Float32))
    @eval begin
        # SUBROUTINE DGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
        #      $                   WORK, LWORK, IWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
        #       DOUBLE PRECISION   RCOND
        # *     ..
        # *     .. Array Arguments ..
        #       INTEGER            IWORK( * )
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
        function gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, rcond)
            Lapack.chkstride1(A, B)
            m, n  = size(A)
            if size(B,1) != m; throw(Lapack.LapackDimMisMatch("gelsd!")); end
            s     = Array($elty, min(m, n))
            rnk   = Array(Int32, 1)
            info  = Array(Int32, 1)
            work  = Array($elty, 1)
            lwork = int32(-1)
            iwork = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(gelsd))), Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                       Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
                      &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      B, &max(1,stride(B,2)), s, &rcond, rnk, work, &lwork, iwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                    iwork = Array(Int32, iwork[1])
                end
            end
            isa(B, Vector) ? B[1:n] : B[1:n,:], rnk[1]
        end
        gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}) = gelsd!(A, B, -1.)
    end
end
for (gelsd, elty, relty) in ((:zgelsd_, Complex128, Float64),
                             (:cgelsd_, Complex64, Float32))
    @eval begin
        # SUBROUTINE ZGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
        #      $                   WORK, LWORK, RWORK, IWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
        #       DOUBLE PRECISION   RCOND
        # *     ..
        # *     .. Array Arguments ..
        #       INTEGER            IWORK( * )
        #       DOUBLE PRECISION   RWORK( * ), S( * )
        #       COMPLEX*16         A( LDA, * ), B( LDB, * ), WORK( * )
        function gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, rcond)
            Lapack.chkstride1(A, B)
            m, n  = size(A)
            if size(B,1) != m; throw(Lapack.LapackDimMisMatch("gelsd!")); end
            s     = Array($elty, min(m, n))
            rnk   = Array(Int32, 1)
            info  = Array(Int32, 1)
            work  = Array($elty, 1)
            lwork = int32(-1)
            rwork = Array($relty, 1)
            iwork = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(gelsd))), Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                       Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$relty},
                       Ptr{$relty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$relty}, Ptr{Int32}, Ptr{Int32}),
                      &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      B, &max(1,stride(B,2)), s, &rcond, rnk, work, &lwork, rwork, iwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                    rwork = Array($relty, int(rwork[1]))
                    iwork = Array(Int32, iwork[1])
                end
            end
            isa(B, Vector) ? B[1:n] : B[1:n,:], rnk[1]
        end
        gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}) = gelsd!(A, B, -1.)
    end
end

# (GE) general matrices eigenvalue-eigenvector and singular value decompositions
for (geev, gesvd, gesdd, elty) in
    ((:dgeev_,:dgesvd_,:dgesdd_,:Float64),
     (:sgeev_,:sgesvd_,:sgesdd_,:Float32),
     (:zgeev_,:zgesvd_,:zgesdd_,:Complex128),
     (:cgeev_,:cgesvd_,:cgesdd_,:Complex64))
    @eval begin
        #      SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
        #      $                  LDVR, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBVL, JOBVR
        #       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
        #      $                   WI( * ), WORK( * ), WR( * )
        function geev!(jobvl::LapackChar, jobvr::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            m, n  = size(A)
            lvecs = jobvl == 'V'
            rvecs = jobvr == 'V'
            VL    = Array($elty, (n, lvecs ? n : 0))
            VR    = Array($elty, (n, rvecs ? n : 0))
            Rtyp  = typeof(real(A[1]))
            cmplx = iscomplex(A)
            if cmplx
                W     = Array($elty, n)
                rwork = Array(Rtyp, 2n)
            else
                WR    = Array($elty, n)
                WI    = Array($elty, n)
            end
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i = 1:2
                if cmplx
                    ccall(dlsym(Base.liblapack, $(string(geev))), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, 
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Rtyp}, Ptr{Int32}),
                          &jobvl, &jobvr, &n, A, &stride(A,2), W, VL, &n, VR, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(dlsym(Base.liblapack, $(string(geev))), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Int32}),
                          &jobvl, &jobvr, &n, A, &stride(A,2), WR, WI, VL, &n,
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
        function gesdd!(job::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            m, n   = size(A)
            minmn  = min(m, n)
            if job == 'A'
                U  = Array($elty, (m, m))
                VT = Array($elty, (n, n))
            elseif job == 'S'
                U  = Array($elty, (m, minmn))
                VT = Array($elty, (minmn, n))
            elseif job == 'O'
                U  = Array($elty, (m, m >= n ? 0 : m))
                VT = Array($elty, (n, m >= n ? n : 0))
            else
                U  = Array($elty, (m, 0))
                VT = Array($elty, (n, 0))
            end
            work   = Array($elty, 1)
            lwork  = int32(-1)
            Rtyp   = typeof(real(A[1]))
            S      = Array(Rtyp, minmn)
            cmplx  = iscomplex(A)
            if cmplx
                rwork = Array(Rtyp, job == 'N' ? 5*minmn : minmn*max(5*minmn+7,2*max(m,n)+2*minmn+1))
            end
            iwork  = Array(Int32, 8*minmn)
            info   = Array(Int32, 1)
            for i = 1:2
                if cmplx
                    ccall(dlsym(Base.liblapack, $(string(gesdd))), Void,
                          (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Rtyp}, Ptr{Int32}, Ptr{Int32}),
                          &job, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, iwork, info)
                else
                    ccall(dlsym(Base.liblapack, $(string(gesdd))), Void,
                          (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                           Ptr{Int32}, Ptr{Int32}),
                          &job, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, iwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            if job == 'O' 
                if m >= n return (A, S, VT) 
                else return (U, S, A) 
                end 
            end
            return (U, S, VT)
        end
        # SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBU, JOBVT
        #       INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
        #      $                   VT( LDVT, * ), WORK( * )
        function gesvd!(jobu::LapackChar, jobvt::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            Rtyp   = typeof(real(A[1]))
            m, n   = size(A)
            minmn  = min(m, n)
            S      = Array(Rtyp, minmn)
            U      = Array($elty, jobu  == 'A'? (m, m):(jobu  == 'S'? (m, minmn) : (m, 0)))
            VT     = Array($elty, jobvt == 'A'? (n, n):(jobvt == 'S'? (minmn, n) : (n, 0)))
            work   = Array($elty, 1)
            cmplx  = iscomplex(A)
            if cmplx rwork = Array(Rtyp, 5minmn) end
            lwork  = int32(-1)
            info   = Array(Int32, 1)
            for i in 1:2
                if cmplx
                    ccall(dlsym(Base.liblapack, $(string(gesvd))), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Rtyp}, Ptr{Int32}),
                          &jobu, &jobvt, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, info)
                else
                    ccall(dlsym(Base.liblapack, $(string(gesvd))), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                           Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty},
                           Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty},
                           Ptr{Int32}, Ptr{Int32}),
                          &jobu, &jobvt, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
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

# (GT) General tridiagonal, decomposition, solver and direct solver
for (gtsv, gttrf, gttrs, elty) in
    ((:dgtsv_,:dgttrf_,:dgttrs_,:Float64),
     (:sgtsv_,:sgttrf_,:sgttrs_,:Float32),
     (:zgtsv_,:zgttrf_,:zgttrs_,:Complex128),
     (:cgtsv_,:cgttrf_,:cgttrs_,:Complex64))     
    @eval begin
        #       SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, LDB, N, NRHS
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * )
        function gtsv!(dl::Vector{$elty}, d::Vector{$elty}, du::Vector{$elty},
                       B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(d)
            if length(dl) != n - 1 || length(du) != n - 1
                throw(LapackDimMisMatch("gtsv!"))
            end
            if n != size(B,1) throw(LapackDimMisMatch("gtsv!")) end
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(gtsv))), Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &n, &size(B,2), dl, d, du, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
        #       SUBROUTINE DGTTRF( N, DL, D, DU, DU2, IPIV, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, N
        #       .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   D( * ), DL( * ), DU( * ), DU2( * )
        function gttrf!(dl::Vector{$elty}, d::Vector{$elty}, du::Vector{$elty})
            n    = length(d)
            if length(dl) != (n-1) || length(du) != (n-1)
                throw(LapackDimMisMatch("gttrf!"))
            end
            du2  = Array($elty, n-2)
            ipiv = Array(Int32, n)
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(gttrf))), Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{Int32}),
                  &n, dl, d, du, du2, ipiv, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            dl, d, du, du2, ipiv
        end
        #       SUBROUTINE DGTTRS( TRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB, INFO )
        #       .. Scalar Arguments ..
        #       CHARACTER          TRANS
        #       INTEGER            INFO, LDB, N, NRHS
        #       .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * ), DU2( * )
        function gttrs!(trans::LapackChar, dl::Vector{$elty}, d::Vector{$elty},
                        du::Vector{$elty}, du2::Vector{$elty}, ipiv::Vector{Int32},
                        B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(d)
            if length(dl) != n - 1 || length(du) != n - 1 throw(LapackDimMisMatch("gttrs!")) end
            if n != size(B,1) throw(LapackDimMisMatch("gttrs!")) end
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(gttrs))), Void,
                   (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                    Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                    Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                   &trans, &n, &size(B,2), dl, d, du, du2, ipiv, B, &stride(B,2), info)
             if info[1] != 0 throw(LapackException(info[1])) end
             B
         end
    end
end

## (OR) orthogonal (or UN, unitary) matrices, extractors and multiplication
for (orglq, orgqr, ormlq, ormqr, elty) in
    ((:dorglq_,:dorgqr_,:dormlq_,:dormqr_,:Float64),
     (:sorglq_,:sorgqr_,:sormlq_,:sormqr_,:Float32),
     (:zunglq_,:zungqr_,:zunmlq_,:zunmqr_,:Complex128),
     (:cunglq_,:cungqr_,:cunmlq_,:cunmqr_,:Complex64))
    @eval begin
        # SUBROUTINE DORGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, K, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function orglq!(A::StridedMatrix{$elty}, tau::Vector{$elty}, k::Integer)
            chkstride1(A)
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(orglq))), Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                       Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &size(A,1), &size(A,2), &k, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A
        end
        # SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, K, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function orgqr!(A::StridedMatrix{$elty}, tau::Vector{$elty}, k::Integer)
            chkstride1(A)
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(orgqr))), Void,
                      (Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                       Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &size(A,1), &size(A,2), &k, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A
        end
        #      SUBROUTINE DORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
        #                         WORK, LWORK, INFO )
        #      .. Scalar Arguments ..
        #      CHARACTER          SIDE, TRANS
        #      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
        #      .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
        function ormlq!(side::LapackChar, trans::LapackChar, A::StridedMatrix{$elty},
                        k::Integer, tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            chkstride1(A, C)
            m     = size(C, 1)
            n     = size(C, 2) # m, n = size(C) won't work if C is a Vector
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(ormlq))), Void,
                      (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &side, &trans, &m, &n, &k, A, &stride(A,2), tau,
                      C, &stride(C,2), work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            C
        end
        #      SUBROUTINE DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
        #                         WORK, INFO )
        #      .. Scalar Arguments ..
        #      CHARACTER          SIDE, TRANS
        #      INTEGER            INFO, K, LDA, LDC, M, N
        #      .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
        function ormqr!(side::LapackChar, trans::LapackChar, A::StridedMatrix{$elty},
                        k::Integer, tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            chkstride1(A, C)
            m     = size(C, 1)
            n     = size(C, 2) # m, n = size(C) won't work if C is a Vector
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(ormqr))), Void,
                      (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &side, &trans, &m, &n, &k, A, &stride(A,2), tau,
                      C, &stride(C,2), work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            C
        end
    end
end

# (PO) positive-definite symmetric matrices,
# Cholesky decomposition, solvers (direct and factored) and inverse.
for (posv, potrf, potri, potrs, pstrf, elty, rtyp) in
    ((:dposv_,:dpotrf_,:dpotri_,:dpotrs_,:dpstrf_,:Float64,:Float64),
     (:sposv_,:spotrf_,:spotri_,:spotrs_,:spstrf_,:Float32,:Float32),
     (:zposv_,:zpotrf_,:zpotri_,:zpotrs_,:zpstrf_,:Complex128,:Float64),
     (:cposv_,:cpotrf_,:cpotri_,:cpotrs_,:cpstrf_,:Complex64,:Float32))
    @eval begin
        ## Caller should check if returned info[1] is zero,
        ## positive values indicate indefiniteness
        #     SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function posv!(uplo::LapackChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            chksquare(A)
            n     = size(A,1)
            if size(B,1) != n throw(LapackDimMisMatch("posv!")) end
            info    = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(posv))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &uplo, &n, &size(B,2), A, &stride(A,2), B, &stride(B,2), info)
            if info[1] < 0 throw(LapackException(info[1])) end
            A, B, info[1]
        end
        ## Caller should check if returned info[1] is zero,
        ## positive values indicate indefiniteness
        # SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function potrf!(uplo::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(potrf))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &uplo, &size(A,1), A, &stride(A,2), info)
            if info[1] < 0 throw(LapackException(info[1])) end
            A, info[1]
        end
        ## Caller should check if returned info[1] is zero,
        ## positive values indicate singularity
        #       SUBROUTINE DPOTRI( UPLO, N, A, LDA, INFO )
        #       .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function potri!(uplo::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(potri))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &uplo, &size(A,1), A, &stride(A,2), info)
            if info[1] < 0 throw(LapackException(info[1])) end
            A, info[1]
        end
        #     SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function potrs!(uplo::LapackChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            chksquare(A)
            n    =  size(A,2)
            if size(B,1) != n error("potrs!: dimension mismatch") end
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(potrs))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &uplo, &n, &size(B,2), A, &stride(A,2), B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
        #       SUBROUTINE DPSTRF( UPLO, N, A, LDA, PIV, RANK, TOL, WORK, INFO )
        #       .. Scalar Arguments ..
        #       DOUBLE PRECISION   TOL
        #       INTEGER            INFO, LDA, N, RANK
        #       CHARACTER          UPLO
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), WORK( 2*N )
        #       INTEGER            PIV( N )
        function pstrf!(uplo::LapackChar, A::StridedMatrix{$elty}, tol::Real)
            chkstride1(A)
            chksquare(A)
            n    = size(A,1)
            piv  = Array(Int32, n)
            rank = Array(Int32, 1)
            work = Array($rtyp, 2n)
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(pstrf))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$rtyp}, Ptr{$rtyp}, Ptr{Int32}),
                  &uplo, &n, A, &stride(A,2), piv, rank, &tol, work, info)
            if info[1] < 0 throw(LapackException(info[1])) end
            A, piv, rank[1], info[1]
        end
    end
end

## (PT) positive-definite, symmetric, tri-diagonal matrices
## Direct solvers for general tridiagonal and symmetric positive-definite tridiagonal
for (ptsv, pttrf, pttrs, elty) in
    ((:dptsv_,:dpttrf_,:dpttrs_,:Float64),
     (:sptsv_,:spttrf_,:spttrs_,:Float32)
#     , (:zptsv_,:zpttrf_,:zpttrs_,:Complex128)  # need to fix calling sequence.
#     , (:cptsv_,:cpttrf_,:cpttrs_,:Complex64)   # D is real, not complex
     )
    @eval begin
        #       SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, LDB, N, NRHS
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   B( LDB, * ), D( * ), E( * )
        function ptsv!(D::Vector{$elty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(D)
            if length(E) != n - 1 || n != size(B,1) throw(LapackDimMismatch("ptsv!")) end
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(ptsv))), Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &n, &size(B,2), D, E, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
        #       SUBROUTINE DPTTRF( N, D, E, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, N
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   D( * ), E( * )
        function pttrf!(D::Vector{$elty}, E::Vector{$elty})
            n    = length(D)
            if length(E) != (n-1) throw(LapackDimMisMatch("pttrf!")) end
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(pttrf))), Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                  &n, D, E, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            D, E
        end
        #       SUBROUTINE DPTTRS( N, NRHS, D, E, B, LDB, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, LDB, N, NRHS
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   B( LDB, * ), D( * ), E( * )
        function pttrs!(D::Vector{$elty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(D)
            if length(E) != (n-1) || size(B,1) != n throw(LapackDimMisMatch("pttrs!")) end
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(pttrs))), Void,
                  (Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &n, &size(B,2), D, E, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end

## (TR) triangular matrices: solver and inverse
for (trtri, trtrs, elty) in
    ((:dtrtri_,:dtrtrs_,:Float64),
     (:strtri_,:strtrs_,Float32),
     (:ztrtri_,:ztrtrs_,:Complex128),
     (:ctrtri_,:ctrtrs_,:Complex64))
    @eval begin
        #     SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          DIAG, UPLO
        #      INTEGER            INFO, LDA, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * )
        function trtri!(uplo::LapackChar, diag::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            m, n    = size(A)
            if m != n error("trtri!: dimension mismatch") end
            lda     = stride(A, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $trtri), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}),
                  &uplo, &diag, &n, A, &lda, info)
            if info[1] < 0 error("trtri!: error $(info[1])") end
            A, info[1]
        end
        #      SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          DIAG, TRANS, UPLO
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function trtrs!(uplo::LapackChar, trans::LapackChar, diag::LapackChar,
                        A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A)
            chksquare(A)
            n    = size(A,2)
            if size(B,1) != n throw(LapackDimMisMatch("trtrs!")) end
            info    = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(trtrs))), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &uplo, &trans, &diag, &n, &size(B,2), A, &stride(A,2),
                  B, &stride(B,2), info)
            if info[1] < 0 throw(LapackException(info[1])) end
            B, info[1]
        end
    end
end

## (ST) Symmetric tridiagonal - eigendecomposition
for (stev, elty) in
    ((:dstev_,:Float64),
     (:sstev_,:Float32)
#     , (:zstev_,:Complex128)  Need to rewrite for ZHEEV, rwork, etc.
#     , (:cstev_,:Complex64)
     )
    @eval begin
        function stev!(job::LapackChar, dv::Vector{$elty}, ev::Vector{$elty})
            n    = length(dv)
            if length(ev) != (n-1) throw(LapackDimMisMatch("stev!")) end
            Zmat = Array($elty, (n, job != 'N' ? n : 0))
            work = Array($elty, max(1, 2n-2))
            info = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(stev))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &job, &n, dv, ev, Zmat, &n, work, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            dv, Zmat
        end
    end
end

## (SY) symmetric matrices - eigendecomposition, Bunch-Kaufman decomposition,
## solvers (direct and factored) and inverse.
for (syconv, syev, sysv, sytrf, sytri, sytrs, elty) in
    ((:dsyconv_,:dsyev_,:dsysv_,:dsytrf_,:dsytri_,:dsytrs_,:Float64),
     (:ssyconv_,:ssyev_,:ssysv_,:ssytrf_,:ssytri_,:ssytrs_,:Float32),
     (:zheconv_,:zheev_,:zhesv_,:zhetrf_,:zhetri_,:zhetrs_,:Complex128),
     (:checonv_,:cheev_,:chesv_,:chetrf_,:chetri_,:chetrs_,:Complex64))
    @eval begin
        #       SUBROUTINE DSYCONV( UPLO, WAY, N, A, LDA, IPIV, WORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO, WAY
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), WORK( * )
        function syconv!(uplo::LapackChar, A::StridedMatrix{$elty}, ipiv::Vector{Int32})
            chkstride1(A)
            chksquare(A)
            n     = size(A,1)
            work  = Array($elty, n)
            info  = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(syconv))), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &uplo, &'C', &n, A, &stride(A,2), ipiv, work, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            A, work
        end
        #       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBZ, UPLO
        #       INTEGER            INFO, LDA, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
        function syev!(jobz::LapackChar, uplo::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            cmplx = iscomplex(A)
            Rtyp  = typeof(real(A[1]))
            n     = size(A, 1)
            W     = Array(Rtyp, n)
            work  = Array($elty, 1)
            lwork = int32(-1)
            if cmplx
                rwork = Array(Rtyp, max(1, 2n-1))
            end
            info  = Array(Int32, 1)
            for i in 1:2
                if cmplx
                    ccall(dlsym(Base.liblapack, $(string(syev))), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                          Ptr{Rtyp}, Ptr{$elty}, Ptr{Int32}, Ptr{Rtyp}, Ptr{Int32}),
                          &jobz, &uplo, &n, A, &stride(A,2), W, work, &lwork, rwork, info)
                else
                    ccall(dlsym(Base.liblapack, $(string(syev))), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                          Ptr{Rtyp}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                          &jobz, &uplo, &n, A, &stride(A,2), W, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            jobz == 'V' ? (W, A) : W
        end
        #       SUBROUTINE DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
        #                         LWORK, INFO )
        #       .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, LDB, LWORK, N, NRHS
        #       .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
        function sysv!(uplo::LapackChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            chksquare(A)
            n     = size(A,1)
            if n != size(B,1) throw(LapackDimMismatch("sysv!")) end
            ipiv  = Array(Int32, n)
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(sysv))), Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &uplo, &n, &size(B,2), A, &stride(A,2), ipiv, B, &stride(B,2),
                      work, &lwork, info)
                if info[1] < 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            B, A, ipiv, info[1]
        end
        #       SUBROUTINE DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, LWORK, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), WORK( * )
        function sytrf!(uplo::LapackChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            n     = size(A,1)
            ipiv  = Array(Int32, n)
            work  = Array($elty, 1)
            lwork = int32(-1)
            info  = Array(Int32, 1)
            for i in 1:2
                ccall(dlsym(Base.liblapack, $(string(sytrf))), Void,
                      (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                       Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &uplo, &n, A, &stride(A,2), ipiv, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, ipiv
        end
        #       SUBROUTINE DSYTRI2( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, LWORK, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), WORK( * )
#         function sytri!(uplo::LapackChar, A::StridedMatrix{$elty}, ipiv::Vector{Int32})
#             chkstride1(A)
#             chksquare(A)
#             n     = size(A,1)
#             work  = Array($elty, 1)
#             lwork = int32(-1)
#             info  = Array(Int32, 1)
#             for i in 1:2
#                 ccall(dlsym(Base.liblapack, $(string(sytri))), Void,
#                       (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
#                        Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
#                       &uplo, &n, A, &stride(A,2), ipiv, work, &lwork, info)
#                 if info[1] != 0 throw(LapackException(info[1])) end
#                 if lwork < 0
#                     lwork = int32(real(work[1]))
#                     work = Array($elty, lwork)
#                 end
#             end
#             A
#         end
        #      SUBROUTINE DSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
        #     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, N
        #     .. Array Arguments ..
        #      INTEGER            IPIV( * )
        #      DOUBLE PRECISION   A( LDA, * ), WORK( * )
        function sytri!(uplo::LapackChar, A::StridedMatrix{$elty}, ipiv::Vector{Int32})
            chkstride1(A)
            chksquare(A)
            n     = size(A,1)
            work  = Array($elty, n)
            info  = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(sytri))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &uplo, &n, A, &stride(A,2), ipiv, work, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            A
        end
        #       SUBROUTINE DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
        #                        
        #       .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        #       .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function sytrs!(uplo::LapackChar, A::StridedMatrix{$elty},
                       ipiv::Vector{Int32}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            chksquare(A)
            n     = size(A,1)
            if n != size(B,1) throw(LapackDimMismatch("sytrs!")) end
            info  = Array(Int32, 1)
            ccall(dlsym(Base.liblapack, $(string(sytrs))), Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &uplo, &n, &size(B,2), A, &stride(A,2), ipiv, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end

end # module
