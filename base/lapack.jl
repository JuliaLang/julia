## The LAPACK module of interfaces to LAPACK subroutines
module LAPACK

const liblapack = Base.liblapack_name

import Base.BlasFloat
import Base.BlasChar
import Base.BlasInt
import Base.blas_int

type LapackException <: Exception
    info::BlasInt
end

type SingularException <: Exception
    info::BlasInt
end

type PosDefException <: Exception
    info::BlasInt
end

type RankDeficientException <: Exception
    info::BlasInt
end

type LapackDimMisMatch <: Exception
    name::ASCIIString
end

function chkstride1(A::StridedVecOrMat...)
    for a in A
        if stride(a,1) != 1 error("LAPACK: Matrix must have contiguous columns") end
    end
end

function chksquare(A::Matrix...)
    for a in A
        m, n = size(a)
        if m != n error("LAPACK: Matrix must be square") end
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
            info = Array(BlasInt, 1)
            n    = size(AB, 2)
            mnmn = min(m, n)
            ipiv = Array(BlasInt, mnmn)
            ccall(($(string(gbtrf)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function gbtrs!(trans::BlasChar, kl::Integer, ku::Integer, m::Integer,
                        AB::StridedMatrix{$elty}, ipiv::Vector{BlasInt},
                        B::StridedVecOrMat{$elty})
            chkstride1(AB, B)
            info = Array(BlasInt, 1)
            n    = size(AB,2)
            if m != n || m != size(B,1) throw(LapackDimMisMatch("gbtrs!")) end
            ccall(($(string(gbtrs)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},   Ptr{BlasInt},
                   Ptr{BlasInt}),
                  &trans, &n, &kl, &ku, &size(B,2), AB, &stride(AB,2), ipiv,
                  B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end

## (GE) general matrices: balancing and back-transforming
for (gebal, gebak, elty, relty) in
    ((:dgebal_, :dgebak_, :Float64, :Float64),
     (:sgebal_, :sgebak_, :Float32, :Float32),
     (:zgebal_, :zgebak_, :Complex128, :Float64),
     (:cgebal_, :cgebak_, :Complex64, :Float32))
    @eval begin
        #     SUBROUTINE DGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          JOB
        #      INTEGER            IHI, ILP, INFO, LDA, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), SCALE( * )
        function gebal!(job::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            n       = size(A, 2)
            info    = Array(BlasInt, 1)
            ihi     = Array(BlasInt, 1)
            ilo     = Array(BlasInt, 1)
            scale   = Array($relty, n)
            ccall(($(string(gebal)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
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
        function gebak!(job::BlasChar, side::BlasChar,
                        ilo::BlasInt, ihi::BlasInt, scale::Vector{$elty},
                        V::StridedMatrix{$elty})
            chkstride1(V)
            chksquare(V)
            info    = Array(BlasInt, 1)
            ccall(($(string(gebak)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
for (gebrd, gelqf, geqlf, geqrf, geqp3, gerqf, getrf, elty, relty) in
    ((:dgebrd_,:dgelqf_,:dgeqlf_,:dgeqrf_,:dgeqp3_,:dgerqf_,:dgetrf_,:Float64,:Float64),
     (:sgebrd_,:sgelqf_,:sgeqlf_,:sgeqrf_,:sgeqp3_,:sgerqf_,:sgetrf_,:Float32,:Float32),
     (:zgebrd_,:zgelqf_,:zgeqlf_,:zgeqrf_,:zgeqp3_,:zgerqf_,:zgetrf_,:Complex128,:Float64),
     (:cgebrd_,:cgelqf_,:cgeqlf_,:cgeqrf_,:cgeqp3_,:cgerqf_,:cgetrf_,:Complex64,:Float32))
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
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(gebrd)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &stride(A,2), d, s, tauq, taup, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
            info  = Array(BlasInt, 1)
            m     = blas_int(size(A, 1))
            n     = blas_int(size(A, 2))
            lda   = blas_int(stride(A, 2))
            tau   = Array($elty, n)
            lwork = blas_int(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(gelqf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &lda, tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
            info  = Array(BlasInt, 1)
            m     = blas_int(size(A, 1))
            n     = blas_int(size(A, 2))
            lda   = blas_int(stride(A, 2))
            tau   = Array($elty, n)
            lwork = blas_int(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(geqlf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &lda, tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
            jpvt  = zeros(BlasInt, n)
            tau   = Array($elty, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            cmplx = iscomplex(A)
            if cmplx; rwork = Array($relty, 2n); end
            for i in 1:2
                if cmplx
                    ccall(($(string(geqp3)),liblapack), Void,
                          (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}),
                          &m, &n, A, &stride(A,2), jpvt, tau, work, &lwork, rwork, info)
                else
                    ccall(($(string(geqp3)),liblapack), Void,
                          (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}),
                          &m, &n, A, &stride(A,2), jpvt, tau, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(geqrf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
            info  = Array(BlasInt, 1)
            m, n  = size(A)
            tau   = Array($elty, n)
            lwork = blas_int(-1)
            work  = Array($elty, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(gerqf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
            info = Array(BlasInt, 1)
            m, n = size(A)
            lda  = stride(A, 2)
            ipiv = Array(BlasInt, min(m,n))
            ccall(($(string(getrf)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function gels!(trans::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            btrn  = trans == 'T'
            m, n  = size(A)
            if size(B,1) != (btrn ? n : m)  throw(LapackDimMisMatch("gels!")) end
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            for i in 1:2
                ccall(($(string(gels)),liblapack), Void,
                      (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &(btrn?'T':'N'), &m, &n, &size(B,2), A, &stride(A,2),
                      B, &stride(B,2), work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
            ipiv    = Array(BlasInt, n)
            info    = Array(BlasInt, 1)
            ccall(($(string(gesv)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function getrs!(trans::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            m, n    = size(A)
            if m != n || size(B, 1) != m error("getrs!: dimension mismatch") end
            nrhs    = size(B, 2)
            info    = Array(BlasInt, 1)
            ccall(($(string(getrs)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function getri!(A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            m, n    = size(A)
            if m != n || n != length(ipiv) error("getri!: dimension mismatch") end
            lda     = stride(A, 2)
            info    = Array(BlasInt, 1)
            lwork   = -1
            work    = Array($elty, 1)
            for i in 1:2
                ccall(($(string(getri)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &n, A, &lda, ipiv, work, &lwork, info)
                if info[1] != 0 error("getri!: error $(info[1])") end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            A
        end
    end
end
for (gelsd, elty) in ((:dgelsd_, :Float64),
                      (:sgelsd_, :Float32))
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
            LAPACK.chkstride1(A, B)
            m, n  = size(A)
            if size(B, 1) != m; throw(LAPACK.LapackDimMisMatch("gelsd!")); end
            if size(B, 1) < n
                newB = Array($elty, n, size(B, 2))
                newB[1:size(B, 1), :] = B
            else
                newB = B
            end
            s     = Array($elty, min(m, n))
            rnk   = Array(BlasInt, 1)
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            iwork = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(gelsd)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      newB, &max(1,stride(B,2),n), s, &rcond, rnk, work, &lwork, iwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    iwork = Array(BlasInt, iwork[1])
                end
            end
            isa(B, Vector) ? newB[1:n] : newB[1:n,:], rnk[1]
        end
        gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}) = gelsd!(A, B, -1.)
    end
end
for (gelsd, elty, relty) in ((:zgelsd_, :Complex128, :Float64),
                             (:cgelsd_, :Complex64, :Float32))
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
            LAPACK.chkstride1(A, B)
            m, n  = size(A)
            if size(B,1) != m; throw(LAPACK.LapackDimMisMatch("gelsd!")); end
            if size(B, 1) < n
                newB = Array($elty, n, size(B, 2))
                newB[1:size(B, 1), :] = B
            else
                newB = B
            end
            s     = Array($elty, min(m, n))
            rnk   = Array(BlasInt, 1)
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            rwork = Array($relty, 1)
            iwork = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(gelsd)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty},
                       Ptr{$relty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$relty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      newB, &max(1,stride(B,2),n), s, &rcond, rnk, work, &lwork, rwork, iwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    rwork = Array($relty, blas_int(rwork[1]))
                    iwork = Array(BlasInt, iwork[1])
                end
            end
            isa(B, Vector) ? newB[1:n] : newB[1:n,:], rnk[1]
        end
        gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}) = gelsd!(A, B, -1.)
    end
end

# (GE) general matrices eigenvalue-eigenvector and singular value decompositions
for (geev, gesvd, gesdd, ggsvd, elty, relty) in
    ((:dgeev_,:dgesvd_,:dgesdd_,:dggsvd_,:Float64,:Float64),
     (:sgeev_,:sgesvd_,:sgesdd_,:sggsvd_,:Float32,:Float32),
     (:zgeev_,:zgesvd_,:zgesdd_,:zggsvd_,:Complex128,:Float64),
     (:cgeev_,:cgesvd_,:cgesdd_,:cggsvd_,:Complex64,:Float32))
    @eval begin
        #      SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
        #      $                  LDVR, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBVL, JOBVR
        #       INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
        #      $                   WI( * ), WORK( * ), WR( * )
        function geev!(jobvl::BlasChar, jobvr::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            m, n  = size(A)
            lvecs = jobvl == 'V'
            rvecs = jobvr == 'V'
            VL    = Array($elty, (n, lvecs ? n : 0))
            VR    = Array($elty, (n, rvecs ? n : 0))
            cmplx = iscomplex(A)
            if cmplx
                W     = Array($elty, n)
                rwork = Array($relty, 2n)
            else
                WR    = Array($elty, n)
                WI    = Array($elty, n)
            end
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i = 1:2
                if cmplx
                    ccall(($(string(geev)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, 
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}),
                          &jobvl, &jobvr, &n, A, &stride(A,2), W, VL, &n, VR, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(($(string(geev)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &jobvl, &jobvr, &n, A, &stride(A,2), WR, WI, VL, &n,
                          VR, &n, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
        function gesdd!(job::BlasChar, A::StridedMatrix{$elty})
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
            lwork  = blas_int(-1)
            S      = Array($relty, minmn)
            cmplx  = iscomplex(A)
            if cmplx
                rwork = Array($relty, job == 'N' ? 7*minmn : 5*minmn*minmn + 5*minmn)
            end
            iwork  = Array(BlasInt, 8*minmn)
            info   = Array(BlasInt, 1)
            for i = 1:2
                if cmplx
                    ccall(($(string(gesdd)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}, Ptr{BlasInt}),
                          &job, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, iwork, info)
                else
                    ccall(($(string(gesdd)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &job, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, iwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
        function gesvd!(jobu::BlasChar, jobvt::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            m, n   = size(A)
            minmn  = min(m, n)
            S      = Array($relty, minmn)
            U      = Array($elty, jobu  == 'A'? (m, m):(jobu  == 'S'? (m, minmn) : (m, 0)))
            VT     = Array($elty, jobvt == 'A'? (n, n):(jobvt == 'S'? (minmn, n) : (n, 0)))
            work   = Array($elty, 1)
            cmplx  = iscomplex(A)
            if cmplx; rwork = Array($relty, 5minmn); end
            lwork  = blas_int(-1)
            info   = Array(BlasInt, 1)
            for i in 1:2
                if cmplx
                    ccall(($(string(gesvd)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                          &jobu, &jobvt, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, info)
                else
                    ccall(($(string(gesvd)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &jobu, &jobvt, &m, &n, A, &stride(A,2), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            if jobu  == 'O' return A, S, VT end
            if jobvt == 'O' return U, S, A end
            U, S, VT
        end
#       SUBROUTINE ZGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,
#      $                   LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ, WORK,
#      $                   RWORK, IWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBQ, JOBU, JOBV
#       INTEGER            INFO, K, L, LDA, LDB, LDQ, LDU, LDV, M, N, P
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IWORK( * )
#       DOUBLE PRECISION   ALPHA( * ), BETA( * ), RWORK( * )
#       COMPLEX*16         A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
#      $                   U( LDU, * ), V( LDV, * ), WORK( * )
        function ggsvd!(jobu::BlasChar, jobv::BlasChar, jobq::BlasChar, A::Matrix{$elty}, B::Matrix{$elty})
            m, n = size(A)
            if size(B, 2) != n; throw(LapackDimMismatch); end
            p = size(B, 1)
            k = Array(BlasInt, 1)
            l = Array(BlasInt, 1)
            lda = max(1,stride(A, 2))
            ldb = max(1,stride(B, 2))
            alpha = Array($relty, n)
            beta = Array($relty, n)
            ldu = max(1, m)
            U = jobu == 'U' ? Array($elty, ldu, m) : Array($elty, 0)
            ldv = max(1, p)
            V = jobv == 'V' ? Array($elty, ldv, p) : Array($elty, 0)
            ldq = max(1, n)
            Q = jobq == 'Q' ? Array($elty, ldq, n) : Array($elty, 0)
            work = Array($elty, max(3n, m, p) + n)
            cmplx = iscomplex(A)
            if cmplx; rwork = Array($relty, 2n); end
            iwork = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            if cmplx
                ccall(($(string(ggsvd)),liblapack), Void,
                    (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt},
                    Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{$relty}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &jobu, &jobv, &jobq, &m, 
                    &n, &p, k, l, 
                    A, &lda, B, &ldb, 
                    alpha, beta, U, &ldu, 
                    V, &ldv, Q, &ldq, 
                    work, rwork, iwork, info)
            else
                ccall(($(string(ggsvd)),liblapack), Void,
                    (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt},
                    Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &jobu, &jobv, &jobq, &m, 
                    &n, &p, k, l, 
                    A, &lda, B, &ldb, 
                    alpha, beta, U, &ldu, 
                    V, &ldv, Q, &ldq, 
                    work, iwork, info)
            end
            if info[1] != 0; throw(LapackException(info[1])); end
            if m - k[1] - l[1] >= 0
                R = triu(A[1:k[1] + l[1],n - k[1] - l[1] + 1:n])
            else
                R = triu([A[1:m, n - k[1] - l[1]:n]; B[m - k[1] + 1:l[1], n - k[1] - l[1]:n]]   )
            end
            return U, V, Q, alpha, beta, k[1], l[1], R
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
            info = Array(BlasInt, 1)
            ccall(($(string(gtsv)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
            ipiv = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            ccall(($(string(gttrf)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{BlasInt}, Ptr{BlasInt}),
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
        function gttrs!(trans::BlasChar, dl::Vector{$elty}, d::Vector{$elty},
                        du::Vector{$elty}, du2::Vector{$elty}, ipiv::Vector{BlasInt},
                        B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(d)
            if length(dl) != n - 1 || length(du) != n - 1 throw(LapackDimMisMatch("gttrs!")) end
            if n != size(B,1) throw(LapackDimMisMatch("gttrs!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(gttrs)),liblapack), Void,
                   (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                    Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(orglq)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &size(A,1), &size(A,2), &k, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = blas_int(real(work[1]))
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
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(orgqr)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &size(A,1), &size(A,2), &k, A, &stride(A,2), tau, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = blas_int(real(work[1]))
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
        function ormlq!(side::BlasChar, trans::BlasChar, A::StridedMatrix{$elty},
                        k::Integer, tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            chkstride1(A, C)
            m     = size(C, 1)
            n     = size(C, 2) # m, n = size(C) won't work if C is a Vector
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(ormlq)),liblapack), Void,
                      (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &side, &trans, &m, &n, &k, A, &stride(A,2), tau,
                      C, &stride(C,2), work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = blas_int(real(work[1]))
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
        function ormqr!(side::BlasChar, trans::BlasChar, A::StridedMatrix{$elty},
                        k::Integer, tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            chkstride1(A, C)
            m     = size(C, 1)
            n     = size(C, 2) # m, n = size(C) won't work if C is a Vector
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(ormqr)),liblapack), Void,
                      (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &side, &trans, &m, &n, &k, A, &stride(A,2), tau,
                      C, &stride(C,2), work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0 
                    lwork = blas_int(real(work[1]))
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
        function posv!(uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            chksquare(A)
            n     = size(A,1)
            if size(B,1) != n throw(LapackDimMisMatch("posv!")) end
            info    = Array(BlasInt, 1)
            ccall(($(string(posv)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function potrf!(uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            info = Array(BlasInt, 1)
            ccall(($(string(potrf)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function potri!(uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            info = Array(BlasInt, 1)
            ccall(($(string(potri)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function potrs!(uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            chksquare(A)
            n    =  size(A,2)
            if size(B,1) != n error("potrs!: dimension mismatch") end
            info = Array(BlasInt, 1)
            ccall(($(string(potrs)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function pstrf!(uplo::BlasChar, A::StridedMatrix{$elty}, tol::Real)
            chkstride1(A)
            chksquare(A)
            n    = size(A,1)
            piv  = Array(BlasInt, n)
            rank = Array(BlasInt, 1)
            work = Array($rtyp, 2n)
            info = Array(BlasInt, 1)
            ccall(($(string(pstrf)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$rtyp}, Ptr{$rtyp}, Ptr{BlasInt}),
                  &uplo, &n, A, &stride(A,2), piv, rank, &tol, work, info)
            if info[1] < 0 throw(LapackException(info[1])) end
            A, piv, rank[1], info[1]
        end
    end
end

## (PT) positive-definite, symmetric, tri-diagonal matrices
## Direct solvers for general tridiagonal and symmetric positive-definite tridiagonal
for (ptsv, pttrf, pttrs, elty, relty) in
    ((:dptsv_,:dpttrf_,:dpttrs_,:Float64,:Float64),
     (:sptsv_,:spttrf_,:spttrs_,:Float32,:Float32), 
     (:zptsv_,:zpttrf_,:zpttrs_,:Complex128,:Float64), 
     (:cptsv_,:cpttrf_,:cpttrs_,:Complex64,:Float32))
    @eval begin
        #       SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, LDB, N, NRHS
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   B( LDB, * ), D( * ), E( * )
        function ptsv!(D::Vector{$relty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(D)
            if length(E) != n - 1 || n != size(B,1) throw(LapackDimMismatch("ptsv!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(ptsv)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &n, &size(B,2), D, E, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
        #       SUBROUTINE DPTTRF( N, D, E, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, N
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   D( * ), E( * )
        function pttrf!(D::Vector{$relty}, E::Vector{$elty})
            n    = length(D)
            if length(E) != (n-1) throw(LapackDimMisMatch("pttrf!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(pttrf)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, D, E, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            D, E
        end
    end
end
for (pttrs, elty, relty) in
    ((:dpttrs_,:Float64,:Float64),
     (:spttrs_,:Float32,:Float32))
    @eval begin
        #       SUBROUTINE DPTTRS( N, NRHS, D, E, B, LDB, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, LDB, N, NRHS
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   B( LDB, * ), D( * ), E( * )
        function pttrs!(D::Vector{$relty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(D)
            if length(E) != (n-1) || size(B,1) != n throw(LapackDimMisMatch("pttrs!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(pttrs)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &n, &size(B,2), D, E, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end
for (pttrs, elty, relty) in
    ((:zpttrs_,:Complex128,:Float64),
     (:cpttrs_,:Complex64,:Float32))
    @eval begin
#       SUBROUTINE ZPTTRS( UPLO, N, NRHS, D, E, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   D( * )
#       COMPLEX*16         B( LDB, * ), E( * )
        function pttrs!(uplo::BlasChar, D::Vector{$relty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            n    = length(D)
            if length(E) != (n-1) || size(B,1) != n throw(LapackDimMisMatch("pttrs!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(pttrs)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), D, E, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end

## (TR) triangular matrices: solver and inverse
for (trtri, trtrs, elty) in
    ((:dtrtri_,:dtrtrs_,:Float64),
     (:strtri_,:strtrs_,:Float32),
     (:ztrtri_,:ztrtrs_,:Complex128),
     (:ctrtri_,:ctrtrs_,:Complex64))
    @eval begin
        #     SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          DIAG, UPLO
        #      INTEGER            INFO, LDA, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * )
        function trtri!(uplo::BlasChar, diag::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            m, n    = size(A)
            if m != n error("trtri!: dimension mismatch") end
            lda     = stride(A, 2)
            info    = Array(BlasInt, 1)
            ccall(($trtri,liblapack), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}),
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
        function trtrs!(uplo::BlasChar, trans::BlasChar, diag::BlasChar,
                        A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A)
            chksquare(A)
            n    = size(A,2)
            if size(B,1) != n throw(LapackDimMisMatch("trtrs!")) end
            info    = Array(BlasInt, 1)
            ccall(($(string(trtrs)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function stev!(job::BlasChar, dv::Vector{$elty}, ev::Vector{$elty})
            n    = length(dv)
            if length(ev) != (n-1) throw(LapackDimMisMatch("stev!")) end
            Zmat = Array($elty, (n, job != 'N' ? n : 0))
            work = Array($elty, max(1, 2n-2))
            info = Array(BlasInt, 1)
            ccall(($(string(stev)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &job, &n, dv, ev, Zmat, &n, work, info)
            if info[1] != 0 throw(LapackException(info[1])) end
            dv, Zmat
        end
    end
end

## (SY) symmetric matrices - eigendecomposition, Bunch-Kaufman decomposition,
## solvers (direct and factored) and inverse.
for (syconv, syev, sysv, sytrf, sytri, sytrs, elty, relty) in
    ((:dsyconv_,:dsyev_,:dsysv_,:dsytrf_,:dsytri_,:dsytrs_,:Float64, :Float64),
     (:ssyconv_,:ssyev_,:ssysv_,:ssytrf_,:ssytri_,:ssytrs_,:Float32, :Float32),
     (:zheconv_,:zheev_,:zhesv_,:zhetrf_,:zhetri_,:zhetrs_,:Complex128, :Float64),
     (:checonv_,:cheev_,:chesv_,:chetrf_,:chetri_,:chetrs_,:Complex64, :Float32))
    @eval begin
        #       SUBROUTINE DSYCONV( UPLO, WAY, N, A, LDA, IPIV, WORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO, WAY
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       INTEGER            IPIV( * )
        #       DOUBLE PRECISION   A( LDA, * ), WORK( * )
        function syconv!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            chksquare(A)
            n     = size(A,1)
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(syconv)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
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
        function syev!(jobz::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            cmplx = iscomplex(A)
            n     = size(A, 1)
            W     = Array($relty, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            if cmplx
                rwork = Array($relty, max(1, 3n-2))
            end
            info  = Array(BlasInt, 1)
            for i in 1:2
                if cmplx
                    ccall(($(string(syev)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                          Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                          &jobz, &uplo, &n, A, &stride(A,2), W, work, &lwork, rwork, info)
                else
                    ccall(($(string(syev)),liblapack), Void,
                          (Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                          Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                          &jobz, &uplo, &n, A, &stride(A,2), W, work, &lwork, info)
                end
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
        function sysv!(uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            chksquare(A)
            n     = size(A,1)
            if n != size(B,1) throw(LapackDimMismatch("sysv!")) end
            ipiv  = Array(BlasInt, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(sysv)),liblapack), Void,
                      (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, &size(B,2), A, &stride(A,2), ipiv, B, &stride(B,2),
                      work, &lwork, info)
                if info[1] < 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
        function sytrf!(uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            n     = size(A,1)
            ipiv  = Array(BlasInt, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(sytrf)),liblapack), Void,
                      (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, A, &stride(A,2), ipiv, work, &lwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
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
#         function sytri!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
#             chkstride1(A)
#             chksquare(A)
#             n     = size(A,1)
#             work  = Array($elty, 1)
#             lwork = blas_int(-1)
#             info  = Array(BlasInt, 1)
#             for i in 1:2
#                 ccall(($(string(sytri)),liblapack), Void,
#                       (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
#                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
#                       &uplo, &n, A, &stride(A,2), ipiv, work, &lwork, info)
#                 if info[1] != 0 throw(LapackException(info[1])) end
#                 if lwork < 0
#                     lwork = blas_int(real(work[1]))
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
        function sytri!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            chksquare(A)
            n     = size(A,1)
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(sytri)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
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
        function sytrs!(uplo::BlasChar, A::StridedMatrix{$elty},
                       ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            chksquare(A)
            n     = size(A,1)
            if n != size(B,1) throw(LapackDimMismatch("sytrs!")) end
            info  = Array(BlasInt, 1)
            ccall(($(string(sytrs)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), A, &stride(A,2), ipiv, B, &stride(B,2), info)
            if info[1] != 0 throw(LapackException(info[1])) end
            B
        end
    end
end

# New symmetric eigen solver
for (syevr, elty) in
    ((:dsyevr_,:Float64),
     (:ssyevr_,:Float32))
    @eval begin
        function syevr!(jobz::BlasChar, range::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, vl::FloatingPoint, vu::FloatingPoint, il::Integer, iu::Integer, Z::StridedMatrix{$elty}, abstol::FloatingPoint)
        #       SUBROUTINE DSYEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU,
        #      $                   ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,
        #      $                   IWORK, LIWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBZ, RANGE, UPLO
        #       INTEGER            IL, INFO, IU, LDA, LDZ, LIWORK, LWORK, M, N
        #       DOUBLE PRECISION   ABSTOL, VL, VU
        # *     ..
        # *     .. Array Arguments ..
        #       INTEGER            ISUPPZ( * ), IWORK( * )
        #       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * ), Z( LDZ, * )    
            chkstride1(A, Z)
            chksquare(A)                    
            n = size(A, 2)
            lda = max(1,stride(A,2))
            m = Array(BlasInt, 1)
            w = Array($elty, n)
            if jobz == 'N'
                ldz = 1
            elseif jobz == 'V'
                if stride(Z, 2) < n; error("Z has too few rows"); end
                if size(Z, 2) < n; error("Z has too few columns"); end
                ldz = max(1, stride(Z, 2))
            else
                error("joz must be 'N' of 'V'")
            end
            isuppz = Array(BlasInt, 2*n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            iwork = Array(BlasInt, 1)
            liwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(syevr)),liblapack), Void,
                    (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, 
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, 
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{BlasInt}),
                    &jobz, &range, &uplo, &n, 
                    A, &lda, &vl, &vu, 
                    &il, &iu, &abstol, m,
                    w, Z, &ldz, isuppz,
                    work, &lwork, iwork, &liwork, 
                    info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            return w[1:m[1]]
        end
    end
end
for (syevr, elty, relty) in
    ((:zheevr_,:Complex128,:Float64),
     (:cheevr_,:Complex64,:Float32))
    @eval begin
        function syevr!(jobz::BlasChar, range::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, vl::FloatingPoint, vu::FloatingPoint, il::Integer, iu::Integer, Z::StridedMatrix{$elty}, abstol::FloatingPoint)
#       SUBROUTINE ZHEEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU,
#      $                   ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,
#      $                   RWORK, LRWORK, IWORK, LIWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, RANGE, UPLO
#       INTEGER            IL, INFO, IU, LDA, LDZ, LIWORK, LRWORK, LWORK,
#      $                   M, N
#       DOUBLE PRECISION   ABSTOL, VL, VU
# *     ..
# *     .. Array Arguments ..
#       INTEGER            ISUPPZ( * ), IWORK( * )
#       DOUBLE PRECISION   RWORK( * ), W( * )
#       COMPLEX*16         A( LDA, * ), WORK( * ), Z( LDZ, * ) 
            chkstride1(A, Z)
            chksquare(A)
            n = size(A, 2)
            lda = max(1,stride(A,2))
            m = Array(BlasInt, 1)
            w = Array($relty, n)
            if jobz == 'N'
                ldz = 1
            elseif jobz == 'V'
                if stride(Z, 2) < n; error("Z has too few rows"); end
                if size(Z, 2) < n; error("Z has too few columns"); end
                ldz = max(1, stride(Z, 2))
            else
                error("joz must be 'N' of 'V'")
            end
            isuppz = Array(BlasInt, 2*n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            rwork = Array($relty, 1)
            lrwork = blas_int(-1)
            iwork = Array(BlasInt, 1)
            liwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(syevr)),liblapack), Void,
                    (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{BlasInt}, 
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, 
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt},
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &jobz, &range, &uplo, &n, 
                    A, &lda, &vl, &vu, 
                    &il, &iu, &abstol, m,
                    w, Z, &ldz, isuppz,
                    work, &lwork, rwork, &lrwork,
                    iwork, &liwork, info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    lrwork = blas_int(rwork[1])
                    rwork = Array($relty, lrwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            return w[1:m[1]]
        end
    end
end
syevr!(A::StridedMatrix, Z::StridedMatrix) = syevr!('V', 'A', 'U', A, 0.0, 0.0, 0, 0, Z, -1.0)
syevr!{T}(A::StridedMatrix{T}) = syevr!('N', 'A', 'U', A, 0.0, 0.0, 0, 0, zeros(T,0,0), -1.0)

# Estimate condition number
for (gecon, elty) in
    ((:dgecon_,:Float64),
     (:sgecon_,:Float32))
    @eval begin
        function gecon!(normtype::BlasChar, A::StridedMatrix{$elty}, anorm::$elty)
#                   SUBROUTINE DGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, IWORK,
#      $                   INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          NORM
#       INTEGER            INFO, LDA, N
#       DOUBLE PRECISION   ANORM, RCOND
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IWORK( * )
#       DOUBLE PRECISION   A( LDA, * ), WORK( * )
            chkstride1(A)
            n = size(A, 2)
            lda = max(1, size(A, 1))
            rcond = Array($elty, 1)
            work = Array($elty, 4n)
            iwork = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            ccall(($(string(gecon)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, 
                   Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}),
                  &normtype, &n, A, &lda, &anorm, rcond, work, iwork,
                  info)
            if info[1] != 0 throw(LapackException(info[1])) end
            return rcond[1]
        end
    end
end
for (gecon, elty, relty) in
    ((:zgecon_,:Complex128,:Float64),
     (:cgecon_,:Complex64, :Float32))
    @eval begin
        function gecon!(normtype::BlasChar, A::StridedMatrix{$elty}, anorm::$relty)
            chkstride1(A)
#       SUBROUTINE ZGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, RWORK,
#      $                   INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          NORM
#       INTEGER            INFO, LDA, N
#       DOUBLE PRECISION   ANORM, RCOND
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   RWORK( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
            chkstride1(A)
            n = size(A, 2)
            lda = max(1, size(A, 1))
            rcond = Array($relty, 1)
            work = Array($elty, 2n)
            rwork = Array($relty, 2n)
            info = Array(BlasInt, 1)
            ccall(($(string(gecon)),liblapack), Void,
                  (Ptr{Uint8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, 
                   Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{$relty},
                   Ptr{BlasInt}),
                  &normtype, &n, A, &lda, &anorm, rcond, work, rwork,
                  info)
            if info[1] < 0 throw(LapackException(info[1])) end
            return rcond[1]
        end
    end
end

# Hessenberg form
for (gehrd, elty) in
    ((:dgehrd_,:Float64),
     (:sgehrd_,:Float32),
     (:zgehrd_,:Complex128),
     (:cgehrd_,:Complex64))
    @eval begin
        function gehrd!(ilo::BlasInt, ihi::BlasInt, A::StridedMatrix{$elty})
#                 .. Scalar Arguments ..
#       INTEGER            IHI, ILO, INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION  A( LDA, * ), TAU( * ), WORK( * )
            chkstride1(A)
            chksquare(A)
            n = size(A, 1)
            tau = Array($elty, n - 1)
            work = Array($elty, 1)
            lwork = blas_int(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gehrd)),liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{BlasInt}),
                    &n, &ilo, &ihi, A, 
                    &max(1,n), tau, work, &lwork, 
                    info)
                if info[1] < 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                end
            end
            return A, tau
        end
    end
end
gehrd!(A::StridedMatrix) = gehrd!(blas_int(1), blas_int(size(A, 1)), A)

# construct Q from Hessenberg
for (orghr, elty) in
    ((:dorghr_,:Float64),
     (:sorghr_,:Float32),
     (:zunghr_,:Complex128),
     (:cunghr_,:Complex64))
    @eval begin
        function orghr!(ilo::BlasInt, ihi::BlasInt, A::StridedMatrix{$elty}, tau::StridedVector{$elty})
# *     .. Scalar Arguments ..
#       INTEGER            IHI, ILO, INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
            chkstride1(A)
            chksquare(A)
            n = size(A, 1)
            if n - length(tau) != 1 throw(LapackDimMismatch) end
            work = Array($elty, 1)
            lwork = blas_int(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(orghr)),liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{BlasInt}),
                    &n, &ilo, &ihi, A, 
                    &max(1,n), tau, work, &lwork, 
                    info)
                if info[1] < 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                end
            end
            return A
        end
    end
end
# Schur form
for (gees, elty) in
    ((:dgees_,:Float64),
     (:sgees_,:Float32))
    @eval begin
        function gees!(jobvs::BlasChar, A::StridedMatrix{$elty})
#     .. Scalar Arguments ..
#     CHARACTER          JOBVS, SORT
#     INTEGER            INFO, LDA, LDVS, LWORK, N, SDIM
#     ..
#     .. Array Arguments ..
#     LOGICAL            BWORK( * )
#     DOUBLE PRECISION   A( LDA, * ), VS( LDVS, * ), WI( * ), WORK( * ),
#    $                   WR( * )
            chkstride1(A)
            chksquare(A)
            sort = 'N'
            n = size(A, 1)
            sdim = Array(BlasInt, 1)
            wr = Array($elty, n)
            wi = Array($elty, n)
            ldvs = jobvs == 'V' ? n : 1
            vs = Array($elty, ldvs, n)
            work = Array($elty, 1)
            lwork = blas_int(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gees)),liblapack), Void,
                    (Ptr{Uint8}, Ptr{Uint8}, Ptr{Void}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{Void}, Ptr{BlasInt}),
                    &jobvs, &sort, [], &n, 
                        A, &max(1, n), sdim, wr,
                        wi, vs, &ldvs, work, 
                        &lwork, [], info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                end
            end
            if all(wi .== 0)
                return A, vs, wr
            else
                return A, vs, complex(wr, wi)
            end
        end
    end
end
for (gees, elty, relty) in
    ((:zgees_,:Complex128,:Float64),
     (:cgees_,:Complex64,:Float32))
    @eval begin
        function gees!(jobvs::BlasChar, A::StridedMatrix{$elty})
# *     .. Scalar Arguments ..
#       CHARACTER          JOBVS, SORT
#       INTEGER            INFO, LDA, LDVS, LWORK, N, SDIM
# *     ..
# *     .. Array Arguments ..
#       LOGICAL            BWORK( * )
#       DOUBLE PRECISION   RWORK( * )
#       COMPLEX*16         A( LDA, * ), VS( LDVS, * ), W( * ), WORK( * )
            chkstride1(A)
            chksquare(A)
            sort = 'N'
            n = size(A, 1)
            sdim = Array(BlasInt, 1)
            w = Array($elty, n)
            ldvs = jobvs == 'V' ? n : 1
            vs = Array($elty, ldvs, n)
            work = Array($elty, 1)
            lwork = blas_int(-1)
            rwork = Array($relty, n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gees)),liblapack), Void,
                    (Ptr{Uint8}, Ptr{Uint8}, Ptr{Void}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, 
                        Ptr{$relty}, Ptr{Void}, Ptr{BlasInt}),
                    &jobvs, &sort, [], &n, 
                        A, &max(1, n), sdim, w,
                        vs, &ldvs, work, &lwork, 
                        rwork, [], info)
                if info[1] != 0 throw(LapackException(info[1])) end
                if lwork < 0
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                end
            end
            return A, vs, w
        end
    end
end
end # module
