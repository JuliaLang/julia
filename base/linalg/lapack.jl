## The LAPACK module of interfaces to LAPACK subroutines
module LAPACK

const liblapack = Base.liblapack_name

import ..LinAlg: BlasFloat, BlasChar, BlasInt, blas_int, LAPACKException,
    DimensionMismatch, SingularException, chkstride1, chksquare

#Generic LAPACK error handlers
macro assertargsok() #Handle only negative info codes - use only if positive info code is useful!
    :(info[1]<0 && throw(ArgumentError("invalid argument #$(-info[1]) to LAPACK call")))
end
macro lapackerror() #Handle all nonzero info codes
    :(info[1]>0 ? throw(LAPACKException(info[1])) : @assertargsok )
end

macro assertnonsingular()
    :(info[1]>0 && throw(SingularError(info[1])))
end
macro assertposdef()
    :(info[1]>0 && throw(PosDefException(info[1])))
end

#Check that upper/lower (for special matrices) is correctly specified
macro chkuplo()
    :((uplo=='U' || uplo=='L') || throw(ArgumentError("""invalid uplo = $uplo

Valid choices are 'U' (upper) or 'L' (lower).""")))
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
                  &m, &n, &kl, &ku, AB, &max(1,stride(AB,2)), ipiv, info)
            @lapackerror
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
            if m != n || m != size(B,1) throw(DimensionMismatch("gbtrs!")) end
            ccall(($(string(gbtrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},   Ptr{BlasInt},
                   Ptr{BlasInt}),
                  &trans, &n, &kl, &ku, &size(B,2), AB, &max(1,stride(AB,2)), ipiv,
                  B, &max(1,stride(B,2)), info)
            @lapackerror
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
            n = chksquare(A)
            info    = Array(BlasInt, 1)
            ihi     = Array(BlasInt, 1)
            ilo     = Array(BlasInt, 1)
            scale   = Array($relty, n)
            ccall(($(string(gebal)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                  &job, &n, A, &max(1,stride(A,2)), ilo, ihi, scale, info)
            @lapackerror
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
                  (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &job, &side, &size(V,1), &ilo, &ihi, scale, &n, V, &max(1,stride(V,2)), info)
            @lapackerror
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
# geqrt - unpivoted QR by WY representation
# geqrt3! - recursive algorithm producing compact WY representation of Q
# gerqf - unpivoted RQ decomposition
# getrf - LU decomposition
#
# These mutating functions take as arguments all the values they
# return, even if the value of the function does not depend on them
# (e.g. the tau argument).  This is so that a factorization can be
# updated in place.  The condensed mutating functions, usually a
# function of A only, are defined after this block.
for (gebrd, gelqf, geqlf, geqrf, geqp3, geqrt, geqrt3, gerqf, getrf, elty, relty) in
    ((:dgebrd_,:dgelqf_,:dgeqlf_,:dgeqrf_,:dgeqp3_,:dgeqrt_,:dgeqrt3_,:dgerqf_,:dgetrf_,:Float64,:Float64),
     (:sgebrd_,:sgelqf_,:sgeqlf_,:sgeqrf_,:sgeqp3_,:sgeqrt_,:sgeqrt3_,:sgerqf_,:sgetrf_,:Float32,:Float32),
     (:zgebrd_,:zgelqf_,:zgeqlf_,:zgeqrf_,:zgeqp3_,:zgeqrt_,:zgeqrt3_,:zgerqf_,:zgetrf_,:Complex128,:Float64),
     (:cgebrd_,:cgelqf_,:cgeqlf_,:cgeqrf_,:cgeqp3_,:cgeqrt_,:cgeqrt3_,:cgerqf_,:cgetrf_,:Complex64,:Float32))
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
                      &m, &n, A, &max(1,stride(A,2)), d, s, tauq, taup, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            tauq, taup
        end
        # SUBROUTINE DGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function gelqf!(A::StridedMatrix{$elty}, tau::Vector{$elty})
            chkstride1(A)
            info  = Array(BlasInt, 1)
            m     = blas_int(size(A, 1))
            n     = blas_int(size(A, 2))
            lda   = blas_int(max(1,stride(A, 2)))
            if length(tau) != min(m,n) throw(DimensionMismatch("gelqf!")) end
            lwork = blas_int(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(gelqf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &lda, tau, work, &lwork, info)
                @lapackerror
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
        function geqlf!(A::StridedMatrix{$elty}, tau::Vector{$elty})
            chkstride1(A)
            info  = Array(BlasInt, 1)
            m     = blas_int(size(A, 1))
            n     = blas_int(size(A, 2))
            lda   = blas_int(max(1,stride(A, 2)))
            if length(tau) != min(m,n) throw(DimensionMismatch("geqlf!")) end
            lwork = blas_int(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(geqlf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &lda, tau, work, &lwork, info)
                @lapackerror
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
        function geqp3!(A::StridedMatrix{$elty}, jpvt::Vector{BlasInt}, tau::Vector{$elty})
            chkstride1(A)
            m, n  = size(A)
            if length(tau) != min(m,n) || length(jpvt) != n throw(DimensionMismatch("geqp3!")) end
            lda   = max(1,stride(A,2))
            if lda == 0 return A, tau, jpvt end # Early exit
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            cmplx = iseltype(A,Complex)
            if cmplx; rwork = Array($relty, 2n); end
            for i in 1:2
                if cmplx
                    ccall(($(string(geqp3)),liblapack), Void,
                          (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}),
                          &m, &n, A, &lda, 
                          jpvt, tau, work, &lwork, 
                          rwork, info)
                else
                    ccall(($(string(geqp3)),liblapack), Void,
                          (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}),
                          &m, &n, A, &lda, 
                          jpvt, tau, work, 
                          &lwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            return A, tau, jpvt
        end
        function geqrt!(A::StridedMatrix{$elty}, T::Matrix{$elty})
            chkstride1(A)
            m, n = size(A)
            minmn = min(m, n)
            nb = size(T, 1)
            nb <= minmn || throw(ArgumentError("Block size $nb > $minmn too large"))
            lda = max(1, m)
            work = Array($elty, nb*n)
            if n > 0
                info = Array(BlasInt, 1)
                ccall(($(string(geqrt)), liblapack), Void, 
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, 
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}),
                     &m, &n, &nb, A, 
                     &lda, T, &nb, work,
                     info)
                @lapackerror
            end
            A, T
        end
        function geqrt3!(A::StridedMatrix{$elty}, T::Matrix{$elty})
            chkstride1(A); chkstride1(T)
            m, n = size(A); p, q = size(T)
            if m < n throw(DimensionMismatch("input matrix cannot have fewer rows than columns")) end
            if p < n || q < n throw(DimensionMismatch("block reflector")) end
            if n > 0
                info = Array(BlasInt, 1)
                ccall(($(string(geqrt3)), liblapack), Void, 
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                     &m, &n, A, &max(1, stride(A, 2)),
                     T, &max(1,stride(T,2)), info)
                @lapackerror
            end
            A, T
        end
        ## geqrfp! - positive elements on diagonal of R - not defined yet
        # SUBROUTINE DGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function geqrf!(A::StridedMatrix{$elty}, tau::Vector{$elty})
            chkstride1(A)
            m, n  = size(A)
            if length(tau) != min(m,n) throw(DimensionMismatch("geqrf!")) end
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(geqrf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &max(1,stride(A,2)), tau, work, &lwork, info)
                @lapackerror
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
        function gerqf!(A::StridedMatrix{$elty},tau::Vector{$elty})
            chkstride1(A)
            info  = Array(BlasInt, 1)
            m, n  = size(A)
            if length(tau) != min(m,n) throw(DimensionMismatch("gerqf!")) end
            lwork = blas_int(-1)
            work  = Array($elty, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(string(gerqf)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &max(1,stride(A,2)), tau, work, &lwork, info)
                @lapackerror
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
            lda  = max(1,stride(A, 2))
            ipiv = Array(BlasInt, min(m,n))
            ccall(($(string(getrf)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &m, &n, A, &lda, ipiv, info)
            @assertargsok
            A, ipiv, info[1] #Error code is stored in LU factorization type
        end
    end
end

gelqf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); gelqf!(A,Array(T,min(m,n))))
geqlf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); geqlf!(A,Array(T,min(m,n))))
geqrt!{T<:BlasFloat}(A::StridedMatrix{T}, nb::Integer) = geqrt!(A,Array(T,nb,minimum(size(A))))
geqrt3!{T<:BlasFloat}(A::StridedMatrix{T}) = (n=size(A,2); geqrt3!(A,Array(T,n,n)))
geqrf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); geqrf!(A,Array(T,min(m,n))))
gerqf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); gerqf!(A,Array(T,min(m,n))))

function geqp3!{T<:BlasFloat}(A::StridedMatrix{T},jpvt::Vector{BlasInt})
    m,n = size(A)
    geqp3!(A,jpvt,Array(T,min(m,n)))
end
function geqp3!{T<:BlasFloat}(A::StridedMatrix{T})
    m,n=size(A)
    geqp3!(A,zeros(BlasInt,n),Array(T,min(m,n)))
end

## Complete orthogonaliztion tools
for (tzrzf, ormrz, elty) in 
    ((:dtzrzf_,:dormrz_,:Float64),
     (:stzrzf_,:sormrz_,:Float32),
     (:ztzrzf_,:zunmrz_,:Complex128),
     (:ctzrzf_,:cunmrz_,:Complex64))
    @eval begin
 #      *       SUBROUTINE ZTZRZF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
 #   22 * 
 #   23 *       .. Scalar Arguments ..
 #   24 *       INTEGER            INFO, LDA, LWORK, M, N
 #   25 *       ..
 #   26 *       .. Array Arguments ..
 #   27 *       COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
        function tzrzf!(A::StridedMatrix{$elty})
            m, n = size(A)
            if n < m throw(DimensionMismatch("matrix cannot have fewer columns than rows")) end
            lda = max(1, m)
            tau = Array($elty, m)
            work = Array($elty, 1)
            lwork = -1
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(tzrzf)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &m, &n, A, &lda, 
                    tau, work, &lwork, info)
                if i == 1
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            A, tau
        end
   # 21 *       SUBROUTINE ZUNMRZ( SIDE, TRANS, M, N, K, L, A, LDA, TAU, C, LDC,
   # 22 *                          WORK, LWORK, INFO )
   # 23 * 
   # 24 *       .. Scalar Arguments ..
   # 25 *       CHARACTER          SIDE, TRANS
   # 26 *       INTEGER            INFO, K, L, LDA, LDC, LWORK, M, N
   # 27 *       ..
   # 28 *       .. Array Arguments ..
   # 29 *       COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
        function ormrz!(side::BlasChar, trans::BlasChar, A::StridedMatrix{$elty}, tau::StridedVector{$elty}, C::StridedMatrix{$elty})
            m, n = size(C)
            k = length(tau)
            l = size(A, 2) - size(A, 1)
            lda = max(1, k)
            ldc = max(1, m)
            work = Array($elty, 1)
            lwork = -1
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(ormrz)), liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{BlasInt}),
                    &side, &trans, &m, &n, 
                    &k, &l, A, &lda, 
                    tau, C, &ldc, work, 
                    &lwork, info)
                if i == 1
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            C
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
            if size(B,1) != (btrn ? n : m)  throw(DimensionMismatch("gels!")) end
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            for i in 1:2
                ccall(($(string(gels)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &(btrn?'T':'N'), &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      B, &max(1,stride(B,2)), work, &lwork, info)
                @lapackerror
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
            n = chksquare(A)
            if size(B,1) != n throw(DimensionMismatch("gesv!")) end
            ipiv    = Array(BlasInt, n)
            info    = Array(BlasInt, 1)
            ccall(($(string(gesv)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)), info)
            @lapackerror
            B, A, ipiv
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
            n = chksquare(A)
            n==size(B, 1) || throw(DimensionMismatch("left and right hand sides do not fit"))
            nrhs = size(B, 2)
            info = Array(BlasInt, 1)
            ccall(($(string(getrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &trans, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)), info)
            @lapackerror
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
            n = chksquare(A)
            n==length(ipiv) || throw(DimensionMismatch("getri!"))
            lda     = max(1,stride(A, 2))
            info    = Array(BlasInt, 1)
            lwork   = -1
            work    = Array($elty, 1)
            for i in 1:2
                ccall(($(string(getri)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &n, A, &lda, ipiv, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            A
        end
    end
end
for (gesvx, elty) in
    ((:dgesvx_,:Float64),
     (:sgesvx_,:Float32))
    @eval begin
        #     SUBROUTINE DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV,
        #                        EQUED, R, C, B, LDB, X, LDX, RCOND, FERR, BERR,
        #                        WORK, IWORK, INFO )
        # 
        #     .. Scalar Arguments ..
        #     CHARACTER          EQUED, FACT, TRANS
        #     INTEGER            INFO, LDA, LDAF, LDB, LDX, N, NRHS
        #     DOUBLE PRECISION   RCOND
        #     ..
        #     .. Array Arguments ..
        #     INTEGER            IPIV( * ), IWORK( * )
        #     DOUBLE PRECISION   A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
        #    $                   BERR( * ), C( * ), FERR( * ), R( * ),
        #    $                   WORK( * ), X( LDX, * 
        #
        function gesvx!(fact::BlasChar, trans::BlasChar, A::StridedMatrix{$elty},
               AF::StridedMatrix{$elty}, ipiv::Vector{BlasInt}, equed::BlasChar,
               R::Vector{$elty}, C::Vector{$elty}, B::StridedVecOrMat{$elty})
            lda, n    = size(A)
            ldaf, n   = size(AF)
            ldb, nrhs = ndims(B)==2 ? size(B) : (size(B,1), 1)
            rcond     = Array($elty, 1)
            ferr      = Array($elty, nrhs)
            berr      = Array($elty, nrhs)
            work      = Array($elty, 4n)
            iwork     = Array($elty, n)
            info      = Array(BlasInt, 1)
            X = Array($elty, n, nrhs)
            ccall(($(string(gesvx)),liblapack), Void,
              (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{BlasChar}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
               Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
               Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
              &fact, &trans, &n, &nrhs, A, &lda, AF, &ldaf, ipiv, &equed, R, C, B,
              &ldb, X, &n, rcond, ferr, berr, work, iwork, info)
            @lapackerror
            if info[1] == n+1 warn("Matrix is singular to working precision.") 
            else @assertnonsingular
            end
            #WORK(1) contains the reciprocal pivot growth factor norm(A)/norm(U)
            X, equed, R, C, B, rcond[1], ferr, berr, work[1]
        end
        #Wrapper for the no-equilibration, no-transpose calculation
        function gesvx!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            n=size(A,1)
            X, equed, R, C, B, rcond, ferr, berr, rpgf = gesvx!('N', 'N', A, Array($elty, n, n), Array(BlasInt, n), 'N', Array($elty, n),  Array($elty, n), B)
            X, rcond, ferr, berr, rpgf
        end
    end
end
for (gesvx, elty, relty) in
    ((:zgesvx_,:Complex128,:Float64),
     (:cgesvx_,:Complex64 ,:Float32))
    @eval begin
        #     SUBROUTINE ZGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV,
        #                        EQUED, R, C, B, LDB, X, LDX, RCOND, FERR, BERR,
        #                        WORK, RWORK, INFO )
        #
        #     .. Scalar Arguments ..
        #     CHARACTER          EQUED, FACT, TRANS
        #     INTEGER            INFO, LDA, LDAF, LDB, LDX, N, NRHS
        #     DOUBLE PRECISION   RCOND
        #     ..
        #     .. Array Arguments ..
        #     INTEGER            IPIV( * )
        #     DOUBLE PRECISION   BERR( * ), C( * ), FERR( * ), R( * ),
        #    $                   RWORK( * )
        #     COMPLEX*16         A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
        #    $                   WORK( * ), X( LDX, * )
        function gesvx!(fact::BlasChar, trans::BlasChar, A::StridedMatrix{$elty},
             AF::StridedMatrix{$elty}, ipiv::Vector{BlasInt}, equed::BlasChar,
             R::Vector{$relty}, C::Vector{$relty}, B::StridedVecOrMat{$elty})
            lda, n    = size(A)
            ldaf, n   = size(AF)
            ldb, nrhs = ndims(B)==2 ? size(B) : (size(B, 1), 1)
            rcond     = Array($relty, 1)
            ferr      = Array($relty, nrhs)
            berr      = Array($relty, nrhs)
            work      = Array($elty, 4n)
            rwork     = Array($relty, 2n)
            info      = Array(BlasInt, 1)
            x = Array($elty, n, nrhs)
            ccall(($(string(gesvx)),liblapack), Void,
              (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{BlasChar}, Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt},
               Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$relty}, Ptr{$relty},
               Ptr{$elty}, Ptr{$relty}, Ptr{BlasInt}),
              &fact, &trans, &n, &nrhs, A, &lda, AF, &ldaf, ipiv, &equed, R, C, B,
              &ldb, X, &n, rcond, ferr, berr, work, rwork, info)
            @lapackerror
            if info[1] == n+1 warn("Matrix is singular to working precision.")
            else @assertnonsingular end
            #RWORK(1) contains the reciprocal pivot growth factor norm(A)/norm(U)
            X, equed, R, C, B, rcond[1], ferr, berr, rwork[1]
        end
        #Wrapper for the no-equilibration, no-transpose calculation
        function gesvx!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            n=size(A,1)
            X, equed, R, C, B, rcond, ferr, berr, rpgf = gesvx!('N', 'N', A, Array($elty, n, n), Array(BlasInt, n), 'N', Array($relty, n),  Array($relty, n), B)
            X, rcond, ferr, berr, rpgf
        end
    end
 end

for (gelsd, gelsy, elty) in 
    ((:dgelsd_,:dgelsy_,:Float64),
     (:sgelsd_,:sgelsy_,:Float32))
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
        function gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, rcond::Real=-one($elty))
            chkstride1(A, B)
            m, n  = size(A)
            if size(B, 1) != m; throw(DimensionMismatch("gelsd!")); end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            s     = Array($elty, min(m, n))
            rcond = convert($elty, rcond)
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
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    iwork = Array(BlasInt, iwork[1])
                end
            end
            isa(B, Vector) ? newB[1:n] : newB[1:n,:], rnk[1]
        end

#       SUBROUTINE DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
#      $                   WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
#       DOUBLE PRECISION   RCOND
# *     ..
# *     .. Array Arguments ..
#       INTEGER            JPVT( * )
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
        function gelsy!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, rcond::Real=eps($elty))
            chkstride1(A, B)
            m = size(A, 1)
            n = size(A, 2)
            nrhs = size(B, 2)
            if m != size(B, 1) throw(DimensionMismatch("left and right hand sides must have same number of rows")) end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            lda = max(1, m)
            ldb = max(1, m, n)
            jpvt = Array(BlasInt, n)
            rcond = convert($elty, rcond)
            rnk = Array(BlasInt, 1)
            work = Array($elty, 1)
            lwork = -1
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gelsy)), liblapack), Void, 
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{BlasInt}),
                    &m, &n, &nrhs, A, 
                    &lda, newB, &ldb, jpvt, 
                    &rcond, rnk, work, &lwork, 
                    info)
                if i == 1
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            isa(B, Vector) ? newB[1:n] : newB[1:n,:], rnk[1]
        end
    end
end
for (gelsd, gelsy, elty, relty) in 
    ((:zgelsd_,:zgelsy_,:Complex128,:Float64),
     (:cgelsd_,:cgelsy_,:Complex64,:Float32))
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
        function gelsd!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, rcond::Real=-one($relty))
            chkstride1(A, B)
            m, n  = size(A)
            if size(B,1) != m; throw(DimensionMismatch("gelsd!")); end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            s     = Array($elty, min(m, n))
            rcond = convert($relty, rcond)
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
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    rwork = Array($relty, blas_int(rwork[1]))
                    iwork = Array(BlasInt, iwork[1])
                end
            end
            isa(B, Vector) ? newB[1:n] : newB[1:n,:], rnk[1]
        end

#       SUBROUTINE ZGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
#      $                   WORK, LWORK, RWORK, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
#       DOUBLE PRECISION   RCOND
# *     ..
# *     .. Array Arguments ..
#       INTEGER            JPVT( * )
#       DOUBLE PRECISION   RWORK( * )
#       COMPLEX*16         A( LDA, * ), B( LDB, * ), WORK( * )
        function gelsy!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, rcond::Real=eps($relty))
            chkstride1(A, B)
            m, n = size(A, 1)
            nrhs = size(B, 2)
            if m != size(B, 1) throw(DimensionMismatch("left and right hand sides must have same number of rows")) end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            lda = max(1, m)
            ldb = max(1, m, n)
            jpvt = Array(BlasInt, n)
            rcond = convert($relty, rcond)
            rnk = Array(BlasInt, 1)
            work = Array($elty, 1)
            lwork = -1
            rwork = Array($relty, 2n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gelsy)), liblapack), Void, 
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$relty}, Ptr{BlasInt}),
                    &m, &n, &nrhs, A, 
                    &lda, newB, &ldb, jpvt, 
                    &rcond, rnk, work, &lwork, 
                    rwork, info)
                if i == 1
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            isa(B, Vector) ? newB[1:n] : newB[1:n,:], rnk[1]
        end
    end
end

for (gglse, elty) in ((:dgglse_, :Float64),
                      (:sgglse_, :Float32),
                      (:zgglse_, :Complex128),
                      (:cgglse_, :Complex64))
    @eval begin
        # SUBROUTINE DGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,
        #      $                   INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LDB, LWORK, M, N, P
        # *     ..
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( * ), D( * ),
        #      $                   WORK( * ), X( * )
        function gglse!(A::StridedMatrix{$elty}, c::StridedVector{$elty},
                        B::StridedMatrix{$elty}, d::StridedVector{$elty})
            chkstride1(A, B)
            m, n = size(A)
            p = size(B, 1)
            if size(B, 2) != n || length(c) != m || length(d) != p
                throw(DimensionMismatch("gglse!"))
            end
            X = zeros($elty, n)
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            for i in 1:2
                ccall(($(string(gglse)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}),
                      &m, &n, &p, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)), c, d, X,
                      work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            X, dot(sub(c, n - p + 1:m), sub(c, n - p + 1:m))
        end
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
            n = chksquare(A)
            lvecs = jobvl == 'V'
            rvecs = jobvr == 'V'
            VL    = Array($elty, (n, lvecs ? n : 0))
            VR    = Array($elty, (n, rvecs ? n : 0))
            cmplx = iseltype(A,Complex)
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
                          (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, 
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}),
                          &jobvl, &jobvr, &n, A, &max(1,stride(A,2)), W, VL, &n, VR, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(($(string(geev)),liblapack), Void,
                          (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &jobvl, &jobvr, &n, A, &max(1,stride(A,2)), WR, WI, VL, &n,
                          VR, &n, work, &lwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            cmplx ? (W, VL, VR) : (WR, WI, VL, VR)
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
            cmplx  = iseltype(A,Complex)
            if cmplx
                rwork = Array($relty, job == 'N' ? 7*minmn :
                              minmn*max(5*minmn+7, 2*max(m,n)+2*minmn+1))
            end
            iwork  = Array(BlasInt, 8*minmn)
            info   = Array(BlasInt, 1)
            for i = 1:2
                if cmplx
                    ccall(($(string(gesdd)),liblapack), Void,
                          (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}, Ptr{BlasInt}),
                          &job, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, iwork, info)
                else
                    ccall(($(string(gesdd)),liblapack), Void,
                          (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &job, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, iwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            job=='O' ? (m>=n? (A, S, VT) : (U, S, A)) : (U, S, VT)
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
            cmplx  = iseltype(A,Complex)
            if cmplx; rwork = Array($relty, 5minmn); end
            lwork  = blas_int(-1)
            info   = Array(BlasInt, 1)
            for i in 1:2
                if cmplx
                    ccall(($(string(gesvd)),liblapack), Void,
                          (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                          &jobu, &jobvt, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, info)
                else
                    ccall(($(string(gesvd)),liblapack), Void,
                          (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &jobu, &jobvt, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            jobu=='O' ? (A, S, VT) : (jobvt=='O' ? (U, S, A) : (U, S, VT))
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
            chkstride1(A, B)
            m, n = size(A)
            if size(B, 2) != n; throw(DimensionMismatch("")); end
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
            cmplx = iseltype(A,Complex)
            if cmplx; rwork = Array($relty, 2n); end
            iwork = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            if cmplx
                ccall(($(string(ggsvd)),liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt},
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
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt},
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
            @lapackerror
            if m - k[1] - l[1] >= 0
                R = triu(A[1:k[1] + l[1],n - k[1] - l[1] + 1:n])
            else
                R = triu([A[1:m, n - k[1] - l[1] + 1:n]; B[m - k[1] + 1:l[1], n - k[1] - l[1] + 1:n]])
            end
            U, V, Q, alpha, beta, k[1], l[1], R
        end
    end
end
for (ggev, elty) in 
    ((:dggev_,:Float64),
     (:sggev_,:Float32))
    @eval begin
    #       SUBROUTINE DGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR, ALPHAI,
#      $                  BETA, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBVL, JOBVR
#       INTEGER            INFO, LDA, LDB, LDVL, LDVR, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
#      $                   B( LDB, * ), BETA( * ), VL( LDVL, * ),
#      $                   VR( LDVR, * ), WORK( * )
        function ggev!(jobvl::BlasChar, jobvr::BlasChar, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A,B)
            n, m = chksquare(A,B)
            n==m || throw(DimensionMismatch("matrices must have same size"))
            lda = max(1, n)
            ldb = max(1, n)
            alphar = Array($elty, n)
            alphai = Array($elty, n)
            beta = Array($elty, n)
            ldvl = jobvl == 'V' ? n : 1
            vl = Array($elty, ldvl, n)
            ldvr = jobvr == 'V' ? n : 1
            vr = Array($elty, ldvr, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(ggev)), liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{BlasInt}),
                    &jobvl, &jobvr, &n, A, 
                    &lda, B, &ldb, alphar, 
                    alphai, beta, vl, &ldvl, 
                    vr, &ldvr, work, &lwork, 
                    info)
                if i == 1
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            alphar, alphai, beta, vl, vr
        end
    end
end
for (ggev, elty, relty) in 
    ((:zggev_,:Complex128,:Float64),
     (:cggev_,:Complex64,:Float32))
    @eval begin
      # SUBROUTINE ZGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHA, BETA,
     # $                  VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )
# *     .. Scalar Arguments ..
      # CHARACTER          JOBVL, JOBVR
      # INTEGER            INFO, LDA, LDB, LDVL, LDVR, LWORK, N
# *     ..
# *     .. Array Arguments ..
      # DOUBLE PRECISION   RWORK( * )
      # COMPLEX*16         A( LDA, * ), ALPHA( * ), B( LDB, * ),
     # $                   BETA( * ), VL( LDVL, * ), VR( LDVR, * ),
     # $                   WORK( * )
        function ggev!(jobvl::BlasChar, jobvr::BlasChar, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A, B)
            n, m = chksquare(A, B)
            n==m || throw(DimensionMismatch("matrices must have same size"))
            lda = ldb = max(1, n)
            alpha = Array($elty, n)
            beta = Array($elty, n)
            ldvl = jobvl == 'V' ? n : 1
            vl = Array($elty, ldvl, n)
            ldvr = jobvr == 'V' ? n : 1
            vr = Array($elty, ldvr, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            rwork = Array($relty, 8n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(ggev)), liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, 
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty},
                     Ptr{BlasInt}),
                    &jobvl, &jobvr, &n, A, 
                    &lda, B, &ldb, alpha, 
                    beta, vl, &ldvl, vr, 
                    &ldvr, work, &lwork, rwork,
                    info)
                if i == 1
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            alpha, beta, vl, vr
        end
    end
end
# One step incremental condition estimation of max/min singular values
for (laic1, elty) in
    ((:dlaic1_,:Float64),
     (:slaic1_,:Float32))
    @eval begin
   # 21 *       SUBROUTINE DLAIC1( JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
   # 22 * 
   # 23 *       .. Scalar Arguments ..
   # 24 *       INTEGER            J, JOB
   # 25 *       DOUBLE PRECISION   C, GAMMA, S, SEST, SESTPR
   # 26 *       ..
   # 27 *       .. Array Arguments ..
   # 28 *       DOUBLE PRECISION   W( J ), X( J )
        function laic1!(job::Integer, x::StridedVector{$elty}, sest::$elty, w::StridedVector{$elty}, gamma::$elty)
            j = length(x)
            if j != length(w) throw(DimensionMismatch("Vectors must have same length")) end
            sestpr = Array($elty, 1)
            s = Array($elty, 1)
            c = Array($elty, 1)
            ccall(($(string(laic1)), liblapack), Void,
                (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                 Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                 Ptr{$elty}),
                &job, &j, x, &sest, 
                w, &gamma, sestpr, s, 
                c)
            sestpr[1], s[1], c[1]
        end
    end
end
for (laic1, elty, relty) in
    ((:zlaic1_,:Complex128,:Float64),
     (:claic1_,:Complex64,:Float32))
    @eval begin
   # 21 *       SUBROUTINE ZLAIC1( JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
   # 22 * 
   # 23 *       .. Scalar Arguments ..
   # 24 *       INTEGER            J, JOB
   # 25 *       DOUBLE PRECISION   SEST, SESTPR
   # 26 *       COMPLEX*16         C, GAMMA, S
   # 27 *       ..
   # 28 *       .. Array Arguments ..
   # 29 *       COMPLEX*16         W( J ), X( J )
        function laic1!(job::Integer, x::StridedVector{$elty}, sest::$relty, w::StridedVector{$elty}, gamma::$elty)
            j = length(x)
            if j != length(w) throw(DimensionMismatch("Vectors must have same length")) end
            sestpr = Array($relty, 1)
            s = Array($elty, 1)
            c = Array($elty, 1)
            ccall(($(string(laic1)), liblapack), Void,
                (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$relty},
                 Ptr{$elty}, Ptr{$elty}, Ptr{$relty}, Ptr{$elty},
                 Ptr{$elty}),
                &job, &j, x, &sest, 
                w, &gamma, sestpr, s, 
                c)
            sestpr[1], s[1], c[1]
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
            n = length(d)
            if length(dl) != n - 1 || length(du) != n - 1
                throw(DimensionMismatch("gtsv!"))
            end
            if n != size(B,1) throw(DimensionMismatch("gtsv!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(gtsv)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &n, &size(B,2), dl, d, du, B, &max(1,stride(B,2)), info)
            @lapackerror
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
                throw(DimensionMismatch("gttrf!"))
            end
            du2  = Array($elty, n-2)
            ipiv = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            ccall(($(string(gttrf)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{BlasInt}, Ptr{BlasInt}),
                  &n, dl, d, du, du2, ipiv, info)
            @lapackerror
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
            n = length(d)
            if length(dl) != n - 1 || length(du) != n - 1 throw(DimensionMismatch("gttrs!")) end
            if n != size(B,1) throw(DimensionMismatch("gttrs!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(gttrs)),liblapack), Void,
                   (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                    Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                   &trans, &n, &size(B,2), dl, d, du, du2, ipiv, B, &max(1,stride(B,2)), info)
             @lapackerror
             B
         end
    end
end

## (OR) orthogonal (or UN, unitary) matrices, extractors and multiplication
for (orglq, orgqr, ormlq, ormqr, gemqrt, elty) in
    ((:dorglq_,:dorgqr_,:dormlq_,:dormqr_,:dgemqrt_,:Float64),
     (:sorglq_,:sorgqr_,:sormlq_,:sormqr_,:sgemqrt_,:Float32),
     (:zunglq_,:zungqr_,:zunmlq_,:zunmqr_,:zgemqrt_,:Complex128),
     (:cunglq_,:cungqr_,:cunmlq_,:cunmqr_,:cgemqrt_,:Complex64))
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
                      &size(A,1), &size(A,2), &k, A, &max(1,stride(A,2)), tau, work, &lwork, info)
                @lapackerror
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
        function orgqr!(A::StridedMatrix{$elty}, tau::Vector{$elty})
            chkstride1(A)
            m = size(A, 1)
            n = min(m, size(A, 2))
            k = length(tau)
            if k > n throw(DimensionMismatch("invalid number of reflectors")) end
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(orgqr)),liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, &k, A, 
                      &max(1,stride(A,2)), tau, work, &lwork, 
                      info)
                @lapackerror
                if lwork < 0 
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A[:,1:n]
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
            m, n = ndims(C)==2 ? size(C) : (size(C, 1), 1)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(ormlq)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &side, &trans, &m, &n, &k, A, &max(1,stride(A,2)), tau,
                      C, &max(1,stride(C,2)), work, &lwork, info)
                @lapackerror
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
                        tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            chkstride1(A, C)
            m, n = ndims(C)==2 ? size(C) : (size(C, 1), 1)
            mA    = size(A, 1)
            k     = length(tau)
            if side == 'L' && m != mA throw(DimensionMismatch("")) end
            if side == 'R' && n != mA throw(DimensionMismatch("")) end            
            if (side == 'L' && k > m) || (side == 'R' && k > n) throw(DimensionMismatch("invalid number of reflectors")) end
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(ormqr)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, 
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, 
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, 
                       Ptr{BlasInt}),
                      &side, &trans, &m, &n, 
                      &k, A, &max(1,stride(A,2)), tau,
                      C, &max(1, stride(C,2)), work, &lwork, 
                      info)
                @lapackerror
                if lwork < 0 
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            C
        end
        function gemqrt!(side::Char, trans::Char, V::Matrix{$elty}, T::Matrix{$elty}, C::StridedVecOrMat{$elty})
            chkstride1(T, C)
            m, n = ndims(C)==2 ? size(C) : (size(C, 1), 1)
            nb, k = size(T)
            if k == 0 return C end
            if side == 'L'
                0 <= k <= m || throw(DimensionMismatch("Wrong value for k"))
                m == size(V,1) || throw(DimensionMismatch(""))
                ldv = stride(V,2)
                ldv >= max(1, m) || throw(DimensionMismatch("Q and C don't fit"))
                wss = n*k
            elseif side == 'R'
                0 <= k <= n || throw(DimensionMismatch("Wrong value for k"))
                n == size(V,1) || throw(DimensionMismatch(""))
                ldv = stride(V,2)
                ldv >= max(1, n) || throw(DimensionMismatch("stride error"))
                wss = m*k
            end
            1 <= nb <= k || throw(DimensionMismatch("Wrong value for nb"))
            ldc = max(1, m)
            work = Array($elty, wss)
            info = Array(BlasInt, 1)
            ccall(($(string(gemqrt)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}),
                &side, &trans, &m, &n,
                &k, &nb, V, &ldv,
                T, &nb, C, &ldc,
                work, info)
            @lapackerror
            return C
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
        #     SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function posv!(uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            n = chksquare(A)
            @chkuplo
            if size(B,1) != n throw(DimensionMismatch("posv!")) end
            info    = Array(BlasInt, 1)
            ccall(($(string(posv)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), B, &max(1,stride(B,2)), info)
            @assertargsok
            @assertposdef
            A, B
        end
        # SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function potrf!(uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            @chkuplo
            lda = max(1,stride(A,2))
            lda==0 && return A, 0
            info = Array(BlasInt, 1)
            ccall(($(string(potrf)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &size(A,1), A, &lda, info)
            @assertargsok
            #info[1]>0 means the leading minor of order info[i] is not positive definite
            #ordinarily, throw Exception here, but return error code here
            #this simplifies isposdef! and factorize!
            return A, info[1]
        end
        #       SUBROUTINE DPOTRI( UPLO, N, A, LDA, INFO )
        #       .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function potri!(uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            @chkuplo
            info = Array(BlasInt, 1)
            ccall(($(string(potri)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &size(A,1), A, &max(1,stride(A,2)), info)
            @assertargsok
            @assertnonsingular
            A
        end
        #     SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function potrs!(uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            n = chksquare(A)
            @chkuplo
            nrhs = size(B,2)
            if size(B,1) != n throw(DimensionMismatch("left and right hand sides do not fit")) end
            lda = max(1,stride(A,2))
            if lda == 0 || nrhs == 0 return B end
            ldb = max(1,stride(B,2))
            info = Array(BlasInt, 1)
            ccall(($(string(potrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, 
                    Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                   &uplo, &n, &nrhs, A, 
                   &lda, B, &ldb, info)
            @lapackerror
            return B
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
            n = chksquare(A)
            @chkuplo
            piv  = Array(BlasInt, n)
            rank = Array(BlasInt, 1)
            work = Array($rtyp, 2n)
            info = Array(BlasInt, 1)
            ccall(($(string(pstrf)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$rtyp}, Ptr{$rtyp}, Ptr{BlasInt}),
                  &uplo, &n, A, &max(1,stride(A,2)), piv, rank, &tol, work, info)
            @lapackerror
            A, piv, rank[1], info[1] #Stored in PivotedCholesky
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
            n = length(D)
            if length(E) != n - 1 || n != size(B,1) throw(DimensionMismatch("ptsv!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(ptsv)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &n, &size(B,2), D, E, B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
        #       SUBROUTINE DPTTRF( N, D, E, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, N
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   D( * ), E( * )
        function pttrf!(D::Vector{$relty}, E::Vector{$elty})
            n = length(D)
            if length(E) != (n-1) throw(DimensionMismatch("pttrf!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(pttrf)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, D, E, info)
            @lapackerror
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
            n = length(D)
            if length(E) != (n-1) || size(B,1) != n throw(DimensionMismatch("pttrs!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(pttrs)),liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &n, &size(B,2), D, E, B, &max(1,stride(B,2)), info)
            @lapackerror
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
            @chkuplo
            n = length(D)
            if length(E) != (n-1) || size(B,1) != n throw(DimensionMismatch("pttrs!")) end
            info = Array(BlasInt, 1)
            ccall(($(string(pttrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), D, E, B, &max(1,stride(B,2)), info)
            @lapackerror
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
            n = chksquare(A)
            @chkuplo
            lda     = max(1,stride(A, 2))
            info    = Array(BlasInt, 1)
            ccall(($(string(trtri)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}),
                  &uplo, &diag, &n, A, &lda, info)
            @lapackerror
            A
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
            n = chksquare(A)
            @chkuplo
            if size(B,1) != n throw(DimensionMismatch("trtrs!")) end
            info    = Array(BlasInt, 1)
            ccall(($(string(trtrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &trans, &diag, &n, &size(B,2), A, &max(1,stride(A,2)),
                  B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
    end
end

#Eigenvector computation and condition number estimation
for (trcon, trevc, elty) in
    ((:dtrcon_,:dtrevc_,:Float64),
     (:strcon_,:strevc_,:Float32))
    @eval begin
        #SUBROUTINE DTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,
        #                   IWORK, INFO )
        #.. Scalar Arguments ..
        #CHARACTER          DIAG, NORM, UPLO
        #INTEGER            INFO, LDA, N
        #DOUBLE PRECISION   RCOND
        #.. Array Arguments ..
        #INTEGER            IWORK( * )
        #DOUBLE PRECISION   A( LDA, * ), WORK( * )
        function trcon!(norm::BlasChar, uplo::BlasChar, diag::BlasChar,
                        A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            @chkuplo
            rcond = Array($elty, 1)
            work  = Array($elty, 3n)
            iwork = Array(BlasInt, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(trcon)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &norm, &uplo, &diag, &n,
                  A, &max(1,stride(A,2)), rcond, work, iwork, info)
            @lapackerror
            rcond[1]
        end
        # SUBROUTINE DTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
        #                    LDVR, MM, M, WORK, INFO )
        #
        # .. Scalar Arguments ..
        # CHARACTER          HOWMNY, SIDE
        # INTEGER            INFO, LDT, LDVL, LDVR, M, MM, N
        # ..
        # .. Array Arguments ..
        # LOGICAL            SELECT( * )
        # DOUBLE PRECISION   T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
        #$                   WORK( * )
        function trevc!(side::BlasChar, howmny::BlasChar,
                select::Vector{Bool}, A::StridedMatrix{$elty},
                VL::StridedMatrix{$elty}=similar(A), VR::StridedMatrix{$elty}=similar(A))
            chkstride1(A)
            chksquare(A)
            ldt, n = size(A)
            ldvl, mm = size(VL)
            ldvr, mm = size(VR)
            m = Array(BlasInt, 1)
            work = Array($elty, 3n)
            info = Array(BlasInt, 1)
            ccall(($(string(trevc)),liblapack), Void,
            (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{Bool}, Ptr{BlasInt}, Ptr{$elty},
            Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
            &side, &howmny, select, &n, A, &ldt, VL, &ldvl, VR, &ldvr, &mm,
            m, work, info)
            @lapackerror

            #Decide what exactly to return
            if howmny=='S' #compute selected eigenvectors
                if side=='L' #left eigenvectors only
                    return select, VL[:,1:m[1]]
                elseif side=='R' #right eigenvectors only
                    return select, VR[:,1:m[1]]
                else #side=='B' #both eigenvectors
                    return select, VL[:,1:m[1]], VR[:,1:m[1]]
                end
            else #compute all eigenvectors
                if side=='L' #left eigenvectors only
                    return VL[:,1:m[1]]
                elseif side=='R' #right eigenvectors only
                    return VR[:,1:m[1]]
                else #side=='B' #both eigenvectors
                    return VL[:,1:m[1]], VR[:,1:m[1]]
                end
            end
        end
    end
end
for (trcon, trevc, elty, relty) in
    ((:ztrcon_,:ztrevc_,Complex128,:Float64),
     (:ctrcon_,:ctrevc_,Complex64,:Float32))
    @eval begin
        #SUBROUTINE ZTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND, WORK,
        #                   RWORK, INFO )
        #.. Scalar Arguments ..
        #CHARACTER          DIAG, NORM, UPLO
        #INTEGER            INFO, LDA, N
        #DOUBLE PRECISION   RCOND
        #.. Array Arguments ..
        #DOUBLE PRECISION   RWORK( * )
        #COMPLEX*16         A( LDA, * ), WORK( * )
        function trcon!(norm::BlasChar, uplo::BlasChar, diag::BlasChar,
                        A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            @chkuplo
            rcond = Array($relty, 1)
            work  = Array($elty, 2n)
            rwork = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(trcon)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty}, Ptr{$relty}, Ptr{BlasInt}),
                  &norm, &uplo, &diag, &n,
                  A, &max(1,stride(A,2)), rcond, work, rwork, info)
            @lapackerror
            rcond[1]
        end

        # SUBROUTINE ZTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
        #                    LDVR, MM, M, WORK, RWORK, INFO )
        #
        # .. Scalar Arguments ..
        # CHARACTER          HOWMNY, SIDE
        # INTEGER            INFO, LDT, LDVL, LDVR, M, MM, N
        # ..
        # .. Array Arguments ..
        # LOGICAL            SELECT( * )
        # DOUBLE PRECISION   RWORK( * )
        # COMPLEX*16         T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
        #$                   WORK( * )
        function trevc!(side::BlasChar, howmny::BlasChar,
                select::Vector{Bool}, A::StridedMatrix{$elty},
                VL::StridedMatrix{$elty}=similar(A), VR::StridedMatrix{$elty}=similar(A))
            chkstride1(A)
            chksquare(A)
            ldt, n = size(A)
            ldvl, mm = size(VL)
            ldvr, mm = size(VR)
            m = Array(BlasInt, 1)
            work = Array($elty, 2n)
            rwork = Array($relty, n)
            info = Array(BlasInt, 1)
            ccall(($(string(trevc)),liblapack), Void,
            (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{Bool}, Ptr{BlasInt}, Ptr{$elty},
            Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
            Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$relty}, Ptr{BlasInt}),
            &side, &howmny, select, &n, A, &ldt, VL, &ldvl, VR, &ldvr, &mm,
            m, work, rwork, info)
            @lapackerror

            #Decide what exactly to return
            if howmny=='S' #compute selected eigenvectors
                if side=='L' #left eigenvectors only
                    return select, VL[:,1:m[1]]
                elseif side=='R' #right eigenvectors only
                    return select, VR[:,1:m[1]]
                else #side=='B' #both eigenvectors
                    return select, VL[:,1:m[1]], VR[:,1:m[1]]
                end
            else #compute all eigenvectors
                if side=='L' #left eigenvectors only
                    return VL[:,1:m[1]]
                elseif side=='R' #right eigenvectors only
                    return VR[:,1:m[1]]
                else #side=='B' #both eigenvectors
                    return VL[:,1:m[1]], VR[:,1:m[1]]
                end
            end
        end
    end
end

## (ST) Symmetric tridiagonal - eigendecomposition
for (stev, stebz, stegr, stein, elty) in
    ((:dstev_,:dstebz_,:dstegr_,:dstein_,:Float64),
     (:sstev_,:sstebz_,:sstegr_,:sstein_,:Float32)
#     , (:zstev_,:Complex128)  Need to rewrite for ZHEEV, rwork, etc.
#     , (:cstev_,:Complex64)
     )
    @eval begin
        #*  DSTEV computes all eigenvalues and, optionally, eigenvectors of a
        #*  real symmetric tridiagonal matrix A.
        function stev!(job::BlasChar, dv::Vector{$elty}, ev::Vector{$elty})
            n = length(dv)
            if length(ev) != (n-1) throw(DimensionMismatch("stev!")) end
            Zmat = Array($elty, (n, job != 'N' ? n : 0))
            work = Array($elty, max(1, 2n-2))
            info = Array(BlasInt, 1)
            ccall(($(string(stev)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &job, &n, dv, ev, Zmat, &n, work, info)
            @lapackerror 
            dv, Zmat
        end
        #*  DSTEBZ computes the eigenvalues of a symmetric tridiagonal
        #*  matrix T.  The user may ask for all eigenvalues, all eigenvalues
        #*  in the half-open interval (VL, VU], or the IL-th through IU-th
        #*  eigenvalues.
        function stebz!(range::Char, order::Char, vl::$elty, vu::$elty, il::Integer, iu::Integer, abstol::Real, dv::Vector{$elty}, ev::Vector{$elty})
            n = length(dv)
            if length(ev) != (n-1) throw(DimensionMismatch("stebz!")) end
            m = Array(BlasInt,1)
            nsplit = Array(BlasInt,1)
            w = Array($elty, n)
            tmp = 0.0
            iblock = Array(BlasInt,n)
            isplit = Array(BlasInt,n)
            work = Array($elty, 4*n)
            iwork = Array(BlasInt,3*n)
            info = Array(BlasInt, 1)
            ccall(($(string(stebz)),liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                Ptr{BlasInt}, Ptr{BlasInt}),
                &range, &order, &n, &vl, 
                &vu, &il, &iu, &abstol, 
                dv, ev, m, nsplit,
                w, iblock, isplit, work, 
                iwork, info)
                @lapackerror
            w[1:m[1]], iblock[1:m[1]], isplit[1:nsplit[1]]
        end
        #*  DSTEGR computes selected eigenvalues and, optionally, eigenvectors
        #*  of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
        #*  a well defined set of pairwise different real eigenvalues, the corresponding
        #*  real eigenvectors are pairwise orthogonal.
        #*
        #*  The spectrum may be computed either completely or partially by specifying
        #*  either an interval (VL,VU] or a range of indices IL:IU for the desired
        #*  eigenvalues.
        function stegr!(jobz::BlasChar, range::BlasChar, dv::Vector{$elty}, ev::Vector{$elty}, vl::Real, vu::Real, il::Integer, iu::Integer)
            n = length(dv)
            if length(ev) != (n-1) throw(DimensionMismatch("stebz!")) end
            eev = [ev, zero($elty)]
            abstol = Array($elty, 1)
            m = Array(BlasInt, 1)
            w = Array($elty, n)
            ldz = jobz == 'N' ? 1 : n
            Z = Array($elty, ldz, n)
            isuppz = Array(BlasInt, 2n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            iwork = Array(BlasInt, 1)
            liwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(stegr)), liblapack), Void, 
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                    Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                    Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &jobz, &range, &n, dv,
                    eev, &vl, &vu, &il,
                    &iu, abstol, m, w,
                    Z, &ldz, isuppz, work,
                    &lwork, iwork, &liwork, info)
                if i == 1
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            @lapackerror
            w[1:m[1]], Z[:,1:m[1]]
        end
        #*  DSTEIN computes the eigenvectors of a real symmetric tridiagonal
        #*  matrix T corresponding to specified eigenvalues, using inverse
        #*  iteration.
        #      SUBROUTINE DSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK,
        #     $                   IWORK, IFAIL, INFO )
        # We allow the user to specify exactly which eigenvectors to get by
        # specifying the eigenvalues (which may be approximate) via w_in
        function stein!(dv::Vector{$elty}, ev_in::Vector{$elty}, w_in::Vector{$elty}, iblock_in::Vector{BlasInt}, isplit_in::Vector{BlasInt})
            n = length(dv)
            if length(ev_in) != (n-1) throw(DimensionMismatch("stein!")) end
            ev = [ev_in; zeros($elty,1)]
            ldz = n #Leading dimension
            #Number of eigenvalues to find
            1<=length(w_in)<=n ? (m=length(w_in)) : throw(DimensionMismatch("stein!"))
            #If iblock and isplit are invalid input, assume worst-case block paritioning,
            # i.e. set the block scheme to be the entire matrix
            iblock = Array(BlasInt,n)
            isplit = Array(BlasInt,n)
            w = Array($elty,n)
            if length(iblock_in) < m #Not enough block specifications
                iblock[1:m] = ones(BlasInt, m)
                w[1:m] = sort(w_in)
            else
                iblock[1:m] = iblock_in
                w[1:m] = w_in #Assume user has sorted the eigenvalues properly
            end
            if length(isplit_in) < 1 #Not enough block specifications
                isplit[1] = n
            else
                isplit[1:length(isplit_in)] = isplit_in
            end

            z = Array($elty,(n,m))
            work = Array($elty, 5*n)
            iwork = Array(BlasInt,n)
            ifail = Array(BlasInt,m)
            info = Array(BlasInt,1)

            ccall(($(string(stein)),liblapack), Void,
                (Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                Ptr{BlasInt}),
                &n, dv, ev, &m, w, iblock, isplit, z, &ldz, work, iwork, ifail, info)
            
            @lapackerror
            all(ifail.==0) || error("failed to converge eigenvectors:\n$(nonzeros(ifail))")
            z
        end
    end
end
stegr!(jobz::BlasChar, dv::Vector, ev::Vector) = stegr!(jobz, 'A', dv, ev, 0.0, 0.0, 0, 0)
        
# Allow user to skip specification of iblock and isplit
stein!(dv::Vector, ev::Vector, w_in::Vector)=stein!(dv, ev, w_in, zeros(BlasInt,0), zeros(BlasInt,0))
# Allow user to specify just one eigenvector to get in stein!
stein!(dv::Vector, ev::Vector, eval::Real)=stein!(dv, ev, [eval], zeros(BlasInt,0), zeros(BlasInt,0))

## (SY) symmetric real matrices - Bunch-Kaufman decomposition,
## solvers (direct and factored) and inverse.
for (syconv, sysv, sytrf, sytri, sytrs, elty) in
    ((:dsyconv_,:dsysv_,:dsytrf_,:dsytri_,:dsytrs_,:Float64),
     (:ssyconv_,:ssysv_,:ssytrf_,:ssytri_,:ssytrs_,:Float32))
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
            n = chksquare(A)
            @chkuplo
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(syconv)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &uplo, &'C', &n, A, &max(1,stride(A,2)), ipiv, work, info)
            @lapackerror
            A, work
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
            n = chksquare(A)
            @chkuplo
            if n != size(B,1) throw(DimensionMismatch("sysv!")) end
            ipiv  = Array(BlasInt, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(sysv)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)),
                      work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            B, A, ipiv
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
            n = chksquare(A)
            @chkuplo
            ipiv  = Array(BlasInt, n)
            n==0 && return A, ipiv
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(sytrf)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, A, &stride(A,2), ipiv, work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            return A, ipiv
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
#             n = chksquare(A)
#             @chkuplo
#             work  = Array($elty, 1)
#             lwork = blas_int(-1)
#             info  = Array(BlasInt, 1)
#             for i in 1:2
#                 ccall(($(string(sytri)),liblapack), Void,
#                       (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
#                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
#                       &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
#                 @assertargsok
#                 @assertnonsingular
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
            n = chksquare(A)
            @chkuplo
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(sytri)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, info)
            @assertargsok
            @assertnonsingular
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
            n = chksquare(A)
            @chkuplo
            if n != size(B,1) throw(DimensionMismatch("sytrs!")) end
            info  = Array(BlasInt, 1)
            ccall(($(string(sytrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
    end
end
## (SY) hermitian matrices - eigendecomposition, Bunch-Kaufman decomposition,
## solvers (direct and factored) and inverse.
for (syconv, hesv, hetrf, hetri, hetrs, elty, relty) in
    ((:zsyconv_,:zhesv_,:zhetrf_,:zhetri_,:zhetrs_,:Complex128, :Float64),
     (:csyconv_,:chesv_,:chetrf_,:chetri_,:chetrs_,:Complex64, :Float32))
    @eval begin
   #   SUBROUTINE ZSYCONV( UPLO, WAY, N, A, LDA, IPIV, WORK, INFO )
   # 22 * 
   # 23 *       .. Scalar Arguments ..
   # 24 *       CHARACTER          UPLO, WAY
   # 25 *       INTEGER            INFO, LDA, N
   # 26 *       ..
   # 27 *       .. Array Arguments ..
   # 28 *       INTEGER            IPIV( * )
   # 29 *       COMPLEX*16         A( LDA, * ), WORK( * )
        function syconv!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            @chkuplo
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(syconv)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &uplo, &'C', &n, A, &max(1,stride(A,2)), ipiv, work, info)
            @lapackerror
            A, work
        end
#       SUBROUTINE ZHESV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LDB, LWORK, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), B( LDB, * ), WORK( * )
        function hesv!(uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            @chkuplo
            if n != size(B,1) throw(DimensionMismatch("hesv!")) end
            ipiv  = Array(BlasInt, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(hesv)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)),
                      work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            B, A, ipiv
        end
#       SUBROUTINE ZHETRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
        function hetrf!(uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            @chkuplo
            ipiv  = Array(BlasInt, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(hetrf)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, ipiv
        end
#       SUBROUTINE ZHETRI2( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
#         function hetri!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
#             chkstride1(A)
#             n = chksquare(A)
#             @chkuplo
#             work  = Array($elty, 1)
#             lwork = blas_int(-1)
#             info  = Array(BlasInt, 1)
#             for i in 1:2
#                 ccall(($(string(hetri)),liblapack), Void,
#                       (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
#                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
#                       &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
#                 @lapackerror
#                 if lwork < 0
#                     lwork = blas_int(real(work[1]))
#                     work = Array($elty, lwork)
#                 end
#             end
#             A
#         end
#       SUBROUTINE ZHETRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
        function hetri!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            @chkuplo
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(hetri)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, info)
            @lapackerror
            A
        end
#       SUBROUTINE ZHETRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), B( LDB, * )
        function hetrs!(uplo::BlasChar, A::StridedMatrix{$elty},
                       ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            if n != size(B,1) throw(DimensionMismatch("hetrs!")) end
            info  = Array(BlasInt, 1)
            ccall(($(string(hetrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)), info)
            @lapackerror 
            B
        end
    end
end
for (sysv, sytrf, sytri, sytrs, elty, relty) in
    ((:zsysv_,:zsytrf_,:zsytri_,:zsytrs_,:Complex128, :Float64),
     (:csysv_,:csytrf_,:csytri_,:csytrs_,:Complex64, :Float32))
    @eval begin
#       SUBROUTINE ZSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
#      $                  LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LDB, LWORK, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), B( LDB, * ), WORK( * )
        function sysv!(uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            @chkuplo
            if n != size(B,1) throw(DimensionMismatch("sysv!")) end
            ipiv  = Array(BlasInt, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(sysv)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)),
                      work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            B, A, ipiv
        end
#       SUBROUTINE ZSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
        function sytrf!(uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            @chkuplo
            ipiv  = Array(BlasInt, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(sytrf)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, ipiv
        end
#       SUBROUTINE ZSYTRI2( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
#         function sytri!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
#             chkstride1(A)
#             n = chksquare(A)
#             @chkuplo
#             work  = Array($elty, 1)
#             lwork = blas_int(-1)
#             info  = Array(BlasInt, 1)
#             for i in 1:2
#                 ccall(($(string(sytri)),liblapack), Void,
#                       (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
#                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
#                       &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
#                 @lapackerror
#                 if lwork < 0
#                     lwork = blas_int(real(work[1]))
#                     work = Array($elty, lwork)
#                 end
#             end
#             A
#         end
#       SUBROUTINE ZSYTRI( UPLO, N, A, LDA, IPIV, WORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
        function sytri!(uplo::BlasChar, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            @chkuplo
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(string(sytri)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                  &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, info)
            @lapackerror
            A
        end
#       SUBROUTINE ZSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       COMPLEX*16         A( LDA, * ), B( LDB, * )
        function sytrs!(uplo::BlasChar, A::StridedMatrix{$elty},
                       ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            @chkuplo
            if n != size(B,1) throw(DimensionMismatch("sytrs!")) end
            info  = Array(BlasInt, 1)
            ccall(($(string(sytrs)),liblapack), Void,
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
    end
end

# Symmetric (real) eigensolvers
for (syev, syevr, sygvd, elty) in
    ((:dsyev_,:dsyevr_,:dsygvd_,:Float64),
     (:ssyev_,:ssyevr_,:ssygvd_,:Float32))
    @eval begin
        #       SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       CHARACTER          JOBZ, UPLO
        #       INTEGER            INFO, LDA, LWORK, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
        function syev!(jobz::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            W     = Array($elty, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(syev)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                      Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &jobz, &uplo, &n, A, &max(1,stride(A,2)), W, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            jobz=='V' ? (W, A) : W
        end
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
        function syevr!(jobz::BlasChar, range::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, vl::FloatingPoint, vu::FloatingPoint, il::Integer, iu::Integer, abstol::FloatingPoint)
            chkstride1(A)
            n = chksquare(A)                   
            lda = max(1,stride(A,2))
            m = Array(BlasInt, 1)
            w = Array($elty, n)
            if jobz == 'N'
                ldz = 1
                Z = Array($elty, ldz, 0)
            elseif jobz == 'V'
                ldz = max(1,n)
                Z = Array($elty, ldz, n)
            end
            isuppz = Array(BlasInt, 2*n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            iwork = Array(BlasInt, 1)
            liwork = blas_int(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(syevr)),liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, 
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
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            w[1:m[1]], Z[:,1:(jobz == 'V' ? m[1] : 0)]
        end    
        syevr!(jobz::BlasChar, A::StridedMatrix{$elty}) = syevr!(jobz, 'A', 'U', A, 0.0, 0.0, 0, 0, -1.0)   
        # Generalized eigenproblem
#           SUBROUTINE DSYGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,
#      $                   LWORK, IWORK, LIWORK, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, UPLO
#       INTEGER            INFO, ITYPE, LDA, LDB, LIWORK, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IWORK( * )
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), W( * ), WORK( * )
        function sygvd!(itype::Integer, jobz::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A, B)
            n, m = chksquare(A, B)
            n==m || throw(DimensionMismatch("Matrices must have same size"))
            lda = ldb = max(1, n)
            w = Array($elty, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            iwork = Array(BlasInt, 1)
            liwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(sygvd)),liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{BlasInt}, Ptr{BlasInt}),
                    &itype, &jobz, &uplo, &n, 
                    A, &lda, B, &ldb, 
                    w, work, &lwork, iwork, 
                    &liwork, info)
                if i == 1
                    lwork = blas_int(work[1])
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            @assertargsok
            @assertnonsingular
            w, A, B
        end
    end
end
# Hermitian eigensolvers
for (syev, syevr, sygvd, elty, relty) in 
    ((:zheev_,:zheevr_,:zhegvd_,:Complex128,:Float64),
     (:cheev_,:cheevr_,:chegvd_,:Complex64,:Float32))
    @eval begin
# SUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO )        
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, UPLO
#       INTEGER            INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   RWORK( * ), W( * )
#       COMPLEX*16         A( LDA, * ), WORK( * )
        function syev!(jobz::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            W     = Array($relty, n)
            work  = Array($elty, 1)
            lwork = blas_int(-1)
            rwork = Array($relty, max(1, 3n-2))
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(string(syev)),liblapack), Void,
                      (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                      Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                      &jobz, &uplo, &n, A, &stride(A,2), W, work, &lwork, rwork, info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            jobz=='V' ? (W, A) : W
        end
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
        function syevr!(jobz::BlasChar, range::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, vl::FloatingPoint, vu::FloatingPoint, il::Integer, iu::Integer, abstol::FloatingPoint)
            chkstride1(A)
            n = chksquare(A)
            lda = max(1,stride(A,2))
            m = Array(BlasInt, 1)
            w = Array($relty, n)
            if jobz == 'N'
                ldz = 1
                Z = Array($elty, ldz, 0)
            elseif jobz == 'V'
                ldz = n
                Z = Array($elty, ldz, n)
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
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, 
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
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    lrwork = blas_int(rwork[1])
                    rwork = Array($relty, lrwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            w[1:m[1]], Z[:,1:(jobz == 'V' ? m[1] : 0)]
        end
        syevr!(jobz::BlasChar, A::StridedMatrix{$elty}) = syevr!(jobz, 'A', 'U', A, 0.0, 0.0, 0, 0, -1.0)
#       SUBROUTINE ZHEGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,
#      $                   LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
# *
# *  -- LAPACK driver routine (version 3.3.1) --
# *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
# *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
# *  -- April 2011                                                      --
# *
# *     .. Scalar Arguments ..
#       CHARACTER          JOBZ, UPLO
#       INTEGER            INFO, ITYPE, LDA, LDB, LIWORK, LRWORK, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IWORK( * )
#       DOUBLE PRECISION   RWORK( * ), W( * )
#       COMPLEX*16         A( LDA, * ), B( LDB, * ), WORK( * )
        function sygvd!(itype::Integer, jobz::BlasChar, uplo::BlasChar, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A, B)
            n, m = chksquare(A, B)
            n==m || throw(DimensionMismatch("Matrices must have same size"))
            lda = ldb = max(1, n)
            w = Array($relty, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            iwork = Array(BlasInt, 1)
            liwork = -one(BlasInt)
            rwork = Array($relty)
            lrwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(sygvd)),liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty},
                     Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &itype, &jobz, &uplo, &n, 
                    A, &lda, B, &ldb, 
                    w, work, &lwork, rwork, 
                    &lrwork, iwork, &liwork, info)
                if i == 1
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                    lrwork = blas_int(rwork[1])
                    rwork = Array($relty, lrwork)
                end
            end
            @assertargsok
            @assertnonsingular
            w, A, B
        end
    end
end

## (BD) Bidiagonal matrices - singular value decomposition
for (bdsqr, relty, elty) in
    ((:dbdsqr_,:Float64,:Float64),
     (:sbdsqr_,:Float32,:Float32),
     (:zbdsqr_,:Float64,:Complex128),
     (:cbdsqr_,:Float32,:Complex64))
    @eval begin
        #*> DBDSQR computes the singular values and, optionally, the right and/or
        #*> left singular vectors from the singular value decomposition (SVD) of
        #*> a real N-by-N (upper or lower) bidiagonal matrix B using the implicit
        #*> zero-shift QR algorithm.
        function bdsqr!(uplo::BlasChar, d::Vector{$relty}, e_::Vector{$relty},
            vt::StridedMatrix{$elty}, u::StridedMatrix{$elty}, c::StridedMatrix{$elty})
            @chkuplo
            n = length(d)
            if length(e_) != n-1 throw(DimensionMismatch("bdsqr!")) end
            ncvt, nru, ncc = size(vt, 2), size(u, 1), size(c, 2)
            ldvt, ldu, ldc = max(1,stride(vt,2)), max(1,stride(u,2)), max(1,stride(c,2))
            work = Array($elty, 4n)
            info = Array(BlasInt,1)

            ccall(($(string(bdsqr)),liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}), 
                &uplo, &n, ncvt, &nru, &ncc,
                d, e_, vt, &ldvt, u,
                &ldu, c, &ldc, work, info)

            @lapackerror
            d, vt, u, c #singular values in descending order, P**T * VT, U * Q, Q**T * C
        end
   end
end

#Defined only for real types
for (bdsdc, elty) in
    ((:dbdsdc_,:Float64),
     (:sbdsdc_,:Float32))
    @eval begin
        #*  DBDSDC computes the singular value decomposition (SVD) of a real
        #*  N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
        #*  using a divide and conquer method
        #*     .. Scalar Arguments ..
        #      CHARACTER          COMPQ, UPLO
        #      INTEGER            INFO, LDU, LDVT, N
        #*     ..
        #*     .. Array Arguments ..
        #      INTEGER            IQ( * ), IWORK( * )
        #      DOUBLE PRECISION   D( * ), E( * ), Q( * ), U( LDU, * ),
        #     $                   VT( LDVT, * ), WORK( * )
        function bdsdc!(uplo::BlasChar, compq::BlasChar, d::Vector{$elty}, e_::Vector{$elty})
            n, ldiq, ldq, ldu, ldvt = length(d), 1, 1, 1, 1
            @chkuplo
            if compq == 'N'
                lwork = 6n
            elseif compq == 'P'
                warn("COMPQ='P' is not tested")
                #TODO turn this into an actual LAPACK call
                #smlsiz=ilaenv(9, $elty==:Float64 ? 'dbdsqr' : 'sbdsqr', string(uplo, compq), n,n,n,n)
                smlsiz=100 #For now, completely overkill
                ldq = n*(11+2*smlsiz+8*int(log((n/(smlsiz+1)))/log(2)))
                ldiq = n*(3+3*int(log(n/(smlsiz+1))/log(2)))
                lwork = 6n
            elseif compq == 'I'
                ldvt=ldu=max(1, n)
                lwork=3*n^2 + 4n
            else
                error(string("COMPQ argument must be 'N', 'P' or 'I' but you said '",compq,"'"))
            end
            u = Array($elty, (ldu,  n))
            vt= Array($elty, (ldvt, n))
            q = Array($elty, ldq)
            iq= Array(BlasInt, ldiq)
            work =Array($elty, lwork)
            iwork=Array(BlasInt, 8n)
            info =Array(BlasInt, 1)
            ccall(($(string(bdsdc)),liblapack), Void,
           (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
            Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
            Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
            &uplo, &compq, &n, d, e_,
            u, &ldu, vt, &ldvt,
            q, iq, work, iwork, info)

            @lapackerror
            compq=='N' ? d : (compq=='P' ? (d, q, iq) : (u, d, vt')) 
        end
    end
end

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
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, 
                   Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}),
                  &normtype, &n, A, &lda, &anorm, rcond, work, iwork,
                  info)
            @lapackerror
            rcond[1]
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
                  (Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, 
                   Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{$relty},
                   Ptr{BlasInt}),
                  &normtype, &n, A, &lda, &anorm, rcond, work, rwork,
                  info)
            @lapackerror
            rcond[1]
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
        function gehrd!(ilo::Integer, ihi::Integer, A::StridedMatrix{$elty})
#                 .. Scalar Arguments ..
#       INTEGER            IHI, ILO, INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION  A( LDA, * ), TAU( * ), WORK( * )
            chkstride1(A)
            n = chksquare(A)
            tau = Array($elty, max(0,n - 1))
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
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, tau
        end
    end
end
gehrd!(A::StridedMatrix) = gehrd!(1, size(A, 1), A)

# construct Q from Hessenberg
for (orghr, elty) in
    ((:dorghr_,:Float64),
     (:sorghr_,:Float32),
     (:zunghr_,:Complex128),
     (:cunghr_,:Complex64))
    @eval begin
        function orghr!(ilo::Integer, ihi::Integer, A::StridedMatrix{$elty}, tau::StridedVector{$elty})
# *     .. Scalar Arguments ..
#       INTEGER            IHI, ILO, INFO, LDA, LWORK, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
            chkstride1(A)
            n = chksquare(A)
            if n - length(tau) != 1 throw(DimensionMismatch("")) end
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
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A
        end
    end
end
# Schur forms
for (gees, gges, elty) in
    ((:dgees_,:dgges_,:Float64),
     (:sgees_,:sgges_,:Float32))
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
            n = chksquare(A)
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
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{Void}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{Void}, Ptr{BlasInt}),
                    &jobvs, &'N', [], &n, 
                        A, &max(1, n), sdim, wr,
                        wi, vs, &ldvs, work, 
                        &lwork, [], info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, vs, all(wi .== 0) ? wr : complex(wr, wi)
        end
        function gges!(jobvsl::Char, jobvsr::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
# *     .. Scalar Arguments ..
#       CHARACTER          JOBVSL, JOBVSR, SORT
#       INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N, SDIM
# *     ..
# *     .. Array Arguments ..
#       LOGICAL            BWORK( * )
#       DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
#      $                   B( LDB, * ), BETA( * ), VSL( LDVSL, * ),
#      $                   VSR( LDVSR, * ), WORK( * )
            chkstride1(A, B)
            n, m = chksquare(A, B)
            n==m || throw(DimensionMismatch("matrices are not of same size"))
            sdim = blas_int(0)
            alphar = Array($elty, n)
            alphai = Array($elty, n)
            beta = Array($elty, n)
            ldvsl = jobvsl == 'V' ? n : 1
            vsl = Array($elty, ldvsl, n)
            ldvsr = jobvsr == 'V' ? n : 1
            vsr = Array($elty, ldvsr, n)
            work = Array($elty, 1)
            lwork = blas_int(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gges)), liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{Void},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{Void},
                        Ptr{BlasInt}),
                    &jobvsl, &jobvsr, &'N', [], 
                    &n, A, &max(1,n), B, 
                    &max(1,n), &sdim, alphar, alphai, 
                    beta, vsl, &ldvsl, vsr, 
                    &ldvsr, work, &lwork, [], 
                    info)
                if i == 1
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            A, B, complex(alphar, alphai), beta, vsl[1:(jobvsl == 'V' ? n : 0),:], vsr[1:(jobvsr == 'V' ? n : 0),:]
        end
    end
end
for (gees, gges, elty, relty) in
    ((:zgees_,:zgges_,:Complex128,:Float64),
     (:cgees_,:cgges_,:Complex64,:Float32))
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
            n = chksquare(A)
            sort = 'N'
            sdim = blas_int(0)
            w = Array($elty, n)
            ldvs = jobvs == 'V' ? n : 1
            vs = Array($elty, ldvs, n)
            work = Array($elty, 1)
            lwork = blas_int(-1)
            rwork = Array($relty, n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gees)),liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{Void}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, 
                        Ptr{$relty}, Ptr{Void}, Ptr{BlasInt}),
                    &jobvs, &sort, [], &n, 
                        A, &max(1, n), &sdim, w,
                        vs, &ldvs, work, &lwork, 
                        rwork, [], info)
                @lapackerror
                if lwork < 0
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, vs, w
        end
        function gges!(jobvsl::Char, jobvsr::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
# *     .. Scalar Arguments ..
#       CHARACTER          JOBVSL, JOBVSR, SORT
#       INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N, SDIM
# *     ..
# *     .. Array Arguments ..
#       LOGICAL            BWORK( * )
#       DOUBLE PRECISION   RWORK( * )
#       COMPLEX*16         A( LDA, * ), ALPHA( * ), B( LDB, * ),
#      $                   BETA( * ), VSL( LDVSL, * ), VSR( LDVSR, * ),
#      $                   WORK( * )
            chkstride1(A, B)
            n, m = chksquare(A, B)
            n==m || throw(DimensionMismatch("matrices are not of same size"))
            sdim = blas_int(0)
            alpha = Array($elty, n)
            beta = Array($elty, n)
            ldvsl = jobvsl == 'V' ? n : 1
            vsl = Array($elty, ldvsl, n)
            ldvsr = jobvsr == 'V' ? n : 1
            vsr = Array($elty, ldvsr, n)
            work = Array($elty, 1)
            lwork = blas_int(-1)
            rwork = Array($relty, 8n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(string(gges)), liblapack), Void,
                    (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{Void},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{Void},
                        Ptr{BlasInt}),
                    &jobvsl, &jobvsr, &'N', [], 
                    &n, A, &max(1,n), B, 
                    &max(1,n), &sdim, alpha, beta, 
                    vsl, &ldvsl, vsr, &ldvsr, 
                    work, &lwork, rwork, [], 
                    info)
                if i == 1
                    lwork = blas_int(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            A, B, alpha, beta, vsl[1:(jobvsl == 'V' ? n : 0),:], vsr[1:(jobvsr == 'V' ? n : 0),:]
        end
    end
end

### Rectangular full packed format

# Symmetric rank-k operation for matrix in RFP format.
for (fn, elty, relty) in ((:dsfrk_, :Float64, :Float64),
                   (:ssfrk_, :Float32, :Float32),
                   (:zhfrk_, :Complex128, :Float64),
                   (:chfrk_, :Complex64, :Float32))
    @eval begin
        function sfrk!(transr::Char, uplo::Char, trans::Char, alpha::Real, A::StridedMatrix{$elty}, beta::Real, C::StridedVector{$elty})
            chkstride1(A)
            if trans == 'N' || trans == 'n'
                n, k = size(A)
            elseif trans == 'T' || trans == 't'
                k, n = size(A)
            end
            lda = max(1, stride(A, 2))
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$relty}, Ptr{$elty}),
                &transr, &uplo, &trans, &n,
                &k, &alpha, A, &lda,
                &beta, C)
            C
        end
    end
end

# Cholesky factorization of a real symmetric positive definite matrix A
for (fn, elty) in ((:dpftrf_, :Float64),
                   (:spftrf_, :Float32),
                   (:zpftrf_, :Complex128),
                   (:cpftrf_, :Complex64))
    @eval begin
        function pftrf!(transr::Char, uplo::Char, A::StridedVector{$elty})
            n = int(div(sqrt(8length(A)), 2))
            info = Array(BlasInt, 1)
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}),
                &transr, &uplo, &n, A,
                info)
            @assertargsok
            @assertnonsingular
            A
        end
    end
end

# Computes the inverse of a (real) symmetric positive definite matrix A using the Cholesky factorization
for (fn, elty) in ((:dpftri_, :Float64),
                   (:spftri_, :Float32),
                   (:zpftri_, :Complex128),
                   (:cpftri_, :Complex64))
    @eval begin
        function pftri!(transr::Char, uplo::Char, A::StridedVector{$elty})
            n = int(div(sqrt(8length(A)), 2))
            info = Array(BlasInt, 1)
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}),
                &transr, &uplo, &n, A, 
                info)
            @assertargsok
            @assertnonsingular
            A
        end
    end
end

# DPFTRS solves a system of linear equations A*X = B with a symmetric positive definite matrix A using the Cholesky factorization
for (fn, elty) in ((:dpftrs_, :Float64),
                   (:spftrs_, :Float32),
                   (:zpftrs_, :Complex128),
                   (:cpftrs_, :Complex64))
    @eval begin
        function pftrs!(transr::Char, uplo::Char, A::StridedVector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            n = int(div(sqrt(8length(A)), 2))
            if n != size(B, 1) throw(DimensionMismatch("arguments must have the same number of rows")) end
            nhrs = size(B, 2)
            ldb = max(1, stride(B, 2))
            info = Array(BlasInt, 1)
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                &transr, &uplo, &n, &nhrs,
                A, B, &ldb, info)
            @assertargsok
            @assertposdef
            B
        end
    end
end

# Solves a matrix equation (one operand is a triangular matrix in RFP format)
for (fn, elty) in ((:dtfsm_, :Float64),
                   (:stfsm_, :Float32),
                   (:ztfsm_, :Complex128),
                   (:ctfsm_, :Complex64))
    @eval begin
        function pftrs!(transr::Char, side::Char, uplo::Char, trans::Char, diag::Char, alpha::Real, A::StridedVector{$elty}, B::StridedMatrix{$elty})
            chkstride1(B)
            m, n = size(B)
            if int(div(sqrt(8length(A)), 2)) != m throw(DimensionMismatch("")) end
            ldb = max(1, stride(B, 2))
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar},
                 Ptr{BlasChar}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}),
                &transr, &side, &uplo, &trans,
                &diag, &m, &n, &alpha,
                A, B, &ldb)
            B
        end
    end
end

# Computes the inverse of a triangular matrix A stored in RFP format.
for (fn, elty) in ((:dtftri_, :Float64),
                   (:stftri_, :Float32),
                   (:ztftri_, :Complex128),
                   (:ctftri_, :Complex64))
    @eval begin
        function tftri!(transr::Char, uplo::Char, diag::Char, A::StridedVector{$elty})
            n = int(div(sqrt(8length(A)), 2))
            info = Array(BlasInt, 1)
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, 
                 Ptr{$elty}, Ptr{BlasInt}),
                &transr, &uplo, &diag, &n, 
                A, info)
            @assertargsok
            @assertnonsingular
            A
        end
    end
end

# Copies a triangular matrix from the rectangular full packed format (TF) to the standard full format (TR)
for (fn, elty) in ((:dtfttr_, :Float64),
                   (:stfttr_, :Float32),
                   (:ztfttr_, :Complex128),
                   (:ctfttr_, :Complex64))
    @eval begin
        function tfttr!(transr::Char, uplo::Char, Arf::StridedVector{$elty})
            n = int(div(sqrt(8length(Arf)), 2))
            info = Array(BlasInt, 1)
            A = Array($elty, n, n)
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                &transr, &uplo, &n, Arf,
                A, &n, info)
            @assertargsok
            A
        end
    end
end

# Copies a triangular matrix from the standard full format (TR) to the rectangular full packed format (TF).
for (fn, elty) in ((:dtrttf_, :Float64),
                   (:strttf_, :Float32),
                   (:ztrttf_, :Complex128),
                   (:ctrttf_, :Complex64))
    @eval begin
        function trttf!(transr::Char, uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = size(A, 1)
            lda = max(1, stride(A, 2))
            info = Array(BlasInt, 1)
            Arf = Array($elty, div(n*(n+1), 2))
            ccall(($(string(fn)), liblapack), Void,
                (Ptr{BlasChar}, Ptr{BlasChar}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}),
                &transr, &uplo, &n, A,
                &lda, Arf, info)
            @assertargsok
            Arf
        end
    end
end

end # module
