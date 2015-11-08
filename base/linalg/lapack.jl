# This file is a part of Julia. License is MIT: http://julialang.org/license

## The LAPACK module of interfaces to LAPACK subroutines
module LAPACK

const liblapack = Base.liblapack_name

import Base.blasfunc

import ..LinAlg: BlasFloat, Char, BlasInt, LAPACKException,
    DimensionMismatch, SingularException, PosDefException, chkstride1, chksquare

#Generic LAPACK error handlers
macro assertargsok() #Handle only negative info codes - use only if positive info code is useful!
    :(info[1]<0 && throw(ArgumentError("invalid argument #$(-info[1]) to LAPACK call")))
end
macro lapackerror() #Handle all nonzero info codes
    :(info[1]>0 ? throw(LAPACKException(info[1])) : @assertargsok )
end

macro assertnonsingular()
    :(info[1]>0 && throw(SingularException(info[1])))
end
macro assertposdef()
    :(info[1]>0 && throw(PosDefException(info[1])))
end

#Check that upper/lower (for special matrices) is correctly specified
function chkuplo(uplo::Char)
    (uplo=='U' || uplo=='L') ||
      throw(ArgumentError(string("uplo argument must be 'U' (upper) or 'L' (lower), got $uplo")))
end

#Check that {c}transpose is correctly specified
function chktrans(trans::Char)
    (trans=='N' || trans=='C' || trans=='T') ||
      throw(ArgumentError(string("trans argument must be 'N' (no transpose), 'T' (transpose), or 'C' (conjugate transpose), got $trans")))
end

#Check that left/right hand side multiply is correctly specified
function chkside(side::Char)
    (side=='L' || side=='R') ||
      throw(ArgumentError(string("side argument must be 'L' (left hand multiply) or 'R' (right hand multiply), got $side")))
end

#Check that unit diagonal flag is correctly specified
function chkdiag(diag::Char)
    (diag=='U' || diag=='N') ||
      throw(ArgumentError(string("diag argument must be 'U' (unit diagonal) or 'N' (non-unit diagonal), got $diag")))
end

subsetrows(X::AbstractVector, Y::AbstractArray, k) = Y[1:k]
subsetrows(X::AbstractMatrix, Y::AbstractArray, k) = Y[1:k, :]

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
            ipiv = similar(AB, BlasInt, mnmn)
            ccall(($(blasfunc(gbtrf)), liblapack), Void,
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
        function gbtrs!(trans::Char, kl::Integer, ku::Integer, m::Integer,
                        AB::StridedMatrix{$elty}, ipiv::Vector{BlasInt},
                        B::StridedVecOrMat{$elty})
            chkstride1(AB, B)
            chktrans(trans)
            info = Array(BlasInt, 1)
            n    = size(AB,2)
            if m != n || m != size(B,1)
                throw(DimensionMismatch("Matrix AB has dimensions $(size(AB)), but right hand side matrix B has dimensions $(size(B))"))
            end
            ccall(($(blasfunc(gbtrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},   Ptr{BlasInt},
                   Ptr{BlasInt}),
                  &trans, &n, &kl, &ku, &size(B,2), AB, &max(1,stride(AB,2)), ipiv,
                  B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
    end
end

"""
    gbtrf!(kl, ku, m, AB) -> (AB, ipiv)

Compute the LU factorization of a banded matrix `AB`. `kl` is the first
subdiagonal containing a nonzero band, `ku` is the last superdiagonal
containing one, and `m` is the first dimension of the matrix `AB`. Returns
the LU factorization in-place and `ipiv`, the vector of pivots used.
"""
gbtrf!(kl::Integer, ku::Integer, m::Integer, AB::StridedMatrix)

"""
    gbtrs!(trans, kl, ku, m, AB, ipiv, B)

Solve the equation `AB * X = B`. `trans` determines the orientation of `AB`. It may
be `N` (no transpose), `T` (transpose), or `C` (conjugate transpose). `kl` is the
first subdiagonal containing a nonzero band, `ku` is the last superdiagonal
containing one, and `m` is the first dimension of the matrix `AB`. `ipiv` is the vector
of pivots returned from `gbtrf!`. Returns the vector or matrix `X`, overwriting `B` in-place.
"""
gbtrs!(trans::Char, kl::Integer, ku::Integer, m::Integer, AB::StridedMatrix, ipiv::Vector{BlasInt}, B::StridedVecOrMat)

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
        function gebal!(job::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            info    = Array(BlasInt, 1)
            ihi     = Array(BlasInt, 1)
            ilo     = Array(BlasInt, 1)
            scale   = similar(A, $relty, n)
            ccall(($(blasfunc(gebal)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function gebak!(job::Char, side::Char,
                        ilo::BlasInt, ihi::BlasInt, scale::Vector{$relty},
                        V::StridedMatrix{$elty})
            chkstride1(V)
            chkside(side)
            n = chksquare(V)
            info    = Array(BlasInt, 1)
            ccall(($(blasfunc(gebak)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$relty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &job, &side, &size(V,1), &ilo, &ihi, scale, &n, V, &max(1,stride(V,2)), info)
            @lapackerror
            V
        end
    end
end

"""
    gebal!(job, A) -> (ilo, ihi, scale)

Balance the matrix `A` before computing its eigensystem or Schur factorization.
`job` can be one of `N` (`A` will not be permuted or scaled), `P` (`A` will only
be permuted), `S` (`A` will only be scaled), or `B` (`A` will be both permuted
and scaled). Modifies `A` in-place and returns `ilo`, `ihi`, and `scale`. If
permuting was turned on, `A[i,j] = 0` if `j > i` and `1 < j < ilo` or `j > ihi`.
`scale` contains information about the scaling/permutations performed.
"""
gebal!(job::Char, A::StridedMatrix)

"""
    gebak!(job, side, ilo, ihi, scale, V)

Transform the eigenvectors `V` of a matrix balanced using `gebal!` to
the unscaled/unpermuted eigenvectors of the original matrix. Modifies `V`
in-place. `side` can be `L` (left eigenvectors are transformed) or `R`
(right eigenvectors are transformed).
"""
gebak!(job::Char, side::Char, ilo::BlasInt, ihi::BlasInt, scale::Vector, V::StridedMatrix)

# (GE) general matrices, direct decompositions
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
            d     = similar(A, $relty, k)
            e     = similar(A, $relty, k)
            tauq  = similar(A, $elty, k)
            taup  = similar(A, $elty, k)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(gebrd)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{$elty},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                     &m, &n, A, &max(1,stride(A,2)),
                     d, e, tauq, taup,
                     work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, d, e, tauq, taup
        end

        # SUBROUTINE DGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function gelqf!(A::StridedMatrix{$elty}, tau::Vector{$elty})
            chkstride1(A)
            info  = Array(BlasInt, 1)
            m     = BlasInt(size(A, 1))
            n     = BlasInt(size(A, 2))
            lda   = BlasInt(max(1,stride(A, 2)))
            if length(tau) != min(m,n)
                throw(DimensionMismatch("tau has length $(length(tau)), but needs length $(min(m,n))"))
            end
            lwork = BlasInt(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(blasfunc(gelqf)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &lda, tau, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
            m     = BlasInt(size(A, 1))
            n     = BlasInt(size(A, 2))
            lda   = BlasInt(max(1,stride(A, 2)))
            if length(tau) != min(m,n)
                throw(DimensionMismatch("tau has length $(length(tau)), but needs length $(min(m,n))"))
            end
            lwork = BlasInt(-1)
            work  = Array($elty, (1,))
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(blasfunc(geqlf)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &lda, tau, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
            if length(tau) != min(m,n)
                throw(DimensionMismatch("tau has length $(length(tau)), but needs length $(min(m,n))"))
            end
            if length(jpvt) != n
                throw(DimensionMismatch("jpvt has length $(length(jpvt)), but needs length $n"))
            end
            lda   = stride(A,2)
            if lda == 0
                return A, tau, jpvt
            end # Early exit
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            cmplx = eltype(A)<:Complex
            if cmplx; rwork = Array($relty, 2n); end
            for i in 1:2
                if cmplx
                    ccall(($(blasfunc(geqp3)), liblapack), Void,
                          (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}),
                          &m, &n, A, &lda,
                          jpvt, tau, work, &lwork,
                          rwork, info)
                else
                    ccall(($(blasfunc(geqp3)), liblapack), Void,
                          (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}),
                          &m, &n, A, &lda,
                          jpvt, tau, work,
                          &lwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            return A, tau, jpvt
        end

        function geqrt!(A::StridedMatrix{$elty}, T::StridedMatrix{$elty})
            chkstride1(A)
            m, n = size(A)
            minmn = min(m, n)
            nb = size(T, 1)
            if nb > minmn
                throw(ArgumentError("Block size $nb > $minmn too large"))
            end
            lda = max(1, stride(A,2))
            work = Array($elty, nb*n)
            if n > 0
                info = Array(BlasInt, 1)
                ccall(($(blasfunc(geqrt)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}),
                     &m, &n, &nb, A,
                     &lda, T, &max(1,stride(T,2)), work,
                     info)
                @lapackerror
            end
            A, T
        end

        function geqrt3!(A::StridedMatrix{$elty}, T::StridedMatrix{$elty})
            chkstride1(A); chkstride1(T)
            m, n = size(A); p, q = size(T)
            if m < n
                throw(DimensionMismatch("Input matrix A has dimensions ($m,$n), but should have more rows than columns"))
            end
            if p != n || q != n
                throw(DimensionMismatch("Block reflector T has dimensions ($p,$q), but should have dimensions ($n,$n)"))
            end
            if n > 0
                info = Array(BlasInt, 1)
                ccall(($(blasfunc(geqrt3)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                     &m, &n, A, &max(1, stride(A, 2)),
                     T, &max(1,stride(T,2)), info)
                @lapackerror
            end
            A, T
        end

        ## geqrfp! - positive elements on diagonal of R - not defined yet
        # SUBROUTINE DGEQRFP( M, N, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function geqrf!(A::StridedMatrix{$elty}, tau::Vector{$elty})
            chkstride1(A)
            m, n  = size(A)
            if length(tau) != min(m,n)
                throw(DimensionMismatch("tau has length $(length(tau)), but needs length $(min(m,n))"))
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(blasfunc(geqrf)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &max(1,stride(A,2)), tau, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
            if length(tau) != min(m,n)
                throw(DimensionMismatch("tau has length $(length(tau)), but needs length $(min(m,n))"))
            end
            lwork = BlasInt(-1)
            work  = Array($elty, 1)
            for i in 1:2                # first call returns lwork as work[1]
                ccall(($(blasfunc(gerqf)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, A, &max(1,stride(A,2)), tau, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
            ipiv = similar(A, BlasInt, min(m,n))
            ccall(($(blasfunc(getrf)), liblapack), Void,
                  (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &m, &n, A, &lda, ipiv, info)
            @assertargsok
            A, ipiv, info[1] #Error code is stored in LU factorization type
        end
    end
end

"""
    gebrd!(A) -> (A, d, e, tauq, taup)

Reduce `A` in-place to bidiagonal form `A = QBP'`. Returns `A`, containing the
bidiagonal matrix `B`; `d`, containing the diagonal elements of `B`; `e`,
containing the off-diagonal elements of `B`; `tauq`, containing the
elementary reflectors representing `Q`; and `taup`, containing the
elementary reflectors representing `P`.
"""
gebrd!(A::StridedMatrix)

"""
    gelqf!(A, tau)

Compute the `LQ` factorization of `A`, `A = LQ`. `tau` contains scalars
which parameterize the elementary reflectors of the factorization. `tau`
must have length greater than or equal to the smallest dimension of `A`.

Returns
`A` and `tau` modified in-place.
"""
gelqf!(A::StridedMatrix, tau::Vector)

"""
    geqlf!(A, tau)

Compute the `QL` factorization of `A`, `A = QL`. `tau` contains scalars
which parameterize the elementary reflectors of the factorization. `tau`
must have length greater than or equal to the smallest dimension of `A`.

Returns `A` and `tau` modified in-place.
"""
geqlf!(A::StridedMatrix, tau::Vector)

"""
    geqp3!(A, jpvt, tau)

Compute the pivoted `QR` factorization of `A`, `AP = QR` using BLAS level 3.
`P` is a pivoting matrix, represented by `jpvt`. `tau` stores the elementary
reflectors. `jpvt` must have length length greater than or equal to `n` if `A`
is an `(m x n)` matrix. `tau` must have length greater than or equal to the
smallest dimension of `A`.

`A`, `jpvt`, and `tau` are modified in-place.
"""
geqp3!(A::StridedMatrix, jpvt::Vector{BlasInt}, tau::Vector)

"""
    geqrt!(A, T)

Compute the blocked `QR` factorization of `A`, `A = QR`. `T` contains upper
triangular block reflectors which parameterize the elementary reflectors of
the factorization. The first dimension of `T` sets the block size and it must
be between 1 and `n`. The second dimension of `T` must equal the smallest
dimension of `A`.

Returns `A` and `T` modified in-place.
"""
geqrt!(A::StridedMatrix, T::StridedMatrix)

"""
    geqrt3!(A, T)

Recursively computes the blocked `QR` factorization of `A`, `A = QR`. `T`
contains upper triangular block reflectors which parameterize the
elementary reflectors of the factorization.  The first dimension of `T` sets the
block size and it must be between 1 and `n`. The second dimension of `T` must
equal the smallest dimension of `A`.

Returns `A` and `T` modified in-place.
"""
geqrt3!(A::StridedMatrix, T::StridedMatrix)

"""
    geqrf!(A, tau)

Compute the `QR` factorization of `A`, `A = QR`. `tau` contains scalars
which parameterize the elementary reflectors of the factorization. `tau`
must have length greater than or equal to the smallest dimension of `A`.

Returns `A` and `tau` modified in-place.
"""
geqrf!(A::StridedMatrix, tau::Vector)

"""
    gerqf!(A, tau)

Compute the `RQ` factorization of `A`, `A = RQ`. `tau` contains scalars
which parameterize the elementary reflectors of the factorization. `tau`
must have length greater than or equal to the smallest dimension of `A`.

Returns `A` and `tau` modified in-place.
"""
gerqf!(A::StridedMatrix, tau::Vector)

"""
    getrf!(A) -> (A, ipiv, info)

Compute the pivoted `LU` factorization of `A`, `A = LU`.

Returns `A`, modified in-place, `ipiv`, the pivoting information, and an `info`
code which indicates success (`info = 0`), a singular value in `U`
(`info = i`, in which case `U[i,i]` is singular), or an error code (`info < 0`).
"""
getrf!(A::StridedMatrix, tau::Vector)

"""
    gelqf!(A) -> (A, tau)

Compute the `LQ` factorization of `A`, `A = LQ`.

Returns `A`, modified in-place, and `tau`, which contains scalars
which parameterize the elementary reflectors of the factorization.
"""
gelqf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); gelqf!(A,similar(A,T,min(m,n))))
"""
    geqlf!(A) -> (A, tau)

Compute the `QL` factorization of `A`, `A = QL`.

Returns `A`, modified in-place, and `tau`, which contains scalars
which parameterize the elementary reflectors of the factorization.
"""
geqlf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); geqlf!(A,similar(A,T,min(m,n))))
"""
    geqrt!(A, nb) -> (A, T)

Compute the blocked `QR` factorization of `A`, `A = QR`. `nb` sets the block size
and it must be between 1 and `n`, the second dimension of `A`.

Returns `A`, modified in-place, and `T`, which contains upper
triangular block reflectors which parameterize the elementary reflectors of
the factorization.
"""
geqrt!{T<:BlasFloat}(A::StridedMatrix{T}, nb::Integer) = geqrt!(A,similar(A,T,nb,minimum(size(A))))
"""
    geqrt3!(A) -> (A, T)

Recursively computes the blocked `QR` factorization of `A`, `A = QR`.

Returns `A`, modified in-place, and `T`, which contains upper triangular block
reflectors which parameterize the elementary reflectors of the factorization.
"""
geqrt3!{T<:BlasFloat}(A::StridedMatrix{T}) = (n=size(A,2); geqrt3!(A,similar(A,T,n,n)))
"""
    geqrf!(A) -> (A, tau)

Compute the `QR` factorization of `A`, `A = QR`.

Returns `A`, modified in-place, and `tau`, which contains scalars
which parameterize the elementary reflectors of the factorization.
"""
geqrf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); geqrf!(A,similar(A,T,min(m,n))))
"""
    gerqf!(A) -> (A, tau)

Compute the `RQ` factorization of `A`, `A = RQ`.

Returns `A`, modified in-place, and `tau`, which contains scalars
which parameterize the elementary reflectors of the factorization.
"""
gerqf!{T<:BlasFloat}(A::StridedMatrix{T}) = ((m,n)=size(A); gerqf!(A,similar(A,T,min(m,n))))

"""
    geqp3!(A, jpvt) -> (A, jpvt, tau)

Compute the pivoted `QR` factorization of `A`, `AP = QR` using BLAS level 3.
`P` is a pivoting matrix, represented by `jpvt`. `jpvt` must have length
greater than or equal to `n` if `A` is an `(m x n)` matrix.

Returns `A` and `jpvt`, modified in-place, and `tau`, which stores the elementary
reflectors.
"""
function geqp3!{T<:BlasFloat}(A::StridedMatrix{T},jpvt::Vector{BlasInt})
    m,n = size(A)
    geqp3!(A,jpvt,similar(A,T,min(m,n)))
end

"""
    geqp3!(A) -> (A, jpvt, tau)

Compute the pivoted `QR` factorization of `A`, `AP = QR` using BLAS level 3.

Returns `A`, modified in-place, `jpvt`, which represents the pivoting matrix `P`,
and `tau`, which stores the elementary reflectors.
"""
function geqp3!{T<:BlasFloat}(A::StridedMatrix{T})
    m,n=size(A)
    geqp3!(A,zeros(BlasInt,n),similar(A,T,min(m,n)))
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
            if n < m
                throw(DimensionMismatch("Input matrix A has dimensions ($m,$n), but cannot have fewer columns than rows"))
            end
            lda = max(1, m)
            tau = similar(A, $elty, m)
            work = Array($elty, 1)
            lwork = -1
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(tzrzf)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &m, &n, A, &lda,
                    tau, work, &lwork, info)
                if i == 1
                    lwork = BlasInt(real(work[1]))
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

        function ormrz!(side::Char, trans::Char, A::StridedMatrix{$elty}, tau::StridedVector{$elty}, C::StridedMatrix{$elty})
            chktrans(trans)
            chkside(side)
            m, n = size(C)
            k = length(tau)
            l = size(A, 2) - size(A, 1)
            lda = max(1, stride(A,2))
            ldc = max(1, stride(C,2))
            work = Array($elty, 1)
            lwork = -1
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(ormrz)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{BlasInt}),
                    &side, &trans, &m, &n,
                    &k, &l, A, &lda,
                    tau, C, &ldc, work,
                    &lwork, info)
                if i == 1
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            C
        end
    end
end

"""
    ormrz!(side, trans, A, tau, C)

Multiplies the matrix `C` by `Q` from the transformation supplied by
`tzrzf!`. Depending on `side` or `trans` the multiplication can be
left-sided (`side = L, Q*C`) or right-sided (`side = R, C*Q`) and `Q`
can be unmodified (`trans = N`), transposed (`trans = T`), or conjugate
transposed (`trans = C`). Returns matrix `C` which is modified in-place
with the result of the multiplication.
"""
ormrz!(side::Char, trans::Char, A::StridedMatrix, tau::StridedVector, C::StridedMatrix)

"""
    tzrzf!(A) -> (A, tau)

Transforms the upper trapezoidal matrix `A` to upper triangular form in-place.
Returns `A` and `tau`, the scalar parameters for the elementary reflectors
of the transformation.
"""
tzrzf!(A::StridedMatrix)

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
        function gels!(trans::Char, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chktrans(trans)
            chkstride1(A, B)
            btrn  = trans == 'T'
            m, n  = size(A)
            if size(B,1) != (btrn ? n : m)
                throw(DimensionMismatch("Matrix A has dimensions ($m,$n), transposed: $btrn, but leading dimension of B is $(size(B,1))"))
            end
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            for i in 1:2
                ccall(($(blasfunc(gels)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &(btrn?'T':'N'), &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      B, &max(1,stride(B,2)), work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            k   = min(m, n)
            F   = m < n ? tril(A[1:k, 1:k]) : triu(A[1:k, 1:k])
            ssr = [begin
                x = zero($elty)
                for j=k+1:size(B,1)
                    x += abs2(B[j, i])
                end
                x
            end for i=1:size(B,2)]
            F, subsetrows(B, B, k), ssr
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
            if size(B,1) != n
                throw(DimensionMismatch("B has leading dimension $(size(B,1)), but needs $n"))
            end
            ipiv    = similar(A, BlasInt, n)
            info    = Array(BlasInt, 1)
            ccall(($(blasfunc(gesv)), liblapack), Void,
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
        function getrs!(trans::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chktrans(trans)
            chkstride1(A, B)
            n = chksquare(A)
            if n != size(B, 1)
                throw(DimensionMismatch("B has leading dimension $(size(B,1)), but needs $n"))
            end
            nrhs = size(B, 2)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(getrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
            if n != length(ipiv)
                throw(DimensionMismatch("ipiv has length $(length(ipiv)), but needs $n"))
            end
            lda     = max(1,stride(A, 2))
            info    = Array(BlasInt, 1)
            lwork   = -1
            work    = Array($elty, 1)
            for i in 1:2
                ccall(($(blasfunc(getri)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &n, A, &lda, ipiv, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            A
        end
    end
end

"""
    gels!(trans, A, B) -> (F, B, ssr)

Solves the linear equation `A * X = B`, `A.' * X =B`, or `A' * X = B` using
a QR or LQ factorization. Modifies the matrix/vector `B` in place with the
solution. `A` is overwritten with its `QR` or `LQ` factorization. `trans`
may be one of `N` (no modification), `T` (transpose), or `C` (conjugate
transpose). `gels!` searches for the minimum norm/least squares solution.
`A` may be under or over determined. The solution is returned in `B`.
"""
gels!(trans::Char, A::StridedMatrix, B::StridedVecOrMat)

"""
    gesv!(A, B) -> (B, A, ipiv)

Solves the linear equation `A * X = B` where `A` is a square matrix using
the `LU` factorization of `A`. `A` is overwritten with its `LU`
factorization and `B` is overwritten with the solution `X`. `ipiv` contains the
pivoting information for the `LU` factorization of `A`.
"""
gesv!(A::StridedMatrix, B::StridedVecOrMat)

"""
    getrs!(trans, A, ipiv, B)

Solves the linear equation `A * X = B`, `A.' * X =B`, or `A' * X = B` for
square `A`. Modifies the matrix/vector `B` in place with the solution. `A`
is the `LU` factorization from `getrf!`, with `ipiv` the pivoting
information. `trans` may be one of `N` (no modification), `T` (transpose),
or `C` (conjugate transpose).
"""
getrs!(trans::Char, A::StridedMatrix, ipiv::Vector{BlasInt}, B::StridedVecOrMat)

"""
    getri!(A, ipiv)

Computes the inverse of `A`, using its `LU` factorization found by
`getrf!`. `ipiv` is the pivot information output and `A`
contains the `LU` factorization of `getrf!`. `A` is overwritten with
its inverse.
"""
getri!(A::StridedMatrix, ipiv::Vector{BlasInt})

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
        function gesvx!(fact::Char, trans::Char, A::StridedMatrix{$elty},
               AF::StridedMatrix{$elty}, ipiv::Vector{BlasInt}, equed::Char,
               R::Vector{$elty}, C::Vector{$elty}, B::StridedVecOrMat{$elty})
            chktrans(trans)
            n   = chksquare(A)
            lda = stride(A,2)
            n   = chksquare(AF)
            ldaf = stride(AF,2)
            nrhs = size(B,2)
            ldb = stride(B,2)
            rcond     = Array($elty, 1)
            ferr      = similar(A, $elty, nrhs)
            berr      = similar(A, $elty, nrhs)
            work      = Array($elty, 4n)
            iwork     = Array(BlasInt, n)
            info      = Array(BlasInt, 1)
            X = similar(A, $elty, n, nrhs)
            ccall(($(blasfunc(gesvx)), liblapack), Void,
              (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{UInt8}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
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
        function gesvx!(A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            n=size(A,1)
            X, equed, R, C, B, rcond, ferr, berr, rpgf = gesvx!('N', 'N', A, similar(A, $elty, n, n), similar(A, BlasInt, n), 'N', similar(A, $elty, n),  similar(A, $elty, n), B)
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
        function gesvx!(fact::Char, trans::Char, A::StridedMatrix{$elty},
             AF::StridedMatrix{$elty}, ipiv::Vector{BlasInt}, equed::Char,
             R::Vector{$relty}, C::Vector{$relty}, B::StridedVecOrMat{$elty})
            chktrans(trans)
            n   = chksquare(A)
            lda = stride(A,2)
            n   = chksquare(AF)
            ldaf = stride(AF,2)
            nrhs = size(B,2)
            ldb = stride(B,2)
            rcond     = Array($relty, 1)
            ferr      = similar(A, $relty, nrhs)
            berr      = similar(A, $relty, nrhs)
            work      = Array($elty, 2n)
            rwork     = Array($relty, 2n)
            info      = Array(BlasInt, 1)
            X = similar(A, $elty, n, nrhs)
            ccall(($(blasfunc(gesvx)), liblapack), Void,
              (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
               Ptr{UInt8}, Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt},
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
            X, equed, R, C, B, rcond, ferr, berr, rpgf = gesvx!('N', 'N', A, similar(A, $elty, n, n), similar(A, BlasInt, n), 'N', similar(A, $relty, n),  similar(A, $relty, n), B)
            X, rcond, ferr, berr, rpgf
        end
    end
end

"""
    gesvx!(fact, trans, A, AF, ipiv, equed, R, C, B) -> (X, equed, R, C, B, rcond, ferr, berr, work)

Solves the linear equation `A * X = B` (`trans = N`), `A.' * X =B`
(`trans = T`), or `A' * X = B` (`trans = C`) using the `LU` factorization
of `A`. `fact` may be `E`, in which case `A` will be equilibrated and copied
to `AF`; `F`, in which case `AF` and `ipiv` from a previous `LU` factorization
are inputs; or `N`, in which case `A` will be copied to `AF` and then
factored. If `fact = F`, `equed` may be `N`, meaning `A` has not been
equilibrated; `R`, meaning `A` was multiplied by `diagm(R)` from the left;
`C`, meaning `A` was multiplied by `diagm(C)` from the right; or `B`, meaning
`A` was multiplied by `diagm(R)` from the left and `diagm(C)` from the right.
If `fact = F` and `equed = R` or `B` the elements of `R` must all be positive.
If `fact = F` and `equed = C` or `B` the elements of `C` must all be positive.

Returns the solution `X`; `equed`, which is an output if `fact` is not `N`,
and describes the equilibration that was performed; `R`, the row equilibration
diagonal; `C`, the column equilibration diagonal; `B`, which may be overwritten
with its equilibrated form `diagm(R)*B` (if `trans = N` and `equed = R,B`) or
`diagm(C)*B` (if `trans = T,C` and `equed = C,B`); `rcond`, the reciprocal
condition number of `A` after equilbrating; `ferr`, the forward error bound for
each solution vector in `X`; `berr`, the forward error bound for each solution
vector in `X`; and `work`, the reciprocal pivot growth factor.
"""
gesvx!(fact::Char, trans::Char, A::StridedMatrix, AF::StridedMatrix,
    ipiv::Vector{BlasInt}, equed::Char, R::Vector, C::Vector, B::StridedVecOrMat)

"""
    gesvx!(A, B)

The no-equilibration, no-transpose simplification of `gesvx!`.
"""
gesvx!(A::StridedMatrix, B::StridedVecOrMat)

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
            if size(B, 1) != m
                throw(DimensionMismatch("B has leading dimension $(size(B,1)) but needs $m"))
            end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            s     = similar(A, $elty, min(m, n))
            rcond = convert($elty, rcond)
            rnk   = Array(BlasInt, 1)
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            iwork = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(gelsd)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      newB, &max(1,stride(B,2),n), s, &rcond, rnk, work, &lwork, iwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                    iwork = Array(BlasInt, iwork[1])
                end
            end
            subsetrows(B, newB, n), rnk[1]
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
            if size(B, 1) != m
                throw(DimensionMismatch("B has leading dimension $(size(B,1)) but needs $m"))
            end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            lda = max(1, m)
            ldb = max(1, m, n)
            jpvt = zeros(BlasInt, n)
            rcond = convert($elty, rcond)
            rnk = Array(BlasInt, 1)
            work = Array($elty, 1)
            lwork = -1
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(gelsy)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{BlasInt}),
                    &m, &n, &nrhs, A,
                    &lda, newB, &ldb, jpvt,
                    &rcond, rnk, work, &lwork,
                    info)
                if i == 1
                    lwork = BlasInt(work[1])
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            subsetrows(B, newB, n), rnk[1]
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
            if size(B, 1) != m
                throw(DimensionMismatch("B has leading dimension $(size(B,1)) but needs $m"))
            end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            s     = similar(A, $relty, min(m, n))
            rcond = convert($relty, rcond)
            rnk   = Array(BlasInt, 1)
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            rwork = Array($relty, 1)
            iwork = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(gelsd)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty},
                       Ptr{$relty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$relty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, &size(B,2), A, &max(1,stride(A,2)),
                      newB, &max(1,stride(B,2),n), s, &rcond, rnk, work, &lwork, rwork, iwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                    rwork = Array($relty, BlasInt(rwork[1]))
                    iwork = Array(BlasInt, iwork[1])
                end
            end
            subsetrows(B, newB, n), rnk[1]
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
            m, n = size(A)
            nrhs = size(B, 2)
            if size(B, 1) != m
                throw(DimensionMismatch("B has leading dimension $(size(B,1)) but needs $m"))
            end
            newB = [B; zeros($elty, max(0, n - size(B, 1)), size(B, 2))]
            lda = max(1, m)
            ldb = max(1, m, n)
            jpvt = zeros(BlasInt, n)
            rcond = convert($relty, rcond)
            rnk = Array(BlasInt, 1)
            work = Array($elty, 1)
            lwork = -1
            rwork = Array($relty, 2n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(gelsy)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$relty}, Ptr{BlasInt}),
                    &m, &n, &nrhs, A,
                    &lda, newB, &ldb, jpvt,
                    &rcond, rnk, work, &lwork,
                    rwork, info)
                if i == 1
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            subsetrows(B, newB, n), rnk[1]
        end
    end
end

"""
    gelsd!(A, B, rcond) -> (B, rnk)

Computes the least norm solution of `A * X = B` by finding the `SVD`
factorization of `A`, then dividing-and-conquering the problem. `B`
is overwritten with the solution `X`. Singular values below `rcond`
will be treated as zero. Returns the solution in `B` and the effective rank
of `A` in `rnk`.
"""
gelsd!(A::StridedMatrix, B::StridedVecOrMat, rcond::Real)

"""
    gelsy!(A, B, rcond) -> (B, rnk)

Computes the least norm solution of `A * X = B` by finding the full `QR`
factorization of `A`, then dividing-and-conquering the problem. `B`
is overwritten with the solution `X`. Singular values below `rcond`
will be treated as zero. Returns the solution in `B` and the effective rank
of `A` in `rnk`.
"""
gelsy!(A::StridedMatrix, B::StridedVecOrMat, rcond::Real)

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
            if size(B, 2) != n
                throw(DimensionMismatch("B has second dimension $(size(B,2)), needs $n"))
            end
            if length(c) != m
                throw(DimensionMismatch("c has length $(length(c)), needs $m"))
            end
            if length(d) != p
                throw(DimensionMismatch("d has length $(length(d)), needs $p"))
            end
            X = zeros($elty, n)
            info  = Array(BlasInt, 1)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            for i in 1:2
                ccall(($(blasfunc(gglse)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}),
                      &m, &n, &p, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)), c, d, X,
                      work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            X, dot(sub(c, n - p + 1:m), sub(c, n - p + 1:m))
        end
    end
end

"""
    gglse!(A, c, B, d) -> (X,res)

Solves the equation `A * x = c` where `x` is subject to the equality
constraint `B * x = d`. Uses the formula `||c - A*x||^2 = 0` to solve.
Returns `X` and the residual sum-of-squares.
"""
gglse!(A::StridedMatrix, c::StridedVector, B::StridedMatrix, d::StridedVector)

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
        function geev!(jobvl::Char, jobvr::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            lvecs = jobvl == 'V'
            rvecs = jobvr == 'V'
            VL    = similar(A, $elty, (n, lvecs ? n : 0))
            VR    = similar(A, $elty, (n, rvecs ? n : 0))
            cmplx = eltype(A)<:Complex
            if cmplx
                W     = similar(A, $elty, n)
                rwork = similar(A, $relty, 2n)
            else
                WR    = similar(A, $elty, n)
                WI    = similar(A, $elty, n)
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i = 1:2
                if cmplx
                    ccall(($(blasfunc(geev)), liblapack), Void,
                          (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}),
                          &jobvl, &jobvr, &n, A, &max(1,stride(A,2)), W, VL, &n, VR, &n,
                          work, &lwork, rwork, info)
                else
                    ccall(($(blasfunc(geev)), liblapack), Void,
                          (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &jobvl, &jobvr, &n, A, &max(1,stride(A,2)), WR, WI, VL, &n,
                          VR, &n, work, &lwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function gesdd!(job::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            m, n   = size(A)
            minmn  = min(m, n)
            if job == 'A'
                U  = similar(A, $elty, (m, m))
                VT = similar(A, $elty, (n, n))
            elseif job == 'S'
                U  = similar(A, $elty, (m, minmn))
                VT = similar(A, $elty, (minmn, n))
            elseif job == 'O'
                U  = similar(A, $elty, (m, m >= n ? 0 : m))
                VT = similar(A, $elty, (n, m >= n ? n : 0))
            else
                U  = similar(A, $elty, (m, 0))
                VT = similar(A, $elty, (n, 0))
            end
            work   = Array($elty, 1)
            lwork  = BlasInt(-1)
            S      = similar(A, $relty, minmn)
            cmplx  = eltype(A)<:Complex
            if cmplx
                rwork = Array($relty, job == 'N' ? 7*minmn :
                              minmn*max(5*minmn+7, 2*max(m,n)+2*minmn+1))
            end
            iwork  = Array(BlasInt, 8*minmn)
            info   = Array(BlasInt, 1)
            for i = 1:2
                if cmplx
                    ccall(($(blasfunc(gesdd)), liblapack), Void,
                          (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$relty}, Ptr{BlasInt}, Ptr{BlasInt}),
                          &job, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, iwork, info)
                else
                    ccall(($(blasfunc(gesdd)), liblapack), Void,
                          (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &job, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, iwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function gesvd!(jobu::Char, jobvt::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            m, n   = size(A)
            minmn  = min(m, n)
            S      = similar(A, $relty, minmn)
            U      = similar(A, $elty, jobu  == 'A'? (m, m):(jobu  == 'S'? (m, minmn) : (m, 0)))
            VT     = similar(A, $elty, jobvt == 'A'? (n, n):(jobvt == 'S'? (minmn, n) : (n, 0)))
            work   = Array($elty, 1)
            cmplx  = eltype(A)<:Complex
            if cmplx; rwork = Array($relty, 5minmn); end
            lwork  = BlasInt(-1)
            info   = Array(BlasInt, 1)
            for i in 1:2
                if cmplx
                    ccall(($(blasfunc(gesvd)), liblapack), Void,
                          (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                          &jobu, &jobvt, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, rwork, info)
                else
                    ccall(($(blasfunc(gesvd)), liblapack), Void,
                          (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                           Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                           Ptr{BlasInt}, Ptr{BlasInt}),
                          &jobu, &jobvt, &m, &n, A, &max(1,stride(A,2)), S, U, &max(1,stride(U,2)), VT, &max(1,stride(VT,2)),
                          work, &lwork, info)
                end
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function ggsvd!(jobu::Char, jobv::Char, jobq::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A, B)
            m, n = size(A)
            if size(B, 2) != n
                throw(DimensionMismatch("B has second dimension $(size(B,2)) but needs $n"))
            end
            p = size(B, 1)
            k = Array(BlasInt, 1)
            l = Array(BlasInt, 1)
            lda = max(1,stride(A, 2))
            ldb = max(1,stride(B, 2))
            alpha = similar(A, $relty, n)
            beta = similar(A, $relty, n)
            ldu = max(1, m)
            U = jobu == 'U' ? similar(A, $elty, ldu, m) : similar(A, $elty, 0)
            ldv = max(1, p)
            V = jobv == 'V' ? similar(A, $elty, ldv, p) : similar(A, $elty, 0)
            ldq = max(1, n)
            Q = jobq == 'Q' ? similar(A, $elty, ldq, n) : similar(A, $elty, 0)
            work = Array($elty, max(3n, m, p) + n)
            cmplx = eltype(A)<:Complex
            if cmplx; rwork = Array($relty, 2n); end
            iwork = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            if cmplx
                ccall(($(blasfunc(ggsvd)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
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
                ccall(($(blasfunc(ggsvd)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
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

"""
    geev!(jobvl, jobvr, A) -> (W, VL, VR)

Finds the eigensystem of `A`. If `jobvl = N`, the left eigenvectors of
`A` aren't computed. If `jobvr = N`, the right eigenvectors of `A`
aren't computed. If `jobvl = V` or `jobvr = V`, the corresponding
eigenvectors are computed. Returns the eigenvalues in `W`, the right
eigenvectors in `VR`, and the left eigenvectors in `VL`.
"""
geev!(jobvl::Char, jobvr::Char, A::StridedMatrix)

"""
    gesdd!(job, A) -> (U, S, VT)

Finds the singular value decomposition of `A`, `A = U * S * V'`,
using a divide and conquer approach. If `job = A`, all the columns of `U` and
the rows of `V'` are computed. If `job = N`, no columns of `U` or rows of `V'`
are computed. If `job = O`, `A` is overwritten with the columns of (thin) `U`
and the rows of (thin) `V'`. If `job = S`, the columns of (thin) `U` and the
rows of (thin) `V'` are computed and returned separately.
"""
gesdd!(job::Char, A::StridedMatrix)

"""
    gesvd!(jobu, jobvt, A) -> (U, S, VT)

Finds the singular value decomposition of `A`, `A = U * S * V'`.
If `jobu = A`, all the columns of `U` are computed. If `jobvt = A` all the rows
of `V'` are computed. If `jobu = N`, no columns of `U` are computed. If
`jobvt = N` no rows of `V'` are computed. If `jobu = O`, `A` is overwritten with
the columns of (thin) `U`. If `jobvt = O`, `A` is overwritten with the rows
of (thin) `V'`. If `jobu = S`, the columns of (thin) `U` are computed
and returned separately. If `jobvt = S` the rows of (thin) `V'` are
computed and returned separately. `jobu` and `jobvt` can't both be `O`.

Returns `U`, `S`, and `Vt`, where `S` are the singular values of `A`.
"""
gesvd!(jobu::Char, jobvt::Char, A::StridedMatrix)

"""
    ggsvd!(jobu, jobv, jobq, A, B) -> (U, V, Q, alpha, beta, k, l, R)

Finds the generalized singular value decomposition of `A` and `B`, `U'*A*Q = D1*R`
and `V'*B*Q = D2*R`. `D1` has `alpha` on its diagonal and `D2` has `beta` on its
diagonal. If `jobu = U`, the orthogonal/unitary matrix `U` is computed. If
`jobv = V` the orthogonal/unitary matrix `V` is computed. If `jobq = Q`,
the orthogonal/unitary matrix `Q` is computed. If `job{u,v,q} = N`, that
matrix is not computed.
"""
ggsvd!(jobu::Char, jobv::Char, jobq::Char, A::StridedMatrix, B::StridedMatrix)

## Expert driver and generalized eigenvalue problem
for (geevx, ggev, elty) in
    ((:dgeevx_,:dggev_,:Float64),
     (:sgeevx_,:sggev_,:Float32))
    @eval begin
   #     SUBROUTINE DGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR, WI,
   #                          VL, LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM,
   #                          RCONDE, RCONDV, WORK, LWORK, IWORK, INFO )
   #
   #       .. Scalar Arguments ..
   #       CHARACTER          BALANC, JOBVL, JOBVR, SENSE
   #       INTEGER            IHI, ILO, INFO, LDA, LDVL, LDVR, LWORK, N
   #       DOUBLE PRECISION   ABNRM
   #       ..
   #       .. Array Arguments ..
   #       INTEGER            IWORK( * )
   #       DOUBLE PRECISION   A( LDA, * ), RCONDE( * ), RCONDV( * ),
   #      $                   SCALE( * ), VL( LDVL, * ), VR( LDVR, * ),
   #      $                   WI( * ), WORK( * ), WR( * )
        function geevx!(balanc::Char, jobvl::Char, jobvr::Char, sense::Char, A::StridedMatrix{$elty})
            n = chksquare(A)
            lda = max(1,stride(A,2))
            wr = similar(A, $elty, n)
            wi = similar(A, $elty, n)
            if balanc ∉ ['N', 'P', 'S', 'B']
                throw(ArgumentError("balanc must be 'N', 'P', 'S', or 'B', but $balanc was passed"))
            end
            ldvl = 0
            if jobvl == 'V'
                ldvl = n
            elseif jobvl == 'N'
                ldvl = 0
            else
                throw(ArgumentError("jobvl must be 'V' or 'N', but $jobvl was passed"))
            end
            VL = similar(A, $elty, ldvl, n)
            ldvr = 0
            if jobvr == 'V'
                ldvr = n
            elseif jobvr == 'N'
                ldvr = 0
            else
                throw(ArgumentError("jobvr must be 'V' or 'N', but $jobvr was passed"))
            end
            VR = similar(A, $elty, ldvr, n)
            ilo = Array(BlasInt, 1)
            ihi = Array(BlasInt, 1)
            scale = similar(A, $elty, n)
            abnrm = Array($elty, 1)
            rconde = similar(A, $elty, n)
            rcondv = similar(A, $elty, n)
            work = Array($elty, 1)
            lwork::BlasInt = -1
            iworksize = 0
            if sense == 'N' || sense == 'E'
                iworksize = 0
            elseif sense == 'V' || sense == 'B'
                iworksize = 2*n - 2
            else
                throw(ArgumentError("sense must be 'N', 'E', 'V' or 'B', but $sense was passed"))
            end
            iwork = Array(BlasInt, iworksize)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(geevx)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                       &balanc, &jobvl, &jobvr, &sense,
                       &n, A, &lda, wr,
                       wi, VL, &max(1,ldvl), VR,
                       &max(1,ldvr), ilo, ihi, scale,
                       abnrm, rconde, rcondv, work,
                       &lwork, iwork, info)
                lwork = convert(BlasInt, work[1])
                work = Array($elty, lwork)
            end
            @lapackerror
            A, wr, wi, VL, VR, ilo[1], ihi[1], scale, abnrm[1], rconde, rcondv
        end
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
        function ggev!(jobvl::Char, jobvr::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A,B)
            n, m = chksquare(A,B)
            if n != m
                throw(DimensionMismatch("A has dimensions $(size(A)), and B has dimensions $(size(B)), but A and B must have the same size"))
            end
            lda = max(1, stride(A, 2))
            ldb = max(1, stride(B, 2))
            alphar = similar(A, $elty, n)
            alphai = similar(A, $elty, n)
            beta = similar(A, $elty, n)
            ldvl = 0
            if jobvl == 'V'
                ldvl = n
            elseif jobvl == 'N'
                ldvl = 1
            else
                throw(ArgumentError("jobvl must be 'V' or 'N', but $jobvl was passed"))
            end
            vl = similar(A, $elty, ldvl, n)
            ldvr = 0
            if jobvr == 'V'
                ldvr = n
            elseif jobvr == 'N'
                ldvr = 1
            else
                throw(ArgumentError("jobvr must be 'V' or 'N', but $jobvr was passed"))
            end
            vr = similar(A, $elty, ldvr, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(ggev)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty},
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
                    lwork = BlasInt(work[1])
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            alphar, alphai, beta, vl, vr
        end
    end
end
for (geevx, ggev, elty, relty) in
    ((:zgeevx_,:zggev_,:Complex128,:Float64),
     (:cgeevx_,:cggev_,:Complex64,:Float32))
    @eval begin
  #     SUBROUTINE ZGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, W, VL,
  #                          LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM, RCONDE,
  #                          RCONDV, WORK, LWORK, RWORK, INFO )
  #
  #       .. Scalar Arguments ..
  #       CHARACTER          BALANC, JOBVL, JOBVR, SENSE
  #       INTEGER            IHI, ILO, INFO, LDA, LDVL, LDVR, LWORK, N
  #       DOUBLE PRECISION   ABNRM
  #       ..
  #       .. Array Arguments ..
  #       DOUBLE PRECISION   RCONDE( * ), RCONDV( * ), RWORK( * ),
  #      $                   SCALE( * )
  #       COMPLEX*16         A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
  #      $                   W( * ), WORK( * )
    function geevx!(balanc::Char, jobvl::Char, jobvr::Char, sense::Char, A::StridedMatrix{$elty})
        n = chksquare(A)
        lda = max(1,stride(A,2))
        w = similar(A, $elty, n)
        if balanc ∉ ['N', 'P', 'S', 'B']
            throw(ArgumentError("balanc must be 'N', 'P', 'S', or 'B', but $balanc was passed"))
        end
        ldvl = 0
        if jobvl == 'V'
            ldvl = n
        elseif jobvl == 'N'
            ldvl = 0
        else
            throw(ArgumentError("jobvl must be 'V' or 'N', but $jobvl was passed"))
        end
        VL = similar(A, $elty, ldvl, n)
        ldvr = 0
        if jobvr == 'V'
            ldvr = n
        elseif jobvr == 'N'
            ldvr = 0
        else
            throw(ArgumentError("jobvr must be 'V' or 'N', but $jobvr was passed"))
        end
        if sense ∉ ['N','E','V','B']
            throw(ArgumentError("sense must be 'N', 'E', 'V' or 'B', but $sense was passed"))
        end
        VR = similar(A, $elty, ldvr, n)
        ilo = Array(BlasInt, 1)
        ihi = Array(BlasInt, 1)
        scale = similar(A, $relty, n)
        abnrm = Array($relty, 1)
        rconde = similar(A, $relty, n)
        rcondv = similar(A, $relty, n)
        work = Array($elty, 1)
        lwork::BlasInt = -1
        rwork = Array($relty, 2n)
        info = Array(BlasInt, 1)
        for i = 1:2
            ccall(($(blasfunc(geevx)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$relty},
                   Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{$relty}, Ptr{BlasInt}),
                   &balanc, &jobvl, &jobvr, &sense,
                   &n, A, &lda, w,
                   VL, &max(1,ldvl), VR, &max(1,ldvr),
                   ilo, ihi, scale, abnrm,
                   rconde, rcondv, work, &lwork,
                   rwork, info)
            lwork = convert(BlasInt, work[1])
            work = Array($elty, lwork)
        end
        @lapackerror
        A, w, VL, VR, ilo[1], ihi[1], scale, abnrm[1], rconde, rcondv
    end

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
        function ggev!(jobvl::Char, jobvr::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A, B)
            n, m = chksquare(A, B)
            if n != m
                throw(DimensionMismatch("A has dimensions $(size(A)), and B has dimensions $(size(B)), but A and B must have the same size"))
            end
            lda = max(1, stride(A, 2))
            ldb = max(1, stride(B, 2))
            alpha = similar(A, $elty, n)
            beta = similar(A, $elty, n)
            ldvl = 0
            if jobvl == 'V'
                ldvl = n
            elseif jobvl == 'N'
                ldvl = 1
            else
                throw(ArgumentError("jobvl must be 'V' or 'N', but $jobvl was passed"))
            end
            vl = similar(A, $elty, ldvl, n)
            ldvr = 0
            if jobvr == 'V'
                ldvr = n
            elseif jobvr == 'N'
                ldvr = 1
            else
                throw(ArgumentError("jobvr must be 'V' or 'N', but $jobvr was passed"))
            end
            vr = similar(A, $elty, ldvr, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            rwork = Array($relty, 8n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(ggev)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty},
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
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            alpha, beta, vl, vr
        end
    end
end

"""
    geevx!(balanc, jobvl, jobvr, sense, A) -> (A, w, VL, VR, ilo, ihi, scale, abnrm, rconde, rcondv)

Finds the eigensystem of `A` with matrix balancing. If `jobvl = N`, the
left eigenvectors of `A` aren't computed. If `jobvr = N`, the right
eigenvectors of `A` aren't computed. If `jobvl = V` or `jobvr = V`, the
corresponding eigenvectors are computed. If `balanc = N`, no balancing is
performed. If `balanc = P`, `A` is permuted but not scaled. If
`balanc = S`, `A` is scaled but not permuted. If `balanc = B`, `A` is
permuted and scaled. If `sense = N`, no reciprocal condition numbers are
computed. If `sense = E`, reciprocal condition numbers are computed for
the eigenvalues only. If `sense = V`, reciprocal condition numbers are
computed for the right eigenvectors only. If `sense = B`, reciprocal
condition numbers are computed for the right eigenvectors and the
eigenvectors. If `sense = E,B`, the right and left eigenvectors must be
computed.
"""
geevx!(balanc::Char, jobvl::Char, jobvr::Char, sense::Char, A::StridedMatrix)

"""
    ggev!(jobvl, jobvr, A, B) -> (alpha, beta, vl, vr)

Finds the generalized eigendecomposition of `A` and `B`. If `jobvl = N`,
the left eigenvectors aren't computed. If `jobvr = N`, the right
eigenvectors aren't computed. If `jobvl = V` or `jobvr = V`, the
corresponding eigenvectors are computed.
"""
ggev!(jobvl::Char, jobvr::Char, A::StridedMatrix, B::StridedMatrix)

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
            if j != length(w)
                throw(DimensionMismatch("Vectors must have same length, but length of x is $j and length of w is $(length(w))"))
            end
            sestpr = Array($elty, 1)
            s = Array($elty, 1)
            c = Array($elty, 1)
            ccall(($(blasfunc(laic1)), liblapack), Void,
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
            if j != length(w)
                throw(DimensionMismatch("Vectors must have same length, but length of x is $j and length of w is $(length(w))"))
            end
            sestpr = Array($relty, 1)
            s = Array($elty, 1)
            c = Array($elty, 1)
            ccall(($(blasfunc(laic1)), liblapack), Void,
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
            if !(n >= length(dl) >= n - 1)
                throw(DimensionMismatch("Subdiagonal has length $(length(dl)), but should be $n or $(n - 1)"))
            end
            if !(n >= length(du) >= n - 1)
                throw(DimensionMismatch("Superdiagonal has length $(length(du)), but should be $n or $(n - 1)"))
            end
            if n != size(B,1)
                throw(DimensionMismatch("B has leading dimension $(size(B,1)), but should have $n"))
            end
            if n == 0
                return B # Early exit if possible
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(gtsv)), liblapack), Void,
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
            if length(dl) != n - 1
                throw(DimensionMismatch("Subdiagonal has length $(length(dl)), but should be $(n - 1)"))
            end
            if length(du) != n - 1
                throw(DimensionMismatch("Superdiagonal has length $(length(du)), but should be $(n - 1)"))
            end
            du2  = similar(d, $elty, n-2)
            ipiv = similar(d, BlasInt, n)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(gttrf)), liblapack), Void,
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
        function gttrs!(trans::Char, dl::Vector{$elty}, d::Vector{$elty},
                        du::Vector{$elty}, du2::Vector{$elty}, ipiv::Vector{BlasInt},
                        B::StridedVecOrMat{$elty})
            chktrans(trans)
            chkstride1(B)
            n = length(d)
            if length(dl) != n - 1
                throw(DimensionMismatch("Subdiagonal has length $(length(dl)), but should be $(n - 1)"))
            end
            if length(du) != n - 1
                throw(DimensionMismatch("Superdiagonal has length $(length(du)), but should be $(n - 1)"))
            end
            if n != size(B,1)
                throw(DimensionMismatch("B has leading dimension $(size(B,1)), but should have $n"))
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(gttrs)), liblapack), Void,
                   (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                    Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                   &trans, &n, &size(B,2), dl, d, du, du2, ipiv, B, &max(1,stride(B,2)), info)
             @lapackerror
             B
         end
    end
end

"""
    gtsv!(dl, d, du, B)

Solves the equation `A * X = B` where `A` is a tridiagonal matrix with
`dl` on the subdiagonal, `d` on the diagonal, and `du` on the
superdiagonal.

Overwrites `B` with the solution `X` and returns it.
"""
gtsv!(dl::Vector, d::Vector, du::Vector, B::StridedVecOrMat)

"""
    gttrf!(dl, d, du) -> (dl, d, du, du2, ipiv)

Finds the `LU` factorization of a tridiagonal matrix with `dl` on the
subdiagonal, `d` on the diagonal, and `du` on the superdiagonal.

Modifies `dl`, `d`, and `du` in-place and returns them and the second
superdiagonal `du2` and the pivoting vector `ipiv`.
"""
gttrf!(dl::Vector, d::Vector, du::Vector)

"""
    gttrs!(trans, dl, d, du, du2, ipiv, B)

Solves the equation `A * X = B` (`trans = N`), `A.' * X = B` (`trans = T`),
or `A' * X = B` (`trans = C`) using the `LU` factorization computed by
`gttrf!`. `B` is overwritten with the solution `X`.
"""
gttrs!(trans::Char, dl::Vector, d::Vector, du::Vector, du2::Vector,
       ipiv::Vector{BlasInt}, B::StridedVecOrMat)

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
        function orglq!(A::StridedMatrix{$elty}, tau::Vector{$elty}, k::Integer = length(tau))
            chkstride1(A)
            n = size(A, 2)
            m = min(n, size(A, 1))
            if k > m
                throw(DimensionMismatch("Invalid number of reflectors: k = $k should be <= m = $m"))
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(orglq)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, &k, A, &max(1,stride(A,2)), tau, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            if m<size(A,1)
                A[1:m,:]
            else
                A
            end
        end
        # SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
        # *     .. Scalar Arguments ..
        #       INTEGER            INFO, K, LDA, LWORK, M, N
        # *     .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
        function orgqr!(A::StridedMatrix{$elty}, tau::Vector{$elty}, k::Integer = length(tau))
            chkstride1(A)
            m = size(A, 1)
            n = min(m, size(A, 2))
            if k > n
                throw(DimensionMismatch("Invalid number of reflectors: k = $k should be <= n = $n"))
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(orgqr)), liblapack), Void,
                      (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &m, &n, &k, A,
                      &max(1,stride(A,2)), tau, work, &lwork,
                      info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            if n<size(A,2)
                A[:,1:n]
            else
                A
            end
        end
        #      SUBROUTINE DORMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
        #                         WORK, LWORK, INFO )
        #      .. Scalar Arguments ..
        #      CHARACTER          SIDE, TRANS
        #      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
        #      .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
        function ormlq!(side::Char, trans::Char, A::StridedMatrix{$elty},
                        tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            chktrans(trans)
            chkside(side)
            chkstride1(A, C)
            m, n = ndims(C)==2 ? size(C) : (size(C, 1), 1)
            nA    = size(A, 2)
            k     = length(tau)
            if side == 'L' && m != nA
                throw(DimensionMismatch("For a left-sided multiplication, the first dimension of C, $m, must equal the second dimension of A, $nA"))
            end
            if side == 'R' && n != nA
                throw(DimensionMismatch("For a right-sided multiplication, the second dimension of C, $m, must equal the second dimension of A, $nA"))
            end
            if side == 'L' && k > m
                throw(DimensionMismatch("Invalid number of reflectors: k = $k should be <= m = $m"))
            end
            if side == 'R' && k > n
                throw(DimensionMismatch("Invalid number of reflectors: k = $k should be <= n = $n"))
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(ormlq)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &side, &trans, &m, &n, &k, A, &max(1,stride(A,2)), tau,
                      C, &max(1,stride(C,2)), work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function ormqr!(side::Char, trans::Char, A::StridedMatrix{$elty},
                        tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            chktrans(trans)
            chkside(side)
            chkstride1(A, C)
            m, n = ndims(C)==2 ? size(C) : (size(C, 1), 1)
            mA    = size(A, 1)
            k     = length(tau)
            if side == 'L' && m != mA
                throw(DimensionMismatch("For a left-sided multiplication, the first dimension of C, $m, must equal the second dimension of A, $mA"))
            end
            if side == 'R' && n != mA
                throw(DimensionMismatch("For a right-sided multiplication, the second dimension of C, $m, must equal the second dimension of A, $mA"))
            end
            if side == 'L' && k > m
                throw(DimensionMismatch("Invalid number of reflectors: k = $k should be <= m = $m"))
            end
            if side == 'R' && k > n
                throw(DimensionMismatch("Invalid number of reflectors: k = $k should be <= n = $n"))
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(ormqr)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}),
                      &side, &trans, &m, &n,
                      &k, A, &max(1,stride(A,2)), tau,
                      C, &max(1, stride(C,2)), work, &lwork,
                      info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            C
        end
        function gemqrt!(side::Char, trans::Char, V::StridedMatrix{$elty}, T::StridedMatrix{$elty}, C::StridedVecOrMat{$elty})
            chktrans(trans)
            chkside(side)
            chkstride1(V, T, C)
            m, n = ndims(C)==2 ? size(C) : (size(C, 1), 1)
            nb, k = size(T)
            if k == 0 return C end
            if side == 'L'
                if !(0 <= k <= m)
                    throw(DimensionMismatch("Wrong value for k = $k: must be between 0 and $m"))
                end
                if m != size(V,1)
                    throw(DimensionMismatch("First dimensions of C, $m, and V, $(size(V,1)) must match"))
                end
                ldv = stride(V,2)
                if ldv < max(1, m)
                    throw(DimensionMismatch("Q and C don't fit! The stride of V, $ldv, is too small"))
                end
                wss = n*k
            elseif side == 'R'
                if !(0 <= k <= n)
                    throw(DimensionMismatch("Wrong value for k = $k: must be between 0 and $n"))
                end
                if n != size(V,1)
                    throw(DimensionMismatch("Second dimension of C, $n, and first dimension of V, $(size(V,1)) must match"))
                end
                ldv = stride(V,2)
                if ldv < max(1, n)
                    throw(DimensionMismatch("Q and C don't fit! The stride of V, $ldv, is too small"))
                end
                wss = m*k
            end
            if !(1 <= nb <= k)
                throw(DimensionMismatch("Wrong value for nb = $nb, which must be between 1 and $k"))
            end
            ldc = stride(C, 2)
            work = Array($elty, wss)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(gemqrt)), liblapack), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}),
                &side, &trans, &m, &n,
                &k, &nb, V, &ldv,
                T, &max(1,stride(T,2)), C, &max(1,ldc),
                work, info)
            @lapackerror
            return C
        end
    end
end

"""
    orglq!(A, tau, k = length(tau))

Explicitly finds the matrix `Q` of a `LQ` factorization after calling
`gelqf!` on `A`. Uses the output of `gelqf!`. `A` is overwritten by `Q`.
"""
orglq!(A::StridedMatrix, tau::Vector, k::Integer = length(tau))

"""
    orgqr!(A, tau, k = length(tau))

Explicitly finds the matrix `Q` of a `QR` factorization after calling
`geqrf!` on `A`. Uses the output of `geqrf!`. `A` is overwritten by `Q`.
"""
orgqr!(A::StridedMatrix, tau::Vector, k::Integer = length(tau))

"""
    ormlq!(side, trans, A, tau, C)

Computes `Q * C` (`trans = N`), `Q.' * C` (`trans = T`), `Q' * C`
(`trans = C`) for `side = L` or the equivalent right-sided multiplication
for `side = R` using `Q` from a `LQ` factorization of `A` computed using
`gelqf!`. `C` is overwritten.
"""
ormlq!(side::Char, trans::Char, A::StridedMatrix, tau::Vector, C::StridedVecOrMat)

"""
    ormqr!(side, trans, A, tau, C)

Computes `Q * C` (`trans = N`), `Q.' * C` (`trans = T`), `Q' * C`
(`trans = C`) for `side = L` or the equivalent right-sided multiplication
for `side = R` using `Q` from a `QR` factorization of `A` computed using
`geqrf!`. `C` is overwritten.
"""
ormqr!(side::Char, trans::Char, A::StridedMatrix, tau::Vector, C::StridedVecOrMat)

"""
    gemqrt!(side, trans, V, T, C)

Computes `Q * C` (`trans = N`), `Q.' * C` (`trans = T`), `Q' * C`
(`trans = C`) for `side = L` or the equivalent right-sided multiplication
for `side = R` using `Q` from a `QR` factorization of `A` computed using
`geqrt!`. `C` is overwritten.
"""
gemqrt!(side::Char, trans::Char, V::StridedMatrix, T::StridedMatrix, C::StridedVecOrMat)

# (PO) positive-definite symmetric matrices,
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
        function posv!(uplo::Char, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            n = chksquare(A)
            chkuplo(uplo)
            if size(B,1) != n
                throw(DimensionMismatch("First dimension of B, $(size(B,1)), and size of A, ($n,$n), must match!"))
            end
            info    = Array(BlasInt, 1)
            ccall(($(blasfunc(posv)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function potrf!(uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            chksquare(A)
            chkuplo(uplo)
            lda = max(1,stride(A,2))
            if lda == 0
                return A, 0
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(potrf)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &size(A,1), A, &lda, info)
            @assertargsok
            #info[1]>0 means the leading minor of order info[i] is not positive definite
            #ordinarily, throw Exception here, but return error code here
            #this simplifies isposdef! and factorize
            return A, info[1]
        end
        #       SUBROUTINE DPOTRI( UPLO, N, A, LDA, INFO )
        #       .. Scalar Arguments ..
        #       CHARACTER          UPLO
        #       INTEGER            INFO, LDA, N
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * )
        function potri!(uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            chkuplo(uplo)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(potri)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
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
        function potrs!(uplo::Char, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A, B)
            n = chksquare(A)
            chkuplo(uplo)
            nrhs = size(B,2)
            if size(B,1) != n
                throw(DimensionMismatch("First dimension of B, $(size(B,1)), and size of A, ($n,$n), must match!"))
            end
            lda = max(1,stride(A,2))
            if lda == 0 || nrhs == 0
                return B
            end
            ldb = max(1,stride(B,2))
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(potrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
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
        function pstrf!(uplo::Char, A::StridedMatrix{$elty}, tol::Real)
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            piv  = similar(A, BlasInt, n)
            rank = Array(BlasInt, 1)
            work = Array($rtyp, 2n)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(pstrf)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$rtyp}, Ptr{$rtyp}, Ptr{BlasInt}),
                  &uplo, &n, A, &max(1,stride(A,2)), piv, rank, &tol, work, info)
            @assertargsok
            A, piv, rank[1], info[1] #Stored in PivotedCholesky
        end
    end
end

"""
    posv!(uplo, A, B) -> (A, B)

Finds the solution to `A * X = B` where `A` is a symmetric or Hermitian
positive definite matrix. If `uplo = U` the upper Cholesky decomposition
of `A` is computed. If `uplo = L` the lower Cholesky decomposition of `A`
is computed. `A` is overwritten by its Cholesky decomposition. `B` is
overwritten with the solution `X`.
"""
posv!(uplo::Char, A::StridedMatrix, B::StridedVecOrMat)

"""
    potrf!(uplo, A)

Computes the Cholesky (upper if `uplo = U`, lower if `uplo = L`)
decomposition of positive-definite matrix `A`. `A` is overwritten and
returned with an info code.
"""
potrf!(uplo::Char, A::StridedMatrix)

"""
    potri!(uplo, A)

Computes the inverse of positive-definite matrix `A` after calling
`potrf!` to find its (upper if `uplo = U`, lower if `uplo = L`) Cholesky
decomposition.

`A` is overwritten by its inverse and returned.
"""
potri!(uplo::Char, A::StridedMatrix)

"""
    potrs!(uplo, A, B)

Finds the solution to `A * X = B` where `A` is a symmetric or Hermitian
positive definite matrix whose Cholesky decomposition was computed by
`potrf!`. If `uplo = U` the upper Cholesky decomposition of `A` was
computed. If `uplo = L` the lower Cholesky decomposition of `A` was
computed. `B` is overwritten with the solution `X`.
"""
potrs!(uplo::Char, A::StridedMatrix, B::StridedVecOrMat)

"""
    pstrf!(uplo, A, tol) -> (A, piv, rank, info)

Computes the (upper if `uplo = U`, lower if `uplo = L`) pivoted Cholesky
decomposition of positive-definite matrix `A` with a user-set tolerance
`tol`. `A` is overwritten by its Cholesky decomposition.

Returns `A`, the pivots `piv`, the rank of `A`, and an `info` code. If `info = 0`,
the factorization succeeded. If `info = i > 0 `, then `A` is indefinite or
rank-deficient.
"""
pstrf!(uplo::Char, A::StridedMatrix, tol::Real)

## (PT) positive-definite, symmetric, tri-diagonal matrices
## Direct solvers for general tridiagonal and symmetric positive-definite tridiagonal
for (ptsv, pttrf, elty, relty) in
    ((:dptsv_,:dpttrf_,:Float64,:Float64),
     (:sptsv_,:spttrf_,:Float32,:Float32),
     (:zptsv_,:zpttrf_,:Complex128,:Float64),
     (:cptsv_,:cpttrf_,:Complex64,:Float32))
    @eval begin
        #       SUBROUTINE DPTSV( N, NRHS, D, E, B, LDB, INFO )
        #       .. Scalar Arguments ..
        #       INTEGER            INFO, LDB, N, NRHS
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   B( LDB, * ), D( * ), E( * )
        function ptsv!(D::Vector{$relty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            n = length(D)
            if length(E) != n - 1
                throw(DimensionMismatch("E has length $(length(E)), but needs $(n - 1)"))
            end
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)) but needs $n"))
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(ptsv)), liblapack), Void,
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
            if length(E) != n - 1
                throw(DimensionMismatch("E has length $(length(E)), but needs $(n - 1)"))
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(pttrf)), liblapack), Void,
                  (Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}),
                  &n, D, E, info)
            @lapackerror
            D, E
        end
    end
end

"""
    ptsv!(D, E, B)

Solves `A * X = B` for positive-definite tridiagonal `A`. `D` is the
diagonal of `A` and `E` is the off-diagonal. `B` is overwritten with the
solution `X` and returned.
"""
ptsv!(D::Vector, E::Vector, B::StridedVecOrMat)

"""
    pttrf!(D, E)

Computes the LDLt factorization of a positive-definite tridiagonal matrix
with `D` as diagonal and `E` as off-diagonal. `D` and `E` are overwritten
and returned.
"""
pttrf!(D::Vector, E::Vector)

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
            if length(E) != n - 1
                throw(DimensionMismatch("E has length $(length(E)), but needs $(n - 1)"))
            end
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)) but needs $n"))
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(pttrs)), liblapack), Void,
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
        function pttrs!(uplo::Char, D::Vector{$relty}, E::Vector{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(B)
            chkuplo(uplo)
            n = length(D)
            if length(E) != n - 1
                throw(DimensionMismatch("E has length $(length(E)), but needs $(n - 1)"))
            end
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)) but needs $n"))
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(pttrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$relty}, Ptr{$elty},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), D, E, B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
    end
end

"""
    pttrs!(D, E, B)

Solves `A * X = B` for positive-definite tridiagonal `A` with diagonal
`D` and off-diagonal `E` after computing `A`'s LDLt factorization using
`pttrf!`. `B` is overwritten with the solution `X`.
"""
pttrs!(D::Vector, E::Vector, B::StridedVecOrMat)

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
        function trtri!(uplo::Char, diag::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            chkdiag(diag)
            lda     = max(1,stride(A, 2))
            info    = Array(BlasInt, 1)
            ccall(($(blasfunc(trtri)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function trtrs!(uplo::Char, trans::Char, diag::Char,
                        A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chktrans(trans)
            chkdiag(diag)
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)) but needs $n"))
            end
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(trtrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                   Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &trans, &diag, &n, &size(B,2), A, &max(1,stride(A,2)),
                  B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
    end
end

"""
    trtri!(uplo, diag, A)

Finds the inverse of (upper if `uplo = U`, lower if `uplo = L`)
triangular matrix `A`. If `diag = N`, `A` has non-unit diagonal elements.
If `diag = U`, all diagonal elements of `A` are one. `A` is overwritten
with its inverse.
"""
trtri!(uplo::Char, diag::Char, A::StridedMatrix)

"""
    trtrs!(uplo, trans, diag, A, B)

Solves `A * X = B` (`trans = N`), `A.' * X = B` (`trans = T`), or
`A' * X = B` (`trans = C`) for (upper if `uplo = U`, lower if `uplo = L`)
triangular matrix `A`. If `diag = N`, `A` has non-unit diagonal elements.
If `diag = U`, all diagonal elements of `A` are one. `B` is overwritten
with the solution `X`.
"""
trtrs!(uplo::Char, trans::Char, diag::Char, A::StridedMatrix, B::StridedVecOrMat)

#Eigenvector computation and condition number estimation
for (trcon, trevc, trrfs, elty) in
    ((:dtrcon_,:dtrevc_,:dtrrfs_,:Float64),
     (:strcon_,:strevc_,:strrfs_,:Float32))
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
        function trcon!(norm::Char, uplo::Char, diag::Char,
                        A::StridedMatrix{$elty})
            chkstride1(A)
            chkdiag(diag)
            n = chksquare(A)
            chkuplo(uplo)
            rcond = Array($elty, 1)
            work  = Array($elty, 3n)
            iwork = Array(BlasInt, n)
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(trcon)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
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
        function trevc!(side::Char, howmny::Char, select::Vector{BlasInt}, T::StridedMatrix{$elty},
                VL::StridedMatrix{$elty} = similar(T), VR::StridedMatrix{$elty} = similar(T))
            # Extract
            if side ∉ ['L','R','B']
                throw(ArgumentError("side argument must be 'L' (left eigenvectors), 'R' (right eigenvectors), or 'B' (both), got $side"))
            end
            n, mm = chksquare(T), size(VL, 2)
            ldt, ldvl, ldvr = stride(T, 2), stride(VL, 2), stride(VR, 2)

            # Check
            chkstride1(T)

            # Allocate
            m = Array(BlasInt, 1)
            work = Array($elty, 3n)
            info = Array(BlasInt, 1)

            ccall(($(blasfunc(trevc)), liblapack), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt},Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}),
                &side, &howmny, select, &n,
                T, &ldt, VL, &ldvl,
                VR, &ldvr, &mm, m,
                work, info)
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
        # SUBROUTINE DTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,
        #                    LDX, FERR, BERR, WORK, IWORK, INFO )
        # .. Scalar Arguments ..
        # CHARACTER          DIAG, TRANS, UPLO
        # INTEGER            INFO, LDA, LDB, LDX, N, NRHS
        # .. Array Arguments ..
        # INTEGER            IWORK( * )
        # DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), BERR( * ), FERR( * ),
        #$                   WORK( * ), X( LDX, * )
        function trrfs!(uplo::Char, trans::Char, diag::Char,
                A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, X::StridedVecOrMat{$elty},
                Ferr::StridedVector{$elty}=similar(B, $elty, size(B,2)), Berr::StridedVector{$elty} = similar(B, $elty, size(B,2)))
            chktrans(trans)
            chkuplo(uplo)
            chkdiag(diag)
            n = size(A,2)
            nrhs = size(B,2)
            if nrhs != size(X,2)
                throw(DimensionMismatch("Second dimensions of B, $nrhs, and X, $(size(X,2)), must match"))
            end
            work = Array($elty, 3n)
            iwork = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(trrfs)), liblapack), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                &uplo, &trans, &diag, &n,
                &nrhs, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)), X, &max(1,stride(X,2)),
                Ferr, Berr, work, iwork, info)
            @lapackerror
            Ferr, Berr
        end
    end
end
for (trcon, trevc, trrfs, elty, relty) in
    ((:ztrcon_,:ztrevc_,:ztrrfs_,:Complex128,:Float64),
     (:ctrcon_,:ctrevc_,:ctrrfs_,:Complex64, :Float32))
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
        function trcon!(norm::Char, uplo::Char, diag::Char,
                        A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            chkdiag(diag)
            rcond = Array($relty, 1)
            work  = Array($elty, 2n)
            rwork = Array($relty, n)
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(trcon)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
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
        function trevc!(side::Char, howmny::Char, select::Vector{BlasInt}, T::StridedMatrix{$elty},
                VL::StridedMatrix{$elty} = similar(T), VR::StridedMatrix{$elty} = similar(T))
            # Extract
            n, mm = chksquare(T), size(VL, 2)
            ldt, ldvl, ldvr = stride(T, 2), stride(VL, 2), stride(VR, 2)

            # Check
            chkstride1(T)
            if side ∉ ['L','R','B']
                throw(ArgumentError("side argument must be 'L' (left eigenvectors), 'R' (right eigenvectors), or 'B' (both), got $side"))
            end

            # Allocate
            m = Array(BlasInt, 1)
            work = Array($elty, 2n)
            rwork = Array($relty, n)
            info = Array(BlasInt, 1)

            ccall(($(blasfunc(trevc)), liblapack), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{$relty}, Ptr{BlasInt}),
                &side, &howmny, select, &n,
                T, &ldt, VL, &ldvl,
                VR, &ldvr, &mm, m,
                work, rwork, info)
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
        # SUBROUTINE ZTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB, X,
        #                    LDX, FERR, BERR, WORK, IWORK, INFO )
        # .. Scalar Arguments ..
        # CHARACTER          DIAG, TRANS, UPLO
        # INTEGER            INFO, LDA, LDB, LDX, N, NRHS
        # .. Array Arguments ..
        # INTEGER            IWORK( * )
        # DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), BERR( * ), FERR( * ),
        #$                   WORK( * ), X( LDX, * )
        function trrfs!(uplo::Char, trans::Char, diag::Char,
                A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty}, X::StridedVecOrMat{$elty},
                Ferr::StridedVector{$relty}=similar(B, $relty, size(B,2)), Berr::StridedVector{$relty}=similar(B, $relty, size(B,2)))
            chktrans(trans)
            chkuplo(uplo)
            chkdiag(diag)
            n=size(A,2)
            nrhs=size(B,2)
            if nrhs != size(X,2)
                throw(DimensionMismatch("Second dimensions of B, $nrhs, and X, $(size(X,2)), must match"))
            end
            work=Array($elty, 2n)
            rwork=Array($relty, n)
            info=Array(BlasInt, 1)
            ccall(($(blasfunc(trrfs)), liblapack), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{$relty}, Ptr{BlasInt}),
                &uplo, &trans, &diag, &n,
                &nrhs, A, &max(1,stride(A,2)), B, &max(1,stride(B,2)), X, &max(1,stride(X,2)),
                Ferr, Berr, work, rwork, info)
            @lapackerror
            Ferr, Berr
        end
    end
end

"""
    trcon!(norm, uplo, diag, A)

Finds the reciprocal condition number of (upper if `uplo = U`, lower if
`uplo = L`) triangular matrix `A`. If `diag = N`, `A` has non-unit
diagonal elements. If `diag = U`, all diagonal elements of `A` are one.
If `norm = I`, the condition number is found in the infinity norm. If
`norm = O` or `1`, the condition number is found in the one norm.
"""
trcon!(norm::Char, uplo::Char, diag::Char, A::StridedMatrix)

"""
    trevc!(side, howmny, select, T, VL = similar(T), VR = similar(T))

Finds the eigensystem of an upper triangular matrix `T`. If `side = R`,
the right eigenvectors are computed. If `side = L`, the left
eigenvectors are computed. If `side = B`, both sets are computed. If
`howmny = A`, all eigenvectors are found. If `howmny = B`, all
eigenvectors are found and backtransformed using `VL` and `VR`. If
`howmny = S`, only the eigenvectors corresponding to the values in
`select` are computed.
"""
trevc!(side::Char, howmny::Char, select::Vector{BlasInt}, T::StridedMatrix,
        VL::StridedMatrix = similar(T), VR::StridedMatrix = similar(T))

"""
    trrfs!(uplo, trans, diag, A, B, X, Ferr, Berr) -> (Ferr, Berr)

Estimates the error in the solution to `A * X = B` (`trans = N`),
`A.' * X = B` (`trans = T`), `A' * X = B` (`trans = C`) for `side = L`,
or the equivalent equations a right-handed `side = R` `X * A` after
computing `X` using `trtrs!`. If `uplo = U`, `A` is upper triangular.
If `uplo = L`, `A` is lower triangular. If `diag = N`, `A` has non-unit
diagonal elements. If `diag = U`, all diagonal elements of `A` are one.
`Ferr` and `Berr` are optional inputs. `Ferr` is the forward error and
`Berr` is the backward error, each component-wise.
"""
trrfs!(uplo::Char, trans::Char, diag::Char, A::StridedMatrix, B::StridedVecOrMat,
       X::StridedVecOrMat, Ferr::StridedVector, Berr::StridedVector)

## (ST) Symmetric tridiagonal - eigendecomposition
for (stev, stebz, stegr, stein, elty) in
    ((:dstev_,:dstebz_,:dstegr_,:dstein_,:Float64),
     (:sstev_,:sstebz_,:sstegr_,:sstein_,:Float32)
#     , (:zstev_,:Complex128)  Need to rewrite for ZHEEV, rwork, etc.
#     , (:cstev_,:Complex64)
     )
    @eval begin
        function stev!(job::Char, dv::Vector{$elty}, ev::Vector{$elty})
            n = length(dv)
            if length(ev) != n - 1
                throw(DimensionMismatch("ev has length $(length(ev)) but needs one less than dv's length, $n)"))
            end
            Zmat = similar(dv, $elty, (n, job != 'N' ? n : 0))
            work = Array($elty, max(1, 2n-2))
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(stev)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
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
            if length(ev) != n - 1
                throw(DimensionMismatch("ev has length $(length(ev)) but needs one less than dv's length, $n)"))
            end
            m = Array(BlasInt,1)
            nsplit = Array(BlasInt,1)
            w = similar(dv, $elty, n)
            tmp = 0.0
            iblock = similar(dv, BlasInt,n)
            isplit = similar(dv, BlasInt,n)
            work = Array($elty, 4*n)
            iwork = Array(BlasInt,3*n)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(stebz)), liblapack), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty},
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
        function stegr!(jobz::Char, range::Char, dv::Vector{$elty}, ev::Vector{$elty}, vl::Real, vu::Real, il::Integer, iu::Integer)
            n = length(dv)
            if length(ev) != n - 1
                throw(DimensionMismatch("ev has length $(length(ev)) but needs one less than dv's length, $n)"))
            end
            eev = [ev; zero($elty)]
            abstol = Array($elty, 1)
            m = Array(BlasInt, 1)
            w = similar(dv, $elty, n)
            ldz = jobz == 'N' ? 1 : n
            Z = similar(dv, $elty, ldz, n)
            isuppz = similar(dv, BlasInt, 2n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            iwork = Array(BlasInt, 1)
            liwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(stegr)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty},
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
                    lwork = BlasInt(work[1])
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            @lapackerror
            w[1:m[1]], Z[:,1:m[1]]
        end

        function stein!(dv::Vector{$elty}, ev_in::Vector{$elty}, w_in::Vector{$elty}, iblock_in::Vector{BlasInt}, isplit_in::Vector{BlasInt})
            n = length(dv)
            if length(ev_in) != n - 1
                throw(DimensionMismatch("ev_in has length $(length(ev_in)) but needs one less than dv's length, $n)"))
            end
            ev = [ev_in; zeros($elty,1)]
            ldz = n #Leading dimension
            #Number of eigenvalues to find
            if !(1 <= length(w_in) <= n)
                throw(DimensionMismatch("w_in has length $(length(w_in)), but needs to be between 1 and $n"))
            end
            m = length(w_in)
            #If iblock and isplit are invalid input, assume worst-case block paritioning,
            # i.e. set the block scheme to be the entire matrix
            iblock = similar(dv, BlasInt,n)
            isplit = similar(dv, BlasInt,n)
            w = similar(dv, $elty,n)
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

            z = similar(dv, $elty,(n,m))
            work = Array($elty, 5*n)
            iwork = Array(BlasInt,n)
            ifail = Array(BlasInt,m)
            info = Array(BlasInt,1)

            ccall(($(blasfunc(stein)), liblapack), Void,
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
stegr!(jobz::Char, dv::Vector, ev::Vector) = stegr!(jobz, 'A', dv, ev, 0.0, 0.0, 0, 0)

# Allow user to skip specification of iblock and isplit
stein!(dv::Vector, ev::Vector, w_in::Vector)=stein!(dv, ev, w_in, zeros(BlasInt,0), zeros(BlasInt,0))
# Allow user to specify just one eigenvector to get in stein!
stein!(dv::Vector, ev::Vector, eval::Real)=stein!(dv, ev, [eval], zeros(BlasInt,0), zeros(BlasInt,0))

"""
    stev!(job, dv, ev) -> (dv, Zmat)

Computes the eigensystem for a symmetric tridiagonal matrix with `dv` as
diagonal and `ev` as off-diagonal. If `job = N` only the eigenvalues are
found and returned in `dv`. If `job = V` then the eigenvectors are also found
and returned in `Zmat`.
"""
stev!(job::Char, dv::Vector, ev::Vector)

"""
    stebz!(range, order, vl, vu, il, iu, abstol, dv, ev) -> (dv, iblock, isplit)

Computes the eigenvalues for a symmetric tridiagonal matrix with `dv` as
diagonal and `ev` as off-diagonal. If `range = A`, all the eigenvalues
are found. If `range = V`, the eigenvalues in the half-open interval
`(vl, vu]` are found. If `range = I`, the eigenvalues with indices between
`il` and `iu` are found. If `order = B`, eigvalues are ordered within a
block. If `order = E`, they are ordered across all the blocks.
`abstol` can be set as a tolerance for convergence.
"""
stebz!(range::Char, order::Char, vl, vu, il::Integer, iu::Integer, abstol::Real, dv::Vector, ev::Vector)

"""
    stegr!(jobz, range, dv, ev, vl, vu, il, iu) -> (w, Z)

Computes the eigenvalues (`jobz = N`) or eigenvalues and eigenvectors
(`jobz = V`) for a symmetric tridiagonal matrix with `dv` as diagonal
and `ev` as off-diagonal. If `range = A`, all the eigenvalues
are found. If `range = V`, the eigenvalues in the half-open interval
`(vl, vu]` are found. If `range = I`, the eigenvalues with indices between
`il` and `iu` are found. The eigenvalues are returned in `w` and the eigenvectors
in `Z`.
"""
stegr!(jobz::Char, range::Char, dv::Vector, ev::Vector, vl::Real, vu::Real, il::Integer, iu::Integer)

"""
    stein!(dv, ev_in, w_in, iblock_in, isplit_in)

Computes the eigenvectors for a symmetric tridiagonal matrix with `dv`
as diagonal and `ev_in` as off-diagonal. `w_in` specifies the input
eigenvalues for which to find corresponding eigenvectors. `iblock_in`
specifies the submatrices corresponding to the eigenvalues in `w_in`.
`isplit_in` specifies the splitting points between the submatrix blocks.
"""
stein!(dv::Vector, ev_in::Vector, w_in::Vector, iblock_in::Vector{BlasInt}, isplit_in::Vector{BlasInt})

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
        function syconv!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(syconv)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function sysv!(uplo::Char, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            chkuplo(uplo)
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)), but needs $n"))
            end
            ipiv  = similar(A, BlasInt, n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(sysv)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)),
                      work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function sytrf!(uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            ipiv  = similar(A, BlasInt, n)
            if n == 0
                return A, ipiv
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(sytrf)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, A, &stride(A,2), ipiv, work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
#         function sytri!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
#             chkstride1(A)
#             n = chksquare(A)
#             chkuplo(uplo)
#             work  = Array($elty, 1)
#             lwork = BlasInt(-1)
#             info  = Array(BlasInt, 1)
#             for i in 1:2
#                 ccall(($(blasfunc(sytri)), liblapack), Void,
#                       (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
#                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
#                       &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
#                 @assertargsok
#                 @assertnonsingular
#                 if lwork < 0
#                     lwork = BlasInt(real(work[1]))
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
        function sytri!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(sytri)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function sytrs!(uplo::Char, A::StridedMatrix{$elty},
                       ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            chkuplo(uplo)
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)), but needs $n"))
            end
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(sytrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function syconv!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(syconv)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function hesv!(uplo::Char, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            chkuplo(uplo)
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)), but needs $n"))
            end
            ipiv  = similar(A, BlasInt, n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(hesv)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)),
                      work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function hetrf!(uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            ipiv  = similar(A, BlasInt, n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(hetrf)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
#         function hetri!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
#             chkstride1(A)
#             n = chksquare(A)
#             chkuplo(uplo)
#             work  = Array($elty, 1)
#             lwork = BlasInt(-1)
#             info  = Array(BlasInt, 1)
#             for i in 1:2
#                 ccall(($(blasfunc(hetri)), liblapack), Void,
#                       (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
#                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
#                       &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
#                 @lapackerror
#                 if lwork < 0
#                     lwork = BlasInt(real(work[1]))
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
        function hetri!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(hetri)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function hetrs!(uplo::Char, A::StridedMatrix{$elty},
                       ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)), but needs $n"))
            end
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(hetrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function sysv!(uplo::Char, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            chkuplo(uplo)
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)), but needs $n"))
            end
            ipiv  = similar(A, BlasInt, n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(sysv)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                       Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)),
                      work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function sytrf!(uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            ipiv  = similar(A, BlasInt, n)
            if n == 0
                return A, ipiv
            end
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(sytrf)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                       Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
                @assertargsok
                @assertnonsingular
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
#         function sytri!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
#             chkstride1(A)
#             n = chksquare(A)
#             chkuplo(uplo)
#             work  = Array($elty, 1)
#             lwork = BlasInt(-1)
#             info  = Array(BlasInt, 1)
#             for i in 1:2
#                 ccall(($(blasfunc(sytri)), liblapack), Void,
#                       (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
#                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
#                       &uplo, &n, A, &max(1,stride(A,2)), ipiv, work, &lwork, info)
#                 @lapackerror
#                 if lwork < 0
#                     lwork = BlasInt(real(work[1]))
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
        function sytri!(uplo::Char, A::StridedMatrix{$elty}, ipiv::Vector{BlasInt})
            chkstride1(A)
            n = chksquare(A)
            chkuplo(uplo)
            work  = Array($elty, n)
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(sytri)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function sytrs!(uplo::Char, A::StridedMatrix{$elty},
                       ipiv::Vector{BlasInt}, B::StridedVecOrMat{$elty})
            chkstride1(A,B)
            n = chksquare(A)
            chkuplo(uplo)
            if n != size(B,1)
                throw(DimensionMismatch("B has first dimension $(size(B,1)), but needs $n"))
            end
            info  = Array(BlasInt, 1)
            ccall(($(blasfunc(sytrs)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                  &uplo, &n, &size(B,2), A, &max(1,stride(A,2)), ipiv, B, &max(1,stride(B,2)), info)
            @lapackerror
            B
        end
    end
end

"""
    syconv!(uplo, A, ipiv) -> (A, work)

Converts a symmetric matrix `A` (which has been factorized into a
triangular matrix) into two matrices `L` and `D`. If `uplo = U`, `A`
is upper triangular. If `uplo = L`, it is lower triangular. `ipiv` is
the pivot vector from the triangular factorization. `A` is overwritten
by `L` and `D`.
"""
syconv!(uplo::Char, A::StridedMatrix, ipiv::Vector{BlasInt})

"""
    sysv!(uplo, A, B) -> (B, A, ipiv)

Finds the solution to `A * X = B` for symmetric matrix `A`. If `uplo = U`,
the upper half of `A` is stored. If `uplo = L`, the lower half is stored.
`B` is overwritten by the solution `X`. `A` is overwritten by its
Bunch-Kaufman factorization. `ipiv` contains pivoting information about the
factorization.
"""
sysv!(uplo::Char, A::StridedMatrix, B::StridedVecOrMat)

"""
    sytrf!(uplo, A) -> (A, ipiv)

Computes the Bunch-Kaufman factorization of a symmetric matrix `A`. If
`uplo = U`, the upper half of `A` is stored. If `uplo = L`, the lower
half is stored.

Returns `A`, overwritten by the factorization, and a pivot vector `ipiv`.
"""
sytrf!(uplo::Char, A::StridedMatrix)

"""
    sytri!(uplo, A, ipiv)

Computes the inverse of a symmetric matrix `A` using the results of
`sytrf!`. If `uplo = U`, the upper half of `A` is stored. If `uplo = L`,
the lower half is stored. `A` is overwritten by its inverse.
"""
sytri!(uplo::Char, A::StridedMatrix, ipiv::Vector{BlasInt})

"""
    sytrs!(uplo, A, ipiv, B)

Solves the equation `A * X = B` for a symmetric matrix `A` using the
results of `sytrf!`. If `uplo = U`, the upper half of `A` is stored.
If `uplo = L`, the lower half is stored. `B` is overwritten by the
solution `X`.
"""
sytrs!(uplo::Char, A::StridedMatrix, ipiv::Vector{BlasInt}, B::StridedVecOrMat)


"""
    hesv!(uplo, A, B) -> (B, A, ipiv)

Finds the solution to `A * X = B` for Hermitian matrix `A`. If `uplo = U`,
the upper half of `A` is stored. If `uplo = L`, the lower half is stored.
`B` is overwritten by the solution `X`. `A` is overwritten by its
Bunch-Kaufman factorization. `ipiv` contains pivoting information about the
factorization.
"""
hesv!(uplo::Char, A::StridedMatrix, B::StridedVecOrMat)

"""
    hetrf!(uplo, A) -> (A, ipiv)

Computes the Bunch-Kaufman factorization of a Hermitian matrix `A`. If
`uplo = U`, the upper half of `A` is stored. If `uplo = L`, the lower
half is stored.

Returns `A`, overwritten by the factorization, and a pivot vector.
"""
hetrf!(uplo::Char, A::StridedMatrix)

"""
    hetri!(uplo, A, ipiv)

Computes the inverse of a Hermitian matrix `A` using the results of
`sytrf!`. If `uplo = U`, the upper half of `A` is stored. If `uplo = L`,
the lower half is stored. `A` is overwritten by its inverse.
"""
hetri!(uplo::Char, A::StridedMatrix, ipiv::Vector{BlasInt})

"""
    hetrs!(uplo, A, ipiv, B)

Solves the equation `A * X = B` for a Hermitian matrix `A` using the
results of `sytrf!`. If `uplo = U`, the upper half of `A` is stored.
If `uplo = L`, the lower half is stored. `B` is overwritten by the
solution `X`.
"""
hetrs!(uplo::Char, A::StridedMatrix, ipiv::Vector{BlasInt}, B::StridedVecOrMat)

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
        function syev!(jobz::Char, uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            W     = similar(A, $elty, n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(syev)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                      Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
                      &jobz, &uplo, &n, A, &max(1,stride(A,2)), W, work, &lwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function syevr!(jobz::Char, range::Char, uplo::Char, A::StridedMatrix{$elty}, vl::AbstractFloat, vu::AbstractFloat, il::Integer, iu::Integer, abstol::AbstractFloat)
            chkstride1(A)
            n = chksquare(A)
            if range == 'I' && !(1 <= il <= iu <= n)
                throw(ArgumentError("illegal choice of eigenvalue indices (il = $il, iu = $iu), which must be between 1 and n = $n"))
            end
            if range == 'V' && vl >= vu
                throw(ArgumentError("Lower boundary, $vl, must be less than upper boundary, $vu"))
            end
            lda = stride(A,2)
            m = Array(BlasInt, 1)
            w = similar(A, $elty, n)
            ldz = n
            if jobz == 'N'
                Z = similar(A, $elty, ldz, 0)
            elseif jobz == 'V'
                Z = similar(A, $elty, ldz, n)
            end
            isuppz = similar(A, BlasInt, 2*n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            iwork = Array(BlasInt, 1)
            liwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(syevr)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{BlasInt}),
                    &jobz, &range, &uplo, &n,
                    A, &max(1,lda), &vl, &vu,
                    &il, &iu, &abstol, m,
                    w, Z, &max(1,ldz), isuppz,
                    work, &lwork, iwork, &liwork,
                    info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            w[1:m[1]], Z[:,1:(jobz == 'V' ? m[1] : 0)]
        end
        syevr!(jobz::Char, A::StridedMatrix{$elty}) = syevr!(jobz, 'A', 'U', A, 0.0, 0.0, 0, 0, -1.0)
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
        function sygvd!(itype::Integer, jobz::Char, uplo::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A, B)
            n, m = chksquare(A, B)
            if n != m
                throw(DimensionMismatch("Dimensions of A, ($n,$n), and B, ($m,$m), must match"))
            end
            lda = max(1, stride(A, 2))
            ldb = max(1, stride(B, 2))
            w = similar(A, $elty, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            iwork = Array(BlasInt, 1)
            liwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(sygvd)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
                     Ptr{BlasInt}, Ptr{BlasInt}),
                    &itype, &jobz, &uplo, &n,
                    A, &lda, B, &ldb,
                    w, work, &lwork, iwork,
                    &liwork, info)
                if i == 1
                    lwork = BlasInt(work[1])
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            @assertargsok
            @assertposdef
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
        function syev!(jobz::Char, uplo::Char, A::StridedMatrix{$elty})
            chkstride1(A)
            n = chksquare(A)
            W     = similar(A, $relty, n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            rwork = Array($relty, max(1, 3n-2))
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(syev)), liblapack), Void,
                      (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                      Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                      &jobz, &uplo, &n, A, &stride(A,2), W, work, &lwork, rwork, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
        function syevr!(jobz::Char, range::Char, uplo::Char, A::StridedMatrix{$elty}, vl::AbstractFloat, vu::AbstractFloat, il::Integer, iu::Integer, abstol::AbstractFloat)
            chkstride1(A)
            n = chksquare(A)
            if range == 'I' && !(1 <= il <= iu <= n)
                throw(ArgumentError("illegal choice of eigenvalue indices (il = $il, iu=$iu), which must be between 1 and n = $n"))
            end
            if range == 'V' && vl >= vu
                throw(ArgumentError("Lower boundary, $vl, must be less than upper boundary, $vu"))
            end
            lda = max(1,stride(A,2))
            m = Array(BlasInt, 1)
            w = similar(A, $relty, n)
            if jobz == 'N'
                ldz = 1
                Z = similar(A, $elty, ldz, 0)
            elseif jobz == 'V'
                ldz = n
                Z = similar(A, $elty, ldz, n)
            end
            isuppz = similar(A, BlasInt, 2*n)
            work  = Array($elty, 1)
            lwork = BlasInt(-1)
            rwork = Array($relty, 1)
            lrwork = BlasInt(-1)
            iwork = Array(BlasInt, 1)
            liwork = BlasInt(-1)
            info  = Array(BlasInt, 1)
            for i in 1:2
                ccall(($(blasfunc(syevr)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt},
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
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                    lrwork = BlasInt(rwork[1])
                    rwork = Array($relty, lrwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                end
            end
            w[1:m[1]], Z[:,1:(jobz == 'V' ? m[1] : 0)]
        end
        syevr!(jobz::Char, A::StridedMatrix{$elty}) = syevr!(jobz, 'A', 'U', A, 0.0, 0.0, 0, 0, -1.0)
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
        function sygvd!(itype::Integer, jobz::Char, uplo::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty})
            chkstride1(A, B)
            n, m = chksquare(A, B)
            if n != m
                throw(DimensionMismatch("Dimensions of A, ($n,$n), and B, ($m,$m), must match"))
            end
            lda = max(1, stride(A, 2))
            ldb = max(1, stride(B, 2))
            w = similar(A, $relty, n)
            work = Array($elty, 1)
            lwork = -one(BlasInt)
            iwork = Array(BlasInt, 1)
            liwork = -one(BlasInt)
            rwork = Array($relty)
            lrwork = -one(BlasInt)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(sygvd)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt},
                     Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{$relty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty},
                     Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}),
                    &itype, &jobz, &uplo, &n,
                    A, &lda, B, &ldb,
                    w, work, &lwork, rwork,
                    &lrwork, iwork, &liwork, info)
                if i == 1
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                    liwork = iwork[1]
                    iwork = Array(BlasInt, liwork)
                    lrwork = BlasInt(rwork[1])
                    rwork = Array($relty, lrwork)
                end
            end
            @assertargsok
            @assertposdef
            w, A, B
        end
    end
end

"""
    syev!(jobz, uplo, A)

Finds the eigenvalues (`jobz = N`) or eigenvalues and eigenvectors
(`jobz = V`) of a symmetric matrix `A`. If `uplo = U`, the upper triangle
of `A` is used. If `uplo = L`, the lower triangle of `A` is used.
"""
syev!(jobz::Char, uplo::Char, A::StridedMatrix)

"""
    syevr!(jobz, range, uplo, A, vl, vu, il, iu, abstol) -> (W, Z)

Finds the eigenvalues (`jobz = N`) or eigenvalues and eigenvectors
(`jobz = V`) of a symmetric matrix `A`. If `uplo = U`, the upper triangle
of `A` is used. If `uplo = L`, the lower triangle of `A` is used. If
`range = A`, all the eigenvalues are found. If `range = V`, the
eigenvalues in the half-open interval `(vl, vu]` are found.
If `range = I`, the eigenvalues with indices between `il` and `iu` are
found. `abstol` can be set as a tolerance for convergence.

The eigenvalues are returned in `W` and the eigenvectors in `Z`.
"""
syevr!(jobz::Char, range::Char, uplo::Char, A::StridedMatrix, vl::AbstractFloat, vu::AbstractFloat, il::Integer, iu::Integer, abstol::AbstractFloat)

"""
    sygvd!(jobz, range, uplo, A, vl, vu, il, iu, abstol) -> (w, A, B)

Finds the generalized eigenvalues (`jobz = N`) or eigenvalues and
eigenvectors (`jobz = V`) of a symmetric matrix `A` and symmetric
positive-definite matrix `B`. If `uplo = U`, the upper triangles
of `A` and `B` are used. If `uplo = L`, the lower triangles of `A` and
`B` are used. If `itype = 1`, the problem to solve is
`A * x = lambda * B * x`. If `itype = 2`, the problem to solve is
`A * B * x = lambda * x`. If `itype = 3`, the problem to solve is
`B * A * x = lambda * x`.
"""
sygvd!(itype::Integer, jobz::Char, uplo::Char, A::StridedMatrix, B::StridedMatrix)

## (BD) Bidiagonal matrices - singular value decomposition
for (bdsqr, relty, elty) in
    ((:dbdsqr_,:Float64,:Float64),
     (:sbdsqr_,:Float32,:Float32),
     (:zbdsqr_,:Float64,:Complex128),
     (:cbdsqr_,:Float32,:Complex64))
    @eval begin
        function bdsqr!(uplo::Char, d::Vector{$relty}, e_::Vector{$relty},
            Vt::StridedMatrix{$elty}, U::StridedMatrix{$elty}, C::StridedMatrix{$elty})

            # Extract number
            n = length(d)
            ncvt, nru, ncc = size(Vt, 2), size(U, 1), size(C, 2)
            ldvt, ldu, ldc = max(1, stride(Vt,2)), max(1, stride(U, 2)), max(1, stride(C,2))

            # Do checks
            chkuplo(uplo)
            if length(e_) != n - 1
                throw(DimensionMismatch("off-diagonal has length $(length(e_)) but should have length $(n - 1)"))
            end
            if ncvt > 0 && ldvt < n
                throw(DimensionMismatch("leading dimension of Vt, $ldvt, must be at least $n"))
            end
            if ldu < nru
                throw(DimensionMismatch("leading dimension of U, $ldu, must be at least $nru"))
            end
            if size(U, 2) != n
                throw(DimensionMismatch("U must have $n columns but has $(size(U, 2))"))
            end
            if ncc > 0 && ldc < n
                throw(DimensionMismatch("leading dimension of C, $ldc, must be at least $n"))
            end

            # Allocate
            work = Array($relty, 4n)
            info = Array(BlasInt,1)

            ccall(($(blasfunc(bdsqr)), liblapack), Void,
                (Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{BlasInt}, Ptr{$relty}, Ptr{$relty}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                 Ptr{BlasInt}, Ptr{$relty}, Ptr{BlasInt}),
                &uplo, &n, &ncvt, &nru,
                &ncc, d, e_, Vt,
                &ldvt, U, &ldu, C,
                &ldc, work, info)

            @lapackerror
            d, Vt, U, C #singular values in descending order, P**T * VT, U * Q, Q**T * C
        end
    end
end

"""
    bdsqr!(uplo, d, e_, Vt, U, C) -> (d, Vt, U, C)

Computes the singular value decomposition of a bidiagonal matrix with
`d` on the diagonal and `e_` on the off-diagonal. If `uplo = U`, `e_` is
the superdiagonal. If `uplo = L`, `e_` is the subdiagonal. Can optionally also
compute the product `Q' * C`.

Returns the singular values in `d`, and the matrix `C` overwritten with `Q' * C`.
"""
bdsqr!(uplo::Char, d::Vector, e_::Vector, Vt::StridedMatrix, U::StridedMatrix, C::StridedMatrix)

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
        function bdsdc!(uplo::Char, compq::Char, d::Vector{$elty}, e_::Vector{$elty})
            n, ldiq, ldq, ldu, ldvt = length(d), 1, 1, 1, 1
            chkuplo(uplo)
            if compq == 'N'
                lwork = 6n
            elseif compq == 'P'
                warn("COMPQ='P' is not tested")
                #TODO turn this into an actual LAPACK call
                #smlsiz=ilaenv(9, $elty==:Float64 ? 'dbdsqr' : 'sbdsqr', string(uplo, compq), n,n,n,n)
                smlsiz=100 #For now, completely overkill
                ldq = n*(11+2*smlsiz+8*round(Int,log((n/(smlsiz+1)))/log(2)))
                ldiq = n*(3+3*round(Int,log(n/(smlsiz+1))/log(2)))
                lwork = 6n
            elseif compq == 'I'
                ldvt=ldu=max(1, n)
                lwork=3*n^2 + 4n
            else
                throw(ArgumentError("COMPQ argument must be 'N', 'P' or 'I', got $(repr(compq))"))
            end
            u = similar(d, $elty, (ldu,  n))
            vt= similar(d, $elty, (ldvt, n))
            q = similar(d, $elty, ldq)
            iq= similar(d, BlasInt, ldiq)
            work =Array($elty, lwork)
            iwork=Array(BlasInt, 8n)
            info =Array(BlasInt, 1)
            ccall(($(blasfunc(bdsdc)), liblapack), Void,
           (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
            Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
            Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}),
            &uplo, &compq, &n, d, e_,
            u, &ldu, vt, &ldvt,
            q, iq, work, iwork, info)

            @lapackerror
            d, e, u, vt, q, iq
        end
    end
end

"""
    bdsdc!(uplo, compq, d, e_) -> (d, e, u, vt, q, iq)

Computes the singular value decomposition of a bidiagonal matrix with `d` on the
diagonal and `e_` on the off-diagonal using a divide and conqueq method.
If `uplo = U`, `e_` is the superdiagonal. If `uplo = L`, `e_` is the subdiagonal.
If `compq = N`, only the singular values are found. If `compq = I`, the singular
values and vectors are found. If `compq = P`, the singular values
and vectors are found in compact form. Only works for real types.

Returns the singular values in `d`, and if `compq = P`, the compact singular
vectors in `iq`.
"""
bdsdc!(uplo::Char, compq::Char, d::Vector, e_::Vector)

for (gecon, elty) in
    ((:dgecon_,:Float64),
     (:sgecon_,:Float32))
    @eval begin
        function gecon!(normtype::Char, A::StridedMatrix{$elty}, anorm::$elty)
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
            n = chksquare(A)
            lda = max(1, stride(A, 2))
            rcond = Array($elty, 1)
            work = Array($elty, 4n)
            iwork = Array(BlasInt, n)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(gecon)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
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
        function gecon!(normtype::Char, A::StridedMatrix{$elty}, anorm::$relty)
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
            n = chksquare(A)
            lda = max(1, stride(A, 2))
            rcond = Array($relty, 1)
            work = Array($elty, 2n)
            rwork = Array($relty, 2n)
            info = Array(BlasInt, 1)
            ccall(($(blasfunc(gecon)), liblapack), Void,
                  (Ptr{UInt8}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                   Ptr{$relty}, Ptr{$relty}, Ptr{$elty}, Ptr{$relty},
                   Ptr{BlasInt}),
                  &normtype, &n, A, &lda, &anorm, rcond, work, rwork,
                  info)
            @lapackerror
            rcond[1]
        end
    end
end

"""
    gecon!(normtype, A, anorm)

Finds the reciprocal condition number of matrix `A`. If `normtype = I`,
the condition number is found in the infinity norm. If `normtype = O` or
`1`, the condition number is found in the one norm. `A` must be the
result of `getrf!` and `anorm` is the norm of `A` in the relevant norm.
"""
gecon!(normtype::Char, A::StridedMatrix, anorm)

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
            tau = similar(A, $elty, max(0,n - 1))
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(gehrd)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{BlasInt}),
                    &n, &ilo, &ihi, A,
                    &max(1, stride(A, 2)), tau, work, &lwork,
                    info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A, tau
        end
    end
end
gehrd!(A::StridedMatrix) = gehrd!(1, size(A, 1), A)

"""
    gehrd!(ilo, ihi, A) -> (A, tau)

Converts a matrix `A` to Hessenberg form. If `A` is balanced with `gebal!`
then `ilo` and `ihi` are the outputs of `gebal!`. Otherwise they should be
`ilo = 1` and `ihi = size(A,2)`. `tau` contains the elementary reflectors of
the factorization.
"""
gehrd!(ilo::Integer, ihi::Integer, A::StridedMatrix)

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
            if n - length(tau) != 1
                throw(DimensionMismatch("tau has length $(length(tau)), needs $(n - 1)"))
            end
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(orghr)), liblapack), Void,
                    (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                     Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt},
                     Ptr{BlasInt}),
                    &n, &ilo, &ihi, A,
                    &max(1, stride(A, 2)), tau, work, &lwork,
                    info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            A
        end
    end
end

"""
    orghr!(ilo, ihi, A, tau)

Explicitly finds `Q`, the orthogonal/unitary matrix from `gehrd!`. `ilo`,
`ihi`, `A`, and `tau` must correspond to the input/output to `gehrd!`.
"""
orghr!(ilo::Integer, ihi::Integer, A::StridedMatrix, tau::StridedVector)

for (gees, gges, elty) in
    ((:dgees_,:dgges_,:Float64),
     (:sgees_,:sgges_,:Float32))
    @eval begin
        function gees!(jobvs::Char, A::StridedMatrix{$elty})
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
            wr = similar(A, $elty, n)
            wi = similar(A, $elty, n)
            ldvs = jobvs == 'V' ? n : 1
            vs = similar(A, $elty, ldvs, n)
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(gees)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{Void}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{Void}, Ptr{BlasInt}),
                    &jobvs, &'N', C_NULL, &n,
                        A, &max(1, stride(A, 2)), sdim, wr,
                        wi, vs, &ldvs, work,
                        &lwork, C_NULL, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
            if n != m
                throw(DimensionMismatch("Dimensions of A, ($n,$n), and B, ($m,$m), must match"))
            end
            sdim = BlasInt(0)
            alphar = similar(A, $elty, n)
            alphai = similar(A, $elty, n)
            beta = similar(A, $elty, n)
            ldvsl = jobvsl == 'V' ? n : 1
            vsl = similar(A, $elty, ldvsl, n)
            ldvsr = jobvsr == 'V' ? n : 1
            vsr = similar(A, $elty, ldvsr, n)
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(gges)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{Void},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                        Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{Void},
                        Ptr{BlasInt}),
                    &jobvsl, &jobvsr, &'N', C_NULL,
                    &n, A, &max(1,stride(A, 2)), B,
                    &max(1,stride(B, 2)), &sdim, alphar, alphai,
                    beta, vsl, &ldvsl, vsr,
                    &ldvsr, work, &lwork, C_NULL,
                    info)
                if i == 1
                    lwork = BlasInt(real(work[1]))
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
        function gees!(jobvs::Char, A::StridedMatrix{$elty})
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
            sdim = BlasInt(0)
            w = similar(A, $elty, n)
            ldvs = jobvs == 'V' ? n : 1
            vs = similar(A, $elty, ldvs, n)
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            rwork = Array($relty, n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(gees)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{Void}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{$relty}, Ptr{Void}, Ptr{BlasInt}),
                    &jobvs, &sort, C_NULL, &n,
                        A, &max(1, stride(A, 2)), &sdim, w,
                        vs, &ldvs, work, &lwork,
                        rwork, C_NULL, info)
                @lapackerror
                if lwork < 0
                    lwork = BlasInt(real(work[1]))
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
            if n != m
                throw(DimensionMismatch("Dimensions of A, ($n,$n), and B, ($m,$m), must match"))
            end
            sdim = BlasInt(0)
            alpha = similar(A, $elty, n)
            beta = similar(A, $elty, n)
            ldvsl = jobvsl == 'V' ? n : 1
            vsl = similar(A, $elty, ldvsl, n)
            ldvsr = jobvsr == 'V' ? n : 1
            vsr = similar(A, $elty, ldvsr, n)
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            rwork = Array($relty, 8n)
            info = Array(BlasInt, 1)
            for i = 1:2
                ccall(($(blasfunc(gges)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{UInt8}, Ptr{Void},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$relty}, Ptr{Void},
                        Ptr{BlasInt}),
                    &jobvsl, &jobvsr, &'N', C_NULL,
                    &n, A, &max(1, stride(A, 2)), B,
                    &max(1, stride(B, 2)), &sdim, alpha, beta,
                    vsl, &ldvsl, vsr, &ldvsr,
                    work, &lwork, rwork, C_NULL,
                    info)
                if i == 1
                    lwork = BlasInt(real(work[1]))
                    work = Array($elty, lwork)
                end
            end
            @lapackerror
            A, B, alpha, beta, vsl[1:(jobvsl == 'V' ? n : 0),:], vsr[1:(jobvsr == 'V' ? n : 0),:]
        end
    end
end

"""
    gees!(jobvs, A) -> (A, vs, w)

Computes the eigenvalues (`jobvs = N`) or the eigenvalues and Schur
vectors (`jobvs = V`) of matrix `A`. `A` is overwritten by its Schur form.

Returns `A`, `vs` containing the Schur vectors, and `w`, containing the
eigenvalues.
"""
gees!(jobvs::Char, A::StridedMatrix)


"""
    gges!(jobvsl, jobvsr, A, B) -> (A, B, alpha, beta, vsl, vsr)

Computes the generalized eigenvalues, generalized Schur form, left Schur
vectors (`jobsvl = V`), or right Schur vectors (`jobvsr = V`) of `A` and
`B`.

The generalized eigenvalues are returned in `alpha` and `beta`. The left Schur
vectors are returned in `vsl` and the right Schur vectors are returned in `vsr`.
"""
gges!(jobvsl::Char, jobvsr::Char, A::StridedMatrix, B::StridedMatrix)

for (trsen, tgsen, elty) in
    ((:dtrsen_, :dtgsen_, :Float64),
     (:strsen_, :stgsen_, :Float32))
    @eval begin
        function trsen!(select::StridedVector{BlasInt}, T::StridedMatrix{$elty}, Q::StridedMatrix{$elty})
# *     .. Scalar Arguments ..
#       CHARACTER          COMPQ, JOB
#       INTEGER            INFO, LDQ, LDT, LIWORK, LWORK, M, N
#       DOUBLE PRECISION   S, SEP
# *     ..
# *     .. Array Arguments ..
#       LOGICAL            SELECT( * )
#       INTEGER            IWORK( * )
#       DOUBLE PRECISION   Q( LDQ, * ), T( LDT, * ), WI( * ), WORK( * ), WR( * )
            chkstride1(T, Q)
            n = chksquare(T)
            ldt = max(1, stride(T, 2))
            ldq = max(1, stride(Q, 2))
            wr = similar(T, $elty, n)
            wi = similar(T, $elty, n)
            m = sum(select)
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            iwork = Array(BlasInt, 1)
            liwork = BlasInt(-1)
            info = Array(BlasInt, 1)
            select = convert(Array{BlasInt}, select)

            for i = 1:2
                ccall(($(blasfunc(trsen)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{$elty}, Ptr{BlasInt}, Ptr{Void}, Ptr{Void},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{BlasInt}),
                    &'N', &'V', select, &n,
                    T, &ldt, Q, &ldq,
                    wr, wi, &m, C_NULL, C_NULL,
                    work, &lwork, iwork, &liwork,
                    info)
                @lapackerror
                if i == 1 # only estimated optimal lwork, liwork
                    lwork  = BlasInt(real(work[1]))
                    liwork = BlasInt(real(iwork[1]))
                    work   = Array($elty, lwork)
                    iwork  = Array(BlasInt, liwork)
                end
            end
            T, Q, all(wi .== 0) ? wr : complex(wr, wi)
        end
        function tgsen!(select::StridedVector{BlasInt}, S::StridedMatrix{$elty}, T::StridedMatrix{$elty},
                                            Q::StridedMatrix{$elty}, Z::StridedMatrix{$elty})
# *       .. Scalar Arguments ..
# *       LOGICAL            WANTQ, WANTZ
# *       INTEGER            IJOB, INFO, LDA, LDB, LDQ, LDZ, LIWORK, LWORK,
# *      $                   M, N
# *       DOUBLE PRECISION   PL, PR
# *       ..
# *       .. Array Arguments ..
# *       LOGICAL            SELECT( * )
# *       INTEGER            IWORK( * )
# *       DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
# *      $                   B( LDB, * ), BETA( * ), DIF( * ), Q( LDQ, * ),
# *      $                   WORK( * ), Z( LDZ, * )
# *       ..
            chkstride1(S, T, Q, Z)
            n, nt, nq, nz = chksquare(S, T, Q, Z)
            if n != nt
                throw(DimensionMismatch("Dimensions of S, ($n,$n), and T, ($nt,$nt), must match"))
            end
            if n != nq
                throw(DimensionMismatch("Dimensions of S, ($n,$n), and Q, ($nq,$nq), must match"))
            end
            if n != nz
                throw(DimensionMismatch("Dimensions of S, ($n,$n), and Z, ($nz,$nz), must match"))
            end
            lds = max(1, stride(S, 2))
            ldt = max(1, stride(T, 2))
            ldq = max(1, stride(Q, 2))
            ldz = max(1, stride(Z, 2))
            m = sum(select)
            alphai = similar(T, $elty, n)
            alphar = similar(T, $elty, n)
            beta = similar(T, $elty, n)
            lwork = BlasInt(-1)
            work = Array($elty, 1)
            liwork = BlasInt(-1)
            iwork = Array(BlasInt, 1)
            info = Array(BlasInt, 1)
            select = convert(Array{BlasInt}, select)

            for i = 1:2
                ccall(($(blasfunc(tgsen)), liblapack), Void,
                       (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty}, Ptr{$elty},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{BlasInt}, Ptr{Void}, Ptr{Void}, Ptr{Void},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{BlasInt}),
                    &0, &1, &1, select,
                    &n, S, &lds, T,
                    &ldt, alphar, alphai, beta,
                    Q, &ldq, Z, &ldz,
                    &m, C_NULL, C_NULL, C_NULL,
                    work, &lwork, iwork, &liwork,
                    info)
                @lapackerror
                if i == 1 # only estimated optimal lwork, liwork
                    lwork  = BlasInt(real(work[1]))
                    work   = Array($elty, lwork)
                    liwork = BlasInt(real(iwork[1]))
                    iwork = Array(BlasInt, liwork)
                end
            end
            S, T, complex(alphar, alphai), beta, Q, Z
        end
    end
end

for (trsen, tgsen, elty) in
    ((:ztrsen_, :ztgsen_, :Complex128),
     (:ctrsen_, :ctgsen_, :Complex64))
    @eval begin
        function trsen!(select::StridedVector{BlasInt}, T::StridedMatrix{$elty}, Q::StridedMatrix{$elty})
# *     .. Scalar Arguments ..
#       CHARACTER          COMPQ, JOB
#       INTEGER            INFO, LDQ, LDT, LWORK, M, N
#       DOUBLE PRECISION   S, SEP
# *     ..
# *     .. Array Arguments ..
#       LOGICAL            SELECT( * )
#       COMPLEX            Q( LDQ, * ), T( LDT, * ), W( * ), WORK( * )
            chkstride1(T, Q)
            n = chksquare(T)
            ldt = max(1, stride(T, 2))
            ldq = max(1, stride(Q, 2))
            w = similar(T, $elty, n)
            m = sum(select)
            work = Array($elty, 1)
            lwork = BlasInt(-1)
            info = Array(BlasInt, 1)
            select = convert(Array{BlasInt}, select)

            for i = 1:2
                ccall(($(blasfunc(trsen)), liblapack), Void,
                    (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                    Ptr{$elty}, Ptr{BlasInt}, Ptr{Void}, Ptr{Void},
                    Ptr{$elty}, Ptr{BlasInt},
                    Ptr{BlasInt}),
                    &'N', &'V', select, &n,
                    T, &ldt, Q, &ldq,
                    w, &m, C_NULL, C_NULL,
                    work, &lwork,
                    info)
                @lapackerror
                if i == 1 # only estimated optimal lwork, liwork
                    lwork  = BlasInt(real(work[1]))
                    work   = Array($elty, lwork)
                end
            end
            T, Q, w
        end
        function tgsen!(select::StridedVector{BlasInt}, S::StridedMatrix{$elty}, T::StridedMatrix{$elty},
                                            Q::StridedMatrix{$elty}, Z::StridedMatrix{$elty})
# *       .. Scalar Arguments ..
# *       LOGICAL            WANTQ, WANTZ
# *       INTEGER            IJOB, INFO, LDA, LDB, LDQ, LDZ, LIWORK, LWORK,
# *      $                   M, N
# *       DOUBLE PRECISION   PL, PR
# *       ..
# *       .. Array Arguments ..
# *       LOGICAL            SELECT( * )
# *       INTEGER            IWORK( * )
# *       DOUBLE PRECISION   DIF( * )
# *       COMPLEX*16         A( LDA, * ), ALPHA( * ), B( LDB, * ),
# *      $                   BETA( * ), Q( LDQ, * ), WORK( * ), Z( LDZ, * )
# *       ..
            chkstride1(S, T, Q, Z)
            n, nt, nq, nz = chksquare(S, T, Q, Z)
            if n != nt
                throw(DimensionMismatch("Dimensions of S, ($n,$n), and T, ($nt,$nt), must match"))
            end
            if n != nq
                throw(DimensionMismatch("Dimensions of S, ($n,$n), and Q, ($nq,$nq), must match"))
            end
            if n != nz
                throw(DimensionMismatch("Dimensions of S, ($n,$n), and Z, ($nz,$nz), must match"))
            end
            lds = max(1, stride(S, 2))
            ldt = max(1, stride(T, 2))
            ldq = max(1, stride(Q, 2))
            ldz = max(1, stride(Z, 2))
            m = sum(select)
            alpha = similar(T, $elty, n)
            beta = similar(T, $elty, n)
            lwork = BlasInt(-1)
            work = Array($elty, 1)
            liwork = BlasInt(-1)
            iwork = Array(BlasInt, 1)
            info = Array(BlasInt, 1)
            select = convert(Array{BlasInt}, select)

            for i = 1:2
                ccall(($(blasfunc(tgsen)), liblapack), Void,
                       (Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty},
                        Ptr{BlasInt}, Ptr{$elty}, Ptr{$elty},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                        Ptr{BlasInt}, Ptr{Void}, Ptr{Void}, Ptr{Void},
                        Ptr{$elty}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                        Ptr{BlasInt}),
                    &0, &1, &1, select,
                    &n, S, &lds, T,
                    &ldt, alpha, beta,
                    Q, &ldq, Z, &ldz,
                    &m, C_NULL, C_NULL, C_NULL,
                    work, &lwork, iwork, &liwork,
                    info)
                @lapackerror
                if i == 1 # only estimated optimal lwork, liwork
                    lwork  = BlasInt(real(work[1]))
                    work   = Array($elty, lwork)
                    liwork = BlasInt(real(iwork[1]))
                    iwork = Array(BlasInt, liwork)
                end
            end
            S, T, alpha, beta, Q, Z
        end
    end
end

"""
    trsen!(compq, job, select, T, Q) -> (T, Q, w)

Reorder the Schur factorization of a matrix and optionally finds reciprocal
condition numbers. If `job = N`, no condition numbers are found. If `job = E`,
only the condition number for this cluster of eigenvalues is found. If
`job = V`, only the condition number for the invariant subspace is found.
If `job = B` then the condition numbers for the cluster and subspace are
found. If `compq = V` the Schur vectors `Q` are updated. If `compq = N`
the Schur vectors are not modified. `select` determines which
eigenvalues are in the cluster.

Returns `T`, `Q`, and reordered eigenvalues in `w`.
"""
trsen!(compq::Char, job::Char, select::StridedVector{BlasInt}, T::StridedMatrix, Q::StridedMatrix)

"""
    tgsen!(select, S, T, Q, Z) -> (S, T, alpha, beta, Q, Z)

Reorders the vectors of a generalized Schur decomposition. `select` specifices
the eigenvalues in each cluster.
"""
tgsen!(select::StridedVector{BlasInt}, S::StridedMatrix, T::StridedMatrix, Q::StridedMatrix, Z::StridedMatrix)

for (fn, elty, relty) in ((:dtrsyl_, :Float64, :Float64),
                   (:strsyl_, :Float32, :Float32),
                   (:ztrsyl_, :Complex128, :Float64),
                   (:ctrsyl_, :Complex64, :Float32))
    @eval begin
        function trsyl!(transa::Char, transb::Char, A::StridedMatrix{$elty}, B::StridedMatrix{$elty}, C::StridedMatrix{$elty}, isgn::Int=1)
            chkstride1(A, B, C)
            m, n = chksquare(A, B)
            lda = max(1, stride(A, 2))
            ldb = max(1, stride(B, 2))
            m1, n1 = size(C)
            if m != m1 || n != n1
                throw(DimensionMismatch("Dimensions of A, ($m,$n), and C, ($m1,$n1), must match"))
            end
            ldc = max(1, stride(C, 2))

            scale = Array($relty, 1)
            info = Array(BlasInt, 1)

            ccall(($(blasfunc(fn)), liblapack), Void,
                (Ptr{UInt8}, Ptr{UInt8}, Ptr{BlasInt}, Ptr{BlasInt}, Ptr{BlasInt},
                 Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt}, Ptr{$elty}, Ptr{BlasInt},
                 Ptr{$relty}, Ptr{BlasInt}),
                &transa, &transb, &isgn, &m, &n,
                A, &lda, B, &ldb, C, &ldc,
                scale, info)
            @lapackerror
            C, scale[1]
        end
    end
end

"""
    trsyl!(transa, transb, A, B, C, isgn=1) -> (C, scale)

Solves the Sylvester matrix equation `A * X +/- X * B = scale*C` where `A` and
`B` are both quasi-upper triangular. If `transa = N`, `A` is not modified.
If `transa = T`, `A` is transposed. If `transa = C`, `A` is conjugate
transposed. Similarly for `transb` and `B`. If `isgn = 1`, the equation
`A * X + X * B = scale * C` is solved. If `isgn = -1`, the equation
`A * X - X * B = scale * C` is solved.

Returns `X` (overwriting `C`) and `scale`.
"""
trsyl!(transa::Char, transb::Char, A::StridedMatrix, B::StridedMatrix, C::StridedMatrix, isgn::Int=1)

end # module
