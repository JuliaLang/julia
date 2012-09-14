## solvers and inverses using factorizations
for (getrs, potrs, getri, potri, trtri, elty) in
    (("dgetrs_","dpotrs_","dgetri_","dpotri_","dtrtri_",:Float64),
     ("sgetrs_","spotrs_","sgetri_","spotri_","strtri_",:Float32),
     ("zgetrs_","zpotrs_","zgetri_","zpotri_","ztrtri_",:Complex128),
     ("cgetrs_","cpotrs_","cgetri_","cpotri_","ctrtri_",:Complex64))
    
    @eval begin
        #     SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          TRANS
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      INTEGER            IPIV( * )
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_getrs(trans::LapackChar, A::StridedMatrix{$elty}, ipiv::Vector{Int32}, B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_getrs: matrix columns must have contiguous elements")
            end
            m, n    = size(A)
            if m != n || size(B, 1) != m error("_jl_lapack_getrs: dimension mismatch") end
            nrhs    = size(B, 2)
            lda     = stride(A, 2)
            ldb     = isa(B, Vector) ? m : stride(B, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $getrs),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &trans, &n, &nrhs, A, &lda, ipiv, B, &ldb, info)
            if info[1] != 0 error("_jl_lapack_getrs: error $(info[1])") end
            B
        end
        #     SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_potrs(uplo::LapackChar, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_potrs: matrix columns must have contiguous elements")
            end
            m, n    = size(A)
            if m != n || size(B,1) != m error("_jl_lapack_potrs: dimension mismatch") end
            nrhs    = size(B, 2)
            lda     = stride(A, 2)
            ldb     = isa(B, Vector) ? m : stride(B, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $potrs),
                  Void,
                  (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  &uplo, &n, &nrhs, A, &lda, B, &ldb, info)
            if info[1] != 0 error("_jl_lapack_potrs: error $(info[1])") end
            B
        end
        #     SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
        #*     .. Scalar Arguments ..
        #      INTEGER            INFO, LDA, LWORK, N
        #*     .. Array Arguments ..
        #      INTEGER            IPIV( * )
        #      DOUBLE PRECISION   A( LDA, * ), WORK( * )
        function _jl_lapack_getri(A::StridedMatrix{$elty}, ipiv::Vector{Int32})
            if stride(A,1) != 1
                error("_jl_lapack_getri: matrix columns must have contiguous elements");
            end
            m, n    = size(A)
            if m != n || n != numel(ipiv) error("_jl_lapack_getri: dimension mismatch") end
            lda     = stride(A, 2)
            info    = Array(Int32, 1)
            lwork   = -1
            work    = Array($elty, 1)
            for i in 1:2
                ccall(dlsym(_jl_liblapack, $getri),
                      Void,
                      (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32},
                       Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                      &n, A, &lda, ipiv, work, &lwork, info)
                if info[1] != 0 error("_jl_lapack_getri: error $(info[1])") end
                if lwork < 0
                    lwork = int32(real(work[1]))
                    work  = Array($elty, lwork)
                end
            end
            A
        end
        #     SUBROUTINE DTRTRI( UPLO, DIAG, N, A, LDA, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          DIAG, UPLO
        #      INTEGER            INFO, LDA, N
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * )
        function _jl_lapack_trtri(uplo::LapackChar, diag::LapackChar, A::StridedMatrix{$elty})
            if stride(A,1) != 1
                error("_jl_lapack_trtri: matrix columns must have contiguous elements");
            end
            m, n    = size(A)
            if m != n error("_jl_lapack_trtri: dimension mismatch") end
            lda     = stride(A, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $trtri),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32},
                   Ptr{Int32}),
                  &uplo, &diag, &n, A, &lda, info)
            if info[1] != 0 error("_jl_lapack_trtri: error $(info[1])") end
            A
        end
    end
end

abstract  Factorization{T}

type CholeskyDense{T} <: Factorization{T}
    LR::Matrix{T}
    uplo::LapackChar
end

# chol() does not check that input matrix is symmetric/hermitian
# It simply uses upper triangular half
function chol{T<:LapackScalar}(A::Matrix{T}, ul::LapackChar)
    if ul != 'U' && ul != 'L'; error("Cholesky: uplo must be 'U' or 'L'"); end
    C = CholeskyDense{T}(Array(T, size(A)), ul)
    chol(C, A)
end
chol{T<:Real}(A::Matrix{T}, ul::LapackChar) = chol(float64(A), ul)
chol(A) = chol(A, 'U')

function _chol{T<:LapackScalar}(C::CholeskyDense{T})
    if _jl_lapack_potrf(C.uplo, C.LR) != 0
        error("Cholesky: Matrix is not positive-definite")
    end
    if C.uplo == 'U'
        triu!(C.LR)
    else
        tril!(C.LR)
    end
    C
end
function chol{T}(C::CholeskyDense{T}, A::Matrix{T})
    copy_to(C.LR, A)
    _chol(C)
end
function chol!{T<:LapackScalar}(A::Matrix{T}, ul::LapackChar)
    C = CholeskyDense{T}(A, ul)
    _chol(C)
end
    
factors(C::CholeskyDense) = C.LR


(\){T<:LapackScalar}(C::CholeskyDense{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_potrs(C.uplo, C.LR, copy(B))

inv{T<:LapackScalar}(C::CholeskyDense{T}) = _jl_lapack_potri(C.uplo, copy(C.LR)) # should symmetrize the result
 
type LUDense{T} <: Factorization{T}
    lu::Matrix{T}
    ipiv::Vector{Int32}
    function LUDense(lu::Matrix{T}, ipiv::Vector{Int32})
        m, n = size(lu)
        m == numel(ipiv) ? new(lu, ipiv) : error("LUDense: dimension mismatch")
    end
end
size(A::LUDense) = size(A.lu)
size(A::LUDense,n) = size(A.lu,n)

lu{T<:LapackScalar}(A::Matrix{T}) = lu!(copy(A))
function lu!{T<:LapackScalar}(A::Matrix{T})
    lu, ipiv = _jl_lapack_getrf(A)
    LUDense{T}(lu, ipiv)
end

lu{T<:Real}(A::Matrix{T}) = lu(float64(A))

function det(lu::LUDense)
    m, n = size(lu.lu)
    if m != n error("det only defined for square matrices") end
    prod(diag(lu.lu)) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -1 : 1)
end

det(A::Matrix) = det(lu(A))

(\){T<:LapackScalar}(lu::LUDense{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_getrs('N', lu.lu, lu.ipiv, copy(B))

inv{T<:LapackScalar}(lu::LUDense{T}) = _jl_lapack_getri(copy(lu.lu), lu.ipiv)

function factors{T<:LapackScalar}(lu::LUDense{T}) 
    LU, ipiv = lu.lu, lu.ipiv
    m, n = size(LU)

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

## Multiplication by Q or Q' from a QR factorization
for (orm2r, elty) in
    (("dorm2r_",:Float64),
     ("sorm2r_",:Float32),
     ("zunm2r_",:Complex128),
     ("cunm2r_",:Complex64))
    @eval begin
        #      SUBROUTINE DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
        #                         WORK, INFO )
        #      .. Scalar Arguments ..
        #      CHARACTER          SIDE, TRANS
        #      INTEGER            INFO, K, LDA, LDC, M, N
        #      .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
        function _jl_lapack_orm2r(side::LapackChar, trans::LapackChar, A::StridedMatrix{$elty}, k::Integer, tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(C,1) != 1
                error("_jl_lapack_orm2r: matrix columns must have contiguous elements");
            end
            m    = size(C, 1)
            n    = size(C, 2)
            if size(A, 1) != m error("_jl_lapack_orm2r: dimension mismatch") end
            lda  = stride(A, 2)
            ldc  = isa(C, Vector) ? m : stride(C, 2)
            work = Array($elty, side == 'L' ? n : m)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $orm2r),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &side, &trans, &m, &n, &k, A, &lda, tau, C, &ldc, work, info)
            if info[1] != 0 error("_jl_lapack_orm2r: error $(info[1])") end
            C
        end
    end
end

abstract QRFactorization{T} <: Factorization{T}

## QR decomposition without column pivots
type QRDense{T} <: QRFactorization{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
    function QRDense(hh::Matrix{T}, tt::Vector{T})
        numel(tt) == min(size(hh)) ? new(hh, tt) : error("QR: mismatched dimensions")
    end
end
size(A::QRFactorization) = size(A.hh)
size(A::QRFactorization,n) = size(A.hh,n)

function qr{T<:LapackScalar}(A::StridedMatrix{T})
    hh, tt = _jl_lapack_geqrf(copy(A))
    QRDense{eltype(A)}(hh, tt)
end

qr{T<:Real}(x::StridedMatrix{T}) = qr(float64(x))

function factors{T<:LapackScalar}(qrd::QRDense{T})
    aa, tau = qrd.hh, qrd.tau
    R = triu(aa[1:min(size(qrd)),:])
    _jl_lapack_orgqr(aa, tau), R
end

## Multiplication by Q from the QR decomposition
(*){T<:LapackScalar}(A::QRFactorization{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_orm2r('L', 'N', A.hh, size(A, 2), A.tau, copy(B))

## Multiplication by Q' from the QR decomposition
Ac_mul_B{T<:LapackScalar}(A::QRFactorization{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_orm2r('L', iscomplex(A.tau) ? 'C' : 'T', A.hh, size(A, 2), A.tau, copy(B))

## Least squares solution.  Should be more careful about cases with m < n
function (\){T<:LapackScalar}(A::QRDense{T}, B::StridedVecOrMat{T})
    n   = numel(A.tau)
    qtb = isa(B, Vector) ? (A' * B)[1:n] : (A' * B)[1:n, :]
    ## Not sure if this avoids copying A.hh[1:n,:] but at least it is not all of A.hh
    _jl_lapack_trtrs('U','N','N', A.hh[1:n,:], qtb)
end

type QRPDense{T} <: QRFactorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    jpvt::Vector{Int32}
    function QRPDense(hh::Matrix{T}, tt::Vector{T}, jj::Vector{Int32})
        m, n = size(hh)
        numel(tt) == min(m,n) && numel(jj) == n ? new(hh,tt,jj) : error("QRPDense: mismatched dimensions")
    end
end

function qrp{T<:LapackScalar}(A::StridedMatrix{T})
    aa, tau, jpvt = _jl_lapack_geqp3(copy(A))
    QRPDense{T}(aa, tau, jpvt)
end
qrp{T<:Real}(x::StridedMatrix{T}) = qrp(float64(x))

function factors{T<:LapackScalar}(qrpd::QRPDense{T})
    aa, tau = qrpd.hh, qrpd.tau
    R = triu(aa[1:min(size(qrpd)),:])
    _jl_lapack_orgqr(aa, tau), R, qrpd.jpvt
end

# FIXME \
function (\){T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRPDense{T},
                                                             B::StridedVecOrMat{T})
    n = numel(A.tau)
    ## Replace this with a direct call to _jl_lapack_trtrs to save copying A.hh?
    ## Actually would need to call the appropriate Lapack subroutine to save copying.
    triu(A.hh[1:n,:]) \ (A' * B)[1:n]   
end

function (\){T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRPDense{T},
                                                             B::StridedVecOrMat{T})
    ## may be better to define one method for B::Vector{T} and another for StridedMatrix
    BV  = isa(B, Vector)
    n   = numel(A.tau)
    qtb = BV ? (A' * B)[1:n] : (A' * B)[1:n, :]
    ans = _jl_lapack_trtrs('U', 'N', 'N', A.hh[1:n,:], qtb)
    BV ? ans[invperm(A.jpvt)] : ans[invperm(A.jpvt), :]
end

##ToDo:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations


#### Factorizations for Tridiagonal ####
type LDLTTridiagonal{T} <: Factorization{T}
    D::Vector{T}
    E::Vector{T}
end
function ldlt{T<:LapackScalar}(A::Tridiagonal{T})
    D = copy(A.d)
    E = copy(A.dl)
    _jl_lapack_pttrf(D, E)
    LDLTTridiagonal{T}(D, E)
end

(\){T<:LapackScalar}(C::LDLTTridiagonal{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_pttrs(C.D, C.E, copy(B))

function factors{T<:LapackScalar}(C::LDLTTridiagonal{T})
    N = length(C.D)
    z = zeros(N-1)
    Tridiagonal(C.E, ones(N), z), Tridiagonal(z, C.D, z)
end

type LUTridiagonal{T} <: Factorization{T}
    lu::Tridiagonal{T}
    ipiv::Vector{Int32}
    function LUTridiagonal(lu::Tridiagonal{T}, ipiv::Vector{Int32})
        m, n = size(lu)
        m == numel(ipiv) ? new(lu, ipiv) : error("LUTridiagonal: dimension mismatch")
    end
end
show(io, lu::LUTridiagonal) = print(io, "LU decomposition of ", summary(lu.lu))

function lu{T<:LapackScalar}(A::Tridiagonal{T})
    lu, ipiv = _jl_lapack_gttrf(copy(A))
    LUTridiagonal{T}(lu, ipiv)
end

function det(lu::LUTridiagonal)
    prod(lu.lu.d) * (bool(sum(lu.ipiv .!= 1:n) % 2) ? -1 : 1)
end

det(A::Tridiagonal) = det(lu(A))

(\){T<:LapackScalar}(lu::LUTridiagonal{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_gttrs('N', lu.lu, lu.ipiv, copy(B))
