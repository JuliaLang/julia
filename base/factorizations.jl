## solvers and inverses using factorizations
for (getrs, potrs, trtrs, getri, potri, trtri, elty) in
    (("dgetrs_","dpotrs_","dtrtrs_","dgetri_","dpotri_","dtrtri_",:Float64),
     ("sgetrs_","spotrs_","strtrs_","sgetri_","spotri_","strtri_",:Float32),
     ("zgetrs_","zpotrs_","ztrtrs_","zgetri_","zpotri_","ztrtri_",:Complex128),
     ("cgetrs_","cpotrs_","ctrtrs_","cgetri_","cpotri_","ctrtri_",:Complex64))
    
    @eval begin
        #     SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          TRANS
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      INTEGER            IPIV( * )
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_getrs(trans::String, A::StridedMatrix{$elty}, ipiv::Vector{Int32}, B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_getrs: matrix columns must have contiguous elements");
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
                  trans, &n, &nrhs, A, &lda, ipiv, B, &ldb, info)
            if info[1] != 0 error("_jl_lapack_getrs: error $(info[1])") end
            B
        end
        #     SUBROUTINE DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
        #*     .. Scalar Arguments ..
        #      CHARACTER          UPLO
        #      INTEGER            INFO, LDA, LDB, N, NRHS
        #     .. Array Arguments ..
        #      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_potrs(uplo::String, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
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
                  uplo, &n, &nrhs, A, &lda, B, &ldb, info)
            if info[1] != 0 error("_jl_lapack_potrs: error $(info[1])") end
            B
        end
        #       SUBROUTINE DTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,
        #                          INFO )
        #       .. Scalar Arguments ..
        #       CHARACTER          DIAG, TRANS, UPLO
        #       INTEGER            INFO, LDA, LDB, N, NRHS
        #       .. Array Arguments ..
        #       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
        function _jl_lapack_trtrs(uplo::String, trans::String, diag::String, A::StridedMatrix{$elty}, B::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(B,1) != 1
                error("_jl_lapack_trtrs: matrix columns must have contiguous elements");
            end
            m, n    = size(A)
            if m != n || size(B, 1) != m error("_jl_lapack_trtrs: dimension mismatch") end
            nrhs    = size(B, 2)
            lda     = stride(A, 2)
            ldb     = isa(B, Vector) ? m : stride(B, 2)
            info    = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $trtrs),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                   Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{Int32}),
                  uplo, trans, diag, &n, &nrhs, A, &lda, B, &ldb, info)
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
        function _jl_lapack_trtri(uplo::String, diag::String, A::StridedMatrix{$elty})
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
                  uplo, diag, &n, A, &lda, info)
            if info[1] != 0 error("_jl_lapack_trtri: error $(info[1])") end
            A
        end
    end
end

abstract  Factorization{T}

type Cholesky{T} <: Factorization{T}
    LR::Matrix{T}
    uplo::String
    function Cholesky(A::Matrix{T}, ul::String)
        UL = uppercase(ul)
        if UL[1] != 'U' && UL[1] != 'L' error("Cholesky: uplo must be 'U' or 'L'") end
        Acopy = copy(A)
        _jl_lapack_potrf(UL, Acopy) == 0 ? new(UL[1] == 'U' ? triu(Acopy) : tril(Acopy), UL) : error("Cholesky: Matrix is not positive-definite")
    end
end

Cholesky(A::Matrix) = Cholesky{typeof(A[1])}(A, "U")

(\){T<:Union(Float64,Float32,Complex128,Complex64)}(C::Cholesky{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_potrs(C.uplo, C.LR, copy(B))

inv(C::Cholesky) = _jl_lapack_potri(C.uplo, copy(C.LR)) # should symmetrize the result
 
type LU{T} <: Factorization{T}
    lu::Matrix{T}
    ipiv::Vector{Int32}
    function LU(lu::Matrix{T}, ipiv::Vector{Int32})
        m, n = size(lu)
        m == numel(ipiv) ? new(lu, ipiv) : error("LU: dimension mismatch")
    end
end

function LU(A::Matrix)
    lu, ipiv = _jl_lapack_getrf(copy(A))
    LU{typeof(A[1])}(lu, ipiv)
end
 
(\){T<:Union(Float64,Float32,Complex128,Complex64)}(lu::LU{T}, B::StridedVecOrMat{T}) =
    _jl_lapack_getrs("N", lu.lu, lu.ipiv, copy(B))
inv(lu::LU) = _jl_lapack_getri(copy(lu.lu), lu.ipiv)

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
        function _jl_lapack_orm2r(side::String, trans::String, A::StridedMatrix{$elty}, k::Integer, tau::Vector{$elty}, C::StridedVecOrMat{$elty})
            if stride(A,1) != 1 || stride(C,1) != 1
                error("_jl_lapack_orm2r: matrix columns must have contiguous elements");
            end
            m    = size(C, 1)
            n    = size(C, 2)
            if size(A, 1) != m error("_jl_lapack_orm2r: dimension mismatch") end
            lda  = stride(A, 2)
            ldc  = isa(C, Vector) ? m : stride(C, 2)
            SIDE = uppercase(side)
            work = Array($elty, SIDE[1] == 'L' ? n : m)
            info = Array(Int32, 1)
            ccall(dlsym(_jl_liblapack, $orm2r),
                  Void,
                  (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{$elty},
                   Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  side, trans, &m, &n, &k, A, &lda, tau, C, &ldc, work, info)
            if info[1] != 0 error("_jl_lapack_orm2r: error $(info[1])") end
            C
        end
    end
end

type QR{T} <: Factorization{T}
    hh::Matrix{T}                       # Householder transformations and R
    tau::Vector{T}                      # Scalar factors of transformations
    function QR(hh::Matrix{T}, tt::Vector{T})
        numel(tt) == min(size(hh)) ? new(hh, tt) : error("QR: mismatched dimensions")
    end
end

function QR(A::Matrix)
    hh, tt = _jl_lapack_geqrf(copy(A))
    QR{typeof(A[1])}(hh, tt)
end

size{T<:Union(Float64,Float32,Complex128,Complex64)}(A::QR{T}) = size(A.hh)
size{T<:Union(Float64,Float32,Complex128,Complex64)}(A::QR{T},n) = size(A.hh,n)

## Multiplication by Q from the QR decomposition
function (*){T<:Union(Float64,Float32,Complex128,Complex64)}(A::QR{T},
                                                             B::StridedVecOrMat{T})
    _jl_lapack_orm2r("L", "N", A.hh, size(A, 2), A.tau, copy(B))
end

## Multiplication by Q' from the QR decomposition
function Ac_mul_B{T<:Union(Float64,Float32,Complex128,Complex64)}(A::QR{T},
                                                                  B::StridedVecOrMat{T})
    _jl_lapack_orm2r("L", iscomplex(A.tau) ? "C" : "T", A.hh, size(A, 2), A.tau, copy(B))
end

## Least squares solution.  Should be more careful about cases with m < n
function (\){T<:Union(Float64,Float32,Complex128,Complex64)}(A::QR{T},
                                                             B::StridedVecOrMat{T})
    n   = numel(A.tau)
    qtb = isa(B, Vector) ? (A' * B)[1:n] : (A' * B)[1:n, :]
    ## Not sure if this avoids copying A.hh[1:n,:] but at least it is not all of A.hh
    _jl_lapack_trtrs("U", "N", "N", A.hh[1:n,:], qtb)
end

type QRP{T} <: Factorization{T}
    hh::Matrix{T}
    tau::Vector{T}
    jpvt::Vector{Int32}
    function QRP(hh::Matrix{T}, tt::Vector{T}, jj::Vector{Int32})
        m, n = size(hh)
        numel(tt) == min(m,n) && numel(jj) == n ? new(hh,tt,jj) : error("QRP: mismatched dimensions")
    end
end

function QRP(A::Matrix)
    hh, tt, jj = _jl_lapack_geqp3(copy(A))
    QRP{typeof(A[1])}(hh, tt, jj)
end

## Not sure how to avoid cut-and-paste programming on these.
## Create another abstract type with both QR{T} and QRP{T} as subtypes?
size{T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRP{T}) = size(A.hh)
size{T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRP{T},n) = size(A.hh,n)

function (*){T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRP{T},
                                                             B::StridedVecOrMat{T})
    _jl_lapack_orm2r("L", "N", A.hh, size(A, 2), A.tau, copy(B))
end

function Ac_mul_B{T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRP{T},
                                                                  B::StridedVecOrMat{T})
    _jl_lapack_orm2r("L", iscomplex(A.tau) ? "C" : "T", A.hh, size(A, 2), A.tau, copy(B))
end

function (\){T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRP{T},
                                                             B::StridedVecOrMat{T})
    n = numel(A.tau)
    ## Replace this with a direct call to _jl_lapack_trtrs to save copying A.hh?
    ## Actually would need to call the appropriate Lapack subroutine to save copying.
    triu(A.hh[1:n,:]) \ (A' * B)[1:n]   
end

function (\){T<:Union(Float64,Float32,Complex128,Complex64)}(A::QRP{T},
                                                             B::StridedVecOrMat{T})
    ## may be better to define one method for B::Vector{T} and another for StridedMatrix
    BV  = isa(B, Vector)
    n   = numel(A.tau)
    qtb = BV ? (A' * B)[1:n] : (A' * B)[1:n, :]
    ans = _jl_lapack_trtrs("U", "N", "N", A.hh[1:n,:], qtb)
    BV ? ans[invperm(A.jpvt)] : ans[invperm(A.jpvt), :]
end

##ToDo:  Add methods for rank(A::QRP{T}) and adjust the (\) method accordingly
##       Add rcond methods for Cholesky, LU, QR and QRP types
## Lower priority: Add LQ, QL and RQ factorizations
