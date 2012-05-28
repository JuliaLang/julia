
# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
for (fname, elty) in ((:dcopy_,:Float64), (:scopy_,:Float32),
                      (:zcopy_,:Complex128), (:ccopy_,:Complex64))
    @eval begin
        function _jl_blas_copy(n::Integer, DX::Union(Ptr{$elty},Array{$elty}), incx::Integer, DY::Union(Ptr{$elty},Array{$elty}), incy::Integer)
            ccall(dlsym(_jl_libblas, $string(fname)),
                  Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, DX, &incx, DY, &incy)
            return DY
        end
    end
end

function copy_to{T<:Union(Float64,Float32,Complex128,Complex64)}(dest::Ptr{T}, src::Ptr{T}, n::Integer)
    if n < 200
        _jl_blas_copy(n, src, 1, dest, 1)
    else
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), dest, src, n*sizeof(T))
    end
    return dest
end

function copy_to{T<:Union(Float64,Float32,Complex128,Complex64)}(dest::Array{T}, src::Array{T})
    n = numel(src)
    if n < 200
        _jl_blas_copy(n, src, 1, dest, 1)
    else
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint), dest, src, n*sizeof(T))
    end
    return dest
end

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
for (fname, elty) in ((:ddot_,:Float64), (:sdot_,:Float32))
    @eval begin
        function _jl_blas_dot(n::Integer, DX::Array{$elty}, incx::Integer,
                              DY::Array{$elty}, incy::Integer)
            ccall(dlsym(_jl_libblas, $string(fname)),
                  $elty,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, DX, &incx, DY, &incy)
        end
    end
end

function dot{T<:Union(Vector{Float64}, Vector{Float32})}(x::T, y::T)
    length(x) != length(y) ? error("Inputs should be of same length") : true
    _jl_blas_dot(length(x), x, 1, y, 1)
end

# ccall is unable to return complex values (Issue #85)
#@blas_dot :zdotc_ Complex128
#@blas_dot :cdotc_ Complex64

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)
for (fname, elty, ret_type) in ((:dnrm2_,:Float64,:Float64),
                                (:snrm2_,:Float32,:Float32),
                                (:dznrm2_,:Complex128,:Float64),
                                (:scnrm2_,:Complex64,:Float32))
    @eval begin
        function _jl_blas_nrm2(n::Integer, X::Array{$elty}, incx::Integer)
            ccall(dlsym(_jl_libblas, $string(fname)),
                  $ret_type,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, X, &incx)
        end
    end
end

norm{T<:Union(Float64,Float32,Complex128,Complex64)}(x::Vector{T}) =
    _jl_blas_nrm2(length(x), x, 1)


# SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
#*     .. Scalar Arguments ..
#      DOUBLE PRECISION DA
#      INTEGER INCX,INCY,N
#*     .. Array Arguments ..
#      DOUBLE PRECISION DX(*),DY(*)
for (fname, elty) in ((:daxpy_,:Float64), (:saxpy_,:Float32),
                      (:zaxpy_,:Complex128), (:caxpy_,:Complex64))
    @eval begin
        function _jl_blas_axpy(n::Integer, x::($elty), 
                               DA::Array{$elty}, incx::Integer, DY::Array{$elty}, incy::Integer)
            ccall(dlsym(_jl_libblas, $string(fname)),
                  Void,
                  (Ptr{Int32}, Ptr{$elty}, Ptr{$elty}, Ptr{Int32}, Ptr{$elty}, Ptr{Int32}),
                  &n, x, DA, &incx, DY, &incy)            
        end
    end
end

# SUBROUTINE DSYRK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       REAL ALPHA,BETA
#       INTEGER K,LDA,LDC,N
#       CHARACTER TRANS,UPLO
# *     ..
# *     .. Array Arguments ..
#       REAL A(LDA,*),C(LDC,*)
for (fname, elty) in ((:dsyrk_,:Float64), (:ssyrk_,:Float32),
                      (:zsyrk_,:Complex128), (:csyrk_,:Complex64))
   @eval begin
       function _jl_blas_syrk(uplo, trans, n::Integer, k::Integer,
                             alpha::($elty), A::StridedMatrix{$elty}, lda::Integer,
                             beta::($elty), C::StridedMatrix{$elty}, ldc::Integer)
           ccall(dlsym(_jl_libblas, $string(fname)),
                 Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &trans, &n, &k,
                 &alpha, A, &lda,
                 &beta, C, &ldc)
       end
   end
end

# SUBROUTINE CHERK(UPLO,TRANS,N,K,ALPHA,A,LDA,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       REAL ALPHA,BETA
#       INTEGER K,LDA,LDC,N
#       CHARACTER TRANS,UPLO
# *     ..
# *     .. Array Arguments ..
#       COMPLEX A(LDA,*),C(LDC,*)
for (fname, elty) in ((:zherk_,:Complex128), (:cherk_,:Complex64))
   @eval begin
       function _jl_blas_herk(uplo, trans, n::Integer, k::Integer,
                             alpha::($elty), A::StridedMatrix{$elty}, lda::Integer,
                             beta::($elty), C::StridedMatrix{$elty}, ldc::Integer)
           ccall(dlsym(_jl_libblas, $string(fname)),
                 Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &trans, &n, &k,
                 &alpha, A, &lda,
                 &beta, C, &ldc)
       end
   end
end

# SUBROUTINE DGBMV(TRANS,M,N,KL,KU,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER INCX,INCY,KL,KU,LDA,M,N
#       CHARACTER TRANS
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
for (fname, elty) in ((:dgbmv_,:Float64), (:sgbmv_,:Float32),
                      (:zgbmv_,:Complex128), (:cgbmv_,:Complex64))
   @eval begin
       function _jl_blas_gbmv(trans, m::Integer, n::Integer, kl::Integer, ku::Integer,
                             alpha::($elty), A::StridedMatrix{$elty}, lda::Integer,
                             x::StridedVector{$elty}, incx::Integer,
                             beta::($elty), y::StridedVector{$elty}, incy::Integer)
           ccall(dlsym(_jl_libblas, $string(fname)),
                 Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &trans, &m, &n, &kl, &ku,
                 &alpha, A, &lda,
                 x, &incx,
                 &beta, y, &incy)
       end

   end
end

#       SUBROUTINE DSBMV(UPLO,N,K,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER INCX,INCY,K,LDA,N
#       CHARACTER UPLO
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),X(*),Y(*)
for (fname, elty) in ((:dsbmv_,:Float64), (:ssbmv_,:Float32),
                      (:zsbmv_,:Complex128), (:csbmv_,:Complex64))
   @eval begin
       function _jl_blas_sbmv(uplo, n::Integer, k::Integer,
                             alpha::($elty), A::StridedMatrix{$elty}, lda::Integer,
                             x::StridedVector{$elty}, incx::Integer,
                             beta::($elty), y::StridedVector{$elty}, incy::Integer)
           ccall(dlsym(_jl_libblas, $string(fname)),
                 Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &uplo, &n, &k,
                 &alpha, A, &lda,
                 x, &incx,
                 &beta, y, &incy)
       end

   end
end

# SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
for (fname, elty) in ((:dgemm_,:Float64), (:sgemm_,:Float32),
                      (:zgemm_,:Complex128), (:cgemm_,:Complex64))
   @eval begin
       function _jl_blas_gemm(transA, transB, m::Integer, n::Integer, k::Integer,
                             alpha::($elty), A::StridedMatrix{$elty}, lda::Integer,
                             B::StridedMatrix{$elty}, ldb::Integer,
                             beta::($elty), C::StridedMatrix{$elty}, ldc::Integer)
           ccall(dlsym(_jl_libblas, $string(fname)),
                 Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 &transA, &transB, &m, &n, &k,
                 &alpha, A, &lda,
                 B, &ldb,
                 &beta, C, &ldc)
       end

   end
end

function (*){T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},
                                                             B::StridedMatrix{T})
    _jl_gemm('N', 'N', A, B)
end

function At_mul_B{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},
                                                             B::StridedMatrix{T})
    if is(A, B) && size(A,1)>=500
        _jl_syrk('T', A)
    else
        _jl_gemm('T', 'N', A, B)
    end
end

function A_mul_Bt{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},
                                                             B::StridedMatrix{T})
    if is(A, B) && size(A,2)>=500
        _jl_syrk('N', A)
    else
        _jl_gemm('N', 'T', A, B)
    end
end

function At_mul_Bt{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},
                                                              B::StridedMatrix{T})
    _jl_gemm('T', 'T', A, B)
end

Ac_mul_B{T<:Union(Float64,Float32)}(A::StridedMatrix{T}, B::StridedMatrix{T}) = At_mul_B(A, B)
function Ac_mul_B{T<:Union(Complex128,Complex64)}(A::StridedMatrix{T},
                                             B::StridedMatrix{T})
    if is(A, B) && size(A,1)>=500
        _jl_herk('C', A)
    else
        _jl_gemm('C', 'N', A, B)
    end
end

A_mul_Bc{T<:Union(Float64,Float32)}(A::StridedMatrix{T}, B::StridedMatrix{T}) = A_mul_Bt(A, B)
function A_mul_Bc{T<:Union(Complex128,Complex64)}(A::StridedMatrix{T},
                                             B::StridedMatrix{T})
    if is(A, B) && size(A,2)>=500
        _jl_herk('N', A)
    else
        _jl_gemm('N', 'C', A, B)
    end
end

function Ac_mul_Bc{T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},
                                                              B::StridedMatrix{T})
    _jl_gemm('C', 'C', A, B)
end

function _jl_copy_upper_to_lower(A::StridedMatrix)
    n = size(A, 1)
    for i = 1:n-1
        for j = i+1:n
            A[j, i] = A[i, j]
        end
    end
end

function _jl_syrk{T<:Union(Float64,Float32,Complex128,Complex64)}(tA,
                                                                  A::StridedMatrix{T})
    if tA == 'T'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'T'
    end

    if mA == 2 && nA == 2; return matmul2x2(tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3(tA,tAt,A,A); end

    if stride(A, 1) != 1
        if tA == 'T'
            return _jl_generic_matmatmul(A.', A)
        else
            return _jl_generic_matmatmul(A, A.')
        end
    end

    # Result array does not need to be initialized as long as beta==0
    C = Array(T, mA, mA)

    _jl_blas_syrk('U', tA, mA, nA,
                  one(T), A, stride(A, 2),
                  zero(T), C, mA)
    _jl_copy_upper_to_lower(C)
    return C
end

function _jl_copy_upper_to_lower_conj(A::StridedMatrix)
    n = size(A, 1)
    for i = 1:n-1
        for j = i+1:n
            A[j, i] = conj(A[i, j])
        end
    end
end

function _jl_herk{T<:Union(Float64,Float32,Complex128,Complex64)}(tA,
                                                                  A::StridedMatrix{T})
    if tA == 'C'
        (nA, mA) = size(A)
        tAt = 'N'
    else
        (mA, nA) = size(A)
        tAt = 'C'
    end

    if mA == 2 && nA == 2; return matmul2x2(tA,tAt,A,A); end
    if mA == 3 && nA == 3; return matmul3x3(tA,tAt,A,A); end

    if stride(A, 1) != 1
        if tA == 'C'
            return _jl_generic_matmatmul(A', A)
        else
            return _jl_generic_matmatmul(A, A')
        end
    end

    # Result array does not need to be initialized as long as beta==0
    C = Array(T, mA, mA)

    _jl_blas_herk('U', tA, mA, nA,
                  one(T), A, stride(A, 2),
                  zero(T), C, mA)
    _jl_copy_upper_to_lower_conj(C)
    return C
end

function _jl_gemm{T<:Union(Float64,Float32,Complex128,Complex64)}(tA, tB,
                                                                  A::StridedMatrix{T},
                                                                  B::StridedMatrix{T})
    if tA != 'N'
        (nA, mA) = size(A)
    else
        (mA, nA) = size(A)
    end
    if tB != 'N'
        (nB, mB) = size(B)
    else
        (mB, nB) = size(B)
    end

    if nA != mB; error("*: argument shapes do not match"); end

    if mA == 2 && nA == 2 && nB == 2; return matmul2x2(tA,tB,A,B); end
    if mA == 3 && nA == 3 && nB == 3; return matmul3x3(tA,tB,A,B); end

    if stride(A, 1) != 1 || stride(B, 1) != 1
        if tA == 'T'
            A = A.'
        elseif tA == 'C'
            A = A'
        end
        if tB == 'T'
            B = B.'
        elseif tB == 'C'
            B = B'
        end
        return _jl_generic_matmatmul(A, B)
    end

    # Result array does not need to be initialized as long as beta==0
    C = Array(T, mA, nB)

    _jl_blas_gemm(tA, tB, mA, nB, nA,
                  one(T), A, stride(A, 2),
                  B, stride(B, 2),
                  zero(T), C, mA)
    return C
end

#SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#*     .. Scalar Arguments ..
#      DOUBLE PRECISION ALPHA,BETA
#      INTEGER INCX,INCY,LDA,M,N
#      CHARACTER TRANS
#*     .. Array Arguments ..
#      DOUBLE PRECISION A(LDA,*),X(*),Y(*)

for (fname, elty) in ((:dgemv_,:Float64), (:sgemv_,:Float32),
                      (:zgemv_,:Complex128), (:cgemv_,:Complex64))
   @eval begin
       function _jl_blas_gemv(trans, m::Integer, n::Integer, 
                             alpha::($elty), A::StridedMatrix{$elty}, lda::Integer,
                             X::StridedVector{$elty}, incx::Integer,
                             beta::($elty), Y::StridedVector{$elty}, incy::Integer)
           ccall(dlsym(_jl_libblas, $string(fname)),
                 Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{Int32},
                  Ptr{$elty}, Ptr{$elty}, Ptr{Int32}),
                 trans, &m, &n,
                 &alpha, A, &lda,
                 X, &incx,
                 &beta, Y, &incy)
       end

   end
end

# TODO: support transposed arguments
function (*){T<:Union(Float64,Float32,Complex128,Complex64)}(A::StridedMatrix{T},
                                                             X::StridedVector{T})
    (mA, nA) = size(A)
    mX = size(X, 1)

    if nA != mX; error("*: argument shapes do not match"); end

    if stride(A, 1) != 1
        return _jl_generic_matvecmul(A, X)
    end

    # Result array does not need to be initialized as long as beta==0
    Y = Array(T, mA)

    _jl_blas_gemv("N", mA, nA,
                 one(T), A, stride(A, 2),
                 X, stride(X, 1),
                 zero(T), Y, 1)
    return Y
end
