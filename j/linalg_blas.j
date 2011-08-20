libBLAS = dlopen("libLAPACK")

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)
macro jl_blas_copy_macro(fname, eltype)
    quote
        function jl_blas_copy(n::Int, DX::Union(Ptr{$eltype},Array{$eltype}), incx::Int, DY::Union(Ptr{$eltype},Array{$eltype}), incy::Int)
            ccall(dlsym(libBLAS, $fname),
                  Void,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(n), DX, int32(incx), DY, int32(incy))
            return DY
        end
    end
end

@jl_blas_copy_macro :dcopy_ Float64
@jl_blas_copy_macro :scopy_ Float32
@jl_blas_copy_macro :zcopy_ Complex128
@jl_blas_copy_macro :ccopy_ Complex64

bcopy_to{T<:Union(Float64,Float32,Complex128,Complex64)}(dest::Ptr{T}, src::Ptr{T}, n::Int) =
    jl_blas_copy(n, src, 1, dest, 1)

function copy_to{T<:Union(Float64,Float32,Complex128,Complex64)}(dest::Ptr{T}, src::Ptr{T}, n::Int)
    if n < 200
        jl_blas_copy(n, src, 1, dest, 1)
    else
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Ulong), dest, src, ulong(n*sizeof(T)))
    end
    return dest
end

function copy_to{T<:Union(Float64,Float32,Complex128,Complex64)}(dest::Array{T}, src::Array{T})
    n = numel(src)
    if n < 200
        jl_blas_copy(n, src, 1, dest, 1)
    else
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Ulong), dest, src, ulong(n*sizeof(T)))
    end
    return dest
end

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
macro jl_blas_dot_macro(fname, eltype)
    quote
        function jl_blas_dot(n::Int, DX::Array{$eltype}, incx::Int,
                             DY::Array{$eltype}, incy::Int)
            ccall(dlsym(libBLAS, $fname),
                  $eltype,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(n), DX, int32(incx), DY, int32(incy))
        end
    end
end

@jl_blas_dot_macro :ddot_ Float64
@jl_blas_dot_macro :sdot_ Float32

function dot{T<:Union(Vector{Float64}, Vector{Float32})}(x::T, y::T)
    length(x) != length(y) ? error("Inputs should be of same length") : true
    jl_blas_dot(length(x), x, 1, y, 1)
end

# ccall is unable to return complex values (Issue #85)
#@blas_dot :zdotc_ Complex128
#@blas_dot :cdotc_ Complex64

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)
macro jl_blas_nrm2_macro(fname, eltype, ret_type)
    quote
        function jl_blas_nrm2(n::Int, X::Array{$eltype}, incx::Int)
            ccall(dlsym(libBLAS, $fname),
                  $ret_type,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(n), X, int32(incx))
        end
    end
end

@jl_blas_nrm2_macro :dnrm2_ Float64 Float64
@jl_blas_nrm2_macro :snrm2_ Float32 Float32
@jl_blas_nrm2_macro :dznrm2_ Complex128 Float64
@jl_blas_nrm2_macro :scnrm2_ Complex64 Float32

norm{T<:Union(Float64,Float32,Complex128,Complex64)}(x::Vector{T}) =
    jl_blas_nrm2(length(x), x, 1)


# SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
#*     .. Scalar Arguments ..
#      DOUBLE PRECISION DA
#      INTEGER INCX,INCY,N
#*     .. Array Arguments ..
#      DOUBLE PRECISION DX(*),DY(*)
macro jl_blas_axpy_macro(fname, eltype)
    quote
        function jl_blas_axpy(n::Int, x::($eltype), 
                              DA::Array{$eltype}, incx::Int, DY::Array{$eltype}, incy::Int)
            ccall(dlsym(libBLAS, $fname),
                  Void,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(n), x, DA, int32(incx), DY, int32(incy))            
        end
    end
end

@jl_blas_axpy_macro :daxpy_ Float64
@jl_blas_axpy_macro :saxpy_ Float32
@jl_blas_axpy_macro :zaxpy_ Complex128
@jl_blas_axpy_macro :caxpy_ Complex64


# SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
macro jl_blas_gemm_macro(fname, eltype)
   quote

       function jl_blas_gemm(transA, transB, m::Int, n::Int, k::Int,
                             alpha::($eltype), A::DenseMat{$eltype}, lda::Int,
                             B::DenseMat{$eltype}, ldb::Int,
                             beta::($eltype), C::DenseMat{$eltype}, ldc::Int)
           a = pointer(A)
           b = pointer(B)
           c = pointer(C)
           ccall(dlsym(libBLAS, $fname),
                 Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}),
                 transA, transB, int32(m), int32(n), int32(k),
                 alpha, a, int32(lda),
                 b, int32(ldb),
                 beta, c, int32(ldc))
       end

   end
end

@jl_blas_gemm_macro :dgemm_ Float64
@jl_blas_gemm_macro :sgemm_ Float32
@jl_blas_gemm_macro :zgemm_ Complex128
@jl_blas_gemm_macro :cgemm_ Complex64

function (*){T<:Union(Float64,Float32,Complex128,Complex64)}(A::DenseMat{T},
                                                             B::DenseMat{T})
    (mA, nA) = size(A)
    (mB, nB) = size(B)

    if nA != mB; error("*: argument shapes do not match"); end

    if mA == 2 && nA == 2 && nB == 2; return matmul2x2(A,B); end
    if mA == 3 && nA == 3 && nB == 3; return matmul3x3(A,B); end

    if stride(A, 1) != 1 || stride(B, 1) != 1
        return invoke(*, (AbstractArray, AbstractArray), A, B)
    end

    # Result array does not need to be initialized as long as beta==0
    C = Array(T, mA, nB)

    jl_blas_gemm("N", "N", mA, nB, nA,
                 convert(T, 1.0), A, stride(A, 2),
                 B, stride(B, 2),
                 convert(T, 0.0), C, mA)
    return C
end

#SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
#*     .. Scalar Arguments ..
#      DOUBLE PRECISION ALPHA,BETA
#      INTEGER INCX,INCY,LDA,M,N
#      CHARACTER TRANS
#*     .. Array Arguments ..
#      DOUBLE PRECISION A(LDA,*),X(*),Y(*)

macro jl_blas_gemv_macro(fname, eltype)
   quote

       function jl_blas_gemv(trans, m::Int, n::Int, 
                             alpha::($eltype), A::DenseMat{$eltype}, lda::Int,
                             X::DenseVec{$eltype}, incx::Int,
                             beta::($eltype), Y::DenseVec{$eltype}, incy::Int)
           a = pointer(A)
           x = pointer(X)
           y = pointer(Y)
           ccall(dlsym(libBLAS, $fname),
                 Void,
                 (Ptr{Uint8}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}),
                 trans, int32(m), int32(n),
                 alpha, a, int32(lda),
                 x, int32(incx),
                 beta, y, int32(incy))
       end

   end
end

@jl_blas_gemv_macro :dgemv_ Float64
@jl_blas_gemv_macro :sgemv_ Float32
@jl_blas_gemv_macro :zgemv_ Complex128
@jl_blas_gemv_macro :cgemv_ Complex64

function (*){T<:Union(Float64,Float32,Complex128,Complex64)}(A::DenseMat{T},
                                                             X::DenseVec{T})
    (mA, nA) = size(A)
    mX = size(X, 1)

    if nA != mX; error("*: argument shapes do not match"); end

    if stride(A, 1) != 1
        return invoke(*, (AbstractArray, AbstractVector), A, X)
    end

    # Result array does not need to be initialized as long as beta==0
    Y = Array(T, mA)

    jl_blas_gemv("N", mA, nA,
                 convert(T, 1.0), A, stride(A, 2),
                 X, stride(X, 1),
                 convert(T, 0.0), Y, 1)
    return Y
end
