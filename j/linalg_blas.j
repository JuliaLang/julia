libBLAS = dlopen("libLAPACK")

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)

macro blas_copy(fname, shape, eltype)
    quote
        bcopy(src::($shape){$eltype}) = copy_to(similar(src), src)

        function bcopy_to(dest::($shape){$eltype}, src::($shape){$eltype})
            ccall(dlsym(libBLAS, $fname),
                  Void,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  numel(src), src, 1, dest, 1)
            return dest
        end
    end
end

@blas_copy :dcopy_ DenseVector Float64
@blas_copy :scopy_ DenseVector Float32
@blas_copy :dcopy_ DenseMatrix Float64
@blas_copy :scopy_ DenseMatrix Float32
@blas_copy :zcopy_ DenseVector Complex128
@blas_copy :ccopy_ DenseVector Complex64
@blas_copy :zcopy_ DenseMatrix Complex128
@blas_copy :ccopy_ DenseMatrix Complex64

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)

macro blas_dot(fname, eltype)
    quote
        function dot(x::DenseVector{$eltype}, y::DenseVector{$eltype})
            ccall(dlsym(libBLAS, $fname),
                  $eltype,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  length(x), x, 1, y, 1)
        end
    end
end

@blas_dot :ddot_ Float64
@blas_dot :sdot_ Float32
# ccall does not work well when complex values are returned
#@blas_dot :zdotc_ Complex128
#@blas_dot :cdotc_ Complex64

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)

macro blas_norm(fname, eltype, ret_type)
    quote
        function norm(x::DenseVector{$eltype})
            ccall(dlsym(libBLAS, $fname),
                  $ret_type,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  length(x), x, 1)
        end
    end
end

@blas_norm :dnrm2_ Float64 Float64
@blas_norm :snrm2_ Float32 Float32
@blas_norm :dznrm2_ Complex128 Float64
@blas_norm :scnrm2_ Complex64 Float32

# SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)

macro blas_matrix_multiply(fname, eltype)
   quote
       function *(A::DenseVecOrMat{$eltype}, B::DenseVecOrMat{$eltype})
           m = size(A, 1)
           if isa(B, Vector); n = 1; else n = size(B, 2); end
           if isa(A, Vector); k = 1; else k = size(A, 2); end
           if k != size(B,1)
               error("*: argument shapes do not match")
           end
           # array does not need to be initialized as long as beta==0
           C = isa(B, Vector) ? Array($eltype, m) : Array($eltype, m, n)

           ccall(dlsym(libBLAS, $fname),
                 Void,
                 (Ptr{Uint8}, Ptr{Uint8}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{Int32},
                  Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}),
                 "N", "N", m, n, k, convert($eltype, 1.0), A, m, B, k, convert($eltype, 0.0), C, m)

           return C
       end
   end
end

@blas_matrix_multiply :dgemm_ Float64
@blas_matrix_multiply :sgemm_ Float32
@blas_matrix_multiply :zgemm_ Complex128
@blas_matrix_multiply :cgemm_ Complex64
