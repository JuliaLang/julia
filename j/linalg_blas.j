libBLAS = dlopen("libLAPACK")

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY)

macro blas_copy(fname, shape, eltype)
    quote
        bcopy(src::($shape){$eltype}) = copy_to(similar(src), src)

        function bcopy_to(dest::($shape){$eltype}, src::($shape){$eltype})
            ccall(dlsym(libBLAS, $fname),
                  Void,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(numel(src)), src, int32(1), dest, int32(1))
            return dest
        end
    end
end

@blas_copy :dcopy_ Vector Float64
@blas_copy :scopy_ Vector Float32
@blas_copy :dcopy_ Matrix Float64
@blas_copy :scopy_ Matrix Float32
@blas_copy :zcopy_ Vector Complex128
@blas_copy :ccopy_ Vector Complex64
@blas_copy :zcopy_ Matrix Complex128
@blas_copy :ccopy_ Matrix Complex64

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)

macro blas_dot(fname, eltype)
    quote
        function dot(x::Vector{$eltype}, y::Vector{$eltype})
            ccall(dlsym(libBLAS, $fname),
                  $eltype,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(length(x)), x, int32(1), y, int32(1))
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
        function norm(x::Vector{$eltype})
            ccall(dlsym(libBLAS, $fname),
                  $ret_type,
                  (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                  int32(length(x)), x, int32(1))
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
       function *(A::VecOrMat{$eltype}, B::VecOrMat{$eltype})
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
                 "N", "N",
                 int32(m), int32(n), int32(k),
                 convert($eltype, 1.0),
                 A, int32(m), B, int32(k),
                 convert($eltype, 0.0), C, int32(m))

           return C
       end
   end
end

@blas_matrix_multiply :dgemm_ Float64
@blas_matrix_multiply :sgemm_ Float32
@blas_matrix_multiply :zgemm_ Complex128
@blas_matrix_multiply :cgemm_ Complex64
