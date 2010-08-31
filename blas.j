libBLAS = dlopen("libBLAS")

typealias DenseVector{T} Array{T,1}
typealias DenseMatrix{T} Array{T,2}
typealias DenseVecOrMat{T} Union(DenseVector{T}, DenseMatrix{T})

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY) 

for (fname, shape, eltype) = (("dcopy_", `DenseVector, Float64),
                              ("scopy_", `DenseVector, Float32),
                              ("dcopy_", `DenseMatrix, Float64),
                              ("scopy_", `DenseMatrix, Float32))
    eval (`function copy (X::($shape){$eltype})
          sz = size(X)
          Y = Array($eltype, sz)
          ccall(dlsym(libBLAS, $fname),
                Void,
                (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
                prod(sz), X, 1, Y, 1)
          return Y
          end
          )
end

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)

for (fname, eltype) = (("ddot_", Float64), ("sdot_", Float32))
    eval(`function dot (x::DenseVector{$eltype}, y::DenseVector{$eltype})
         ccall(dlsym(libBLAS, $fname),
               $eltype,
               (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
               length(x), x, 1, y, 1)
         end
         )
end

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)

for (fname, eltype) = (("ddot_", Float64), ("sdot_", Float32))
    eval(`function norm (x::DenseVector{$eltype})
         ccall(dlsym(libBLAS, $fname),
               $eltype,
               (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
               length(x), x, 1)
         end
         )
end

# SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)

for (fname, eltype) = (("dgemm_", Float64), ("sgemm_", Float32))
    eval(`function * (A::DenseVecOrMat{$eltype}, B::DenseVecOrMat{$eltype})
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
             "N", "N", m, n, k, 1.0, A, m, B, k, 0.0, C, m)

         return C
         end
         )
end
