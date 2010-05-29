libBLAS = dlopen("libBLAS")

typealias VectorOrMatrix{T} Union(Vector{T}, Matrix{T})

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY) 

for (fname, shape, eltype) = (("dcopy_", `Vector, Float64),
                              ("scopy_", `Vector, Float32),
                              ("dcopy_", `Matrix, Float64),
                              ("scopy_", `Matrix, Float32))
    eval (`function copy (X::($shape){$eltype})
          sz = size(X)
          Y = zeros($eltype, sz)
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
    eval(`function dot (x::Vector{$eltype}, y::Vector{$eltype})
         ccall(dlsym(libBLAS, $fname),
               $eltype,
               (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
               length(x), x, 1, y, 1)
         end
         )
end

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)

for (fname, eltype) = (("ddot_", Float64), ("sdot_", Float32))
    eval(`function norm (x::Vector{$eltype})
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
    eval(`function * (A::VectorOrMatrix{$eltype}, B::VectorOrMatrix{$eltype})
         m = size(A, 1)
         if isa(B, Vector) == 1; n = 1; else n = size(B, 2); end
         if isa(A, Vector) == 1; k = 1; else k = size(A, 2); end
         assert (k == size(B,1))
         C = zeros($eltype, m, n)

         ccall(dlsym(libBLAS, $fname),
             Void,
             (Ptr{Char}, Ptr{Char}, Ptr{Int32}, Ptr{Int32}, Ptr{Int32},
              Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32},
              Ptr{$eltype}, Ptr{Int32},
              Ptr{$eltype}, Ptr{$eltype}, Ptr{Int32}),
             "N", "N", m, n, k, 1.0, A, m, B, k, 0.0, C, m)

         return C
         end
         )
end
