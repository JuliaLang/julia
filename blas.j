libBLAS = dlopen("libBLAS")

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY) 

function jl_gen_copy(fname, shape, eltype)
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

jl_gen_copy("dcopy_", `Vector, Float64)
jl_gen_copy("scopy_", `Vector, Float32)
jl_gen_copy("dcopy_", `Matrix, Float64)
jl_gen_copy("scopy_", `Matrix, Float32)

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)

function jl_gen_dot(fname, eltype)
    eval(`function dot (x::Vector{$eltype}, y::Vector{$eltype})
         ccall(dlsym(libBLAS, $fname),
               $eltype,
               (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
               length(x), x, 1, y, 1)
         end
         )
end

jl_gen_dot("ddot_", Float64)
jl_gen_dot("sdot_", Float32)

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)

function jl_gen_norm(fname, eltype)
    eval(`function norm (x::Vector{$eltype})
         ccall(dlsym(libBLAS, $fname),
               $eltype,
               (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
               length(x), x, 1)
         end
         )
end

jl_gen_norm("ddot_", Float64)
jl_gen_norm("sdot_", Float32)

# SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)

function jl_gen_mtimes(fname, eltype)
    eval(`function * (A::Matrix{$eltype}, B::Matrix{$eltype})
         m = size(A, 1)
         n = size(B, 2)
         k = size(A, 2)
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

jl_gen_mtimes("dgemm_", Float64)
jl_gen_mtimes("sgemm_", Float32)
