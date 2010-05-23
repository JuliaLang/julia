libBLAS = dlopen("libBLAS")
libLAPACK = dlopen("libLAPACK")

# SUBROUTINE DCOPY(N,DX,INCX,DY,INCY) 

function jl_gen_copy (fname, shape, eltype)
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

jl_gen_copy ("dcopy_", `Vector, Float64)
jl_gen_copy ("scopy_", `Vector, Float32)
jl_gen_copy ("dcopy_", `Matrix, Float64)
jl_gen_copy ("scopy_", `Matrix, Float32)

# DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)

function jl_gen_dot (fname, eltype)
    eval(`function dot (x::Vector{$eltype}, y::Vector{$eltype})
         ccall(dlsym(libBLAS, $fname),
               $eltype, 
               (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}), 
               length(x), x, 1, y, 1)
         end
         )
end

jl_gen_dot ("ddot_", Float64)
jl_gen_dot ("sdot_", Float32)

# DOUBLE PRECISION FUNCTION DNRM2(N,X,INCX)

function jl_gen_norm (fname, eltype)
    eval(`function norm (x::Vector{$eltype})
         ccall(dlsym(libBLAS, $fname),
               $eltype,
               (Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}),
               length(x), x, 1)
         end
         )
end

jl_gen_norm ("ddot_", Float64)
jl_gen_norm ("sdot_", Float32)

#       SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
# *     .. Scalar Arguments ..
#       DOUBLE PRECISION ALPHA,BETA
#       INTEGER K,LDA,LDB,LDC,M,N
#       CHARACTER TRANSA,TRANSB
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)

function jl_gen_mtimes(fname, eltype)
    eval (`function * (A::Matrix{$eltype}, B::Matrix{$eltype})
          m = size(A, 1)
          n = size(B, 2)
          k = size(A, 2)
          
          assert (k == size(B,1))
          C = zeros($eltype, m, n)
          
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

jl_gen_mtimes ("dgemm_", Float64)
jl_gen_mtimes ("sgemm_", Float32)

# SUBROUTINE DPOTRF( UPLO, N, A, LDA, INFO )
# *     .. Scalar Arguments ..
#       CHARACTER          UPLO
#       INTEGER            INFO, LDA, N
# *     ..
# *     .. Array Arguments ..
#       DOUBLE PRECISION   A( LDA, * )

function jl_gen_chol(fname, eltype)
    eval(`function chol (A::Matrix{$eltype})
         info = [0]
         n = size(A, 1)
         R = triu(A)
         ccall(dlsym(libLAPACK, $fname),
               Int32,
               (Ptr{Uint8}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               "U", n, R, n, info)
         if info[1] > 0; error("Matrix not Positive Definite"); end
         return R
         end
         )
end

jl_gen_chol("dpotrf_", Float64)
jl_gen_chol("spotrf_", Float32)

#       SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
# *     .. Scalar Arguments ..
#       INTEGER            INFO, LDA, LDB, N, NRHS
# *     ..
# *     .. Array Arguments ..
#       INTEGER            IPIV( * )
#       DOUBLE PRECISION   A( LDA, * ), B( LDB, * )

function jl_gen_mldivide (fname, eltype)
    eval(`function \ (A::Matrix{$eltype}, B::Matrix{$eltype})
        info = [0]
         n = size(A, 1)
         nrhs = size(B, 2)
         ipiv = ones(Int32, n)
         X = copy(B)
         ccall(dlsym(libLAPACK, $fname),
               Int32,
               (Ptr{Int32}, Ptr{Int32}, Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}, 
                Ptr{$eltype}, Ptr{Int32}, Ptr{Int32}),
               n, nrhs, A, n, ipiv, X, n, info)
         if info[1] > 0; error("U is singular"); end
         return X
         end
         )
end

jl_gen_mldivide("dgesv_", Float64)
jl_gen_mldivide("sgesv_", Float32)
