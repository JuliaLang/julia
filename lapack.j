libLAPACK = dlopen("libLAPACK")

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
