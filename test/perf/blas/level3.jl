# This file is a part of Julia. License is MIT: http://julialang.org/license

# gemm on various size matrices

function matmultest(n, iter)
    a = rand(n,n)
    b = similar(a)
    for i=1:iter
        A_mul_B!(b, a, a)
    end
    b
end

problemsizes = [(2^2, 10^6, "tiny"), (2^4, 10^5, "small"), (2^6, 10^4, "medium"), (2^8, 10^2, "large")]
testdata = [(matmultest, "matmul", "matrix-matrix multiplication", problemsizes)]
include("../perfgeneric.jl")

