# This file is a part of Julia. License is MIT: http://julialang.org/license

function gemvtest(n, iter)
    A = rand(n,n)
    x = rand(n)
    z = similar(x)
    for i = 1:iter
        z = A * x
    end
    z
end

problemsizes =[(2, 10^6, "tiny"), (2^4, 10^5, "small"), (2^6, 10^4, "medium"), (2^8, 10^3, "large"), (2^10, 10^2, "huge")]
testdata = [(gemvtest, "gemv", "matrix-vector multiplication", problemsizes)]
include("../perfgeneric.jl")
