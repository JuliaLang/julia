# This file is a part of Julia. License is MIT: http://julialang.org/license

function dottest(n, iter)
    a = rand(n)
    b = rand(n)
    c = similar(a)
    for i = 1:iter
        c = dot(a,b)
    end
    c
end

function axpytest(n, iter)
    a = rand(1)[1]
    x = rand(n)
    y = zeros(n)
    for i = 1:iter
        BLAS.axpy!(a, x, y)
    end
    y
end

problemsizes = [(2, 10^6, "tiny"), (2^4, 10^6, "small"), (2^6, 10^6, "medium"), (2^8, 10^5, "large"), (2^10, 10^5, "huge")]
testdata = [(dottest, "dot", "dot product", problemsizes), (axpytest, "axpy", "axpy", problemsizes)]
include("../perfgeneric.jl")

