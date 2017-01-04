# This file is a part of Julia. License is MIT: http://julialang.org/license

# Inner produce of x and y
function inner( x, y )
    s = zero(eltype(x))
    @simd for i=1:length(x)
        @inbounds s += x[i]*y[i]
    end
    s
end

function flog_inner( m, x, y )
    s = zero(eltype(x))
    for j=1:m
        s += inner(x,y)
    end
    s
end

for t in [Float32,Float64]
    n = 1000
    x = rand(t,n)
    y = rand(t,n)
    bits = 8*sizeof(t)
    @timeit(flog_inner(100,x,y), "inner_$bits", "SIMD inner product for type $t", "SIMD")
end

