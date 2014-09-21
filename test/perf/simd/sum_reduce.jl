function sum_reduce(x, istart, iend)
    s = zero(eltype(x))
    @simd for i = istart:iend
        @inbounds s += x[i]
    end
    s
end

function flog_sum_reduce( m, x )
    s = zero(eltype(x))
    for j=1:m
        # Try different starting and ending indices.
        sum_reduce(x,j,length(x)-(j-1))
    end
    s
end

for t in [Float32,Float64]
    n = 1000
    x = rand(t,n)
    bits = 8*sizeof(t)
    @timeit(flog_sum_reduce(100,x), "sum_reduction_$bits", "SIMD sum reduction over array of type $t", "SIMD")
end

