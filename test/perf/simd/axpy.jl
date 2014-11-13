# Compute y += a*x using @simd for vectors x and y
function simd_axpy( a, x, y )
    # LLVM's auto-vectorizer typically vectorizes this loop even without @simd
    @simd for i=1:length(x)
        @inbounds y[i] += a*x[i]
    end
end

# Run axpy(a,x,y) m times
function flog_axpy( m, a, x, y )
    for j=1:m
        simd_axpy(a,x,y)
    end
end

# Run axpy for Float32 and Float64
for t in [Float32,Float64]
    n = 1000
    x = rand(t,n)
    y = rand(t,n)
    a = convert(t,0.5)
    bits = 8*sizeof(t)
    @timeit(flog_axpy(100,a,x,y), "simd_axpy_$bits", "SIMD BLAS axpy for type $t", "SIMD")
end
