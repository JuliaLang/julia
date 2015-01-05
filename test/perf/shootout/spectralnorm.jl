#
# The Computer Language Benchmarks Game
# spectral-norm benchmark
# http://shootout.alioth.debian.org/u32/performance.php?test=spectralnorm
#
# Based on the Javascript program
#

A(i,j) = 1.0 / ((i+j)*(i+j+1.0)/2.0+i+1.0)

function Au(u,w)
    n = length(u)
    for i = 1:n, j = 1:n
        j == 1 && (w[i] = 0)
        w[i] += A(i-1,j-1) * u[j]
    end
end

function Atu(w,v)
    n = length(w)
    for i = 1:n, j = 1:n
        j == 1 && (v[i] = 0)
        v[i] += A(j-1,i-1) * w[j]
    end
end

function approximate(n)
    u = ones(Float64,n)
    v = zeros(Float64,n)
    w = zeros(Float64,n)
    vv = vBv = 0
    for i = 1:10
        Au(u,w)
        Atu(w,v)
        Au(v,w)
        Atu(w,u)
    end
    for i = 1:n
        vBv += u[i]*v[i]
        vv += v[i]*v[i]
    end
    return sqrt(vBv/vv)
end

function spectralnorm(N::Int=100)
    approximate(N)
end

# @assert spectralnorm(100) == 1.274219991
# @timeit spectralnorm(500) "spectralnorm(n=500)"
# @timeit spectralnorm(3000) "spectralnorm(n=3000)"
# @timeit spectralnorm(5500) "spectralnorm(n=5500)"
