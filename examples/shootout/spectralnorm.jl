#
# The Computer Language Benchmarks Game
# spectral-norm benchmark
# http://shootout.alioth.debian.org/u32/performance.php?test=spectralnorm
#
# Based on the Scala program
#

# infinite matrix function and its transpose
A(i, j) = 1.0 / ((i+j)*(i+j+1)/2 +i+1)
At(j, i) = 1.0 / ((i+j)*(i+j+1)/2 +i+1)

# w <- M * v
function mult(N, v, w, M)
    for i = 1:N
        s = 0.0
        for j = 1:N
            s += M(i-1, j-1) * v[j]
        end
        w[i] = s
    end
end

function approximate(N)
    u = ones(N)
    v = ones(N)
    w = ones(N)
    for i = 1:10
        mult(N, u, w, A)
        mult(N, w, v, At)
        mult(N, v, w, A)
        mult(N, w, u, At)
    end

    vbv = 0.0
    vv = 0.0
    for i = 1:N
        vbv += u[i] * v[i]
        vv += v[i] * v[i]
    end

    sqrt(vbv / vv)
end

function spectralnorm(N)
    @printf("%.09f\n", approximate(N))
end

if length(ARGS) >= 1
    N = int(ARGS[1])
else
    N = 100
end
spectralnorm(N)
