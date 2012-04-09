#Assume running from julia base dir
load("timing.jl")
load("../../extras/bigint.jl")

function pidigits(N)
    """
    See http://shootout.alioth.debian.org
    Transliterated from Mario Perucci Python's program
    """

    i = k = ns = 0
    k1 = 1
    n,a,d,t,u = map(BigInt,(1,0,1,0,0))

    while i<N
        k += 1
        t = lshift(n,uint(1))
        n *= k
        a += t
        k1 += 2
        a *= k1
        d *= k1

        if a >= n
            t,u = divmod(n*3 +a, d)
            u += n
            if d > u
                ns = ns*10 + t
                i += 1
                if mod(i,N-10) == 0
                    #show(ns)
                    #printf("\t:%d\n", i)
#                    if i >= N
#                        return ns
#                    end
                    ns = 0
                end
                a -= d*t
                a *= 10
                n *= 10

            end
        end
    end
    return ns
end

@assert pidigits(1000) == 9216420198

if length(ARGS)==1
    N = int(ARGS[1])
else
    N = 1000
end
#pidigits(N)
@timeit pidigits(N) "pidigits"


