#
# The Computer Language Benchmarks Game
# fannkuch-redux benchmark
# http://shootout.alioth.debian.org/u32/performance.php?test=fannkuchredux
#
# Based on the Scala program
#

# flips the first a[1] elements of a, 
# assuming a is a permutation of the set {1, ..., length(a)}
flip(a) = [reverse(a[1:a[1]]), a[a[1]+1:end]]

function fannkuch(n)
    perm = [1:n]
    counts = map(int, zeros(n))
    r = n+1
    flips = 0
    maxflips = 0
    nperm = 0
    checksum = 0
    while r > 1 
        while (r != 2)
            counts[r-1] = r-1
            r -= 1
        end

        # count flips
        flips = 0
        p = copy(perm)
        while (p[1] != 1)
            p = flip(p)
            flips += 1
        end

        if (flips > maxflips)
            maxflips = flips
        end
        if nperm % 2 == 0
            checksum += flips
        else
            checksum -= flips
        end
        
        go = true
        while go
            if r == n+1
                println(checksum)
                return maxflips
            end
            p0 = perm[1]
            for i = 1:r-1
                perm[i] = perm[i+1]
            end
            perm[r] = p0
            counts[r] -= 1
            if counts[r] > 0 
                go = false
            else
                r += 1
            end
        end

        nperm += 1
    end
    maxflips
end

if length(ARGS) >= 1
    N = int(ARGS[1])
else
    N = 7
end
@printf("Pfannkuchen(%i) = %i\n", N, fannkuch(N))
