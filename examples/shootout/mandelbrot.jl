# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by David Campbell

const ITER = 50

function mandel(z::Complex128)
    c = z
    for n = 1:ITER
        if abs(z) > 2
            return false
        end
        z = z^2 + c
    end
    return true
end

function draw_mandel(M::Array{Uint8, 2}, n::Int)
    for y = 0:n-1
        ci = 2y/n - 1
        for x = 0:n-1
            c = complex(2x/n - 1.5, ci)
            if mandel(c)
                M[div(x, 8) + 1, y + 1] |= 1 << uint8(7 - x%8)
            end
        end
    end
end

function main(args, stream)
    if length(args) > 0
        n = int(args[1])
    else
        n = 200
    end

    if n%8 != 0
        error("Error: n of $n is not divisible by 8")
    end

    M = zeros(Uint8, div(n, 8), n)
    draw_mandel(M, n)
    write(stream, "P4\n$n $n\n")
    write(stream, M)
    flush(stream)
end

#main([1600], open("mandel.txt", "w"))
main(ARGS, stdout_stream)
