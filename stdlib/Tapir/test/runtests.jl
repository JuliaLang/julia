using Test
using Tapir

function pfib(N)
    if N <= 1
        return N
    end
    x1 = Ref{Int64}()
    local x2
    Tapir.@sync begin
        Tapir.@spawn x1[] = pfib(N - 1)
        x2 = pfib(N - 2)
    end
    return x[] + x2
end

function saxpy(Z, X, Y, a)
    Tapir.foreach(eachindex(Z, Y, X)) do I
        @inbounds Z[I] = a*X[I] + Y[I]
    end
    Z
end

# saxpy(zeros(N), ones(N), ones(N), 1.0)