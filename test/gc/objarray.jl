# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random: seed!
seed!(1)

abstract type Cell end

struct CellA<:Cell
    a::Ref{Int}
end

struct CellB<:Cell
    b::String
end

function fillcells!(mc::Array{Cell})
    for ind in eachindex(mc)
        mc[ind] = ifelse(rand() > 0.5, CellA(ind), CellB(string(ind)))
    end
    return mc
end

function work(size)
    mcells = Array{Cell}(undef, size, size)
    fillcells!(mcells)
end

function run(maxsize)
    Threads.@threads for i in 1:maxsize
        work(i*375)
    end
end

# Memory usage 581 MB
run(4)
GC.gc()
