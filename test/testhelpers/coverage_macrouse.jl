# This file is a part of Julia. License is MIT: https://julialang.org/license

include(joinpath(@__DIR__, "coverage_macros.jl"))
using .CoverageMacros

function usesmac(n)
    return @twice(n)
end

exit(usesmac(21) == 42 ? 0 : 1)
