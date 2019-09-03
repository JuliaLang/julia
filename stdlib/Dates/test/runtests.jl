# This file is a part of Julia. License is MIT: https://julialang.org/license

module DateTests

for file in readlines(joinpath(@__DIR__, "testgroups"))
    include(file * ".jl")
end

end
