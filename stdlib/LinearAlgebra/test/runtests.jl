# This file is a part of Julia. License is MIT: https://julialang.org/license

include("use_mkl.jl")

for file in readlines(joinpath(@__DIR__, "testgroups"))
    include(file * ".jl")
end
