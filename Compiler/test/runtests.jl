# This file is a part of Julia. License is MIT: https://julialang.org/license
using Test, Compiler

for file in readlines(joinpath(@__DIR__, "testgroups"))
    file == "special_loading" && continue # Only applicable to Base.Compiler
    include(file * ".jl")
end
