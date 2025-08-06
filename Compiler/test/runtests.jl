# This file is a part of Julia. License is MIT: https://julialang.org/license

Base.runtests(["Compiler"]; propagate_project=true)

#=
# To run serially
using Test, Compiler
@testset "Compiler.jl" begin
    for file in readlines(joinpath(@__DIR__, "testgroups"))
        file == "special_loading" && continue # Only applicable to Base.Compiler
        testfile = file * ".jl"
        @eval @testset $testfile include($testfile)
    end
end
=#
