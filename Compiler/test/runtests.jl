# This file is a part of Julia. License is MIT: https://julialang.org/license

Base.runtests(["Compiler"]; propagate_project=true)

#=
# To run serially
using Test, Compiler
@testset "Compiler.jl" begin
    for file in readlines(joinpath(@__DIR__, "testgroups"))
        file == "special_loading" && continue # Only applicable to Base.Compiler
        startswith(lstrip(file), "#") && continue # Comment out files to ignore them
        testfile = file * ".jl"
        println("Testing $testfile...")
        @eval @testset $testfile include($testfile)
    end
end
=#
