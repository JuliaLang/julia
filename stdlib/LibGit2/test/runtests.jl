# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, LibGit2

@testset verbose=true "LibGit2 $test" for test in eachline(joinpath(@__DIR__, "testgroups"))
    include("$test.jl")
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(LibGit2))
end
