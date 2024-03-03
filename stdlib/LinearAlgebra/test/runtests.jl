# This file is a part of Julia. License is MIT: https://julialang.org/license
using Test, LinearAlgebra

for file in readlines(joinpath(@__DIR__, "testgroups"))
    include(file * ".jl")
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(LinearAlgebra))
end
