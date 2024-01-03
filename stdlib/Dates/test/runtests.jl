# This file is a part of Julia. License is MIT: https://julialang.org/license

module DateTests

using Test, Dates

for file in readlines(joinpath(@__DIR__, "testgroups"))
    include(file * ".jl")
end

@testset "Docstrings" begin
    undoc = Docs.undocumented_names(Dates)
    @test_broken undoc == []
    @test undoc == [:adjust]
end

end
