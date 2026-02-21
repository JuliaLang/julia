# This file is a part of Julia. License is MIT: https://julialang.org/license

module DateTests

using Test, Dates

@test isempty(Test.detect_closure_boxes(Dates))

for file in readlines(joinpath(@__DIR__, "testgroups"))
    include(file * ".jl")
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(Dates))
end

end
