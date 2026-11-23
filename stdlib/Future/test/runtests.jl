# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Future

@test isempty(Test.detect_closure_boxes(Future))

@testset "copy!" begin
    @test Future.copy!(Set{Int}(), Set([1])) == Set([1])
    @test Future.copy!(Dict{Int,Int}(), Dict(1 => 2)) == Dict(1 => 2)
    @test Future.copy!(zeros(2), [1.0, 2.0]) == [1.0, 2.0]
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(Future))
end
