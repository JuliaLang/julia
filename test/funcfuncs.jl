# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# test the basic function functions

@testset "iteration" begin
    @test supertype^0 == identity
    @test supertype^1 == supertype
    @test supertype^2 == supertype ∘ supertype
end
