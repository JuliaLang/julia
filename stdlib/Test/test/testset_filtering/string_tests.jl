# String variant test - string concatenation include
# This file is included via: include(variant * "_tests.jl") where variant = "string"

@testset "String Variant Tests" begin
    @testset "string manipulation" begin
        @test string(:beta) == "beta"
        @test Symbol("beta") == :beta
        @test lowercase("HELLO") == "hello"
    end

    @testset "string analysis" begin
        @test length("beta") == 4
        @test startswith("beta test", "beta")
        @test endswith("test beta", "beta")
    end
end
