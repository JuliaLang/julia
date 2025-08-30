# Math variant test - string concatenation include
# This file is included via: include(variant * "_tests.jl") where variant = "math"

@testset "Math Variant Tests" begin
    @testset "arithmetic operations" begin
        @test 2^3 == 8
        @test sqrt(16) == 4
        @test abs(-5) == 5
    end

    @testset "mathematical functions" begin
        @test sin(0) == 0
        @test cos(0) == 1
        @test round(3.7) == 4
    end
end
