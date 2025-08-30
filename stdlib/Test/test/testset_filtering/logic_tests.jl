# Logic variant test - string concatenation include
# This file is included via: include(variant * "_tests.jl") where variant = "logic"

@testset "Logic Variant Tests" begin
    @testset "logical operations" begin
        @test string(:gamma) == "gamma"
        @test startswith("gamma", "gam")
        @test !isempty("gamma")
    end

    @testset "conditional logic" begin
        @test (true ? "yes" : "no") == "yes"
        @test (false ? "yes" : "no") == "no"
        @test all([true, true, true]) == true
        @test any([false, true, false]) == true
    end
end
