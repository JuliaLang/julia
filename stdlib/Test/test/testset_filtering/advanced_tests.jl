# Advanced test file used by variable-based includes
# This file is included via:
# - Variable include: for file in test_files; include(file); end

@testset "Advanced Tests" begin
    @testset "boolean logic" begin
        @test true && true == true
        @test false || true == true
        @test !false == true
        @test !(true && false) == true
    end

    @testset "comparison operators" begin
        @test 5 > 3
        @test 2 <= 2
        @test 4 != 5
        @test 7 >= 7
    end

    @testset "array operations" begin
        @test length([1,2,3]) == 3
        @test sum([1,2,3]) == 6
        @test [1,2] ∪ [2,3] == [1,2,3]
    end
end
