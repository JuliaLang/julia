# Shared test file used by multiple include patterns
# This file is included via:
# - Static include: include("shared_tests.jl")
# - Dynamic include: include(joinpath(TESTDIR, "shared_tests.jl"))
# - Variable include: for file in test_files; include(file); end
# - Readlines include: for file in readlines(...); include(file); end

@testset "Shared Tests" begin
    @testset "basic math" begin
        @test 1 + 1 == 2
        @test 2 * 3 == 6
        @test 10 ÷ 2 == 5
    end

    @testset "string operations" begin
        @test length("hello") == 5
        @test uppercase("test") == "TEST"
        @test startswith("hello world", "hello")
    end
end
