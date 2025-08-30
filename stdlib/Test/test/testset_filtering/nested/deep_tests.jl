# Deep nested path test - demonstrates joinpath() resolution
# This file is included via: include(joinpath(@__DIR__, "nested", "deep_tests.jl"))

@testset "Deep Path Tests" begin
    @testset "path operations" begin
        @test joinpath("a", "b") == "a/b" || joinpath("a", "b") == "a\\b"  # Handle both Unix and Windows
        @test dirname("/a/b/c.txt") == "/a/b"
        @test normpath("a/b/../c") == "a/c" || normpath("a/b/../c") == "a\\c"
    end

    @testset "nested path tests" begin
        @testset "deep nesting" begin
            @test basename("/a/b/file.txt") == "file.txt"
            @test splitext("file.txt")[2] == ".txt"
            @testset "very deep nesting" begin
                @test isfile(@__FILE__)
                @test basename(@__FILE__) == "deep_tests.jl"
            end
        end
    end
end
