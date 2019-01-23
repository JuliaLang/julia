using JSON
using Test
using Dates
using Distributed: RemoteChannel
using OffsetArrays

import DataStructures

include("json-samples.jl")

@testset "Parser" begin
    @testset "Parser Failures" begin
        include("parser/invalid-input.jl")
    end

    @testset "parsefile" begin
        include("parser/parsefile.jl")
    end

    @testset "dicttype" begin
        include("parser/dicttype.jl")
    end

    @testset "inttype" begin
        include("parser/inttype.jl")
    end

    @testset "Miscellaneous" begin
        # test for single values
        @test JSON.parse("true") == true
        @test JSON.parse("null") == nothing
        @test JSON.parse("\"hello\"") == "hello"
        @test JSON.parse("\"a\"") == "a"
        @test JSON.parse("1") == 1
        @test JSON.parse("1.5") == 1.5
        @test JSON.parse("[true]") == [true]
    end
end

@testset "Serializer" begin
    @testset "Standard Serializer" begin
        include("standard-serializer.jl")
    end

    @testset "Lowering" begin
        include("lowering.jl")
    end

    @testset "Custom Serializer" begin
        include("serializer.jl")
    end
end

@testset "Integration" begin
    # ::Nothing values should be encoded as null
    testDict = Dict("a" => nothing)
    nothingJson = JSON.json(testDict)
    nothingDict = JSON.parse(nothingJson)
    @test testDict == nothingDict

    @testset "async" begin
        include("async.jl")
    end

    @testset "indentation" begin
        include("indentation.jl")
    end

    @testset "JSON Checker" begin
        include("json-checker.jl")
    end
end

@testset "Regression" begin
    @testset "for issue #$i" for i in [21, 26, 57, 109, 152, 163]
        include("regression/issue$(lpad(string(i), 3, "0")).jl")
    end
end

# Check that printing to the default stdout doesn't fail
