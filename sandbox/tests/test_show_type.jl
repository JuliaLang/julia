# Tests for Base_show(io, ::Type) and Base_show_type (type display)
include("../show_type.jl")

using Test

@testset "Base_show for simple concrete types" begin
    @test Base_repr(Int) == "Int64"
    @test Base_repr(Float64) == "Float64"
    @test Base_repr(String) == "String"
    @test Base_repr(Any) == "Any"
end

@testset "Base_show for parameterized types" begin
    @test Base_repr(Vector{Int}) == "Vector{Int64}"
    @test Base_repr(Dict{String, Int}) == "Dict{String, Int64}"
    @test Base_repr(Pair{String, Int64}) == "Pair{String, Int64}"
end

@testset "Base_show for Union{}" begin
    @test Base_repr(Union{}) == "Union{}"
end

@testset "Base_show for Union types" begin
    @test Base_repr(Union{Int, Float64}) == "Union{Float64, Int64}"
end

@testset "Base_show for primary type without params" begin
    @test Base_repr(Array) == "Array"
    @test Base_repr(Tuple) == "Tuple"
    @test string(Array) == "Array"
    @test string(Tuple{Array}) == "Tuple{Array}"
end
