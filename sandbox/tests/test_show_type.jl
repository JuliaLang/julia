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

# Verify that default show (without budget) produces full untruncated output.
# This ensures that repr, sprint, and print are not broken by the budget system.
@testset "default show produces full type output (no truncation)" begin
    T = Vector{Vector{Vector{Vector{Int}}}}
    str = sprint(Base_show, T)
    @test str == "Vector{Vector{Vector{Vector{Int64}}}}"
    @test !contains(str, "…")

    @test Base_repr(T) == "Vector{Vector{Vector{Vector{Int64}}}}"

    T2 = Dict{String, Vector{Pair{Symbol, Int}}}
    str2 = sprint(Base_show, T2)
    @test str2 == "Dict{String, Vector{Pair{Symbol, Int64}}}"
    @test !contains(str2, "…")

    T3 = typeof(view([1,2,3], 1:2))
    str3 = sprint(Base_show, T3)
    @test !contains(str3, "…")
    @test contains(str3, "SubArray")
    @test contains(str3, "UnitRange")

    T4 = Tuple{Int, Vector{Float64}, Dict{String, Any}}
    str4 = sprint(Base_show, T4)
    @test !contains(str4, "…")
    @test contains(str4, "Tuple{")
    @test contains(str4, "Dict{String, Any}")
end
