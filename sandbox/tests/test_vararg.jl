# Tests for Base_show(io, ::Core.TypeofVararg) display
include("../show_type.jl")

using Test

@testset "Vararg display" begin
    @test Base_repr(Tuple{String, Int64, Int64, Int64, Int64}) == "Tuple{String, Vararg{Int64, 4}}"
    @test Base_repr(Tuple{Vararg{N, 10}} where N) == "NTuple{10, N} where N"
    @test Base_repr(Tuple{Vararg{10, N}} where N) == "Tuple{Vararg{10, N}} where N"
end
