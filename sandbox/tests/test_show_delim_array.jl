# Tests for Base_show_delim_array
include("../show_type.jl")

using Test

@testset "Base_show_delim_array" begin
    sdastr(f, n) =
        sprint((io, x) -> Base_show_delim_array(io, x, "[", ",", "]", false, f, n), Iterators.take(1:f+n, f+n))
    @test sdastr(1, 0) == "[1]"
    @test sdastr(1, 1) == "[1]"
    @test sdastr(1, 2) == "[1, 2]"
    @test sdastr(2, 2) == "[2, 3]"
    @test sdastr(3, 3) == "[3, 4, 5]"
end
