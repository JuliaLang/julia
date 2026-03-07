# Tests for Base_type_depth_limit
include("../show_type.jl")

using Test

@testset "Base_type_depth_limit" begin
    tdl = Base_type_depth_limit

    str = repr(typeof(view([1, 2, 3], 1:2)))
    @test tdl(str, 0, maxdepth = 1) == "SubArray{…}"
    @test tdl(str, 0, maxdepth = 2) == "SubArray{$(Int), 1, Vector{…}, Tuple{…}, true}"
    @test tdl(str, 0, maxdepth = 3) == "SubArray{$(Int), 1, Vector{$(Int)}, Tuple{UnitRange{…}}, true}"
    @test tdl(str, 0, maxdepth = 4) == "SubArray{$(Int), 1, Vector{$(Int)}, Tuple{UnitRange{$(Int)}}, true}"
    @test tdl(str, 3) == "SubArray{…}"
    @test tdl(str, 44) == "SubArray{…}"
    @test tdl(str, 45) == "SubArray{$(Int), 1, Vector{…}, Tuple{…}, true}"
    @test tdl(str, 59) == "SubArray{$(Int), 1, Vector{…}, Tuple{…}, true}"
    @test tdl(str, 60) == "SubArray{$(Int), 1, Vector{$(Int)}, Tuple{UnitRange{…}}, true}"
    @test tdl(str, 100) == "SubArray{$(Int), 1, Vector{$(Int)}, Tuple{UnitRange{$(Int)}}, true}"

    str = repr(Vector{V} where V<:AbstractVector{T} where T<:Real)
    @test tdl(str, 0, maxdepth = 1) == "Vector{…} where {…}"
    @test tdl(str, 0, maxdepth = 2) == "Vector{V} where {T<:Real, V<:AbstractVector{…}}"
    @test tdl(str, 0, maxdepth = 3) == "Vector{V} where {T<:Real, V<:AbstractVector{T}}"
    @test tdl(str, 20) == "Vector{…} where {…}"
    @test tdl(str, 46) == "Vector{…} where {…}"
    @test tdl(str, 47) == "Vector{V} where {T<:Real, V<:AbstractVector{T}}"

    str = "F49231{Vector,Val{('}','}')},Vector{Vector{Vector{Vector}}},Tuple{Int,Int,Int,Int,Int,Int,Int},Int,Int,Int}"
    @test tdl(str, 105) == "F49231{Vector,Val{('}','}')},Vector{Vector{Vector{…}}},Tuple{Int,Int,Int,Int,Int,Int,Int},Int,Int,Int}"
    @test tdl(str, 85) == "F49231{Vector,Val{…},Vector{…},Tuple{…},Int,Int,Int}"
end
