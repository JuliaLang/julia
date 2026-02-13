# Tests for compact printing of homogeneous tuples and NamedTuples
include("../show_type.jl")

using Test

@testset "compact printing of homogeneous tuples" begin
    @test Base_repr(NTuple{7,Int64}) == "NTuple{7, Int64}"
    @test Base_repr(Tuple{Float64, Float64, Float64, Float64}) == "NTuple{4, Float64}"
    @test Base_repr(Tuple{Float32, Float32, Float32}) == "Tuple{Float32, Float32, Float32}"
    @test Base_repr(Tuple{String, Int64, Int64, Int64}) == "Tuple{String, Int64, Int64, Int64}"
    @test Base_repr(Tuple{String, Int64, Int64, Int64, Int64}) == "Tuple{String, Vararg{Int64, 4}}"
    @test Base_repr(NTuple) == "NTuple{N, T} where {N, T}"
    @test Base_repr(Tuple{NTuple{N}, Vararg{NTuple{N}, 4}} where N) == "NTuple{5, NTuple{N, T} where T} where N"
    @test Base_repr(Tuple{Float64, NTuple{N}, Vararg{NTuple{N}, 4}} where N) == "Tuple{Float64, Vararg{NTuple{N, T} where T, 5}} where N"
end

@testset "NamedTuple printing" begin
    @test Base_repr(@NamedTuple{kw::Int64}) == "@NamedTuple{kw::Int64}"
    @test Base_repr(@NamedTuple{kw::Union{Float64, Int64}, kw2::Int64}) == "@NamedTuple{kw::Union{Float64, Int64}, kw2::Int64}"
    @test Base_repr(@NamedTuple{kw::@NamedTuple{kw2::Int64}}) == "@NamedTuple{kw::@NamedTuple{kw2::Int64}}"
    @test Base_repr(@NamedTuple{kw::NTuple{7, Int64}}) == "@NamedTuple{kw::NTuple{7, Int64}}"
    @test Base_repr(@NamedTuple{a::Float64, b}) == "@NamedTuple{a::Float64, b}"
    @test Base_repr(@NamedTuple{var"#"::Int64}) == "@NamedTuple{var\"#\"::Int64}"
    # abstract namedtuples cannot use @NamedTuple format
    @test Base_repr(NamedTuple{(:a, :b), NTuple{N, Int64}} where N) == "NamedTuple{(:a, :b), NTuple{N, Int64}} where N"
end

@testset "issue #42931" begin
    @test Base_repr(NTuple{4, :A}) == "Tuple{:A, :A, :A, :A}"
    @test Base_repr(NTuple{3, :A}) == "Tuple{:A, :A, :A}"
    @test Base_repr(NTuple{2, :A}) == "Tuple{:A, :A}"
    @test Base_repr(NTuple{1, :A}) == "Tuple{:A}"
    @test Base_repr(NTuple{0, :A}) == "Tuple{}"

    @test Base_repr(Tuple{:A, :A, :A, :B}) == "Tuple{:A, :A, :A, :B}"
    @test Base_repr(Tuple{:A, :A, :A, :A}) == "Tuple{:A, :A, :A, :A}"
    @test Base_repr(Tuple{:A, :A, :A}) == "Tuple{:A, :A, :A}"
    @test Base_repr(Tuple{:A}) == "Tuple{:A}"
    @test Base_repr(Tuple{}) == "Tuple{}"

    @test Base_repr(Tuple{Vararg{N, 10}} where N) == "NTuple{10, N} where N"
    @test Base_repr(Tuple{Vararg{10, N}} where N) == "Tuple{Vararg{10, N}} where N"
end
