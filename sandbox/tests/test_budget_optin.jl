# Tests for opt-in type node budget via IOContext properties.
include("../show_type.jl")

using Test

# Helper: show a type with a given budget and return the string.
function show_with_budget(T, budget::Int)
    buf = IOBuffer()
    io = IOContext(buf, :type_budget => Ref(budget))
    Base_show(io, T)
    return String(take!(buf))
end

@testset "opt-in type node budget via IOContext" begin
    T = Vector{Vector{Vector{Vector{Int}}}}
    full = "Vector{Vector{Vector{Vector{Int64}}}}"

    @testset "generous budget produces full output" begin
        str = show_with_budget(T, 100)
        @test str == full
    end

    @testset "tight budget produces truncated output" begin
        str = show_with_budget(T, 3)
        @test contains(str, "…")
        @test startswith(str, "Vector{")
        @test sizeof(str) < sizeof(full)
    end

    @testset "budget of 1 truncates after outermost type" begin
        str = show_with_budget(T, 1)
        @test contains(str, "…")
    end

    @testset "budget applies across complex types" begin
        T2 = Dict{String, Vector{Pair{Symbol, Int}}}
        str_full = sprint(Base_show, T2)
        @test !contains(str_full, "…")

        str_limited = show_with_budget(T2, 3)
        @test contains(str_limited, "…")
        @test sizeof(str_limited) < sizeof(str_full)
    end

    @testset "budget is shared across recursive calls" begin
        T3 = Tuple{Vector{Int}, Dict{String, Float64}, Set{Symbol}}
        budget = Ref(5)
        buf = IOBuffer()
        io = IOContext(buf, :type_budget => budget)
        Base_show(io, T3)
        str = String(take!(buf))
        # The budget should have been decremented by each type node visited
        @test budget[] < 5
        # With a budget of 5, some nodes should be truncated
        @test contains(str, "…")
    end

    @testset "no budget properties means no truncation" begin
        T4 = Vector{Vector{Vector{Vector{Vector{Int}}}}}
        buf = IOBuffer()
        io = IOContext(buf)
        Base_show(io, T4)
        str = String(take!(buf))
        @test !contains(str, "…")
        @test str == "Vector{Vector{Vector{Vector{Vector{Int64}}}}}"
    end
end
