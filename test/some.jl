# This file is a part of Julia. License is MIT: https://julialang.org/license

## promote()

@test promote_type(Some{Int}, Some{Float64}) == Some
@test promote_type(Some{Int}, Some{Real}) == Some{Real}
@test promote_type(Some{Int}, Nothing) == Union{Some{Int},Nothing}

## convert()

# These conversions must fail to prevent ambiguities
# when a value to wrap is already a Some or a Nothing
@test_throws MethodError convert(Some, 1)
@test_throws MethodError convert(Union{Some, Nothing}, 1)
@test_throws MethodError convert(Some{Int}, 1)
@test_throws MethodError convert(Union{Some{Int}, Nothing}, 1)

@test convert(Some, Some(1)) === convert(Union{Some, Nothing}, Some(1)) === Some(1)
@test convert(Some{Int}, Some(1)) === convert(Union{Some{Int}, Nothing}, Some(1)) === Some(1)
@test convert(Some{Int}, Some(1.0)) === Some(1)
@test convert(Union{Some{Int}, Nothing}, Some(1.0)) === Some(1)

@test_throws MethodError convert(Some, nothing)
@test_throws MethodError convert(Some{Int}, nothing)

@test convert(Some, Some(nothing)) === Some(nothing)
@test convert(Some{Nothing}, Some(nothing)) === Some(nothing)

@test convert(Union{Some, Nothing}, nothing) === nothing
@test convert(Union{Some{Int}, Nothing}, nothing) === nothing

@test convert(Union{Int, Nothing}, nothing) === nothing
@test convert(Union{Int, Nothing}, 1) === 1
@test convert(Union{Int, Nothing}, 1.0) === 1
@test convert(Nothing, nothing) === nothing
@test_throws MethodError convert(Nothing, 1)

## show()

@test sprint(show, Some(1)) == "Some(1)"
@test sprint(show, Some(Some(1))) == "Some(Some(1))"
@test repr([Some(1)]) == "Some{$Int}[1]"
@test repr([Some(Some(1))]) == "Some{Some{$Int}}[Some(1)]"

##  == and isequal nothing

@test Some(1) != nothing
@test Some(nothing) != nothing
@test !isequal(Some(1), nothing)
@test !isequal(Some(nothing), nothing)

@testset "something" begin
    @test_throws ArgumentError something()
    @test something(1) === 1
    @test something(missing) === missing
    @test_throws ArgumentError something(nothing)
    @test something(nothing, 1) === 1
    @test something(1, nothing) === 1
    @test_throws ArgumentError something(nothing, nothing)
    @test something(nothing, 1, 2) === 1
    @test something(1, nothing, 2) === 1
    @test something(nothing, nothing, 2) === 2

    @test something(Some(1)) === 1
    @test something(Some(nothing)) === nothing
    @test something(Some(missing)) === missing
    @test something(Some(1), 0) === 1
    @test something(Some(nothing), 0) === nothing
    @test something(nothing, Some(nothing)) === nothing
    @test something(Some(1), nothing) === 1
    @test something(nothing, Some(1)) === 1
    @test something(nothing, Some(1), nothing) === 1
    @test something(nothing, Some(1), Some(2)) === 1
    @test something(Some(1), nothing, Some(2)) === 1

    @test something(nothing, missing) === missing
    @test something(missing, nothing) === missing
    @test something(nothing, missing, nothing) === missing
    @test something(missing, nothing, missing) === missing
end

# issue #26927
a = [missing, nothing, Some(nothing), Some(missing)]
@test a isa Vector{Union{Missing, Nothing, Some}}
@test a[1] === missing && a[2] === nothing && a[3] === Some(nothing) && a[4] === Some(missing)
b = [ "replacement", "replacement", nothing, missing ]
@test b isa Vector{Union{Missing, Nothing, String}}
# the original operation from the issue, though it was not the source of the problem
@test all(coalesce.(a, "replacement") .=== ["replacement", nothing, Some(nothing), Some(missing)])
@test all(something.(a, "replacement") .=== [missing, "replacement", nothing, missing])

# notnothing()

using Base: notnothing
@test notnothing(1) === 1
@test_throws ArgumentError notnothing(nothing)

# isnothing()
@test !isnothing(1)
@test isnothing(nothing)

# skipnothing()
@testset "skipnothing" begin
    x = skipnothing([1, 2, nothing, 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = skipnothing([1  2; nothing 4])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = collect(skipnothing([nothing]))
    @test eltype(x) === Union{}
    @test isempty(collect(x))
    @test collect(x) isa Vector{Union{}}

    x = collect(skipnothing(Union{Int, Nothing}[]))
    @test eltype(x) === Int
    @test isempty(collect(x))
    @test collect(x) isa Vector{Int}

    x = skipnothing([nothing, nothing, 1, 2, nothing, 4, nothing, nothing])
    @test eltype(x) === Int
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    x = skipnothing(v for v in [nothing, 1, nothing, 2, 4])
    @test eltype(x) === Any
    @test collect(x) == [1, 2, 4]
    @test collect(x) isa Vector{Int}

    @testset "mapreduce" begin
        # Vary size to test splitting blocks with several configurations of nothing values
        for T in (Int, Float64),
            A in (rand(T, 10), rand(T, 1000), rand(T, 10000))
            if T === Int
                @test sum(A) === sum(skipnothing(A)) ===
                    reduce(+, skipnothing(A)) === mapreduce(identity, +, skipnothing(A))
            else
                @test sum(A) ≈ sum(skipnothing(A)) ===
                    reduce(+, skipnothing(A)) === mapreduce(identity, +, skipnothing(A))
            end
            @test mapreduce(cos, *, A) ≈ mapreduce(cos, *, skipnothing(A))

            B = Vector{Union{T,Nothing}}(A)
            replace!(x -> rand(Bool) ? x : nothing, B)
            if T === Int
                @test sum(collect(skipnothing(B))) === sum(skipnothing(B)) ===
                    reduce(+, skipnothing(B)) === mapreduce(identity, +, skipnothing(B))
            else
                @test sum(collect(skipnothing(B))) ≈ sum(skipnothing(B)) ===
                    reduce(+, skipnothing(B)) === mapreduce(identity, +, skipnothing(B))
            end
            @test mapreduce(cos, *, collect(skipnothing(A))) ≈ mapreduce(cos, *, skipnothing(A))

            # Test block full of nothing values
            B[1:length(B)÷2] .= nothing
            if T === Int
                @test sum(collect(skipnothing(B))) == sum(skipnothing(B)) ==
                    reduce(+, skipnothing(B)) == mapreduce(identity, +, skipnothing(B))
            else
                @test sum(collect(skipnothing(B))) ≈ sum(skipnothing(B)) ==
                    reduce(+, skipnothing(B)) == mapreduce(identity, +, skipnothing(B))
            end

            @test mapreduce(cos, *, collect(skipnothing(A))) ≈ mapreduce(cos, *, skipnothing(A))
        end

        # Patterns that exercize code paths for inputs with 1 or 2 non-nothing values
        @test sum(skipnothing([1, nothing, nothing, nothing])) === 1
        @test sum(skipnothing([nothing, nothing, nothing, 1])) === 1
        @test sum(skipnothing([1, nothing, nothing, nothing, 2])) === 3
        @test sum(skipnothing([nothing, nothing, nothing, 1, 2])) === 3

        for n in 0:3
            itr = skipnothing(Vector{Union{Int,Nothing}}(fill(nothing, n)))
            @test sum(itr) == reduce(+, itr) == mapreduce(identity, +, itr) === 0
            @test_throws ArgumentError reduce(x -> x/2, itr)
            @test_throws ArgumentError mapreduce(x -> x/2, +, itr)
        end
    end
end
