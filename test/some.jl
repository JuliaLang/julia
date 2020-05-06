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

#Eltype
@test eltype(Some{Int}) == Int
@test eltype(Some(1)) == Int

# Broadcast with Some
@testset "Some Broadcast" begin
    @test Some(1) .+ 1 == 2
    @test Some([1, 2]) .+ [[1, 2,], [3, 4]] == [[2, 4], [4, 6]]
    @test isa.(Some([1,2,3]), [Array, Dict, Int]) == [true, false, false]
end
