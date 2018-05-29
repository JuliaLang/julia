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

# coalesce()

for v in (nothing, missing)
    @test coalesce(1) === 1
    @test coalesce(v) === v
    @test coalesce(v, 1) === 1
    @test coalesce(1, v) === 1
    @test coalesce(v, v) === v
    @test coalesce(v, 1, 2) === 1
    @test coalesce(1, v, 2) === 1
    @test coalesce(v, v, 2) === 2
    @test coalesce(v, v, v) === v

    @test coalesce(Some(1)) === 1
    @test coalesce(Some(v)) === v
    @test coalesce(Some(1), 0) === 1
    @test coalesce(Some(v), 0) === v
    @test coalesce(v, Some(v)) === v
    @test coalesce(Some(1), v) === 1
    @test coalesce(v, Some(1)) === 1
    @test coalesce(v, Some(1), v) === 1
    @test coalesce(v, Some(1), Some(2)) === 1
    @test coalesce(Some(1), v, Some(2)) === 1

    @test coalesce(v, missing) === missing
    @test coalesce(v, nothing) === nothing
    @test coalesce(v, missing, v) === v
    @test coalesce(v, nothing, v) === v

    # issue #26927
    a = [missing, nothing, Some(nothing), Some(missing)]
    @test a isa Vector{Union{Missing, Nothing, Some}}
    @test a[1] === missing && a[2] === nothing && a[3] === Some(nothing) && a[4] === Some(missing)
    b = [ "replacement", "replacement", nothing, missing ]
    @test b isa Vector{Union{Missing, Nothing, String}}
    # the original operation from the issue, though it was not the source of the problem
    @test all(coalesce.(a, "replacement") .=== b)
end

# notnothing()

using Base: notnothing
@test notnothing(1) === 1
@test_throws ArgumentError notnothing(nothing)
