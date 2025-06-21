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
@test_throws ErrorException("cannot convert a value to nothing for assignment") convert(Nothing, 1)

## show()

@test sprint(show, Some(1)) == "Some(1)"
@test sprint(show, Some(Some(1))) == "Some(Some(1))"
@test repr([Some(1)]) == "Some{$Int}[1]"
@test repr([Some(Some(1))]) == "Some{Some{$Int}}[Some(1)]"

##  == and isequal nothing

@test Some(1) !== nothing
@test Some(nothing) !== nothing
@test !isequal(Some(1), nothing)
@test !isequal(Some(nothing), nothing)

# Some with something else is false
@test !=(Some(nothing), nothing)
@test !=(nothing, Some(nothing))

# Two `Some`s forward to their wrapped things
@test ==(Some([0x1]), Some([1]))

# propagate wrapped missings
@test !=(Some(1), Some(missing)) isa Missing
@test !=(Some(missing), Some(1)) isa Missing
@test ==(Some(missing), Some(missing)) isa Missing

# Make sure to still propagate non-wrapped Missing
@test ==(Some(1), missing) isa Missing
@test ==(missing, Some(1)) isa Missing

@test isequal(Some([0x1]), Some([1]))
@test !isequal(missing, Some(missing))
@test !isequal(Some(missing), missing)
@test isequal(Some(missing), Some(missing))

# hashing implications
@test hash(Some(0x1)) != hash(0x1)
@test hash(Some(0x1)) == hash(Some(1))
@test hash((Some(1),)) != hash((1, Some))

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

@testset "@something" begin
    @test_throws ArgumentError @something()
    @test_throws ArgumentError @something(nothing)
    @test @something(1) === 1
    @test @something(Some(nothing)) === nothing

    @test @something(1, error("failed")) === 1
    @test_throws ErrorException @something(nothing, error("failed"))

    # Ensure that the internal variable doesn't conflict with a user defined variable
    @test let val = 1
        @something(val)
    end == 1
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

# type stability
@test fieldtype(typeof(Some(Int)), 1) === Type{Int}
