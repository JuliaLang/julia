# This file is a part of Julia. License is MIT: https://julialang.org/license

## eltype()

@test eltype(Some(1)) === Int

## promote()

@test promote_type(Some{Int}, Some{Float64}) === Some{Float64}

## convert()

# These conversions must fail to prevent ambiguities
# when a value to wrap is already a Some or a Void
@test_throws MethodError convert(Some, 1)
@test_throws MethodError convert(Union{Some, Void}, 1)
@test_throws MethodError convert(Some{Int}, 1)
@test_throws MethodError convert(Union{Some{Int}, Void}, 1)

@test convert(Some, Some(1)) === convert(Union{Some, Void}, Some(1)) === Some(1)
@test convert(Some{Int}, Some(1)) === convert(Union{Some{Int}, Void}, Some(1)) === Some(1)
@test convert(Some{Int}, Some(1.0)) === Some(1)
@test convert(Union{Some{Int}, Void}, Some(1.0)) === Some(1)

@test_throws MethodError convert(Some, nothing)
@test_throws MethodError convert(Some{Int}, nothing)

@test convert(Some, Some(nothing)) === Some(nothing)
@test convert(Some{Void}, Some(nothing)) === Some(nothing)
@test convert(Some, Some(nothing)) === Some(nothing)
@test convert(Some{Void}, Some(nothing)) === Some(nothing)

@test convert(Union{Some, Void}, nothing) === nothing
@test convert(Union{Some, Void}, nothing) === nothing
@test convert(Union{Some{Int}, Void}, nothing) === nothing

@test convert(Union{Int, Void}, nothing) === nothing
@test convert(Union{Int, Void}, 1) === 1
@test convert(Union{Int, Void}, 1.0) === 1
@test convert(Void, nothing) === nothing
@test_throws MethodError convert(Void, 1)

## show()

@test sprint(show, Some(1)) == "Some(1)"
@test sprint(showcompact, Some(1)) == "1"
@test sprint(show, Some(Some(1))) == "Some(Some(1))"
@test sprint(showcompact, Some(Some(1))) == "1"

##  == and isequal nothing

@test Some(1) != nothing
@test Some(nothing) != nothing
@test !isequal(Some(1), nothing)
@test !isequal(Some(nothing), nothing)

## get()
@test get(Some(1)) === 1
@test get(Some(nothing)) === nothing
@test_throws MethodError get(nothing)

@test get(Some(1), 0) === 1
@test get(Some(nothing), 0) === nothing
@test get(nothing, 0) === 0
