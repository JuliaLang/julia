@testset "Type definitions" begin

test_mod = Module(:TestMod)

@test JuliaLowering.include_string(test_mod, """
abstract type A end
""") === nothing
@test supertype(test_mod.A) === Any
@test isabstracttype(test_mod.A)

@test JuliaLowering.include_string(test_mod, """
abstract type B <: A end
""") === nothing
@test supertype(test_mod.B) === test_mod.A

@test JuliaLowering.include_string(test_mod, """
abstract type C{X} end
""") === nothing

@test JuliaLowering.include_string(test_mod, """
abstract type D{X<:A} end
""") === nothing
@test test_mod.D{test_mod.B} isa Type
@test_throws Exception test_mod.D{Int}

@test JuliaLowering.include_string(test_mod, """
abstract type E <: C{E} end
""") === nothing
@test test_mod.E isa Type

@test JuliaLowering.include_string(test_mod, """
primitive type P <: A 16 end
""") === nothing
@test isconcretetype(test_mod.P)
@test supertype(test_mod.P) === test_mod.A
@test reinterpret(test_mod.P, 0x0001) isa test_mod.P
@test reinterpret(UInt16, reinterpret(test_mod.P, 0x1337)) === 0x1337

@test JuliaLowering.include_string(test_mod, """
struct S1{X,Y} <: A
    x::X
    y::Y
    z
end
""") === nothing
@test !isconcretetype(test_mod.S1)
@test fieldnames(test_mod.S1) == (:x, :y, :z)
@test fieldtypes(test_mod.S1) == (Any, Any, Any)
@test isconcretetype(test_mod.S1{Int,String})
@test fieldtypes(test_mod.S1{Int,String}) == (Int, String, Any)
@test supertype(test_mod.S1) == test_mod.A

# Inner constructors: one field non-Any
@test JuliaLowering.include_string(test_mod, """
struct S2
    x::Int
    y
end
""") === nothing
@test length(methods(test_mod.S2)) == 2
let s = test_mod.S2(42, "hi")
    # exact types
    @test s.x === 42
    @test s.y == "hi"
end
let s = test_mod.S2(42.0, "hi")
    # converted types
    @test s.x === 42
    @test s.y == "hi"
end

# Constructors: All fields Any
@test JuliaLowering.include_string(test_mod, """
struct S3
    x
    y
end
""") === nothing
@test length(methods(test_mod.S3)) == 1
let s = test_mod.S3(42, "hi")
    @test s.x === 42
    @test s.y == "hi"
end

# Inner constructors: All fields Any; dynamically tested against whatever
# S4_Field resolves to
@test JuliaLowering.include_string(test_mod, """
S4_Field = Any # actually Any!

struct S4
    x::S4_Field
    y
end
""") === nothing
@test length(methods(test_mod.S4)) == 1
let s = test_mod.S4(42, "hi")
    @test s.x === 42
    @test s.y == "hi"
end

# Inner & outer constructors; parameterized types
@test JuliaLowering.include_string(test_mod, """
struct S5{U}
    x::U
    y
end
""") === nothing
@test length(methods(test_mod.S5)) == 1
let s = test_mod.S5(42, "hi")
    @test s isa test_mod.S5{Int}
    @test s.x === 42
    @test s.y == "hi"
end
@test length(methods(test_mod.S5{Int})) == 1
let s = test_mod.S5{Int}(42.0, "hi")
    @test s isa test_mod.S5{Int}
    @test s.x === 42
    @test s.y == "hi"
end
let s = test_mod.S5{Any}(42.0, "hi")
    @test s isa test_mod.S5{Any}
    @test s.x === 42.0
    @test s.y == "hi"
end

# Test cases from
# https://github.com/JuliaLang/julia/issues/36104
# https://github.com/JuliaLang/julia/pull/36121
JuliaLowering.include_string(test_mod, """
# issue #36104
module M36104
struct T36104
    v::Vector{M36104.T36104}
end
struct T36104   # check that redefining it works, issue #21816
    v::Vector{T36104}
end
end
""")
@test fieldtypes(test_mod.M36104.T36104) == (Vector{test_mod.M36104.T36104},)
@test_throws ErrorException("expected") JuliaLowering.include_string(test_mod, """struct X36104; x::error("expected"); end""")
@test isdefined(test_mod, :X36104)
JuliaLowering.include_string(test_mod, "struct X36104; x::Int; end")
@test fieldtypes(test_mod.X36104) == (Int,)
JuliaLowering.include_string(test_mod, "primitive type P36104 8 end")
@test_throws ErrorException("invalid redefinition of constant TestMod.P36104") #=
    =# JuliaLowering.include_string(test_mod, "primitive type P36104 16 end")

# Struct with outer constructor where one typevar is constrained by the other
# See https://github.com/JuliaLang/julia/issues/27269)
@test JuliaLowering.include_string(test_mod, """
struct X27269{T, S <: Vector{T}}
    v::Vector{S}
end
""") === nothing
@test test_mod.X27269([[1,2]]) isa test_mod.X27269{Int, Vector{Int}}

end
