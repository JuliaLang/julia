@testset "Type definitions" begin

test_mod = Module(:TestMod)

Base.eval(test_mod, :(struct XX{S,T,U,W} end))

@test JuliaLowering.include_string(test_mod, """
XX{Int, <:Integer, Float64, >:AbstractChar}
""") == (test_mod.XX{Int, T, Float64, S} where {T <: Integer, S >: AbstractChar})

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
@test JuliaLowering.include_string(test_mod, """
function S5{Int}(x::Int)
    S5(x, x)
end
""") === nothing
let s = test_mod.S5{Int}(1)
    @test s.x === 1
    @test s.y === 1
    @test s isa test_mod.S5{Int}
end
@test_throws MethodError test_mod.S5{Int}(1.1)
@test JuliaLowering.include_string(test_mod, """
function S5{T}(x, y, z) where {T<:AbstractFloat}
    S5(x, x)
end
""") === nothing
let s = test_mod.S5{Float64}(Float64(1.1), 0, 0)
    @test s.x === 1.1
    @test s.y === 1.1
    @test s isa test_mod.S5{Float64}
end
@test JuliaLowering.include_string(test_mod, """
S5{<:AbstractFloat}(x) = S5(x, x)
""") === nothing
let s = test_mod.S5{<:AbstractFloat}(Float64(1.1))
    @test s.x === 1.1
    @test s.y === 1.1
    @test s isa test_mod.S5{Float64}
end
@test JuliaLowering.include_string(test_mod, """
S5{T}(x::T) where {T<:Real} = S5(x, x)
""") === nothing
let s = test_mod.S5{Real}(pi)
    @test s.x === pi
    @test s.y === pi
    @test s isa test_mod.S5{<:Real}
end
outer_mod = Module()
@test JuliaLowering.include_string(test_mod, """
Base.Vector{T}(x::T) where {S5<:T<:S5} = T[x]
""") === nothing
let v = Base.Vector{test_mod.S5}(test_mod.S5(1,1))
    @test v isa Vector{test_mod.S5}
    @test v[1] === test_mod.S5(1,1)
end

# User defined inner constructors and helper functions for structs without type params
@test JuliaLowering.include_string(test_mod, """
struct S6
    x
    S6_f() = new(42)

    "some docs"
    S6() = S6_f()
    S6(x) = new(x)
end
""") === nothing
let s = test_mod.S6()
    @test s isa test_mod.S6
    @test s.x === 42
end
let s = test_mod.S6(2)
    @test s isa test_mod.S6
    @test s.x === 2
end
@test docstrings_equal(@doc(test_mod.S6), Markdown.doc"some docs")

# User defined inner constructors and helper functions for structs with type params
@test JuliaLowering.include_string(test_mod, """
struct S7{S,T}
    x::S
    y

    # Cases where full struct type may be deduced and used in body
    S7{Int,String}() = new(10.0, "y1")
    S7{S,T}() where {S,T} = new(10.0, "y2")
    S7{Int,T}() where {T} = new(10.0, "y3")
    (::Type{S7{Int,UInt8}})() = new{Int,UInt8}(10.0, "y4")

    # Cases where new{...} is called
    S7() = new{Int,Int}(10.0, "y5")
    S7{UInt8}() = S7_f()
    S7_f() = new{UInt8,UInt8}(10.0, "y6")
end
""") === nothing
let s = test_mod.S7{Int,String}()
    @test s isa test_mod.S7{Int,String}
    @test s.x === 10
    @test s.y === "y1"
end
let s = test_mod.S7{UInt16,UInt16}()
    @test s isa test_mod.S7{UInt16,UInt16}
    @test s.x === UInt16(10)
    @test s.y === "y2"
end
let s = test_mod.S7{Int,UInt16}()
    @test s isa test_mod.S7{Int,UInt16}
    @test s.x === 10
    @test s.y === "y3"
end
let s = test_mod.S7{Int,UInt8}()
    @test s isa test_mod.S7{Int,UInt8}
    @test s.x === 10
    @test s.y === "y4"
end
let s = test_mod.S7()
    @test s isa test_mod.S7{Int,Int}
    @test s.x === 10
    @test s.y === "y5"
end
let s = test_mod.S7{UInt8}()
    @test s isa test_mod.S7{UInt8,UInt8}
    @test s.x === UInt8(10)
    @test s.y === "y6"
end

# new() with splats and typed fields
@test JuliaLowering.include_string(test_mod, """
struct S8
    x::Int
    y::Float64

    S8(xs, ys) = new(xs..., ys...)
end
""") === nothing
let s = test_mod.S8((10.0,), (20,))
    @test s isa test_mod.S8
    @test s.x === 10
    @test s.y === 20.0
end
# Wrong number of args checked by lowering
@test_throws ArgumentError test_mod.S8((1,), ())
@test_throws ArgumentError test_mod.S8((1,2,3), ())

# new() with splats and untyped fields
@test JuliaLowering.include_string(test_mod, """
struct S9
    x
    y

    S9(xs) = new(xs...)
end
""") === nothing
let s = test_mod.S9((10.0,20))
    @test s isa test_mod.S9
    @test s.x === 10.0
    @test s.y === 20
end
# Wrong number of args checked by the runtime
@test_throws ArgumentError test_mod.S9((1,))
@test_throws ArgumentError test_mod.S9((1,2,3))

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
@test !isdefined(test_mod, :X36104)
JuliaLowering.include_string(test_mod, "struct X36104; x::Int; end")
@test fieldtypes(test_mod.X36104) == (Int,)
JuliaLowering.include_string(test_mod, "primitive type P36104 8 end")
JuliaLowering.include_string(test_mod, "const orig_P36104 = P36104")
JuliaLowering.include_string(test_mod, "primitive type P36104 16 end")
@test test_mod.P36104 !== test_mod.orig_P36104

# Struct with outer constructor where one typevar is constrained by the other
# See https://github.com/JuliaLang/julia/issues/27269)
@test JuliaLowering.include_string(test_mod, """
struct X27269{T, S <: Vector{T}}
    v::Vector{S}
end
""") === nothing
@test test_mod.X27269([[1,2]]) isa test_mod.X27269{Int, Vector{Int}}

end
