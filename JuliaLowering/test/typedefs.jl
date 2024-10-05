@testset "Type definitions" begin

test_mod = Module()

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

end
