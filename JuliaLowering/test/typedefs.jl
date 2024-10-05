@testset "Type definitions" begin

test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
abstract type A end
""") === nothing
@test supertype(test_mod.A) === Any

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

end
