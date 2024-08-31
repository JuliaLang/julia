@testset "Declarations" begin

test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
begin
    local x::Int = 1.0
    x
end
""") === 1

# In value position, yeild the right hand side, not `x`
@test JuliaLowering.include_string(test_mod, """
local x::Int = 1.0
""") === 1.0

# TODO unadorned declarations
# @test JuliaLowering.include_string(test_mod, """
# let
#     x::Int = 1.0
# end
# """) === 1

@test JuliaLowering.include_string(test_mod, """
let
    local x::Int = 1
    x1 = x
    x = 20.0
    x2 = x
    (x1,x2)
end
""") === (1, 20)

# Global decls with types
@test JuliaLowering.include_string(test_mod, """
global a_typed_global::Int = 10.0
""") === 10.0
@test Core.get_binding_type(test_mod, :a_typed_global) === Int
@test test_mod.a_typed_global === 10

# Also allowed in nontrivial scopes in a top level thunk
@test JuliaLowering.include_string(test_mod, """
let
    global a_typed_global_2::Int = 10.0
end
""") === 10.0
@test Core.get_binding_type(test_mod, :a_typed_global_2) === Int
@test test_mod.a_typed_global_2 === 10

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
begin
    local x::T = 1
    local x::S = 1
end
""")

# Const not supported on locals
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
const local x = 1
""")
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
let
    const x = 1
end
""")

# global type decls only allowed at top level
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
function f()
    global x::Int = 1
end
""")

test_ir_cases(joinpath(@__DIR__, "decls_ir.jl"))

end
