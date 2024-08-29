using Test

include("utils.jl")

@testset "JuliaLowering.jl" begin

include("syntax_graph.jl")

# Basic end-to-end / smoke tests

test_mod = Module()

#-------------------------------------------------------------------------------
# Scopes
@test JuliaLowering.include_string(test_mod,
"""
let
    y = 0
    x = 1
    let x = x + 1
        y = x
    end
    (x, y)
end
""") == (1, 2)

JuliaLowering.include_string(test_mod, """
    x = 101
    y = 202
""")
@test test_mod.x == 101
@test test_mod.y == 202
@test JuliaLowering.include_string(test_mod, "x + y") == 303

# wrap expression in scope block of `scope_type`
function wrapscope(ex, scope_type)
    g = JuliaLowering.ensure_attributes(ex._graph, scope_type=Symbol)
    ex = JuliaLowering.reparent(g, ex)
    makenode(ex, ex, K"scope_block", ex; scope_type=scope_type)
end

assign_z_2 = parsestmt(SyntaxTree, "begin z = 2 end", filename="foo.jl")
JuliaLowering.eval(test_mod, :(z=1))
@test test_mod.z == 1
# neutral (eg, for loops) and hard (eg, let) scopes create a new binding for z
JuliaLowering.eval(test_mod, wrapscope(assign_z_2, :neutral))
@test test_mod.z == 1
JuliaLowering.eval(test_mod, wrapscope(assign_z_2, :hard))
@test test_mod.z == 1
# but wrapping neutral scope in soft scope uses the existing binding in test_mod
JuliaLowering.eval(test_mod, wrapscope(wrapscope(assign_z_2, :neutral), :soft))
@test test_mod.z == 2

#-------------------------------------------------------------------------------
# Blocks
@test JuliaLowering.include_string(test_mod, """
begin
end
""") == nothing

#-------------------------------------------------------------------------------
# Placeholders
@test JuliaLowering.include_string(test_mod, """_ = 10""") == 10

assign_underscore = parsestmt(SyntaxTree, "_ + 1", filename="foo.jl")
exc = try
    JuliaLowering.eval(test_mod, assign_underscore)
catch exc
    exc
end
@test exc.msg == "all-underscore identifiers are write-only and their values cannot be used in expressions"
@test JuliaLowering.is_ancestor(exc.ex, assign_underscore[1])

#-------------------------------------------------------------------------------
# Declarations

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

@test_throws LoweringError JuliaLowering.include_string(test_mod, """
begin
    local x::T = 1
    local x::S = 1
end
""")

test_ir_cases(joinpath(@__DIR__, "decls_ir.jl"))

#-------------------------------------------------------------------------------
# Function calls
# Splatting
@test JuliaLowering.include_string(test_mod, """
let
    x = 1
    y = 2
    zs = (3,4)
    w = 5
    (tuple(zs...),
     tuple(zs..., w),
     tuple(y, zs...),
     tuple(x, y, zs..., w))
end
""") == ((3,4),
         (3,4,5),
         (2,3,4),
         (1,2,3,4,5))

#-------------------------------------------------------------------------------
# using / import
JuliaLowering.include_string(test_mod, """
    using JuliaSyntax
    using JuliaLowering: SyntaxTree
    using JuliaLowering: SyntaxTree as st
    import JuliaLowering: SyntaxTree as st1, SyntaxTree as st2
""")
@test test_mod.SyntaxTree === JuliaLowering.SyntaxTree
@test test_mod.st === JuliaLowering.SyntaxTree
@test test_mod.st1 === JuliaLowering.SyntaxTree
@test test_mod.st2 === JuliaLowering.SyntaxTree
@test test_mod.parsestmt === JuliaSyntax.parsestmt

C = JuliaLowering.include_string(test_mod, """
module C
    module D
        function f()
            "hi"
        end
    end
    module E
        using ...C.D: f
    end
end
""")
@test C.D.f === C.E.f

include("functions.jl")
include("macros.jl")
include("modules.jl")
include("desugaring.jl")
include("branching.jl")
include("loops.jl")

end
