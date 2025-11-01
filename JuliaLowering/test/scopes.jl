@testset "Scopes" begin

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

@test JuliaLowering.include_string(test_mod, """
begin
    local x = 1
    local x = 2
    let (x,y) = (:x,:y)
        (y,x)
    end
end
""") === (:y,:x)

# Types on left hand side of type decls refer to the outer scope
# (In the flisp implementation they refer to the inner scope, but this seems
# like a bug.)
@test JuliaLowering.include_string(test_mod, """
let x::Int = 10.0
    local Int = Float64
    x
end
""") === 10

# Closures in let syntax can only capture values from the outside
# (In the flisp implementation it captures from inner scope, but this is
# inconsistent with let assignment where the rhs refers to the outer scope and
# thus seems like a bug.)
@test JuliaLowering.include_string(test_mod, """
begin
    local y = :outer_y
    let f() = y
        local y = :inner_y
        f()
    end
end
""") === :outer_y

# wrap expression in scope block of `scope_type`
function wrapscope(ex, scope_type)
    g = JuliaLowering.ensure_attributes(ex._graph, scope_type=Symbol)
    ex = JuliaLowering.reparent(g, ex)
    makenode(ex, ex, K"scope_block", ex; scope_type=scope_type)
end

assign_z_2 = parsestmt(SyntaxTree, "begin z = 2 end", filename="foo.jl")
Base.eval(test_mod, :(z=1))
@test test_mod.z == 1
# neutral (eg, for loops) and hard (eg, let) scopes create a new binding for z
JuliaLowering.eval(test_mod, wrapscope(assign_z_2, :neutral))
@test test_mod.z == 1
JuliaLowering.eval(test_mod, wrapscope(assign_z_2, :hard))
@test test_mod.z == 1
# but wrapping neutral scope in soft scope uses the existing binding in test_mod
JuliaLowering.eval(test_mod, wrapscope(wrapscope(assign_z_2, :neutral), :soft))
@test test_mod.z == 2

end
