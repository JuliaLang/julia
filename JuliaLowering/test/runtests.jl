using Test

using JuliaLowering
using JuliaSyntax
using JuliaSyntax: sourcetext
using JuliaLowering: @ast

include("utils.jl")

@testset "JuliaLowering.jl" begin

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
    g = JuliaLowering.ensure_attributes(ex.graph, scope_type=Symbol)
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
# Functions
@test JuliaLowering.include_string(test_mod, """
begin
    function f(x)
        y = x + 1
        "hi", x, y
    end

    f(1)
end
""") == ("hi", 1, 2)


#-------------------------------------------------------------------------------
# module
A = JuliaLowering.include_string(test_mod, """
module A
    function g()
        return "hi"
    end
end
""", "module_test")
@test A isa Module
@test A.g() == "hi"
@test A.include isa Function
@test A.Base === Base
@test A.eval(:(x = -1)) == -1 && A.x == -1

B = JuliaLowering.include_string(test_mod, """
baremodule B
end
""", "baremodule_test")
@test B.Core === Core
@test !isdefined(B, :include)
@test !isdefined(B, :Base)

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

#-------------------------------------------------------------------------------
# Syntax quoting & interpolation
ex = JuliaLowering.include_string(test_mod, """
begin
    x = 10
    y = :(g(z))
    quote
        f(\$(x+1), \$y)
    end
end
""")
@test ex ~ @ast_ [K"block"
    [K"call"
        "f"::K"Identifier"
        11::K"Value"
        [K"call"
            "g"::K"Identifier"
            "z"::K"Identifier"
        ]
    ]
]
@test sourcetext(ex[1]) == "f(\$(x+1), \$y)"
@test sourcetext(ex[1][2]) == "x+1"
@test sourcetext(ex[1][3]) == "g(z)"

# Test expression flags are preserved during interpolation
@test JuliaSyntax.is_infix_op_call(JuliaLowering.include_string(test_mod, """
let
    x = 1
    :(\$x + \$x)
end
"""))

#-------------------------------------------------------------------------------
# Macro expansion

JuliaLowering.include_string(test_mod, """
module M
    using JuliaLowering: JuliaLowering, @ast, @chk
    using JuliaSyntax

    # Introspection
    macro __MODULE__()
        __context__.mod
    end

    macro __FILE__()
        JuliaLowering.filename(__context__.macroname)
    end

    macro __LINE__()
        JuliaLowering.source_location(__context__.macroname)[1]
    end

    someglobal = "global in module M"

    # Macro with local variables
    macro foo(ex)
        :(let x = "`x` from @foo"
            (x, someglobal, \$ex)
        end)
    end

    # # Recursive macro call
    # # TODO: Need branching!
    # macro recursive(N)
    #     Nval = N.value #::Int
    #     if Nval < 1
    #         return N
    #     end
    #     quote
    #         x = \$N
    #         (@recursive \$(Nval-1), x)
    #     end
    # end
end
""")

@test JuliaLowering.include_string(test_mod, """
let 
    x = "`x` from outer scope"
    M.@foo x
end
""") == ("`x` from @foo", "global in module M", "`x` from outer scope")

@test JuliaLowering.include_string(test_mod, """
#line1
(M.@__MODULE__(), M.@__FILE__(), M.@__LINE__())
""", "foo.jl") == (test_mod, "foo.jl", 2)

Base.eval(test_mod.M, :(
# Recursive macro call
function var"@recursive"(mctx, N)
    @chk kind(N) == K"Integer"
    Nval = N.value::Int
    if Nval < 1
        return N
    end
    @ast mctx (@HERE) [K"block"
        [K"="(@HERE)
            "x"::K"Identifier"(@HERE)
            N
        ]
        [K"tuple"(@HERE)
            "x"::K"Identifier"(@HERE)
            [K"macrocall"(@HERE)
                "@recursive"::K"Identifier"
                (Nval-1)::K"Integer"
            ]
        ]
    ]
end
))

@test JuliaLowering.include_string(test_mod, """
M.@recursive 3
""") == (3, (2, (1, 0)))

@test_throws JuliaLowering.LoweringError JuliaLowering.include_string(test_mod, """
macro mmm(a; b=2)
end
""")

@test_throws JuliaLowering.LoweringError JuliaLowering.include_string(test_mod, """
macro A.b(ex)
end
""")

end
