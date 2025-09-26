@testset "modules and top level code" begin

test_mod = Module()

A = JuliaLowering.include_string(test_mod, """
module A
    function g()
        return "hi"
    end
end
""", "module_test")
@test A isa Module
@test A.g() == "hi"
@test A.include isa Base.IncludeInto
@test A.eval isa Core.EvalInto
@test A.Base === Base
@test A.eval(:(x = -2)) == -2
@test A.x == -2

B = JuliaLowering.include_string(test_mod, """
baremodule B
end
""", "baremodule_test")
@test B.Core === Core
@test !isdefined(B, :include)
@test !isdefined(B, :eval)
@test !isdefined(B, :Base)

# Module init order
Amod = JuliaLowering.include_string(test_mod, """
module A
    init_order = []
    __init__() = push!(init_order, "A")
    module B
        using ..A
        __init__() = push!(A.init_order, "B")
    end
    module C
        using ..A
        __init__() = push!(A.init_order, "C")
        module D
            using ...A
            __init__() = push!(A.init_order, "D")
        end
        module E
            using ...A
            __init__() = push!(A.init_order, "E")
        end
    end
end
""")
@test Amod.init_order == ["B", "D", "E", "C", "A"]

# Macros in top level and module expressions may be used immediately in the
# next top level statement after their definition
@test JuliaLowering.include_string(test_mod, """
macro mac1()
    :(42)
end

@mac1
""") === 42

@test JuliaLowering.include_string(test_mod, """
module ModuleTopLevelEvalWorldTest
    macro mac2()
        :(101)
    end

    xx = @mac2
end

ModuleTopLevelEvalWorldTest.xx
""") === 101

function test_mapexpr_1(ex::JuliaLowering.SyntaxTree)
    JuliaLowering.@ast JuliaSyntax.syntax_graph(ex) ex 10101::K"Integer"
end

@test JuliaLowering.include_string(test_mapexpr_1, test_mod, """
this + expression + will + be + replaced
""") === 10101

function test_mapexpr_2(ex::Expr)
    20202
end

@test JuliaLowering.include_string(test_mapexpr_2, test_mod, """
this + expression + will + be + replaced
"""; expr_compat_mode=true) === 20202

end
