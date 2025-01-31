@testset "using / import" begin

test_mod = Module()

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

JuliaLowering.include_string(test_mod, """
x = 1
y = 2
export x
public y
""")
@test Base.isexported(test_mod, :x)
@test Base.ispublic(test_mod, :x)
@test Base.ispublic(test_mod, :y)
@test !Base.isexported(test_mod, :y)

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

end
