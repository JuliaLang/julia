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

# Test that `using` F brings in the symbol G immediately
F = JuliaLowering.include_string(test_mod, """
module F
    export G
    module G
        export G_global
        G_global = "exported from G"
    end
end
""")
JuliaLowering.include_string(test_mod, """
using .F, .G
""")
@test test_mod.F === F
@test test_mod.G === F.G
@test test_mod.G_global === "exported from G"

# Similarly, that import makes symbols available immediately
H = JuliaLowering.include_string(test_mod, """
module H
    module I
        module J
        end
    end
end
""")
JuliaLowering.include_string(test_mod, """
import .H.I, .I.J
""")
@test test_mod.I === H.I
@test test_mod.J === H.I.J
@test test_mod.G_global === "exported from G"

end
