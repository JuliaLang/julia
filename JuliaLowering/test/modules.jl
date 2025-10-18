@testset "modules" begin

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

end
