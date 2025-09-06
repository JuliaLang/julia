@testset "JuliaLowering.jl" begin

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

# modules allowed in nested code in global scope
@test typeof(JuliaLowering.include_string(test_mod, """
begin
    module C
    end
end
""")) == Module

end
