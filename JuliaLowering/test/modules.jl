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

# modules allowed in nested code in global scope
@test typeof(JuliaLowering.include_string(test_mod, """
begin
    module C
    end
end
""")) == Module

# Modules not allowed in local scope
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
let
    module C
    end
end
""")
@test_throws LoweringError JuliaLowering.include_string(test_mod, """
function f()
    module C
    end
end
""")

end
