@testset "using / import" begin

test_mod = Module()

# Test attributes are correctly set for export/public
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

# Test various forms of `using`
C = JuliaLowering.include_string(test_mod, """
module C
    module D
        export x
        public y, f
        x = [101]
        y = [202]

        function f()
            "hi"
        end
    end
    module E
        using ..D: f
        using ..D
        using .D: y as D_y
        using .D: x as D_x_2, y as D_y_2
        import .D.y as D_y_3
    end
end
""")
@test C.D.f === C.E.f
@test C.D.x === C.E.x
@test C.D.y === C.E.D_y
@test C.D.x === C.E.D_x_2
@test C.D.y === C.E.D_y_2
@test C.D.y === C.E.D_y_3

# Test that using F brings in the exported symbol G immediately and that it can
# be used next in the import list.
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
