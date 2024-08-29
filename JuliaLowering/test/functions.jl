@testset "Functions" begin

test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
begin
    function f(x)
        y = x + 1
        "hi", x, y
    end

    f(1)
end
""") == ("hi", 1, 2)

@test JuliaLowering.include_string(test_mod, """
begin
    function g(x)::Int
        if x == 1
            return 42.0
        end
        0xff
    end
    (g(1), g(2))
end
""") === (42, 255)

Base.include_string(test_mod,
"""
    struct X end

    # Erroneous `convert` to test type assert in function return values
    Base.convert(::Type{X}, y) = y
""")

@test_throws TypeError JuliaLowering.include_string(test_mod, """
begin
    function h()::X
        return nothing
    end
    h()
end
""")

test_ir_cases(joinpath(@__DIR__, "functions_ir.jl"))

end
