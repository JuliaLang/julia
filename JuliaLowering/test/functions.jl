@testset "Functions" begin

test_mod = Module()

# Function calls
# Splatting
@test JuliaLowering.include_string(test_mod, """
let
    x = 1
    y = 2
    zs = (3,4)
    w = 5
    (tuple(zs...),
     tuple(zs..., w),
     tuple(y, zs...),
     tuple(x, y, zs..., w))
end
""") == ((3,4),
         (3,4,5),
         (2,3,4),
         (1,2,3,4,5))

# Keyword calls
Base.eval(test_mod, :(
begin
    function kwtest(; kws...)
        values(kws)
    end

    function kwtest()
        "non-kw version of kwtest"
    end
end
))


@test JuliaLowering.include_string(test_mod, """
let
    kws = (c=3,d=4)
    kwtest(; kws..., a=1, d=0, e=5)
end
""") == (c=3, d=0, a=1, e=5)

@test JuliaLowering.include_string(test_mod, """
let
    kws = (;)
    kwtest(; kws..., kws...)
end
""") == "non-kw version of kwtest"

# literal_pow
@test JuliaLowering.include_string(test_mod, """
2^4
""") == 16

#-------------------------------------------------------------------------------
# Function definitions
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
    function unused_arg(x, _, y)
        x + y
    end
    unused_arg(1,2,3)
end
""") == 4

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

# static parameters
@test JuliaLowering.include_string(test_mod, """
begin
    function h(x, y)
        "fallback"
    end
    function h(::Vector{T}, ::S) where {T, S <: T}
        T, S
    end
    (h(1, 2), h(Number[0xff], 1.0), h(Int[1], 1), h(Int[1], 1.0))
end
""") === ("fallback", (Number, Float64), (Int, Int), "fallback")

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

end
