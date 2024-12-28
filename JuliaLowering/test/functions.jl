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

    # Note this definition generates an arguably-spurious warning when run via
    # `Pkg.test()` due to the use of `--warn-override=true` in the test
    # harness.
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

Base.eval(test_mod,
:(struct X1{T} end)
)

# `where` params used in function obj type
@test JuliaLowering.include_string(test_mod, """
begin
    function (x::X1{T})() where T
        T
    end
    X1{Int}()()
end
""") === Int

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

# Default positional arguments
@test JuliaLowering.include_string(test_mod, """
begin
    function f_def_simple(x=1, y=2, z=x)
        (x,y,z)
    end

    (f_def_simple(), f_def_simple(10), f_def_simple(10,20), f_def_simple(10,20,30))
end
""") == ((1,2,1), (10,2,10), (10,20,10), (10,20,30))

@test JuliaLowering.include_string(test_mod, """
begin
    function f_def_placeholders(::T=1, _::S=1.0) where {T,S}
        (T,S)
    end

    (f_def_placeholders(), f_def_placeholders(1.0), f_def_placeholders(1.0, 1))
end
""") == ((Int,Float64), (Float64,Float64), (Float64,Int))

@test JuliaLowering.include_string(test_mod, """
begin
    function f_def_typevars(x, y::S=[1], z::U=2) where {T, S<:AbstractVector{T}, U}
        (x, y, z, T, S, U)
    end

    (f_def_typevars(1), f_def_typevars(1,[1.0]), f_def_typevars(1,[1.0],-1.0))
end
""") == ((1, [1], 2, Int, Vector{Int}, Int),
         (1, [1.0], 2, Float64, Vector{Float64}, Int),
         (1, [1.0], -1.0, Float64, Vector{Float64}, Float64))

@test JuliaLowering.include_string(test_mod, """
begin
    function f_def_slurp(x=1, ys...)
        (x, ys)
    end

    (f_def_slurp(), f_def_slurp(2), f_def_slurp(2,3))
end
""") == ((1, ()),
         (2, ()),
         (2, (3,)))

@test JuliaLowering.include_string(test_mod, """
begin
    function f_def_slurp_splat(ys...=(1,2)...)
        ys
    end

    (f_def_slurp_splat(), f_def_slurp_splat(10,20))
end
""") == ((1,2),
         (10,20))

@test JuliaLowering.include_string(test_mod, """
begin
    function f_destructure(x, (y,z)::Tuple{Int,Int}, (w,)...=(4,)...)
        (x,y,z,w)
    end

    f_destructure(1, (2,3))
end
""") == (1,2,3,4)

@testset "Slot flags" begin

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nospecialize(u, v, @nospecialize(x), y, @nospecialize(z))
            (u, v, x, y, z)
        end

        f_nospecialize(1,2,3,4,5)
    end
    """) == (1,2,3,4,5)
    # We dig into the internal of `Method` here to check which slots have been
    # flagged as nospecialize.
    @test only(methods(test_mod.f_nospecialize)).nospecialize == 0b10100

    JuliaLowering.include_string(test_mod, """
    function f_slotflags(x, y, f, z)
        f() + x + y
    end
    """)
    @test only(methods(test_mod.f_slotflags)).called == 0b0100

end

end
