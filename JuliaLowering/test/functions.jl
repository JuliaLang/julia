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

@testset "Default positional arguments" begin
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
        function f_def_ret_type(x=1.0)::Int
            x
        end

        (f_def_ret_type(), f_def_ret_type(10.0))
    end
    """) === (1,10)

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
        function f_def_destructure(x, (y,z)::Tuple{Int,Int}, (w,)...=(4,)...)
            (x,y,z,w)
        end

        f_def_destructure(1, (2,3))
    end
    """) == (1,2,3,4)

end

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

@testset "Keyword functions" begin
    JuliaLowering.include_string(test_mod, """
    function f_kw_simple(a::Int=1, b::Float64=1.0; x::Char='a', y::Bool=true)
        (a, b, x, y)
    end
    """)

    @test test_mod.f_kw_simple()               === (1, 1.0, 'a', true)
    @test test_mod.f_kw_simple(x='b')          === (1, 1.0, 'b', true)
    @test test_mod.f_kw_simple(y=false)        === (1, 1.0, 'a', false)
    @test test_mod.f_kw_simple(x='b', y=false) === (1, 1.0, 'b', false)

    @test test_mod.f_kw_simple(20)                 === (20, 1.0, 'a', true)
    @test test_mod.f_kw_simple(20; x='b')          === (20, 1.0, 'b', true)
    @test test_mod.f_kw_simple(20; y=false)        === (20, 1.0, 'a', false)
    @test test_mod.f_kw_simple(20; x='b', y=false) === (20, 1.0, 'b', false)

    @test test_mod.f_kw_simple(20, 2.0)                 === (20, 2.0, 'a', true)
    @test test_mod.f_kw_simple(20, 2.0; x='b')          === (20, 2.0, 'b', true)
    @test test_mod.f_kw_simple(20, 2.0; y=false)        === (20, 2.0, 'a', false)
    @test test_mod.f_kw_simple(20, 2.0; x='b', y=false) === (20, 2.0, 'b', false)

    # Bad types for keyword args throw a type error
    @test_throws(TypeError(Symbol("keyword argument"), :x, Char, 100),
                 test_mod.f_kw_simple(x=100))
    @test_throws(TypeError(Symbol("keyword argument"), :y, Bool, 100),
                 test_mod.f_kw_simple(y=100))

    # Keywords which aren't present throw an error
    try
        test_mod.f_kw_simple(20; not_present=100)
        @test false
    catch exc
        @test exc isa MethodError
        @test exc.f == Core.kwcall
        @test exc.args == ((; not_present=100), test_mod.f_kw_simple, 20, 1.0)
    end

    # Slurping of positional args with keywords
    JuliaLowering.include_string(test_mod, """
    function f_pos_slurp_with_kws(z, args...; x=1,y=2)
        args
    end
    """)
    @test test_mod.f_pos_slurp_with_kws(3, 2, 1; x = 100) === (2,1)
    @test test_mod.f_pos_slurp_with_kws(3, 2, 1) === (2,1)

    # Slurping of keyword args
    JuliaLowering.include_string(test_mod, """
    function f_kw_slurp_all(; kws...)
        kws
    end
    """)
    @test values(test_mod.f_kw_slurp_all(x = 1, y = 2)) === (x=1, y=2)
    @test values(test_mod.f_kw_slurp_all()) === (;)

    # Slurping of keyword args
    JuliaLowering.include_string(test_mod, """
    function f_kw_slurp_some(; x=1, y=2, kws...)
        kws
    end
    """)
    @test values(test_mod.f_kw_slurp_some(z=3, x = 1, y = 2, w=4)) === (z=3, w=4)
    @test values(test_mod.f_kw_slurp_some(x = 1)) === (;)
    @test values(test_mod.f_kw_slurp_some()) === (;)

    # Keyword defaults which depend on other keywords.
    JuliaLowering.include_string(test_mod, """
    begin
        aaa = :outer
        function f_kw_default_dependencies(; x=1, y=x, bbb=aaa, aaa=:aaa_kw, ccc=aaa)
            (x, y, bbb, aaa, ccc)
        end
    end
    """)
    @test values(test_mod.f_kw_default_dependencies()) === (1, 1, :outer, :aaa_kw, :aaa_kw)
    @test values(test_mod.f_kw_default_dependencies(x = 10)) === (10, 10, :outer, :aaa_kw, :aaa_kw)
    @test values(test_mod.f_kw_default_dependencies(x = 10, aaa=:blah)) === (10, 10, :outer, :blah, :blah)

    # Keywords with static parameters
    JuliaLowering.include_string(test_mod, """
    function f_kw_sparams(x::X, y::Y; a::A, b::B) where {X,Y,A,B}
        (X,Y,A,B)
    end
    """)
    @test values(test_mod.f_kw_sparams(1, 1.0; a="a", b='b')) === (Int, Float64, String, Char)

    # Keywords with static parameters, where some keyword types can be inferred
    # based on the positional parameters and others cannot.
    JuliaLowering.include_string(test_mod, """
    function f_kw_type_errors(x::X; a::F, b::X) where {X<:Integer,F<:AbstractFloat}
        (X,F)
    end
    """)
    @test values(test_mod.f_kw_type_errors(1; a=1.0, b=10)) === (Int, Float64)
    # The following is a keyword TypeError because we can infer `X` based on
    # the positional parameters and use that to check the type of `b`.
    @test_throws TypeError values(test_mod.f_kw_type_errors(1; a=1.0, b="str"))
    # The following is only a method error as we can't infer `F` prior to
    # dispatching to the body function.
    @test_throws MethodError values(test_mod.f_kw_type_errors(1; a="str", b=10))

    # Throwing of UndefKeywordError
    JuliaLowering.include_string(test_mod, """
    function f_kw_no_default(; x)
        x
    end
    """)
    @test test_mod.f_kw_no_default(x = 10) == 10
    @test_throws UndefKeywordError(:x) test_mod.f_kw_no_default() == 10

    # Closure with keywords
    cl = JuliaLowering.include_string(test_mod, """
    let y = 1
        function f_kw_closure(; x=10)
            x + y
        end
    end
    """)
    @test cl() == 11
    @test cl(x = 20) == 21
end

@testset "Broadcast" begin
    @test JuliaLowering.include_string(test_mod, """
    let x = [1,2], y = [3,4], z = [5,6]
        x .* y .+ z
    end
    """) == [8, 14]

    @test JuliaLowering.include_string(test_mod, """
    let nums = [1, 2, 3]
        string.(nums, base=2; pad=2)
    end
    """) == ["01", "10", "11"]

    @test JuliaLowering.include_string(test_mod, """
    let lhs = [0,0], x = [1,2], y = [3,4], z = [5,6]
        lhs .= x .* y .+ z
        lhs
    end
    """) == [8, 14]

    @test JuliaLowering.include_string(test_mod, """
    [1,2] .+ ([3,4] .< [5,6] .< [7,1])
    """) == [2, 2]

    @test JuliaLowering.include_string(test_mod, """
    let
        x = [0,0,0,0]
        x[begin+1:end-1] .= [1,2] .+ [3,4]
        x
    end
    """) == [0,4,6,0]
end

end
