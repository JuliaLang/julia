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

# Nested splatting
@test JuliaLowering.include_string(test_mod, """
let
    xs = [[1, 2], [3, 4]]
    tuple((xs...)...)
end
""") == (1, 2, 3, 4)

@test JuliaLowering.include_string(test_mod, """
let
    xs = [[1, 2]]
    ys = [[3, 4]]
    tuple((xs...)..., (ys...)...)
end
""") == (1, 2, 3, 4)

# Multiple (>2) nested splat
@test JuliaLowering.include_string(test_mod, """
let
    xs = [[[1, 2]]]
    tuple(((xs...)...)...)
end
""") == (1, 2)
@test JuliaLowering.include_string(test_mod, """
let
    xs = [[[1, 2]]]
    ys = [[[3, 4]]]
    tuple(((xs...)...)..., ((ys...)...)...)
end
""") == (1, 2, 3, 4)
@test JuliaLowering.include_string(test_mod, """
let
    xs = [[[1, 2]]]
    ys = [[[3, 4]]]
    tuple(((xs...)...)..., ((ys...)...))
end
""") == (1, 2, [3, 4])

# Trailing comma case should still work (different semantics)
@test JuliaLowering.include_string(test_mod, """
let
    xs = [[1, 2], [3, 4]]
    tuple((xs...,)...)
end
""") == ([1, 2], [3, 4])

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
# Arrow syntax
@test JuliaLowering.include_string(test_mod, """
let
    f = ((x::T, y::T) where T) -> x + y
    f(1, 2)
end
""") === 3

@test JuliaLowering.include_string(test_mod, """
let
    f = ((x::T; y=2) where T) -> x + y
    f(1)
end
""") === 3

# Passes desugaring, but T is detected as unused and throws an error.
# Is it clear whether this should be `f(x::T) where T` or `f(x::T where T)`?
@test JuliaLowering.include_string(test_mod, """
let
    f = ((x::T) where T) -> x
    f(1)
end
""") === 1

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

# Static parameter may be undefined
@test JuliaLowering.include_string(test_mod, """
begin
    func_undef_static_param(x::Union{T,Nothing}) where T = @isdefined(T)
    (func_undef_static_param(nothing), func_undef_static_param(42))
end
""") === (false, true)

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
# or anywhere
@test JuliaLowering.include_string(test_mod, """
let f = function foo(y::X1{T})::X1{T} where T
        y
    end
    f(X1{Int}())
end
""") == test_mod.X1{Int}()
@test JuliaLowering.include_string(test_mod, """
let f = function foo(y::X1{<:T})::X1{<:T} where T
        y
    end
    f(X1{Int}())
end
""") == test_mod.X1{Int}()

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

@test JuliaLowering.include_string(test_mod, """
x = 0
function f_return_in_value_pos()
    global x
    x = return 42
end

(f_return_in_value_pos(), x)
""") === (42, 0)

@test JuliaLowering.include_string(test_mod, """
function f_return_in_call()
    f_return_in_call(return 123)
end

f_return_in_call()
""") === 123

@test JuliaLowering.include_string(test_mod, raw"""
function f_return_in_interpolation()
    :(1 + $(return 123))
end

f_return_in_interpolation()
""") === 123

@test JuliaLowering.include_string(test_mod, raw"""
function f_return_in_pparam_default(x, y=(return x), z=10)
    (x, y, z)
end
(f_return_in_pparam_default(1),
 f_return_in_pparam_default(1,2),
 f_return_in_pparam_default(1,2,3))
""") === (1, (1,2,10), (1,2,3))

@test JuliaLowering.include_string(test_mod, raw"""
function f_return_in_pparam_default2(x, y=(return x), z=(return y); kw=1)
    (x, y, z, kw)
end
(f_return_in_pparam_default2(1),
 f_return_in_pparam_default2(1,2),
 f_return_in_pparam_default2(1,2,3),
 f_return_in_pparam_default2(1;kw=0),
 f_return_in_pparam_default2(1,2;kw=0),
 f_return_in_pparam_default2(1,2,3;kw=0))
""") === (1, 2, (1,2,3,1), 1, 2, (1,2,3,0))

@testset "Optional positional arguments" begin
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_def_simple(x=1, y=2, z=x)
            (x,y,z)
        end

        (f_def_simple(), f_def_simple(10), f_def_simple(10,20), f_def_simple(10,20,30))
    end
    """) == ((1,2,1), (10,2,10), (10,20,10), (10,20,30))

    # anon forms
    @test JL.include_string(test_mod, "((x=1,y=2,z=3,va...)->(x,y,z,va))()") == (1,2,3,())
    @test JL.include_string(test_mod, "((x=1,y=2,z=3,va...)->(x,y,z,va))(0)") == (0,2,3,())
    @test JL.include_string(test_mod, "((x=1,y=2,z=3,va...)->(x,y,z,va))(0,0)") == (0,0,3,())
    @test JL.include_string(test_mod, "((x=1,y=2,z=3,va...)->(x,y,z,va))(0,0,0)") == (0,0,0,())
    @test JL.include_string(test_mod, "((x=1,y=2,z=3,va...)->(x,y,z,va))(0,0,0,0)") == (0,0,0,(0,))
    @test JL.include_string(test_mod, "(function (x=1,y=2,z=3,va...); (x,y,z,va); end)()") == (1,2,3,())
    @test JL.include_string(test_mod, "(function (x=1,y=2,z=3,va...); (x,y,z,va); end)(0)") == (0,2,3,())
    @test JL.include_string(test_mod, "(function (x=1,y=2,z=3,va...); (x,y,z,va); end)(0,0)") == (0,0,3,())
    @test JL.include_string(test_mod, "(function (x=1,y=2,z=3,va...); (x,y,z,va); end)(0,0,0)") == (0,0,0,())
    @test JL.include_string(test_mod, "(function (x=1,y=2,z=3,va...); (x,y,z,va); end)(0,0,0,0)") == (0,0,0,(0,))

    # defaults containing previous args
    @test JL.include_string(test_mod, "((x=1,y=x,z=x,va...=x)->(x,y,z,va))()") == (1,1,1,(1,))
    @test JL.include_string(test_mod, "((x=1,y=x,z=x,va...=x)->(x,y,z,va))(2)") == (2,2,2,(2,))
    @test JL.include_string(test_mod, "((x=1,y=x,z=y+x,va...=z+y+x)->(x,y,z,va))()") == (1,1,2,(4,))
    @test JL.include_string(test_mod, "((x=1,y=x,z=y+x,va...=z+y+x)->(x,y,z,va))(2)") == (2,2,4,(8,))
    # defaults shadowed by later args (resolution should not pick the arg)
    @test JL.include_string(test_mod, "let x = 1; ((x=x)->(x,))(); end") == (1,)
    @test JL.include_string(test_mod, "let x = 1; ((x=x)->(x,))(0); end") == (0,)
    @test JL.include_string(test_mod, "let x = 1; ((x...=x)->(x,))(); end") == ((1,),)
    @test JL.include_string(test_mod, "let x = 1; ((x...=x)->(x,))(0); end") == ((0,),)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y=y)->(x,y))(); end") == (2,2,)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y=y)->(x,y))(0); end") == (0,2,)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y=y)->(x,y))(0,0); end") == (0,0,)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y...=y)->(x,y))(); end") == (2,(2,),)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y...=y)->(x,y))(0); end") == (0,(2,),)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y...=y)->(x,y))(0,0); end") == (0,(0,),)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y=y,z=y)->(x,y,z))(); end") == (2,2,2)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y=y,z=y)->(x,y,z))(0); end") == (0,2,2)
    @test JL.include_string(test_mod, "let y = 2; ((x=y,y=y,z=y)->(x,y,z))(0,0); end") == (0,0,0)
    # defaults containing previous sparams
    @test JL.include_string(test_mod, "(((x::T=1,y=T) where T)->(x,y,T))()") == (1, Int, Int)
    @test JL.include_string(test_mod, "(((x::T=1,y=T) where T)->(x,y,T))(true)") == (true, Bool, Bool)
    @test JL.include_string(test_mod, "(((x::Type{T}=Vector{Int},y=T) where T)->(x,y,T))()") ==
        (Vector{Int}, Vector{Int}, Vector{Int})
    @test JL.include_string(test_mod, "(((x::Type{T}=Vector{Int},y=T) where T)->(x,y,T))(Bool)") ==
        (Bool, Bool, Bool)
    # https://github.com/JuliaLang/JuliaLowering.jl/issues/158
    @test JL.include_string(
        test_mod, "(((::Type{T}=Vector{UInt8}, sz=Base.aligned_sizeof(eltype(T))) where T)->sz)()") ==
            1
    @test JL.include_string(
        test_mod, "(((::Type{T}=Vector{UInt8}, sz=Base.aligned_sizeof(eltype(T))) where T)->sz)(Int32)") ==
            4

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
        function f_def_typevar_vararg_undef(x::T, y::Vararg{S}) where {T,S}
            (x, y, @isdefined S)
        end

        (f_def_typevar_vararg_undef(1), f_def_typevar_vararg_undef(1,2), f_def_typevar_vararg_undef(1,2,3))
    end
    """) === ((1, (), false), (1, (2,), true), (1, (2, 3), true))

    @test JuliaLowering.include_string(test_mod, """
    begin
        f_def_typevar_with_lowerbound(x::T) where {T>:Int} =
            (x, @isdefined(T))
        (f_def_typevar_with_lowerbound(1), f_def_typevar_with_lowerbound(1.0))
    end
    """) == ((1, true), (1.0, false))

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

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_optarg_complex_spbounds(p::T, o=1) where {T<:Complex{<:Real}}
            T, p, o
        end
        f_optarg_complex_spbounds(Complex(1)), f_optarg_complex_spbounds(Complex(2), Complex(3))
    end
    """) == ((Complex{Int},Complex(1),1), (Complex{Int},Complex(2),Complex(3)))

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_optarg_complex_spbounds2(p::T, o::T=Complex(0)) where {T<:Complex{<:Real}}
            T, p, o
        end
        f_optarg_complex_spbounds2(Complex(1)), f_optarg_complex_spbounds2(Complex(2))
    end
    """) == ((Complex{Int},Complex(1),Complex(0)), (Complex{Int},Complex(2),Complex(0)))

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_optarg_complex_spbounds_rett(p::T, o=1)::T where {T<:Complex{<:Real}}
            p
        end
        f_optarg_complex_spbounds_rett(Complex(1)), f_optarg_complex_spbounds_rett(Complex(2))
    end
    """) == (Complex(1), Complex(2))

    # flisp will evaluate the sparam bound multiple times
    let res = JuliaLowering.include_string(test_mod, """
        let eval_spbounds_counter = 0
            global function f_optarg_eval_spbounds_counter(
                    p::T, o=1,_=2,_=3) where {
                        T<:Complex{<:(eval_spbounds_counter += 1; Real)}}
                (p, eval_spbounds_counter)
            end
            f_optarg_eval_spbounds_counter(Complex(1))
        end
        """)
        @test res == (Complex(1), 1)
    end
end

@testset "slotflags" begin
    JuliaLowering.include_string(test_mod, """
    function f_slotflags(x, y, f, z)
        f() + x + y
    end
    """)
    @test only(methods(test_mod.f_slotflags)).called == 0b0100
end

@testset "nospecialize" begin
    # note f(a,b,c) means a is arg 1, not f
    function test_arg_unspecialized(f::Function, arg_i::Int)
        for m in methods(f)
            arg_i > m.nargs-1 && return nothing
            @test m.nospecialize & (1 << (arg_i-1)) != 0
        end
    end
    function test_arg_specialized(f::Function, arg_i::Int)
        for m in methods(f)
            arg_i > m.nargs-1 && return nothing
            @test m.nospecialize & (1 << (arg_i-1)) == 0
        end
    end

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

    # Branching combined with nospecialize meta in CodeInfo
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_branch_meta(@nospecialize(x), cond)
            if cond
                x + 1
            else
                x + 2
            end
        end

        (f_branch_meta(10, false), f_branch_meta(20, true))
    end
    """) == (12, 21)

    # @nospecialize with multiple args in function body
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nospecialize_multi_body(a, b, c, d)
            @nospecialize a c d
            (a, b, c, d)
        end

        f_nospecialize_multi_body(1, 2, 3, 4)
    end
    """) == (1, 2, 3, 4)
    @test only(methods(test_mod.f_nospecialize_multi_body)).nospecialize == 0b1101

    # @nospecialize with single arg in function body
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nospecialize_single_body(a, b)
            @nospecialize b
            (a, b)
        end

        f_nospecialize_single_body(1, 2)
    end
    """) == (1, 2)
    @test only(methods(test_mod.f_nospecialize_single_body)).nospecialize == 0b10

    # @nospecialize with zero args in function body (blanket nospecialize)
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nospecialize_zero_body(a, b, c)
            @nospecialize
            (a, b, c)
        end

        f_nospecialize_zero_body(1, 2, 3)
    end
    """) == (1, 2, 3)
    # 0-arg @nospecialize sets all bits (-1 == typemax(Int32) for nospecialize)
    @test only(methods(test_mod.f_nospecialize_zero_body)).nospecialize == -1

    # @nospecialize with default value in signature
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_nospecialize_default(x, @nospecialize(y=1))
            (x, y)
        end

        (f_nospecialize_default(10, 20), f_nospecialize_default(30))
    end
    """) == ((10, 20), (30, 1))
    # The 2-arg method has nospecialize on y (bit 2), the 1-arg forwarding method has no y
    ms = collect(methods(test_mod.f_nospecialize_default))
    @test any(m -> m.nargs == 3 && m.nospecialize == 0b10, ms)
    @test any(m -> m.nargs == 2 && m.nospecialize == 0b00, ms)

    # Body-level @nospecialize with default value in signature
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_body_nospecialize_default(x, y=1)
            @nospecialize
            (x, y)
        end
        (f_body_nospecialize_default(10, 20), f_body_nospecialize_default(30))
    end
    """) == ((10, 20), (30, 1))
    # The 2-arg method has nospecialize on y (bit 2), the 1-arg forwarding method has no y
    ms = collect(methods(test_mod.f_body_nospecialize_default))
    @test count(m -> m.nargs == 3 && m.nospecialize == -1, ms) == 1
    @test count(m -> m.nargs == 2 && m.nospecialize == -1, ms) == 1

    # body nospecialize into complex sig: all
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_body_nospecialize_nontrivial_sig(x::T, y::Vector{<:U}=[])::Any where T where U
            @nospecialize
            (x, y)
        end
        (f_body_nospecialize_nontrivial_sig(10, [20]), f_body_nospecialize_nontrivial_sig(30))
    end
    """) == ((10, [20]), (30, []))
    test_arg_unspecialized(test_mod.f_body_nospecialize_nontrivial_sig, 1)
    test_arg_unspecialized(test_mod.f_body_nospecialize_nontrivial_sig, 2)
    # should be blanket-nospecialized
    ms = collect(methods(test_mod.f_body_nospecialize_nontrivial_sig))
    @test count(m -> m.nargs == 3 && m.nospecialize == -1, ms) == 1
    @test count(m -> m.nargs == 2 && m.nospecialize == -1, ms) == 1

    # body nospecialize into complex sig: by name
    @test JuliaLowering.include_string(test_mod, """
    begin
        function f_body_nospecialize_nontrivial_sig2(x::T, y::Vector{<:U}=[])::Any where T where U
            @nospecialize x
            (x, y)
        end
        (f_body_nospecialize_nontrivial_sig2(10, [20]), f_body_nospecialize_nontrivial_sig2(30))
    end
    """) == ((10, [20]), (30, []))
    test_arg_unspecialized(test_mod.f_body_nospecialize_nontrivial_sig2, 1)
    test_arg_specialized(test_mod.f_body_nospecialize_nontrivial_sig2, 2)

    # callable type: should compile, but nospecialize doesn't do anything
    @test JuliaLowering.include_string(test_mod, """
    struct nospecialize_callable_type; field; end
    (@nospecialize(x::nospecialize_callable_type))() = (x.field,)
    nospecialize_callable_type(0)()
    """) == (0,)
    @test JuliaLowering.include_string(test_mod, """
    (@nospecialize(::nospecialize_callable_type))(x::Int) = (x,)
    nospecialize_callable_type(0)(1)
    """) == (1,)
    @test JuliaLowering.include_string(test_mod, """
    (@nospecialize(_::nospecialize_callable_type))(x::Int, y::Int) = (x,y)
    nospecialize_callable_type(0)(1,2)
    """) == (1,2)
    @test JuliaLowering.include_string(test_mod, """
    function (self::nospecialize_callable_type)(x::Int, y::Int, z::Int)
        @nospecialize self
        (self.field,x,y,z)
    end
    nospecialize_callable_type(0)(1,2,3)
    """) == (0,1,2,3)
    @test JuliaLowering.include_string(test_mod, """
    (@nospecialize((;field)::nospecialize_callable_type))(x::Int, y::Int, z::Int, a::Int) = (field,x,y,z,a)
    nospecialize_callable_type(0)(1,2,3,4)
    """) == (0,1,2,3,4)
    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    (@nospecialize(x)::nospecialize_callable_type)() = 1
    """)

    # function name: should compile, but nospecialize doesn't do anything
    @test_broken JuliaLowering.include_string(test_mod, """
    (@nospecialize(_))(x::Int) = ()
    func_nospecialize_self(1)
    """) == ()
    @test JuliaLowering.include_string(test_mod, """
    (@nospecialize(func_nospecialize_self))(x::Int) = (x,)
    func_nospecialize_self(1)
    """) == (1,)

    # all positional arg forms
    @testset for arg0 in [:x, :(x::Type), :(::Type), :(_), :(_::Type)],
        arg1 in [arg0, Expr(:..., arg0)],
        arg2 in [arg1, Expr(:kw, arg1, :Int)],
        expander in [fl_macroexpand, jl_macroexpand]

        @testset let expanded = expander(
            test_mod, :(function (specialized, @nospecialize($arg2))
                            specialized
                        end))
            f = jl_eval(test_mod, expanded)
            test_arg_specialized(f, 1)
            test_arg_unspecialized(f, 2)
        end

        @testset let expanded = expander(
            test_mod, :(function ($arg2,)
                            @nospecialize
                        end))
            f = jl_eval(test_mod, expanded)
            test_arg_unspecialized(f, 1)
        end
    end

    # nospecialize should still compile where flisp drops it
    @test jl_eval(
        test_mod,
        :(let bad(@nospecialize(x) = 1) = x
              (bad(0), bad())
          end)) == (0, 1)
    @test jl_eval(
        test_mod,
        :(let bad(@nospecialize(x::Int) = 1) = x
              (bad(0), bad())
          end)) == (0, 1)

    @testset "kwargs" for expander in [fl_macroexpand, (_,x)->x]
        local f
        @test (f = jl_eval(test_mod, expander(
            test_mod, quote
                function (@nospecialize(a); kw=1)
                    (a, kw)
                end
            end))) isa Function
        Core.@latestworld
        @test f(1, kw=2) == (1,2) && f(3) == (3,1)
        test_arg_unspecialized(f, 1)
        @test only(methods(Core.kwcall, (NamedTuple,typeof(f),Any))).nospecialize == 1 << 2

        # Body-level @nospecialize
        @test (f = jl_eval(test_mod, expander(
            test_mod, quote
                function (a; kw=1)
                    @nospecialize a
                    (a, kw)
                end
            end))) isa Function
        Core.@latestworld
        @test f(1, kw=2) == (1,2) && f(3) == (3,1)
        test_arg_unspecialized(f, 1)
        @test only(methods(Core.kwcall, (NamedTuple,typeof(f),Any))).nospecialize == 1 << 2

        # kw nospecialize.  TODO: The body method is local; how do we get it out
        # for testing?
        @test (f = jl_eval(test_mod, expander(
            test_mod, quote
                function (a; @nospecialize(kw=1))
                    (a, kw)
                end
            end))) isa Function
        Core.@latestworld
        @test f(1, kw=2) == (1,2) && f(3) == (3,1)
        test_arg_specialized(f, 1)

        # kw... nospecialize (same TODO)
        @test (f = jl_eval(test_mod, expander(
            test_mod, quote
                function (a; @nospecialize(kw...))
                    (a, kw...)
                end
            end))) isa Function
        Core.@latestworld
        @test f(1, kw=2, a=3) == (1,:kw=>2,:a=>3)
        test_arg_specialized(f, 1)
    end

    # macros already mark all non-internal args nospecialize
    @testset "macro definitions" begin
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(x)); end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(x::Int)); end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(x=1)); end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(x::Int=1)); end)) isa Function

        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(x); @nospecialize(); end)) isa Function

        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(x); @nospecialize(x); x; end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(x::Int); @nospecialize(x); x; end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(x=1); @nospecialize(x); x; end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(x::Int=1); @nospecialize(x); x; end)) isa Function

        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(_)); end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(_::Int)); end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(_=1)); end)) isa Function
        @gensym sym
        @test jl_eval(test_mod, :(macro $sym(@nospecialize(_::Int=1)); end)) isa Function
    end
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

    @testset "anonymous forms" begin
        f = JL.include_string(test_mod, "function (;kw); kw; end")
        @test f(;kw=1) == 1
        @test_throws UndefKeywordError f(;)
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "function (;kw::Int); kw; end")
        @test f(;kw=1) == 1
        @test_throws TypeError f(;kw=1.1)
        @test_throws UndefKeywordError f(;)
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "function (;kw=2); kw; end")
        @test f(;kw=1) == 1
        @test f(;) == 2
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "function (;kw::Int=2); kw; end")
        @test f(;kw=1) == 1
        @test f(;) == 2
        @test_throws TypeError f(;kw=1.1)
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "function (;kw...); kw; end")
        @test f(;kw=1) isa Base.Pairs
        @test (f(;kw=1))[:kw] == 1
        exkw = (;k3=3)
        @test f(;k1=1, k2=2, exkw...) isa Base.Pairs
        @test (f(;k1=1, k2=2, exkw...))[:k1] == 1
        @test (f(;k1=1, k2=2, exkw...))[:k2] == 2
        @test (f(;k1=1, k2=2, exkw...))[:k3] == 3
        # ->
        f = JL.include_string(test_mod, "(;kw)->kw")
        @test f(;kw=1) == 1
        @test_throws UndefKeywordError f(;)
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "(;kw::Int)->kw")
        @test f(;kw=1) == 1
        @test_throws TypeError f(;kw=1.1)
        @test_throws UndefKeywordError f(;)
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "(;kw=2)->kw")
        @test f(;kw=1) == 1
        @test f(;) == 2
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "(;kw::Int=2)->kw")
        @test f(;kw=1) == 1
        @test f(;) == 2
        @test_throws TypeError f(;kw=1.1)
        @test_throws MethodError f(1)
        f = JL.include_string(test_mod, "(;kw...)->kw")
        @test f(;kw=1) isa Base.Pairs
        @test (f(;kw=1))[:kw] == 1
        exkw = (;k3=3)
        @test f(;k1=1, k2=2, exkw...) isa Base.Pairs
        @test (f(;k1=1, k2=2, exkw...))[:k1] == 1
        @test (f(;k1=1, k2=2, exkw...))[:k2] == 2
        @test (f(;k1=1, k2=2, exkw...))[:k3] == 3
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

    # Slurping with defaults depending on keyword names
    JuliaLowering.include_string(test_mod, """
    function f_kw_slurp_dep(; a=1, b=a, kws...)
        (a, b, length(kws))
    end
    """)
    @test test_mod.f_kw_slurp_dep(; a=1) == (1, 1, 0)
    @test test_mod.f_kw_slurp_dep(; a=2, c=3) == (2, 2, 1)

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

    # depend on positional args
    @test JuliaLowering.include_string(test_mod, """
    function f_kw_pos_dependencies(p1, o1=1, va...; kw1=p1, kw2=o1, kw3=va)
        (p1, o1, va..., kw1, kw2, kw3...)
    end
    """) isa Function
    @test test_mod.f_kw_pos_dependencies('p', 'o', 'v', 'v') ==
        ('p', 'o', 'v', 'v', 'p', 'o', 'v', 'v')
    @test test_mod.f_kw_pos_dependencies('p', 'o', 'v') ==
        ('p', 'o', 'v', 'p', 'o', 'v')
    @test test_mod.f_kw_pos_dependencies('p', 'o') ==
        ('p', 'o', 'p', 'o')
    @test test_mod.f_kw_pos_dependencies('p') ==
        ('p', 1, 'p', 1)

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

    # Return type annotation using default argument names
    # The return type must be evaluated in a scope where keyword args are bound.
    JuliaLowering.include_string(test_mod, """
    function f_default_rett(T::Type=Int)::Vector{T}
        T[1,2,3]
    end
    """)
    @test test_mod.f_default_rett() isa Vector{Int}
    @test test_mod.f_default_rett(Float64) isa Vector{Float64}

    # Return type annotation using keyword argument names
    # The return type must be evaluated in a scope where keyword args are bound.
    JuliaLowering.include_string(test_mod, """
    function f_kw_rett(; T::Type=Int)::Vector{T}
        T[1,2,3]
    end
    """)
    @test test_mod.f_kw_rett() isa Vector{Int}
    @test test_mod.f_kw_rett(T=Float64) isa Vector{Float64}

    JuliaLowering.include_string(test_mod, """
    function f_kw_rett2(; T::Type=Int)::Union{Vector{<:T}, Vector{<:AbstractVector{<:T}}}
        false && return T[]
        T == Int ? T[1,2,3] : [T[1,2],T[3,4]]
    end
    """)
    @test test_mod.f_kw_rett2() isa Vector{Int}
    @test test_mod.f_kw_rett2() == Int[1,2,3]
    @test test_mod.f_kw_rett2(T=Float64) isa Vector{Vector{Float64}}
    @test test_mod.f_kw_rett2(T=Float64) == [Float64[1.0,2.0],Float64[3.0,4.0]]

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
    f = JuliaLowering.include_string(test_mod, """
    function f_kw_closure_outer(; x=1)
        function f_kw_closure(; y=2)
            (x, y)
        end
    end
    """)
    @test f() isa Function
    @test f()() == (1, 2)
    @test f()(y = 3) == (1, 3)
    @test f(x = 10) isa Function
    @test f(x = 10)(y = 10) == (10, 10)
    f = JuliaLowering.include_string(test_mod, """
    function f_kw_closure_capt_default(; x=1)
        function f_kw_closure(; y=x)
            (x, y)
        end
    end
    """)
    @test f() isa Function
    @test f()() == (1, 1)
    @test f(x=2)(y=3) == (2, 3)
    f = JuliaLowering.include_string(test_mod, """
    let outer_capt = 0
    function f_kw_closure_capt_default(; x=1)
        function f_kw_closure(; y=x)
            (outer_capt, x, y)
        end
    end
    end
    """)
    @test f() isa Function
    @test f()() == (0, 1, 1)
    @test f(x=2)(y=3) == (0, 2, 3)

    f = JuliaLowering.include_string(test_mod, """
    function f_kw_anon(outervar)
        (a,;kw=1)->a+kw+outervar
    end
    """)

    @test f(100) isa Function
    @test f(100)(2) == 103
    @test f(100)(2;kw=2) == 104

    @testset "complex arg types requiring temporaries" begin
        @test JL.include_string(
            test_mod, """
            let f = function (x::Vector{<:Number};kw=[2])
                        (x,kw)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == (([1], [2]), ([1], [0]))
        @test JL.include_string(
            test_mod, """
            let f = function (x::Vector{<:Number}, o1=10, o2=20;kw=[2])
                        (x,kw,o1,o2)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == (([1], [2], 10, 20), ([1], [0], 10, 20))
        @test JL.include_string(
            test_mod, """
            let f = function (x;kw::Vector{<:Number}=x)
                        (x,kw)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == (([1], [1]), ([1], [0]))
        @test JL.include_string(
            test_mod, """
            let f = function (x, o1=10, o2=20;kw::Vector{<:Number}=x)
                        (x,kw,o1,o2)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == (([1], [1], 10, 20), ([1], [0], 10, 20))
        @test JL.include_string(
            test_mod, """
            let f = function (o1::Vector{<:Number}=[10];kw=1)
                        (kw,o1)
                    end
                f(), f([1]), f(;kw=2), f([1]; kw=2)
            end
        """) == ((1, [10]), (1, [1]), (2, [10]), (2, [1]))
    end
    @testset "complex sparam bounds requiring temporaries" begin
        @test JL.include_string(
            test_mod, """
            let f = function (x::T;kw=[2]) where {T<:Vector{<:Number}}
                        (T,x,kw)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == ((Vector{Int}, [1], [2]), (Vector{Int}, [1], [0]))
        @test JL.include_string(
            test_mod, """
            let f = function (x::T, o1=10, o2=20;kw=[2]) where {T<:Vector{<:Number}}
                        (T,x,kw,o1,o2)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == ((Vector{Int}, [1], [2], 10, 20), (Vector{Int}, [1], [0], 10, 20))
        @test JL.include_string(
            test_mod, """
            let f = function (x;kw::T=x) where {T<:Vector{<:Number}}
                        (T,x,kw)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == ((Vector{Int}, [1], [1]), (Vector{Int}, [1], [0]))
        @test JL.include_string(
            test_mod, """
            let f = function (x, o1=10, o2=20;kw::T=x) where {T<:Vector{<:Number}}
                        (T,x,kw,o1,o2)
                    end
                f([1]), f([1], kw=[0])
            end
        """) == ((Vector{Int}, [1], [1], 10, 20), (Vector{Int}, [1], [0], 10, 20))
        @test JL.include_string(
            test_mod, """
            let f = function (o1::T=[10];kw=1) where {T<:Vector{<:Number}}
                        (T,kw,o1)
                    end
                f(), f([1]), f(;kw=2), f([1]; kw=2)
            end
        """) == ((Vector{Int}, 1, [10]), (Vector{Int}, 1, [1]),
                 (Vector{Int}, 2, [10]), (Vector{Int}, 2, [1]))
    end

    @testset "destructured args" begin
        @test JL.include_string(
            test_mod, "(function ((d1,d2);kw); [d1,d2,kw]; end)((1,2);kw=3)") == [1,2,3]
        # with kw default
        @test JL.include_string(
            test_mod, "(function ((d1,d2);kw=4); [d1,d2,kw]; end)((1,2);kw=3)") == [1,2,3]
        @test JL.include_string(
            test_mod, "(function ((d1,d2);kw=4); [d1,d2,kw]; end)((1,2))") == [1,2,4]
        # flisp doesn't do this either
        @test_broken JL.include_string(
            test_mod, "(function ((d1,d2);kw1=d1); [d1,d2,kw1]; end)((1,2))") == [1,2,1]
        # with kw deps
        @test JL.include_string(
            test_mod, "(function ((d1,d2);kw1=1,kw2=kw1); [d1,d2,kw1,kw2]; end)((1,2);kw1=9)") == [1,2,9,9]
        @test JL.include_string(
            test_mod, "(function ((d1,d2);kw1=1,kw2=kw1); [d1,d2,kw1,kw2]; end)((1,2);kw1=9,kw2=10)") == [1,2,9,10]
    end
end

# Brittle test, needs fixing if kw_body naming or kwarg implementation changes
@testset "(AI) kw function helper is declared in the correct module" begin
    # Extending another module's keyword function with a new keyword method must
    # reserve the hidden `#kw_body#...` global in the *extending* module (the
    # call/eval site), never in the extended function's home module -- reserving
    # it in a foreign (possibly precompiled/closed) module breaks incremental
    # compilation.  This is most easily broken when the method name arrives as an
    # interpolated `GlobalRef` *value* (the StatsBase/TracedSample shape), whose
    # `:mod` attribute would otherwise steer the reservation to the owner module.
    kwbodies(m) = filter(s -> occursin("kw_body", String(s)), names(m; all=true))

    # (a) interpolated `GlobalRef` value as the method name (the regressing case)
    OwnerA = Module()
    JL.include_string(OwnerA, "sample(x; y=1) = x + y")
    a_before = Set(kwbodies(OwnerA))
    ExtA = Module()
    @eval ExtA const OwnerA = $OwnerA
    JL.include_string(ExtA, """
        let fn = GlobalRef(OwnerA, :sample)
            @eval \$fn(x::Symbol; y=1) = y
        end
    """)
    @test isempty(setdiff(Set(kwbodies(OwnerA)), a_before))  # no new global in owner
    @test !isempty(kwbodies(ExtA))                           # reserved in extender
    @test OwnerA.sample(3; y=10) == 13                       # original method intact
    @test OwnerA.sample(:s; y=7) == 7                        # new method dispatches
    @test OwnerA.sample(:s) == 1                             # ...with its own default

    # (b) syntactic dotted name reaches the same conclusion (guards the common path)
    OwnerB = Module()
    JL.include_string(OwnerB, "sample(x; y=1) = x + y")
    b_before = Set(kwbodies(OwnerB))
    ExtB = Module()
    @eval ExtB const OwnerB = $OwnerB
    JL.include_string(ExtB, "function OwnerB.sample(x::Symbol; y=1); y; end")
    @test isempty(setdiff(Set(kwbodies(OwnerB)), b_before))
    @test !isempty(kwbodies(ExtB))
    @test OwnerB.sample(:s; y=7) == 7
end

@testset "pre-desugared arg::Vararg" begin
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_nosplat = function (x::Vararg{Int})
            x
        end
        f_vararg_nosplat(1,2,3)
    end
    """) == (1, 2, 3)

    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_nosplat = function ((a,b,c)::Vararg{Int})
            (a,b,c)
        end
        f_vararg_nosplat(1,2,3)
    end
    """) == (1, 2, 3)

    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_nosplat = function (((a,b)...,c)::Vararg{Int})
            (a,b,c)
        end
        f_vararg_nosplat(1,2,3)
    end
    """) == (1, 2, 3)

    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_nosplat = function (((a,b)...,c)::Vararg{Tuple{Vararg{Int}}})
            (a,b,c)
        end
        f_vararg_nosplat((1,2),(3,),(4,))
    end
    """) == ((1, 2), (3,), (4,))

    @testset "(AI) in keyword functions" begin
        # A trailing positional `Vararg{T,N}` written with an explicit `::Vararg`
        # annotation (rather than `...`) must still be splatted when the keyword
        # wrappers forward it to the body method.  The zero-keyword path (defaulting
        # sorter), the explicit-keyword path, and splatted keywords must all work,
        # and `N` may be referenced in the signature and body.
        JuliaLowering.include_string(test_mod, """
        function f_vararg_N_kws(y::Integer, args::Vararg{Integer,N}; kwargs...) where {N}
            (y, args, N, kwargs)
        end
        """)
        @test test_mod.f_vararg_N_kws(1, 2, 3) === (1, (2, 3), 2, Base.pairs(NamedTuple()))
        let r = test_mod.f_vararg_N_kws(1, 2, 3; foo=1)
            @test (r[1], r[2], r[3]) === (1, (2, 3), 2)
            @test r[4][:foo] == 1
        end
        let ekw = (a=1, b=2), r = test_mod.f_vararg_N_kws(1, 2; ekw...)
            @test (r[1], r[2], r[3]) === (1, (2,), 1)
            @test (r[4][:a], r[4][:b]) == (1, 2)
        end

        # `N` used as a keyword default (exercises the sorter/body kw forwarding too).
        JuliaLowering.include_string(test_mod, """
        function f_vararg_N_kwdefault(y::Integer, args::Vararg{Integer,N}; scale::Int=N) where {N}
            (y, args, scale)
        end
        """)
        @test test_mod.f_vararg_N_kwdefault(1, 2, 3) === (1, (2, 3), 2)
        @test test_mod.f_vararg_N_kwdefault(1, 2, 3; scale=10) === (1, (2, 3), 10)

        # `Vararg{T}` with no count, and a bare `Vararg`, plus `Vararg` on an
        # anonymous (unnamed) positional argument.
        JuliaLowering.include_string(test_mod, """
        function f_vararg_T_kws(y, args::Vararg{Integer}; kwargs...)
            (y, args, kwargs)
        end
        """)
        @test test_mod.f_vararg_T_kws(1, 2, 3) === (1, (2, 3), Base.pairs(NamedTuple()))
        @test test_mod.f_vararg_T_kws(1, 2, 3; foo=1)[3][:foo] == 1

        JuliaLowering.include_string(test_mod, """
        function f_vararg_bare_kws(y, args::Vararg; kwargs...)
            (y, args, kwargs)
        end
        """)
        @test test_mod.f_vararg_bare_kws(1, 2, 3) === (1, (2, 3), Base.pairs(NamedTuple()))
        @test test_mod.f_vararg_bare_kws(1; z=9)[3][:z] == 9

        JuliaLowering.include_string(test_mod, """
        function f_vararg_anon_kws(y, ::Vararg{Integer,N}; kwargs...) where {N}
            (y, N, kwargs)
        end
        """)
        @test test_mod.f_vararg_anon_kws(1, 2, 3) === (1, 2, Base.pairs(NamedTuple()))
        @test test_mod.f_vararg_anon_kws(1, 2, 3; k=1)[3][:k] == 1

        # Equivalent `args::T...` and plain `args...` forms with kwargs (already
        # handled, covered here for parity).
        JuliaLowering.include_string(test_mod, """
        function f_vararg_dots_typed_kws(y, args::Integer...; kwargs...)
            (y, args, kwargs)
        end
        """)
        @test test_mod.f_vararg_dots_typed_kws(1, 2, 3) === (1, (2, 3), Base.pairs(NamedTuple()))
        @test test_mod.f_vararg_dots_typed_kws(1, 2, 3; foo=1)[3][:foo] == 1

        JuliaLowering.include_string(test_mod, """
        function f_vararg_dots_kws(y, args...; kwargs...)
            (y, args, kwargs)
        end
        """)
        @test test_mod.f_vararg_dots_kws(1, 2, 3) === (1, (2, 3), Base.pairs(NamedTuple()))
        @test test_mod.f_vararg_dots_kws(1, 2, 3; foo=1)[3][:foo] == 1

        # Vararg-annotated positional args carrying a default value (`K"kw"`-wrapped
        # in the AST), both named and anonymous.
        JuliaLowering.include_string(test_mod, """
        function f_vararg_default_kws(y, args::Vararg{Int,N}=1; k=1) where {N}
            (y, args, N, k)
        end
        """)
        @test test_mod.f_vararg_default_kws(1, 2, 3) === (1, (2, 3), 2, 1)
        @test test_mod.f_vararg_default_kws(1, 2, 3; k=9) === (1, (2, 3), 2, 9)

        JuliaLowering.include_string(test_mod, """
        function f_vararg_anon_default_kws(y, ::Vararg{Int,N}=1; k=1) where {N}
            (y, N, k)
        end
        """)
        @test test_mod.f_vararg_anon_default_kws(1, 2, 3) === (1, 2, 1)
        @test test_mod.f_vararg_anon_default_kws(1, 2, 3; k=9) === (1, 2, 9)

        # Callable-type method with a trailing Vararg and keywords (shape from
        # SerializedElementArrays.jl).
        JuliaLowering.include_string(test_mod, """
        struct VKS{T,N}
            dims::NTuple{N,Int}
        end
        function (A::Type{VKS{<:Any,N}})(::UndefInitializer, dims::Vararg{Integer,N}; kw=1) where {N}
            (N, dims, kw)
        end
        """)
        @test test_mod.VKS{<:Any,2}(undef, 3, 4) === (2, (3, 4), 1)
        @test test_mod.VKS{<:Any,1}(undef, 7; kw=9) === (1, (7,), 9)
    end
end

@testset "all known valid positional argument forms" begin
    make_defaults(x) = let (ps, vals) = x
        # (p1,p2,p3) => (v1,v2,v3) to
        # ((kw p1 v1),(kw p2 v2),(kw p3 v3)) => (v1,v2,v3)
        map(zip(ps, vals)) do pv
            Expr(:kw, pv[1], pv[2])
        end => vals
    end
    make_typed(pv) = let (ps, vals) = pv
        new_ps = map(ps) do p
            # types go under `...`
            if Meta.isexpr(p, :...)
                Expr(:..., Expr(:(::), p.args[1], Any))
            else
                Expr(:(::), p, Any)
            end
        end
        new_ps => vals
    end

    pparams_req = let
        # tuple of params => tuple of acceptable values
        pparams_untyped = [
            # x,y,z must be defined for testing
            (:x,
             :y,
             :z) =>
                 (1,2,3),
            (:x,
             Expr(:tuple, :y, :z)) =>
                 (1,(2,3)),
            (:x,
             Expr(:tuple, Expr(:parameters, :y, :z))) =>
                 (1,(;y=2,z=3)),
            (:x,
             Expr(:tuple, Expr(:..., :y), :z)) =>
                (1,(2,3,4)),
            (Expr(:tuple, Expr(:tuple, :x, :y), :z),) =>
                (((1,2),3),),
            (Expr(:tuple, Expr(:..., Expr(:tuple, :x, :y)), :z),) =>
                ((1,2,3),),
            (Expr(:tuple, Expr(:..., Expr(:tuple, :x, :y)), :z),) =>
                ((1,2,3,4,5),),
            (:x,
             :y,
             Expr(:..., :z)) =>
                 (1,2,3),
        ]
        pparams_typed = map(make_typed, pparams_untyped)
        vcat(pparams_untyped, pparams_typed)
    end

    @testset "required args" for (params_i, args_i) in pparams_req
        @testset let f_expr = Expr(:function,
                                   Expr(:call, gensym(), params_i...),
                                   Expr(:tuple, :x, :y, :z)),
                f_st = JuliaLowering.expr_to_est(f_expr)

            local func_ref, func_test
            @test ((func_ref = fl_eval(test_mod, f_expr)) isa Function)
            @test ((func_test = jl_eval(test_mod, f_st)) isa Function)
            Core.@latestworld
            @test func_ref(args_i...) == func_test(args_i...)
        end
    end

    pparams_default = map(make_defaults, pparams_req)

    @testset "default args" for (params_i, args_i) in pparams_default
        @testset let f_expr = Expr(:function,
                                   Expr(:call, gensym(), params_i...),
                                   Expr(:tuple, :x, :y, :z)),
                    f_st = JuliaLowering.expr_to_est(f_expr)

            local func_ref, func_test
            @test ((func_ref = fl_eval(test_mod, f_expr)) isa Function)
            @test ((func_test = jl_eval(test_mod, f_st)) isa Function)
            Core.@latestworld
            @test func_ref(args_i...) == func_test(args_i...)
            @test func_ref() == func_test()
        end
    end

    # test vararg-tuples and splatted defaults separately, as providing defaults
    # must be done with a syntactic splat, and some variants are valid syntax
    # but not callable (may later be disallowed)
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_tuple = function ((x,y,z)...)
            (x,y,z)
        end
        f_vararg_tuple(1,2,3), f_vararg_tuple(1,2,3,4,5)
    end
    """) === ((1,2,3), (1,2,3))
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_tuple = function ((x,y,z)...=(1,2,3)...)
            (x,y,z)
        end
        f_vararg_tuple(4,5,6,7), f_vararg_tuple()
    end
    """) === ((4,5,6), (1,2,3))
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_tuple = function ((x,(y,z))...=(1,(2,3))...)
            (x,y,z)
        end
        f_vararg_tuple(4,(5,6),7), f_vararg_tuple()
    end
    """) === ((4,5,6), (1,2,3))
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_tuple = function ((x,(y,z)...)...=(1,(2,3)...)...)
            (x,y,z)
        end
        f_vararg_tuple(4,5,6,7), f_vararg_tuple()
    end
    """) === ((4,5,6), (1,2,3))

    # uncallable(?)
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_tuple = function ((x,y,z)::Tuple...)
            (x,y,z)
        end
    end
    """) isa Function
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_tuple = function ((;x,y,z)...)
            (x,y,z)
        end
    end
    """) isa Function
    @test JuliaLowering.include_string(test_mod, """
    let
        f_vararg_tuple = function ((;x,y,z)::NamedTuple...)
            (x,y,z)
        end
    end
    """) isa Function

    # final default arg may always be splatted, even if no-op or followed by va
    @test JuliaLowering.include_string(test_mod, """
    let
        f = function (x=1...)
            x
        end
        f(), f(2), try; f(9,9); catch e; "fail"; end
    end
    """) === (1, 2, "fail")
    @test JuliaLowering.include_string(test_mod, """
    let
        f = function (x=1..., args...)
            x, args
        end
        f(), f(2), f(3,4,5)
    end
    """) === ((1, ()),
              (2, ()),
              (3, (4,5)))
end

@testset "first-arg destructuring" begin
    @eval test_mod struct XY; x; y; end
    Core.@latestworld
    xy = test_mod.XY(1,2)
    @test JL.include_string(test_mod, "((;x,y)::XY)(arg) = (x,y,arg)") === nothing
    Core.@latestworld
    @test xy(3) == (1,2,3)

    @eval test_mod begin
        struct XYVec; val::Core.SimpleVector; end
        Base.iterate(x::XYVec) = Base.iterate(x.val)
        Base.iterate(x::XYVec, i) = Base.iterate(x.val, i)
    end
    xy = test_mod.XYVec(Core.svec(1,2,999))
    @test JL.include_string(test_mod, "((x,y)::XYVec)(arg) = (x,y,arg)") === nothing
    Core.@latestworld
    @test xy(3) == (1,2,3)

    # `...`
    xy = test_mod.XYVec(Core.svec(1,2,9,9,9))
    @test JL.include_string(test_mod, "((x,y,rest...)::XYVec)() = (x,y,rest...,)") === nothing
    Core.@latestworld
    @test xy() == (1,2,9,9,9)

    xy = test_mod.XYVec(Core.svec(1,9,9,9,2))
    @test JL.include_string(test_mod, "((x,rest...,y)::XYVec)(a1,a2) = (x,rest...,y,a1,a2)") === nothing
    Core.@latestworld
    @test xy(0,0) == (1,9,9,9,2,0,0)
end

@testset "sparam in keyword default" begin
    # The keyword default is evaluated in the body method, which carries all
    # of the function's static parameters
    @test JL.include_string(test_mod,
        "f_kwdef_sp(y::T; k=T) where T = (y, k); f_kwdef_sp(1)") == (1, Int)
    # ... but an sparam unused in the signature is undetermined at dispatch
    JL.include_string(test_mod, "f_kwdef_sp_undet(y; k=T) where T = (y, k)")
    @test_throws UndefVarError test_mod.f_kwdef_sp_undet(1)
end

@testset "anonymous static parameters" begin
    # `where _` declares a static parameter which can never be referenced
    @test JL.include_string(test_mod, "f_anon_sp(x) where _ = x; f_anon_sp(42)") == 42
    @test JL.include_string(
        test_mod, "f_anon_sp2(x::T) where {T, _} = (x, T); f_anon_sp2(1.5)") == (1.5, Float64)
    @test_throws LoweringError JL.include_string(
        test_mod, "f_anon_sp3(x) where {_, _} = x"; expr_compat_mode=true)
    # Currently allowed (like arguments).  Could error like flisp.
    @test_throws LoweringError JL.include_string(
        test_mod, "f_anon_sp3(x) where {_, _} = x") broken=true
end

@testset "first arg `where`" begin
    @eval test_mod struct A12238{T} end
    Core.@latestworld
    @test JL.include_string(test_mod, "(A12238{T} where T<:Real)(x) = 0") === nothing
    @test test_mod.A12238{<:Real}(0) == 0
    @test_throws MethodError test_mod.A12238{<:Integer}(0)

    # Nested where
    @eval test_mod struct A12238_2{T, U}; x::T; y::U; end
    Core.@latestworld
    @test JL.include_string(
        test_mod,
        "(A12238_2{T, U} where T<:U where U<:Real)(x) = A12238_2(x,x)") === nothing
    @test (test_mod.A12238_2{T, U} where {U<:Real, T<:U})(0) ===
        test_mod.A12238_2{Int, Int}(0, 0)

    # Implicit whereparams
    @eval test_mod struct A12238_3{T, U}; x::T; y::U; end
    Core.@latestworld
    @test JL.include_string(
        test_mod,
        "(A12238_3{<:Real, <:AbstractVector{<:Real}})() = A12238_3(1,Int[1])") === nothing
    @test (test_mod.A12238_3{<:Real, <:AbstractVector{<:Real}})() isa
        test_mod.A12238_3{Int, Vector{Int}}
    @test (test_mod.A12238_3{<:Real, <:AbstractVector{<:Real}})().x == 1
    @test (test_mod.A12238_3{<:Real, <:AbstractVector{<:Real}})().y == [1]

    # >:
    @eval test_mod struct A12238_4{T} end
    Core.@latestworld
    @test JL.include_string(
        test_mod,
        "(A12238_4{T} where T>:Int)(x) = x") === nothing
    @test test_mod.A12238_4{>:Int}(1) == 1
    @test_throws MethodError test_mod.A12238_4{<:Int}(1)
end

@testset "Write-only placeholder function arguments" begin
    # positional arguments may be duplicate placeholders.  keyword arguments can
    # contain placeholders, but they must be unique
    params_req = [""
                  "_"
                  "::Int"
                  "_, _"
                  "(_, _)"]
    params_opt = [""
                  "::Int=2"
                  "_=2"]
    params_va  = ["", "_..."]
    params_kw  = [""
                  "; _"
                  "; _::Int"
                  "; _::Int=1"
                  "; _=1, __=2"
                  "; _..."
                  "; _=1, __..."]
    for req in params_req, opt in params_opt, va in params_va, kw in params_kw
        arg_str = join(filter(!isempty, (req, opt, va, kw)), ", ")
        f_str = "function ($arg_str); end"
        @testset "$f_str" begin
            @test JuliaLowering.include_string(test_mod, f_str) isa Function
        end
        f_lam_str = "($arg_str)->nothing"
        @testset "$f_lam_str" begin
            @test JuliaLowering.include_string(test_mod, f_lam_str) isa Function
        end
    end
end

@testset "Badly-parsed anonymous forms (fix_arglist)" begin
    @test JL.include_string(test_mod, "(()->nothing)()") == nothing
    @test JL.include_string(test_mod, "((a...)->(a...,))(1,2,3)") == (1,2,3)
    @test JL.include_string(test_mod, "((a::Int)->(a))(1)") == 1
    @test JL.include_string(test_mod, "((a::Int...)->(a...,))(1,2,3)") == (1,2,3)
    @test JL.include_string(test_mod, "((;)->nothing)()") == nothing
    @test JL.include_string(test_mod, "((a;)->a)(1)") == 1
    @test JL.include_string(test_mod, "((a;b=2)->(a,b))(1)") == (1,2)
    @test JL.include_string(test_mod, "((a;b=2)->(a,b))(1;b=3)") == (1,3)
    @test JL.include_string(test_mod, "((a=0;b=2)->(a,b))()") == (0,2)
    @test JL.include_string(test_mod, "((a=0;b=2)->(a,b))(1)") == (1,2)
    @test JL.include_string(test_mod, "((a=0;b=2)->(a,b))(;b=3)") == (0,3)
    @test JL.include_string(test_mod, "((a=0;b=2)->(a,b))(1;b=3)") == (1,3)
    @test_throws LoweringError JL.include_string(test_mod, "(a=0;b=2;c=3)->nothing")

    # try again with `where`
    @test JL.include_string(test_mod, "(((a::T...)      where T<:U where U<:Any) ->(a...,))(1,2,3)") == (1,2,3)
    @test JL.include_string(test_mod, "(((a::T;)        where T<:U where U<:Any) ->a)(1)") == 1
    @test JL.include_string(test_mod, "(((a::T;b=2)     where T<:U where U<:Any) ->(a,b))(1)") == (1,2)
    @test JL.include_string(test_mod, "(((a::T;b=2)     where T<:U where U<:Any) ->(a,b))(1;b=3)") == (1,3)
    @test JL.include_string(test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) ->(a,b))()") == (0,2)
    @test JL.include_string(test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) ->(a,b))(1)") == (1,2)
    @test JL.include_string(test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) ->(a,b))(;b=3)") == (0,3)
    @test JL.include_string(test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) ->(a,b))(1;b=3)") == (1,3)
    @test_throws LoweringError JL.include_string(test_mod, "(a=0;b=2;c=3)->nothing")

    # `...` is the only parser-reachable bad form with (function notcall _) forms
    @test JL.include_string(test_mod, "(function (a...); (a...,); end)(1,2,3)") == (1,2,3)
    @test JL.include_string(test_mod, "(function (a::Int...); (a...,); end)(1,2,3)") == (1,2,3)
    # test with where: need empty tv list to avoid unused sparam warning
    @test jl_eval(test_mod,
                  Expr(:call,
                       Expr(:function, Expr(:where, Expr(:where, Expr(:..., :a))),
                            Expr(:block, Expr(:tuple, Expr(:..., :a)))),
                       1,2,3)) == (1,2,3)
    @test JL.include_string(test_mod, "(function (a::T) where T<:U where U<:Any; a; end)(1)") == 1
    @test JL.include_string(test_mod, "(function (a::T...) where T<:U where U<:Any; a; end)(1,2,3)") == (1,2,3)
end

@testset "Consequences of accepting badly-parsed anonymous forms" begin
    # kw
    @test jl_eval(test_mod,
                  Expr(:call,
                       Expr(:function, Expr(:kw, :a, 1),
                            Expr(:block, Expr(:tuple, :a))),
                       )) == (1,)

    # empty block
    @test jl_eval(test_mod,
                  Expr(:call,
                       Expr(:function, Expr(:block),
                            Expr(:block, Expr(:tuple))),
                       )) == ()

    # unwrapped or block-wrapped arg
    @testset for a1 in [:a, Expr(:(::), :a, :Int)],
        a2 in [a1, Expr(:(=), :a, 0), Expr(:kw, :a, 0)],
        wrap_where in [identity, x->Expr(:where, x), x->Expr(:where, Expr(:where, x))]

        @test jl_eval(test_mod,
                      Expr(:call,
                           Expr(:function, wrap_where(a2),
                                Expr(:block, Expr(:tuple, :a))),
                           1)) == (1,)
        @test jl_eval(test_mod,
                      Expr(:call,
                           Expr(:function, wrap_where(Expr(:block, a2)),
                                Expr(:block, Expr(:tuple, :a))),
                           1)) == (1,)
    end

    # two-arg block
    @test jl_eval(test_mod,
                  Expr(:call,
                       Expr(:function, Expr(:block, :a, :b),
                            Expr(:block, Expr(:tuple, :a, :b))),
                       1, Expr(:kw, :b, 2))) == (1,2)
end

@testset "assignment to where-wrapped-tuple" begin
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a,b,c::T)     where T<:U where U<:Any) = (a,b,c))(1,2,3)") == (1,2,3)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a,b=0,c::T=0) where T<:U where U<:Any) = (a,b,c))(1)") == (1,0,0)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a,b=0,c::T=0) where T<:U where U<:Any) = (a,b,c))(1,2)") == (1,2,0)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a,b=0,c::T=0) where T<:U where U<:Any) = (a,b,c))(1,2,3)") == (1,2,3)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T...)      where T<:U where U<:Any) = (a...,))(1,2,3)") == (1,2,3)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T;)        where T<:U where U<:Any) = a)(1)") == 1
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T;b=2)     where T<:U where U<:Any) = (a,b))(1)") == (1,2)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T;b=2)     where T<:U where U<:Any) = (a,b))(1;b=3)") == (1,3)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) = (a,b))()") == (0,2)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) = (a,b))(1)") == (1,2)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) = (a,b))(;b=3)") == (0,3)
    @test_throws LoweringError JL.include_string(
        test_mod, "(((a::T=0;b=2)   where T<:U where U<:Any) = (a,b))(1;b=3)") == (1,3)
    @test_throws LoweringError JL.include_string(
        test_mod, "(a=0;b=2;c=3) where T = nothing")
    @test_throws LoweringError jl_eval(
        test_mod,
        Expr(:call,
             Expr(:(=), Expr(:where, Expr(:where, Expr(:..., :a))),
                  Expr(:block, Expr(:tuple, Expr(:..., :a)))),
             1,2,3)) == (1,2,3)
end

@testset "Assigned-to arguments" begin
    # These examples are all macros, since they have specialized de-optimization
    # behavior that sends un-optimized code straight to codegen. Normal compiled
    # functions essentially always pass through SSA conversion on the way to the
    # optimizer, erasing these slots (potentially hiding bugs in slot handling)

    @test JuliaLowering.include_string(test_mod, raw"""
    macro m_assigned_args_1(x)
        x = x + 1
        return x
    end
    var"@m_assigned_args_1"(LineNumberNode(0, nothing), Main, 2)
    """; expr_compat_mode=true) == 3

    @test JuliaLowering.include_string(test_mod, raw"""
    macro m_assigned_args_2(x, y = 1)
        (y, x) = (x + 1, y + 1)
        return y - x
    end
    (
        var"@m_assigned_args_2"(LineNumberNode(0, nothing), Main, 2),
        var"@m_assigned_args_2"(LineNumberNode(0, nothing), Main, 1, 2),
    )
    """; expr_compat_mode=true) == (1, -1)

    for expr_compat_mode in (false, true)
        @test JuliaLowering.include_string(test_mod, raw"""
        macro m_assigned_args(ex)
            ex = Base.remove_linenums!(ex)
            return ex
        end
        ((@m_assigned_args 1 + 1), @m_assigned_args 1)
        """; expr_compat_mode) == (2, 1)
    end
end

@testset "Generated functions" begin; for expr_compat_mode in (false, true)
    local genfunc_s, genfunc_f
    @eval test_mod import JuliaLowering.@legacy_quote_to_syntax

    @testset "returning special syntax forms" begin
        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated f_gen_nothing() = nothing
            f_gen_nothing()
        end
        """; expr_compat_mode) == nothing

        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated f_gen_quotenothing() = :(nothing)
            f_gen_quotenothing()
        end
        """; expr_compat_mode) == nothing

        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated f_gen_quotenodenothing() = QuoteNode(nothing)
            f_gen_quotenodenothing()
        end
        """; expr_compat_mode) == nothing

        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated f_gen_quotenodeexpr() = QuoteNode(Expr(:begin, nothing))
            f_gen_quotenodeexpr()
        end
        """; expr_compat_mode) == Expr(:begin, nothing)

        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated f_gen_gr_nothing() = GlobalRef(Core, :nothing)
            f_gen_gr_nothing()
        end
        """; expr_compat_mode) == nothing

        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated f_gen_quotegr_nothing() = :(GlobalRef(Core, :nothing))
            f_gen_quotegr_nothing()
        end
        """; expr_compat_mode) == GlobalRef(Core, :nothing)

        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated f_gen_quotenodegr_nothing() = QuoteNode(GlobalRef(Core, :nothing))
            f_gen_quotenodegr_nothing()
        end
        """; expr_compat_mode) == GlobalRef(Core, :nothing)
    end

    @test JuliaLowering.include_string(test_mod, raw"""
    begin
        @generated function f_gen_trivial(x)
            x
        end

        f_gen_trivial(1), f_gen_trivial(Int[1])
    end
    """; expr_compat_mode) == (Int, Vector{Int})

    @test JuliaLowering.include_string(test_mod, raw"""
    begin
        function f_gen_trivial_if(x)
            if @generated
                x
            else
                :($x, "nongen")
            end
        end

        f_gen_trivial_if(1), f_gen_trivial_if(Int[1])
    end
    """; expr_compat_mode) == (Int, Vector{Int})

    @testset "anonymous forms" begin
        @test JuliaLowering.include_string(test_mod, """
        let
            f = @generated function (x); x; end
            f(1), f(Int[1])
        end
        """; expr_compat_mode) == (Int, Vector{Int})
        @test JuliaLowering.include_string(test_mod, """
        let
            f = (x)->(if @generated(); x; else; "nongen"; end)
            f(1), f(Int[1])
        end
        """; expr_compat_mode) == (Int, Vector{Int})
    end

    @testset "destructured args" begin
        genfunc_s = raw"""
        function ((d1,d2)::T) where {T}
            if @generated
                :($T, "gen")
            else
                :($T, "nongen")
            end
        end
        """
        @test (genfunc_f = JL.include_string(test_mod, genfunc_s; expr_compat_mode)) isa Function
        @test genfunc_f((1,2)) == (Tuple{Int, Int}, "gen")
    end

    @testset "destructured args: values" begin
        genfunc_s = raw"""
        function ((d1,d2)::T) where {T}
            if @generated
                :(d1, d2, $T, "gen")
            else
                :($T, "nongen")
            end
        end
        """
        @test (genfunc_f = JL.include_string(test_mod, genfunc_s; expr_compat_mode)) isa Function
        @test genfunc_f((1,2)) == (1, 2, Tuple{Int, Int}, "gen")
    end

    @testset "(AI) destructured args: shapes" begin
        # A destructured-tuple argument in a fully-`@generated` function whose
        # body is a `quote`/`Expr(:block)` (not a bare single expression): the
        # implicit `(names...) = <arg>` prologue must reach the generated code.
        @test JL.include_string(test_mod, raw"""
            @generated function fds_named(x, (a, b)); quote a + b end; end
            fds_named(1, (2, 3))
        """; expr_compat_mode) == 5
        # Same, with the generated body built as an explicit `Expr(:block, ...)`.
        @test JL.include_string(test_mod, raw"""
            @generated function fds_exprblock(x, (a, b)); Expr(:block, :(a + b)); end
            fds_exprblock(1, (2, 3))
        """; expr_compat_mode) == 5
        # Nested destructuring.
        @test JL.include_string(test_mod, raw"""
            @generated function fds_nested(x, (a, (b, c))); quote a + b + c end; end
            fds_nested(1, (2, (3, 4)))
        """; expr_compat_mode) == 9
        # Destructured first argument.
        @test JL.include_string(test_mod, raw"""
            @generated function fds_first((a, b)); quote a + b end; end
            fds_first((2, 3))
        """; expr_compat_mode) == 5
        # Positional vararg after a destructured argument.
        @test JL.include_string(test_mod, raw"""
            @generated function fds_va((a, b), xs...); quote a + b + length(xs) end; end
            fds_va((2, 3), 10, 20)
        """; expr_compat_mode) == 7
        # Destructured argument alongside keyword arguments.
        @test JL.include_string(test_mod, raw"""
            @generated function fds_kw((a, b); k=0); quote a + b + k end; end
            fds_kw((2, 3); k=10)
        """; expr_compat_mode) == 15
        # Multiple destructured args
        @test JL.include_string(test_mod, raw"""
            @generated function fds_multi((a, b), (c, d)); quote a + b + c + d end; end
            fds_multi((1, 2), (3, 4))
        """; expr_compat_mode) == 10
    end

    @testset "keyword args" begin
        genfunc_f = JL.include_string(test_mod, raw"""
        function (parg::Tuple{T}; kw) where {T}
            if @generated
                :($parg, $T, $kw, "gen")
            else
                :($parg, $T, $kw, "nongen")
            end
        end
        """; expr_compat_mode)

        @test genfunc_f((1,); kw=1) ==
                (Tuple{Int}, Int, Int, "gen")
        @test_throws UndefKeywordError genfunc_f((1,))

        genfunc_f = JL.include_string(test_mod, raw"""
        function (parg::Tuple{T}; kw::Vector{T}) where {T}
            if @generated
                :($parg, $T, $kw, "gen")
            else
                :($parg, $T, $kw, "nongen")
            end
        end
        """; expr_compat_mode)

        @test genfunc_f((1,); kw=[1]) == (Tuple{Int}, Int, Vector{Int}, "gen")
        @test_throws UndefKeywordError genfunc_f((1,))
        @test_throws TypeError genfunc_f((1,); kw=1)

        genfunc_f = JL.include_string(test_mod, raw"""
        function (; kw::T, rkw...) where {T}
            if @generated
                :($T, $kw, $rkw, "gen")
            else
                :($T, $kw, $rkw, "nongen")
            end
        end
        """; expr_compat_mode)

        @test genfunc_f(; kw=1) ==
            (Int, Int, Base.Pairs{Symbol, Union{}, Nothing, @NamedTuple{}}, "gen")
        @test genfunc_f(; kw=1, kw2=2) ==
            (Int, Int, Base.Pairs{Symbol, Int, Nothing, @NamedTuple{kw2::Int}}, "gen")
        @test_throws UndefKeywordError genfunc_f()
    end

    @test JuliaLowering.include_string(test_mod, raw"""
    begin
        @generated function f_gen(x::NTuple{N,T}) where {N,T}
            quote
                ($x, $N, $T)
            end
        end

        f_gen((1,2,3,4,5))
    end
    """; expr_compat_mode) == (NTuple{5,Int}, 5, Int)

    @test JuliaLowering.include_string(test_mod, """
    begin
        @generated function f_gen_unnamed_args(::Type{T}, y, ::Type{U}) where {T, U}
            return (T, y, U)
        end

        f_gen_unnamed_args(Int, UInt8(3), Float64)
    end
    """; expr_compat_mode) == (Int, UInt8, Float64)

    @test JuliaLowering.include_string(test_mod, raw"""
    begin
        function f_partially_gen(x::NTuple{N,T}) where {N,T}
            shared = :shared_stuff
            if @generated
                if N == 2
                    error("intentionally broken codegen (will trigger nongen branch)")
                end
                quote
                    unshared = (:gen, ($x, $N, $T))
                end
            else
                unshared = (:nongen, (typeof(x), N, T))
            end
            (shared, unshared)
        end

        (f_partially_gen((1,2)), f_partially_gen((1,2,3,4,5)))
    end
    """; expr_compat_mode) ==
        ((:shared_stuff, (:nongen, (NTuple{2,Int}, 2, Int))),
         (:shared_stuff, (:gen, (NTuple{5,Int}, 5, Int))))

    @test JuliaLowering.include_string(test_mod, raw"""
    begin
        @generated function f_gen_calls_macros(x::T) where {T}
            s = @raw_str "foo"
            :(@raw_str $s)
        end
        f_gen_calls_macros(1)
    end
    """; expr_compat_mode) === "foo"
    @test JuliaLowering.include_string(test_mod, raw"""begin
        @generated function calls_versioned_macro(::Type{T}, ::Val{i}) where {T, i}
            i isa Integer || @goto err
            return i
            @label err
            return 0
        end

        calls_versioned_macro(Tuple{Int}, Val(1))
    end """; expr_compat_mode) == 1

    @testset "(AI) anonymous args promoted by optional/keyword args" begin
        # A `@generated` method with >=2 anonymous args (`::T` or `_`) whose
        # placeholder slots get promoted to `#arg#` identifiers because the
        # method also has an optional positional or keyword arg used to fail at
        # first call with "function argument name not unique".

        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated function g_anon_opt(x,
                                           ::Val{A}=Val(false),
                                           ::Val{B}=Val(false)) where {A,B}
                :( (x, A, B) )
            end
            g_anon_opt(1)
        end
        """; expr_compat_mode) === (1, false, false)

        # calling the same function at two different type instantiations
        @test JuliaLowering.include_string(test_mod, raw"""
            (g_anon_opt(1, Val(:a), Val(:b)), g_anon_opt(2.0, Val(3)))
        """; expr_compat_mode) === ((1, :a, :b), (2.0, 3, false))

        # 2 anonymous required args forced by an unrelated keyword arg
        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated function g_anon_kw(::Val{A}, ::Val{B}; kw=1) where {A,B}
                :( (A, B, kw) )
            end
            g_anon_kw(Val(1), Val(2))
        end
        """; expr_compat_mode) === (1, 2, 1)
        @test JuliaLowering.include_string(test_mod,
            "g_anon_kw(Val(1), Val(2); kw=5)"; expr_compat_mode) === (1, 2, 5)

        # underscore args (also anonymous), forced by a default
        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated function g_anon_underscore(_, _, z=10)
                :( z )
            end
            g_anon_underscore(:a, :b)
        end
        """; expr_compat_mode) === 10

        # named + anonymous mix, with the body reading the named arg while the
        # generator also uses the where-params
        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated function g_named_anon(a, ::Val{A}, ::Val{B}=Val(0); k=7) where {A,B}
                :( (a, A, B, k) )
            end
            g_named_anon("hi", Val(1))
        end
        """; expr_compat_mode) === ("hi", 1, 0, 7)

        # single anonymous arg (no collision possible) still works with a kwarg
        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated function g_one_anon(x, ::Val{A}; k=3) where {A}
                :( (x, A, k) )
            end
            g_one_anon(1, Val(2))
        end
        """; expr_compat_mode) === (1, 2, 3)

        # Pathological: a user arg literally named `#arg#` (the promotion name)
        # must remain a real, body-referenceable slot -- the discriminator is a
        # metadata tag on promoted anonymous args, not a name match.
        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated function g_user_hasharg(var"#arg#", ::Val{A}=Val(0)) where {A}
                :( (var"#arg#", A) )
            end
            g_user_hasharg(5)
        end
        """; expr_compat_mode) === (5, 0)
    end

    @testset "hygiene in generated functions" begin
        # (AI) A generator whose returned body is a bare macrocall to an
        # old-style macro that re-wraps `esc`'d fragments in a freshly-built,
        # unescaped `Expr` (e.g.  `Base.Cartesian.@nif`), referencing the
        # generated function's own arguments and static parameters. Those
        # escaped references unwind to the generator's base layer, so the
        # synthesized argument/sparam names of the staged method must live in
        # that same layer -- otherwise they resolve as bogus module globals
        # (`UndefVarError`).
        @test JuliaLowering.include_string(test_mod, raw"""
        begin
            @generated function find_first_eq(x, itr::I) where {
                    N, I <: Tuple{Vararg{Any, N}}
                }
                return :(Base.Cartesian.@nif $(N + 1) d -> (x == getfield(itr, d)) d -> (d) d -> (nothing))
            end
            (find_first_eq(20, (10, 20, 30)), find_first_eq(99, (10, 20, 30)))
        end"""; expr_compat_mode) === (2, nothing)
        @test JuliaLowering.include_string(test_mod, raw"""begin
            @generated function nif_uses_sparam(x, ::Type{T}) where {T}
                return :(Base.Cartesian.@nif 2 d -> (x isa T) d -> (T) d -> (nothing))
            end
            nif_uses_sparam(1, Int)
        end"""; expr_compat_mode) === Int
    end
end

    genfunc_quote_s = """
    begin
        function f_gen_quote_1(::Tuple{T}) where {T}
            out = @legacy_quote_to_syntax :(:x1,first)
            if @generated
            else
            end
            return out
        end

        f_gen_quote_1((1,))
    end
    """
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=true) == :(:x1,first)
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=false) ≈
            @ast_ [K"tuple" [K"inert" "x1"::K"Identifier"] "first"::K"Identifier"]

    genfunc_quote_s = """
    begin
        function f_gen_quote_2(::Tuple{T}) where {T}
            out = nothing
            if @generated
                @legacy_quote_to_syntax :(out = @legacy_quote_to_syntax :(:x2,generated))
            else
                out = (:x2,nongen)
            end
            return out
        end

        f_gen_quote_2((1,))
    end
    """
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=true) == :(:x2,generated)
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=false) ≈
            @ast_ [K"tuple" [K"inert" "x2"::K"Identifier"] "generated"::K"Identifier"]

    genfunc_quote_s = """
    begin
        function f_gen_quote_3(::Tuple{T}) where {T}
            if @generated
            else
            end
            return @legacy_quote_to_syntax :(:x4,after)
        end

        f_gen_quote_3((1,))
    end
    """
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=true) == :(:x4,after)
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=false) ≈
            @ast_ [K"tuple" [K"inert" "x4"::K"Identifier"] "after"::K"Identifier"]

    genfunc_quote_s = raw"""
    begin
        function f_gen_interpolate(::Tuple{T}) where {T}
            out = :(:x1,first)
            if @generated
                out = @legacy_quote_to_syntax :($out, generated)
            else
                out = @legacy_quote_to_syntax :($out, nongen)
            end
            return out
        end

        f_gen_interpolate((1,))
    end
    """
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=true) == :((:x1,first),nongen)
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=false) ≈
            @ast_ [K"tuple" [K"tuple"
                             [K"inert" "x1"::K"Identifier"]
                             "first"::K"Identifier"]
                   "nongen"::K"Identifier"]

    genfunc_quote_s = raw"""
    begin
        @eval function f_gen_eval_quote_1(::Tuple{T}) where {T}
            out = $(Expr(:quote, Expr(:call, :+, 1, Expr(:if, Expr(:generated), 1, 2))))
            if @generated
            else
            end
            return out
        end
        f_gen_eval_quote_1((1,))
    end
    """
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=true) ==
            :(1 + $(Expr(:if, Expr(:generated), 1, 2)))
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=false) ==
            :(1 + $(Expr(:if, Expr(:generated), 1, 2)))

    # Test generated function edges to bindings
    # (see also https://github.com/JuliaLang/julia/pull/57230)
    JuliaLowering.include_string(test_mod, raw"""
    const delete_me = 4
    @generated f_generated_return_delete_me() = return quote; delete_me; end
    """)
    @test test_mod.f_generated_return_delete_me() == 4
    Base.delete_binding(test_mod, :delete_me)
    @test_throws UndefVarError test_mod.f_generated_return_delete_me()
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

@testset "method table overlays" begin
    OverlayModule = Module()

    @eval OverlayModule Base.Experimental.@MethodTable mt
    @test_broken JL.include_string(OverlayModule, """
        Base.Experimental.@overlay mt function sin(x::Float64); 1; end
    """) isa Method
    @test_broken JL.include_string(OverlayModule, """
        Base.Experimental.@overlay mt cos(x::Float64) = 2
    """) isa Method
    @test_broken JL.include_string(OverlayModule, """
        Base.Experimental.@overlay mt tan(x::T) where {T} = 3
    """) isa Method

    let ms = Base._methods_by_ftype(
        Tuple{typeof(sin), Float64}, nothing, 1, Base.get_world_counter())
        @test only(ms).method.module === Base.Math
    end
    let ms = Base._methods_by_ftype(
        Tuple{typeof(sin), Float64}, OverlayModule.mt, 1, Base.get_world_counter())
        @test only(ms).method.module === OverlayModule
    end
    let ms = Base._methods_by_ftype(
        Tuple{typeof(sin), Int}, OverlayModule.mt, 1, Base.get_world_counter())
        @test isempty(ms)
    end

end
