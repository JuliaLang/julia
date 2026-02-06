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

    # @nospecialize on unnamed arguments (issue #44428)
    JuliaLowering.include_string(test_mod, """
    function f_nospecialize_unnamed(@nospecialize(::Any), @nospecialize(x::Any))
        x
    end
    """)
    @test only(methods(test_mod.f_nospecialize_unnamed)).nospecialize == 0b11

    JuliaLowering.include_string(test_mod, """
    function f_slotflags(x, y, f, z)
        f() + x + y
    end
    """)
    @test only(methods(test_mod.f_slotflags)).called == 0b0100

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
            @test ((func_ref = Core.eval(test_mod, reference_lower(test_mod, f_expr))) isa Function)
            @test ((func_test = JuliaLowering.eval(test_mod, f_st)) isa Function)
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
            @test ((func_ref = Core.eval(test_mod, reference_lower(test_mod, f_expr))) isa Function)
            @test ((func_test = JuliaLowering.eval(test_mod, f_st)) isa Function)
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
end

    genfunc_quote_s = """
    begin
        function f_gen_quote_1(::Tuple{T}) where {T}
            out = :(:x1,first)
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
                :(out = :(:x2,generated))
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
            return :(:x4,after)
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
                out = :($out, generated)
            else
                out = :($out, nongen)
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
        @eval function f_gen_quote_1(::Tuple{T}) where {T}
            out = $(Expr(:quote, Expr(:call, :+, 1, Expr(:if, Expr(:generated), 1, 2))))
            if @generated
            else
            end
            return out
        end
        f_gen_quote_1((1,))
    end
    """
    @test JuliaLowering.include_string(
        test_mod, genfunc_quote_s; expr_compat_mode=true) ==
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

end
