@testset "macro tests" begin

test_mod = Module(:macro_test)
Base.eval(test_mod, :(const var"@ast" = $(JuliaLowering.var"@ast")))
Base.eval(test_mod, :(const var"@K_str" = $(JuliaLowering.var"@K_str")))

# These libraries may either be packages or vendored into Base - need to pull
# them in via relative paths in the `using` statements below.
Base.eval(test_mod, :(const JuliaLowering = $(JuliaLowering)))
Base.eval(test_mod, :(const JuliaSyntax = $(JuliaSyntax)))

JuliaLowering.include_string(test_mod, raw"""
module M
    using ..JuliaLowering: JuliaLowering, adopt_scope
    using ..JuliaSyntax

    # Introspection
    macro __MODULE__()
        __context__.scope_layer.mod
    end

    macro __FILE__()
        JuliaLowering.filename(__context__.macrocall)
    end

    macro __LINE__()
        JuliaLowering.source_location(__context__.macrocall)[1]
    end

    someglobal = "global in module M"

    # Macro with local variables
    macro foo(ex)
        :(begin
            x = "`x` from @foo"
            (x, someglobal, $ex)
        end)
    end

    # Set `a_global` in M
    macro set_a_global(val)
        :(begin
            global a_global = $val
        end)
    end

    macro set_other_global(ex, val)
        :(begin
            global $ex = $val
        end)
    end

    macro set_global_in_parent(ex)
        sym_ex = quote; sym_introduced_from_M; end
        e1 = adopt_scope(sym_ex[1], __context__)
        quote
            $e1 = $ex
            nothing
        end
    end

    macro inner()
        :(y, z)
    end

    macro outer()
        :((x, @inner))
    end

    macro recursive(N)
        Nval = N.value::Int
        if Nval < 1
            return N
        end
        quote
            x = $N
            (x, @recursive $(Nval-1))
        end
    end
end
""")

@test JuliaLowering.include_string(test_mod, """
let
    x = "`x` from outer scope"
    M.@foo x
end
""") == ("`x` from @foo", "global in module M", "`x` from outer scope")
@test !isdefined(test_mod.M, :x)


@test JuliaLowering.include_string(test_mod, """
#line1
(M.@__MODULE__(), M.@__FILE__(), M.@__LINE__())
""", "foo.jl") == (test_mod, "foo.jl", 2)

@test !isdefined(test_mod.M, :a_global)
@test JuliaLowering.include_string(test_mod, """
begin
    M.@set_a_global 42
    M.a_global
end
""") == 42

JuliaLowering.include_string(test_mod, """
M.@set_global_in_parent "bent hygiene!"
""")
@test test_mod.sym_introduced_from_M == "bent hygiene!"

JuliaLowering.include_string(test_mod, "M.@set_other_global global_in_test_mod 100")
@test !isdefined(test_mod.M, :global_in_test_mod)
@test test_mod.global_in_test_mod == 100

@test JuliaLowering.include_string(test_mod, """
M.@recursive 3
""") == (3, (2, (1, 0)))

ex = JuliaLowering.parsestmt(JuliaLowering.SyntaxTree, "M.@outer()", filename="foo.jl")
ctx, expanded = JuliaLowering.expand_forms_1(test_mod, ex, false, Base.get_world_counter())
@test JuliaLowering.sourcetext.(JuliaLowering.flattened_provenance(expanded[2])) == [
    "M.@outer()"
    "@inner"
    "(y, z)"
]

@test JuliaLowering.include_string(test_mod, raw"""
v"1.14"
""") isa VersionNumber
@test JuliaLowering.include_string(test_mod, raw"""
v"1.14"
""";expr_compat_mode=true) isa VersionNumber
@test JuliaLowering.include_string(test_mod, raw"""
Base.Experimental.@VERSION
""") isa NamedTuple
@test JuliaLowering.include_string(test_mod, raw"""
Base.Experimental.@VERSION
""";expr_compat_mode=true) isa NamedTuple

# World age support for macro expansion
JuliaLowering.include_string(test_mod, raw"""
macro world_age_test()
    1
end
""")
world1 = Base.get_world_counter()
JuliaLowering.include_string(test_mod, raw"""
macro world_age_test()
    2
end
""")
world2 = Base.get_world_counter()

call_world_arg_test = JuliaLowering.parsestmt(JuliaLowering.SyntaxTree, "@world_age_test()")
    @test JuliaLowering.expand_forms_1(test_mod, call_world_arg_test, false, world1)[2] ≈
        @ast_ 1::K"Value"
    @test JuliaLowering.expand_forms_1(test_mod, call_world_arg_test, false, world2)[2] ≈
        @ast_ 2::K"Value"

# Layer parenting
@test expanded[1].scope_layer == 2
@test expanded[2][1].scope_layer == 3
@test getfield.(ctx.scope_layers, :parent_layer) == [0,1,2]

JuliaLowering.include_string(test_mod, """
f_throw(x) = throw(x)
macro m_throw(x)
    :(\$(f_throw(x)))
end
""")
let (err, st) = try
        JuliaLowering.include_string(test_mod, "_never_exist = @m_throw 42")
    catch e
        e, stacktrace(catch_backtrace())
    end
    @test err isa JuliaLowering.MacroExpansionError
    @test !isnothing(err.err)
    # Check that `catch_backtrace` can capture the stacktrace of the macro functions
    @test any(sf->sf.func===:f_throw, st)
    @test any(sf->sf.func===Symbol("@m_throw"), st)
end

let err = try
        JuliaLowering.include_string(test_mod, "_never_exist = @m_not_exist 42")
    catch e
        e
    end
    @test err isa JuliaLowering.MacroExpansionError
    @test err.msg == "Macro not found"
    @test err.err isa UndefVarError
end

@test JuliaLowering.include_string(test_mod, "@ccall strlen(\"foo\"::Cstring)::Csize_t") == 3
@test JuliaLowering.include_string(test_mod, "@ccall gc_safe=true strlen(\"asdf\"::Cstring)::Csize_t") == 4
@test JuliaLowering.include_string(test_mod, """
begin
    buf = zeros(UInt8, 20)
    @ccall sprintf(buf::Ptr{UInt8}, "num:%d str:%s"::Cstring; 42::Cint, "hello"::Cstring)::Cint
    String(buf)
end
""") == "num:42 str:hello\0\0\0\0"

let (err, st) = try
        JuliaLowering.include_string(test_mod, "@ccall strlen(\"foo\"::Cstring)")
    catch e
        e, stacktrace(catch_backtrace())
    end
    @test err isa JuliaLowering.MacroExpansionError
    @test err.msg == "expected a return type annotation `::SomeType`"
    @test isnothing(err.err)
    # Check that `catch_backtrace` can capture the stacktrace of the macro function
    @test any(sf->sf.func===:ccall_macro_parse, st)
end

# Tests for interop between old and new-style macros

# Hygiene interop
JuliaLowering.include_string(test_mod, raw"""
    macro call_oldstyle_macro(a)
        quote
            x = "x in call_oldstyle_macro"
            @oldstyle $a x
        end
    end

    macro newstyle(a, b, c)
        quote
            x = "x in @newstyle"
            ($a, $b, $c, x)
        end
    end
""")
# TODO: Make this macro lowering go via JuliaSyntax rather than the flisp code
# (JuliaSyntax needs support for old-style quasiquote processing)
Base.eval(test_mod, :(
macro oldstyle(a, b)
    quote
        x = "x in @oldstyle"
        @newstyle $(esc(a)) $(esc(b)) x
    end
end
))
@test JuliaLowering.include_string(test_mod, """
let x = "x in outer scope"
    @call_oldstyle_macro x
end
""") == ("x in outer scope",
         "x in call_oldstyle_macro",
         "x in @oldstyle",
         "x in @newstyle")

# Old style unhygenic escaping with esc()
Base.eval(test_mod, :(
macro oldstyle_unhygenic()
    esc(:x)
end
))
@test JuliaLowering.include_string(test_mod, """
let x = "x in outer scope"
    @oldstyle_unhygenic
end
""") == "x in outer scope"

# Exceptions in old style macros
Base.eval(test_mod, :(
macro oldstyle_error()
    error("Some error in old style macro")
end
))
@test try
    JuliaLowering.include_string(test_mod, """
    @oldstyle_error
    """)
catch exc
    sprint(showerror, exc)
end == """
MacroExpansionError while expanding @oldstyle_error in module Main.macro_test:
@oldstyle_error
└─────────────┘ ── Error expanding macro
Caused by:
Some error in old style macro"""

@test sprint(
    showerror,
    JuliaLowering.MacroExpansionError(
        JuliaLowering.expr_to_est(:(foo), LineNumberNode(1)),
        "fake error")) ==
            "MacroExpansionError:\n#= line 1 =# - fake error"

# Old-style macros returning non-Expr values
Base.eval(test_mod, :(
macro oldstyle_non_Expr()
    42
end
))
@test JuliaLowering.include_string(test_mod, """
@oldstyle_non_Expr
""") === 42

# New-style macros called with the wrong arguments
JuliaLowering.include_string(test_mod, raw"""
macro method_error_test(a)
end
""")
Base.eval(test_mod, :(
macro method_error_test()
end
))
try
    JuliaLowering.include_string(test_mod, raw"""
    @method_error_test x y
    """)
    @test false
catch exc
    @test exc isa JuliaLowering.MacroExpansionError
    mexc = exc.err
    @test mexc isa MethodError
    @test mexc.args isa Tuple{JuliaLowering.MacroContext, JuliaLowering.SyntaxTree, JuliaLowering.SyntaxTree}
end

@testset "calling with old/new macro signatures" begin
    # Old defined with 1 arg, new with 2 args, both with 3 (but with different values)
    Base.eval(test_mod, :(macro sig_mismatch(x); x; end))
    Base.eval(test_mod, :(macro sig_mismatch(x, y, z); z; end))
    JuliaLowering.include_string(test_mod, "macro sig_mismatch(x, y); x; end")
    JuliaLowering.include_string(test_mod, "macro sig_mismatch(x, y, z); x; end")

    @test JuliaLowering.include_string(test_mod, "@sig_mismatch(1)") === 1
    @test JuliaLowering.include_string(test_mod, "@sig_mismatch(1, 2)") === 1
    @test JuliaLowering.include_string(test_mod, "@sig_mismatch(1, 2, 3)") === 1 # 3 if we prioritize old sig
    err = try
        JuliaLowering.include_string(test_mod, "@sig_mismatch(1, 2, 3, 4)") === 1
    catch exc
        sprint(showerror, exc, context=:module=>test_mod)
    end
    @test startswith(err, """
    MacroExpansionError while expanding @sig_mismatch in module Main.macro_test:
    @sig_mismatch(1, 2, 3, 4)
    └───────────────────────┘ ── Error expanding macro
    Caused by:
    MethodError: no method matching var"@sig_mismatch"(""")
end

@testset "old macros producing exotic expr heads" begin
    @test JuliaLowering.include_string(test_mod, """
    let # example from @preserve docstring
        x = Ref{Int}(101)
        p = Base.unsafe_convert(Ptr{Int}, x)
        GC.@preserve x unsafe_load(p)
    end""") === 101 # Expr(:gc_preserve)

    # JuliaLowering.jl/issues/121
    @test JuliaLowering.include_string(test_mod, """
    GC.@preserve @static if true @__MODULE__ else end
    """) isa Module
    @test JuliaLowering.include_string(test_mod, """
    GC.@preserve @static if true v"1.14" else end
    """) isa VersionNumber

    # JuliaLowering.jl/issues/144
    @test JuliaLowering.include_string(test_mod, """
    f_preserve144() = let
        val = Any[]
        GC.@preserve val begin; end
    end
    f_preserve144()
    """) == nothing

    # JuliaLowering.jl/issues/145
    @test JuliaLowering.include_string(test_mod, """
    f_preserve145() = let
        debug_buffer = IOBuffer()
        # inside function to force compilation
        GC.@preserve debug_buffer 1
    end
    f_preserve145()
    """) == 1

    # only invokelatest produces :isglobal now, so MWE here
    Base.eval(test_mod, :(macro isglobal(x); esc(Expr(:isglobal, x)); end))
    @test JuliaLowering.include_string(test_mod, """
    some_global = 1
    function isglobal_chk(some_arg)
       local some_local = 1
       (@isglobal(some_undefined), @isglobal(some_global), @isglobal(some_arg), @isglobal(some_local))
    end
    isglobal_chk(1)
    """) === (true, true, false, false)
    # with K"Placeholder"s
    @test JuliaLowering.include_string(test_mod, """
    __ = 1
    function isglobal_chk(___)
       local ____ = 1
       (@isglobal(_), @isglobal(__), @isglobal(___), @isglobal(____))
    end
    isglobal_chk(1)
    """) === (false, false, false, false)

    # @test appears to be the only macro in base to use :inert
    test_result = JuliaLowering.include_string(test_mod, """
    using Test
    @test identity(123) === 123
    """; expr_compat_mode=true)
    @test test_result.value === true

    # @enum produces Expr(:toplevel)
    JuliaLowering.include_string(test_mod, """
    @enum SOME_ENUM X1 X2 X3
    """; expr_compat_mode=true)
    @test test_mod.SOME_ENUM <: Enum
    @test test_mod.X1 isa Enum

    # @testset produces :tryfinally with secret third arg
    @eval test_mod :(using Test)
    @test JuliaLowering.include_string(test_mod, "@test true") isa Test.Pass
    @testset let jltestset = JuliaLowering.include_string(test_mod, """
    @testset begin
        @test true
    end
    """; expr_compat_mode=true)
        @test jltestset isa Test.AbstractTestSet
        @test jltestset.n_passed == 1
    end
end

@testset "macros producing meta forms" begin
    function find_method_ci(thunk)
        ci = thunk.args[1]::Core.CodeInfo
        m = findfirst(x->(x isa Expr && x.head === :method && length(x.args) === 3), ci.code)
        ci.code[m].args[3]
    end
    jlower_e(s) = JuliaLowering.to_lowered_expr(
        JuliaLowering.lower(
            test_mod, JuliaLowering.parsestmt(
                JuliaLowering.SyntaxTree, s);
            expr_compat_mode=true))

    prog = "Base.@assume_effects :foldable function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).purity === find_method_ci(our).purity

    prog = "Base.@inline function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).inlining === find_method_ci(our).inlining

    prog = "Base.@noinline function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).inlining === find_method_ci(our).inlining

    prog = "Base.@constprop :none function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).constprop === find_method_ci(our).constprop

    prog = "Base.@nospecializeinfer function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).nospecializeinfer === find_method_ci(our).nospecializeinfer

    prog = "Base.@propagate_inbounds function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).propagate_inbounds === find_method_ci(our).propagate_inbounds

end

@testset "scope layers for normally-inert ASTs" begin
    # Right hand side of `.`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = :(hi)
        :(A.$x)
    end
    """) ≈ @ast_ [K"."
        "A"::K"Identifier"
        [K"inert" "hi"::K"Identifier"]
    ]
    # module
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = :(AA)
        :(module $x
        end
        )
    end
    """) ≈ @ast_ [K"module"
        v"1.14.0"::K"Value"
        true::K"Value"
        "AA"::K"Identifier"
        [K"block"]
    ]

    # In macro expansion, require that expressions passed in as macro
    # *arguments* get the lexical scope of the calling context, even for the
    # `x` in `M.$x` where the right hand side of `.` is normally quoted.
    @test JuliaLowering.include_string(test_mod, raw"""
        let x = :(someglobal)
            @eval M.$x
        end
    """) == "global in module M"

    JuliaLowering.include_string(test_mod, raw"""
        let y = 101
            @eval module AA
                x = $y
            end
        end
    """)
    @test test_mod.AA.x == 101

    # "Deferred hygiene" in macros which emit quoted code currently doesn't
    # work as might be expected.
    #
    # The old macro system also doesn't handle this - here's the equivalent
    # implementation
    # macro make_quoted_code(init, y)
    #     QuoteNode(:(let
    #         x = "inner x"
    #         $(esc(init))
    #         ($(esc(y)), x)
    #     end))
    # end
    #
    # TODO: The following should throw an error rather than producing a
    # surprising value, or work "as expected" whatever that is!
    JuliaLowering.include_string(test_mod, raw"""
    macro make_quoted_code(init, y)
        q = :(let
            x = "inner x"
            $init
            ($y, x)
        end)
        @ast q._graph q [K"inert_syntaxtree" q]
    end
    """)
    code = JuliaLowering.include_string(test_mod, """@make_quoted_code(x="outer x", x)""")
    @test_broken JuliaLowering.eval(test_mod, code) == ("outer x", "inner x")
end

@testset "toplevel macro hygiene" begin
    @eval test_mod global mod = $test_mod
    @eval test_mod module MacroMod
    global mod = MacroMod
    macro escaped_toplevel()
        esc(Expr(:toplevel, :(mod)))
    end
    macro inner_escaped_toplevel()
        Expr(:toplevel, esc(:(mod)))
    end
    macro unescaped_toplevel()
        Expr(:toplevel, :(mod))
    end
    end
    @test JuliaLowering.include_string(test_mod, "MacroMod.@escaped_toplevel") === test_mod
    @test JuliaLowering.include_string(test_mod, "MacroMod.@inner_escaped_toplevel") === test_mod
    @test JuliaLowering.include_string(test_mod, "MacroMod.@unescaped_toplevel") === test_mod.MacroMod
end

# JuliaLang/JuliaLowering.jl#120
#
# `__module__` should be expanded as the lexical module containing the expanded
# code, not the module corresponding to the current hygienic scope
JuliaLowering.include_string(test_mod, raw"""
module Mod1
macro indirect_MODULE()
    return :(@__MODULE__())
end
end
""")
code = JuliaLowering.include_string(test_mod, """Mod1.@indirect_MODULE()""")
@test JuliaLowering.eval(test_mod, code) === test_mod # !== test_mod.Mod1
# the lowering/eval iterator needs to expand in the correct world age (currently
# the only way to hit this from user code is macros producing toplevel)

@testset "macros defining macros" begin
    @eval test_mod macro make_and_use_macro_toplevel()
        Expr(:toplevel,
             esc(:(macro from_toplevel_expansion()
                   :(123)
               end)),
             esc(:(@from_toplevel_expansion())))
    end

    @test JuliaLowering.include_string(
        test_mod, "@make_and_use_macro_toplevel()"; expr_compat_mode=true) === 123

    if isdefined(test_mod, Symbol("@from_toplevel_expansion"))
        Base.delete_binding(test_mod, Symbol("@from_toplevel_expansion"))
    end

    @test JuliaLowering.include_string(
        test_mod, "@make_and_use_macro_toplevel()"; expr_compat_mode=false) === 123
end

@testset "SIMD loopinfo" begin
    @test JuliaLowering.include_string(test_mod, raw"""
    @eval let
        n = 10
        x = zeros(n)
        i = 1
        while i ≤ n
            x[i] += 1
            i += 1
            $(Expr(:loopinfo, Symbol("julia.simdloop"), nothing))  # Mark loop as SIMD loop
        end
        sum(x)
    end
    """; expr_compat_mode=true) == 10.0

    @test JuliaLowering.include_string(test_mod, raw"""
    @eval let
        n = 10
        x = zeros(n)
        i = 1
        while i ≤ n
            x[i] += 1
            i += 1
            $(Expr(:loopinfo, Symbol("julia.simdloop"), Symbol("julia.ivdep")))  # Mark loop as SIMD loop
        end
        sum(x)
    end
    """; expr_compat_mode=true) == 10.0

    JuliaLowering.include_string(test_mod, """
    @noinline function inner(x, y)
        s = zero(eltype(x))
        for i in eachindex(x, y)
            @inbounds s += x[i]*y[i]
        end
        return s
    end
    """)

    JuliaLowering.include_string(test_mod, """
    @noinline function innersimd(x, y)
        s = zero(eltype(x))
        @simd for i in eachindex(x, y)
            @inbounds s += x[i] * y[i]
        end
        return s
    end
    """)

    @test test_mod.inner([1,2,3], [1,2,3]) == 14
    @test test_mod.innersimd([1,2,3], [1,2,3]) == 14
end

@testset "@boundscheck / @inbounds" begin
    JuliaLowering.include_string(test_mod, """
    function sum_inbounds(A::AbstractArray)
        r = zero(eltype(A))
        for i in eachindex(A)
            @inbounds r += A[i]
        end
        return r
    end
    """; expr_compat_mode=true)
    @test test_mod.sum_inbounds([1,2,3]) == 6

    JuliaLowering.include_string(test_mod, """
    @inline function g_boundscheck(A, i)
        @boundscheck checkbounds(A, i)
        return A[i]
    end
    """; expr_compat_mode=true)
    @test test_mod.g_boundscheck(1:2, 2) == 2
end

@testset "@__FUNCTION__ and Expr(:thisfunction)" begin
    @testset "Basic usage" begin
        # @__FUNCTION__ in regular functions
        JuliaLowering.include_string(test_mod, raw"""
        test_function_basic() = @__FUNCTION__
        """; expr_compat_mode=true)
        @test test_mod.test_function_basic() === test_mod.test_function_basic

        # Expr(:thisfunction) in regular functions
        JuliaLowering.include_string(test_mod, raw"""
            @eval regular_func() = @__FUNCTION__
        """; expr_compat_mode=true)
        @test test_mod.regular_func() === test_mod.regular_func
    end

    @testset "Recursion" begin
        # Factorial with @__FUNCTION__
        JuliaLowering.include_string(test_mod, raw"""
        factorial_function(n) = n <= 1 ? 1 : n * (@__FUNCTION__)(n - 1)
        """; expr_compat_mode=true)
        @test test_mod.factorial_function(5) == 120

        # Fibonacci with Expr(:thisfunction)
        JuliaLowering.include_string(test_mod, raw"""
        struct RecursiveCallableStruct; end
        (::RecursiveCallableStruct)(n) = n <= 1 ? n : @__FUNCTION__()(n-1) + @__FUNCTION__()(n-2)
        """; expr_compat_mode=true)
        @test test_mod.RecursiveCallableStruct()(10) === 55

        # Anonymous function recursion
        @test JuliaLowering.include_string(test_mod, raw"""
        (n -> n <= 1 ? 1 : n * (@__FUNCTION__)(n - 1))(5)
        """; expr_compat_mode=true) == 120
    end

    @testset "Closures and nested functions" begin
        # Prevents boxed closures
        JuliaLowering.include_string(test_mod, raw"""
        function make_closure()
            fib(n) = n <= 1 ? 1 : (@__FUNCTION__)(n - 1) + (@__FUNCTION__)(n - 2)
            return fib
        end
        """; expr_compat_mode=true)
        Test.@inferred test_mod.make_closure()
        closure = test_mod.make_closure()
        @test closure(5) == 8
        Test.@inferred closure(5)

        # Complex closure of closures
        JuliaLowering.include_string(test_mod, raw"""
        function f1()
            function f2()
                function f3()
                    return @__FUNCTION__
                end
                return (@__FUNCTION__), f3()
            end
            return (@__FUNCTION__), f2()...
        end
        """; expr_compat_mode=true)
        Test.@inferred test_mod.f1()
        @test test_mod.f1()[1] === test_mod.f1
        @test test_mod.f1()[2] !== test_mod.f1
        @test test_mod.f1()[3] !== test_mod.f1
        @test test_mod.f1()[3]() === test_mod.f1()[3]
        @test test_mod.f1()[2]()[2]() === test_mod.f1()[3]
    end

    @testset "Do blocks" begin
        function test_do_block()
            result = JuliaLowering.include_string(test_mod, raw"""
            map([1, 2, 3]) do x
                return (@__FUNCTION__, x)
            end
            """; expr_compat_mode=true)
            # All should refer to the same do-block function
            @test all(r -> r[1] === result[1][1], result)
            # Values should be different
            @test [r[2] for r in result] == [1, 2, 3]
            # It should be different than `test_do_block`
            @test result[1][1] !== test_do_block
        end
        test_do_block()
    end

    @testset "Keyword arguments" begin
        # @__FUNCTION__ with kwargs
        JuliaLowering.include_string(test_mod, raw"""
        f_thisfunction_kw(; n) = n <= 1 ? 1 : n * (@__FUNCTION__)(; n = n - 1)
        """; expr_compat_mode=true)
        @test test_mod.f_thisfunction_kw(n = 5) == 120

        # Expr(:thisfunction) with kwargs
        JuliaLowering.include_string(test_mod, raw"""
        f_thisfunction_kw2(; n=1) = n <= 1 ? n : n * @__FUNCTION__()(; n=n-1)
        """; expr_compat_mode=true)
        result = test_mod.f_thisfunction_kw2(n=5)
        @test result == 120
    end

    @testset "Callable structs" begin
        # @__FUNCTION__ in callable structs
        JuliaLowering.include_string(test_mod, raw"""
        module A
            struct CallableStruct{T}; val::T; end
            (c::CallableStruct)() = @__FUNCTION__
        end
        """; expr_compat_mode=true)
        JuliaLowering.include_string(test_mod, raw"""
        using .A: CallableStruct
        """; expr_compat_mode=true)
        c = test_mod.CallableStruct(5)
        @test c() === c

        # In closures, var"#self#" should refer to the enclosing function,
        # NOT the enclosing struct instance
        JuliaLowering.include_string(test_mod, raw"""
        struct CallableStruct2; end
        @eval function (obj::CallableStruct2)()
            function inner_func()
                @__FUNCTION__
            end
            inner_func
        end
        """; expr_compat_mode=true)

        let cs = test_mod.CallableStruct2()
            @test cs()() === cs()
            @test cs()() !== cs
        end

        # Accessing values via self-reference
        JuliaLowering.include_string(test_mod, raw"""
        struct CallableStruct3
            value::Int
        end
        (obj::CallableStruct3)() = @__FUNCTION__()
        (obj::CallableStruct3)(x) = @__FUNCTION__().value + x
        """; expr_compat_mode=true)

        let cs = test_mod.CallableStruct3(42)
            @test cs() === cs
            @test cs(10) === 52
        end

        # Callable struct with args and kwargs
        JuliaLowering.include_string(test_mod, raw"""
        struct CallableStruct4
        end
        @eval function (obj::CallableStruct4)(x, args...; y=2, kws...)
            return (; func=(@__FUNCTION__), x, args, y, kws)
        end
        """; expr_compat_mode=true)
        c = test_mod.CallableStruct4()
        @test c(1).func === c
        @test c(2, 3).args == (3,)
        @test c(2; y=4).y == 4
        @test c(2; y=4, a=5, b=6, c=7).kws[:c] == 7
    end

    @testset "Special cases" begin
        # Generated functions
        JuliaLowering.include_string(test_mod, raw"""
        let
            @generated foo2() = @__FUNCTION__
            foo2() === foo2
        end
        """; expr_compat_mode=true)

        # Struct constructors
        let
            JuliaLowering.include_string(test_mod, raw"""
            struct Cols{T<:Tuple}
                cols::T
                operator
                Cols(args...; operator=union) = (new{typeof(args)}(args, operator); string(@__FUNCTION__))
            end
            """; expr_compat_mode=true)
            result = @invokelatest test_mod.Cols(1, 2, 3)
            @test occursin("Cols", result)
        end

        # Should not access arg-map for local variables
        # TODO: worth the special case?
        JuliaLowering.include_string(test_mod, raw"""
            function f_thisfunction_argmap end
            function (f_thisfunction_argmap::typeof(f_thisfunction_argmap))()
                f_thisfunction_argmap = 1
                @__FUNCTION__
            end
        """; expr_compat_mode=true)
        @test_broken test_mod.f_thisfunction_argmap() ===
            test_mod.f_thisfunction_argmap
    end

    @test JuliaLowering.include_string(test_mod, """
        @eval let f=[ ()->$(Expr(:thisfunction)) for i = 1:1 ][1]; f() === f; end
    """; expr_compat_mode=true)
end

@testset "macro source LineNumberNode" begin
    Base.include_string(test_mod, raw"""
    macro srcfile()
        string(__source__.file)
    end
    """)

    mac_ex = Expr(:macrocall, Symbol("@srcfile"), LineNumberNode(1, "goodfile"))
    mac_st = JuliaLowering.expr_to_est(mac_ex, LineNumberNode(1, "badfile"))

    @test JuliaLowering.eval(test_mod, mac_st) === "goodfile"

    # tolerate nothing
    mac_ex = Expr(:macrocall, Symbol("@srcfile"), nothing)
    mac_st = JuliaLowering.expr_to_est(mac_ex, LineNumberNode(1, "badfile"))
    @test JuliaLowering.eval(test_mod, mac_st) == "none"
end

end
