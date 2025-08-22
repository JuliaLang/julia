module macros

using JuliaLowering, Test

module test_mod end

JuliaLowering.include_string(test_mod, raw"""
module M
    using JuliaLowering: JuliaLowering, @ast, @chk, adopt_scope
    using JuliaSyntax

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
        e1 = adopt_scope(:(sym_introduced_from_M), __context__)
        quote
            $e1 = $ex
            nothing
        end
    end

    macro inner()
        :(y)
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
ctx, expanded = JuliaLowering.expand_forms_1(test_mod, ex, false)
@test JuliaLowering.sourcetext.(JuliaLowering.flattened_provenance(expanded[2])) == [
    "M.@outer()"
    "@inner"
    "y"
]
# Layer parenting
@test expanded[1].scope_layer == 2
@test expanded[2].scope_layer == 3
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
@test JuliaLowering.include_string(test_mod, "@ccall strlen(\"asdf\"::Cstring)::Csize_t gc_safe=true") == 4
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
    @test err.msg == "Expected a return type annotation `::SomeType`"
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
MacroExpansionError while expanding @oldstyle_error in module Main.macros.test_mod:
@oldstyle_error
└─────────────┘ ── Error expanding macro
Caused by:
Some error in old style macro"""

@test sprint(
    showerror,
    JuliaLowering.MacroExpansionError(
        JuliaLowering.expr_to_syntaxtree(:(foo), LineNumberNode(1)),
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
        sprint(showerror, exc, context=:module=>@__MODULE__)
    end
    @test startswith(err, """
    MacroExpansionError while expanding @sig_mismatch in module Main.macros.test_mod:
    @sig_mismatch(1, 2, 3, 4)
    └───────────────────────┘ ── Error expanding macro
    Caused by:
    MethodError: no method matching var"@sig_mismatch"(::JuliaLowering.MacroContext, ::JuliaLowering.SyntaxTree""")
end

@testset "old macros producing exotic expr heads" begin
    @test JuliaLowering.include_string(test_mod, """
    let # example from @preserve docstring
        x = Ref{Int}(101)
        p = Base.unsafe_convert(Ptr{Int}, x)
        GC.@preserve x unsafe_load(p)
    end""") === 101 # Expr(:gc_preserve)

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
end

end # module macros
