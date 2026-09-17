test_mod = @newmod(macro_test, Main, JL_NEW_EDITION)
@eval test_mod import JuliaLowering
Base.eval(test_mod, :(const var"@ast" = $(JuliaLowering.var"@ast")))

# Set up identity macros for use in this file
# - `old_e`, escaping its whole output, should do nothing to an expression
# - `new_m`, introducing no new syntax, should behave exactly as `old_e` does
# - `old_*` should behave the same across JL and flisp
# - `old_h` should not be specified too hard here (buggy renaming pass)
fl_eval(test_mod, :(macro old_e(x); esc(x); end))
fl_eval(test_mod, :(macro old_h(x); x; end))
JuliaLowering.include_string(test_mod, "macro new_m(x); x; end")
fl_eval(test_mod, :(global mvar = "global mvar"))

@testset "edition sanity-check" begin
    @test JuliaLowering.include_string(
        test_mod, "JuliaLowering.@edition") ==
        JuliaSyntax.JL_NEW_EDITION
    @test jl_eval(
        test_mod, "JuliaLowering.@edition"; edition=JL_NEW_EDITION) ==
        JuliaSyntax.JL_NEW_EDITION
    @test jl_eval(
        test_mod, "JuliaLowering.@edition"; edition=JL_OLD_EDITION) ==
        JuliaSyntax.JL_OLD_EDITION

    # TODO: test the edition of returned syntax
    @test jl_eval(@newmod(), """
    JuliaLowering.@edition JuliaSyntax.JL_NEW_EDITION macro m(); end
    """; edition=JL_NEW_EDITION) isa Function
    @test jl_eval(@newmod(), """
    JuliaLowering.@edition JuliaSyntax.JL_NEW_EDITION macro m(); end
    """; edition=JL_OLD_EDITION) isa Function

    @test jl_eval(@newmod(), """
    JuliaLowering.@edition JuliaSyntax.JL_OLD_EDITION macro m(); end
    """; edition=JL_NEW_EDITION) isa Function
    @test jl_eval(@newmod(), """
    JuliaLowering.@edition JuliaSyntax.JL_OLD_EDITION macro m(); end
    """; edition=JL_OLD_EDITION) isa Function
end

# Basic checks that arbitrary nesting of transparent macros (no new syntax in new
# macros, escaped/unhygienic in old macros) doesn't introduce opaque layers
@testset "basic transparent macros: old macros" for run in [
    (x::String)->Base.include_string(
        test_mod, "#=FLISP SANITY-CHECK=# "*x),
    (x::String)->jl_eval(
        test_mod, "#=JL COMPAT=# "*x; edition=JL_OLD_EDITION),
    (x::String)->jl_eval(
        test_mod, "#=JL=# "*x; edition=JL_NEW_EDITION)]

    @test run("@old_e let mvar = 0; mvar; end") == 0
    @test run("@old_e let @old_e(mvar = 0); mvar; end") == 0
    @test run("@old_e let @old_e(@old_e(mvar = 0)); mvar; end") == 0
    @test run("@old_e let @old_e(mvar) = 0; mvar; end") == 0
    @test run("@old_e let @old_e(@old_e(mvar)) = 0; mvar; end") == 0
    @test run("@old_e let mvar = 0; @old_e(mvar); end") == 0
    @test run("@old_e let mvar = 0; @old_e(@old_e(mvar)); end") == 0
    @test run("@old_e let @old_e(@old_e(mvar) = 0); @old_e(mvar); end") == 0

    @test run("@old_h let mvar = 0; mvar; end") == 0
    @test run("@old_h let @old_e(mvar = 0); mvar; end") == 0
    @test run("@old_h let @old_e(@old_e(mvar = 0)); mvar; end") == 0
    @test run("@old_h let @old_e(mvar) = 0; mvar; end") == 0
    @test run("@old_h let @old_e(@old_e(mvar)) = 0; mvar; end") == 0
    @test run("@old_h let mvar = 0; @old_e(mvar); end") == 0
    @test run("@old_h let mvar = 0; @old_e(@old_e(mvar)); end") == 0
    @test run("@old_h let @old_e(@old_e(mvar) = 0); @old_e(mvar); end") == 0

    @test run("@old_h @old_h let mvar = 0; mvar; end") == 0
    @test run("@old_h @old_h let @old_e(mvar = 0); mvar; end") == 0
    @test run("@old_h @old_h let @old_e(@old_e(mvar = 0)); mvar; end") == 0
    @test run("@old_h @old_h let @old_e(mvar) = 0; mvar; end") == 0
    @test run("@old_h @old_h let @old_e(@old_e(mvar)) = 0; mvar; end") == 0
    @test run("@old_h @old_h let mvar = 0; @old_e(mvar); end") == 0
    @test run("@old_h @old_h let mvar = 0; @old_e(@old_e(mvar)); end") == 0
    @test run("@old_h @old_h let @old_e(@old_e(mvar) = 0); @old_e(mvar); end") == 0
end
@testset "basic transparent macros: new macros only" for edition in [JL_OLD_EDITION, JL_NEW_EDITION]
    local run = (x::String)->jl_eval(test_mod, x; edition)

    @test run("@new_m let mvar = 0; mvar; end") == 0
    @test run("@new_m let @new_m(mvar = 0); mvar; end") == 0
    @test run("@new_m let @new_m(@new_m(mvar = 0)); mvar; end") == 0
    @test run("@new_m let @new_m(mvar) = 0; mvar; end") == 0
    @test run("@new_m let @new_m(@new_m(mvar)) = 0; mvar; end") == 0
    @test run("@new_m let mvar = 0; @new_m(mvar); end") == 0
    @test run("@new_m let mvar = 0; @new_m(@new_m(mvar)); end") == 0
    @test run("@new_m let @new_m(@new_m(mvar) = 0); @new_m(mvar); end") == 0
end
@testset "basic transparent macros: new+old interop" for edition in [JL_OLD_EDITION, JL_NEW_EDITION],
    mcall in ["@old_e ", "@new_m ", "@old_e @new_m ", "@new_m @old_e "],
    old_h in ["", "@old_h "]

    local run = (x::String)->jl_eval(test_mod, x; edition)

    @test run(old_h*mcall*"let mvar = 0; mvar; end") == 0
    @test run(old_h*"let ("*mcall*"mvar = 0); mvar; end") == 0
    @test run(old_h*"let ("*mcall*"mvar) = 0; mvar; end") == 0
    @test run(old_h*"let mvar = 0; ("*mcall*"mvar); end") == 0

    @testset for mcall2 in ["@old_e ", "@new_m ", "@old_e @new_m ", "@new_m @old_e "]
        @test run(old_h*mcall*"let ("*mcall2*"mvar) = 0; mvar; end") == 0
        @test run(old_h*mcall*"let mvar = 0; ("*mcall2*"mvar); end") == 0
        @test run(old_h*"let ("*mcall*"mvar = 0); ("*mcall2*"mvar); end") == 0
    end
end

# More simple checks with no difference between macro module and macrocall module
isdefined(test_mod, :x) && Base.delete_binding(test_mod, :x)
fl_eval(test_mod, :(macro old_read_x(); :x; end))
fl_eval(test_mod, :(macro old_suggest_x(arg)
                        quote
                            let x = "suggested (old)"
                                $(esc(arg))
                            end
                        end
                    end))
JuliaLowering.include_string(test_mod, raw"""
    macro new_read_x(); @legacy_quote_to_syntax :x; end
""")
JuliaLowering.include_string(test_mod, raw"""
    macro new_suggest_x(arg)
        @legacy_quote_to_syntax quote
            let x = "suggested (new)"
                $arg
            end
        end
    end
""")
@testset "basic hygiene: check that name resolution fails where it should (flisp)"  for run in [
    (x::String)->fl_eval(test_mod,JuliaSyntax.parsestmt(Expr, "#=FLISP SANITY-CHECK=# "*x)),
    (x::String)->jl_eval(
        test_mod, "#=JL COMPAT=# "*x; edition=JL_OLD_EDITION),
    (x::String)->jl_eval(
        test_mod, "#=JL=# "*x; edition=JL_NEW_EDITION)]
    @test_throws UndefVarError run("@old_read_x()")
    @test_throws UndefVarError run("let x = 0; @old_read_x(); end")
    @test_throws UndefVarError run("@old_suggest_x(x)")
    @test_throws UndefVarError run("@old_suggest_x(@old_read_x())")
    @test run("let x = 1; @old_suggest_x(x); end") == 1
    @test run("@old_suggest_x(let x = 1; x; end)") == 1
    @test_throws UndefVarError run("@old_suggest_x(let x = 1; @old_read_x(); end)") == 1
end
@testset "basic hygiene: check that name resolution fails where it should (new)" for run in [
    (x::String)->jl_eval(
        test_mod, "#=JL COMPAT=# "*x; edition=JL_OLD_EDITION),
    (x::String)->jl_eval(
        test_mod, "#=JL=# "*x; edition=JL_NEW_EDITION)]

    @test_throws UndefVarError run("@new_read_x()")
    @test_throws UndefVarError run("let x = 0; @new_read_x(); end")
    @test_throws UndefVarError run("@new_suggest_x(x)")
    @test_throws UndefVarError run("@new_suggest_x(@new_read_x())")
    @test run("let x = 1; @new_suggest_x(x); end") == 1
    @test run("@new_suggest_x(let x = 1; x; end)") == 1
    @test_throws UndefVarError run("@new_suggest_x(let x = 1; @new_read_x(); end)") == 1

    @testset "old/new interop" begin
        @testset for wrapper in ["", "@old_e ", "@old_h ", "@new_m "]
            @test_throws UndefVarError run(wrapper*"@old_suggest_x(@new_read_x())")
            @test_throws UndefVarError run(wrapper*"@new_suggest_x(@old_read_x())")
            @test run(wrapper*"let x = 1; @old_suggest_x(x); end") == 1
            @test run(wrapper*"let x = 1; @new_suggest_x(x); end") == 1
            @test run(wrapper*"@old_suggest_x(let x = 1; x; end)") == 1
            @test run(wrapper*"@new_suggest_x(let x = 1; x; end)") == 1
        end
    end
end

@eval test_mod (global test_mod_global = "test_mod_global")
@newmod(EvalMod, test_mod)
@testset "@eval" for run in [
    (x::String)->fl_eval(test_mod,JuliaSyntax.parsestmt(Expr, "#=FLISP SANITY-CHECK=# "*x)),
    (x::String)->jl_eval(
        test_mod, "#=JL COMPAT=# "*x; edition=JL_OLD_EDITION),
    (x::String)->jl_eval(
        test_mod, "#=JL=# "*x; edition=JL_NEW_EDITION)]

    has_syntax = run(raw"@legacy_quote_to_syntax :x") isa SyntaxTree
    treetype = has_syntax ? SyntaxTree : Expr
    symtype = has_syntax ? SyntaxTree : Symbol
    valtype = has_syntax ? SyntaxTree : Any

    @test run(raw"@eval nothing") == nothing
    @test run(raw"@eval :sym") == :sym
    @test run(raw"@eval QuoteNode(:sym)") == QuoteNode(:sym)
    @test run(raw"@eval Expr(:call, :identity, 1)") == Expr(:call, :identity, 1)
    @test run(raw"@eval :(identity(1))") == Expr(:call, :identity, 1)
    # syntax edition of the caller should be propagated to JL.eval
    @test run(raw"@eval @legacy_quote_to_syntax(:sym)") isa symtype
    @test run(raw"@eval @legacy_quote_to_syntax(:(identity(1)))") isa treetype
    @test run(raw"@eval @eval @legacy_quote_to_syntax(:(identity(1)))") isa treetype

    # quoting behaves the same as outside of eval
    @test run(raw"@eval(:(1 + 2))") == Expr(:call, :+, 1, 2)
    @test run(raw"@eval(:true)") == true
    @test run(raw"@eval(:x)") == :x

    # interpolation
    @test run(raw"let x = nothing; @eval $x; end") == nothing
    @test run(raw"let x = :identity; @eval $x; end") == Base.identity
    @test run(raw"let x = QuoteNode(:sym); @eval $x; end") == :sym
    @test run(raw"let x = Expr(:call, :identity, 1); @eval $x; end") == 1
    @test run(raw"let x = :(identity(1)); @eval $x; end") == 1
    @test run(raw"let x = @legacy_quote_to_syntax(:identity); @eval $x; end") == Base.identity
    @test run(raw"let x = @legacy_quote_to_syntax(:(identity(1))); @eval $x; end") == 1

    # interpolate into quote
    @test run(raw"let test_mod_global = 0xbad
        @eval (@legacy_quote_to_syntax :($test_mod_global))
    end") isa valtype
    @test run(raw"let test_mod_global = 0xbad
        @eval @legacy_quote_to_syntax(:(1,$test_mod_global))
    end") isa treetype

    # interpolate into quote, double-unquote (mixes of syntax and expr may not
    # need to work)
    @test run(raw"let x = @legacy_quote_to_syntax(:identity)
        @eval (:($($x)))
    end") == Base.identity
    @test run(raw"let x = @legacy_quote_to_syntax(:identity)
        @eval (@legacy_quote_to_syntax :($($x)))
    end") isa valtype
    @test run(raw"let x = @legacy_quote_to_syntax(:identity)
        @eval @legacy_quote_to_syntax(:(1,$$x))
    end") isa treetype
    @test run(raw"let x = @legacy_quote_to_syntax(:identity)
        @eval $(@eval (:(1,$$x)))
    end") == (1, Base.identity)

    # module eval-ed into
    @test run(raw"@eval @__MODULE__") == test_mod
    @test run(raw"@eval @eval @__MODULE__") == test_mod
    # two-arg eval should not obey typical hygiene: decls go to specified module
    @test run(raw"@eval EvalMod @__MODULE__") == test_mod.EvalMod
    run(raw"@eval EvalMod global eval_mod_global = 1"); Core.@latestworld
    @test test_mod.EvalMod.eval_mod_global == 1
    run(raw"@eval EvalMod eval_mod_global_implicit = 1"); Core.@latestworld
    @test test_mod.EvalMod.eval_mod_global_implicit == 1
    # standard hygiene atop two-arg eval
    fl_eval(test_mod, :(module MacroMod
                        module MacroModInner; end
                        macro m_setglobal(); esc(:(mmglobal0 = 0)); end
                        macro m_eval_inner(x); :(@eval $MacroModInner $x) ; end
                        end))
    Core.@latestworld
    @eval test_mod.EvalMod (const MacroMod2 = $(test_mod.MacroMod))
    @eval test_mod.EvalMod (const var"@legacy_quote_to_syntax" =
        $(test_mod.var"@legacy_quote_to_syntax"))
    @test run(raw"@eval EvalMod @legacy_quote_to_syntax(:(1 + 1))") isa treetype
    @test run(raw"@eval @eval EvalMod @legacy_quote_to_syntax(:(1 + 1))") isa treetype

    @test run(raw"@eval EvalMod MacroMod2.@m_setglobal") == 0
    Core.@latestworld
    @test isdefined(test_mod.EvalMod, :mmglobal0)
    @test !isdefined(test_mod, :mmglobal0)
    @test !isdefined(test_mod.MacroMod, :mmglobal0)

    @test run(raw"@eval EvalMod MacroMod2.@m_eval_inner(global mmglobal1 = 1)") == 1
    Core.@latestworld
    @test isdefined(test_mod.MacroMod.MacroModInner, :mmglobal1)
    @test !isdefined(test_mod, :mmglobal1)
    @test !isdefined(test_mod.MacroMod, :mmglobal1)
    @test !isdefined(test_mod.EvalMod, :mmglobal1)

    # interpolation into top-level: symbol declared in the new module
    run(raw"let x = @legacy_quote_to_syntax(:sym)
        @eval EvalMod module tmp; module inner_eval_mod; global $x = 123; end; end
    end") isa Module
    Core.@latestworld
    @test test_mod.EvalMod.tmp.inner_eval_mod isa Module
    @test test_mod.EvalMod.tmp.inner_eval_mod.sym == 123

    # hygiene
    run("let eval_result = 0; @eval 1+1; eval_result; end") == 0

    @testset "(AI) single-arg @eval does not over-preserve hygiene" for edition in (JL_OLD_EDITION, JL_NEW_EDITION)
        root = @newmod(root)
        jl_eval(root, raw"""
        module MacB
            import JuliaLowering.@legacy_quote_to_syntax
            macro do_eval()
                @legacy_quote_to_syntax quote
                    @eval (@__MODULE__)
                end
            end
        end
        module MacA
            import JuliaLowering.@legacy_quote_to_syntax
            import ..MacB
            macro wrap()
                @legacy_quote_to_syntax quote
                    @eval (@__MODULE__)
                end
            end
            macro via_b()
                @legacy_quote_to_syntax quote
                    MacB.@do_eval()
                end
            end
            macro wrap_ee()
                @legacy_quote_to_syntax quote
                    @eval @eval (@__MODULE__)
                end
            end
            macro wrap_two_arg()
                # two-arg control: explicit target module; the payload's
                # `@__MODULE__` must still see the *target* module
                @legacy_quote_to_syntax quote
                    @eval MacB (@__MODULE__)
                end
            end
            macro wrap_arg(ex)
                # caller-provided payload (caller's hygiene layer)
                @legacy_quote_to_syntax quote
                    @eval $ex
                end
            end
            macro wrap_fn()
                # `@eval` captures the module current when the enclosing function
                # *definition* is expanded, like flisp
                @legacy_quote_to_syntax quote
                    () -> @eval (@__MODULE__)
                end
            end
            macro mkmod()
                mod = gensym("EvalMod")
                @legacy_quote_to_syntax quote
                    @eval module $mod
                        const inside = (@__MODULE__)
                    end
                end
            end
            macro mkmod_payload(ex)
                mod = gensym("EvalMod2")
                @legacy_quote_to_syntax quote
                    @eval module $mod
                        $ex
                    end
                end
            end
        end
        module Sub
            import ..MacA
        end
        """; edition)
        Core.@latestworld

        run(str) = jl_eval(root, str; edition)

        # `@eval` inside another macro's unescaped expansion evaluates in the
        # caller's module, not the macro's
        @test run("MacA.@wrap()") === root
        # ... even when the `@eval`-ing macro is called by another macro's expansion
        # (flisp: still the dynamic module, not either macro's module)
        @test run("MacA.@via_b()") === root
        # `@eval` nested in `@eval` re-expands against the outer target
        @test run("MacA.@wrap_ee()") === root
        # two-arg control: explicit module wins; payload `@__MODULE__` follows it
        @test run("MacA.@wrap_two_arg()") === root.MacB
        # macro-generated closure: `@eval` binds the definition-time module
        @test Base.invokelatest(run("MacA.@wrap_fn()")) === root
        # the same macro evaluated into a different module follows the live module
        @test jl_eval(
            root.Sub, "MacA.@wrap()"; edition) === root.Sub

        # Caller-provided payloads evaluate in the caller's module
        @test run("MacA.@wrap_arg(arg_marker = (@__MODULE__))") === root
        if edition == JL_NEW_EDITION
            # With SyntaxTree-passed arguments the payload keeps the caller's
            # hygiene: the global lands in `root` and is visible there. (In
            # the old syntax edition the old-style Expr round-trip re-layers the payload
            # with the macro's hygiene and the assignment becomes a hygienic
            # toplevel local -- a pre-existing divergence from flisp tracked by
            # the "hygienic toplevel assignments" TODO in scope_analysis.jl.)
            @test Base.invokelatest(isdefined, root, :arg_marker)
            @test Base.invokelatest(getfield, root, :arg_marker) === root
        end

        # The SafeTestsets shape: a macro-generated `@eval module $mod ... end`
        # creates the module under the dynamic (caller) module
        m = run("MacA.@mkmod()")
        @test m isa Module
        @test parentmodule(m) === root
        @test Base.invokelatest(getfield, m, :inside) === m
        # ... and user payload interpolated into the module body sees the fresh
        # module as its dynamic module (a user's own `@eval` inside a
        # `@safetestset` acts on the anonymous test module)
        m2 = run("MacA.@mkmod_payload(@eval user_marker = (@__MODULE__))")
        @test m2 isa Module
        @test parentmodule(m2) === root
        @test Base.invokelatest(getfield, m2, :user_marker) === m2

        if edition == JL_OLD_EDITION
            # Escaped expansions (old-style macros only): same dynamic target
            jl_eval(root, raw"""
        module MacEsc
            macro wrap_esc()
                esc(quote
                    @eval esc_marker = (@__MODULE__)
                end)
            end
        end
        """; edition)
            Core.@latestworld
            @test run("MacEsc.@wrap_esc()") === root
            @test Base.invokelatest(isdefined, root, :esc_marker)
            @test Base.invokelatest(getfield, root, :esc_marker) === root
        end

        # An old-style (flisp-defined and -lowered) macro whose expansion calls
        # `@eval` gets the same treatment when invoked under JuliaLowering
        fl_eval(root, :(module MacFl
                        macro flwrap()
                            quote
                                @eval (@__MODULE__)
                            end
                        end
                        end))
        Core.@latestworld
        @test run("MacFl.@flwrap()") === root
    end

    @testset "(AI) const shows up in caller mod" begin
        Core.eval(test_mod, :(module MacHome2
                       macro make_const()
                           :( @eval const CMARKER = 42 )
                       end
                       end))
        Core.eval(test_mod, :(MacHome2.@make_const()))
        @test isdefined(test_mod, :CMARKER)

        jl_eval(test_mod, :(module MacHome2
                       macro make_const()
                           :( @eval const CMARKER = 42 )
                       end
                       end); edition=JL_OLD_EDITION)
        JuliaLowering.eval(test_mod, :(MacHome2.@make_const()))
        @test isdefined(test_mod, :CMARKER)
    end
end

@eval test_mod module hscope_mod; global hscope_g = 123; end
@eval test_mod module nothing_mod; end
@eval test_mod global hscope_g = 234
@testset "hygienic scope should be usable without macros" begin
    @test jl_eval(
        test_mod, Expr(
            Symbol("hygienic-scope"),
            1, test_mod); edition=JL_OLD_EDITION) == 1
    @test jl_eval(
        test_mod, Expr(
            Symbol("hygienic-scope"),
            :hscope_g,
            test_mod.hscope_mod); edition=JL_OLD_EDITION) == 123
    @test jl_eval(
        test_mod, Expr(
            Symbol("hygienic-scope"),
            Expr(:escape, :hscope_g),
            test_mod.nothing_mod); edition=JL_OLD_EDITION) == 234
    @test jl_eval(
        test_mod, Expr(
            Symbol("hygienic-scope"), Expr(
                Symbol("hygienic-scope"),
                Expr(:escape, Expr(:escape, :hscope_g)),
                test_mod.nothing_mod),
            test_mod.nothing_mod); edition=JL_OLD_EDITION) == 234
    @test jl_eval(
        test_mod, Expr(
            Symbol("hygienic-scope"), Expr(
                Symbol("hygienic-scope"),
                Expr(:escape, :hscope_g),
                test_mod.nothing_mod),
            test_mod.hscope_mod); edition=JL_OLD_EDITION) == 123
end

Base.eval(test_mod, :(
    test_hscope(x, mod=$test_mod) = Expr(Symbol("hygienic-scope"), x, mod)
))
Base.eval(test_mod, :(
    # +3 new scopes and -4 escapes = normal unhygienic macro
    macro oldstyle_silly_scopes(x, y)
        stmt1 = test_hscope(test_hscope(test_hscope(esc(esc(esc(esc(:($x = 123))))))))
        stmt2 = esc(test_hscope(esc(test_hscope(esc(test_hscope(esc(:($y = 456))))))))
        Expr(:block, stmt1, stmt2)
    end))
@testset "escape and hygienic-scope forms" for run in [
    (x::String)->Base.include_string(
        test_mod, "#=FLISP SANITY-CHECK=# "*x),
    (x::String)->jl_eval(
        test_mod, "#=JL COMPAT=# "*x; edition=JL_OLD_EDITION),
    (x::String)->jl_eval(
        test_mod, "#=JL=# "*x; edition=JL_NEW_EDITION)]

    @test run(raw"""
    let (x, y) = (0, 0); @oldstyle_silly_scopes(x, y); (x, y); end
    """) === (123, 456)
    @test run(raw"""begin
    global_x, global_y = 0, 0
    @oldstyle_silly_scopes(global_x, global_y)
    global_x, global_y
    end""") === (123, 456)
end

@testset "apply_expansion_layer mutation testing" begin
    local test_mod = @newmod(apply_expansion_layer, Main, JL_NEW_EDITION)
    # recursion can't stop at module/toplevel/inert without tweaks, because a
    # macro can pull random stuff out of it.  This also tests calling into macro
    # expansion from macros, mostly because re-using macros I've already written
    # is the easiest way to create non-surface-syntax SyntaxTree as of writing.
    JuliaLowering.include_string(test_mod, raw"""
    macro undo_inert(x)
        x2 = JuliaLowering.macroexpand(x)
        x2[1]
    end
    """)
    @test JuliaLowering.include_string(test_mod, raw"""
    let foo = 1; @undo_inert(:foo); end
    """) == 1
    @test JuliaLowering.include_string(test_mod, raw"""
    let foo = 1; @undo_inert(@legacy_quote_to_syntax(:foo)); end
    """) == 1

    JuliaLowering.include_string(test_mod, raw"""
    macro mk_toplevel(x, y, z)
        JuliaSyntax.newnode(
            __context__.macrocall, K"toplevel",
            JuliaSyntax.SyntaxList(x, y, z))
    end
    macro toplevel_first_child(x)
        x2 = JuliaLowering.macroexpand(x)
        x2[1]
    end
    """)
    JuliaLowering.include_string(test_mod, raw"""
    macro mk_module(x, y, z)
        @legacy_quote_to_syntax :(module mk_module_mod; $x; $y; $z; end)
    end
    macro module_first_child(x)
        x2 = JuliaLowering.macroexpand(x)
        x2[end][1]
    end
    """)
    # sanity
    @test JuliaLowering.include_string(test_mod, """
    @mk_toplevel(1, :y, "z")
    """) == "z"
    @test JuliaLowering.include_string(test_mod, """
    @mk_module(1, :y, "z")
    """) isa Module
    @test JuliaLowering.include_string(test_mod, """
    let (x, y, z) = (1, :y, "z")
        @toplevel_first_child(@mk_toplevel(x, y, z))
    end
    """) == 1
    @test JuliaLowering.include_string(test_mod, """
    let (x, y, z) = (1, :y, "z")
        @toplevel_first_child(@mk_toplevel(x, y, z))
    end
    """) == 1

    # escape should obey quote/unquote
    jl_eval(test_mod, raw"""
    macro esc_in_quote(); Expr(:quote, Expr(:escape, :x)); end
    """; edition=JL_OLD_EDITION)
    @test jl_eval(test_mod, raw"""
    @esc_in_quote
    """; edition=JL_OLD_EDITION) == Expr(:escape, :x)
    @test JuliaLowering.include_string(test_mod, raw"""
    @esc_in_quote
    """) == Expr(:escape, :x)

    jl_eval(test_mod, raw"""
    macro esc_in_unquote(); Expr(:quote, Expr(:$, Expr(:escape, :x))); end
    """; edition=JL_OLD_EDITION)
    @test jl_eval(test_mod, raw"""
    let x = 1; @esc_in_unquote(); end
    """; edition=JL_OLD_EDITION) == 1
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = 1; @esc_in_unquote(); end
    """) == 1
end

JuliaLowering.include_string(test_mod, raw"""
module M
    using ..JuliaLowering: JuliaLowering, adopt_scope, @legacy_quote_to_syntax
    using ..JuliaSyntax

    # Introspection
    macro __MODULE__()
        JuliaLowering.syntax_module(__context__.macrocall)
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
        @legacy_quote_to_syntax :(begin
            x = "`x` from @foo"
            (x, someglobal, $ex)
        end)
    end

    # Set `a_global` in M
    macro set_a_global(val)
        @legacy_quote_to_syntax :(begin
            global a_global = $val
        end)
    end

    macro set_other_global(ex, val)
        @legacy_quote_to_syntax :(begin
            global $ex = $val
        end)
    end

    macro set_global_in_parent(ex)
        sym_ex = @legacy_quote_to_syntax quote; sym_introduced_from_M; end
        e1 = adopt_scope(__context__.macrocall, sym_ex[1])
        @legacy_quote_to_syntax quote
            $e1 = $ex
            nothing
        end
    end

    macro inner()
        @legacy_quote_to_syntax :(y, z)
    end

    macro outer()
        @legacy_quote_to_syntax :((x, @inner))
    end

    macro recursive(N)
        Nval = N.value::Int
        if Nval < 1
            return N
        end
        @legacy_quote_to_syntax quote
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
expanded = JuliaLowering.macroexpand(test_mod, ex)
@test JuliaSyntax.sourcetext.(JuliaLowering.flattened_provenance(expanded[2])) == [
    "M.@outer()"
    "@inner"
    "(y, z)"
]

@testset "expansion special case: macrocall in do expression" for edition in [JL_OLD_EDITION, JL_NEW_EDITION]
    @test jl_eval(test_mod, raw"""
    macro mac_called_in_do_expression(dofunc, arg)
        @legacy_quote_to_syntax :($dofunc($arg))
    end
    """; edition) isa Function
    @test jl_eval(test_mod, raw"""
    @mac_called_in_do_expression(9) do x
        x * 10
    end
    """; edition) == 90
    @test jl_eval(test_mod, raw"""
    let fp = @cfunction(Cint, (Cint,)) do x
            x + Cint(1)
        end
        ccall(fp isa Ptr ? fp : fp.ptr, Cint, (Cint,), 2)
    end
    """; edition) == 3
end

@test JuliaLowering.include_string(test_mod, raw"""
v"1.14"
""") isa VersionNumber
@test jl_eval(test_mod, raw"""
v"1.14"
""";edition=JL_OLD_EDITION) isa VersionNumber
@test JuliaLowering.include_string(test_mod, raw"""
Base.Experimental.@VERSION
""") isa NamedTuple
@test jl_eval(test_mod, raw"""
Base.Experimental.@VERSION
""";edition=JL_OLD_EDITION) isa NamedTuple

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

call_world_arg_test = JuliaLowering.rebase_layers(JuliaLowering.parsestmt(JuliaLowering.SyntaxTree, "@world_age_test()"), test_mod)
    @test JuliaLowering.expand_forms_1(call_world_arg_test, world1, true) ≈
        @ast_ 1::K"Value"
    @test JuliaLowering.expand_forms_1(call_world_arg_test, world2, true) ≈
        @ast_ 2::K"Value"

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
    # TODO: store this in DebugInfo
    @test_broken any(sf->sf.func===Symbol("@m_throw"), st)
    @test any(sf->sf.func===Symbol("macro expansion"), st)
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

@testset "(AI) macro name resolution" begin
    lib = @newmod(macname_lib, test_mod)
    def = @newmod(macname_def, test_mod)
    use = @newmod(macname_use, test_mod)
    for (m, s) in ((lib, :lib_inner), (def, :def_inner), (use, :use_inner))
        fl_eval(m, :(macro inner(); QuoteNode($(QuoteNode(s))); end))
    end
    fl_eval(lib, :(macro name(); Symbol("@inner"); end))
    fl_eval(lib, :(macro name_esc(); esc(Symbol("@inner")); end))
    fl_eval(def, :(import ..macname_lib: @name, @name_esc))
    fl_eval(def, :(const N = $lib))
    fl_eval(use, :(const M = $lib))
    fl_eval(def, quote
        macro only_in_def(); QuoteNode(:only_in_def); end
        macro outer_esc();      Expr(:macrocall, esc(Symbol("@inner")), __source__); end
        macro outer_esc_missing(); Expr(:macrocall, esc(Symbol("@only_in_def")), __source__); end
        macro outer_hyg();      Expr(:macrocall, Symbol("@inner"), __source__); end
        macro outer_esc_dot();  Expr(:macrocall, esc(Expr(:., :M, QuoteNode(Symbol("@inner")))), __source__); end
        macro outer_escl_dot(); Expr(:macrocall, Expr(:., esc(:M), QuoteNode(Symbol("@inner"))), __source__); end
        macro outer_hyg_dot();  Expr(:macrocall, Expr(:., :N, QuoteNode(Symbol("@inner"))), __source__); end
        macro outer_mc();       Expr(:macrocall, Expr(:macrocall, Symbol("@name"), __source__), __source__); end
        macro outer_mc_esc();   Expr(:macrocall, Expr(:macrocall, Symbol("@name_esc"), __source__), __source__); end
        macro outer_hs();       Expr(:macrocall, Expr(Symbol("hygienic-scope"), Symbol("@inner"), N), __source__); end
        macro outer_gr();       Expr(:macrocall, GlobalRef(N, Symbol("@inner")), __source__); end
        macro outer_val();      Expr(:macrocall, N.var"@inner", __source__); end
    end)

    # New-style expansions: caller-supplied identifier keeps the caller's layer
    jl_eval(def, raw"""
    macro outer_new_hyg();     @legacy_quote_to_syntax(:(@inner())); end
    macro outer_new_hyg_dot(); @legacy_quote_to_syntax(:(N.@inner())); end
    macro outer_new_dot(m);    @legacy_quote_to_syntax(:($m.@inner())); end
    """; edition=JL_NEW_EDITION)
    fl_eval(use, :(import ..macname_def: @outer_esc, @outer_esc_missing,
                   @outer_hyg, @outer_esc_dot, @outer_escl_dot, @outer_hyg_dot,
                   @outer_mc, @outer_mc_esc, @outer_hs, @outer_gr, @outer_val,
                   @outer_new_hyg, @outer_new_hyg_dot, @outer_new_dot))
    Core.@latestworld

    for edition in (JL_OLD_EDITION, JL_NEW_EDITION)
        jl(ex) = jl_eval(use, ex; edition)
        @test jl(:(@outer_esc())) === :use_inner
        err = try jl(:(@outer_esc_missing())); nothing; catch e; e; end
        @test err isa MacroExpansionError
        @test err.err isa UndefVarError && err.err.var === Symbol("@only_in_def")
        @test err.err.scope === use
        # An unescaped name resolves in the macro's module (hygiene)
        @test jl(:(@outer_hyg())) === :def_inner
        # Dotted names: the leftmost identifier resolves in its own layer,
        # whether that differs from the `.`'s layer (`escl_dot`) or not
        @test jl(:(@outer_esc_dot())) === :lib_inner
        @test jl(:(@outer_escl_dot())) === :lib_inner
        @test jl(:(@outer_hyg_dot())) === :lib_inner
        # A macrocall in name position: its output is hygienic in its macro's
        # module (`lib`), unless the output escapes the name (then `def`)
        @test jl(:(@outer_mc())) === :lib_inner
        @test jl(:(@outer_mc_esc())) === :def_inner
        # Explicit hygienic-scope, GlobalRef and function values in name position
        @test jl(:(@outer_hs())) === :lib_inner
        @test jl(:(@outer_gr())) === :lib_inner
        @test jl(:(@outer_val())) === :lib_inner
        # New-style macros
        @test jl(:(@outer_new_hyg())) === :def_inner
        @test jl(:(@outer_new_hyg_dot())) === :lib_inner
        @test jl(:(@outer_new_dot(M))) === :lib_inner
    end

    # flisp agrees on every case it can express
    fl(ex) = fl_eval(use, ex)
    @test fl(:(@outer_hyg())) === :def_inner
    @test fl(:(@outer_hyg_dot())) === :lib_inner
    @test fl(:(@outer_mc())) === :lib_inner
    @test fl(:(@outer_mc_esc())) === :def_inner
    @test fl(:(@outer_hs())) === :lib_inner
    @test fl(:(@outer_gr())) === :lib_inner
    @test fl(:(@outer_val())) === :lib_inner
    for ex in (:(@outer_esc()), :(@outer_esc_dot()), :(@outer_escl_dot()))
        err = try fl(ex); nothing; catch e; e; end
        err isa LoadError && (err = err.error)
        @test err isa ErrorException && occursin("used outside of macro expansion", err.msg)
    end
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

# Hygiene interop:
# call_oldstyle_macro -> oldstyle -> newstyle3
JuliaLowering.include_string(test_mod, raw"""
    macro call_oldstyle_macro(a)
        @legacy_quote_to_syntax quote
            x = "x in call_oldstyle_macro"
            @oldstyle $a x
        end
    end

    macro newstyle3(a, b, c)
        @legacy_quote_to_syntax quote
            x = "x in @newstyle3"
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
        @newstyle3 $(esc(a)) $(esc(b)) x
    end
end
))
@test JuliaLowering.include_string(test_mod, """
let x = "x in outer scope"
    @call_oldstyle_macro x
end
""") == ("x in call_oldstyle_macro",
         "x in call_oldstyle_macro",
         "x in @oldstyle",
         "x in @newstyle3")
# #  would be ideal, but we can't get hygiene through oldstyle
# ("x in outer scope",
#  "x in call_oldstyle_macro",
#  "x in @oldstyle",
#  "x in @newstyle3")

# Old style unhygienic escaping with esc()
Base.eval(test_mod, :(
macro oldstyle_unhygienic()
    esc(:x)
end
))
@test JuliaLowering.include_string(test_mod, """
let x = "x in outer scope"
    @oldstyle_unhygienic
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
LoadError: MacroExpansionError while expanding @oldstyle_error in module Main.macro_test:
@oldstyle_error
└─────────────┘ ── Error expanding macro
Caused by:
Some error in old style macro
in expression starting at string:1"""

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
    @test exc isa LoadError
    mexc = exc.error.err
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
    LoadError: MacroExpansionError while expanding @sig_mismatch in module Main.macro_test:
    @sig_mismatch(1, 2, 3, 4)
    └───────────────────────┘ ── Error expanding macro
    Caused by:
    MethodError: no method matching var"@sig_mismatch"(""")
end

@testset "old macros producing exotic expr heads (or are otherwise complex)" for edition in [JL_OLD_EDITION, JL_NEW_EDITION]
    @test jl_eval(test_mod, """
    let # example from @preserve docstring
        x = Ref{Int}(101)
        p = Base.unsafe_convert(Ptr{Int}, x)
        GC.@preserve x unsafe_load(p)
    end"""; edition) === 101 # Expr(:gc_preserve)

    # JuliaLowering.jl/issues/121
    @test JuliaLowering.include_string(test_mod, """
    GC.@preserve @static if true @__MODULE__ else end
    """) isa Module
    @test jl_eval(test_mod, """
    GC.@preserve @static if true v"1.14" else end
    """; edition) isa VersionNumber

    # JuliaLowering.jl/issues/144
    @test jl_eval(test_mod, """
    f_preserve144() = let
        val = Any[]
        GC.@preserve val begin; end
    end
    f_preserve144()
    """; edition) == nothing

    # JuliaLowering.jl/issues/145
    @test jl_eval(test_mod, """
    f_preserve145() = let
        debug_buffer = IOBuffer()
        # inside function to force compilation
        GC.@preserve debug_buffer 1
    end
    f_preserve145()
    """; edition) == 1

    # only invokelatest produces :isglobal now, so MWE here
    Base.eval(test_mod, :(macro isglobal(x); esc(Expr(:isglobal, x)); end))
    @test jl_eval(test_mod, """
    some_global = 1
    function isglobal_chk(some_arg)
       local some_local = 1
       (@isglobal(some_undefined), @isglobal(some_global), @isglobal(some_arg), @isglobal(some_local))
    end
    isglobal_chk(1)
    """; edition) === (true, true, false, false)
    # with K"Placeholder"s
    @test jl_eval(test_mod, """
    __ = 1
    function isglobal_chk(___)
       local ____ = 1
       (@isglobal(_), @isglobal(__), @isglobal(___), @isglobal(____))
    end
    isglobal_chk(1)
    """; edition) === (false, false, false, false)

    # @test appears to be the only macro in base to use :inert
    test_result = jl_eval(test_mod, """
    using Test
    @test identity(123) === 123
    """; edition)
    @test test_result.value === true

    # @enum produces Expr(:toplevel)
    jl_eval(test_mod, """
    @enum SOME_ENUM X1 X2 X3
    """; edition)
    Core.@latestworld
    @test test_mod.SOME_ENUM <: Enum
    @test test_mod.X1 isa Enum

    # @deprecate also produces Expr(:toplevel), and :public with expression
    # hygiene different from the contained names.
    @testset "@deprecate" begin
        @test jl_eval(test_mod, """
        module DeprecateMod
            d2(x) = x+1
            @deprecate d1(x) d2(0)
        end
        """; edition) isa Module
        Core.@latestworld
        @test isdefined(test_mod.DeprecateMod, :d2)
        @test isdefined(test_mod.DeprecateMod, :d1)
        @test Base.isexported(test_mod.DeprecateMod, :d1)
        @test !Base.isexported(test_mod, :d1)
    end

    # @testset produces :tryfinally with secret third arg
    @eval test_mod :(using Test)
    @test JuliaLowering.include_string(test_mod, "@test true") isa Test.Pass
    @testset let jltestset = jl_eval(test_mod, """
    @testset begin
        @test true
    end
    """; edition)
        @test jltestset isa Test.AbstractTestSet
        @test jltestset.n_passed == 1
    end

    # aliasscope
    @test jl_eval(
        test_mod,
        :(function simple_aliasscope(A, B)
              Base.Experimental.@aliasscope @inbounds for I in eachindex(A, B)
                  A[I] = Base.Experimental.Const(B)[I]
              end
              return 0
          end); edition) isa Function
    @test jl_eval(
        test_mod,
        :(let A = [1,2,3], B = [4,5,6]
              simple_aliasscope(A,B), A, B
          end); edition) == (0, [4,5,6], [4,5,6])

    @test JuliaLowering.include_string(test_mod, """@fastmath 1 <= 2 <= 3""")

    # top-level nospecialize
    @test JuliaLowering.include_string(test_mod, """
    module NospecializeMod
        @nospecialize
        f(x) = x
    end
    first(methods(NospecializeMod.f)).nospecialize
    """) == -1
end

@testset "empty meta" begin
    @test fl_eval(test_mod, Expr(:meta)) == nothing
    @test fl_eval(test_mod, Expr(:block, Expr(:meta))) == nothing
    @test fl_eval(test_mod, Expr(:call,
                                 Expr(:function, Expr(:call, :func_empty_meta),
                                      Expr(:block, Expr(:meta))))) == nothing
    @test jl_eval(test_mod, Expr(:meta); edition=JL_NEW_EDITION) == nothing
    @test jl_eval(test_mod, Expr(:block, Expr(:meta)); edition=JL_NEW_EDITION) == nothing
    @test jl_eval(test_mod, Expr(:call,
                                 Expr(:function, Expr(:call, :func_empty_meta),
                                      Expr(:block, Expr(:meta)))); edition=JL_NEW_EDITION) == nothing
end

@testset "macros producing meta forms" for edition in [JL_OLD_EDITION, JL_NEW_EDITION]
    function find_method_ci(thunk)
        ci = thunk.args[1]::Core.CodeInfo
        m = findfirst(ci.code) do x
            x isa Expr && x.head === :call && length(x.args) >= 5 &&
                x.args[1] isa GlobalRef && x.args[1].name === :define_method
        end
        ci.code[m].args[5]
    end
    jlower_e(s) = JuliaLowering.to_lowered_expr(
        jl_lower(
            test_mod, JuliaLowering.parsestmt(
                JuliaLowering.SyntaxTree, s);
            edition))

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

    prog = "Base.@assume_effects :total @inline function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).inlining === find_method_ci(our).inlining
    @test find_method_ci(ref).purity === find_method_ci(our).purity

    prog = "Base.@assume_effects :consistent Base.@assume_effects :nothrow function foo(); end"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).purity === find_method_ci(our).purity

    prog = "Base.@pure @inline foo(x) = x + 1"
    ref = Meta.lower(test_mod, Meta.parse(prog))
    our = jlower_e(prog)
    @test find_method_ci(ref).purity === find_method_ci(our).purity
    @test find_method_ci(ref).inlining === find_method_ci(our).inlining

    # TODO: no api for option retrieval, just check that it compiles
    let options_mod = Module()
        @test fl_eval(options_mod, :(Base.Experimental.@optlevel 1)) == nothing
        @test jl_eval(options_mod, :(Base.Experimental.@optlevel 1); edition=JL_NEW_EDITION) == nothing
        @test fl_eval(options_mod, :(Base.Experimental.@max_methods 1)) == nothing
        @test jl_eval(options_mod, :(Base.Experimental.@max_methods 1); edition=JL_NEW_EDITION) == nothing
    end
end

# partially robot-generated
@testset "meta-like forms not using the `meta` expression" for edition in (JL_OLD_EDITION, JL_NEW_EDITION)
    @testset "in value position" begin
        @test fl_eval(test_mod, Expr(:boundscheck)) isa Bool
        @test jl_eval(test_mod, Expr(:boundscheck); edition) isa Bool

        @test fl_eval(test_mod, Expr(:inbounds, true)) === nothing
        @test fl_eval(test_mod, Expr(:inbounds, false)) === nothing
        @test fl_eval(test_mod, Expr(:inbounds, :pop)) === nothing
        @test jl_eval(test_mod, Expr(:inbounds, true); edition) === nothing
        @test jl_eval(test_mod, Expr(:inbounds, false); edition) === nothing
        @test jl_eval(test_mod, Expr(:inbounds, :pop); edition) === nothing

        @testset for inline in (:inline, :noinline)
            @testset let ex = Expr(:block,
                                   Expr(inline, true),
                                   Expr(inline, false))
                @test fl_eval(test_mod, ex) === nothing
                @test jl_eval(test_mod, ex; edition) === nothing
            end
            @testset let ex = Expr(:function, Expr(:tuple),
                                   Expr(:block,
                                        Expr(inline, true),
                                        Expr(inline, false)))
                local f
                f = fl_eval(test_mod, ex)
                Core.@latestworld
                @test f() === nothing

                f = jl_eval(test_mod, ex; edition)
                Core.@latestworld
                @test f() === nothing
            end
        end
    end

    function find_method_ci(thunk)
        ci = thunk.args[1]::Core.CodeInfo
        m = findfirst(ci.code) do x
            x isa Expr && x.head === :call && length(x.args) >= 5 &&
                x.args[1] isa GlobalRef && x.args[1].name === :define_method
        end
        ci.code[m].args[5]
    end
    jlower_e(s) = JuliaLowering.to_lowered_expr(
        jl_lower(
            test_mod, JuliaLowering.parsestmt(
                JuliaLowering.SyntaxTree, s);
            edition))
    our_ssaflags(prog) = find_method_ci(jlower_e(prog)).ssaflags

    local INBOUNDS = Core.Compiler.IR_FLAG_INBOUNDS
    local INLINE   = Core.Compiler.IR_FLAG_INLINE
    local NOINLINE = Core.Compiler.IR_FLAG_NOINLINE

    # `compute_ssaflags` shifts the encoded purity overrides up by NUM_IR_FLAGS.
    purity_mask(eo::Base.EffectsOverride) =
        UInt32(Base.encode_effects_override(eo)) << Core.Compiler.NUM_IR_FLAGS

    # check any IR statement in `prog` has `flags`
    has_any(prog, flags) = any(f -> (f & flags) == flags, our_ssaflags(prog))
    has_none(prog, flags) = all(f -> (f & flags) == 0,    our_ssaflags(prog))

    @testset "boundscheck" begin
        jl_eval(test_mod, """
        @inline function g_boundscheck(A, i)
            @boundscheck checkbounds(A, i)
            return A[i]
        end
        """; edition)
        @test test_mod.g_boundscheck(1:2, 2) == 2
        @test_throws BoundsError test_mod.g_boundscheck(1:2, 3)

        # The boundscheck marker itself does not set IR_FLAG_INBOUNDS — it is
        # a separate runtime predicate, not an annotation.
        @test has_none("function f(A,i); @boundscheck checkbounds(A,i); A[i]; end",
                       INBOUNDS)
        # `Expr(:boundscheck)` should survive lowering as a top-level
        # statement (it gets rewritten by inlining/codegen, not lowering).
        let our = find_method_ci(jlower_e(
                "function f(A,i); @boundscheck checkbounds(A,i); A[i]; end"))
            @test any(s -> s isa Expr && s.head === :boundscheck, our.code)
        end
    end

    @testset "inbounds" begin
        jl_eval(test_mod, """
        function sum_inbounds(A::AbstractArray)
            r = zero(eltype(A))
            for i in eachindex(A)
                @inbounds r += A[i]
            end
            return r
        end
        """; edition)
        @test test_mod.sum_inbounds([1,2,3]) == 6

        @test has_none("function f(A,i); A[i]; end", INBOUNDS)
        @test has_any("function f(A,i); @inbounds A[i]; end", INBOUNDS)
        @test has_any("""
            function f(A)
                s = zero(eltype(A))
                @inbounds for i in eachindex(A)
                    s += A[i]
                end
                s
            end
        """, INBOUNDS)
        let flags = our_ssaflags("""
                function f(A, i, j)
                    z = @inbounds A[i]
                    A[j]
                end
            """)
            @test any(f -> (f & INBOUNDS) != 0, flags)  # inside @inbounds
            @test any(f -> (f & INBOUNDS) == 0, flags)  # outside
        end
    end

    @testset "inline" begin
        @test has_any("function f(g,x); @inline g(x); end", INLINE)
        @test has_none("function f(g,x); g(x); end", INLINE)
        @test has_none("function f(g,x); @inline g(x); end", NOINLINE)
        @test has_any("function f(g,x); @inline g(x) + g(x); end", INLINE)

        # Bare `@inline` inside a function body (1.8+) emits
        # `Expr(:meta, :inline)`; no statement gets a call-site IR_FLAG_INLINE.
        jl_eval(test_mod, """
        function bare_inline(x)
            @inline
            x * 2
        end
        """; edition)
        @test test_mod.bare_inline(3) == 6
        @test has_none("function f(x); @inline; x * 2; end", INLINE)

        # `@inline` on a definition is handled by the meta-expression path
        # (covered in "macros producing meta forms"); confirm it still runs
        # and that no call-site INLINE bit leaks into the body.
        jl_eval(test_mod, """
        @inline f_inline_def(x) = x + 1
        """; edition)
        @test test_mod.f_inline_def(2) == 3
        @test has_none("@inline f(x) = x + 1", INLINE)
    end

    @testset "noinline" begin
        # Analogous to `@inline` but pushes IR_FLAG_NOINLINE.
        @test has_any("function f(g,x); @noinline g(x); end", NOINLINE)
        @test has_none("function f(g,x); g(x); end", NOINLINE)
        @test has_none("function f(g,x); @noinline g(x); end", INLINE)
        @test has_any("function f(g,x); @noinline g(x) + g(x); end", NOINLINE)

        jl_eval(test_mod, """
        function bare_noinline(x)
            @noinline
            x * 2
        end
        """; edition)
        @test test_mod.bare_noinline(3) == 6
        @test has_none("function f(x); @noinline; x * 2; end", NOINLINE)

        jl_eval(test_mod, """
        @noinline f_noinline_def(x) = x + 1
        """; edition)
        @test test_mod.f_noinline_def(2) == 3

        # Innermost annotation wins when @inline / @noinline nest: the inner
        # call gets IR_FLAG_INLINE; the outer @noinline still applies to
        # statements outside the inner region.
        let flags = our_ssaflags("""
                function f(g, x)
                    @noinline let
                        a = @inline g(x)
                        b = g(x)
                        (a, b)
                    end
                end
            """)
            @test any(f -> (f & INLINE)   != 0, flags)
            @test any(f -> (f & NOINLINE) != 0, flags)
        end
    end

    @testset "purity" begin
        # Sanity: plain function with no purity annotation has no purity bits set.
        @test has_none("function f(g,x); g(x); end",
                       UInt32(0xFFFF) << Core.Compiler.NUM_IR_FLAGS)
        # `@assume_effects :foo expr` at a call site expands to
        #   (block (purity ...11 bool args...) (local (= val expr)) (purity) val)
        # where the trailing zero-arg `(purity)` is the region-end token.
        @test has_any("function f(g,x); Base.@assume_effects :nothrow g(x); end",
                      purity_mask(Base.EffectsOverride(nothrow=true)))
        # Multiple atomic settings combine to set both bits at once.
        @test has_any(
            "function f(g,x); Base.@assume_effects :consistent :effect_free g(x); end",
            purity_mask(Base.EffectsOverride(consistent=true, effect_free=true)))

        # Function form goes through a different path: `(meta (purity args...))`
        jl_eval(test_mod, """
        Base.@assume_effects :total f_assume_def(x) = x
        """; edition)
        @test test_mod.f_assume_def(5) == 5
        prog_def = "Base.@assume_effects :total function f_assume_total(x); x; end"
        ref_ci = find_method_ci(Meta.lower(test_mod, Meta.parse(prog_def)))
        our_ci = find_method_ci(jlower_e(prog_def))
        @test ref_ci.purity === our_ci.purity

        prog = """
        Base.@assume_effects :total function f_assume_nospecialize(x)
            @nospecialize x
            x
        end
        """
        ref_ci = find_method_ci(fl_lower(test_mod, Meta.parse(prog)))
        our_ci = find_method_ci(jlower_e(prog))
        @test ref_ci.purity === our_ci.purity
    end
end

@testset "scope layers for normally-inert ASTs" begin
    # Right hand side of `.`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = @legacy_quote_to_syntax :(hi)
        @legacy_quote_to_syntax :(A.$x)
    end
    """) ≈ @ast_ [K"."
        "A"::K"Identifier"
        [K"inert" "hi"::K"Identifier"]
    ]
    # module
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = @legacy_quote_to_syntax :(AA)
        @legacy_quote_to_syntax :(module $x end)
    end
    """) ≈ @ast_ [K"module"
        JL_NEW_EDITION::K"Value"
        true::K"Value"
        "AA"::K"Identifier"
        [K"block"]
    ]

    # In macro expansion, require that expressions passed in as macro
    # *arguments* get the lexical scope of the calling context, even for the
    # `x` in `M.$x` where the right hand side of `.` is normally quoted.
    @test jl_eval(test_mod, raw"""
        let x = @legacy_quote_to_syntax :(someglobal)
            @eval M.$x
        end
    """; edition=JL_NEW_EDITION) == "global in module M"
    @test jl_eval(test_mod, raw"""
        let x = @legacy_quote_to_syntax :(someglobal)
            @eval M.$x
        end
    """; edition=JL_OLD_EDITION) == "global in module M"

    # @eval quoting should embed the value, not the syntax
    @test jl_eval(test_mod, raw"""
        let some_local = 101
            @eval module AA
                x = $some_local
            end
        end
    """; edition=JL_NEW_EDITION) isa Module
    @test test_mod.AA.x == 101
    @test jl_eval(test_mod, raw"""
        let some_local = 101
            @eval module AA
                x = $some_local
            end
        end
    """; edition=JL_OLD_EDITION) isa Module
    @test test_mod.AA.x == 101

    # "Deferred hygiene" in macros which emit quoted code.  OK to break
    #
    # The old macro system doesn't handle this - here's the equivalent
    # implementation
    # macro make_quoted_code(init, y)
    #     QuoteNode(:(let
    #         x = "inner x"
    #         $(esc(init))
    #         ($(esc(y)), x)
    #     end))
    # end
    JuliaLowering.include_string(test_mod, raw"""
    macro make_quoted_code(init, y)
        q = @legacy_quote_to_syntax :(let
            x = "inner x"
            $init
            ($y, x)
        end)
        @ast _ q [K"syntaxinert" q]
    end
    """)
    code = JuliaLowering.include_string(test_mod, """@make_quoted_code(x="outer x", x)""")
    @test JuliaLowering.eval(test_mod, code) == ("outer x", "inner x")
end

@testset "toplevel macro hygiene" for run in [JuliaLowering.include_string,
                                              Base.include_string]
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
    Core.@latestworld
    @test run(test_mod, "MacroMod.@escaped_toplevel") === test_mod
    @test run(test_mod, "MacroMod.@inner_escaped_toplevel") === test_mod
    @test run(test_mod, "MacroMod.@unescaped_toplevel") === test_mod.MacroMod

    unrelated = @newmod(unrelated)
    @eval unrelated const MacroMod = $(test_mod.MacroMod)
    @eval unrelated global mod = 123

    @test run(unrelated, "MacroMod.@escaped_toplevel") == 123
    @test run(unrelated, "MacroMod.@inner_escaped_toplevel") == 123
    @test run(unrelated, "MacroMod.@unescaped_toplevel") === test_mod.MacroMod
end

@testset "toplevel macro hygiene: @__MODULE__" for run in [JuliaLowering.include_string,
                                                           Base.include_string]
    @eval test_mod module MacroMod
    macro atmodule_in_toplevel()
        Expr(:toplevel, :(@__MODULE__))
    end
    macro atmodule_in_module()
        Expr(:toplevel, Expr(:module, true, esc(:atmod_mod), Expr(
            :block, :(global global_mod = @__MODULE__))))
    end
    end
    Core.@latestworld
    @test run(test_mod, "MacroMod.@atmodule_in_toplevel") === test_mod
    @test run(test_mod, "MacroMod.@atmodule_in_module") isa Module
    Core.@latestworld
    @test isdefined(test_mod, :atmod_mod)
    @test test_mod.atmod_mod.global_mod == test_mod.atmod_mod
end

# JuliaLang/JuliaLowering.jl#120
#
# `__module__` should be expanded as the lexical module containing the expanded
# code, not the module corresponding to the current hygienic scope
JuliaLowering.include_string(test_mod, raw"""
module Mod1
import ..JuliaLowering.@legacy_quote_to_syntax
macro indirect_MODULE()
    return @legacy_quote_to_syntax :(@__MODULE__())
end
end
""")
code = JuliaLowering.include_string(test_mod, """Mod1.@indirect_MODULE()""")
@test JuliaLowering.eval(test_mod, code) === test_mod # !== test_mod.Mod1
# the lowering/eval iterator needs to expand in the correct world age (currently
# the only way to hit this from user code is macros producing toplevel)

@testset "old macros defining modules" begin
    # escaped module nested in tmpmod_1
    jl_eval(test_mod, :(
        module MacMod
        macro makemod(name)
            Expr(:toplevel,
                 esc(Expr(:module, false, :tmpmod_1,
                          Expr(:block,
                               Expr(:module, false, name,
                                    Expr(:block, Expr(:const, Expr(:(=), :c, 1))))))))
        end
        end); edition=JL_OLD_EDITION)

    @testset for edition in [JL_OLD_EDITION, JL_NEW_EDITION]
        @test JuliaLowering.include_string(
            test_mod, "MacMod.@makemod(newmod)") isa Module
        Core.@latestworld
        # module name should escape macmod->test_mod
        @test test_mod.tmpmod_1.newmod isa Module
        @test !isdefined(test_mod.MacMod, :newmod)
        @test !isdefined(test_mod.MacMod, :tmpmod_1)
        # const in mod body should work
        @test test_mod.tmpmod_1.newmod.c == 1
    end

    # escaped module name
    jl_eval(test_mod, :(
        module MacMod
        macro makemod(name)
            Expr(:toplevel,
                 Expr(:module, false, esc(name),
                      Expr(:block,
                           Expr(:const, Expr(:(=), esc(:c), 1)))))
        end
        end); edition=JL_OLD_EDITION)

    @testset for edition in [JL_OLD_EDITION, JL_NEW_EDITION]
        @test JuliaLowering.include_string(
            test_mod, "MacMod.@makemod(newmod)") isa Module
        Core.@latestworld
        # module name should escape macmod->test_mod
        @test test_mod.newmod isa Module
        @test !isdefined(test_mod.MacMod, :newmod)
        @test test_mod.newmod.c == 1
    end
end

@testset "(AI) old macro attribution survives a nested eval in its body (#32)" begin
    Base.eval(test_mod, :(module MacDefMod
        const secret = 99
        macro getsecret()
            __module__.eval(:(nested_eval_side_effect = 1 + 1))
            return :(secret)   # bare name -> resolves in the defining module
        end
    end))
    Core.@latestworld
    # `secret` must resolve in MacDefMod (== mod_for_ast), matching flisp.
    @test JuliaLowering.include_string(test_mod, "MacDefMod.@getsecret()") == 99
    @test test_mod.nested_eval_side_effect == 2
    @test fl_eval(test_mod, :(MacDefMod.@getsecret())) == 99
end

@testset "macros defining macros" begin
    @eval test_mod macro make_and_use_macro_toplevel()
        Expr(:toplevel,
             esc(:(macro from_toplevel_expansion()
                   :(123)
               end)),
             esc(:(@from_toplevel_expansion())))
    end

    @test jl_eval(
        test_mod, "@make_and_use_macro_toplevel()"; edition=JL_OLD_EDITION) === 123

    if isdefined(test_mod, Symbol("@from_toplevel_expansion"))
        Base.delete_binding(test_mod, Symbol("@from_toplevel_expansion"))
    end

    @test jl_eval(
        test_mod, "@make_and_use_macro_toplevel()"; edition=JL_NEW_EDITION) === 123

    # unescaped top-level macro should be visible in the old system
    @test jl_eval(test_mod, raw"""
    module MacrosDefiningMacros
    macro make_old_unescaped_macro(name)
        :(macro $name(x)
            x
        end)
    end
    macro make_old_escaped_macro(name)
        :(macro $(esc(name))(x)
            x
        end)
    end
    end
    """; edition=JL_OLD_EDITION) isa Module
    @test jl_eval(test_mod, raw"""
    MacrosDefiningMacros.@make_old_unescaped_macro make_old_unescaped_macro_out
    """; edition=JL_OLD_EDITION) isa Function
    @test jl_eval(test_mod, raw"""
    MacrosDefiningMacros.@make_old_escaped_macro make_old_escaped_macro_out
    """; edition=JL_OLD_EDITION) isa Function
    @test jl_eval(test_mod, raw"""
    MacrosDefiningMacros.@make_old_unescaped_macro_out 1
    """; edition=JL_OLD_EDITION) == 1
    @test jl_eval(test_mod, raw"""
    @make_old_escaped_macro_out 1
    """; edition=JL_OLD_EDITION) == 1

    # standard scope-layer-carrying macro arg `name` should work in new system
    @test jl_eval(test_mod, raw"""
    macro make_new_nameprovided_macro(name)
        @legacy_quote_to_syntax :(macro $name(x)
            x
        end)
    end
    @make_new_nameprovided_macro make_new_nameprovided_macro_out
    @make_new_nameprovided_macro_out 1
    """; edition=JL_NEW_EDITION) == 1

    # unhygienic new macro def shouldn't be visible (may change)
    @test jl_eval(test_mod, raw"""
    macro make_new_anaphoric_macro_fail()
        @legacy_quote_to_syntax :(macro make_new_anaphoric_macro_fail_out(x)
            x
        end)
    end
    @make_new_anaphoric_macro_fail
    """; edition=JL_NEW_EDITION) isa Function
    @test_throws MacroExpansionError jl_eval(
        test_mod, "@make_new_anaphoric_macro_fail_out"; edition=JL_NEW_EDITION)
end

@testset "SIMD loopinfo" begin
    @test jl_eval(test_mod, raw"""
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
    """; edition=JL_OLD_EDITION) == 10.0

    @test jl_eval(test_mod, raw"""
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
    """; edition=JL_OLD_EDITION) == 10.0

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

@testset "@__FUNCTION__ and Expr(:thisfunction)" begin
    @testset "Basic usage" begin
        # @__FUNCTION__ in regular functions
        jl_eval(test_mod, raw"""
        test_function_basic() = @__FUNCTION__
        """; edition=JL_OLD_EDITION)
        @test test_mod.test_function_basic() === test_mod.test_function_basic

        # Expr(:thisfunction) in regular functions
        jl_eval(test_mod, raw"""
            @eval regular_func() = @__FUNCTION__
        """; edition=JL_OLD_EDITION)
        @test test_mod.regular_func() === test_mod.regular_func
    end

    @testset "Recursion" begin
        # Factorial with @__FUNCTION__
        jl_eval(test_mod, raw"""
        factorial_function(n) = n <= 1 ? 1 : n * (@__FUNCTION__)(n - 1)
        """; edition=JL_OLD_EDITION)
        @test test_mod.factorial_function(5) == 120

        # Fibonacci with Expr(:thisfunction)
        jl_eval(test_mod, raw"""
        struct RecursiveCallableStruct; end
        (::RecursiveCallableStruct)(n) = n <= 1 ? n : @__FUNCTION__()(n-1) + @__FUNCTION__()(n-2)
        """; edition=JL_OLD_EDITION)
        @test test_mod.RecursiveCallableStruct()(10) === 55

        # Anonymous function recursion
        @test jl_eval(test_mod, raw"""
        (n -> n <= 1 ? 1 : n * (@__FUNCTION__)(n - 1))(5)
        """; edition=JL_OLD_EDITION) == 120
    end

    @testset "Closures and nested functions" begin
        # Prevents boxed closures
        jl_eval(test_mod, raw"""
        function make_closure()
            fib(n) = n <= 1 ? 1 : (@__FUNCTION__)(n - 1) + (@__FUNCTION__)(n - 2)
            return fib
        end
        """; edition=JL_OLD_EDITION)
        Test.@inferred test_mod.make_closure()
        closure = test_mod.make_closure()
        @test closure(5) == 8
        Test.@inferred closure(5)

        # Complex closure of closures
        jl_eval(test_mod, raw"""
        function f1()
            function f2()
                function f3()
                    return @__FUNCTION__
                end
                return (@__FUNCTION__), f3()
            end
            return (@__FUNCTION__), f2()...
        end
        """; edition=JL_OLD_EDITION)
        Test.@inferred test_mod.f1()
        @test test_mod.f1()[1] === test_mod.f1
        @test test_mod.f1()[2] !== test_mod.f1
        @test test_mod.f1()[3] !== test_mod.f1
        @test test_mod.f1()[3]() === test_mod.f1()[3]
        @test test_mod.f1()[2]()[2]() === test_mod.f1()[3]
    end

    @testset "Do blocks" begin
        function test_do_block()
            result = jl_eval(test_mod, raw"""
            map([1, 2, 3]) do x
                return (@__FUNCTION__, x)
            end
            """; edition=JL_OLD_EDITION)
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
        jl_eval(test_mod, raw"""
        f_thisfunction_kw(; n) = n <= 1 ? 1 : n * (@__FUNCTION__)(; n = n - 1)
        """; edition=JL_OLD_EDITION)
        @test test_mod.f_thisfunction_kw(n = 5) == 120

        # Expr(:thisfunction) with kwargs
        jl_eval(test_mod, raw"""
        f_thisfunction_kw2(; n=1) = n <= 1 ? n : n * @__FUNCTION__()(; n=n-1)
        """; edition=JL_OLD_EDITION)
        result = test_mod.f_thisfunction_kw2(n=5)
        @test result == 120
    end

    @testset "Callable structs" begin
        # @__FUNCTION__ in callable structs
        jl_eval(test_mod, raw"""
        module A
            struct CallableStruct{T}; val::T; end
            (c::CallableStruct)() = @__FUNCTION__
        end
        """; edition=JL_OLD_EDITION)
        jl_eval(test_mod, raw"""
        using .A: CallableStruct
        """; edition=JL_OLD_EDITION)
        c = test_mod.CallableStruct(5)
        @test c() === c

        # In closures, var"#self#" should refer to the enclosing function,
        # NOT the enclosing struct instance
        jl_eval(test_mod, raw"""
        struct CallableStruct2; end
        @eval function (obj::CallableStruct2)()
            function inner_func()
                @__FUNCTION__
            end
            inner_func
        end
        """; edition=JL_OLD_EDITION)

        let cs = test_mod.CallableStruct2()
            @test cs()() === cs()
            @test cs()() !== cs
        end

        # Accessing values via self-reference
        jl_eval(test_mod, raw"""
        struct CallableStruct3
            value::Int
        end
        (obj::CallableStruct3)() = @__FUNCTION__()
        (obj::CallableStruct3)(x) = @__FUNCTION__().value + x
        """; edition=JL_OLD_EDITION)

        let cs = test_mod.CallableStruct3(42)
            @test cs() === cs
            @test cs(10) === 52
        end

        # Callable struct with args and kwargs
        jl_eval(test_mod, raw"""
        struct CallableStruct4
        end
        @eval function (obj::CallableStruct4)(x, args...; y=2, kws...)
            return (; func=(@__FUNCTION__), x, args, y, kws)
        end
        """; edition=JL_OLD_EDITION)
        c = test_mod.CallableStruct4()
        @test c(1).func === c
        @test c(2, 3).args == (3,)
        @test c(2; y=4).y == 4
        @test c(2; y=4, a=5, b=6, c=7).kws[:c] == 7
    end

    @testset "Special cases" begin
        # Generated functions
        jl_eval(test_mod, raw"""
        let
            @generated foo2() = @__FUNCTION__
            foo2() === foo2
        end
        """; edition=JL_OLD_EDITION)

        # Struct constructors
        let
            jl_eval(test_mod, raw"""
            struct Cols{T<:Tuple}
                cols::T
                operator
                Cols(args...; operator=union) = (new{typeof(args)}(args, operator); string(@__FUNCTION__))
            end
            """; edition=JL_OLD_EDITION)
            result = @invokelatest test_mod.Cols(1, 2, 3)
            @test occursin("Cols", result)
        end

        # Should not access arg-map for local variables
        # TODO: worth the special case?
        jl_eval(test_mod, raw"""
            function f_thisfunction_argmap end
            function (f_thisfunction_argmap::typeof(f_thisfunction_argmap))()
                f_thisfunction_argmap = 1
                @__FUNCTION__
            end
        """; edition=JL_OLD_EDITION)
        @test_broken test_mod.f_thisfunction_argmap() ===
            test_mod.f_thisfunction_argmap
    end

    @test jl_eval(test_mod, """
        @eval let f=[ ()->$(Expr(:thisfunction)) for i = 1:1 ][1]; f() === f; end
    """; edition=JL_OLD_EDITION)
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

@testset "macro QuoteNode + inert behavior" begin
    Base.include_string(test_mod, raw"""
    macro quoted_gr()
        QuoteNode(GlobalRef(Base, :dontresolveme))
    end
    """)
    let gr = JuliaLowering.include_string(test_mod, "@quoted_gr")
        @test gr.mod === Base
        @test gr.name === :dontresolveme
    end
end

@testset "Base macros" begin
    jl_eval(test_mod,
            :(function test_invokelatest()
                  @eval invokelatest_target(x, y) = x + y
                  out = @invokelatest(invokelatest_target(1, 2))
                  Base.delete_binding(@__MODULE__, :invokelatest_target)
                  out
              end); edition=JL_NEW_EDITION)
    # the following test needs to define this to be effective
    @test_throws UndefVarError JuliaLowering.include_string(test_mod, "invokelatest_target(1,2)")
    @test JuliaLowering.include_string(test_mod, "test_invokelatest()") === 3

    for edition in (OLDEST_EDITION, JL_OLD_EDITION)
        _version = jl_eval(test_mod,
            "Base.Experimental.@VERSION";
            edition
        )
        @test _version isa NamedTuple
        @test _version.syntax == edition
    end
end

# produces import/using in module that is `@eval`ed.
@testset "safetestset" begin
    macro_mod = @newmod(macro_mod, test_mod)
    jl_eval(macro_mod, raw"""
    macro safetestset(testname, expr)
        quote
            @eval module $(gensym("safetestset_mod"))
            using Test
            @testset $testname $expr
            end
            nothing
        end
    end
    """; edition=JL_OLD_EDITION)

    jl_eval(test_mod, """
    macro_mod.@safetestset "Tests" begin
        a = 1; b = 2; c = a + b; @test c == 3
        @isdefined(a) == true
    end
    """; edition=JL_OLD_EDITION)
    @test !isdefined(test_mod, :a)
    @test !isdefined(macro_mod, :a)
end

# Method annotations propagate from the body to positional-default wrappers
# and the `Core.kwcall` sorter (matching flisp's `propagate-method-meta`).
@testset "method meta propagation" begin
    JuliaLowering.include_string(test_mod, raw"""
    @inline function _test_opts_kw(x::Int, y::Int=1; k::Int=2)
        x + y + k
    end
    """)
    fkw = test_mod._test_opts_kw
    @test fkw(1) == 4
    for m in (which(fkw, (Int,)), which(fkw, (Int, Int)),
              which(Core.kwcall, (NamedTuple{(:k,), Tuple{Int}}, typeof(fkw), Int)))
        @test Base.uncompressed_ast(m).inlining == 0x01
    end

    # Destructuring prepends assignments but must retain the metadata.
    JuliaLowering.include_string(test_mod, raw"""
    @inline function _test_opts_destr((a, b)::Tuple{Int,Int}, y::Int=1; k::Int=2)
        a + b + y + k
    end
    """)
    fd = test_mod._test_opts_destr
    @test fd((1, 2)) == 6
    for m in (which(fd, (Tuple{Int,Int},)), which(fd, (Tuple{Int,Int}, Int)),
              which(Core.kwcall, (NamedTuple{(:k,), Tuple{Int}}, typeof(fd), Tuple{Int,Int})))
        @test Base.uncompressed_ast(m).inlining == 0x01
    end

    # @nospecializeinfer
    local f = JuliaLowering.include_string(@newmod(), raw"""
    Base.@nospecializeinfer function f(@nospecialize(x), y::Int=1)
        (x, y)
    end
    """)
    @test which(f, (Any, Int)).nospecializeinfer == true
    @test which(f, (Any,)).nospecializeinfer == true

    local f = JuliaLowering.include_string(@newmod(), raw"""
    Base.@nospecializeinfer function f(@nospecialize(x); k::Int=1)
        (x, k)
    end
    """)
    local sorter = x->which(Core.kwcall, (NamedTuple{(:k,), Tuple{Int}}, typeof(x), Any))
    @test sorter(f).nospecializeinfer == true
end
