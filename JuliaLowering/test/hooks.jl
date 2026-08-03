@testset "hooks" begin
    test_mod = Module()

    @testset "`core_lowering_hook`" begin
        # Non-AST types are often sent through lowering
        stuff = Any[LineNumberNode(1), 123, 123.123, true, "foo", test_mod]
        for s in stuff
            @test JL.core_lowering_hook(s, test_mod) == Core.svec(s)
        end

        for ast_type in (Expr, JL.SyntaxTree)
            ex = parsestmt(ast_type, "[1,2,3] .+= 1")
            out = JL.core_lowering_hook(ex, test_mod)
            @test out isa Core.SimpleVector && out[1] isa Expr
            val = Core.eval(test_mod, out[1])
            @test val == [2,3,4]
        end

        # file argument mismatch with embedded linenumbernodes shouldn't crash
        ex = Expr(:block, LineNumberNode(111), :(x = 1), LineNumberNode(222), :(x + 1))
        lwr = JuliaLowering.core_lowering_hook(ex, test_mod, "foo.jl", 333)[1]
        @test Core.eval(test_mod, lwr) === 2
    end

    function jeval(str)
        prog = parseall(Expr, str)
        try
            JL.activate!()
            return Core.eval(test_mod, prog)
        finally
            JL.activate!(false)
        end
    end
    @testset "integration: `JuliaLowering.activate!`" begin
        out = jeval("global asdf = 1")
        @test out === 1
        @test isdefined(test_mod, :asdf)

        out = jeval("module M; x = 1; end")
        @test out isa Module
        @test isdefined(test_mod, :M)
        @test isdefined(test_mod.M, :x)

        @test jeval("@ccall jl_value_ptr(nothing::Any)::Ptr{Cvoid}") isa Ptr{Cvoid}

        # Tricky cases with symbols
        out = jeval("""module M2
                Base.@constprop :aggressive function f(x); x; end
                const what = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), Core.nothing)
            end""")
        @test out isa Module
        @test isdefined(test_mod, :M2)
        @test isdefined(test_mod.M2, :f)
        @test isdefined(test_mod.M2, :what)

        out = jeval(""" "docstring" module M3 end """)
        @test out isa Module
        @test isdefined(test_mod, :M3)

        # Macros may produce toplevel expressions.  Note that julia handles
        # this case badly (macro expansion replaces M5_inner with a
        # globalref) and we handle esc(:M5_inner) badly
        out = jeval("""module M5
            macro newmod()
                return quote
                    let a = 1
                        $(Expr(:toplevel,
                               Expr(:module, true, :M5_inner,
                                    Expr(:block, :(global asdf = 1)))))
                    end
                end
            end
            @newmod()
            end""")
        @test out isa Module
        @test isdefined(test_mod, :M5)
        @test isdefined(test_mod.M5, :M5_inner)
        @test isdefined(test_mod.M5.M5_inner, :asdf)

        @test jeval("Base.@propagate_inbounds @inline meta_double_quote_issue(x) = x") isa Function
    end

    @testset "(AI) `include_string` with `mapexpr`" begin
        seen = Any[]
        out = JL.include_string(ex -> (push!(seen, ex); ex), Module(:MapexprSeen),
                                "aa = 1\n\nbb = aa + 1\nbb*10", "none")
        @test seen == [Meta.parse("aa = 1"), Meta.parse("bb = aa + 1"), Meta.parse("bb*10")]
        @test out === 20

        function mapexpr_sees(code)
            seen = Any[]
            JL.include_string(ex -> (push!(seen, ex); nothing), Module(:MapexprCmp),
                              code, "none")
            only(seen)
        end
        for code in ("xx += 1", "for i in 1:2; end", "function g(a); a; end",
                     "@inline h(a) = a", "\"doc\" k(a) = a", "using Base.Threads",
                     "const cc = 1", "a.b = 2", "x[1] = 2", "if p; q; else; r; end",
                     "macro mmm(); end")
            @test mapexpr_sees(code) == Meta.parse(code)
        end
        # FIXME: our `:module` expressions have the syntax version as an extra
        # leading argument - `Expr(:module, v"1.14.0", true, :MMM, body)` - so
        # they don't have the shape `mapexpr` expects
        @test_broken mapexpr_sees("module MMM; end") == Meta.parse("module MMM; end")

        out = JL.include_string(ex -> Expr(:module, true, :Renamed, ex.args[end]),
                                Module(:MapexprModule), "module Orig; yy = 1; end", "none")
        @test out isa Module && nameof(out) === :Renamed
        @test Base.invokelatest(() -> isdefined(out, :yy))

        # Statements are mapped and evaluated one at a time (rather than all
        # mapped up front), so `mapexpr` may depend on earlier evaluation
        log = Symbol[]
        logmod = Module(:MapexprOrder)
        Core.eval(logmod, :(const log = $log))
        JL.include_string(ex -> (push!(log, :map); ex), logmod,
                          "push!(log, :eval)\npush!(log, :eval)", "none")
        @test log == [:map, :eval, :map, :eval]

        # return value
        @test JL.include_string(ex -> Expr(:call, :+, ex, 100), Module(:MapexprAdd),
                                "1+1\n2+2", "none") === 104
        # returning `nothing` drops a statement
        @test JL.include_string(ex -> nothing, Module(:MapexprDrop),
                                "error(\"not evaluated\")", "none") === nothing
        @test JL.include_string(ex -> error("never called"), Module(:MapexprEmpty),
                                "# just a comment\n", "none") === nothing

        noop(x) = x
        # Definitions survive the round trip (world age)
        m = Module(:MapexprDefs)
        @test JL.include_string(noop, m, "module Inner; zz = 5; end", "none") isa Module
        @test Base.invokelatest(() -> isdefined(m.Inner, :zz))
        @test JL.include_string(noop, m, "f(x) = x + 1\nf(2)", "none") === 3
        @test JL.include_string(noop, m, "macro mm(); 7; end\n@mm", "none") === 7

        # `expr_compat_mode` still applies
        @test JL.include_string(noop, Module(:MapexprCompat),
                                """
                                macro plus1(ex)
                                    :(\$(esc(ex)) + 1)
                                end
                                qq = 10
                                @plus1 qq
                                """, "none"; expr_compat_mode=true) === 11

        # Errors in the included code and in `mapexpr` itself both propagate
        @test_throws "boom" JL.include_string(noop, Module(:MapexprErr),
                                              "error(\"boom\")", "none")
        @test_throws "in mapexpr" JL.include_string(ex -> error("in mapexpr"),
                                                    Module(:MapexprErr2), "1+1", "none")
    end
end
