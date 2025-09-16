const JL = JuliaLowering

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

    if isdefined(Core, :_lower)
        function jeval(str)
            prog = parseall(Expr, str)
            local out
            try
                JL.activate!()
                out = Core.eval(test_mod, prog)
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

            # Tricky cases with symbols
            out = jeval("""module M
                Base.@constprop :aggressive function f(x); x; end
                const what = ccall(:jl_value_ptr, Ptr{Cvoid}, (Any,), Core.nothing)
            end""")
            @test out isa Module
            @test isdefined(test_mod, :M)
            @test isdefined(test_mod.M, :f)
            @test isdefined(test_mod.M, :what)

            # TODO: broken, commented to prevent error logging
            # @test jeval("Base.@propagate_inbounds @inline meta_double_quote_issue(x) = x") isa Function
        end
    end
end
