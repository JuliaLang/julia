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
    end

    @testset "integration: `JuliaLowering.activate!`" begin
        prog = parseall(Expr, "global asdf = 1")
        JL.activate!()
        out = Core.eval(test_mod, prog)
        JL.activate!(false)
        @test out === 1
        @test isdefined(test_mod, :asdf)

        prog = parseall(Expr, "module M; x = 1; end")
        JL.activate!()
        out = Core.eval(test_mod, prog)
        JL.activate!(false)
        @test out isa Module
        @test isdefined(test_mod, :M)
        @test isdefined(test_mod.M, :x)
    end
end
