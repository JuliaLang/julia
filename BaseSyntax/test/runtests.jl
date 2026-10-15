using Test
using BaseSyntax

@testset "BaseSyntax" begin
    if isdefined(Base, :BaseSyntax)
        @test BaseSyntax.Syntax === Base.BaseSyntax.Syntax === Base.Syntax
    end
    @static if isdefined(BaseSyntax, :Syntax)
        ctx = BaseSyntax.SyntaxContext(Main, BaseSyntax.JL_OLD_EDITION)
        st = BaseSyntax.@mknode(; head=:identifier, value="x",
                                source=LineNumberNode(1, :none), context=ctx)
        @test BaseSyntax.head(st) === :identifier
        @test BaseSyntax.syntax_name(st) == "x"
        ex = :(f(x, y))
        @test BaseSyntax.syntax_to_expr(BaseSyntax.expr_to_syntax(ex)) == ex
    end
end
