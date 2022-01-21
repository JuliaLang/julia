
@testset "Parse tree conversion" begin
    @testset "Quote nodes" begin
        @test Expr(child(parse_all(SyntaxNode, ":(a)"), 1)) == QuoteNode(:a)
        @test Expr(child(parse_all(SyntaxNode, ":(:a)"), 1)) ==
            Expr(:quote, QuoteNode(:a))
        @test Expr(child(parse_all(SyntaxNode, ":(1+2)"), 1)) ==
            Expr(:quote, Expr(:call, :+, 1, 2))
    end
end
