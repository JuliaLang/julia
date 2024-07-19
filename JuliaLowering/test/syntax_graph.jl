@testset "SyntaxTree" begin
    # Expr conversion
    @test Expr(parsestmt(SyntaxTree, "begin a + b ; c end", filename="none")) ==
        Meta.parse("begin a + b ; c end")
end
