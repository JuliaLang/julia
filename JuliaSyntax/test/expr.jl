
@testset "Expr conversion" begin
    @testset "Quote nodes" begin
        @test parseall(Expr, ":(a)", rule=:atom) == QuoteNode(:a)
        @test parseall(Expr, ":(:a)", rule=:atom) == Expr(:quote, QuoteNode(:a))
        @test parseall(Expr, ":(1+2)", rule=:atom) == Expr(:quote, Expr(:call, :+, 1, 2))
        # Compatibility hack for VERSION >= v"1.4"
        # https://github.com/JuliaLang/julia/pull/34077
        @test parseall(Expr, ":true", rule=:atom) == Expr(:quote, true)
    end

    @testset "Short form function line numbers" begin
        # A block is added to hold the line number node
        @test parseall(Expr, "f() = xs", rule=:statement) ==
            Expr(:(=),
                 Expr(:call, :f),
                 Expr(:block,
                      LineNumberNode(1),
                      :xs))
        # flisp parser quirk: In a for loop the block is not added, despite
        # this defining a short-form function.
        @test parseall(Expr, "for f() = xs\nend", rule=:statement) ==
            Expr(:for,
                 Expr(:(=), Expr(:call, :f), :xs),
                 Expr(:block))
    end

    @testset "Long form anonymous functions" begin
        @test parseall(Expr, "function (xs...)\nbody end", rule=:statement) ==
            Expr(:function,
                 Expr(:..., :xs),
                 Expr(:block, :body))
    end
end
