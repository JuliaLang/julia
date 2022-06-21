
@testset "Expr conversion" begin
    @testset "Quote nodes" begin
        @test parseall(Expr, ":(a)", rule=:atom) == QuoteNode(:a)
        @test parseall(Expr, ":(:a)", rule=:atom) == Expr(:quote, QuoteNode(:a))
        @test parseall(Expr, ":(1+2)", rule=:atom) == Expr(:quote, Expr(:call, :+, 1, 2))
        # Compatibility hack for VERSION >= v"1.4"
        # https://github.com/JuliaLang/julia/pull/34077
        @test parseall(Expr, ":true", rule=:atom) == Expr(:quote, true)
    end

    @testset "Line numbers" begin
        @testset "Blocks" begin
            @test parseall(Expr, "begin a\nb\n\nc\nend", rule=:statement) ==
                Expr(:block,
                     LineNumberNode(1),
                     :a,
                     LineNumberNode(2),
                     :b,
                     LineNumberNode(4),
                     :c,
                )
            @test parseall(Expr, "begin end", rule=:statement) ==
                Expr(:block,
                     LineNumberNode(1)
                )

            @test parseall(Expr, "a\n\nb") ==
                Expr(:toplevel,
                     LineNumberNode(1),
                     :a,
                     LineNumberNode(3),
                     :b,
                )

            @test parseall(Expr, "module A\n\nbody\nend", rule=:statement) ==
                Expr(:module,
                     true,
                     :A,
                     Expr(:block,
                          LineNumberNode(1),
                          LineNumberNode(3),
                          :body,
                     ),
                )
        end

        @testset "Function definition lines" begin
            @test parseall(Expr, "function f()\na\n\nb\nend", rule=:statement) ==
                Expr(:function,
                     Expr(:call, :f),
                     Expr(:block,
                         LineNumberNode(1),
                         LineNumberNode(2),
                         :a,
                         LineNumberNode(4),
                         :b,
                     )
                )
            @test parseall(Expr, "f() = 1", rule=:statement) ==
                Expr(:(=),
                     Expr(:call, :f),
                     Expr(:block,
                          LineNumberNode(1),
                          1
                     )
                )

            # function/macro without methods
            @test parseall(Expr, "function f end", rule=:statement) ==
                Expr(:function, :f)
            @test parseall(Expr, "macro f end", rule=:statement) ==
                Expr(:macro, :f)
        end

        @testset "elseif" begin
            @test parseall(Expr, "if a\nb\nelseif c\n d\nend", rule=:statement) ==
                Expr(:if,
                     :a,
                     Expr(:block,
                          LineNumberNode(2),
                          :b),
                     Expr(:elseif,
                          Expr(:block,
                               LineNumberNode(3),  # Line number for elseif condition
                               :c),
                          Expr(:block,
                               LineNumberNode(4),
                               :d),
                     )
                )
        end

        @testset "No line numbers in for/let bindings" begin
            @test parseall(Expr, "for i=is, j=js\nbody\nend", rule=:statement) ==
                Expr(:for,
                     Expr(:block,
                         Expr(:(=), :i, :is),
                         Expr(:(=), :j, :js),
                     ),
                     Expr(:block,
                         LineNumberNode(2),
                         :body
                     )
                )
            @test parseall(Expr, "let i=is, j=js\nbody\nend", rule=:statement) ==
                Expr(:let,
                     Expr(:block,
                         Expr(:(=), :i, :is),
                         Expr(:(=), :j, :js),
                     ),
                     Expr(:block,
                         LineNumberNode(2),
                         :body
                     )
                )
        end
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
                 Expr(:block,
                      LineNumberNode(1)
                     ))
    end

    @testset "Long form anonymous functions" begin
        @test parseall(Expr, "function (xs...)\nbody end", rule=:statement) ==
            Expr(:function,
                 Expr(:..., :xs),
                 Expr(:block,
                      LineNumberNode(1),
                      LineNumberNode(2),
                      :body))
    end
end
