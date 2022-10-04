@testset "Expr conversion" begin
    @testset "Quote nodes" begin
        @test parseatom(Expr, ":(a)") == QuoteNode(:a)
        @test parseatom(Expr, ":(:a)") == Expr(:quote, QuoteNode(:a))
        @test parseatom(Expr, ":(1+2)") == Expr(:quote, Expr(:call, :+, 1, 2))
        # Compatibility hack for VERSION >= v"1.4"
        # https://github.com/JuliaLang/julia/pull/34077
        @test parseatom(Expr, ":true") == Expr(:quote, true)
    end

    @testset "Line numbers" begin
        @testset "Blocks" begin
            @test parse(Expr, "begin a\nb\n\nc\nend") ==
                Expr(:block,
                     LineNumberNode(1),
                     :a,
                     LineNumberNode(2),
                     :b,
                     LineNumberNode(4),
                     :c,
                )
            @test parse(Expr, "begin end") ==
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

            @test parse(Expr, "module A\n\nbody\nend") ==
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
            @test parse(Expr, "function f()\na\n\nb\nend") ==
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
            @test parse(Expr, "f() = 1") ==
                Expr(:(=),
                     Expr(:call, :f),
                     Expr(:block,
                          LineNumberNode(1),
                          1
                     )
                )

            # function/macro without methods
            @test parse(Expr, "function f end") ==
                Expr(:function, :f)
            @test parse(Expr, "macro f end") ==
                Expr(:macro, :f)
        end

        @testset "elseif" begin
            @test parse(Expr, "if a\nb\nelseif c\n d\nend") ==
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
            @test parse(Expr, "for i=is, j=js\nbody\nend") ==
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
            @test parse(Expr, "let i=is, j=js\nbody\nend") ==
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
        @test parse(Expr, "f() = xs") ==
            Expr(:(=),
                 Expr(:call, :f),
                 Expr(:block,
                      LineNumberNode(1),
                      :xs))
        # flisp parser quirk: In a for loop the block is not added, despite
        # this defining a short-form function.
        @test parse(Expr, "for f() = xs\nend") ==
            Expr(:for,
                 Expr(:(=), Expr(:call, :f), :xs),
                 Expr(:block,
                      LineNumberNode(1)
                     ))
    end

    @testset "Long form anonymous functions" begin
        @test parse(Expr, "function (xs...)\nbody end") ==
            Expr(:function,
                 Expr(:..., :xs),
                 Expr(:block,
                      LineNumberNode(1),
                      LineNumberNode(2),
                      :body))
    end

    @testset "String conversions" begin
        # String unwrapping / wrapping
        @test parse(Expr, "\"str\"") == "str"
        @test parse(Expr, "\"\$(\"str\")\"") ==
            Expr(:string, Expr(:string, "str"))
        # Concatenation of string chunks in triple quoted cases
        @test parse(Expr, "```\n  a\n  b```") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), LineNumberNode(1),
                 "a\nb")
        @test parse(Expr, "\"\"\"\n  a\n  \$x\n  b\n  c\"\"\"") ==
            Expr(:string, "a\n", :x, "\nb\nc")
    end

    @testset "do block conversion" begin
        @test parse(Expr, "f(x) do y\n body end") ==
            Expr(:do, Expr(:call, :f, :x),
                 Expr(:->, Expr(:tuple, :y),
                      Expr(:block,
                           LineNumberNode(2),
                           :body)))
    end

    @testset "= to Expr(:kw) conversion" begin
        # Call
        @test parse(Expr, "f(a=1)") ==
            Expr(:call, :f, Expr(:kw, :a, 1))
        @test parse(Expr, "f(; b=2)") ==
            Expr(:call, :f, Expr(:parameters, Expr(:kw, :b, 2)))
        @test parse(Expr, "f(a=1; b=2)") ==
            Expr(:call, :f, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:kw, :a, 1))

        # Infix call = is not :kw
        @test parse(Expr, "(x=1) != 2") ==
            Expr(:call, :!=, Expr(:(=), :x, 1), 2)

        # Dotcall
        @test parse(Expr, "f.(a=1; b=2)") ==
            Expr(:., :f, Expr(:tuple,
                              Expr(:parameters, Expr(:kw, :b, 2)),
                              Expr(:kw, :a, 1)))

        # Named tuples
        @test parse(Expr, "(a=1,)") ==
            Expr(:tuple, Expr(:(=), :a, 1))
        @test parse(Expr, "(a=1,; b=2)") ==
            Expr(:tuple, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:(=), :a, 1))
        @test parse(Expr, "(a=1,; b=2; c=3)") ==
            Expr(:tuple,
                 Expr(:parameters,
                      Expr(:parameters, Expr(:kw, :c, 3)),
                      Expr(:kw, :b, 2)),
                 Expr(:(=), :a, 1))

        # ref
        @test parse(Expr, "x[i=j]") ==
            Expr(:ref, :x, Expr(:kw, :i, :j))

        # vect/braces
        @test parse(Expr, "[a=1,; b=2]") ==
            Expr(:vect,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))
        @test parse(Expr, "{a=1,; b=2}") ==
            Expr(:braces,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))

        # dotted = is not :kw
        @test parse(Expr, "f(a .= 1)") ==
            Expr(:call, :f, Expr(:.=, :a, 1))
    end
end
