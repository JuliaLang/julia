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

    @testset "Char conversions" begin
        @test parse(Expr, "'a'") == 'a'
        @test parse(Expr, "'α'") == 'α'
        @test parse(Expr, "'\\xce\\xb1'") == 'α'
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
        @test parse(Expr, "(i=j)[x]") ==
            Expr(:ref, Expr(:(=), :i, :j), :x)
        @test parse(Expr, "x[a, b; i=j]") ==
            Expr(:ref, :x, Expr(:parameters, Expr(:(=), :i, :j)), :a, :b)
        # curly
        @test parse(Expr, "(i=j){x}") ==
            Expr(:curly, Expr(:(=), :i, :j), :x)
        @test parse(Expr, "x{a, b; i=j}") ==
            Expr(:curly, :x, Expr(:parameters, Expr(:(=), :i, :j)), :a, :b)

        # vect
        @test parse(Expr, "[a=1,; b=2]") ==
            Expr(:vect,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))
        # braces
        @test parse(Expr, "{a=1,; b=2}") ==
            Expr(:braces,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))

        # dotted = is not :kw
        @test parse(Expr, "f(a .= 1)") ==
            Expr(:call, :f, Expr(:.=, :a, 1))
    end

    @testset "dotcall" begin
        @test parse(Expr, "f.(x,y)") == Expr(:., :f, Expr(:tuple, :x, :y))
        @test parse(Expr, "f.(x=1)") == Expr(:., :f, Expr(:tuple, Expr(:kw, :x, 1)))
        @test parse(Expr, "x .+ y")  == Expr(:call, Symbol(".+"), :x, :y)
        @test parse(Expr, "(x=1) .+ y") == Expr(:call, Symbol(".+"), Expr(:(=), :x, 1), :y)
        @test parse(Expr, "a .< b .< c") == Expr(:comparison, :a, Symbol(".<"),
                                                 :b, Symbol(".<"), :c)
        @test parse(Expr, ".*(x)")  == Expr(:call, Symbol(".*"), :x)
        @test parse(Expr, ".+(x)")  == Expr(:call, Symbol(".+"), :x)
        @test parse(Expr, ".+x")    == Expr(:call, Symbol(".+"), :x)
    end

    @testset "where" begin
        @test parse(Expr, "A where {X, Y; Z}") == Expr(:where, :A, Expr(:parameters, :Z), :X, :Y)
    end

    @testset "macrocall" begin
        # line numbers
        @test parse(Expr, "@m\n") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1))
        @test parse(Expr, "\n@m") == Expr(:macrocall, Symbol("@m"), LineNumberNode(2))
        # parameters
        @test parse(Expr, "@m(x; a)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1),
                                              Expr(:parameters, :a), :x)
        @test parse(Expr, "@m(a=1; b=2)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1),
                                                  Expr(:parameters, Expr(:kw, :b, 2)), Expr(:(=), :a, 1))
        # @__dot__
        @test parse(Expr, "@.") == Expr(:macrocall, Symbol("@__dot__"), LineNumberNode(1))
        @test parse(Expr, "using A: @.") == Expr(:using, Expr(Symbol(":"), Expr(:., :A), Expr(:., Symbol("@__dot__"))))
    end

    @testset "try" begin
        @test parse(Expr, "try x catch e; y end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y))
        @test parse(Expr, "try x finally y end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 false,
                 false,
                 Expr(:block, LineNumberNode(1), :y))
        @test parse(Expr, "try x catch e; y finally z end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 Expr(:block, LineNumberNode(1), :z))
        @test parse(Expr, "try x catch e; y else z end", version=v"1.8") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 false,
                 Expr(:block, LineNumberNode(1), :z))
        @test parse(Expr, "try x catch e; y else z finally w end", version=v"1.8") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 Expr(:block, LineNumberNode(1), :w),
                 Expr(:block, LineNumberNode(1), :z))
    end
end
