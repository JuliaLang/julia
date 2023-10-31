@testset "Expr parsing with $method" for method in ["build_tree", "SyntaxNode conversion"]
    parseatom, parsestmt, parseall =
        if method == "build_tree"
            ((s; kws...) -> JuliaSyntax.parseatom(Expr, s; kws...),
             (s; kws...) -> JuliaSyntax.parsestmt(Expr, s; kws...),
             (s; kws...) -> JuliaSyntax.parseall(Expr, s; kws...))
        else
            ((s; kws...) -> Expr(JuliaSyntax.parseatom(SyntaxNode, s; keep_parens=true, kws...)),
             (s; kws...) -> Expr(JuliaSyntax.parsestmt(SyntaxNode, s; keep_parens=true, kws...)),
             (s; kws...) -> Expr(JuliaSyntax.parseall(SyntaxNode, s; keep_parens=true, kws...)))
        end

    @testset "Quote nodes" begin
        @test parseatom(":(a)") == QuoteNode(:a)
        @test parseatom(":(:a)") == Expr(:quote, QuoteNode(:a))
        @test parseatom(":(1+2)") == Expr(:quote, Expr(:call, :+, 1, 2))
        # Compatibility hack for VERSION >= v"1.4"
        # https://github.com/JuliaLang/julia/pull/34077
        @test parseatom(":true") == Expr(:quote, true)
    end

    @testset "Line numbers" begin
        @testset "Blocks" begin
            @test parsestmt("begin a\nb\n\nc\nend") ==
                Expr(:block,
                     LineNumberNode(1),
                     :a,
                     LineNumberNode(2),
                     :b,
                     LineNumberNode(4),
                     :c,
                )
            @test parsestmt("(a;b;c)") ==
                Expr(:block,
                     :a,
                     LineNumberNode(1),
                     :b,
                     LineNumberNode(1),
                     :c,
                )
            @test parsestmt("begin end") ==
                Expr(:block,
                     LineNumberNode(1)
                )
            @test parsestmt("(;;)") ==
                Expr(:block)

            @test parseall("a\n\nb") ==
                Expr(:toplevel,
                     LineNumberNode(1),
                     :a,
                     LineNumberNode(3),
                     :b,
                )
            @test parsestmt("a;b") ==
                Expr(:toplevel, :a, :b)

            @test parsestmt("module A\n\nbody\nend") ==
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
            @test parsestmt("function f()\na\n\nb\nend") ==
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
            @test parsestmt("f() = 1") ==
                Expr(:(=),
                     Expr(:call, :f),
                     Expr(:block,
                          LineNumberNode(1),
                          1
                     )
                )
            @test parsestmt("macro f()\na\nend") ==
                Expr(:macro,
                     Expr(:call, :f),
                     Expr(:block,
                         LineNumberNode(1),
                         LineNumberNode(2),
                         :a,
                     )
                )

            # function/macro without methods
            @test parsestmt("function f end") ==
                Expr(:function, :f)
            @test parsestmt("macro f end") ==
                Expr(:macro, :f)

            # weird cases with extra parens
            @test parsestmt("function (f() where T) end") ==
                Expr(:function, Expr(:where, Expr(:call, :f), :T),
                     Expr(:block, LineNumberNode(1), LineNumberNode(1)))
            @test parsestmt("function (f()::S) end") ==
                Expr(:function, Expr(:(::), Expr(:call, :f), :S),
                     Expr(:block, LineNumberNode(1), LineNumberNode(1)))
        end

        @testset "->" begin
            @test parsestmt("a -> b") ==
                Expr(:->, :a, Expr(:block, LineNumberNode(1), :b))
            # @test parsestmt("a -> (\nb;c)") ==
            #     Expr(:->, :a, Expr(:block, LineNumberNode(1), :b))
            @test parsestmt("a -> begin\nb\nc\nend") ==
                Expr(:->, :a, Expr(:block,
                                   LineNumberNode(1),
                                   LineNumberNode(2), :b,
                                   LineNumberNode(3), :c))
        end

        @testset "elseif" begin
            @test parsestmt("if a\nb\nelseif c\n d\nend") ==
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

        @testset "No line numbers in let bindings" begin
            @test parsestmt("let i=is, j=js\nbody\nend") ==
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

        @testset "Loops" begin
            @test parsestmt("for x=xs\n\nend") ==
                Expr(:for,
                     Expr(:(=), :x, :xs),
                     Expr(:block,
                          LineNumberNode(1),
                          LineNumberNode(3)
                     )
                )
            @test parsestmt("for x=xs\ny\nend") ==
                Expr(:for,
                     Expr(:(=), :x, :xs),
                     Expr(:block,
                          LineNumberNode(2),
                          :y,
                          LineNumberNode(3)
                     )
                )
            @test parsestmt("while cond\n\nend") ==
                Expr(:while,
                     :cond,
                     Expr(:block,
                          LineNumberNode(1),
                          LineNumberNode(3)
                     )
                )
            @test parsestmt("while cond\ny\nend") ==
                Expr(:while,
                     :cond,
                     Expr(:block,
                          LineNumberNode(2),
                          :y,
                          LineNumberNode(3)
                     )
                )
        end
    end

    @testset "Short form function line numbers" begin
        # A block is added to hold the line number node
        @test parsestmt("f() = xs") ==
            Expr(:(=),
                 Expr(:call, :f),
                 Expr(:block,
                      LineNumberNode(1),
                      :xs))

        @test parsestmt("f() =\n(a;b)") ==
            Expr(:(=),
                 Expr(:call, :f),
                 Expr(:block,
                      LineNumberNode(1),
                      :a,
                      LineNumberNode(2),
                      :b))

        @test parsestmt("f() =\nbegin\na\nb\nend") ==
            Expr(:(=),
                 Expr(:call, :f),
                 Expr(:block,
                      LineNumberNode(1),
                      LineNumberNode(3),
                      :a,
                      LineNumberNode(4),
                      :b))

        @test parsestmt("let f(x) =\ng(x)=1\nend") ==
            Expr(:let,
                 Expr(:(=),
                      Expr(:call, :f, :x),
                      Expr(:block,
                           LineNumberNode(1),
                           Expr(:(=),
                               Expr(:call, :g, :x),
                               Expr(:block,
                                    LineNumberNode(2),
                                    1)))),
                 Expr(:block,
                      LineNumberNode(3)))

        # `.=` doesn't introduce short form functions
        @test parsestmt("f() .= xs") ==
            Expr(:(.=), Expr(:call, :f), :xs)
    end

    @testset "for" begin
        @test parsestmt("for i=is body end") ==
            Expr(:for,
                 Expr(:(=), :i, :is),
                 Expr(:block,
                     LineNumberNode(1),
                     :body,
                     LineNumberNode(1)
                 )
            )
        @test parsestmt("for i=is, j=js\nbody\nend") ==
            Expr(:for,
                 Expr(:block,
                     Expr(:(=), :i, :is),
                     Expr(:(=), :j, :js),
                 ),
                 Expr(:block,
                     LineNumberNode(2),
                     :body,
                     LineNumberNode(3),
                 )
            )
    end

    @testset "Long form anonymous functions" begin
        @test parsestmt("function (xs...)\nbody end") ==
            Expr(:function,
                 Expr(:..., :xs),
                 Expr(:block,
                      LineNumberNode(1),
                      LineNumberNode(2),
                      :body))
    end

    @testset "String conversions" begin
        # String unwrapping / wrapping
        @test parsestmt("\"str\"") == "str"
        @test parsestmt("\"\$(\"str\")\"") ==
            Expr(:string, Expr(:string, "str"))
        # Concatenation of string chunks in triple quoted cases
        @test parsestmt("```\n  a\n  b```") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), LineNumberNode(1),
                 "a\nb")
        @test parsestmt("\"\"\"\n  a\n  \$x\n  b\n  c\"\"\"") ==
            Expr(:string, "a\n", :x, "\nb\nc")
        # Incomplete cases
        @test parsestmt("`x", ignore_errors=true) ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), LineNumberNode(1),
                 Expr(:string, "x", Expr(:error)))
        @test parsestmt("`", ignore_errors=true) ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), LineNumberNode(1),
                 Expr(:string, Expr(:error)))
    end

    @testset "Char conversions" begin
        @test parsestmt("'a'") == 'a'
        @test parsestmt("'α'") == 'α'
        @test parsestmt("'\\xce\\xb1'") == 'α'
    end

    @testset "do block conversion" begin
        @test parsestmt("f(x) do y\n body end") ==
            Expr(:do,
                 Expr(:call, :f, :x),
                 Expr(:->, Expr(:tuple, :y),
                      Expr(:block,
                           LineNumberNode(2),
                           :body)))

        @test parsestmt("@f(x) do y body end") ==
            Expr(:do,
                 Expr(:macrocall, Symbol("@f"), LineNumberNode(1), :x),
                 Expr(:->, Expr(:tuple, :y),
                      Expr(:block,
                           LineNumberNode(1),
                           :body)))

        @test parsestmt("f(x; a=1) do y body end") ==
            Expr(:do,
                 Expr(:call, :f, Expr(:parameters, Expr(:kw, :a, 1)), :x),
                 Expr(:->, Expr(:tuple, :y),
                      Expr(:block,
                           LineNumberNode(1),
                           :body)))

        # Test calls with do inside them
        @test parsestmt("g(f(x) do y\n body end)") ==
            Expr(:call,
                 :g,
                 Expr(:do,
                      Expr(:call, :f, :x),
                      Expr(:->, Expr(:tuple, :y),
                           Expr(:block,
                                LineNumberNode(2),
                                :body))))
    end

    @testset "= to Expr(:kw) conversion" begin
        # Call
        @test parsestmt("f(a=1)") ==
            Expr(:call, :f, Expr(:kw, :a, 1))
        @test parsestmt("f(; b=2)") ==
            Expr(:call, :f, Expr(:parameters, Expr(:kw, :b, 2)))
        @test parsestmt("f(a=1; b=2)") ==
            Expr(:call, :f, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:kw, :a, 1))
        @test parsestmt("f(a; b; c)") == 
            Expr(:call, :f, Expr(:parameters, Expr(:parameters, :c), :b), :a)
        @test parsestmt("+(a=1,)") ==
            Expr(:call, :+, Expr(:kw, :a, 1))
        @test parsestmt("(a=1)()") ==
            Expr(:call, Expr(:(=), :a, 1))

        # Operator calls:  = is not :kw
        @test parsestmt("(x=1) != 2") ==
            Expr(:call, :!=, Expr(:(=), :x, 1), 2)
        @test parsestmt("+(a=1)") == 
            Expr(:call, :+, Expr(:(=), :a, 1))
        @test parsestmt("(a=1)'") == 
            Expr(Symbol("'"), Expr(:(=), :a, 1))
        @test parsestmt("(a=1)'ᵀ") == 
            Expr(:call, Symbol("'ᵀ"), Expr(:(=), :a, 1))

        # Dotcall
        @test parsestmt("f.(a=1; b=2)") ==
            Expr(:., :f, Expr(:tuple,
                              Expr(:parameters, Expr(:kw, :b, 2)),
                              Expr(:kw, :a, 1)))

        # Named tuples
        @test parsestmt("(a=1,)") ==
            Expr(:tuple, Expr(:(=), :a, 1))
        @test parsestmt("(a=1,; b=2)") ==
            Expr(:tuple, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:(=), :a, 1))
        @test parsestmt("(a=1,; b=2; c=3)") ==
            Expr(:tuple,
                 Expr(:parameters,
                      Expr(:parameters, Expr(:kw, :c, 3)),
                      Expr(:kw, :b, 2)),
                 Expr(:(=), :a, 1))

        # ref
        @test parsestmt("x[i=j]") ==
            Expr(:ref, :x, Expr(:kw, :i, :j))
        @test parsestmt("(i=j)[x]") ==
            Expr(:ref, Expr(:(=), :i, :j), :x)
        @test parsestmt("x[a, b; i=j]") ==
            Expr(:ref, :x, Expr(:parameters, Expr(:(=), :i, :j)), :a, :b)
        # curly
        @test parsestmt("(i=j){x}") ==
            Expr(:curly, Expr(:(=), :i, :j), :x)
        @test parsestmt("x{a, b; i=j}") ==
            Expr(:curly, :x, Expr(:parameters, Expr(:(=), :i, :j)), :a, :b)

        # vect
        @test parsestmt("[a=1,; b=2]") ==
            Expr(:vect,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))
        # braces
        @test parsestmt("{a=1,; b=2}") ==
            Expr(:braces,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))

        # dotted = is not :kw
        @test parsestmt("f(a .= 1)") ==
            Expr(:call, :f, Expr(:.=, :a, 1))

        # = inside parens in calls and tuples
        @test parsestmt("f(((a = 1)))") ==
            Expr(:call, :f, Expr(:kw, :a, 1))
        @test parsestmt("(((a = 1)),)") ==
            Expr(:tuple, Expr(:(=), :a, 1))
        @test parsestmt("(;((a = 1)),)") ==
            Expr(:tuple, Expr(:parameters, Expr(:kw, :a, 1)))
    end

    @testset "Field access syntax" begin
        @test parsestmt("a.b") == Expr(:., :a, QuoteNode(:b))
        @test parsestmt("a.\$b") == Expr(:., :a, QuoteNode(Expr(:$, :b)))
        @test parsestmt("a.:b") == Expr(:., :a, QuoteNode(:b))
        @test parsestmt("a.@b x") == Expr(:macrocall,
                                          Expr(:., :a, QuoteNode(Symbol("@b"))),
                                          LineNumberNode(1),
                                          :x)
    end

    @testset "dotcall / dotted operators" begin
        @test parsestmt("f.(x,y)") == Expr(:., :f, Expr(:tuple, :x, :y))
        @test parsestmt("f.(x=1)") == Expr(:., :f, Expr(:tuple, Expr(:kw, :x, 1)))
        @test parsestmt("f.(a=1; b=2)") ==
            Expr(:., :f, Expr(:tuple, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:kw, :a, 1)))
        @test parsestmt("(a=1).()") == Expr(:., Expr(:(=), :a, 1), Expr(:tuple))
        @test parsestmt("x .+ y")  == Expr(:call, Symbol(".+"), :x, :y)
        @test parsestmt("(x=1) .+ y") == Expr(:call, Symbol(".+"), Expr(:(=), :x, 1), :y)
        @test parsestmt("a .< b .< c") == Expr(:comparison, :a, Symbol(".<"),
                                                     :b, Symbol(".<"), :c)
        @test parsestmt("a .< (.<) .< c") == Expr(:comparison, :a, Symbol(".<"),
                                                        Expr(:., :<), Symbol(".<"), :c)
        @test parsestmt(".*(x)")    == Expr(:call, Symbol(".*"), :x)
        @test parsestmt(".+(x)")    == Expr(:call, Symbol(".+"), :x)
        @test parsestmt(".+x")      == Expr(:call, Symbol(".+"), :x)
        @test parsestmt("(.+)(x)")  == Expr(:call, Expr(:., :+), :x)
        @test parsestmt("(.+).(x)") == Expr(:., Expr(:., :+), Expr(:tuple, :x))

        @test parsestmt(".+")    == Expr(:., :+)
        @test parsestmt(":.+")   == QuoteNode(Symbol(".+"))
        @test parsestmt(":(.+)") == Expr(:quote, (Expr(:., :+)))
        @test parsestmt("quote .+ end")   == Expr(:quote,
                                                        Expr(:block,
                                                             LineNumberNode(1),
                                                             Expr(:., :+)))
        @test parsestmt(".+{x}") == Expr(:curly, Symbol(".+"), :x)

        # Quoted syntactic ops act different when in parens
        @test parsestmt(":.=")   == QuoteNode(Symbol(".="))
        @test parsestmt(":(.=)") == QuoteNode(Symbol(".="))

        # A few other cases of bare dotted ops
        @test parsestmt("f(.+)")   == Expr(:call, :f, Expr(:., :+))
        @test parsestmt("(a, .+)") == Expr(:tuple, :a, Expr(:., :+))
        @test parsestmt("A.:.+")   == Expr(:., :A, QuoteNode(Symbol(".+")))

        # Issue #341
        @test parsestmt("./x", ignore_errors=true) == Expr(:call, Expr(:error, Expr(:., :/)), :x)
    end

    @testset "let" begin
        @test parsestmt("let x=1\n end") ==
            Expr(:let, Expr(:(=), :x, 1),  Expr(:block, LineNumberNode(2)))
        @test parsestmt("let x=1 ; end") ==
            Expr(:let, Expr(:(=), :x, 1),  Expr(:block, LineNumberNode(1)))
        @test parsestmt("let x ; end") ==
            Expr(:let, :x, Expr(:block, LineNumberNode(1)))
        @test parsestmt("let x::1 ; end") ==
            Expr(:let, Expr(:(::), :x, 1), Expr(:block, LineNumberNode(1)))
        @test parsestmt("let x=1,y=2 end") ==
            Expr(:let, Expr(:block, Expr(:(=), :x, 1), Expr(:(=), :y, 2)), Expr(:block, LineNumberNode(1)))
        @test parsestmt("let x+=1 ; end") ==
            Expr(:let, Expr(:block, Expr(:+=, :x, 1)), Expr(:block, LineNumberNode(1)))
        @test parsestmt("let ; end") ==
            Expr(:let, Expr(:block), Expr(:block, LineNumberNode(1)))
        @test parsestmt("let ; body end") ==
            Expr(:let, Expr(:block), Expr(:block, LineNumberNode(1), :body))
        @test parsestmt("let\na\nb\nend") ==
            Expr(:let, Expr(:block), Expr(:block, LineNumberNode(2), :a, LineNumberNode(3), :b))
    end

    @testset "where" begin
        @test parsestmt("A where T") == Expr(:where, :A, :T)
        @test parsestmt("A where {T}") == Expr(:where, :A, :T)
        @test parsestmt("A where {S, T}") == Expr(:where, :A, :S, :T)
        @test parsestmt("A where {X, Y; Z}") == Expr(:where, :A, Expr(:parameters, :Z), :X, :Y)
    end

    @testset "macrocall" begin
        # line numbers
        @test parsestmt("@m\n") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1))
        @test parsestmt("\n@m") == Expr(:macrocall, Symbol("@m"), LineNumberNode(2))
        # parameters
        @test parsestmt("@m(x; a)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1),
                                              Expr(:parameters, :a), :x)
        @test parsestmt("@m(a=1; b=2)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1),
                                                  Expr(:parameters, Expr(:kw, :b, 2)), Expr(:(=), :a, 1))
        # @__dot__
        @test parsestmt("@.") == Expr(:macrocall, Symbol("@__dot__"), LineNumberNode(1))
        @test parsestmt("using A: @.") == Expr(:using, Expr(Symbol(":"), Expr(:., :A), Expr(:., Symbol("@__dot__"))))

        # var""
        @test parsestmt("@var\"#\" a") == Expr(:macrocall, Symbol("@#"), LineNumberNode(1), :a)
        @test parsestmt("A.@var\"#\" a") == Expr(:macrocall, Expr(:., :A, QuoteNode(Symbol("@#"))), LineNumberNode(1), :a)

        # Square brackets
        @test parsestmt("@S[a,b]") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:vect, :a, :b))
        @test parsestmt("@S[a b]") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:hcat, :a, :b))
        @test parsestmt("@S[a; b]") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:vcat, :a, :b))
        @test parsestmt("@S[a ;; b]", version=v"1.7") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:ncat, 2, :a, :b))
    end

    @testset "var" begin
        @test parsestmt("var\"x\"") == :x
        @test parsestmt("var\"\"")         == Symbol("")
        @test parsestmt("var\"\\\"\"")     == Symbol("\"")
        @test parsestmt("var\"\\\\\\\"\"") == Symbol("\\\"")
        @test parsestmt("var\"\\\\x\"")    == Symbol("\\\\x")
        @test parsestmt("var\"x\"+y")      == Expr(:call, :+, :x, :y)
    end

    @testset "vect" begin
        @test parsestmt("[x,y ; z]") == Expr(:vect, Expr(:parameters, :z), :x, :y)
    end

    @testset "concatenation" begin
        @test parsestmt("[a ;;; b ;;;; c]", version=v"1.7") ==
            Expr(:ncat, 4, Expr(:nrow, 3, :a, :b), :c)
        @test parsestmt("[a b ; c d]") ==
            Expr(:vcat, Expr(:row, :a, :b), Expr(:row, :c, :d))
        @test parsestmt("[a\nb]") == Expr(:vcat, :a, :b)
        @test parsestmt("[a b]") == Expr(:hcat, :a, :b)
        @test parsestmt("[a b ; c d]") ==
            Expr(:vcat, Expr(:row, :a, :b), Expr(:row, :c, :d))

        @test parsestmt("T[a ;;; b ;;;; c]", version=v"1.7") ==
            Expr(:typed_ncat, :T, 4, Expr(:nrow, 3, :a, :b), :c)
        @test parsestmt("T[a b ; c d]") ==
            Expr(:typed_vcat, :T, Expr(:row, :a, :b), Expr(:row, :c, :d))
        @test parsestmt("T[a\nb]") == Expr(:typed_vcat, :T, :a, :b)
        @test parsestmt("T[a b]") == Expr(:typed_hcat, :T, :a, :b)
        @test parsestmt("T[a b ; c d]") ==
            Expr(:typed_vcat, :T, Expr(:row, :a, :b), Expr(:row, :c, :d))
    end

    @testset "generators" begin
        @test parsestmt("(x for a in as for b in bs)") ==
            Expr(:flatten, Expr(:generator,
                                Expr(:generator, :x, Expr(:(=), :b, :bs)),
                                Expr(:(=), :a, :as)))
        @test parsestmt("(x for a in as, b in bs)") ==
            Expr(:generator, :x, Expr(:(=), :a, :as), Expr(:(=), :b, :bs))
        @test parsestmt("(x for a in as, b in bs if z)") ==
            Expr(:generator, :x,
                 Expr(:filter, :z, Expr(:(=), :a, :as), Expr(:(=), :b, :bs)))
        @test parsestmt("(x for a in as, b in bs for c in cs, d in ds)") ==
            Expr(:flatten, 
                Expr(:generator, 
                     Expr(:generator, :x, Expr(:(=), :c, :cs), Expr(:(=), :d, :ds)),
                     Expr(:(=), :a, :as), Expr(:(=), :b, :bs)))
        @test parsestmt("(x for a in as for b in bs if z)") ==
            Expr(:flatten, Expr(:generator,
                                Expr(:generator, :x, Expr(:filter, :z, Expr(:(=), :b, :bs))),
                                Expr(:(=), :a, :as)))
        @test parsestmt("(x for a in as if z for b in bs)") ==
            Expr(:flatten, Expr(:generator,
                                Expr(:generator, :x, Expr(:(=), :b, :bs)),
                                Expr(:filter, :z, Expr(:(=), :a, :as))))
        @test parsestmt("[x for a = as for b = bs if cond1 for c = cs if cond2]" ) ==
            Expr(:comprehension,
                 Expr(:flatten,
                      Expr(:generator,
                           Expr(:flatten,
                                Expr(:generator,
                                     Expr(:generator,
                                          :x,
                                          Expr(:filter,
                                               :cond2,
                                               Expr(:(=), :c, :cs))),
                                     Expr(:filter,
                                          :cond1,
                                          Expr(:(=), :b, :bs)))),
                           Expr(:(=), :a, :as))))
        @test parsestmt("[x for a = as if begin cond2 end]" ) ==
            Expr(:comprehension, Expr(:generator, :x,
                                      Expr(:filter,
                                           Expr(:block, LineNumberNode(1), :cond2),
                                           Expr(:(=), :a, :as))))
        @test parsestmt("(x for a in as if z)") ==
            Expr(:generator, :x, Expr(:filter, :z, Expr(:(=), :a, :as)))
    end

    @testset "try" begin
        @test parsestmt("try x catch e; y end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y))
        @test parsestmt("try x finally y end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 false,
                 false,
                 Expr(:block, LineNumberNode(1), :y))
        @test parsestmt("try x catch e; y finally z end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 Expr(:block, LineNumberNode(1), :z))
        @test parsestmt("try x catch e; y else z end", version=v"1.8") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 false,
                 Expr(:block, LineNumberNode(1), :z))
        @test parsestmt("try x catch e; y else z finally w end", version=v"1.8") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 Expr(:block, LineNumberNode(1), :w),
                 Expr(:block, LineNumberNode(1), :z))
        # finally before catch
        @test parsestmt("try x finally y catch e z end", ignore_warnings=true) ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :z),
                 Expr(:block, LineNumberNode(1), :y))
        # empty recovery
        @test parsestmt("try x end", ignore_errors=true) ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 false, false,
                 Expr(:block, Expr(:error)))
    end

    @testset "juxtapose" begin
        @test parsestmt("2x") == Expr(:call, :*, 2, :x)
        @test parsestmt("(2)(3)x") == Expr(:call, :*, 2, 3, :x)
    end

    @testset "Core.@doc" begin
        @test parsestmt("\"x\" f") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@doc")), LineNumberNode(1), "x", :f)
        @test parsestmt("\n\"x\" f") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@doc")), LineNumberNode(2), "x", :f)
    end

    @testset "return" begin
        @test parsestmt("return x") == Expr(:return, :x)
        @test parsestmt("return")  == Expr(:return, nothing)
    end

    @testset "Large integer macros" begin
        @test parsestmt("0x00000000000000001") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@uint128_str")),
                 nothing, "0x00000000000000001")

        @test parsestmt("(0x00000000000000001)") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@uint128_str")),
                 nothing, "0x00000000000000001")
    end

    @testset "struct" begin
        @test parsestmt("struct A end") ==
            Expr(:struct, false, :A, Expr(:block, LineNumberNode(1)))
        @test parsestmt("mutable struct A end") ==
            Expr(:struct, true, :A, Expr(:block, LineNumberNode(1)))

        @test parsestmt("struct A <: B \n a::X \n end") ==
            Expr(:struct, false, Expr(:<:, :A, :B),
                 Expr(:block, LineNumberNode(2), Expr(:(::), :a, :X)))
        @test parsestmt("struct A \n a \n b \n end") ==
            Expr(:struct, false, :A,
                 Expr(:block, LineNumberNode(2), :a, LineNumberNode(3), :b))
        @test parsestmt("struct A const a end", version=v"1.8") ==
            Expr(:struct, false, :A, Expr(:block, LineNumberNode(1), Expr(:const, :a)))
    end

    @testset "export" begin
        @test parsestmt("export a") == Expr(:export, :a)
        @test parsestmt("export @a") == Expr(:export, Symbol("@a"))
        @test parsestmt("export @var\"'\"") == Expr(:export, Symbol("@'"))
        @test parsestmt("export a, \n @b") == Expr(:export, :a, Symbol("@b"))
        @test parsestmt("export +, ==") == Expr(:export, :+, :(==))
        @test parsestmt("export \n a") == Expr(:export, :a)
    end

    @testset "global/const/local" begin
        @test parsestmt("global x") == Expr(:global, :x)
        @test parsestmt("local x") == Expr(:local, :x)
        @test parsestmt("global x,y") == Expr(:global, :x, :y)
        @test parsestmt("global const x = 1") == Expr(:const, Expr(:global, Expr(:(=), :x, 1)))
        @test parsestmt("local const x = 1") == Expr(:const, Expr(:local, Expr(:(=), :x, 1)))
        @test parsestmt("const global x = 1") == Expr(:const, Expr(:global, Expr(:(=), :x, 1)))
        @test parsestmt("const local x = 1") == Expr(:const, Expr(:local, Expr(:(=), :x, 1)))
        @test parsestmt("const x,y = 1,2") == Expr(:const, Expr(:(=), Expr(:tuple, :x, :y), Expr(:tuple, 1, 2)))
        @test parsestmt("const x = 1") == Expr(:const, Expr(:(=), :x, 1))
        @test parsestmt("global x ~ 1") == Expr(:global, Expr(:call, :~, :x, 1))
        @test parsestmt("global x += 1") == Expr(:global, Expr(:+=, :x, 1))

        # Parsing of global/local with 
        @test parsestmt("global (x,y)") == Expr(:global, :x, :y)
        @test parsestmt("local (x,y)") == Expr(:local, :x, :y)
    end

    @testset "tuples" begin
        @test parsestmt("(;)")       == Expr(:tuple, Expr(:parameters))
        @test parsestmt("(; a=1)")   == Expr(:tuple, Expr(:parameters, Expr(:kw, :a, 1)))
        @test parsestmt("(; a=1; b=2)") ==
            Expr(:tuple, Expr(:parameters, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:kw, :a, 1)))
        @test parsestmt("(a; b; c,d)") ==
            Expr(:tuple, Expr(:parameters, Expr(:parameters, :c, :d), :b), :a)
    end

    @testset "module" begin
        @test parsestmt("module A end") ==
            Expr(:module, true,  :A, Expr(:block, LineNumberNode(1), LineNumberNode(1)))
        @test parsestmt("baremodule A end") ==
            Expr(:module, false, :A, Expr(:block, LineNumberNode(1), LineNumberNode(1)))
    end

    @testset "errors" begin
        @test parsestmt("--", ignore_errors=true) ==
            Expr(:error, "invalid operator: `--`")
        @test parseall("a b", ignore_errors=true) ==
            Expr(:toplevel, LineNumberNode(1), :a,
                 LineNumberNode(1), Expr(:error, :b))
        @test parsestmt("(x", ignore_errors=true) ==
            Expr(:block, :x, Expr(:error))
        @test parsestmt("x do", ignore_errors=true) ==
            Expr(:block, :x, Expr(:error, :do))
        @test parsestmt("x var\"y\"", ignore_errors=true) ==
            Expr(:block, :x, Expr(:error, :var, ErrorVal(), "y", ErrorVal()))
        @test parsestmt("var\"y", ignore_errors=true) ==
            Expr(:var, :y, Expr(:error))
    end

    @testset "import" begin
        @test parsestmt("import A") == Expr(:import, Expr(:., :A))
        @test parsestmt("import A.(:b).:c: x.:z", ignore_warnings=true) ==
            Expr(:import, Expr(Symbol(":"), Expr(:., :A, :b, :c), Expr(:., :x, :z)))
        # Stupid parens and quotes in import paths
        @test parsestmt("import A.:+", ignore_warnings=true) ==
            Expr(:import, Expr(:., :A, :+))
        @test parsestmt("import A.(:+)", ignore_warnings=true) ==
            Expr(:import, Expr(:., :A, :+))
        @test parsestmt("import A.:(+)", ignore_warnings=true) ==
            Expr(:import, Expr(:., :A, :+))
        @test parsestmt("import A.:(+) as y", ignore_warnings=true, version=v"1.6") ==
            Expr(:import, Expr(:as, Expr(:., :A, :+), :y))
    end
end
