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
            @test parsestmt(Expr, "begin a\nb\n\nc\nend") ==
                Expr(:block,
                     LineNumberNode(1),
                     :a,
                     LineNumberNode(2),
                     :b,
                     LineNumberNode(4),
                     :c,
                )
            @test parsestmt(Expr, "begin end") ==
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

            @test parsestmt(Expr, "module A\n\nbody\nend") ==
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
            @test parsestmt(Expr, "function f()\na\n\nb\nend") ==
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
            @test parsestmt(Expr, "f() = 1") ==
                Expr(:(=),
                     Expr(:call, :f),
                     Expr(:block,
                          LineNumberNode(1),
                          1
                     )
                )

            # function/macro without methods
            @test parsestmt(Expr, "function f end") ==
                Expr(:function, :f)
            @test parsestmt(Expr, "macro f end") ==
                Expr(:macro, :f)

            # weird cases with extra parens
            @test parsestmt(Expr, "function (f() where T) end") ==
                Expr(:function, Expr(:where, Expr(:call, :f), :T),
                     Expr(:block, LineNumberNode(1), LineNumberNode(1)))
            @test parsestmt(Expr, "function (f()::S) end") ==
                Expr(:function, Expr(:(::), Expr(:call, :f), :S),
                     Expr(:block, LineNumberNode(1), LineNumberNode(1)))
        end

        @testset "elseif" begin
            @test parsestmt(Expr, "if a\nb\nelseif c\n d\nend") ==
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
            @test parsestmt(Expr, "for i=is, j=js\nbody\nend") ==
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
            @test parsestmt(Expr, "let i=is, j=js\nbody\nend") ==
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
        @test parsestmt(Expr, "f() = xs") ==
            Expr(:(=),
                 Expr(:call, :f),
                 Expr(:block,
                      LineNumberNode(1),
                      :xs))
        # flisp parser quirk: In a for loop the block is not added, despite
        # this defining a short-form function.
        @test parsestmt(Expr, "for f() = xs\nend") ==
            Expr(:for,
                 Expr(:(=), Expr(:call, :f), :xs),
                 Expr(:block,
                      LineNumberNode(1)
                     ))
    end

    @testset "Long form anonymous functions" begin
        @test parsestmt(Expr, "function (xs...)\nbody end") ==
            Expr(:function,
                 Expr(:..., :xs),
                 Expr(:block,
                      LineNumberNode(1),
                      LineNumberNode(2),
                      :body))
    end

    @testset "String conversions" begin
        # String unwrapping / wrapping
        @test parsestmt(Expr, "\"str\"") == "str"
        @test parsestmt(Expr, "\"\$(\"str\")\"") ==
            Expr(:string, Expr(:string, "str"))
        # Concatenation of string chunks in triple quoted cases
        @test parsestmt(Expr, "```\n  a\n  b```") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), LineNumberNode(1),
                 "a\nb")
        @test parsestmt(Expr, "\"\"\"\n  a\n  \$x\n  b\n  c\"\"\"") ==
            Expr(:string, "a\n", :x, "\nb\nc")
    end

    @testset "Char conversions" begin
        @test parsestmt(Expr, "'a'") == 'a'
        @test parsestmt(Expr, "'α'") == 'α'
        @test parsestmt(Expr, "'\\xce\\xb1'") == 'α'
    end

    @testset "do block conversion" begin
        @test parsestmt(Expr, "f(x) do y\n body end") ==
            Expr(:do, Expr(:call, :f, :x),
                 Expr(:->, Expr(:tuple, :y),
                      Expr(:block,
                           LineNumberNode(2),
                           :body)))
    end

    @testset "= to Expr(:kw) conversion" begin
        # Call
        @test parsestmt(Expr, "f(a=1)") ==
            Expr(:call, :f, Expr(:kw, :a, 1))
        @test parsestmt(Expr, "f(; b=2)") ==
            Expr(:call, :f, Expr(:parameters, Expr(:kw, :b, 2)))
        @test parsestmt(Expr, "f(a=1; b=2)") ==
            Expr(:call, :f, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:kw, :a, 1))
        @test parsestmt(Expr, "f(a; b; c)") == 
            Expr(:call, :f, Expr(:parameters, Expr(:parameters, :c), :b), :a)
        @test parsestmt(Expr, "+(a=1,)") ==
            Expr(:call, :+, Expr(:kw, :a, 1))
        @test parsestmt(Expr, "(a=1)()") ==
            Expr(:call, Expr(:(=), :a, 1))

        # Operator calls:  = is not :kw
        @test parsestmt(Expr, "(x=1) != 2") ==
            Expr(:call, :!=, Expr(:(=), :x, 1), 2)
        @test parsestmt(Expr, "+(a=1)") == 
            Expr(:call, :+, Expr(:(=), :a, 1))
        @test parsestmt(Expr, "(a=1)'") == 
            Expr(Symbol("'"), Expr(:(=), :a, 1))
        @test parsestmt(Expr, "(a=1)'ᵀ") == 
            Expr(:call, Symbol("'ᵀ"), Expr(:(=), :a, 1))

        # Dotcall
        @test parsestmt(Expr, "f.(a=1; b=2)") ==
            Expr(:., :f, Expr(:tuple,
                              Expr(:parameters, Expr(:kw, :b, 2)),
                              Expr(:kw, :a, 1)))

        # Named tuples
        @test parsestmt(Expr, "(a=1,)") ==
            Expr(:tuple, Expr(:(=), :a, 1))
        @test parsestmt(Expr, "(a=1,; b=2)") ==
            Expr(:tuple, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:(=), :a, 1))
        @test parsestmt(Expr, "(a=1,; b=2; c=3)") ==
            Expr(:tuple,
                 Expr(:parameters,
                      Expr(:parameters, Expr(:kw, :c, 3)),
                      Expr(:kw, :b, 2)),
                 Expr(:(=), :a, 1))

        # ref
        @test parsestmt(Expr, "x[i=j]") ==
            Expr(:ref, :x, Expr(:kw, :i, :j))
        @test parsestmt(Expr, "(i=j)[x]") ==
            Expr(:ref, Expr(:(=), :i, :j), :x)
        @test parsestmt(Expr, "x[a, b; i=j]") ==
            Expr(:ref, :x, Expr(:parameters, Expr(:(=), :i, :j)), :a, :b)
        # curly
        @test parsestmt(Expr, "(i=j){x}") ==
            Expr(:curly, Expr(:(=), :i, :j), :x)
        @test parsestmt(Expr, "x{a, b; i=j}") ==
            Expr(:curly, :x, Expr(:parameters, Expr(:(=), :i, :j)), :a, :b)

        # vect
        @test parsestmt(Expr, "[a=1,; b=2]") ==
            Expr(:vect,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))
        # braces
        @test parsestmt(Expr, "{a=1,; b=2}") ==
            Expr(:braces,
                 Expr(:parameters, Expr(:(=), :b, 2)),
                 Expr(:(=), :a, 1))

        # dotted = is not :kw
        @test parsestmt(Expr, "f(a .= 1)") ==
            Expr(:call, :f, Expr(:.=, :a, 1))

        # = inside parens in calls and tuples
        @test parsestmt(Expr, "f(((a = 1)))") ==
            Expr(:call, :f, Expr(:kw, :a, 1))
        @test parsestmt(Expr, "(((a = 1)),)") ==
            Expr(:tuple, Expr(:(=), :a, 1))
        @test parsestmt(Expr, "(;((a = 1)),)") ==
            Expr(:tuple, Expr(:parameters, Expr(:kw, :a, 1)))
    end

    @testset "dotcall / dotted operators" begin
        @test parsestmt(Expr, "f.(x,y)") == Expr(:., :f, Expr(:tuple, :x, :y))
        @test parsestmt(Expr, "f.(x=1)") == Expr(:., :f, Expr(:tuple, Expr(:kw, :x, 1)))
        @test parsestmt(Expr, "f.(a=1; b=2)") ==
            Expr(:., :f, Expr(:tuple, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:kw, :a, 1)))
        @test parsestmt(Expr, "(a=1).()") == Expr(:., Expr(:(=), :a, 1), Expr(:tuple))
        @test parsestmt(Expr, "x .+ y")  == Expr(:call, Symbol(".+"), :x, :y)
        @test parsestmt(Expr, "(x=1) .+ y") == Expr(:call, Symbol(".+"), Expr(:(=), :x, 1), :y)
        @test parsestmt(Expr, "a .< b .< c") == Expr(:comparison, :a, Symbol(".<"),
                                                 :b, Symbol(".<"), :c)
        @test parsestmt(Expr, ".*(x)")    == Expr(:call, Symbol(".*"), :x)
        @test parsestmt(Expr, ".+(x)")    == Expr(:call, Symbol(".+"), :x)
        @test parsestmt(Expr, ".+x")      == Expr(:call, Symbol(".+"), :x)
        @test parsestmt(Expr, "(.+)(x)")  == Expr(:call, Expr(:., :+), :x)
        @test parsestmt(Expr, "(.+).(x)") == Expr(:., Expr(:., :+), Expr(:tuple, :x))

        @test parsestmt(Expr, ".+")    == Expr(:., :+)
        @test parsestmt(Expr, ":.+")   == QuoteNode(Symbol(".+"))
        @test parsestmt(Expr, ":(.+)") == Expr(:quote, (Expr(:., :+)))
        @test parsestmt(Expr, "quote .+ end")   == Expr(:quote,
                                                        Expr(:block,
                                                             LineNumberNode(1),
                                                             Expr(:., :+)))
        @test parsestmt(Expr, ".+{x}") == Expr(:curly, Symbol(".+"), :x)

        # Quoted syntactic ops act different when in parens
        @test parsestmt(Expr, ":.=")   == QuoteNode(Symbol(".="))
        @test parsestmt(Expr, ":(.=)") == QuoteNode(Symbol(".="))

        # A few other cases of bare dotted ops
        @test parsestmt(Expr, "f(.+)")   == Expr(:call, :f, Expr(:., :+))
        @test parsestmt(Expr, "(a, .+)") == Expr(:tuple, :a, Expr(:., :+))
        @test parsestmt(Expr, "A.:.+")   == Expr(:., :A, QuoteNode(Symbol(".+")))
    end

    @testset "let" begin
        @test parsestmt(Expr, "let x=1\n end") ==
            Expr(:let, Expr(:(=), :x, 1),  Expr(:block, LineNumberNode(2)))
        @test parsestmt(Expr, "let x=1 ; end") ==
            Expr(:let, Expr(:(=), :x, 1),  Expr(:block, LineNumberNode(1)))
        @test parsestmt(Expr, "let x ; end") ==
            Expr(:let, :x, Expr(:block, LineNumberNode(1)))
        @test parsestmt(Expr, "let x::1 ; end") ==
            Expr(:let, Expr(:(::), :x, 1), Expr(:block, LineNumberNode(1)))
        @test parsestmt(Expr, "let x=1,y=2 end") ==
            Expr(:let, Expr(:block, Expr(:(=), :x, 1), Expr(:(=), :y, 2)), Expr(:block, LineNumberNode(1)))
        @test parsestmt(Expr, "let x+=1 ; end") ==
            Expr(:let, Expr(:block, Expr(:+=, :x, 1)), Expr(:block, LineNumberNode(1)))
        @test parsestmt(Expr, "let ; end") ==
            Expr(:let, Expr(:block), Expr(:block, LineNumberNode(1)))
        @test parsestmt(Expr, "let ; body end") ==
            Expr(:let, Expr(:block), Expr(:block, LineNumberNode(1), :body))
        @test parsestmt(Expr, "let\na\nb\nend") ==
            Expr(:let, Expr(:block), Expr(:block, LineNumberNode(2), :a, LineNumberNode(3), :b))
    end

    @testset "where" begin
        @test parsestmt(Expr, "A where {X, Y; Z}") == Expr(:where, :A, Expr(:parameters, :Z), :X, :Y)
    end

    @testset "macrocall" begin
        # line numbers
        @test parsestmt(Expr, "@m\n") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1))
        @test parsestmt(Expr, "\n@m") == Expr(:macrocall, Symbol("@m"), LineNumberNode(2))
        # parameters
        @test parsestmt(Expr, "@m(x; a)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1),
                                              Expr(:parameters, :a), :x)
        @test parsestmt(Expr, "@m(a=1; b=2)") == Expr(:macrocall, Symbol("@m"), LineNumberNode(1),
                                                  Expr(:parameters, Expr(:kw, :b, 2)), Expr(:(=), :a, 1))
        # @__dot__
        @test parsestmt(Expr, "@.") == Expr(:macrocall, Symbol("@__dot__"), LineNumberNode(1))
        @test parsestmt(Expr, "using A: @.") == Expr(:using, Expr(Symbol(":"), Expr(:., :A), Expr(:., Symbol("@__dot__"))))

        # var""
        @test parsestmt(Expr, "@var\"#\" a") == Expr(:macrocall, Symbol("@#"), LineNumberNode(1), :a)
        @test parsestmt(Expr, "A.@var\"#\" a") == Expr(:macrocall, Expr(:., :A, QuoteNode(Symbol("@#"))), LineNumberNode(1), :a)

        # Square brackets
        @test parsestmt(Expr, "@S[a,b]") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:vect, :a, :b))
        @test parsestmt(Expr, "@S[a b]") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:hcat, :a, :b))
        @test parsestmt(Expr, "@S[a; b]") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:vcat, :a, :b))
        @test parsestmt(Expr, "@S[a ;; b]", version=v"1.7") ==
            Expr(:macrocall, Symbol("@S"), LineNumberNode(1), Expr(:ncat, 2, :a, :b))
    end

    @testset "vect" begin
        @test parsestmt(Expr, "[x,y ; z]") == Expr(:vect, Expr(:parameters, :z), :x, :y)
    end

    @testset "try" begin
        @test parsestmt(Expr, "try x catch e; y end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y))
        @test parsestmt(Expr, "try x finally y end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 false,
                 false,
                 Expr(:block, LineNumberNode(1), :y))
        @test parsestmt(Expr, "try x catch e; y finally z end") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 Expr(:block, LineNumberNode(1), :z))
        @test parsestmt(Expr, "try x catch e; y else z end", version=v"1.8") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 false,
                 Expr(:block, LineNumberNode(1), :z))
        @test parsestmt(Expr, "try x catch e; y else z finally w end", version=v"1.8") ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :y),
                 Expr(:block, LineNumberNode(1), :w),
                 Expr(:block, LineNumberNode(1), :z))
        # finally before catch
        @test parsestmt(Expr, "try x finally y catch e z end", ignore_warnings=true) ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 :e,
                 Expr(:block, LineNumberNode(1), :z),
                 Expr(:block, LineNumberNode(1), :y))
        # empty recovery
        @test parsestmt(Expr, "try x end", ignore_errors=true) ==
            Expr(:try,
                 Expr(:block, LineNumberNode(1), :x),
                 false, false,
                 Expr(:block, Expr(:error)))
    end

    @testset "juxtapose" begin
        @test parsestmt(Expr, "2x") == Expr(:call, :*, 2, :x)
        @test parsestmt(Expr, "(2)(3)x") == Expr(:call, :*, 2, 3, :x)
    end

    @testset "Core.@doc" begin
        @test parsestmt(Expr, "\"x\" f") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@doc")), LineNumberNode(1), "x", :f)
        @test parsestmt(Expr, "\n\"x\" f") ==
            Expr(:macrocall, GlobalRef(Core, Symbol("@doc")), LineNumberNode(2), "x", :f)
    end

    @testset "return" begin
        @test parsestmt(Expr, "return x") == Expr(:return, :x)
        @test parsestmt(Expr, "return")  == Expr(:return, nothing)
    end

    @testset "struct" begin
        @test parsestmt(Expr, "struct A end") ==
            Expr(:struct, false, :A, Expr(:block, LineNumberNode(1)))
        @test parsestmt(Expr, "mutable struct A end") ==
            Expr(:struct, true, :A, Expr(:block, LineNumberNode(1)))

        @test parsestmt(Expr, "struct A <: B \n a::X \n end") ==
            Expr(:struct, false, Expr(:<:, :A, :B),
                 Expr(:block, LineNumberNode(2), Expr(:(::), :a, :X)))
        @test parsestmt(Expr, "struct A \n a \n b \n end") ==
            Expr(:struct, false, :A,
                 Expr(:block, LineNumberNode(2), :a, LineNumberNode(3), :b))
        @test parsestmt(Expr, "struct A const a end", version=v"1.8") ==
            Expr(:struct, false, :A, Expr(:block, LineNumberNode(1), Expr(:const, :a)))
    end

    @testset "export" begin
        @test parsestmt(Expr, "export a") == Expr(:export, :a)
        @test parsestmt(Expr, "export @a") == Expr(:export, Symbol("@a"))
        @test parsestmt(Expr, "export @var\"'\"") == Expr(:export, Symbol("@'"))
        @test parsestmt(Expr, "export a, \n @b") == Expr(:export, :a, Symbol("@b"))
        @test parsestmt(Expr, "export +, ==") == Expr(:export, :+, :(==))
        @test parsestmt(Expr, "export \n a") == Expr(:export, :a)
    end

    @testset "global/const/local" begin
        @test parsestmt(Expr, "global x") == Expr(:global, :x)
        @test parsestmt(Expr, "local x") == Expr(:local, :x)
        @test parsestmt(Expr, "global x,y") == Expr(:global, :x, :y)
        @test parsestmt(Expr, "global const x = 1") == Expr(:const, Expr(:global, Expr(:(=), :x, 1)))
        @test parsestmt(Expr, "local const x = 1") == Expr(:const, Expr(:local, Expr(:(=), :x, 1)))
        @test parsestmt(Expr, "const global x = 1") == Expr(:const, Expr(:global, Expr(:(=), :x, 1)))
        @test parsestmt(Expr, "const local x = 1") == Expr(:const, Expr(:local, Expr(:(=), :x, 1)))
        @test parsestmt(Expr, "const x,y = 1,2") == Expr(:const, Expr(:(=), Expr(:tuple, :x, :y), Expr(:tuple, 1, 2)))
        @test parsestmt(Expr, "const x = 1") == Expr(:const, Expr(:(=), :x, 1))
        @test parsestmt(Expr, "global x ~ 1") == Expr(:global, Expr(:call, :~, :x, 1))
        @test parsestmt(Expr, "global x += 1") == Expr(:global, Expr(:+=, :x, 1))
    end

    @testset "tuples" begin
        @test parsestmt(Expr, "(;)")       == Expr(:tuple, Expr(:parameters))
        @test parsestmt(Expr, "(; a=1)")   == Expr(:tuple, Expr(:parameters, Expr(:kw, :a, 1)))
        @test parsestmt(Expr, "(; a=1; b=2)") ==
            Expr(:tuple, Expr(:parameters, Expr(:parameters, Expr(:kw, :b, 2)), Expr(:kw, :a, 1)))
        @test parsestmt(Expr, "(a; b; c,d)") ==
            Expr(:tuple, Expr(:parameters, Expr(:parameters, :c, :d), :b), :a)
    end

    @testset "module" begin
        @test parsestmt(Expr, "module A end") ==
            Expr(:module, true,  :A, Expr(:block, LineNumberNode(1), LineNumberNode(1)))
        @test parsestmt(Expr, "baremodule A end") ==
            Expr(:module, false, :A, Expr(:block, LineNumberNode(1), LineNumberNode(1)))
    end


    @testset "errors" begin
        @test parsestmt(Expr, "--", ignore_errors=true) ==
            Expr(:error, "invalid operator: `--`")
        @test parseall(Expr, "a b", ignore_errors=true) ==
            Expr(:toplevel, LineNumberNode(1), :a,
                 LineNumberNode(1), Expr(:error, :b))
        @test parsestmt(Expr, "(x", ignore_errors=true) ==
            Expr(:block, :x, Expr(:error))
    end

    @testset "import" begin
        @test parsestmt(Expr, "import A") == Expr(:import, Expr(:., :A))
        @test parsestmt(Expr, "import A.(:b).:c: x.:z", ignore_warnings=true) ==
            Expr(:import, Expr(Symbol(":"), Expr(:., :A, :b, :c), Expr(:., :x, :z)))
        # Stupid parens and quotes in import paths
        @test parsestmt(Expr, "import A.:+", ignore_warnings=true) ==
            Expr(:import, Expr(:., :A, :+))
        @test parsestmt(Expr, "import A.(:+)", ignore_warnings=true) ==
            Expr(:import, Expr(:., :A, :+))
        @test parsestmt(Expr, "import A.:(+)", ignore_warnings=true) ==
            Expr(:import, Expr(:., :A, :+))
        @test parsestmt(Expr, "import A.:(+) as y", ignore_warnings=true, version=v"1.6") ==
            Expr(:import, Expr(:as, Expr(:., :A, :+), :y))
    end
end
