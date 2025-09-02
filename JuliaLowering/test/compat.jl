using Test
using JuliaSyntax
using JuliaLowering
const JS = JuliaSyntax
const JL = JuliaLowering

@testset "expr->syntaxtree" begin
    @testset "semantics only" begin
        # Test that `s` evaluates to the same thing both under normal parsing
        # and with the expr->tree->expr transformation

        programs = [
            "let x = 2; x += 5; x -= 1; [1] .*= 1; end",
            "let var\"x\" = 123; x; end",
            "try; 1; catch e; e; else; 2; finally; 3; end",
            "for x in 1:2, y in 3:4; x + y; end",
            "[x+y for x in 1:2, y in 3:4]",
            "Int[x+y for x in 1:2, y in 3:4 if true]",
            "for x in 1; x+=1\n if true\n continue \n elseif false \n break\n end\n end",
            "Base.Meta.@lower 1",
            "function foo(x, y=1; z, what::Int=5); x + y + z + what; end; foo(1,2;z=3)",
            "(()->1)()",
            "((x)->2)(3)",
            "((x,y)->4)(5,6)",
            "filter([1,2,3]) do x; x > 1; end",
            """
            struct X
                f1::Int # hi
                "foo"
                f2::Int
                f3::Int
                X(y) = new(y,y,y)
            end
            """,
            "global x,y",
            "global (x,y)",
            "999999999999999999999999999999999999999",
            "0x00000000000000001",
            "(0x00000000000000001)",
            "let x = 1; 2x; end",
            "let x = 1; (2)(3)x; end",
            "if false\n1\nelseif true\n 3\nend",
            "\"str\"",
            "\"\$(\"str\")\"",
            "'a'",
            "'α'",
            "'\\xce\\xb1'",
            "let x = 1; \"\"\"\n  a\n  \$x\n  b\n  c\"\"\"; end",
            "try throw(0) catch e; 1 end",
            "try 0 finally 1 end",
            "try throw(0) catch e; 1 finally 2 end",
            "try throw(0) catch e; 1 else 2 end",
            "try throw(0) catch e; 1 else 2 finally 3 end",
            "try throw(0) finally 1 catch e; 2 end",
            ":.+",
            ":.=",
            ":(.=)",
            ":+=",
            ":(+=)",
            ":.+=",
            ":(.+=)",
        ]

        test_mod_1 = Module()
        test_mod_2 = Module()

        for p in programs
            @testset "`$p`" begin
                local good_expr, good_out, test_st, test_expr, test_out
                try
                    good_expr = JS.parseall(Expr, p; ignore_errors=true)
                    good_out = Core.eval(test_mod_1, good_expr)
                catch e
                    @error "Couldn't eval the reference expression---fix your test"
                    rethrow(e)
                end

                test_st = JuliaLowering.expr_to_syntaxtree(good_expr)
                test_expr = Expr(test_st)
                test_out = Core.eval(test_mod_2, test_expr)

                @test good_out == test_out
            end
        end
    end

    # Remove any information that can't be recovered from an Expr
    function normalize_st!(st)
        k = JS.kind(st)
        args = JS.children(st)

        if JS.is_infix_op_call(st) && (k === K"call" || k === K"dotcall")
            # Infix calls are not preserved in Expr; we need to re-order the children
            pre_st_args = JL.NodeId[st[2]._id, st[1]._id]
            for c in st[3:end]
                push!(pre_st_args, c._id)
            end
            pre_st_flags = (JS.flags(st) & ~JS.INFIX_FLAG) | JS.PREFIX_CALL_FLAG
            JL.setchildren!(st._graph, st._id, pre_st_args)
            JL.setflags!(st._graph, st._id, pre_st_flags)
        elseif JS.is_postfix_op_call(st) && (k === K"call" || k === K"dotcall")
            pre_st_args = JL.NodeId[st[end]._id]
            for c in st[1:end-1]
                push!(pre_st_args, c._id)
            end
            pre_st_flags = (JS.flags(st) & ~JS.POSTFIX_OP_FLAG) | JS.PREFIX_CALL_FLAG
            JL.setchildren!(st._graph, st._id, pre_st_args)
            JL.setflags!(st._graph, st._id, pre_st_flags)
        elseif k in JS.KSet"tuple block macrocall"
            JL.setflags!(st._graph, st._id, JS.flags(st) & ~JS.PARENS_FLAG)
        elseif k === K"toplevel"
            JL.setflags!(st._graph, st._id, JS.flags(st) & ~JS.TOPLEVEL_SEMICOLONS_FLAG)
        end

        if k in JS.KSet"tuple call dotcall macrocall vect curly braces <: >:"
            JL.setflags!(st._graph, st._id, JS.flags(st) & ~JS.TRAILING_COMMA_FLAG)
        end

        k === K"quote" && JL.setflags!(st._graph, st._id, JS.flags(st) & ~JS.COLON_QUOTE)
        k === K"wrapper" && JL.sethead!(st._graph, st._id, K"block")

        # All ops are prefix ops in an expr.
        # Ignore trivia (shows up on some K"error"s)
        JL.setflags!(st._graph, st._id, JS.flags(st) &
            ~JS.PREFIX_OP_FLAG & ~JS.INFIX_FLAG & ~JS.TRIVIA_FLAG & ~JS.NON_TERMINAL_FLAG)

        for c in JS.children(st)
            normalize_st!(c)
        end
        return st
    end

    function st_roughly_equal(; st_good, st_test)
        normalize_st!(st_good)

        if kind(st_good) === kind(st_test) === K"error"
            # We could consider some sort of equivalence later, but we would
            # need to specify within JS what the error node contains.
            return true
        end

        out = kind(st_good) === kind(st_test) &&
            JS.flags(st_good) === JS.flags(st_test) &&
            JS.numchildren(st_good) === JS.numchildren(st_test) &&
            JS.is_leaf(st_good) === JS.is_leaf(st_test) &&
            get(st_good, :value, nothing) === get(st_test, :value, nothing) &&
            get(st_good, :name_val, nothing) === get(st_test, :name_val, nothing) &&
            all(map((cg, ct)->st_roughly_equal(;st_good=cg, st_test=ct),
                    JS.children(st_good), JS.children(st_test)))

        !out && @warn("!st_roughly_equal (normalized_reference, st_test):",
                      JS.sourcetext(st_good), st_good, st_test)
        return out
    end

    @testset "SyntaxTree equivalence (tests taken from JuliaSyntax expr.jl)" begin
        # test that string->tree->expr->tree ~= string->tree
        #                             ^^
        programs = [
            "begin a\nb\n\nc\nend",
            "(a;b;c)",
            "begin end",
            "(;;)",
            "a;b",
            "module A\n\nbody\nend",
            "function f()\na\n\nb\nend",
            "f() = 1",
            "macro f()\na\nend",
            "function f end",
            "macro f end",
            "function (f() where {T}) end",
            "function (f()::S) end",
            "a -> b",
            "(a,) -> b",
            "(a where {T}) -> b",
            "a -> (\nb;c)",
            "a -> begin\nb\nc\nend",
            "(a;b=1) -> c",
            "(a...;b...) -> c",
            "(;) -> c",
            "a::T -> b",
            "let i=is, j=js\nbody\nend",
            "for x=xs\n\nend",
            "for x=xs\ny\nend",
            "while cond\n\nend",
            "while cond\ny\nend",
            "f() = xs",
            "f() =\n(a;b)",
            "f() =\nbegin\na\nb\nend",
            "let f(x) =\ng(x)=1\nend",
            "f() .= xs",
            "for i=is body end",
            "for i=is, j=js\nbody\nend",
            "f(x) do y\n body end",
            "@f(x) do y body end",
            "f(x; a=1) do y body end",
            "g(f(x) do y\n body end)",
            "f(a=1)",
            "f(; b=2)",
            "f(a=1; b=2)",
            "f(a; b; c)",
            "+(a=1,)",
            "(a=1)()",
            "(x=1) != 2",
            "+(a=1)",
            "(a=1)'",
            "f.(a=1; b=2)",
            "(a=1,)",
            "(a=1,; b=2)",
            "(a=1,; b=2; c=3)",
            "x[i=j]",
            "(i=j)[x]",
            "x[a, b; i=j]",
            "(i=j){x}",
            "x{a, b; i=j}",
            "[a=1,; b=2]",
            "{a=1,; b=2}",
            "f(a .= 1)",
            "f(((a = 1)))",
            "(((a = 1)),)",
            "(;((a = 1)),)",
            "a.b",
            "a.@b x",
            "f.(x,y)",
            "f.(x=1)",
            "f.(a=1; b=2)",
            "(a=1).()",
            "x .+ y",
            "(x=1) .+ y",
            "a .< b .< c",
            "a .< (.<) .< c",
            "quote .+ end",
            ".+(x)",
            ".+x",
            "f(.+)",
            "(a, .+)",
            "x += y",
            "x .+= y",
            "x \u2212= y",
            "let x=1\n end",
            "let x=1 ; end",
            "let x ; end",
            "let x::1 ; end",
            "let x=1,y=2 end",
            "let x+=1 ; end",
            "let ; end",
            "let ; body end",
            "let\na\nb\nend",
            "A where {T}",
            "A where {S, T}",
            "A where {X, Y; Z}",
            "@m\n",
            "\n@m",
            "@m(x; a)",
            "@m(a=1; b=2)",
            "@S[a,b]",
            "@S[a b]",
            "@S[a; b]",
            "@S[a ;; b]",
            "[x,y ; z]",
            "[a ;;; b ;;;; c]",
            "[a b ; c d]",
            "[a\nb]",
            "[a b]",
            "[a b ; c d]",
            "T[a ;;; b ;;;; c]",
            "T[a b ; c d]",
            "T[a\nb]",
            "T[a b]",
            "T[a b ; c d]",
            "(x for a in as for b in bs)",
            "(x for a in as, b in bs)",
            "(x for a in as, b in bs if z)",
            "(x for a in as, b in bs for c in cs, d in ds)",
            "(x for a in as for b in bs if z)",
            "(x for a in as if z for b in bs)",
            "[x for a = as for b = bs if cond1 for c = cs if cond2]" ,
            "[x for a = as if begin cond2 end]" ,
            "(x for a in as if z)",
            "return x",
            "struct A end",
            "mutable struct A end",
            "struct A <: B \n a::X \n end",
            "struct A \n a \n b \n end",
            "struct A const a end",
            "export a",
            "export +, ==",
            "export \n a",
            "global x",
            "local x",
            "global x,y",
            "const x,y = 1,2",
            "const x = 1",
            "global x ~ 1",
            "global x += 1",
            "(;)",
            "(; a=1)",
            "(; a=1; b=2)",
            "(a; b; c,d)",
            "module A end",
            "baremodule A end",
            "import A",
            "A.x",
            "A.\$x",
        ]

        for p in programs
            @testset "`$p`" begin
                st_good = JS.parsestmt(JL.SyntaxTree, p; ignore_errors=true)
                st_test = JL.expr_to_syntaxtree(Expr(st_good))
                @test st_roughly_equal(;st_good, st_test)
            end
        end
    end

    @testset "provenance via scavenging for LineNumberNodes" begin
        # Provenenance of a node should be the last seen LineNumberNode in the
        # depth-first traversal of the Expr, or the initial line given if none
        # have been seen yet.  If none have been seen and no initial line was
        # given, .source should still be defined on all nodes (of unspecified
        # value, but hopefully a helpful value for the user.)
        ex = Expr(:block,
                  LineNumberNode(123),
                  Expr(:block,
                       Expr(:block, LineNumberNode(456)),
                       Expr(:block)),
                  Expr(:block,
                       Expr(:block),
                       Expr(:block)))

        # No initial line provided
        st = JuliaLowering.expr_to_syntaxtree(ex)
        for i in length(st._graph.edge_ranges)
            @test !isnothing(get(SyntaxTree(st._graph, i), :source, nothing))
        end
        @test let lnn = st[1].source;    lnn isa LineNumberNode && lnn.line === 123; end
        @test let lnn = st[1][1].source; lnn isa LineNumberNode && lnn.line === 123; end
        @test let lnn = st[1][2].source; lnn isa LineNumberNode && lnn.line === 456; end
        @test let lnn = st[2].source;    lnn isa LineNumberNode && lnn.line === 456; end
        @test let lnn = st[2][1].source; lnn isa LineNumberNode && lnn.line === 456; end
        @test let lnn = st[2][2].source; lnn isa LineNumberNode && lnn.line === 456; end

        # Same tree, but provide an initial line
        st = JuliaLowering.expr_to_syntaxtree(ex, LineNumberNode(789))
        @test let lnn = st.source;       lnn isa LineNumberNode && lnn.line === 789; end
        @test let lnn = st[1].source;    lnn isa LineNumberNode && lnn.line === 123; end
        @test let lnn = st[1][1].source; lnn isa LineNumberNode && lnn.line === 123; end
        @test let lnn = st[1][2].source; lnn isa LineNumberNode && lnn.line === 456; end
        @test let lnn = st[2].source;    lnn isa LineNumberNode && lnn.line === 456; end
        @test let lnn = st[2][1].source; lnn isa LineNumberNode && lnn.line === 456; end
        @test let lnn = st[2][2].source; lnn isa LineNumberNode && lnn.line === 456; end

        ex = parsestmt(Expr, """
        begin
            try
                maybe
                lots
                of
                lines
            catch exc
                y
            end
        end""")
        st = JuliaLowering.expr_to_syntaxtree(ex, LineNumberNode(1))

        # sanity: ensure we're testing the tree we expect
        @test kind(st) === K"block"
        @test kind(st[1]) === K"try"
        @test kind(st[1][1]) === K"block"
        @test kind(st[1][1][1]) === K"Identifier" && st[1][1][1].name_val === "maybe"
        @test kind(st[1][1][2]) === K"Identifier" && st[1][1][2].name_val === "lots"
        @test kind(st[1][1][3]) === K"Identifier" && st[1][1][3].name_val === "of"
        @test kind(st[1][1][4]) === K"Identifier" && st[1][1][4].name_val === "lines"
        @test kind(st[1][2]) === K"catch"
        @test kind(st[1][2][1]) === K"Identifier" && st[1][2][1].name_val === "exc"
        @test kind(st[1][2][2]) === K"block"
        @test kind(st[1][2][2][1]) === K"Identifier" && st[1][2][2][1].name_val === "y"

        @test let lnn = st.source;             lnn isa LineNumberNode && lnn.line === 1; end
        @test let lnn = st[1].source;          lnn isa LineNumberNode && lnn.line === 2; end
        @test let lnn = st[1][1].source;       lnn isa LineNumberNode && lnn.line === 2; end
        @test let lnn = st[1][1][1].source;    lnn isa LineNumberNode && lnn.line === 3; end
        @test let lnn = st[1][1][2].source;    lnn isa LineNumberNode && lnn.line === 4; end
        @test let lnn = st[1][1][3].source;    lnn isa LineNumberNode && lnn.line === 5; end
        @test let lnn = st[1][1][4].source;    lnn isa LineNumberNode && lnn.line === 6; end
        @test let lnn = st[1][2].source;       lnn isa LineNumberNode && lnn.line === 6; end
        @test let lnn = st[1][2][1].source;    lnn isa LineNumberNode && lnn.line === 6; end
        @test let lnn = st[1][2][2].source;    lnn isa LineNumberNode && lnn.line === 6; end
        @test let lnn = st[1][2][2][1].source; lnn isa LineNumberNode && lnn.line === 8; end

    end
end
