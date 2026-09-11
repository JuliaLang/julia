using .JuliaSyntax: SyntaxTree, SyntaxList, @stm, prov, prov_end, provenance,
    macro_prov, macro_prov_end, flattened_provenance, sourceref,
    unexpanded_sourceref, newleaf, newnode, mkleaf, mknode, mktree, copy_ast,
    unalias_nodes, annotate_parent!, _setattr!, getmeta, SyntaxContext,
    ScopeLayer, children

"""
Build a hand-made tree for the DAG-shaped tests below.  Each node carries a
distinct integer in `.value` so nodes copied by `unalias_nodes` and friends can
be traced back to the node they were copied from.
"""
function tnode(tag::Int, cs::SyntaxTree...)
    st = isempty(cs) ?
        newleaf(LineNumberNode(tag), K"Value") :
        newnode(LineNumberNode(tag), K"block", SyntaxList(cs...))
    _setattr!(st, :value, tag)
end

"All nodes of `st` in preorder, with one entry per occurrence"
function flat_nodes(st::SyntaxTree, out=SyntaxList())
    push!(out, st)
    for c in children(st)
        flat_nodes(c, out)
    end
    out
end

@testset "SyntaxTree parsing" begin
    # Errors should fall through
    @test parsestmt(SyntaxTree, ""; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, " "; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "@"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "@@@"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "(a b c)"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "'a b c'"; ignore_errors=true) isa SyntaxTree
    # Malformed literals become ErrorVal-valued leaves rather than identifiers
    @test parsestmt(SyntaxTree, "1.e"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "x = 1._"; ignore_errors=true) isa SyntaxTree
end

@testset "SyntaxTree type stability" begin
    st0 = parsestmt(SyntaxTree, "f(::Int)")
    # `children` must not leak the `Union{Nothing}` of the raw field into inference.
    @test @inferred(children(st0)) isa Vector{SyntaxTree}
    @test @inferred(prov(st0)) isa SyntaxTree
end

@testset "SyntaxTree provenance accessors" begin

    @testset "prov, prov_end, provenance, sourceref" begin
        # st3 <- st2 <- st1, with st3 referring to source text
        st3 = tnode(3)
        st2 = mkleaf(st3)
        st1 = mkleaf(st2)

        @test prov(st1) === st2
        @test prov(prov(st1)) === st3
        @test prov(prov(prov(st1))) === st3

        @test prov_end(st1) === st3
        @test prov_end(prov_end(st1)) === st3

        @test sourceref(st1) == LineNumberNode(3)
        @test sourceref(prov_end(st1)) == LineNumberNode(3)

        @test provenance(st1) == SyntaxList(st2, st3)
        @test provenance(prov_end(st1)) == SyntaxList()
    end

    @testset "flattened_provenance" begin
        ctx_with_unexpanded(u) = SyntaxContext(
            ScopeLayer(JuliaSyntax, nothing),
            u,
            v"0.0",
            false)

        st1 = _setattr!(newleaf(LineNumberNode(1), K"Identifier"), :value, "st1")
        st2 = _setattr!(mkleaf(st1), :value, "st2")
        st3 = _setattr!(mkleaf(st2), :value, "st3")

        stm1 = _setattr!(newleaf(LineNumberNode(1, :m), K"Identifier"), :value, "stm1")
        stm2 = _setattr!(mkleaf(stm1), :value, "stm2")
        stm3 = _setattr!(mkleaf(stm1), :value, "stm3")
        stm_unused = _setattr!(newleaf(LineNumberNode(0), K"Identifier"), :value, "stm_unused")

        stmm1 = _setattr!(newleaf(LineNumberNode(1, :mm), K"Identifier"), :value, "stmm1")
        stmm2 = _setattr!(mkleaf(stmm1), :value, "stmm2")
        stmm3 = _setattr!(mkleaf(stmm2), :value, "stmm3")

        _setattr!(st1, :context, ctx_with_unexpanded(stm_unused))
        _setattr!(st2, :context, ctx_with_unexpanded(stm_unused))
        _setattr!(st3, :context, ctx_with_unexpanded(stm3))
        _setattr!(stm3, :context, ctx_with_unexpanded(stmm3))

        # julia> JL._show_provtree(stdout, st3, "")
        # st3
        # ├─ st2
        # │  ├─ st1
        # │  │  ├─ @ nothing:1
        # │  │  └─ stm_unused
        # │  │     └─ @ nothing:0
        # │  └─ stm_unused
        # │     └─ @ nothing:0
        # └─ stm3
        #    ├─ stm1
        #    │  └─ @ m:1
        #    └─ stmm3
        #       └─ stmm2
        #          └─ stmm1
        #             └─ @ mm:1

        @test macro_prov(st3) == stm3
        @test macro_prov(st2) == stm_unused
        @test macro_prov(st1) == stm_unused
        @test macro_prov(stm3) == stmm3
        @test macro_prov(stm2) == nothing
        @test macro_prov(stm1) == nothing
        @test macro_prov(stmm3) == nothing
        @test macro_prov(stmm2) == nothing
        @test macro_prov(stmm1) == nothing
        @test macro_prov_end(st3) == stmm3
        @test macro_prov_end(st2) == stm_unused
        @test macro_prov_end(st1) == stm_unused
        @test macro_prov_end(stm3) == stmm3
        @test macro_prov_end(stm2) == nothing
        @test macro_prov_end(stm1) == nothing
        @test macro_prov_end(stmm3) == nothing
        @test macro_prov_end(stmm2) == nothing
        @test macro_prov_end(stmm1) == nothing
        @test unexpanded_sourceref(st3) == LineNumberNode(1, :mm)
        @test unexpanded_sourceref(st2) == LineNumberNode(0)
        @test unexpanded_sourceref(st1) == LineNumberNode(0)
        @test unexpanded_sourceref(stm3) == LineNumberNode(1, :mm)
        @test unexpanded_sourceref(stm2) == LineNumberNode(1, :m)
        @test unexpanded_sourceref(stm1) == LineNumberNode(1, :m)
        @test unexpanded_sourceref(stmm3) == LineNumberNode(1, :mm)
        @test unexpanded_sourceref(stmm2) == LineNumberNode(1, :mm)
        @test unexpanded_sourceref(stmm1) == LineNumberNode(1, :mm)
        @test flattened_provenance(st3) == SyntaxList(stmm1, stm1, st1)
        @test flattened_provenance(st2) == SyntaxList(stm_unused, st1)
        @test flattened_provenance(st1) == SyntaxList(stm_unused, st1)
        @test flattened_provenance(stm3) == SyntaxList(stmm1, stm1)
        @test flattened_provenance(stm2) == SyntaxList(stm1)
        @test flattened_provenance(stm1) == SyntaxList(stm1)
        @test flattened_provenance(stmm3) == SyntaxList(stmm1)
        @test flattened_provenance(stmm2) == SyntaxList(stmm1)
        @test flattened_provenance(stmm1) == SyntaxList(stmm1)
    end
end

@testset "SyntaxTree utils" begin
    @testset "copy_ast, mktree" begin
        # A one-child tree whose root also has a provenance chain of its own
        leaf = tnode(3)
        st2 = newnode(LineNumberNode(1), K"block", SyntaxList(leaf))
        st = mknode(st2, children(st2))   # st.source === st2

        stcopy = copy_ast(st)
        @test stcopy !== st
        @test st ≈ stcopy
        @test stcopy[1] !== st[1]
        # `.source` chains are copied too
        @test prov(stcopy) !== prov(st)
        @test prov(st) ≈ prov(stcopy)

        # Every node is copied at most once, so aliasing is preserved
        shared = tnode(1)
        aliased = newnode(LineNumberNode(0), K"block", SyntaxList(shared, shared))
        acopy = copy_ast(aliased)
        @test aliased ≈ acopy
        @test acopy[1] !== shared
        @test acopy[1] === acopy[2]

        # Unlike copy_ast, mktree extends the provenance chain rather than
        # copying it
        stcopy2 = mktree(st)
        @test stcopy2 !== st
        @test st ≈ stcopy2
        @test stcopy2[1] !== st[1]
        @test stcopy2.source === st
        @test stcopy2[1].source === st[1]
    end

    @testset "unalias_nodes" begin
        # 1 -+-> 2 -+
        #    |      +-> 4
        #    +-> 3 -+
        build1() = let n4 = tnode(4)
            tnode(1, tnode(2, n4), tnode(3, n4))
        end
        ref = build1()
        st = build1()
        src4 = st[1][1].source
        stu = unalias_nodes(st)
        @test stu === st                    # unaliases in place
        @test ref ≈ stu
        @test length(flat_nodes(stu)) == 5  # node 4 copied once
        @test allunique(flat_nodes(stu))
        # the copy keeps node 4's attributes, and doesn't extend its provenance
        @test 4 == stu[1][1].value == stu[2][1].value
        @test src4 === stu[1][1].source === stu[2][1].source

        #           +-> 5
        #           |
        # 1 -+-> 2 -+---->>>-> 6
        #    |           |||
        #    +-> 3 -> 7 -+||
        #    |            ||
        #    +-> 4 -+-----+|
        #           |      |
        #           +------+
        build2() = let n6 = tnode(6)
            tnode(1,
                  tnode(2, tnode(5), n6),
                  tnode(3, tnode(7, n6)),
                  tnode(4, n6, n6))
        end
        ref = build2()
        stu = unalias_nodes(build2())
        @test ref ≈ stu
        # node 6 occurs four times, so it should be copied three times
        @test length(flat_nodes(stu)) == 10
        @test allunique(flat_nodes(stu))
        @test 6 == stu[1][2].value == stu[2][1][1].value ==
            stu[3][1].value == stu[3][2].value

        # 1 -+-> 2 ->-> 4 -+----> 5 ->-> 7
        #    |      |      |         |
        #    +-> 3 -+      +-->-> 6 -+
        #        |            |
        #        +------------+
        build3() = let n7 = tnode(7),
                       n5 = tnode(5, n7),
                       n6 = tnode(6, n7),
                       n4 = tnode(4, n5, n6)
            tnode(1, tnode(2, n4), tnode(3, n4, n6))
        end
        ref = build3()
        stu = unalias_nodes(build3())
        @test ref ≈ stu
        @test length(flat_nodes(stu)) == 15
        @test allunique(flat_nodes(stu))
        # attrs of nodes 4-7 survive copying
        @test 4 == stu[1][1].value == stu[2][1].value
        @test 5 == stu[1][1][1].value == stu[2][1][1].value
        @test 6 == stu[1][1][2].value == stu[2][1][2].value == stu[2][2].value
        @test 7 == stu[1][1][1][1].value == stu[1][1][2][1].value ==
            stu[2][1][1][1].value == stu[2][1][2][1].value == stu[2][2][1].value
    end

    @testset "annotate_parent" begin
        chk_parent(st, parent) = getmeta(st, :parent, nothing) === parent &&
            all(c->chk_parent(c, st), children(st))
        # 1 -+-> 2 ->-> 4 --> 5
        #    |      |
        #    +-> 3 -+
        st = let n4 = tnode(4, tnode(5))
            tnode(1, tnode(2, n4), tnode(3, n4))
        end
        st = annotate_parent!(st)
        @test chk_parent(st, nothing)
    end
end

@testset "SyntaxList" begin
    st = parsestmt(SyntaxTree, "function foo end")

    sl0 = SyntaxList()
    @test sl0 isa SyntaxList
    @test length(sl0) == 0

    sl1 = SyntaxList(st)
    @test sl1 isa SyntaxList
    @test length(sl1) == 1
    @test sl1[1] === st

    sl2 = SyntaxList(st, st)
    @test sl2 isa SyntaxList
    @test length(sl2) == 2
    @test sl2[2] === st
end

@testset "@stm SyntaxTree pattern-matching" begin
    st = parsestmt(SyntaxTree, "foo(a,b=1,c(d=2))")
    # (call foo a (kw b 1) (call c (kw d 2)))

    @testset "basic functionality" begin
        @test @stm st begin
            _ -> true
        end

        @test @stm st begin
            x -> x isa SyntaxTree
        end

        @test @stm st begin
            [K"function" f a b c] -> false
            [K"call" f a b c] -> true
        end

        @test @stm st begin
            [K"function" _ _ _ _] -> false
            [K"call" _ _ _ _] -> true
        end

        @test @stm st begin
            [K"call" f a b] -> false
            [K"call" f a b c d] -> false
            [K"call" f a b c] -> true
        end

        @test @stm st begin
            [K"call" f a b c] ->
                kind(f) === K"Identifier" &&
                kind(b) === K"kw" &&
                kind(c) === K"call"
        end
    end

    @testset "errors" begin
        # no match
        @test_throws ErrorException @stm st begin
            [K"Identifier"] -> false
        end

        # assuming we run this checker by default
        @testset "_stm_check_usage" begin
            bad = Expr[
                :(@stm st begin
                      [a] -> false
                  end)
                :(@stm st begin
                      [K"None",a] -> false
                  end)
                :(@stm st begin
                      [K"None" a a] -> false
                  end)
                :(@stm st begin
                      x
                  end)
                :(@stm st begin
                      x() -> false
                  end)
                :(@stm st begin
                      (a, b=1) -> false
                  end)
                :(@stm st begin
                      [K"None" a... b...] -> false
                  end)
            ]
            for e in bad
                Base.remove_linenums!(e)
                @testset "$(string(e))" begin
                @test_throws AssertionError macroexpand(@__MODULE__, e)
                end
            end
        end
    end

    @testset "nested patterns" begin
        @test 1 === @stm st begin
            [K"call" [K"Identifier"] [K"Identifier"] [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" [K"Identifier"] k2]]] -> 1
            [K"call" [K"Identifier"] [K"Identifier"] [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" _ k2]]] -> 2
            [K"call" [K"Identifier"] [K"Identifier"] [K"kw" _ k1] [K"call" _ _]] -> 3
            [K"call" [K"Identifier"] [K"Identifier"] _ _ ] -> 4
            [K"call" _ _ _ _] -> 5
        end
        @test 1 === @stm st begin
            [K"call" _ _ [K"None" [K"Identifier"] k1] [K"None" [K"Identifier"] [K"None" [K"None"] k2]]] -> 5
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"None" [K"Identifier"] [K"None" [K"None"] k2]]] -> 4
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"None" [K"None"] k2]]] -> 3
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" [K"None"] k2]]] -> 2
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" [K"Identifier"] k2]]] -> 1
        end
        @test 1 === @stm st begin
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" [K"Identifier"] k2] bad]] -> 4
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" [K"Identifier"] k2 bad]]] -> 3
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" [K"Identifier" bad] k2]]] -> 2
            [K"call" _ _ [K"kw" [K"Identifier"] k1] [K"call" [K"Identifier"] [K"kw" [K"Identifier"] k2]]] -> 1
        end
    end

    @testset "vcat form (newlines in pattern)" begin
        @test @stm st begin
            [K"call"
             f
             a
             b
             c] -> true
        end
        @test @stm st begin
            [K"call"
             f a b c] -> true
        end
        @test @stm st begin
            [K"call"


             f a b c] -> true
        end
        @test @stm st begin
            [K"call"
             [K"Identifier"] [K"Identifier"]
             [K"kw" [K"Identifier"] k1]
             [K"call"
              [K"Identifier"]
              [K"kw"
               [K"Identifier"]
               k2]]] -> true
        end
    end

    @testset "SyntaxList splat matching" begin
        # NB: a splat binds a view of the parent's children, not a SyntaxList
        # trailing splat
        @test @stm st begin
            [K"call" f _...] -> true
        end
        @test @stm st begin
            [K"call" f args...] -> kind(f) === K"Identifier"
        end
        @test @stm st begin
            [K"call" f args...] ->
                args isa AbstractVector{SyntaxTree} && length(args) === 3
        end
        @test @stm st begin
            [K"call" f args...] -> kind(args[1]) === K"Identifier" &&
                kind(args[2]) === K"kw" &&
                kind(args[3]) === K"call"
        end
        @test @stm st begin
            [K"call" f a b c empty...] ->
                empty isa AbstractVector{SyntaxTree} && length(empty) === 0
        end

        # binds after splat
        @test @stm st begin
            [K"call" f args... last] ->
                args isa AbstractVector{SyntaxTree} &&
                length(args) === 2
        end
        @test @stm st begin
            [K"call" f args... last] ->
                kind(f) === K"Identifier" &&
                kind(args[1]) === K"Identifier" &&
                kind(args[2]) === K"kw" &&
                kind(last) === K"call"
        end
        @test @stm st begin
            [K"call" empty... f a b c] ->
                empty isa AbstractVector{SyntaxTree} && length(empty) === 0
        end
    end

    @testset "`when` clauses affect matching" begin
        @test @stm st begin
            (_, when=false) -> false
            (_, when=true) -> true
        end
        @test @stm st begin
            ([K"call" _...], when=false) -> false
            ([K"call" _...], when=true) -> true
        end
        @test @stm st begin
            ([K"call" _ _...], when=kind(st[1])===K"Identifier") -> true
        end
        @test @stm st begin
            ([K"call" f _...], when=kind(f)===K"Identifier") -> true
        end
    end

    @testset "effects of when=cond" begin
        let x = Int[]
            @test @stm st begin
                (_, when=(push!(x, 1); true)) -> x == [1]
            end
            empty!(x)

            @test @stm st begin
                (_, when=(push!(x, 1); false)) -> false
                (_, when=(push!(x, 2); false)) -> false
                (_, when=(push!(x, 3); true)) -> x == [1, 2, 3]
            end
            empty!(x)

            @test @stm st begin
                ([K"block"], when=(push!(x, 123); false)) -> false
                (_, when=(push!(x, 1); true)) -> x == [1]
            end
            empty!(x)

            @test @stm st begin
                (x_pat, when=((x_when = x_pat); true)) -> x_pat == x_when
            end
        end
    end
end
