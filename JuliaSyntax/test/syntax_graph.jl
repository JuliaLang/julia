using .JuliaSyntax: SyntaxGraph, SyntaxTree, SyntaxList, freeze_attrs, unfreeze_attrs, ensure_attributes, ensure_attributes!, delete_attributes, copy_ast, attrdefs, @stm, NodeId, SourceRef, SourceAttrType, Kind

@testset "SyntaxGraph attrs" begin
    st = parsestmt(SyntaxTree, "function foo end")
    g_init = unfreeze_attrs(st._graph)
    gf1 = freeze_attrs(g_init)
    gu1 = unfreeze_attrs(gf1)

    # Check that a default graph has required attrs
    g_empty = SyntaxGraph()
    @test (:kind=>Kind) in attrdefs(g_empty)
    @test (:source=>SourceAttrType) in attrdefs(g_empty)
    @test (:value=>Any) in attrdefs(g_empty)
    @test (:name_val=>String) in attrdefs(g_empty)

    # Check that freeze/unfreeze do their jobs
    @test gf1.attributes isa NamedTuple
    @test gu1.attributes isa Dict
    @test Set(keys(gf1.attributes)) == Set(keys(gu1.attributes))

    # ensure_attributes
    gf2 = ensure_attributes(gf1, test_attr=Symbol, foo=Type)
    gu2 = ensure_attributes(gu1, test_attr=Symbol, foo=Type)
    # returns a graph with the same attribute storage
    @test gf2.attributes isa NamedTuple
    @test gu2.attributes isa Dict
    # does its job
    @test (:test_attr=>Symbol) in attrdefs(gf2)
    @test (:foo=>Type) in attrdefs(gf2)
    @test Set(keys(gf2.attributes)) == Set(keys(gu2.attributes))
    # no mutation
    @test !((:test_attr=>Symbol) in attrdefs(gf1))
    @test !((:foo=>Type) in attrdefs(gf1))
    @test Set(keys(gf1.attributes)) == Set(keys(gu1.attributes))

    # delete_attributes
    gf3 = delete_attributes(gf2, :test_attr, :foo)
    gu3 = delete_attributes(gu2, :test_attr, :foo)
    # returns a graph with the same attribute storage
    @test gf3.attributes isa NamedTuple
    @test gu3.attributes isa Dict
    # does its job
    @test !((:test_attr=>Symbol) in attrdefs(gf3))
    @test !((:foo=>Type) in attrdefs(gf3))
    @test Set(keys(gf3.attributes)) == Set(keys(gu3.attributes))
    # no mutation
    @test (:test_attr=>Symbol) in attrdefs(gf2)
    @test (:foo=>Type) in attrdefs(gf2)
    @test Set(keys(gf2.attributes)) == Set(keys(gu2.attributes))
end

@testset "SyntaxTree parsing" begin
    # Errors should fall through
    @test parsestmt(SyntaxTree, "@"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "@@@"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "(a b c)"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "'a b c'"; ignore_errors=true) isa SyntaxTree
end

@testset "SyntaxTree utils" begin
    "For filling required attrs in graphs created by hand"
    function testgraph(edge_ranges, edges, more_attrs...)
        kinds = Dict(map(i->(i=>K"block"), eachindex(edge_ranges)))
        sources = Dict{Int, SourceAttrType}(
            map(i->(i=>LineNumberNode(i)), eachindex(edge_ranges)))
        orig = Dict(map(i->(i=>i), eachindex(edge_ranges)))
        SyntaxGraph(
            edge_ranges,
            edges,
            Dict(:kind => kinds, :source => sources,
                 :orig => orig, more_attrs...))
    end

    @testset "copy_ast" begin
        # 1 --> 2 --> 3     src(7-9) = line 7-9
        # 4 --> 5 --> 6     src(i) = i + 3
        # 7 --> 8 --> 9
        g = testgraph([1:1, 2:2, 0:-1, 3:3, 4:4, 0:-1, 5:5, 6:6, 0:-1],
                      [2, 3, 5, 6, 8, 9],
                      :source => Dict{Int, SourceAttrType}(enumerate([
                          map(i->i+3, 1:6)...
                          map(LineNumberNode, 7:9)...])))
        st = SyntaxTree(g, 1)
        stcopy = copy_ast(g, st)
        # Each node should be copied once
        @test length(g.edge_ranges) === 18
        @test st._id != stcopy._id
        @test st ≈ stcopy
        @test st.source !== stcopy.source
        @test st.source[1] !== stcopy.source[1]
        @test st.source[1][1] !== stcopy.source[1][1]

        stcopy2 = copy_ast(g, st; copy_source=false)
        # Only nodes 1-3 should be copied
        @test length(g.edge_ranges) === 21
        @test st._id != stcopy2._id
        @test st ≈ stcopy2
        @test st.source === stcopy2.source
        @test st.source[1] === stcopy2.source[1]
        @test st.source[1][1] === stcopy2.source[1][1]

        # Copy into a new graph
        new_g = ensure_attributes!(SyntaxGraph(); attrdefs(g)...)
        stcopy3 = copy_ast(new_g, st)
        @test length(new_g.edge_ranges) === 9
        @test st ≈ stcopy3

        new_g = ensure_attributes!(SyntaxGraph(); attrdefs(g)...)
        # Disallow for now, since we can't prevent dangling sourcerefs
        @test_throws ErrorException copy_ast(new_g, st; copy_source=false)
    end

    @testset "unalias_nodes" begin
        # 1 -+-> 2 ->-> 4
        #    |      |
        #    +-> 3 -+
        g = testgraph([1:2, 3:3, 4:4, 0:-1], [2, 3, 4, 4])
        st = SyntaxTree(g, 1)
        stu = JuliaSyntax.unalias_nodes(st)
        @test st ≈ stu
        @test length(stu._graph.edge_ranges) == 5
        @test length(stu._graph.edges) == 4
        # Properties of node 4 should be preserved
        @test 4 == stu[1][1].orig == stu[2][1].orig
        @test st[1][1].source == stu[1][1].source == stu[2][1].source
        @test stu[1][1]._id != stu[2][1]._id

        #           +-> 5
        #           |
        # 1 -+-> 2 -+---->>>-> 6
        #    |           |||
        #    +-> 3 -> 7 -+||
        #    |            ||
        #    +-> 4 -+-----+|
        #           |      |
        #           +------+
        g = testgraph([1:3, 4:5, 6:6, 7:8, 0:-1, 0:-1, 9:9],
                      [2, 3, 4, 5, 6, 7, 6, 6, 6])
        st = SyntaxTree(g, 1)
        stu = JuliaSyntax.unalias_nodes(st)
        @test st ≈ stu
        # node 6 should be copied three times
        @test length(stu._graph.edge_ranges) == 10
        @test length(stu._graph.edges) == 9
        # the four copies of node 6 should have attrs identical to the original and distinct ids
        @test 6 == stu[1][2].orig == stu[2][1][1].orig == stu[3][1].orig == stu[3][2].orig
        @test stu[1][2]._id != stu[2][1][1]._id != stu[3][1]._id != stu[3][2]._id

        # 1 -+-> 2 ->-> 4 -+----> 5 ->-> 7
        #    |      |      |         |
        #    +-> 3 -+      +-->-> 6 -+
        #        |            |
        #        +------------+
        g = testgraph([1:2, 3:3, 4:5, 6:7, 8:8, 9:9, 0:-1],
                      [2,3,4,4,6,5,6,7,7])
        st = SyntaxTree(g, 1)
        stu = JuliaSyntax.unalias_nodes(st)
        @test st ≈ stu
        @test length(stu._graph.edge_ranges) == 15
        @test length(stu._graph.edges) == 14
        # attrs of nodes 4-7
        @test 4 == stu[1][1].orig == stu[2][1].orig
        @test 5 == stu[1][1][1].orig == stu[2][1][1].orig
        @test 6 == stu[1][1][2].orig == stu[2][1][2].orig == stu[2][2].orig
        @test 7 == stu[1][1][1][1].orig == stu[1][1][2][1].orig ==
            stu[2][1][1][1].orig == stu[2][1][2][1].orig == stu[2][2][1].orig
        # ensure no duplication
        @test stu[1][1][1][1]._id != stu[1][1][2][1]._id !=
            stu[2][1][1][1]._id != stu[2][1][2][1]._id != stu[2][2][1]._id
    end

    @testset "prune" begin
        # [1]-+-> 2         5 --> 6
        #     |
        #     +-> 3 --> 4   7
        g = testgraph([1:2, 0:-1, 3:3, 0:-1, 4:4, 0:-1, 0:-1], [2, 3, 4, 6])
        st = SyntaxTree(g, 1)
        stp = JuliaSyntax.prune(st)
        @test st ≈ stp
        @test length(stp._graph.edge_ranges) === 4
        @test stp.source == LineNumberNode(1)
        @test stp[1].source == LineNumberNode(2)
        @test stp[2].source == LineNumberNode(3)
        @test stp[2][1].source == LineNumberNode(4)

        # (also checks that the last prune didn't destroy the graph)
        # 1 -+-> 2         5 --> 6
        #    |
        #    +-> 3 --> 4  [7]
        st = SyntaxTree(g, 7)
        stp = JuliaSyntax.prune(st)
        @test st ≈ stp
        @test length(stp._graph.edge_ranges) === 1
        @test stp.orig == 7

        # 1 -+->[2]->-> 4
        #    |      |
        #    +-> 3 -+
        g = testgraph([1:2, 3:3, 4:4, 0:-1], [2, 3, 4, 4])
        st = SyntaxTree(g, 2)
        stp = JuliaSyntax.prune(st)
        @test st ≈ stp
        @test length(stp._graph.edge_ranges) === 2
        @test stp.orig == 2
        @test stp[1].orig == 4

        #  9 -->[1]--> 5    src(1) = 2
        # 10 --> 2 --> 6    src(2) = 3
        # 11 --> 3 --> 7    src(3) = 4
        # 12 --> 4 --> 8    else src(i) = line(i)
        g = testgraph([1:1, 2:2, 3:3, 4:4, 0:-1, 0:-1, 0:-1, 0:-1, 5:5, 6:6, 7:7, 8:8],
                      [5, 6, 7, 8, 1, 2, 3, 4],
                      :source => Dict{Int, SourceAttrType}(
                          1=>2, 2=>3, 3=>4,
                          map(i->(i=>LineNumberNode(i)), 4:12)...))
        st = SyntaxTree(g, 1)
        stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[4]))
        @test st ≈ stp
        # 1, 5, 4, 8 should remain
        @test length(stp._graph.edge_ranges) === 4
        @test stp.source isa NodeId
        orig_4 = SyntaxTree(stp._graph, stp.source)
        @test orig_4.source === LineNumberNode(4)
        @test numchildren(orig_4) === 1
        @test orig_4[1].source === LineNumberNode(8)
        @test stp[1].source === LineNumberNode(5)

        # Try again with node 3 explicitly marked reachable
        stp = JuliaSyntax.prune(st, keep=SyntaxList(g, NodeId[3, 4]))
        @test st ≈ stp
        # 1, 5, 4, 8, and now 3, 7 as well
        @test length(stp._graph.edge_ranges) === 6
        @test stp.source isa NodeId
        @test stp[1].source === LineNumberNode(5)

        orig_3 = SyntaxTree(stp._graph, stp.source)
        @test orig_3.source isa NodeId
        orig_4 = SyntaxTree(stp._graph, orig_3.source)
        @test orig_4.source === LineNumberNode(4)

        @test numchildren(orig_3) === 1
        @test numchildren(orig_4) === 1
        @test orig_3[1].source === LineNumberNode(7)
        @test orig_4[1].source === LineNumberNode(8)

        # Try again with no node provenance
        stp = JuliaSyntax.prune(st, keep=nothing)
        @test st ≈ stp
        @test length(stp._graph.edge_ranges) === 2
        @test stp.source === LineNumberNode(4)
        @test stp[1].source === LineNumberNode(5)

        # test with real parsed, then copied output---not many properties we can
        # check without fragile tests, but there are some.
        code = "begin; x1=1; x2=2; x3=3; x4=begin; 4; end; begin; end; end"
        st0 = parsestmt(SyntaxTree, code)
        st0_dup1 = JuliaSyntax.mknode(st0, children(st0))
        st0_dup2 = JuliaSyntax.mknode(st0_dup1, children(st0_dup1))
        stp = JuliaSyntax.prune(st0_dup2; keep=st0)
        @test st0_dup2 ≈ stp
        @test length(stp._graph.edge_ranges) <
            length(st0_dup2._graph.edge_ranges)
        @test stp.source isa NodeId
        @test SyntaxTree(stp._graph, stp.source) ≈ st0
        @test sourcetext(stp) == code
        # try without preserving st0
        stp = JuliaSyntax.prune(st0_dup2, keep=nothing)
        @test st0_dup2 ≈ stp
        @test length(stp._graph.edge_ranges) <
            length(st0_dup2._graph.edge_ranges)
        @test stp.source isa SourceRef
        @test sourcetext(stp) == code
    end

    @testset "annotate_parent" begin
        chk_parent(st, parent) = get(st, :parent, nothing) === parent &&
            all(c->chk_parent(c, st._id), children(st))
        # 1 -+-> 2 ->-> 4 --> 5
        #    |      |
        #    +-> 3 -+
        g = testgraph([1:2, 3:3, 4:4, 5:5, 0:-1], [2, 3, 4, 4, 5])
        st = JuliaSyntax.annotate_parent!(SyntaxTree(g, 1))
        @test chk_parent(st, nothing)
        # NamedTuple-based attrs
        g = JuliaSyntax.freeze_attrs(testgraph([1:2, 3:3, 4:4, 5:5, 0:-1], [2, 3, 4, 4, 5]))
        st = JuliaSyntax.annotate_parent!(SyntaxTree(g, 1))
        @test chk_parent(st, nothing)
    end
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
        # trailing splat
        @test @stm st begin
            [K"call" f _...] -> true
        end
        @test @stm st begin
            [K"call" f args...] -> kind(f) === K"Identifier"
        end
        @test @stm st begin
            [K"call" f args...] -> args isa SyntaxList && length(args) === 3
        end
        @test @stm st begin
            [K"call" f args...] -> kind(args[1]) === K"Identifier" &&
                kind(args[2]) === K"kw" &&
                kind(args[3]) === K"call"
        end
        @test @stm st begin
            [K"call" f a b c empty...] -> empty isa SyntaxList && length(empty) === 0
        end

        # binds after splat
        @test @stm st begin
            [K"call" f args... last] ->
                args isa SyntaxList &&
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
            [K"call" empty... f a b c] -> empty isa SyntaxList && length(empty) === 0
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
