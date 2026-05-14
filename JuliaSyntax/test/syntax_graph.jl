using .JuliaSyntax: SyntaxGraph, SyntaxTree, SyntaxList, ensure_attributes, ensure_attributes!, delete_attributes, copy_ast, attrdefs, @stm, NodeId, SourceRef, SourceAttrType, Kind, syntax_graph, prov, prov_end, provenance, macro_prov, flattened_provenance, sourceref, newleaf, mkleaf, mknode, mktree, setattr!, hasattr

"For filling required attrs in graphs created by hand"
function testgraph(edge_ranges, edges, more_attrs...)
    kinds = Dict{NodeId, Any}(map(i->(i=>K"block"), eachindex(edge_ranges)))
    sources = Dict{NodeId, Any}(
        map(i->(i=>LineNumberNode(i)), eachindex(edge_ranges)))
    orig = Dict{NodeId, Any}(map(i->(i=>i), eachindex(edge_ranges)))
    SyntaxGraph(
        edge_ranges,
        edges,
        Dict{Symbol, Dict{NodeId, Any}}(
            :kind => kinds, :source => sources,
            :orig => orig, more_attrs...))
end

@testset "SyntaxGraph attrs" begin
    g_dict = SyntaxGraph()
    g_nt = ensure_attributes(
        SyntaxGraph(Vector{UnitRange{Int}}(), Vector{NodeId}(), (;)),
        kind=Kind,
        source=SourceAttrType,
        syntax_flags=UInt16,
        value=Any,
        name_val=String,
        mod=Module)

    # Check that a default graph has required attrs
    @test (:kind=>Any) in attrdefs(g_dict)
    @test (:source=>Any) in attrdefs(g_dict)
    @test (:value=>Any) in attrdefs(g_dict)
    @test (:name_val=>Any) in attrdefs(g_dict)
    @test (:kind=>Kind) in attrdefs(g_nt)
    @test (:source=>SourceAttrType) in attrdefs(g_nt)
    @test (:value=>Any) in attrdefs(g_nt)
    @test (:name_val=>String) in attrdefs(g_nt)

    # ensure_attributes
    g_nt2 = ensure_attributes(g_nt, test_attr=Symbol, foo=Type)
    g_dict2 = ensure_attributes(g_dict, test_attr=Symbol, foo=Type)
    # returns a graph with the same attribute storage
    @test g_nt2.attributes isa NamedTuple
    @test g_dict2.attributes isa Dict
    # does its job
    @test (:test_attr=>Symbol) in attrdefs(g_nt2)
    @test (:foo=>Type) in attrdefs(g_nt2)
    @test Set(keys(g_nt2.attributes)) == Set(keys(g_dict2.attributes))
    # no mutation
    @test !((:test_attr=>Symbol) in attrdefs(g_nt))
    @test !((:foo=>Type) in attrdefs(g_nt))
    @test Set(keys(g_nt.attributes)) == Set(keys(g_dict.attributes))

    # delete_attributes
    g_nt3 = delete_attributes(g_nt2, :test_attr, :foo)
    g_dict3 = delete_attributes(g_dict2, :test_attr, :foo)
    # returns a graph with the same attribute storage
    @test g_nt3.attributes isa NamedTuple
    @test g_dict3.attributes isa Dict
    # does its job
    @test !((:test_attr=>Symbol) in attrdefs(g_nt3))
    @test !((:foo=>Type) in attrdefs(g_nt3))
    @test Set(keys(g_nt3.attributes)) == Set(keys(g_dict3.attributes))
    # no mutation
    @test (:test_attr=>Symbol) in attrdefs(g_nt2)
    @test (:foo=>Type) in attrdefs(g_nt2)
    @test Set(keys(g_nt2.attributes)) == Set(keys(g_dict2.attributes))
end

@testset "SyntaxTree parsing" begin
    # Errors should fall through
    @test parsestmt(SyntaxTree, ""; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, " "; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "@"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "@@@"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "(a b c)"; ignore_errors=true) isa SyntaxTree
    @test parsestmt(SyntaxTree, "'a b c'"; ignore_errors=true) isa SyntaxTree
end

@testset "SyntaxTree provenance accessors" begin

    @testset "prov, prov_end, provenance, sourceref" begin
        # 1 --> 2 --> 3     src(7-9) = line 7-9
        # 4 --> 5 --> 6     src(i) = i + 3
        # 7 --> 8 --> 9
        g = testgraph([1:1, 2:2, 0:-1, 3:3, 4:4, 0:-1, 5:5, 6:6, 0:-1],
                      [2, 3, 5, 6, 8, 9],
                      :source => Dict{Int, SourceAttrType}(enumerate([
                          map(i->i+3, 1:6)...
                              map(LineNumberNode, 7:9)...])))
        st = SyntaxTree(g, 1)
        @test prov(st)._id == 4
        @test prov(prov(st))._id == 7
        @test prov(prov(prov(st)))._id == 7

        @test prov_end(st)._id == 7
        @test prov_end(prov_end(st))._id == 7

        @test sourceref(st) == LineNumberNode(7)
        @test sourceref(prov_end(st)) == LineNumberNode(7)

        @test provenance(st).ids == NodeId[4, 7]
        @test provenance(prov_end(st)).ids == NodeId[]
    end

    @testset "flattened_provenance" begin
        g = SyntaxGraph()
        ensure_attributes!(g; macro_source=NodeId)
        st1 = setattr!(newleaf(g, LineNumberNode(1), K"Identifier"), :name_val, "st1")
        st2 = setattr!(mkleaf(st1), :name_val, "st2")
        st3 = setattr!(mkleaf(st2), :name_val, "st3")

        stm1 = setattr!(newleaf(g, LineNumberNode(1, :m), K"Identifier"), :name_val, "stm1")
        stm2 = setattr!(mkleaf(stm1), :name_val, "stm2")
        stm3 = setattr!(mkleaf(stm1), :name_val, "stm3")
        stm_unused = setattr!(newleaf(g, LineNumberNode(0), K"Identifier"), :name_val, "stm_unused")

        stmm1 = setattr!(newleaf(g, LineNumberNode(1, :mm), K"Identifier"), :name_val, "stmm1")
        stmm2 = setattr!(mkleaf(stmm1), :name_val, "stmm2")
        stmm3 = setattr!(mkleaf(stmm2), :name_val, "stmm3")

        setattr!(st1, :macro_source, stm_unused._id)
        setattr!(st2, :macro_source, stm_unused._id)
        setattr!(st3, :macro_source, stm3._id)
        setattr!(stm3, :macro_source, stmm3._id)

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
    mprov(st::SyntaxTree) = get(st, :macro_source, nothing) isa NodeId ?
        SyntaxTree(st._graph, st.macro_source) : nothing

    @testset "copy_ast, mktree" begin
        # 1 --> 2 --> 3     src(7-9) = line 7-9
        # 4 --> 5 --> 6     src(i) = i + 3
        # 7 --> 8 --> 9
        g = testgraph([1:1, 2:2, 0:-1, 3:3, 4:4, 0:-1, 5:5, 6:6, 0:-1],
                      [2, 3, 5, 6, 8, 9],
                      :source => Dict{Int, SourceAttrType}(enumerate([
                          map(i->i+3, 1:6)...
                          map(LineNumberNode, 7:9)...])))
        st = SyntaxTree(g, 1)
        new_g = ensure_attributes!(SyntaxGraph(); attrdefs(g)...)

        stcopy = copy_ast(new_g, st)
        # Each node should be copied once
        @test length(new_g.edge_ranges) === length(g.edge_ranges)
        @test st ≈ stcopy
        @test prov(st) ≈ prov(stcopy)
        @test prov(prov(st)) ≈ prov(prov(stcopy))

        stcopy2 = mktree(st)
        # Only nodes 1-3 should be copied
        @test length(g.edge_ranges) === 12
        @test st._id != stcopy2._id
        @test st ≈ stcopy2
        @test stcopy2.source === st._id

        # Disallow copying into the same graph; slow for no good reason
        @test_throws "mktree" copy_ast(st._graph, st)
    end

    @testset "unalias_nodes" begin
        # 1 -+-> 2 ->-> 4    src(4) = 5
        #    |      |       msrc(4) = 6
        #    +-> 3 -+
        #               5
        #               6
        g = testgraph([1:2, 3:3, 4:4, 0:-1, 0:-1, 0:-1], [2, 3, 4, 4],
                      :macro_source => Dict{Int, SourceAttrType}(4=>6))
        setattr!(g, 4, :source, 5)

        st = SyntaxTree(g, 1)
        stu = JuliaSyntax.unalias_nodes(st)
        @test st ≈ stu
        @test length(stu._graph.edge_ranges) == 7
        @test length(stu._graph.edges) == 4
        # Properties of node 4 should be preserved
        @test 4 == stu[1][1].orig == stu[2][1].orig
        @test st[1][1].source == stu[1][1].source == stu[2][1].source
        @test st[1][1].macro_source == stu[1][1].macro_source == stu[2][1].macro_source
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

        # [1]--> 4    src(1) = 2, msrc(1) = 3
        #  2 --> 5
        #  3 --> 6
        g = testgraph([1:1, 2:2, 3:3, 0:-1, 0:-1, 0:-1], [4, 5, 6],
                      :source => Dict{Int, SourceAttrType}(
                          1=>2,map(i->(i=>LineNumberNode(i)), 2:6)...),
                      :macro_source => Dict{Int, SourceAttrType}(1=>3))
        st = SyntaxTree(g, 1)
        let stp = JuliaSyntax.prune(st)
            @test st ≈ stp
            @test length(stp._graph.edge_ranges) === 2
            @test stp.orig == 1
            @test stp[1].orig == 4
            @test isempty(stp._graph.macro_source)
        end
        let stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[2]))
            @test st ≈ stp
            @test length(stp._graph.edge_ranges) === 4
            @test stp.orig == 1
            @test stp[1].orig == 4
            @test isempty(stp._graph.macro_source)
            @test prov(stp).orig == 2
        end
        let stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[2, 3]))
            @test st ≈ stp
            @test prov(st) ≈ prov(stp)
            @test length(stp._graph.edge_ranges) === 6
            @test hasattr(stp, :macro_source)
            @test SyntaxTree(stp._graph, stp.macro_source).orig == 3
        end

        # 1
        # 10 -->[2]--> 6    src(2) = 3
        # 11 --> 3 --> 7    src(3) = 4, msrc(3) = 4
        # 12 --> 4 --> 8    src(4) = 5
        # 13 --> 5 --> 9    else src(i) = line(i)
        g = testgraph([0:-1, 1:1, 2:2, 3:3, 4:4, 0:-1, 0:-1, 0:-1, 0:-1, 5:5, 6:6, 7:7, 8:8],
                      [6, 7, 8, 9, 2, 3, 4, 5],
                      :source => Dict{Int, SourceAttrType}(
                          2=>3, 3=>4, 4=>5, 1=>LineNumberNode(1),
                          map(i->(i=>LineNumberNode(i)), 4:12)...),
                      :macro_source => Dict{Int, SourceAttrType}(3=>4))
        st = SyntaxTree(g, 2)
        stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[5]))
        @test st ≈ stp
        # 1, 5, 4, 8 should remain; macro_source is removed
        @test length(stp._graph.edge_ranges) === 4
        @test isempty(stp._graph.macro_source)

        # need both 3 and 4 preserved to keep the macro_source link (though we
        # may want it to be a stronger prune-preserved reference in the future.)
        let stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[5, 3]))
            @test st ≈ stp
            @test length(stp._graph.edge_ranges) === 6
            @test isempty(stp._graph.macro_source)
        end
        let stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[5, 4]))
            @test st ≈ stp
            @test length(stp._graph.edge_ranges) === 6
            @test isempty(stp._graph.macro_source)
        end
        let stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[3]))
            @test st ≈ stp
            @test length(stp._graph.edge_ranges) === 4
            @test isempty(stp._graph.macro_source)
        end
        let stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[4]))
            @test st ≈ stp
            @test length(stp._graph.edge_ranges) === 4
            @test isempty(stp._graph.macro_source)
        end
        let stp = JuliaSyntax.prune(st; keep=SyntaxList(g, NodeId[3, 4]))
            @test st ≈ stp
            @test length(stp._graph.edge_ranges) === 6
            @test hasattr(prov(stp), :macro_source)
            new_4 = SyntaxTree(stp._graph, prov(stp).macro_source)
            @test new_4.orig == 4
            @test new_4[1].source == LineNumberNode(8)
        end

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
    end
end

@testset "SyntaxList" begin
    st = parsestmt(SyntaxTree, "function foo end")
    g = st._graph

    # constructors
    sl0 = SyntaxList(g)
    @test sl0 isa SyntaxList
    @test length(sl0) == 0
    @test syntax_graph(sl0) == g

    sl1_id = SyntaxList(g, st._id)
    @test sl1_id isa SyntaxList
    @test length(sl1_id) == 1
    @test sl1_id[1] == st
    @test syntax_graph(sl1_id) == g

    sl1 = SyntaxList(st)
    @test sl1 isa SyntaxList
    @test length(sl1) == 1
    @test sl1[1] == st
    @test syntax_graph(sl1) == g

    sl2 = SyntaxList(st, st)
    @test sl2 isa SyntaxList
    @test length(sl2) == 2
    @test sl2[2] == st
    @test syntax_graph(sl2) == g
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
