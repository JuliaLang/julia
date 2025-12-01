@testset "SyntaxGraph attrs" begin
    st = parsestmt(SyntaxTree, "function foo end")
    g_init = JuliaLowering.unfreeze_attrs(st._graph)
    gf1 = JuliaLowering.freeze_attrs(g_init)
    gu1 = JuliaLowering.unfreeze_attrs(gf1)

    # Check that freeze/unfreeze do their jobs
    @test gf1.attributes isa NamedTuple
    @test gu1.attributes isa Dict
    @test Set(keys(gf1.attributes)) == Set(keys(gu1.attributes))

    # ensure_attributes
    gf2 = JuliaLowering.ensure_attributes(gf1, test_attr=Symbol, foo=Type)
    gu2 = JuliaLowering.ensure_attributes(gu1, test_attr=Symbol, foo=Type)
    # returns a graph with the same attribute storage
    @test gf2.attributes isa NamedTuple
    @test gu2.attributes isa Dict
    # does its job
    @test (:test_attr=>Symbol) in JuliaLowering.attrdefs(gf2)
    @test (:foo=>Type) in JuliaLowering.attrdefs(gf2)
    @test Set(keys(gf2.attributes)) == Set(keys(gu2.attributes))
    # no mutation
    @test !((:test_attr=>Symbol) in JuliaLowering.attrdefs(gf1))
    @test !((:foo=>Type) in JuliaLowering.attrdefs(gf1))
    @test Set(keys(gf1.attributes)) == Set(keys(gu1.attributes))

    # delete_attributes
    gf3 = JuliaLowering.delete_attributes(gf2, :test_attr, :foo)
    gu3 = JuliaLowering.delete_attributes(gu2, :test_attr, :foo)
    # returns a graph with the same attribute storage
    @test gf3.attributes isa NamedTuple
    @test gu3.attributes isa Dict
    # does its job
    @test !((:test_attr=>Symbol) in JuliaLowering.attrdefs(gf3))
    @test !((:foo=>Type) in JuliaLowering.attrdefs(gf3))
    @test Set(keys(gf3.attributes)) == Set(keys(gu3.attributes))
    # no mutation
    @test (:test_attr=>Symbol) in JuliaLowering.attrdefs(gf2)
    @test (:foo=>Type) in JuliaLowering.attrdefs(gf2)
    @test Set(keys(gf2.attributes)) == Set(keys(gu2.attributes))
end

@testset "SyntaxTree" begin
    # Expr conversion
    @test Expr(parsestmt(SyntaxTree, "begin a + b ; c end", filename="none")) ==
        Meta.parse("begin a + b ; c end")

    tree1 = JuliaLowering.@SyntaxTree :(some_unique_identifier)
    @test tree1 isa SyntaxTree
    @test kind(tree1) == K"Identifier"
    @test tree1.name_val == "some_unique_identifier"

    tree2 = JuliaLowering.@SyntaxTree quote
        x
        $tree1
    end
    @test tree2 isa SyntaxTree
    @test kind(tree2) == K"block"
    @test kind(tree2[1]) == K"Identifier" && tree2[1].name_val == "x"
    @test kind(tree2[2]) == K"Identifier" && tree2[2].name_val == "some_unique_identifier"

    "For filling required attrs in graphs created by hand"
    function testgraph(edge_ranges, edges, more_attrs...)
        kinds = Dict(map(i->(i=>K"block"), eachindex(edge_ranges)))
        sources = Dict(map(i->(i=>LineNumberNode(i)), eachindex(edge_ranges)))
        SyntaxGraph(
            edge_ranges,
            edges,
            Dict(:kind => kinds, :source => sources, more_attrs...))
    end

    @testset "copy_ast" begin
        # 1 --> 2 --> 3     src(7-9) = line 7-9
        # 4 --> 5 --> 6     src(i) = i + 3
        # 7 --> 8 --> 9
        g = testgraph([1:1, 2:2, 0:-1, 3:3, 4:4, 0:-1, 5:5, 6:6, 0:-1],
                      [2, 3, 5, 6, 8, 9],
                      :source => Dict(enumerate([
                          map(i->i+3, 1:6)...
                          map(LineNumberNode, 7:9)...])))
        st = SyntaxTree(g, 1)
        stcopy = JuliaLowering.copy_ast(g, st)
        # Each node should be copied once
        @test length(g.edge_ranges) === 18
        @test st._id != stcopy._id
        @test st ≈ stcopy
        @test st.source !== stcopy.source
        @test st.source[1] !== stcopy.source[1]
        @test st.source[1][1] !== stcopy.source[1][1]

        stcopy2 = JuliaLowering.copy_ast(g, st; copy_source=false)
        # Only nodes 1-3 should be copied
        @test length(g.edge_ranges) === 21
        @test st._id != stcopy2._id
        @test st ≈ stcopy2
        @test st.source === stcopy2.source
        @test st.source[1] === stcopy2.source[1]
        @test st.source[1][1] === stcopy2.source[1][1]

        # Copy into a new graph
        new_g = ensure_attributes!(SyntaxGraph(); JuliaLowering.attrdefs(g)...)
        stcopy3 = JuliaLowering.copy_ast(new_g, st)
        @test length(new_g.edge_ranges) === 9
        @test st ≈ stcopy3

        new_g = ensure_attributes!(SyntaxGraph(); JuliaLowering.attrdefs(g)...)
        # Disallow for now, since we can't prevent dangling sourcerefs
        @test_throws ErrorException JuliaLowering.copy_ast(new_g, st; copy_source=false)
    end
end
