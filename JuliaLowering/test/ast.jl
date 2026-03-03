@testset "assert_syntaxtree" begin
    st = parsestmt(SyntaxTree, "function foo end")
    g = st._graph
    @test JuliaLowering.assert_syntaxtree(st) === nothing

    bad_st = JuliaSyntax.newleaf(g, st, K"Identifier")
    @test_throws "needs attribute name_val" JuliaLowering.assert_syntaxtree(bad_st)
    @test_throws "needs attribute name_val" show(bad_st)

    bad_st = JuliaSyntax.newleaf(g, st, K"code_info")
    @test_throws "unrecognized leaf kind" JuliaLowering.assert_syntaxtree(bad_st)

    bad_st = JuliaSyntax.newnode(g, st, K"code_info", NodeId[])
    @test_throws "needs attribute is_toplevel_thunk" JuliaLowering.assert_syntaxtree(bad_st)

    JuliaSyntax.setchildren!(g, bad_st._id, NodeId[bad_st._id])
    @test_throws "cycle detected" JuliaLowering.assert_syntaxtree(bad_st)

    cyc_1 = JuliaSyntax.newnode(g, st, K"block", NodeId[])
    cyc_2 = JuliaSyntax.newnode(g, st, K"block", NodeId[cyc_1._id])
    JuliaSyntax.setchildren!(g, cyc_1._id, NodeId[cyc_2._id])
    @test_throws "cycle detected" JuliaLowering.assert_syntaxtree(cyc_1)
    @test_throws "cycle detected" JuliaLowering.assert_syntaxtree(cyc_2)
end

@testset "flatten_blocks" begin
    let
        st = @ast_ [K"block"]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"block"]

        st = @ast_ [K"block" 1::K"Value"]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"block" 1::K"Value"]

        st = @ast_ [K"block" 1::K"Value" [K"block" 1::K"Value"]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"block" 1::K"Value" 1::K"Value"]

        st = @ast_ [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]

        st = @ast_ [K"block" 1::K"Value" [K"block"]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"block" 1::K"Value" "nothing"::K"core"]

        st = @ast_ [K"block" 1::K"Value" [K"block"] 1::K"Value"]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"block" 1::K"Value" 1::K"Value"]

        st = @ast_ [K"block" [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"block" [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]]

        # repeat with call wrapper
        st = @ast_ [K"call" [K"block"]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"call" [K"block"]]

        st = @ast_ [K"call" [K"block" 1::K"Value"]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"call" [K"block" 1::K"Value"]]

        st = @ast_ [K"call" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"call" [K"block" 1::K"Value" 1::K"Value"]]

        st = @ast_ [K"call" [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"call" [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]]

        st = @ast_ [K"call" [K"block" 1::K"Value" [K"block"]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"call" [K"block" 1::K"Value" "nothing"::K"core"]]

        st = @ast_ [K"call" [K"block" 1::K"Value" [K"block"] 1::K"Value"]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"call" [K"block" 1::K"Value" 1::K"Value"]]

        st = @ast_ [K"call" [K"block" [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [K"call" [K"block" [K"inert" [K"block" 1::K"Value" [K"block" 1::K"Value"]]]]]
    end
end
