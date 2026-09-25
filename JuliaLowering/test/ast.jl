@testset "assert_syntaxtree" begin
    st = parsestmt(SyntaxTree, "function foo end")
    @test JuliaLowering.assert_syntaxtree(st) === nothing

    bad_st = JuliaSyntax.newleaf(st, :identifier)
    @test_throws "needs value" JuliaLowering.assert_syntaxtree(bad_st)
    @test_throws "needs value" show(bad_st)

    bad_st = JuliaSyntax.newleaf(st, :code_info)
    @test_throws "unrecognized leaf kind" JuliaLowering.assert_syntaxtree(bad_st)

    setfield!(bad_st, :children, SyntaxList(bad_st))
    @test_throws "cycle detected" JuliaLowering.assert_syntaxtree(bad_st)

    cyc_1 = JuliaSyntax.newnode(st, :block, SyntaxList())
    cyc_2 = JuliaSyntax.newnode(st, :block, SyntaxList(cyc_1))
    setfield!(cyc_1, :children, SyntaxList(cyc_2))
    @test_throws "cycle detected" JuliaLowering.assert_syntaxtree(cyc_1)
    @test_throws "cycle detected" JuliaLowering.assert_syntaxtree(cyc_2)
end

@testset "flatten_blocks" begin
    let
        st = @ast_ [:block]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:block]

        st = @ast_ [:block 1::value]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:block 1::value]

        st = @ast_ [:block 1::value [:block 1::value]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:block 1::value 1::value]

        st = @ast_ [:inert [:block 1::value [:block 1::value]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:inert [:block 1::value [:block 1::value]]]

        st = @ast_ [:block 1::value [:block]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:block 1::value (::nothing)]

        st = @ast_ [:block 1::value [:block] 1::value]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:block 1::value 1::value]

        st = @ast_ [:block [:inert [:block 1::value [:block 1::value]]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:block [:inert [:block 1::value [:block 1::value]]]]

        # repeat with call wrapper
        st = @ast_ [:call [:block]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:call [:block]]

        st = @ast_ [:call [:block 1::value]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:call [:block 1::value]]

        st = @ast_ [:call [:block 1::value [:block 1::value]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:call [:block 1::value 1::value]]

        st = @ast_ [:call [:inert [:block 1::value [:block 1::value]]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:call [:inert [:block 1::value [:block 1::value]]]]

        st = @ast_ [:call [:block 1::value [:block]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:call [:block 1::value (::nothing)]]

        st = @ast_ [:call [:block 1::value [:block] 1::value]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:call [:block 1::value 1::value]]

        st = @ast_ [:call [:block [:inert [:block 1::value [:block 1::value]]]]]
        @test JuliaLowering.flatten_blocks(st) ≈
            @ast_ [:call [:block [:inert [:block 1::value [:block 1::value]]]]]
    end
end
