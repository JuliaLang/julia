using JuliaSyntax: @mknode

@testset "assert_syntaxtree" begin
    st = parsestmt(SyntaxTree, "function foo end")
    @test JuliaLowering.assert_syntaxtree(st) === nothing
    @test_throws "needs value" @mknode(;source=st, context=st.context, head=:identifier)
    @test_throws "unrecognized leaf" @mknode(st; head=:code_info, children=nothing)
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
