using JuliaSyntax: @mknode
using JuliaLowering: @stm

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

@testset "@stm SyntaxTree pattern-matching" begin
    st = JuliaSyntax.parsestmt(SyntaxTree, "foo(a,b=1,c(d=2))")
    # (call foo a (kw b 1) (call c (kw d 2)))

    @testset "basic functionality" begin
        @test @stm st begin
            _ -> true
        end

        @test @stm st begin
            x -> x isa SyntaxTree
        end

        @test @stm st begin
            [:function f a b c] -> false
            [:call f a b c] -> true
        end

        @test @stm st begin
            [:function _ _ _ _] -> false
            [:call _ _ _ _] -> true
        end

        @test @stm st begin
            [:call f a b] -> false
            [:call f a b c d] -> false
            [:call f a b c] -> true
        end

        @test @stm st begin
            [:call f a b c] ->
                head(f) === :identifier &&
                head(b) === :kw &&
                head(c) === :call
        end
    end

    @testset "errors" begin
        # no match
        @test_throws ErrorException @stm st begin
            [:identifier] -> false
        end

        # assuming we run this checker by default
        @testset "_stm_check_usage" begin
            bad = Expr[
                :(@stm st begin
                      [a] -> false
                  end)
                :(@stm st begin
                      [:none,a] -> false
                  end)
                :(@stm st begin
                      [:none a a] -> false
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
                      [:none a... b...] -> false
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
            [:call [:identifier] [:identifier] [:kw [:identifier] k1] [:call [:identifier] [:kw [:identifier] k2]]] -> 1
            [:call [:identifier] [:identifier] [:kw [:identifier] k1] [:call [:identifier] [:kw _ k2]]] -> 2
            [:call [:identifier] [:identifier] [:kw _ k1] [:call _ _]] -> 3
            [:call [:identifier] [:identifier] _ _ ] -> 4
            [:call _ _ _ _] -> 5
        end
        @test 1 === @stm st begin
            [:call _ _ [:none [:identifier] k1] [:none [:identifier] [:none [:none] k2]]] -> 5
            [:call _ _ [:kw [:identifier] k1] [:none [:identifier] [:none [:none] k2]]] -> 4
            [:call _ _ [:kw [:identifier] k1] [:call [:identifier] [:none [:none] k2]]] -> 3
            [:call _ _ [:kw [:identifier] k1] [:call [:identifier] [:kw [:none] k2]]] -> 2
            [:call _ _ [:kw [:identifier] k1] [:call [:identifier] [:kw [:identifier] k2]]] -> 1
        end
        @test 1 === @stm st begin
            [:call _ _ [:kw [:identifier] k1] [:call [:identifier] [:kw [:identifier] k2] bad]] -> 4
            [:call _ _ [:kw [:identifier] k1] [:call [:identifier] [:kw [:identifier] k2 bad]]] -> 3
            [:call _ _ [:kw [:identifier] k1] [:call [:identifier] [:kw [:identifier bad] k2]]] -> 2
            [:call _ _ [:kw [:identifier] k1] [:call [:identifier] [:kw [:identifier] k2]]] -> 1
        end
    end

    @testset "vcat form (newlines in pattern)" begin
        @test @stm st begin
            [:call
             f
             a
             b
             c] -> true
        end
        @test @stm st begin
            [:call
             f a b c] -> true
        end
        @test @stm st begin
            [:call


             f a b c] -> true
        end
        @test @stm st begin
            [:call
             [:identifier] [:identifier]
             [:kw [:identifier] k1]
             [:call
              [:identifier]
              [:kw
               [:identifier]
               k2]]] -> true
        end
    end

    @testset "SyntaxList splat matching" begin
        # NB: a splat binds a view of the parent's children, not a SyntaxList
        # trailing splat
        @test @stm st begin
            [:call f _...] -> true
        end
        @test @stm st begin
            [:call f args...] -> head(f) === :identifier
        end
        @test @stm st begin
            [:call f args...] ->
                args isa AbstractVector{SyntaxTree} && length(args) === 3
        end
        @test @stm st begin
            [:call f args...] -> head(args[1]) === :identifier &&
                head(args[2]) === :kw &&
                head(args[3]) === :call
        end
        @test @stm st begin
            [:call f a b c empty...] ->
                empty isa AbstractVector{SyntaxTree} && length(empty) === 0
        end

        # binds after splat
        @test @stm st begin
            [:call f args... last] ->
                args isa AbstractVector{SyntaxTree} &&
                length(args) === 2
        end
        @test @stm st begin
            [:call f args... last] ->
                head(f) === :identifier &&
                head(args[1]) === :identifier &&
                head(args[2]) === :kw &&
                head(last) === :call
        end
        @test @stm st begin
            [:call empty... f a b c] ->
                empty isa AbstractVector{SyntaxTree} && length(empty) === 0
        end
    end

    @testset "`when` clauses affect matching" begin
        @test @stm st begin
            (_, when=false) -> false
            (_, when=true) -> true
        end
        @test @stm st begin
            ([:call _...], when=false) -> false
            ([:call _...], when=true) -> true
        end
        @test @stm st begin
            ([:call _ _...], when=head(st[1])===:identifier) -> true
        end
        @test @stm st begin
            ([:call f _...], when=head(f)===:identifier) -> true
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
                ([:block], when=(push!(x, 123); false)) -> false
                (_, when=(push!(x, 1); true)) -> x == [1]
            end
            empty!(x)

            @test @stm st begin
                (x_pat, when=((x_when = x_pat); true)) -> x_pat == x_when
            end
        end
    end
end
