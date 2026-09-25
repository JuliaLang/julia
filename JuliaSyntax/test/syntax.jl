using .JuliaSyntax: SyntaxTree, SyntaxList, prov, prov_end, provenance,
    macro_prov, macro_prov_end, flattened_provenance, sourceref,
    unexpanded_sourceref, unalias_nodes, annotate_parent!, getmeta, SyntaxContext,
    ScopeLayer, children, @mknode

const DUMMY_CONTEXT = SyntaxContext(@__MODULE__, (0,0))

"""
Build a hand-made tree for the DAG-shaped tests below.  Each node carries a
distinct integer in `.value` so nodes copied by `unalias_nodes` and friends can
be traced back to the node they were copied from.
"""
function tnode(tag::Int, cs::SyntaxTree...)
    isempty(cs) ?
        SyntaxTree(:value, nothing, tag, LineNumberNode(tag), DUMMY_CONTEXT) :
        SyntaxTree(:block, SyntaxList(cs...), tag, LineNumberNode(tag), DUMMY_CONTEXT)
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

@testset "SyntaxTree compound assignment heads" begin
    # Must not depend on `show(::SyntaxTree)`, which JuliaLowering defines
    @test head(parsestmt(SyntaxTree, "x += 1")) === :+=
    @test head(parsestmt(SyntaxTree, "x .>>>= 1")) === :.>>>=
    @test parsestmt(SyntaxTree, ":(+=)")[1].value == "+="
    @test parsestmt(SyntaxTree, ":.>>>=")[1].value == ".>>>="
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
        st2 = @mknode(st3)
        st1 = @mknode(st2)

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
            (0, 0),
            false)

        stm_unused = SyntaxTree(:identifier, nothing, "stm_unused", LineNumberNode(0), DUMMY_CONTEXT)

        stmm1 = SyntaxTree(:identifier, nothing, "stmm1", LineNumberNode(1, :mm), DUMMY_CONTEXT)
        stmm2 = @mknode(stmm1; value="stmm2")
        stmm3 = @mknode(stmm2; value="stmm3")

        stm1 = SyntaxTree(:identifier, nothing, "stm1", LineNumberNode(1, :m), DUMMY_CONTEXT)
        stm2 = @mknode(stm1; value="stm2")
        stm3 = SyntaxTree(:identifier, nothing, "stm3", stm2, ctx_with_unexpanded(stmm3))

        st1 = SyntaxTree(:identifier, nothing, "st1", LineNumberNode(1),
                         ctx_with_unexpanded(stm_unused))
        st2 = SyntaxTree(:identifier, nothing, "st2", st1, ctx_with_unexpanded(stm_unused))
        st3 = SyntaxTree(:identifier, nothing, "st3", st2, ctx_with_unexpanded(stm3))

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
        #    ├─ stm2
        #    │  └─ stm1
        #    │     └─ @ m:1
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
