using Base: SyntaxTree, SyntaxContext, ScopeLayer, @mknode, prov, prov_end,
    provenance, sourceref, macro_prov, macro_prov_end, flattened_provenance,
    unexpanded_sourceref
using .JuliaSyntax: children

const DUMMY_CONTEXT = SyntaxContext(@__MODULE__, (0,0))

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
        st3 = @mknode(;head=:value, value=3, context=DUMMY_CONTEXT, source=LineNumberNode(3))
        st2 = @mknode(st3)
        st1 = @mknode(st2)

        @test prov(st1) === st2
        @test prov(prov(st1)) === st3
        @test prov(prov(prov(st1))) === st3

        @test prov_end(st1) === st3
        @test prov_end(prov_end(st1)) === st3

        @test sourceref(st1) == LineNumberNode(3)
        @test sourceref(prov_end(st1)) == LineNumberNode(3)

        @test provenance(st1) == SyntaxTree[st2, st3]
        @test provenance(prov_end(st1)) == SyntaxTree[]
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
        @test flattened_provenance(st3) == SyntaxTree[stmm1, stm1, st1]
        @test flattened_provenance(st2) == SyntaxTree[stm_unused, st1]
        @test flattened_provenance(st1) == SyntaxTree[stm_unused, st1]
        @test flattened_provenance(stm3) == SyntaxTree[stmm1, stm1]
        @test flattened_provenance(stm2) == SyntaxTree[stm1]
        @test flattened_provenance(stm1) == SyntaxTree[stm1]
        @test flattened_provenance(stmm3) == SyntaxTree[stmm1]
        @test flattened_provenance(stmm2) == SyntaxTree[stmm1]
        @test flattened_provenance(stmm1) == SyntaxTree[stmm1]
    end
end
