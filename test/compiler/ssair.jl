# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Meta
using Core.IR
const Compiler = Core.Compiler
using .Compiler: CFG, BasicBlock, NewSSAValue

include(normpath(@__DIR__, "irutils.jl"))

make_bb(preds, succs) = BasicBlock(Compiler.StmtRange(0, 0), preds, succs)

function make_ci(code)
    ci = (Meta.@lower 1 + 1).args[1]
    ci.code = code
    nstmts = length(ci.code)
    ci.ssavaluetypes = nstmts
    ci.codelocs = fill(Int32(1), nstmts)
    ci.ssaflags = fill(Int32(0), nstmts)
    return ci
end

# TODO: this test is broken
#let code = Any[
#        GotoIfNot(SlotNumber(2), 4),
#        Expr(:(=), SlotNumber(3), 2),
#        # Test a SlotNumber as a value of a PhiNode
#        PhiNode(Int32[2,3], Any[1, SlotNumber(3)]),
#        ReturnNode(SSAValue(3))
#    ]
#
#    ci = eval(Expr(:new, CodeInfo,
#        code,
#        nothing,
#        Any[Any, Any, Any],
#        Any[Any],
#        UInt8[0, 0, 0],
#        Any[Symbol("Self"), :arg, :slot],
#        false, false, false, false
#    ))
#
#    NullLineInfo = Core.LineInfoNode(Main, Symbol(""), Symbol(""), Int32(0), Int32(0))
#    Compiler.run_passes(ci, 1, [NullLineInfo])
#    # XXX: missing @test
#end

# Issue #31121

# We have the following CFG and corresponding DFS numbering:
#
#     CFG     DFS
#
#      A       1
#      | \     | \
#      B C     2 5
#     /|/     /|/
#    | D     | 3
#     \|      \|
#      E       4
#
# In the bug `E` got the wrong dominator (`B` instead of `A`), because the DFS
# tree had the wrong parent (i.e. we recorded the parent of `4` as `2` rather
# than `3`, so the idom search missed that `1` is `3`'s semi-dominator). Here
# we manually construct that CFG and verify that the DFS records the correct
# parent.
let cfg = CFG(BasicBlock[
    make_bb([]     , [2, 3]),
    make_bb([1]    , [4, 5]),
    make_bb([1]    , [4]   ),
    make_bb([2, 3] , [5]   ),
    make_bb([2, 4] , []    ),
], Int[])
    dfs = Compiler.DFS(cfg.blocks)
    @test dfs.from_pre[dfs.to_parent_pre[dfs.to_pre[5]]] == 4
    let correct_idoms = Compiler.naive_idoms(cfg.blocks),
        correct_pidoms = Compiler.naive_idoms(cfg.blocks, true)
        @test Compiler.construct_domtree(cfg.blocks).idoms_bb == correct_idoms
        @test Compiler.construct_postdomtree(cfg.blocks).idoms_bb == correct_pidoms
        # For completeness, reverse the order of pred/succ in the CFG and verify
        # the answer doesn't change (it does change the which node is chosen
        # as the semi-dominator, since it changes the DFS numbering).
        for (a, b, c, d) in Iterators.product(((true, false) for _ = 1:4)...)
            let blocks = copy(cfg.blocks)
                a && (blocks[1] = make_bb(blocks[1].preds, reverse(blocks[1].succs)))
                b && (blocks[2] = make_bb(blocks[2].preds, reverse(blocks[2].succs)))
                c && (blocks[4] = make_bb(reverse(blocks[4].preds), blocks[4].succs))
                d && (blocks[5] = make_bb(reverse(blocks[5].preds), blocks[5].succs))
                cfg′ = CFG(blocks, cfg.index)
                @test Compiler.construct_domtree(cfg′.blocks).idoms_bb == correct_idoms
                @test Compiler.construct_postdomtree(cfg′.blocks).idoms_bb == correct_pidoms
            end
        end
    end
end

# test >:
let
    f(a, b) = a >: b
    code_typed(f, Tuple{Any, Any})
    # XXX: missing @test
end

for compile in ("min", "yes")
    cmd = `$(Base.julia_cmd()) --compile=$compile interpreter_exec.jl`
    if !success(pipeline(Cmd(cmd, dir=@__DIR__); stdout=stdout, stderr=stderr))
        error("Interpreter test failed, cmd : $cmd")
    end
end

# Issue #27104
# Test whether meta nodes are still present after code optimization.
let
    @noinline f(x, y) = x + y
    @test any(code_typed(f)[1][1].code) do ex
        Meta.isexpr(ex, :meta)
    end
end

# PR #32145
# Make sure IncrementalCompact can handle blocks with predecessors of index 0
# while removing blocks with no predecessors.
let cfg = CFG(BasicBlock[
    make_bb([]        , [2, 4]),
    make_bb([1]       , [4, 5]),
    make_bb([]        , [4]   ), # should be removed
    make_bb([0, 1, 2] , [5]   ), # 0 predecessor should be preserved
    make_bb([2, 3]    , []    ),
], Int[])
    insts = Compiler.InstructionStream([], [], Any[], Int32[], UInt8[])
    code = Compiler.IRCode(insts, cfg, LineInfoNode[], [], Expr[], [])
    compact = Compiler.IncrementalCompact(code, true)
    @test length(compact.result_bbs) == 4 && 0 in compact.result_bbs[3].preds
end

# Issue #32579 - Optimizer bug involving type constraints
function f32579(x::Int, b::Bool)
    if b
        x = nothing
    end
    if isa(x, Int)
        y = x
    else
        y = x
    end
    if isa(y, Nothing)
        z = y
    else
        z = y
    end
    return z === nothing
end
@test f32579(0, true) === true
@test f32579(0, false) === false

# Test for bug caused by renaming blocks improperly, related to PR #32145
let ci = make_ci([
        # block 1
        Core.Compiler.GotoIfNot(Expr(:boundscheck), 6),
        # block 2
        Expr(:call, GlobalRef(Base, :size), Core.Compiler.Argument(3)),
        Core.Compiler.ReturnNode(),
        # block 3
        Core.PhiNode(),
        Core.Compiler.ReturnNode(),
        # block 4
        GlobalRef(Main, :something),
        GlobalRef(Main, :somethingelse),
        Expr(:call, Core.SSAValue(6), Core.SSAValue(7)),
        Core.Compiler.GotoIfNot(Core.SSAValue(8), 11),
        # block 5
        Core.Compiler.ReturnNode(Core.SSAValue(8)),
        # block 6
        Core.Compiler.ReturnNode(Core.SSAValue(8))
    ])
    ir = Core.Compiler.inflate_ir(ci)
    ir = Core.Compiler.compact!(ir, true)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# Test that the verifier doesn't choke on cglobals (which aren't linearized)
let ci = make_ci([
        Expr(:call, GlobalRef(Main, :cglobal),
                    Expr(:call, Core.tuple, :(:c)), Nothing),
                    Core.Compiler.ReturnNode()
    ])
    ir = Core.Compiler.inflate_ir(ci)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# Test that GlobalRef in value position is non-canonical
let ci = make_ci([
        Expr(:call, GlobalRef(Main, :something_not_defined_please))
        ReturnNode(SSAValue(1))
    ])
    ir = Core.Compiler.inflate_ir(ci)
    ir = Core.Compiler.compact!(ir, true)
    @test_throws ErrorException Core.Compiler.verify_ir(ir, false)
end

# Issue #29107
let ci = make_ci([
        # Block 1
        Core.Compiler.GotoNode(6),
        # Block 2
        # The following phi node gets deleted because it only has one edge, so
        # the call to `something` is made to use the value of `something2()`,
        # even though this value is defined after it. We don't want this to
        # happen even though this block is dead because subsequent optimization
        # passes may look at all code, dead or not.
        Core.PhiNode(Int32[2], Any[Core.SSAValue(4)]),
        Expr(:call, :something, Core.SSAValue(2)),
        Expr(:call, :something2),
        Core.Compiler.GotoNode(2),
        # Block 3
        Core.Compiler.ReturnNode(1000)
    ])
    ir = Core.Compiler.inflate_ir(ci)
    ir = Core.Compiler.compact!(ir, true)
    # Make sure that if there is a call to `something` (block 2 should be
    # removed entirely with working DCE), it doesn't use any SSA values that
    # come after it.
    for i in 1:length(ir.stmts)
        s = ir.stmts[i]
        if Meta.isexpr(s, :call) && s.args[1] === :something
            if isa(s.args[2], SSAValue)
                @test s.args[2].id <= i
            end
        end
    end
end

# Make sure dead blocks that are removed are not still referenced in live phi
# nodes
let ci = make_ci([
        # Block 1
        Core.Compiler.GotoNode(3),
        # Block 2 (no predecessors)
        Core.Compiler.ReturnNode(3),
        # Block 3
        Core.PhiNode(Int32[1, 2], Any[100, 200]),
        Core.Compiler.ReturnNode(Core.SSAValue(3))
    ])
    ir = Core.Compiler.inflate_ir(ci)
    ir = Core.Compiler.compact!(ir, true)
    @test Core.Compiler.verify_ir(ir) == nothing
end

# issue #37919
let ci = code_lowered(()->@isdefined(_not_def_37919_), ())[1]
    ir = Core.Compiler.inflate_ir(ci)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# Test dynamic update of domtree with edge insertions and deletions in the
# following CFG:
#
#     1,1
#     |  \
#     |   \
#     |    3,4 <
#     |    |    \
#     2,2  4,5   |
#     |    |    /
#     |    6,6 /
#     |   /
#     |  /
#     5,3
#
# Nodes indicate BB number, preorder number
# Edges point down, except the arrow that points up
let cfg = CFG(BasicBlock[
        make_bb([],     [3, 2]), # the order of the successors is deliberate
        make_bb([1],    [5]),    # and is to determine the preorder numbers
        make_bb([1, 6], [4]),
        make_bb([3],    [6]),
        make_bb([2, 6], []),
        make_bb([4],    [5, 3]),
    ], Int[])
    domtree = Compiler.construct_domtree(cfg.blocks)
    @test domtree.dfs_tree.to_pre == [1, 2, 4, 5, 3, 6]
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 1, 4]

    # Test removal of edge between a parent and child in the DFS tree, which
    # should trigger complete recomputation of domtree (first case in algorithm
    # for removing edge from domtree dynamically)
    Compiler.cfg_delete_edge!(cfg, 2, 5)
    Compiler.domtree_delete_edge!(domtree, cfg.blocks, 2, 5)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 6, 4]
    # Add edge back (testing first case for insertion)
    Compiler.cfg_insert_edge!(cfg, 2, 5)
    Compiler.domtree_insert_edge!(domtree, cfg.blocks, 2, 5)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 1, 4]

    # Test second case in algorithm for removing edges from domtree, in which
    # `from` is on a semidominator path from the semidominator of `to` to `to`
    Compiler.cfg_delete_edge!(cfg, 6, 5)
    Compiler.domtree_delete_edge!(domtree, cfg.blocks, 6, 5)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 2, 4]
    # Add edge back (testing second case for insertion)
    Compiler.cfg_insert_edge!(cfg, 6, 5)
    Compiler.domtree_insert_edge!(domtree, cfg.blocks, 6, 5)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 1, 4]

    # Test last case for removing edges, in which edge does not satisfy either
    # of the above conditions
    Compiler.cfg_delete_edge!(cfg, 6, 3)
    Compiler.domtree_delete_edge!(domtree, cfg.blocks, 6, 3)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 1, 4]
    # Add edge back (testing second case for insertion)
    Compiler.cfg_insert_edge!(cfg, 6, 3)
    Compiler.domtree_insert_edge!(domtree, cfg.blocks, 6, 3)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 1, 4]

    # Try removing all edges from root
    Compiler.cfg_delete_edge!(cfg, 1, 2)
    Compiler.domtree_delete_edge!(domtree, cfg.blocks, 1, 2)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 0, 1, 3, 6, 4]
    Compiler.cfg_delete_edge!(cfg, 1, 3)
    Compiler.domtree_delete_edge!(domtree, cfg.blocks, 1, 3)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 0, 0, 0, 0, 0]
    # Add edges back
    Compiler.cfg_insert_edge!(cfg, 1, 2)
    Compiler.domtree_insert_edge!(domtree, cfg.blocks, 1, 2)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 0, 0, 2, 0]
    Compiler.cfg_insert_edge!(cfg, 1, 3)
    Compiler.domtree_insert_edge!(domtree, cfg.blocks, 1, 3)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks) == [0, 1, 1, 3, 1, 4]
end

# Issue #41975 - SSA conversion drops type check
f_if_typecheck() = (if nothing; end; unsafe_load(Ptr{Int}(0)))
@test_throws TypeError f_if_typecheck()

@test let # https://github.com/JuliaLang/julia/issues/42258
    code = quote
        function foo()
            a = @noinline rand(rand(0:10))
            if isempty(a)
                err = BoundsError(a)
                throw(err)
                return nothing
            end
            return a
        end
        code_typed(foo; optimize=true)

        code_typed(Core.Compiler.setindex!, (Core.Compiler.UseRef,Core.Compiler.NewSSAValue); optimize=true)
    end |> string
    cmd = `$(Base.julia_cmd()) -g 2 -e $code`
    stderr = IOBuffer()
    success(pipeline(Cmd(cmd); stdout=stdout, stderr=stderr)) && isempty(String(take!(stderr)))
end

@testset "code_ircode" begin
    @test first(only(Base.code_ircode(+, (Float64, Float64)))) isa Compiler.IRCode
    @test first(only(Base.code_ircode(+, (Float64, Float64); optimize_until = 3))) isa
          Compiler.IRCode
    @test first(only(Base.code_ircode(+, (Float64, Float64); optimize_until = "SROA"))) isa
          Compiler.IRCode

    function demo(f)
        f()
        f()
        f()
    end
    @test first(only(Base.code_ircode(demo))) isa Compiler.IRCode
    @test first(only(Base.code_ircode(demo; optimize_until = 3))) isa Compiler.IRCode
    @test first(only(Base.code_ircode(demo; optimize_until = "SROA"))) isa Compiler.IRCode
end

let
    function test_useref(stmt, v, op)
        if isa(stmt, Expr)
            @test stmt.args[op] === v
        elseif isa(stmt, GotoIfNot)
            @test stmt.cond === v
        elseif isa(stmt, ReturnNode) || isa(stmt, UpsilonNode)
            @test stmt.val === v
        elseif isa(stmt, SSAValue) || isa(stmt, NewSSAValue)
            @test stmt === v
        elseif isa(stmt, PiNode)
            @test stmt.val === v && stmt.typ === typeof(stmt)
        elseif isa(stmt, PhiNode) || isa(stmt, PhiCNode)
            @test stmt.values[op] === v
        end
    end

    function _test_userefs(@nospecialize stmt)
        ex = Expr(:call, :+, Core.SSAValue(3), 1)
        urs = Core.Compiler.userefs(stmt)::Core.Compiler.UseRefIterator
        it = Core.Compiler.iterate(urs)
        while it !== nothing
            ur = getfield(it, 1)::Core.Compiler.UseRef
            op = getfield(it, 2)::Int
            v1 = Core.Compiler.getindex(ur)
            # set to dummy expression and then back to itself to test `_useref_setindex!`
            v2 = Core.Compiler.setindex!(ur, ex)
            test_useref(v2, ex, op)
            Core.Compiler.setindex!(ur, v1)
            @test Core.Compiler.getindex(ur) === v1
            it = Core.Compiler.iterate(urs, op)
        end
    end

    function test_userefs(body)
        for stmt in body
            _test_userefs(stmt)
        end
    end

    # this isn't valid code, we just care about looking at a variety of IR nodes
    body = Any[
        Expr(:enter, 11),
        Expr(:call, :+, SSAValue(3), 1),
        Expr(:throw_undef_if_not, :expected, false),
        Expr(:leave, 1),
        Expr(:(=), SSAValue(1), Expr(:call, :+, SSAValue(3), 1)),
        UpsilonNode(),
        UpsilonNode(SSAValue(2)),
        PhiCNode(Any[SSAValue(5), SSAValue(7), SSAValue(9)]),
        PhiCNode(Any[SSAValue(6)]),
        PhiNode(Int32[8], Any[SSAValue(7)]),
        PiNode(SSAValue(6), GotoNode),
        GotoIfNot(SSAValue(3), 10),
        GotoNode(5),
        SSAValue(7),
        NewSSAValue(9),
        ReturnNode(SSAValue(11)),
    ]

    test_userefs(body)
end

let ir = Base.code_ircode((Bool,Any)) do c, x
        println(x, 1) #1
        if c
            println(x, 2) #2
        else
            println(x, 3) #3
        end
        println(x, 4) #4
    end |> only |> first
    # IR legality check
    @test length(ir.cfg.blocks) == 4
    for i = 1:4
        @test any(ir.cfg.blocks[i].stmts) do j
            inst = ir.stmts[j][:inst]
            iscall((ir, println), inst) &&
            inst.args[3] == i
        end
    end
    # domination analysis
    domtree = Core.Compiler.construct_domtree(ir.cfg.blocks)
    @test Core.Compiler.dominates(domtree, 1, 2)
    @test Core.Compiler.dominates(domtree, 1, 3)
    @test Core.Compiler.dominates(domtree, 1, 4)
    for i = 2:4
        for j = 1:4
            i == j && continue
            @test !Core.Compiler.dominates(domtree, i, j)
        end
    end
    # post domination analysis
    post_domtree = Core.Compiler.construct_postdomtree(ir.cfg.blocks)
    @test Core.Compiler.postdominates(post_domtree, 4, 1)
    @test Core.Compiler.postdominates(post_domtree, 4, 2)
    @test Core.Compiler.postdominates(post_domtree, 4, 3)
    for i = 1:3
        for j = 1:4
            i == j && continue
            @test !Core.Compiler.postdominates(post_domtree, i, j)
        end
    end
end

@testset "issue #46967: undef stmts introduced by compaction" begin
    # generate some IR
    function foo(i)
        j = i+42
        j == 1 ? 1 : 2
    end
    ir = only(Base.code_ircode(foo, (Int,)))[1]
    instructions = length(ir.stmts)

    # get the addition instruction
    add_stmt = ir.stmts[1]
    @test Meta.isexpr(add_stmt[:inst], :call) && add_stmt[:inst].args[3] == 42

    # replace the addition with a slightly different one
    inst = Core.Compiler.NewInstruction(Expr(:call, add_stmt[:inst].args[1], add_stmt[:inst].args[2], 999), Int)
    node = Core.Compiler.insert_node!(ir, 1, inst)
    Core.Compiler.setindex!(add_stmt, node, :inst)

    # perform compaction (not by calling compact! because with DCE the bug doesn't trigger)
    compact = Core.Compiler.IncrementalCompact(ir)
    state = Core.Compiler.iterate(compact)
    while state !== nothing
        state = Core.Compiler.iterate(compact, state[2])
    end
    ir = Core.Compiler.complete(compact)

    # test that the inserted node was compacted
    @test Core.Compiler.length(ir.new_nodes) == 0

    # test that we performed copy propagation, but that the undef node was trimmed
    @test length(ir.stmts) == instructions

    @test show(devnull, ir) === nothing
end

@testset "IncrementalCompact statefulness" begin
    foo(i) = i == 1 ? 1 : 2
    ir = only(Base.code_ircode(foo, (Int,)))[1]
    compact = Core.Compiler.IncrementalCompact(ir)

    # set up first iterator
    x = Core.Compiler.iterate(compact)
    x = Core.Compiler.iterate(compact, x[2])

    # set up second iterator
    x = Core.Compiler.iterate(compact)

    # consume remainder
    while x !== nothing
        x = Core.Compiler.iterate(compact, x[2])
    end

    ir = Core.Compiler.complete(compact)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# insert_node! operations
# =======================

import Core: SSAValue
import Core.Compiler: NewInstruction, insert_node!

# insert_node! for pending node
let ir = Base.code_ircode((Int,Int); optimize_until="inlining") do a, b
        a^b
    end |> only |> first
    @test length(ir.stmts) == 2
    @test Meta.isexpr(ir.stmts[1][:inst], :invoke)

    newssa = insert_node!(ir, SSAValue(1), NewInstruction(Expr(:call, println, SSAValue(1)), Nothing), #=attach_after=#true)
    newssa = insert_node!(ir, newssa, NewInstruction(Expr(:call, println, newssa), Nothing), #=attach_after=#true)

    ir = Core.Compiler.compact!(ir)
    @test length(ir.stmts) == 4
    @test Meta.isexpr(ir.stmts[1][:inst], :invoke)
    call1 = ir.stmts[2][:inst]
    @test iscall((ir,println), call1)
    @test call1.args[2] === SSAValue(1)
    call2 = ir.stmts[3][:inst]
    @test iscall((ir,println), call2)
    @test call2.args[2] === SSAValue(2)
end

# insert_node! with new instruction with flag computed
let ir = Base.code_ircode((Int,Int); optimize_until="inlining") do a, b
        a^b
    end |> only |> first
    invoke_idx = findfirst(ir.stmts.inst) do @nospecialize(x)
        Meta.isexpr(x, :invoke)
    end
    @test invoke_idx !== nothing
    invoke_expr = ir.stmts.inst[invoke_idx]

    # effect-ful node
    let compact = Core.Compiler.IncrementalCompact(Core.Compiler.copy(ir))
        insert_node!(compact, SSAValue(1), NewInstruction(Expr(:call, println, SSAValue(1)), Nothing), #=attach_after=#true)
        state = Core.Compiler.iterate(compact)
        while state !== nothing
            state = Core.Compiler.iterate(compact, state[2])
        end
        ir = Core.Compiler.finish(compact)
        new_invoke_idx = findfirst(ir.stmts.inst) do @nospecialize(x)
            x == invoke_expr
        end
        @test new_invoke_idx !== nothing
        new_call_idx = findfirst(ir.stmts.inst) do @nospecialize(x)
            iscall((ir,println), x) && x.args[2] === SSAValue(invoke_idx)
        end
        @test new_call_idx !== nothing
        @test new_call_idx == new_invoke_idx+1
    end

    # effect-free node
    let compact = Core.Compiler.IncrementalCompact(Core.Compiler.copy(ir))
        insert_node!(compact, SSAValue(1), NewInstruction(Expr(:call, GlobalRef(Base, :add_int), SSAValue(1), SSAValue(1)), Int), #=attach_after=#true)
        state = Core.Compiler.iterate(compact)
        while state !== nothing
            state = Core.Compiler.iterate(compact, state[2])
        end
        ir = Core.Compiler.finish(compact)

        ir = Core.Compiler.finish(compact)
        new_invoke_idx = findfirst(ir.stmts.inst) do @nospecialize(x)
            x == invoke_expr
        end
        @test new_invoke_idx !== nothing
        new_call_idx = findfirst(ir.stmts.inst) do @nospecialize(x)
            iscall((ir,Base.add_int), x) && x.args[2] === SSAValue(invoke_idx)
        end
        @test new_call_idx === nothing # should be deleted during the compaction
    end
end
