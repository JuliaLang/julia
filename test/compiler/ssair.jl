# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Meta
using Core.IR
const Compiler = Core.Compiler
using .Compiler: CFG, BasicBlock

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
#    NullLineInfo = Core.LineInfoNode(Symbol(""), Symbol(""), 0, 0)
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
    dfs = Compiler.DFS(cfg, Compiler.BBNumber(1))
    @test dfs.numbering[dfs.parents[dfs.reverse[5]]] == 4
    let correct_idoms = Compiler.naive_idoms(cfg)
        @test Compiler.SNCA(cfg) == correct_idoms
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
                @test Compiler.SNCA(cfg′) == correct_idoms
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
    code = Compiler.IRCode(insts, cfg, LineInfoNode[], [], [], [])
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
    @test Core.Compiler.verify_ir(ir) == nothing
end

# Test that GlobalRef in value position is non-canonical
let ci = (Meta.@lower 1 + 1).args[1]
    ci.code = [
        Expr(:call, GlobalRef(Main, :something_not_defined_please))
        ReturnNode(SSAValue(1))
    ]
    nstmts = length(ci.code)
    ci.ssavaluetypes = nstmts
    ci.codelocs = fill(Int32(1), nstmts)
    ci.ssaflags = fill(Int32(0), nstmts)
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
        if isa(s, Expr) && s.head == :call && s.args[1] == :something
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
