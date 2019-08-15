# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core.IR
const Compiler = Core.Compiler

# TODO: this test is broken
#let code = Any[
#        Expr(:gotoifnot, SlotNumber(2), 4),
#        Expr(:(=), SlotNumber(3), 2),
#        # Test a SlotNumber as a value of a PhiNode
#        PhiNode(Any[2,3], Any[1, SlotNumber(3)]),
#        Expr(:return, SSAValue(3))
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
using .Compiler: CFG, BasicBlock

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
make_bb(preds, succs) = BasicBlock(Compiler.StmtRange(0, 0), preds, succs)
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
            let cfg′ = Compiler.copy(cfg)
                a && reverse!(cfg′.blocks[1].succs)
                b && reverse!(cfg′.blocks[2].succs)
                c && reverse!(cfg′.blocks[4].preds)
                d && reverse!(cfg′.blocks[5].preds)
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
    code = Compiler.IRCode(
        [], [], Int32[], UInt8[], cfg, LineInfoNode[], [], [], [])
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
