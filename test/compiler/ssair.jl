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

function verify_ircode(ir)
    Compiler.verify_ir(ir)
    Compiler.verify_linetable(ir.linetable)
end

function singleblock_ircode(n::Int)
    insts = Compiler.InstructionStream(n)
    insts.inst .= map(1:n) do i
        # Dummy expression of form `initial_position => previous_ssavalue` to help visual
        # inspection:
        x = i == 1 ? nothing : Compiler.SSAValue(i - 1)
        Expr(:call, GlobalRef(Base, :(=>)), i, x)
    end
    insts.inst[end] = Core.ReturnNode(Core.SSAValue(n - 1))
    insts.line .= (n + 1) .+ range(1; step = 2, length = n)
    fill!(insts.type, Any)
    cfg = CFG([BasicBlock(Compiler.StmtRange(1, n), Int[], Int[])], Int[])
    Compiler.cfg_reindex!(cfg)
    linetable = [
        [LineInfoNode(Main, :f, :dummy, Int32(i), Int32(0)) for i in 1:n]
        [
            LineInfoNode(
                Main,
                Symbol(:f_, i, :_, j),
                :dummy,
                Int32(1000 * i + j),
                Int32(j == 1 ? i : n + 2(i - 1) + (j - 1)),
            ) for i in 1:n for j in 1:2
        ]
    ]
    ir = Compiler.IRCode(insts, cfg, linetable, [], Expr[], [])
    verify_ircode(ir)
    return ir
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
    let correct_idoms = Compiler.naive_idoms(cfg.blocks)
        @test Compiler.construct_domtree(cfg.blocks).idoms_bb == correct_idoms
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
    @test Core.Compiler.verify_ir(ir) == nothing
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

###
### CFG manipulation tools
###

function allocate_branches!(ir::Compiler.IRCode, positions_nbranches)
    blocks = Core.Compiler.allocate_goto_sequence!(
        ir,
        [p => 2n for (p, n) in positions_nbranches],
    )
    for sp in Core.Compiler.split_positions(blocks)
        for (n, block) in enumerate(Compiler.inserted_blocks(sp))
            if isodd(n)
                ibb1 = block.newbb
                ibb3 = ibb1 + 2
                b1 = ir.cfg.blocks[ibb1]
                ir.stmts.inst[last(b1.stmts)] = GotoIfNot(false, ibb3)
                Core.Compiler.cfg_insert_edge!(ir.cfg, ibb1, ibb3)
            end
        end
    end
    return blocks
end

inserted_block_ranges(info) = [sp.prebb:sp.postbb for sp in Compiler.split_positions(info)]

"""
    inlineinfo(ir::IRCode, line::Integer)

Extract inlining information at `line` that does not depend on `ir.linetable`.
"""
inlineinfo(ir, line) =
    map(Base.IRShow.compute_loc_stack(ir.linetable, Int32(line))) do i
        node = ir.linetable[i]
        (; node.method, node.file, node.line)
    end

"""
    check_linetable(ir, ir0, info)

Test `ir.linetable` invariances of `allocate_new_blocks!` where the arguments are used as in

```julia
ir = copy(ir0)
info = Compiler.allocate_new_blocks!(ir, ...)
```

or some equivalent code.
"""
function check_linetable(ir, ir0, info)
    (; positions_nblocks) = info
    function splabel((; index))
        origpos, _ = positions_nblocks[index]
        "Statement $origpos (= first(positions_nblocks[$index]))"
    end
    iblabel((; nth)) = "$nth-th inserted block at this split point"
    @testset "Goto nodes reflect original statement lines" begin
        @testset "$(splabel(sp))" for sp in Compiler.split_positions(info)
            origpos, _ = positions_nblocks[sp.index]
            moved = ir.stmts[first(ir.cfg.blocks[sp.postbb].stmts)]

            @testset "Moved statement has the same inline info stack" begin
                orig = ir0.stmts[origpos][:line]
                @test inlineinfo(ir, moved[:line]) == inlineinfo(ir0, orig)
            end

            @testset "Pre-split block" begin
                goto = ir.stmts[last(ir.cfg.blocks[sp.prebb].stmts)]
                @test goto[:line] == moved[:line]
            end

            @testset "$(iblabel(ib))" for ib in Compiler.inserted_blocks(sp)
                goto = ir.stmts[last(ir.cfg.blocks[ib.newbb].stmts)]
                @test goto[:line] == moved[:line]
            end
        end
    end
end

@testset "Split a block in two" begin
    ir0 = singleblock_ircode(3)
    ir = copy(ir0)
    info = Compiler.allocate_goto_sequence!(ir, [2 => 0])
    verify_ircode(ir)
    @test inserted_block_ranges(info) == [1:2]
    @test ir.cfg == CFG(
        [
            BasicBlock(Compiler.StmtRange(1, 2), Int[], [2]),
            BasicBlock(Compiler.StmtRange(3, 4), [1], Int[]),
        ],
        [3],
    )
    b1, _ = ir.cfg.blocks
    @test ir.stmts[last(b1.stmts)][:inst] == GotoNode(2)
    check_linetable(ir, ir0, info)
end

@testset "Add one branch (two new blocks) to a single-block IR" begin
    ir0 = singleblock_ircode(3)
    ir = copy(ir0)
    info = allocate_branches!(ir, [2 => 1])
    verify_ircode(ir)
    @test inserted_block_ranges(info) == [1:4]
    @test ir.cfg == CFG(
        [
            BasicBlock(Compiler.StmtRange(1, 2), Int64[], [2])
            BasicBlock(Compiler.StmtRange(3, 3), [1], [3, 4])
            BasicBlock(Compiler.StmtRange(4, 4), [2], [4])
            BasicBlock(Compiler.StmtRange(5, 6), [3, 2], Int64[])
        ],
        [3, 4, 5],
    )
    (b1, b2, b3, _) = ir.cfg.blocks
    @test ir.stmts.inst[last(b1.stmts)] == GotoNode(2)
    @test ir.stmts.inst[last(b2.stmts)] == GotoIfNot(false, 4)
    @test ir.stmts.inst[last(b3.stmts)] == GotoNode(4)
    check_linetable(ir, ir0, info)
end

@testset "Insert two more blocks to a two-block IR" begin
    ir0 = singleblock_ircode(3)
    @testset "Split a block in two" begin
        info = Compiler.allocate_goto_sequence!(ir0, [2 => 0])
        verify_ircode(ir0)
        @test inserted_block_ranges(info) == [1:2]
    end

    ir = copy(ir0)
    info = Compiler.allocate_goto_sequence!(ir, [2 => 1, 4 => 1])
    @test length(ir.stmts) == 8
    @test inserted_block_ranges(info) == [1:3, 4:6]
    verify_ircode(ir)
    @test ir.cfg == CFG(
        [
            BasicBlock(Compiler.StmtRange(1, 2), Int64[], [2])
            BasicBlock(Compiler.StmtRange(3, 3), [1], [3])
            BasicBlock(Compiler.StmtRange(4, 4), [2], [4])
            BasicBlock(Compiler.StmtRange(5, 6), [3], [5])
            BasicBlock(Compiler.StmtRange(7, 7), [4], [6])
            BasicBlock(Compiler.StmtRange(8, 8), [5], Int64[])
        ],
        [3, 4, 5, 7, 8],
    )
    @test [ir.stmts.inst[last(b.stmts)] for b in ir.cfg.blocks[1:end-1]] == GotoNode.(2:6)
    check_linetable(ir, ir0, info)
end

@testset "Split a block of a pre-compact IR (attach before)" begin
    ir0 = singleblock_ircode(3)
    st = Expr(:call, :new_instruction)
    Compiler.insert_node!(ir0, 2, Compiler.NewInstruction(st, Any))

    ir = copy(ir0)
    info = allocate_branches!(ir, [2 => 0])
    @test inserted_block_ranges(info) == [1:2]
    verify_ircode(ir)
    check_linetable(ir, ir0, info)

    ir = Core.Compiler.compact!(ir)
    verify_ircode(ir)
    @test ir.cfg == CFG(
        [
            BasicBlock(Compiler.StmtRange(1, 3), Int64[], [2])
            BasicBlock(Compiler.StmtRange(4, 5), [1], Int64[])
        ],
        [4],
    )
    @test ir.stmts[2][:inst] == st
end

@testset "Split a block of a pre-compact IR (attach after)" begin
    ir0 = singleblock_ircode(3)
    st = Expr(:call, :new_instruction)
    Compiler.insert_node!(ir0, 2, Compiler.NewInstruction(st, Any), true)

    ir = copy(ir0)
    info = allocate_branches!(ir, [2 => 0])
    @test inserted_block_ranges(info) == [1:2]
    verify_ircode(ir)
    check_linetable(ir, ir0, info)

    ir = Core.Compiler.compact!(ir)
    verify_ircode(ir)
    @test ir.cfg == CFG(
        [
            BasicBlock(Compiler.StmtRange(1, 2), Int64[], [2])
            BasicBlock(Compiler.StmtRange(3, 5), [1], Int64[])
        ],
        [3],
    )
    @test ir.stmts[4][:inst] == st
end
