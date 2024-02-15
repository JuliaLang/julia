# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Meta
using Core.IR
const Compiler = Core.Compiler
using .Compiler: CFG, BasicBlock, NewSSAValue

include("irutils.jl")

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
#    NullLineInfo = Core.LineInfoNode(Main, Symbol(""), Symbol(""), Int32(0), UInt32(0))
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

# test code execution with the default compile-mode
module CompilerExecTest
include("interpreter_exec.jl")
end

# test code execution with the interpreter mode (compile=min)
module InterpreterExecTest
Base.Experimental.@compiler_options compile=min
include("interpreter_exec.jl")
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
    ir = Compiler.IRCode(insts, cfg, Core.LineInfoNode[], Any[], Expr[], Compiler.VarState[])
    compact = Compiler.IncrementalCompact(ir, true)
    @test length(compact.cfg_transform.result_bbs) == 4 && 0 in compact.cfg_transform.result_bbs[3].preds
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
let code = Any[
        # block 1
        Expr(:boundscheck),
        Core.Compiler.GotoIfNot(SSAValue(1), 6),
        # block 2
        Expr(:call, GlobalRef(Base, :size), Core.Compiler.Argument(3)),
        Core.Compiler.ReturnNode(),
        # block 3
        Core.PhiNode(),
        Core.Compiler.ReturnNode(),
        # block 4
        GlobalRef(Main, :something),
        GlobalRef(Main, :somethingelse),
        Expr(:call, Core.SSAValue(7), Core.SSAValue(8)),
        Core.Compiler.GotoIfNot(Core.SSAValue(9), 12),
        # block 5
        Core.Compiler.ReturnNode(Core.SSAValue(9)),
        # block 6
        Core.Compiler.ReturnNode(Core.SSAValue(9))
    ]
    ir = make_ircode(code)
    ir = Core.Compiler.compact!(ir, true)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# Test that the verifier doesn't choke on cglobals (which aren't linearized)
let code = Any[
        Expr(:call, GlobalRef(Main, :cglobal),
                    Expr(:call, Core.tuple, :(:c)), Nothing),
                    Core.Compiler.ReturnNode()
    ]
    ir = make_ircode(code)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# Test that GlobalRef in value position is non-canonical
let code = Any[
        Expr(:call, GlobalRef(Main, :something_not_defined_please))
        ReturnNode(SSAValue(1))
    ]
    ir = make_ircode(code; verify=false)
    ir = Core.Compiler.compact!(ir, true)
    @test_throws ErrorException Core.Compiler.verify_ir(ir, false)
end

# Issue #29107
let code = Any[
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
    ]
    ir = make_ircode(code)
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

# Make sure dead blocks that are removed are not still referenced in live phi nodes
let code = Any[
        # Block 1
        Core.Compiler.GotoNode(3),
        # Block 2 (no predecessors)
        Core.Compiler.ReturnNode(3),
        # Block 3
        Core.PhiNode(Int32[1, 2], Any[100, 200]),
        Core.Compiler.ReturnNode(Core.SSAValue(3))
    ]
    ir = make_ircode(code; verify=false)
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

let # https://github.com/JuliaLang/julia/issues/42258
    code = """
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
        """
    cmd = `$(Base.julia_cmd()) -g 2 -e $code`
    stderr = IOBuffer()
    @test success(pipeline(Cmd(cmd); stdout, stderr))
    @test readchomp(stderr) == ""
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

# slots after SSA conversion
function f_with_slots(a, b)
    # `c` and `d` are local variables
    c = a + b
    d = c > 0
    return (c, d)
end
let # #self#, a, b, c, d
    unopt = code_typed1(f_with_slots, (Int,Int); optimize=false)
    @test length(unopt.slotnames) == length(unopt.slotflags) == length(unopt.slottypes) == 5
    ir_withslots = first(only(Base.code_ircode(f_with_slots, (Int,Int); optimize_until="convert")))
    @test length(ir_withslots.argtypes) == 5
    # #self#, a, b
    opt = code_typed1(f_with_slots, (Int,Int); optimize=true)
    @test length(opt.slotnames) == length(opt.slotflags) == length(opt.slottypes) == 3
    ir_ssa = first(only(Base.code_ircode(f_with_slots, (Int,Int); optimize_until="slot2reg")))
    @test length(ir_ssa.argtypes) == 3
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
        Expr(:leave, Core.SSAValue(1)),
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
            inst = ir.stmts[j][:stmt]
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
    @test Meta.isexpr(add_stmt[:stmt], :call) && add_stmt[:stmt].args[3] == 42

    # replace the addition with a slightly different one
    inst = Core.Compiler.NewInstruction(Expr(:call, add_stmt[:stmt].args[1], add_stmt[:stmt].args[2], 999), Int)
    node = Core.Compiler.insert_node!(ir, 1, inst)
    Core.Compiler.setindex!(add_stmt, node, :stmt)

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
    nstmts = length(ir.stmts)
    invoke_idx = findfirst(@nospecialize(stmt)->Meta.isexpr(stmt, :invoke), ir.stmts.stmt)
    @test invoke !== nothing

    invoke_ssa = SSAValue(invoke_idx)
    newssa = insert_node!(ir, invoke_ssa, NewInstruction(Expr(:call, println, invoke_ssa), Nothing), #=attach_after=#true)
    newssa = insert_node!(ir, newssa, NewInstruction(Expr(:call, println, newssa), Nothing), #=attach_after=#true)

    ir = Core.Compiler.compact!(ir)
    @test length(ir.stmts) == nstmts + 2
    @test Meta.isexpr(ir.stmts[invoke_idx][:stmt], :invoke)
    call1 = ir.stmts[invoke_idx+1][:stmt]
    @test iscall((ir,println), call1)
    @test call1.args[2] === invoke_ssa
    call2 = ir.stmts[invoke_idx+2][:stmt]
    @test iscall((ir,println), call2)
    @test call2.args[2] === SSAValue(invoke_idx+1)
end

# Issue #50379 - insert_node!(::IncrementalCompact, ...) at end of basic block
let code = Any[
        # block 1
        #= %1: =# Expr(:boundscheck),
        #= %2: =# Core.Compiler.GotoIfNot(SSAValue(1), 4),
        # block 2
        #= %3: =# Expr(:call, println, Argument(1)),
        # block 3
        #= %4: =# Core.PhiNode(),
        #= %5: =# Core.Compiler.ReturnNode(),
    ]
    ir = make_ircode(code)

    # Insert another call at end of "block 2"
    compact = Core.Compiler.IncrementalCompact(ir)
    new_inst = NewInstruction(Expr(:call, println, Argument(1)), Nothing)
    insert_node!(compact, SSAValue(3), new_inst, #= attach_after =# true)

    # Complete iteration
    x = Core.Compiler.iterate(compact)
    while x !== nothing
        x = Core.Compiler.iterate(compact, x[2])
    end
    ir = Core.Compiler.complete(compact)

    @test Core.Compiler.verify_ir(ir) === nothing
end

# compact constant PiNode
let code = Any[
        PiNode(0.0, Const(0.0))
        ReturnNode(SSAValue(1))
    ]
    ir = make_ircode(code)
    ir = Core.Compiler.compact!(ir)
    @test fully_eliminated(ir)
end

# insert_node! with new instruction with flag computed
let ir = Base.code_ircode((Int,Int); optimize_until="inlining") do a, b
        a^b
    end |> only |> first
    invoke_idx = findfirst(@nospecialize(stmt)->Meta.isexpr(stmt, :invoke), ir.stmts.stmt)
    @test invoke_idx !== nothing
    invoke_expr = ir.stmts.stmt[invoke_idx]
    invoke_ssa = SSAValue(invoke_idx)

    # effect-ful node
    let compact = Core.Compiler.IncrementalCompact(Core.Compiler.copy(ir))
        insert_node!(compact, invoke_ssa, NewInstruction(Expr(:call, println, invoke_ssa), Nothing), #=attach_after=#true)
        state = Core.Compiler.iterate(compact)
        while state !== nothing
            state = Core.Compiler.iterate(compact, state[2])
        end
        ir = Core.Compiler.finish(compact)
        new_invoke_idx = findfirst(@nospecialize(stmt)->stmt==invoke_expr, ir.stmts.stmt)
        @test new_invoke_idx !== nothing
        new_call_idx = findfirst(ir.stmts.stmt) do @nospecialize(stmt)
            iscall((ir,println), stmt) && stmt.args[2] === SSAValue(new_invoke_idx)
        end
        @test new_call_idx !== nothing
        @test new_call_idx == new_invoke_idx+1
    end

    # effect-free node
    let compact = Core.Compiler.IncrementalCompact(Core.Compiler.copy(ir))
        insert_node!(compact, invoke_ssa, NewInstruction(Expr(:call, GlobalRef(Base, :add_int), invoke_ssa, invoke_ssa), Int), #=attach_after=#true)
        state = Core.Compiler.iterate(compact)
        while state !== nothing
            state = Core.Compiler.iterate(compact, state[2])
        end
        ir = Core.Compiler.finish(compact)

        ir = Core.Compiler.finish(compact)
        new_invoke_idx = findfirst(@nospecialize(stmt)->stmt==invoke_expr, ir.stmts.stmt)
        @test new_invoke_idx !== nothing
        new_call_idx = findfirst(ir.stmts.stmt) do @nospecialize(x)
            iscall((ir,Base.add_int), x) && x.args[2] === SSAValue(new_invoke_idx)
        end
        @test new_call_idx === nothing # should be deleted during the compaction
    end
end

@testset "GotoIfNot folding" begin
    # After IRCode conversion, following the targets of a GotoIfNot should never lead to
    # statically unreachable code.
    function f_with_maybe_nonbool_cond(a::Int, r::Bool)
        a = r ? true : a
        if a
            # The following conditional can be resolved statically, since `a === true`
            # This test checks that it becomes a static `goto` despite its wide slottype.
            x = a ? 1 : 2.
        else
            x = a ? 1 : 2.
        end
        return x
    end
    let
        # At least some statements should have been found to be statically unreachable and wrapped in Const(...)::Union{}
        unopt = code_typed1(f_with_maybe_nonbool_cond, (Int, Bool); optimize=false)
        @test any(j -> isa(unopt.code[j], Core.Const) && unopt.ssavaluetypes[j] == Union{}, 1:length(unopt.code))

        # Any GotoIfNot destinations after IRCode conversion should not be statically unreachable
        ircode = first(only(Base.code_ircode(f_with_maybe_nonbool_cond, (Int, Bool); optimize_until="convert")))
        for i = 1:length(ircode.stmts)
            expr = ircode.stmts[i][:stmt]
            if isa(expr, GotoIfNot)
                # If this statement is Core.Const(...)::Union{}, that means this code was not reached
                @test !(isa(ircode.stmts[i+1][:stmt], Core.Const) && (unopt.ssavaluetypes[i+1] === Union{}))
                @test !(isa(ircode.stmts[expr.dest][:stmt], Core.Const) && (unopt.ssavaluetypes[expr.dest] === Union{}))
            end
        end
    end
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
                ir.stmts.stmt[last(b1.stmts)] = GotoIfNot(false, ibb3)
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

function single_block(x)
    x+2x
end

#=
Input:

    #1
        %1 = $inst1     _
        %2 = $inst2      `-- split before %2
        return %2

Output:

    #1
        %1 = $inst1
        goto #2
    #2
        %3 = $inst2
        return %3
=#
@testset "Split a block in two" begin
    ir0, _ = only(Base.code_ircode(single_block, (Float64,), optimize_until = "compact 1"))
    @test length(ir0.stmts) == 3

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

#=
Input:

    #1
        %1 = $inst1    _
        %2 = $inst2     `-- split before %2 and insert two blocks
        return %2

Output:

    #1
        %1 = $inst1
        goto #2
    #2
        goto #4 if not false
    #3
        goto #4
    #4
        %5 = $inst2
        return %5
=#
@testset "Add one branch (two new blocks) to a single-block IR" begin
    ir0, _ = only(Base.code_ircode(single_block, (Float64,), optimize_until = "compact 1"))
    @test length(ir0.stmts) == 3

    ir = copy(ir0)
    info = allocate_branches!(ir, [2 => 1])
    # TODO: Access to undef in linetable
    # verify_ircode(ir)
    @test inserted_block_ranges(info) == [1:4]
    @test ir.cfg == CFG(
        [
            BasicBlock(Compiler.StmtRange(1, 2), Int[], [2])
            BasicBlock(Compiler.StmtRange(3, 3), [1], [3, 4])
            BasicBlock(Compiler.StmtRange(4, 4), [2], [4])
            BasicBlock(Compiler.StmtRange(5, 6), [3, 2], Int[])
        ],
        [3, 4, 5],
    )
    (b1, b2, b3, _) = ir.cfg.blocks
    @test ir.stmts.stmt[last(b1.stmts)] == GotoNode(2)
    @test ir.stmts.stmt[last(b2.stmts)] == GotoIfNot(false, 4)
    @test ir.stmts.stmt[last(b3.stmts)] == GotoNode(4)
    check_linetable(ir, ir0, info)
end

#=
Input:

    #1                 _
        %1 = $inst1     `-- split before %1 and insert one block
        goto #2
    #2
        %3 = $inst2    _
        return %3       `-- split before %4 (`return %3`) and insert one block

Output:

    #1
        goto #2
    #2
        goto #3
    #3
        %1 = $inst1
        goto #4
    #4
        %5 = $inst2
        goto #5
    #5
        goto #6
    #6
        return %5

This transformation is testing inserting multiple basic blocks at once.  It also tests that
inserting at boundary locations work.
=#
@testset "Insert two more blocks to a two-block IR" begin
    ir0, _ = only(Base.code_ircode(single_block, (Float64,), optimize_until = "compact 1"))
    @test length(ir0.stmts) == 3

    @testset "Split a block in two" begin
        info = Compiler.allocate_goto_sequence!(ir0, [2 => 0])
        verify_ircode(ir0)
        @test inserted_block_ranges(info) == [1:2]
    end

    ir = copy(ir0)
    info = Compiler.allocate_goto_sequence!(ir, [1 => 1, 4 => 1])
    @test length(ir.stmts) == 8
    @test inserted_block_ranges(info) == [1:3, 4:6]
    verify_ircode(ir)
    @test ir.cfg == CFG(
        [
            BasicBlock(Compiler.StmtRange(1, 1), Int[], [2])
            BasicBlock(Compiler.StmtRange(2, 2), [1], [3])
            BasicBlock(Compiler.StmtRange(3, 4), [2], [4])
            BasicBlock(Compiler.StmtRange(5, 6), [3], [5])
            BasicBlock(Compiler.StmtRange(7, 7), [4], [6])
            BasicBlock(Compiler.StmtRange(8, 8), [5], Int[])
        ],
        [2, 3, 5, 7, 8],
    )
    @test [ir.stmts.stmt[last(b.stmts)] for b in ir.cfg.blocks[1:end-1]] == GotoNode.(2:6)
    check_linetable(ir, ir0, info)
end

#=
Input:

    #1
        %1 = $inst1
        %3 = new_instruction()   _
        %2 = $inst2               `-- split before %2
        return %2

Output:

    #1
        %1 = $inst1
        %2 = new_instruction()     # in the pre-split-point BB
        goto #2
    #2
        %4 = $inst2
        return %4
=#
@testset "Split a block of a pre-compact IR (attach before)" begin
    ir0, _ = only(Base.code_ircode(single_block, (Float64,), optimize_until = "compact 1"))
    @test length(ir0.stmts) == 3

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
            BasicBlock(Compiler.StmtRange(1, 3), Int[], [2])
            BasicBlock(Compiler.StmtRange(4, 5), [1], Int[])
        ],
        [4],
    )
    @test ir.stmts[2][:inst] == st
end

#=
Input:

    #1
        %1 = $inst1              _
        %2 = $inst2               `-- split before %2
        %3 = new_instruction()
        return %2

Output:

    #1
        %1 = $inst1
        goto #2
    #2
        %3 = $inst2
        %4 = new_instruction()     # in the post-split-point BB
        return %3
=#
@testset "Split a block of a pre-compact IR (attach after)" begin
    ir0, _ = only(Base.code_ircode(single_block, (Float64,), optimize_until = "compact 1"))
    @test length(ir0.stmts) == 3

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
            BasicBlock(Compiler.StmtRange(1, 2), Int[], [2])
            BasicBlock(Compiler.StmtRange(3, 5), [1], Int[])
        ],
        [3],
    )
    @test ir.stmts[4][:inst] == st
end
