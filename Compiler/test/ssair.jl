# This file is a part of Julia. License is MIT: https://julialang.org/license

include("setup_Compiler.jl")
include("irutils.jl")

using Test

using .Compiler: BasicBlock, CFG, NewSSAValue

make_bb(preds, succs) = BasicBlock(Compiler.StmtRange(0, 0), preds, succs)

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
        @test Compiler.construct_domtree(cfg).idoms_bb == correct_idoms
        @test Compiler.construct_postdomtree(cfg).idoms_bb == correct_pidoms
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
                @test Compiler.construct_domtree(cfg′).idoms_bb == correct_idoms
                @test Compiler.construct_postdomtree(cfg′).idoms_bb == correct_pidoms
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
    insts = Compiler.InstructionStream([], [], Compiler.CallInfo[], Int32[], UInt32[])
    di = Compiler.DebugInfoStream(insts.line)
    ir = Compiler.IRCode(insts, cfg, di, Any[], Expr[], Compiler.VarState[])
    compact = Compiler.IncrementalCompact(ir, true)
    @test length(compact.cfg_transform.result_bbs) == 4 && 0 in compact.cfg_transform.result_bbs[3].preds
end

# Test that removing a self-edge during compaction only scans compacted phi statements.
let code = Any[
        # Block 1
        Compiler.GotoNode(2),
        # Block 2
        Core.PhiNode(Int32[1, 3], Any[1, 2]),
        Compiler.GotoIfNot(true, 2),
        # Block 3
        Compiler.ReturnNode(0),
    ]
    ir = make_ircode(code)
    ir = Compiler.compact!(ir, true)
    @test Compiler.verify_ir(ir) === nothing
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
        Compiler.GotoIfNot(SSAValue(1), 6),
        # block 2
        Expr(:call, size, Compiler.Argument(3)),
        Compiler.ReturnNode(),
        # block 3
        Core.PhiNode(),
        Compiler.ReturnNode(),
        # block 4
        GlobalRef(Main, :something),
        GlobalRef(Main, :somethingelse),
        Expr(:call, Core.SSAValue(7), Core.SSAValue(8)),
        Compiler.GotoIfNot(Core.SSAValue(9), 12),
        # block 5
        Compiler.ReturnNode(Core.SSAValue(9)),
        # block 6
        Compiler.ReturnNode(Core.SSAValue(9))
    ]
    ir = make_ircode(code)
    ir = Compiler.compact!(ir, true)
    @test Compiler.verify_ir(ir) === nothing
end

# Test that the verifier accepts a syntactic-tuple first argument to :foreignglobal (cglobal)
let code = Any[
        Expr(:foreignglobal, Expr(:tuple, QuoteNode(:c))),
        Compiler.ReturnNode()
    ]
    ir = make_ircode(code)
    @test Compiler.verify_ir(ir) === nothing
end

# Test that GlobalRef in value position is non-canonical
let code = Any[
        Expr(:call, GlobalRef(Main, :something_not_defined_please))
        ReturnNode(SSAValue(1))
    ]
    ir = make_ircode(code; verify=false)
    ir = Compiler.compact!(ir, true)
    @test_throws ["IR verification failed.", "Code location: "] Compiler.verify_ir(ir, false)
end

# Test that static_parameter in value position is non-canonical
let code = Any[
        Expr(:call, identity, Expr(:static_parameter, 1))
        ReturnNode(SSAValue(1))
    ]
    ir = make_ircode(code; verify=false)
    ir = Compiler.compact!(ir, true)
    @test_throws ["IR verification failed.", "Code location: "] Compiler.verify_ir(ir, false)
end

# Test that the verifier rejects each kind of malformed IR, and says why
function verifier_output(f)
    return mktemp() do path, io
        err = redirect_stdio(; stdout=io, stderr=io) do
            try
                f()
            catch err
                err
            end
        end
        flush(io)
        return err, read(path, String)
    end
end
function set_bb_stmts!(ir::Compiler.IRCode, bb::Int, first::Int, last::Int)
    (; preds, succs) = ir.cfg.blocks[bb]
    ir.cfg.blocks[bb] = BasicBlock(Compiler.StmtRange(first, last), preds, succs)
    return ir
end
function retarget_edge!(ir::Compiler.IRCode, from::Int, old::Int, new::Int)
    blocks = ir.cfg.blocks
    replace!(blocks[from].succs, old => new)
    filter!(!=(from), blocks[old].preds)
    push!(blocks[new].preds, from)
    return ir
end
tuple_call(args...) = Expr(:call, GlobalRef(Core, :tuple), args...)
branch_ir() = make_ircode(Any[GotoIfNot(true, 3), ReturnNode(1), ReturnNode(2)])
enter_ir() = make_ircode(Any[EnterNode(3), ReturnNode(1), ReturnNode(2)])
modified(f, ir::Compiler.IRCode=branch_ir()) = (f(ir); ir)
let cases = Pair{String,Compiler.IRCode}[
        "CFG index length (0) does not correspond to # of blocks 3" =>
            modified(ir -> empty!(ir.cfg.index)),
        "IR type length is invalid 4 / 3" => modified(ir -> push!(ir.stmts.type, Any)),
        "IR info length is invalid 4 / 3" => modified(ir -> push!(ir.stmts.info, Compiler.NoCallInfo())),
        "IR line length is invalid 10 / 9" => modified(ir -> push!(ir.stmts.line, 0)),
        "IR flag length is invalid 4 / 3" => modified(ir -> push!(ir.stmts.flag, 0x0)),
        "Predecessor 9 of block 2 out of bounds for IR" =>
            modified(ir -> push!(ir.cfg.blocks[2].preds, 9)),
        "Predecessor 2 of block 3 not in successor list" =>
            modified(ir -> push!(ir.cfg.blocks[3].preds, 2)),
        "Double edge from 1 to 2 not correctly accounted" =>
            modified(ir -> (ir.cfg.blocks[1].succs .= 2; empty!(ir.cfg.blocks[3].preds))),
        "Successor 9 of block 2 out of bounds for IR" =>
            modified(ir -> push!(ir.cfg.blocks[2].succs, 9)),
        "Successor 3 of block 2 not in predecessor list" =>
            modified(ir -> push!(ir.cfg.blocks[2].succs, 3)),
        "First statement of BB 3 (0) out of bounds for IR (length=3)" =>
            modified(ir -> set_bb_stmts!(ir, 3, 0, 3)),
        "Last statement of BB 3 (9) out of bounds for IR (length=3)" =>
            modified(ir -> set_bb_stmts!(ir, 3, 3, 9)),
        "End of BB 1 (1) is not one less than CFG index (3)" =>
            modified(ir -> (ir.cfg.index[1] = 3)),
        "First statement of BB 2 (3) does not match end of previous (1)" =>
            modified(ir -> (set_bb_stmts!(ir, 2, 3, 3); ir.cfg.index[2] = 4)),
        "Block 2 ends in return or unreachable, but has successors" =>
            modified(ir -> (push!(ir.cfg.blocks[2].succs, 3); push!(ir.cfg.blocks[3].preds, 2))),
        "does not match GotoNode terminator (3)" =>
            modified(ir -> retarget_edge!(ir, 1, 3, 2),
                make_ircode(Any[GotoNode(3), ReturnNode(1), ReturnNode(2)])),
        "Block 1 terminator forms a double edge to block 2" =>
            make_ircode(Any[GotoIfNot(true, 2), ReturnNode(1)]; verify=false),
        "does not match GotoIfNot terminator" => modified(ir -> retarget_edge!(ir, 1, 3, 1)),
        "does not match :enter terminator" =>
            modified(ir -> (filter!(!=(3), ir.cfg.blocks[1].succs); empty!(ir.cfg.blocks[3].preds)),
                enter_ir()),
        "does not match :enter terminator" => modified(ir -> (ir.stmts.stmt[1] = EnterNode(2)), enter_ir()),
        "does not match fall-through terminator %2" =>
            modified(ir -> retarget_edge!(ir, 2, 3, 1),
                make_ircode(Any[GotoIfNot(true, 3), tuple_call(), ReturnNode(1)])),
        # an `:enter` must end its block too
        "does not match fall-through terminator %2" =>
            modified(make_ircode(Any[EnterNode(4), tuple_call(), ReturnNode(1), ReturnNode(2)])) do ir
                set_bb_stmts!(ir, 1, 1, 2)
                set_bb_stmts!(ir, 2, 3, 3)
                ir.cfg.index[1] = 3
            end,
        "End of last BB 1 does not match last IR statement 2" =>
            modified(ir -> (empty!(ir.cfg.index); resize!(ir.cfg.blocks, 1)),
                make_ircode(Any[ReturnNode(1), ReturnNode(2)])),
        "φ node 4 is not at the beginning of the basic block 3" =>
            make_ircode(Any[GotoIfNot(true, 3), GotoNode(3), tuple_call(),
                PhiNode(Int32[1, 2], Any[1, 2]), ReturnNode(SSAValue(4))]; verify=false),
        "Edge list φ node 3 in bb 3 not unique (double edge?)" =>
            make_ircode(Any[GotoIfNot(true, 3), GotoNode(3),
                PhiNode(Int32[1, 1], Any[1, 2]), ReturnNode(SSAValue(3))]; verify=false),
        "of φ node 3 not in predecessor list" =>
            modified(ir -> (ir.stmts.stmt[3] = PhiNode(Int32[1, 3], Any[1, 2])),
                make_ircode(Any[GotoIfNot(true, 3), GotoNode(3),
                    PhiNode(Int32[1, 2], Any[1, 2]), ReturnNode(SSAValue(3))])),
        # a non-φ statement among the φ nodes is checked like an operand
        "Left over slot detected in converted IR" =>
            make_ircode(Any[GotoIfNot(true, 3), GotoNode(3), PhiNode(Int32[1, 2], Any[1, 2]),
                SlotNumber(2), PhiNode(Int32[1, 2], Any[1, 2]), tuple_call(),
                ReturnNode(SSAValue(3))]; verify=false),
        "Operand 1 of PhiC node 1 must be an SSA Value." =>
            make_ircode(Any[PhiCNode(Any[1]), ReturnNode(1)]; verify=false),
        "Operand 1 of PhiC node 2 must reference an Upsilon node." =>
            make_ircode(Any[tuple_call(), PhiCNode(Any[SSAValue(1)]), ReturnNode(1)]; verify=false),
        "Terminator 1 in bb 1 is not the last statement in the block" =>
            modified(ir -> (ir.stmts.stmt[1] = ReturnNode(2)),
                make_ircode(Any[tuple_call(), ReturnNode(1)])),
        "Explicit terminators (other than ReturnNode) must have `Any` type" =>
            modified(ir -> (ir.stmts.type[1] = Int), make_ircode(Any[GotoNode(2), ReturnNode(1)])),
        "Assignment should have been removed during SSA conversion" =>
            make_ircode(Any[Expr(:(=), SSAValue(1), 1), ReturnNode(1)]; verify=false),
        "malformed isdefined" =>
            make_ircode(Any[Expr(:isdefined, Argument(1), 1, 2), ReturnNode(1)]; verify=false),
        "malformed throw_undef_if_not" =>
            make_ircode(Any[Expr(:throw_undef_if_not, :x, true, 1, 2), ReturnNode(1)]; verify=false),
        "Malformed :leave - Expected `Nothing` or SSAValue" =>
            make_ircode(Any[Expr(:leave, 1), ReturnNode(1)]; verify=false),
        "Malformed :leave - argument ssavalue should point to `nothing` or :enter" =>
            make_ircode(Any[tuple_call(), Expr(:leave, SSAValue(1)), ReturnNode(1)]; verify=false),
        "Def (0) is invalid in final IR" =>
            make_ircode(Any[tuple_call(SSAValue(0)), ReturnNode(1)]; verify=false),
        "Def (-1) is invalid in final IR" =>
            make_ircode(Any[tuple_call(SSAValue(-1)), ReturnNode(1)]; verify=false),
        "Def (5) points to non-existent new node" =>
            make_ircode(Any[tuple_call(SSAValue(5)), ReturnNode(1)]; verify=false),
        "Def (2) does not dominate use (1) in same BB" =>
            make_ircode(Any[tuple_call(SSAValue(2)), tuple_call(), ReturnNode(1)]; verify=false),
        "Basic Block 2 does not dominate block 3 (tried to use value %2 at %4)" =>
            make_ircode(Any[GotoIfNot(true, 4), tuple_call(), ReturnNode(1),
                tuple_call(SSAValue(2)), ReturnNode(1)]; verify=false),
        "At statement %2: Invalid use of value statement or terminator %1" =>
            make_ircode(Any[GotoNode(2), tuple_call(SSAValue(1)), ReturnNode(1)]; verify=false),
        "Basic Block 2 does not dominate block 3 (tried to use value %2 at %4)" =>
            make_ircode(Any[GotoIfNot(true, 4), tuple_call(), ReturnNode(1),
                ReturnNode(SSAValue(2))]; verify=false),
        "Basic Block 2 does not dominate block 3 (tried to use value %2 at %4)" =>
            make_ircode(Any[GotoIfNot(true, 4), tuple_call(), ReturnNode(1),
                GotoIfNot(SSAValue(2), 6), ReturnNode(1), ReturnNode(2)]; verify=false),
        "At statement %2: Invalid use of value statement or terminator %1" =>
            make_ircode(Any[GotoNode(2), ReturnNode(SSAValue(1))]; verify=false),
        "At statement %1: Left over SSA marker" =>
            make_ircode(Any[tuple_call(Compiler.OldSSAValue(1)), ReturnNode(1)]; verify=false),
        "At statement %1: Left over SSA marker" =>
            make_ircode(Any[tuple_call(NewSSAValue(1)), ReturnNode(1)]; verify=false),
    ]
    @testset "$msg" for (msg, ir) in cases
        err, out = verifier_output(() -> Compiler.verify_ir(ir))
        @test err isa ErrorException && startswith(err.msg, "IR verification failed.")
        @test occursin(msg, out)
    end
end
# Test that the error names the method instance, and that the IR is printed
let ir = modified(ir -> push!(ir.cfg.blocks[2].succs, 3)),
    mi = Base.method_instance(identity, (Int,))
    err, out = verifier_output() do
        Compiler.verify_ir(ir, true, false, Compiler.SimpleInferenceLattice.instance, mi)
    end
    @test occursin("Method instance: $mi", err.msg)
    @test occursin("goto #3 if not true", out)
end
# Test that nothing is printed when printing is turned off
let ir = modified(ir -> push!(ir.cfg.blocks[2].succs, 3))
    err, out = verifier_output(() -> Compiler.verify_ir(ir, false))
    @test err isa ErrorException
    @test isempty(out)
end
# Test that a debuginfo edge index with no matching edge is reported
let di = Compiler.DebugInfoStream(Int32[0, 0, 0, 0, 1, 0])
    @test verifier_output(() -> Compiler.verify_linetable(di, 2)) ==
        (nothing, "Malformed debuginfo index into edges\n")
end

# Test unusual IR that the verifier accepts
let cases = Compiler.IRCode[
        # a block ending in a call known to throw may have no successors
        modified(make_ircode(Any[GotoIfNot(true, 3), tuple_call(), ReturnNode(1)])) do ir
            empty!(ir.cfg.blocks[2].succs)
            filter!(!=(2), ir.cfg.blocks[3].preds)
            ir.stmts.type[2] = Union{}
        end,
        # a φ edge from an unreachable block
        make_ircode(Any[GotoNode(3), GotoNode(3), PhiNode(Int32[1, 2], Any[1, 2]),
            ReturnNode(SSAValue(3))]; verify=false),
        # globals that are only tested for definedness, and a GC preserve token
        make_ircode(Any[Expr(:isdefined, GlobalRef(Main, :undefined_global_verify_ir)),
            Expr(:throw_undef_if_not, GlobalRef(Main, :undefined_global_verify_ir), SSAValue(1)),
            Expr(:gc_preserve_begin), Expr(:gc_preserve_end, SSAValue(3)), ReturnNode(1)];
            verify=false),
        # a constant of a top module, as lowering emits it
        make_ircode(Any[tuple_call(GlobalRef(Base, :pi)), ReturnNode(1)]),
        # a use of a node that is still pending insertion
        modified(make_ircode(Any[tuple_call(), tuple_call(), ReturnNode(1)])) do ir
            new = Compiler.insert_node!(ir, 2, Compiler.NewInstruction(tuple_call(), Any))
            ir.stmts.stmt[2] = tuple_call(new)
        end,
    ]
    for ir in cases
        @test Compiler.verify_ir(ir) === nothing
    end
end

# Issue #29107
let code = Any[
        # Block 1
        Compiler.GotoNode(6),
        # Block 2
        # The following phi node gets deleted because it only has one edge, so
        # the call to `something` is made to use the value of `something2()`,
        # even though this value is defined after it. We don't want this to
        # happen even though this block is dead because subsequent optimization
        # passes may look at all code, dead or not.
        Core.PhiNode(Int32[2], Any[Core.SSAValue(4)]),
        Expr(:call, :something, Core.SSAValue(2)),
        Expr(:call, :something2),
        Compiler.GotoNode(2),
        # Block 3
        Compiler.ReturnNode(1000)
    ]
    ir = make_ircode(code)
    ir = Compiler.compact!(ir, true)
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
        Compiler.GotoNode(3),
        # Block 2 (no predecessors)
        Compiler.ReturnNode(3),
        # Block 3
        Core.PhiNode(Int32[1, 2], Any[100, 200]),
        Compiler.ReturnNode(Core.SSAValue(3))
    ]
    ir = make_ircode(code; verify=false)
    ir = Compiler.compact!(ir, true)
    @test Compiler.verify_ir(ir) === nothing
end

# issue #37919
let ci = only(code_lowered(()->@isdefined(_not_def_37919_), ()))
    ir = Compiler.inflate_ir(ci)
    @test Compiler.verify_ir(ir) === nothing
end

let code = Any[
        # block 1
        GotoIfNot(Argument(2), 4)
        # block 2
        Expr(:call, throw, "potential throw")
        ReturnNode() # unreachable
        # block 3
        ReturnNode(Argument(3))
    ]
    ir = make_ircode(code; slottypes=Any[Any,Bool,Int])
    visited = BitSet()
    @test !Compiler.visit_conditional_successors(ir, #=bb=#1) do succ::Int
        push!(visited, succ)
        return false
    end
    @test 2 ∈ visited
    @test 3 ∈ visited
    oc = Core.OpaqueClosure(ir)
    @test oc(false, 1) == 1
    @test_throws "potential throw" oc(true, 1)
end

let code = Any[
        # block 1
        GotoIfNot(Argument(2), 3)
        # block 2
        ReturnNode(Argument(3))
        # block 3
        Expr(:call, throw, "potential throw")
        ReturnNode() # unreachable
    ]
    ir = make_ircode(code; slottypes=Any[Any,Bool,Int])
    visited = BitSet()
    @test !Compiler.visit_conditional_successors(ir, #=bb=#1) do succ::Int
        push!(visited, succ)
        return false
    end
    @test 2 ∈ visited
    @test 3 ∈ visited
    oc = Core.OpaqueClosure(ir)
    @test oc(true, 1) == 1
    @test_throws "potential throw" oc(false, 1)
end

let code = Any[
        # block 1
        GotoIfNot(Argument(2), 5)
        # block 2
        GotoNode(3)
        # block 3
        Expr(:call, throw, "potential throw")
        ReturnNode()
        # block 4
        Expr(:call, Core.Intrinsics.add_int, Argument(3), Argument(4))
        GotoNode(7)
        # block 5
        ReturnNode(SSAValue(5))
    ]
    ir = make_ircode(code; slottypes=Any[Any,Bool,Int,Int])
    visited = BitSet()
    @test !Compiler.visit_conditional_successors(ir, #=bb=#1) do succ::Int
        push!(visited, succ)
        return false
    end
    @test 2 ∈ visited
    @test 3 ∈ visited
    @test 4 ∈ visited
    @test 5 ∈ visited
    oc = Core.OpaqueClosure(ir)
    @test oc(false, 1, 1) == 2
    @test_throws "potential throw" oc(true, 1, 1)
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
    domtree = Compiler.construct_domtree(cfg)
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
    # Add edge back (testing last case for insertion)
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

function cfg_from_succs(succs::Vector{Vector{Int}})
    preds = [Int[] for _ in succs]
    for (from, bb_succs) in pairs(succs), to in bb_succs
        push!(preds[to], from)
    end
    return CFG(BasicBlock[make_bb(preds[bb], succs[bb]) for bb in eachindex(succs)], Int[])
end
function same_as_fresh_domtree(domtree, blocks::Vector{BasicBlock})
    fresh = Compiler.construct_domtree(blocks)
    domtree.idoms_bb == fresh.idoms_bb == Compiler.naive_idoms(blocks) || return false
    for bb in eachindex(blocks)
        Compiler.bb_unreachable(fresh, bb) && continue
        domtree.nodes[bb].level == fresh.nodes[bb].level || return false
        sort(domtree.nodes[bb].children) == sort(fresh.nodes[bb].children) || return false
    end
    return true
end

# Test that toggling any edge of a small CFG twice, updating the dominator tree
# incrementally each time, matches recomputing the tree
let graphs = [
        [[3, 2], [5], [4], [6], Int[], [5, 3]],
        [[2, 3], [4, 5], [4], [5], Int[]],
        [[2], [3, 4], [2], Int[], [4]], # block 5 is unreachable
        [[5, 6], Int[], [3], [2], [4, 2], [2, 5]],
    ]
    failures = Tuple{Vector{Vector{Int}},Int,Int}[]
    for succs in graphs
        cfg = cfg_from_succs(succs)
        domtree = Compiler.construct_domtree(cfg)
        for from in eachindex(succs), to in 2:length(succs), _ in 1:2
            if to in cfg.blocks[from].succs
                Compiler.cfg_delete_edge!(cfg, from, to)
                Compiler.domtree_delete_edge!(domtree, cfg.blocks, from, to)
            else
                Compiler.cfg_insert_edge!(cfg, from, to)
                Compiler.domtree_insert_edge!(domtree, cfg.blocks, from, to)
            end
            same_as_fresh_domtree(domtree, cfg.blocks) || push!(failures, (succs, from, to))
        end
    end
    @test isempty(failures)
end

# Test a CFG large enough for path compression to use its worklist
let n = 40
    succs = [bb < n ? [bb + 1] : [2] for bb in 1:n]
    push!(succs[10], 30)
    push!(succs[25], 5)
    cfg = cfg_from_succs(succs)
    domtree = Compiler.construct_domtree(cfg)
    @test domtree.idoms_bb == Compiler.naive_idoms(cfg.blocks)
    Compiler.cfg_delete_edge!(cfg, 25, 5)
    Compiler.domtree_delete_edge!(domtree, cfg.blocks, 25, 5)
    @test same_as_fresh_domtree(domtree, cfg.blocks)
end

# Test dominator queries on the CFG of the dynamic update test above
let cfg = cfg_from_succs([[3, 2], [5], [4], [6], Int[], [5, 3]])
    domtree = Compiler.construct_domtree(cfg)
    @test Compiler.nearest_common_dominator(domtree, 4, 6) == 4
    @test Compiler.nearest_common_dominator(domtree, 6, 4) == 4
    @test Compiler.nearest_common_dominator(domtree, 2, 6) == 1
    @test Compiler.nearest_common_dominator(domtree, 5, 5) == 5
    @test Compiler.nearest_common_dominator(domtree, 0, 3) == 0
    @test Compiler.nearest_common_dominator(domtree, 3, 0) == 0
    postdomtree = Compiler.construct_postdomtree(cfg)
    @test Compiler.nearest_common_dominator(postdomtree, 3, 6) == 6
    @test Compiler.nearest_common_dominator(postdomtree, 2, 3) == 5
    function dominated_blocks(root)
        bbs = Int[]
        for bb in Compiler.dominated(domtree, root)
            push!(bbs, bb)
        end
        return sort!(bbs)
    end
    @test dominated_blocks(1) == 1:6
    @test dominated_blocks(2) == [2]
    @test dominated_blocks(3) == [3, 4, 6]
end

# Test that renaming the blocks of a dominator tree, and dropping an unreachable
# one, gives the tree of the renamed CFG
let succs = [[3, 2], [5], [4], [6], Int[], [5, 3], [6]],
    rename = [1, 6, 2, 5, 4, 3, -1]
    domtree = Compiler.construct_domtree(cfg_from_succs(succs))
    renamed_succs = Vector{Vector{Int}}(undef, 6)
    for (old, new) in pairs(rename)
        new == -1 || (renamed_succs[new] = rename[succs[old]])
    end
    renamed = cfg_from_succs(renamed_succs)
    Compiler.rename_nodes!(domtree, rename)
    fresh = Compiler.construct_domtree(renamed)
    @test domtree.idoms_bb == [0, 1, 5, 1, 2, 1]
    @test same_as_fresh_domtree(domtree, renamed.blocks)
    @test domtree.dfs_tree.to_pre == fresh.dfs_tree.to_pre
    @test domtree.dfs_tree.to_post == fresh.dfs_tree.to_post
end

# Test that copies of a DFS tree are equal to it but share no storage with it
let dfs = Compiler.construct_domtree(cfg_from_succs([[3, 2], [5], [4], [6], Int[], [5, 3]])).dfs_tree
    fields(d) = (d.to_pre, d.from_pre, d.to_post, d.from_post, d.to_parent_pre)
    for dfs′ in (Compiler.copy(dfs), Compiler.copy!(Compiler.DFSTree(0), dfs))
        @test fields(dfs′) == fields(dfs)
        @test !any(map(===, fields(dfs′), fields(dfs)))
    end
end

# Issue #41975 - SSA conversion drops type check
f_if_typecheck() = (if nothing; end; unsafe_load(Ptr{Int}(0)))
@test_throws TypeError f_if_typecheck()

let # https://github.com/JuliaLang/julia/issues/42258
    code = """
        using Base: Compiler

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

        code_typed(Compiler.setindex!, (Compiler.UseRef,Compiler.NewSSAValue); optimize=true)
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
    @test first(only(Base.code_ircode(+, (Float64, Float64); optimize_until = "CC: SROA"))) isa
          Compiler.IRCode

    function demo(f)
        f()
        f()
        f()
    end
    @test first(only(Base.code_ircode(demo))) isa Compiler.IRCode
    @test first(only(Base.code_ircode(demo; optimize_until = 3))) isa Compiler.IRCode
    @test first(only(Base.code_ircode(demo; optimize_until = "CC: SROA"))) isa Compiler.IRCode
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
    ir_withslots = first(only(Base.code_ircode(f_with_slots, (Int,Int); optimize_until="CC: CONVERT")))
    @test length(ir_withslots.argtypes) == 5
    # #self#, a, b
    opt = code_typed1(f_with_slots, (Int,Int); optimize=true)
    @test length(opt.slotnames) == length(opt.slotflags) == length(opt.slottypes) == 3
    ir_ssa = first(only(Base.code_ircode(f_with_slots, (Int,Int); optimize_until="CC: SLOT2REG")))
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
        elseif isa(stmt, SSAValue) || isa(stmt, NewSSAValue) || isa(stmt, Argument)
            @test stmt === v
        elseif isa(stmt, PiNode)
            @test stmt.val === v && stmt.typ === typeof(stmt)
        elseif isa(stmt, PhiNode) || isa(stmt, PhiCNode)
            @test stmt.values[op] === v
        end
    end

    function _test_userefs(@nospecialize stmt)
        ex = Expr(:call, :+, Core.SSAValue(3), 1)
        urs = Compiler.userefs(stmt)::Compiler.UseRefIterator
        it = Compiler.iterate(urs)
        while it !== nothing
            ur = getfield(it, 1)::Compiler.UseRef
            op = getfield(it, 2)::Int
            v1 = Compiler.getindex(ur)
            # set to dummy expression and then back to itself to test `_useref_setindex!`
            v2 = Compiler.setindex!(ur, ex)
            test_useref(v2, ex, op)
            Compiler.setindex!(ur, v1)
            @test Compiler.getindex(ur) === v1
            it = Compiler.iterate(urs, op)
        end
    end

    function test_userefs(body)
        for stmt in body
            _test_userefs(stmt)
        end
    end

    # this isn't valid code, we just care about looking at a variety of IR nodes
    body = Any[
        EnterNode(11),
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
        Argument(1),
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
    domtree = Compiler.construct_domtree(ir)
    @test Compiler.dominates(domtree, 1, 2)
    @test Compiler.dominates(domtree, 1, 3)
    @test Compiler.dominates(domtree, 1, 4)
    for i = 2:4
        for j = 1:4
            i == j && continue
            @test !Compiler.dominates(domtree, i, j)
        end
    end
    # post domination analysis
    post_domtree = Compiler.construct_postdomtree(ir)
    @test Compiler.postdominates(post_domtree, 4, 1)
    @test Compiler.postdominates(post_domtree, 4, 2)
    @test Compiler.postdominates(post_domtree, 4, 3)
    for i = 1:3
        for j = 1:4
            i == j && continue
            @test !Compiler.postdominates(post_domtree, i, j)
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
    inst = Compiler.NewInstruction(Expr(:call, add_stmt[:stmt].args[1], add_stmt[:stmt].args[2], 999), Int)
    node = Compiler.insert_node!(ir, 1, inst)
    Compiler.setindex!(add_stmt, node, :stmt)

    # perform compaction (not by calling compact! because with DCE the bug doesn't trigger)
    compact = Compiler.IncrementalCompact(ir)
    state = Compiler.iterate(compact)
    while state !== nothing
        state = Compiler.iterate(compact, state[2])
    end
    ir = Compiler.complete(compact)

    # test that the inserted node was compacted
    @test Compiler.length(ir.new_nodes) == 0

    # test that we performed copy propagation, but that the undef node was trimmed
    @test length(ir.stmts) == instructions

    @test show(devnull, ir) === nothing
end

# insert_node! operations
# =======================

import Core: SSAValue
import .Compiler: NewInstruction, insert_node!

# insert_node! for pending node
let ir = Base.code_ircode((Int,Int); optimize_until="CC: INLINING") do a, b
        a^b
    end |> only |> first
    ir = Compiler.compact!(ir)
    nstmts = length(ir.stmts)
    invoke_idx = findfirst(@nospecialize(stmt)->Meta.isexpr(stmt, :invoke), ir.stmts.stmt)
    @test invoke !== nothing

    invoke_ssa = SSAValue(invoke_idx)
    newssa = insert_node!(ir, invoke_ssa, NewInstruction(Expr(:call, println, invoke_ssa), Nothing), #=attach_after=#true)
    newssa = insert_node!(ir, newssa, NewInstruction(Expr(:call, println, newssa), Nothing), #=attach_after=#true)

    ir = Compiler.compact!(ir)
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
        #= %2: =# Compiler.GotoIfNot(SSAValue(1), 4),
        # block 2
        #= %3: =# Expr(:call, println, Argument(1)),
        # block 3
        #= %4: =# Core.PhiNode(),
        #= %5: =# Compiler.ReturnNode(),
    ]
    ir = make_ircode(code)

    # Insert another call at end of "block 2"
    compact = Compiler.IncrementalCompact(ir)
    new_inst = NewInstruction(Expr(:call, println, Argument(1)), Nothing)
    insert_node!(compact, SSAValue(3), new_inst, #= attach_after =# true)

    # Complete iteration
    x = Compiler.iterate(compact)
    while x !== nothing
        x = Compiler.iterate(compact, x[2])
    end
    ir = Compiler.complete(compact)

    @test Compiler.verify_ir(ir) === nothing
end

# compact constant PiNode
let code = Any[
        PiNode(0.0, Const(0.0))
        ReturnNode(SSAValue(1))
    ]
    ir = make_ircode(code)
    ir = Compiler.compact!(ir)
    @test fully_eliminated(ir)
end

# insert_node! with new instruction with flag computed
let ir = Base.code_ircode((Int,Int); optimize_until="CC: INLINING") do a, b
        a^b
    end |> only |> first
    ir = Compiler.compact!(ir)
    invoke_idx = findfirst(@nospecialize(stmt)->Meta.isexpr(stmt, :invoke), ir.stmts.stmt)
    @test invoke_idx !== nothing
    invoke_expr = ir.stmts.stmt[invoke_idx]
    invoke_ssa = SSAValue(invoke_idx)

    # effect-ful node
    let compact = Compiler.IncrementalCompact(Compiler.copy(ir))
        insert_node!(compact, invoke_ssa, NewInstruction(Expr(:call, println, invoke_ssa), Nothing), #=attach_after=#true)
        state = Compiler.iterate(compact)
        while state !== nothing
            state = Compiler.iterate(compact, state[2])
        end
        ir = Compiler.finish(compact)
        new_invoke_idx = findfirst(@nospecialize(stmt)->stmt==invoke_expr, ir.stmts.stmt)
        @test new_invoke_idx !== nothing
        new_call_idx = findfirst(ir.stmts.stmt) do @nospecialize(stmt)
            iscall((ir,println), stmt) && stmt.args[2] === SSAValue(new_invoke_idx)
        end
        @test new_call_idx !== nothing
        @test new_call_idx == new_invoke_idx+1
    end

    # effect-free node
    let compact = Compiler.IncrementalCompact(Compiler.copy(ir))
        insert_node!(compact, invoke_ssa, NewInstruction(Expr(:call, GlobalRef(Base, :add_int), invoke_ssa, invoke_ssa), Int), #=attach_after=#true)
        state = Compiler.iterate(compact)
        while state !== nothing
            state = Compiler.iterate(compact, state[2])
        end
        ir = Compiler.finish(compact)

        ir = Compiler.finish(compact)
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
        ircode = first(only(Base.code_ircode(f_with_maybe_nonbool_cond, (Int, Bool); optimize_until="CC: CONVERT")))
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

# Test that things don't break if one branch of the frontend PhiNode becomes unreachable
const global_error_switch_const1::Bool = false
function gen_unreachable_phinode_edge1(world::UInt, source, args...)
    ci = make_codeinfo(Any[
        # block 1
        GlobalRef(@__MODULE__, :global_error_switch_const1),
        GotoIfNot(SSAValue(1), 4),
        # block 2
        Expr(:call, identity, Argument(3)),
        # block 3
        PhiNode(Int32[2, 3], Any[Argument(2), SSAValue(3)]),
        ReturnNode(SSAValue(4))
    ]; slottypes=Any[Any,Int,Int])
    ci.slotnames = Symbol[:var"#self#", :x, :y]
    ci.nargs = 3
    ci.isva = false
    return ci
end
@eval function f_unreachable_phinode_edge1(x, y)
    $(Expr(:meta, :generated, gen_unreachable_phinode_edge1))
    $(Expr(:meta, :generated_only))
    #= no body =#
end
@test f_unreachable_phinode_edge1(1, 2) == 1

const global_error_switch_const2::Bool = true
function gen_unreachable_phinode_edge2(world::UInt, source, args...)
    ci = make_codeinfo(Any[
        # block 1
        GlobalRef(@__MODULE__, :global_error_switch_const2),
        GotoIfNot(SSAValue(1), 4),
        # block 2
        Expr(:call, identity, Argument(3)),
        # block 3
        PhiNode(Int32[2, 3], Any[Argument(2), SSAValue(3)]),
        ReturnNode(SSAValue(4))
    ]; slottypes=Any[Any,Int,Int])
    ci.slotnames = Symbol[:var"#self#", :x, :y]
    ci.nargs = 3
    ci.isva = false
    return ci
end
@eval function f_unreachable_phinode_edge2(x, y)
    $(Expr(:meta, :generated, gen_unreachable_phinode_edge2))
    $(Expr(:meta, :generated_only))
    #= no body =#
end
@test f_unreachable_phinode_edge2(1, 2) == 2

# Preserve an undefined incoming value when slot2ssa creates a PhiNode (#55388).
function undef_phinode55388(nextstate, loopcond, valuecond, returncond)
    nextstate && @goto state3
    while loopcond
        ct = valuecond ? [] : nothing
        if returncond
            return
            @label state3
        end
        Base.donotdelete(ct)
    end
    nothing
end
let src = code_typed1(undef_phinode55388, (Bool, Bool, Bool, Bool))
    @test any(src.code) do stmt
        isa(stmt, PhiNode) || return false
        return any(i -> !isassigned(stmt.values, i), eachindex(stmt.edges))
    end
end
@test_throws UndefVarError undef_phinode55388(true, true, true, false)
@test undef_phinode55388(false, false, false, false) === nothing

# ... and the same for a slot whose definitions live inside an exception region,
# which reaches the `PhiCNode`/`UpsilonNode` path instead (#55388).
@noinline maythrow55388(x) = x === nothing ? throw(ArgumentError("nothing")) : x
function undef_phicnode55388(nextstate, loopcond, valuecond)
    nextstate && @goto state3
    while loopcond
        local ct
        try
            ct = maythrow55388(valuecond ? [] : nothing)
        catch
            ct = nothing
        end
        @label state3
        Base.donotdelete(ct)
    end
    nothing
end
let src = code_typed1(undef_phicnode55388, (Bool, Bool, Bool))
    @test any(src.code) do stmt
        isa(stmt, PhiNode) || return false
        return any(i -> !isassigned(stmt.values, i), eachindex(stmt.edges))
    end
end
@test_throws UndefVarError undef_phicnode55388(true, true, true)
@test undef_phicnode55388(false, false, false) === nothing

global global_error_switch::Bool = true
function gen_must_throw_phinode_edge(world::UInt, source, _)
    ci = make_codeinfo(Any[
        # block 1
        GlobalRef(@__MODULE__, :global_error_switch),
        GotoIfNot(SSAValue(1), 4),
        # block 2
        Expr(:call, error, "This error is expected"),
        # block 3
        PhiNode(Int32[2, 3], Any[1, 2]),
        ReturnNode(SSAValue(4))
    ]; slottypes=Any[Any])
    ci.slotnames = Symbol[:var"#self#"]
    ci.nargs = 1
    ci.isva = false
    return ci
end
@eval function f_must_throw_phinode_edge()
    $(Expr(:meta, :generated, gen_must_throw_phinode_edge))
    $(Expr(:meta, :generated_only))
    #= no body =#
end
let ir = first(only(Base.code_ircode(f_must_throw_phinode_edge)))
    @test !any(@nospecialize(x)->isa(x,PhiNode), ir.stmts.stmt)
end
@test_throws ErrorException f_must_throw_phinode_edge()
global global_error_switch = false
@test f_must_throw_phinode_edge() == 1

# Test that IRShow debuginfo printing works with IRCode owned by the active Compiler module.
function irshow_debuginfo_smoke(x)
    y = x + 1
    return y
end
let ir = first(only(Base.code_ircode(irshow_debuginfo_smoke, (Int,))))
    output = sprint(Compiler.IRShow.show_ir, ir,
                    Compiler.IRShow.default_config(ir; debuginfo=:source_inline))
    @test occursin("return", output)
end

function roundtrip_di(codelocs, firstline, nstmts)
    str = ccall(:jl_compress_codelocs,
                Any, (Int32, Any, Int), firstline, codelocs, nstmts)::String;
    di = Core.DebugInfo(:foo, nothing, Core.svec(), str)
    return ccall(:jl_uncompress_codelocs, Any, (Any, Int), di, nstmts)
end

# Test roundtrip of debuginfo compression
let cl = Int32[32, 1, 1, 1000, 240, 230, 0, 0, 0]
    @test roundtrip_di(cl, -1, 3) == cl
    @test roundtrip_di(cl, 0, 3) == cl
    @test roundtrip_di(cl, 1, 3) == cl
    @test roundtrip_di(cl, 32, 3) == cl
    @test roundtrip_di(cl, 33, 3) == cl
end
let cl = Int32[0,0,0,255,0,0,256,0,0,257,0,0]
    @test roundtrip_di(cl, -1, 4) == cl
    @test roundtrip_di(cl, 0, 4) == cl
    @test roundtrip_di(cl, 1, 4) == cl
    @test roundtrip_di(cl, 32, 4) == cl
    @test roundtrip_di(cl, 33, 4) == cl
end

# Test line comparisons with byte-precise debuginfo.
let sbt = String(UInt8[
        1, 0, 0, 0, # byte offset
        1, 0, 0, 0, # line offset
        2, 0, 0, 0, # number of locations
        1, 1,       # byte and span encoding lengths
        0, 1, 2, 1, # byte spans
        0, 2,       # line starts
    ])
    codelocs = Int32[1, 0, 0, 2, 0, 0]
    str = ccall(:jl_compress_codelocs,
                Any, (Int32, Any, Int), Int32(-1), codelocs, 2)::String
    di = Core.DebugInfo(:foo, sbt, Core.svec(), str)
    @test !Compiler.changed_lineinfo(di, 1, 1)
    @test Compiler.changed_lineinfo(di, 2, 1)
    @test !Compiler._should_instrument(di)
end

@test_throws ErrorException Base.code_ircode(+, (Float64, Float64); optimize_until = "nonexisting pass name")
@test_throws ErrorException Base.code_ircode(+, (Float64, Float64); optimize_until = typemax(Int))

#57153 check that the CFG has a #0 block predecessor and that we don't fail to compile code that observes that
function _worker_task57153()
    while true
        r = let
        try
            if @noinline rand(Bool)
                return nothing
            end
            q, m
        finally
            missing
        end
        end
        r[1]::Bool
    end
end
let ir = Base.code_ircode(_worker_task57153, (), optimize_until="CC: COMPACT_2")[1].first
    @test findfirst(x->x==0, ir.cfg.blocks[1].preds) !== nothing
end

# Tests that CFG edge cleanup during compaction doesn't corrupt iteration codegen.
Trips_60660 = let
    Ts = (Float64, Float32)
    [(Ta, Tb, Tc) for Ta in Ts for Tb in Ts for Tc in Ts]
end
@test Trips_60660 == [
    (Float64, Float64, Float64),
    (Float64, Float64, Float32),
    (Float64, Float32, Float64),
    (Float64, Float32, Float32),
    (Float32, Float64, Float64),
    (Float32, Float64, Float32),
    (Float32, Float32, Float64),
    (Float32, Float32, Float32),
]

# Tests that phi-edge cleanup during compaction terminates when the phi block
# begins with a `nothing` statement rather than a PhiNode (#62818).
function f62818(c)
    r = Ref(false)
    while true
        if c !== nothing
            r[] = true
        end
        if r[]
            continue
        else
            break
        end
    end
end
@test only(Base.return_types(f62818, Tuple{Nothing})) === Nothing
@test f62818(nothing) === nothing
