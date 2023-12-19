# This file is a part of Julia. License is MIT: https://julialang.org/license

Core.PhiNode() = Core.PhiNode(Int32[], Any[])

isterminator(@nospecialize(stmt)) = isa(stmt, GotoNode) || isa(stmt, GotoIfNot) || isa(stmt, ReturnNode) || isa(stmt, EnterNode)

struct CFG
    blocks::Vector{BasicBlock}
    index::Vector{Int} # map from instruction => basic-block number
                       # TODO: make this O(1) instead of O(log(n_blocks))?
end

copy(c::CFG) = CFG(BasicBlock[copy(b) for b in c.blocks], copy(c.index))
==(a::CFG, b::CFG) = a.blocks == b.blocks && a.index == b.index

function cfg_insert_edge!(cfg::CFG, from::Int, to::Int)
    # Assumes that this edge does not already exist
    push!(cfg.blocks[to].preds, from)
    push!(cfg.blocks[from].succs, to)
    nothing
end

function cfg_delete_edge!(cfg::CFG, from::Int, to::Int)
    preds = cfg.blocks[to].preds
    succs = cfg.blocks[from].succs
    # Assumes that blocks appear at most once in preds and succs
    deleteat!(preds, findfirst(x::Int->x==from, preds)::Int)
    deleteat!(succs, findfirst(x::Int->x==to, succs)::Int)
    nothing
end

function cfg_reindex!(cfg::CFG)
    resize!(cfg.index, length(cfg.blocks) - 1)
    for ibb in 2:length(cfg.blocks)
        cfg.index[ibb-1] = first(cfg.blocks[ibb].stmts)
    end
    return cfg
end

function bb_ordering()
    lt = (<=)
    by = x::BasicBlock -> first(x.stmts)
    ord(lt, by, nothing, Forward)
end

function block_for_inst(index::Vector{Int}, inst::Int)
    return searchsortedfirst(index, inst, lt=(<=))
end

function block_for_inst(index::Vector{BasicBlock}, inst::Int)
    return searchsortedfirst(index, BasicBlock(StmtRange(inst, inst)), bb_ordering())-1
end

block_for_inst(cfg::CFG, inst::Int) = block_for_inst(cfg.index, inst)

@inline function basic_blocks_starts(stmts::Vector{Any})
    jump_dests = BitSet()
    push!(jump_dests, 1) # function entry point
    # First go through and compute jump destinations
    for idx in 1:length(stmts)
        stmt = stmts[idx]
        # Terminators
        if isa(stmt, GotoIfNot)
            push!(jump_dests, idx+1)
            push!(jump_dests, stmt.dest)
        elseif isa(stmt, ReturnNode)
            idx < length(stmts) && push!(jump_dests, idx+1)
        elseif isa(stmt, GotoNode)
            # This is a fake dest to force the next stmt to start a bb
            idx < length(stmts) && push!(jump_dests, idx+1)
            push!(jump_dests, stmt.label)
        elseif isa(stmt, EnterNode)
            # :enter starts/ends a BB
            push!(jump_dests, idx)
            push!(jump_dests, idx+1)
            # The catch block is a jump dest
            if stmt.catch_dest != 0
                push!(jump_dests, stmt.catch_dest)
            end
        elseif isa(stmt, Expr)
            if stmt.head === :leave
                # :leave terminates a BB
                push!(jump_dests, idx+1)
            end
        end
        if isa(stmt, PhiNode)
            for edge in stmt.edges
                if edge == idx - 1
                    push!(jump_dests, idx)
                end
            end
        end
    end
    # and add add one more basic block start after the last statement
    for i = length(stmts):-1:1
        if stmts[i] !== nothing
            push!(jump_dests, i+1)
            break
        end
    end
    return jump_dests
end

function compute_basic_blocks(stmts::Vector{Any})
    # Compute ranges
    bb_starts = basic_blocks_starts(stmts) # ::BitSet and already sorted
    pop!(bb_starts, 1)
    basic_block_index = Int[bb for bb in bb_starts]
    blocks = Vector{BasicBlock}(undef, length(basic_block_index))
    let first = 1
        for (i, last) in enumerate(basic_block_index)
            blocks[i] = BasicBlock(StmtRange(first, last - 1))
            first = last
        end
    end
    # Compute successors/predecessors
    for (num, b) in enumerate(blocks)
        terminator = stmts[last(b.stmts)]
        if isa(terminator, ReturnNode)
            # return never has any successors
            continue
        end
        if isa(terminator, GotoNode)
            block′ = block_for_inst(basic_block_index, terminator.label)
            push!(blocks[block′].preds, num)
            push!(b.succs, block′)
            continue
        end
        # Conditional Branch
        if isa(terminator, GotoIfNot)
            block′ = block_for_inst(basic_block_index, terminator.dest)
            if block′ == num + 1
                # This GotoIfNot acts like a noop - treat it as such.
                # We will drop it during SSA renaming
            else
                push!(blocks[block′].preds, num)
                push!(b.succs, block′)
            end
        elseif isa(terminator, EnterNode)
            # :enter gets a virtual edge to the exception handler and
            # the exception handler gets a virtual edge from outside
            # the function.
            if terminator.catch_dest != 0
                block′ = block_for_inst(basic_block_index, terminator.catch_dest)
                push!(blocks[block′].preds, num)
                push!(blocks[block′].preds, 0)
                push!(b.succs, block′)
            end
        end
        # statement fall-through
        if num + 1 <= length(blocks)
            push!(blocks[num + 1].preds, num)
            push!(b.succs, num + 1)
        end
    end
    return CFG(blocks, basic_block_index)
end

# this function assumes insert position exists
function is_valid_phiblock_stmt(@nospecialize(stmt))
    isa(stmt, PhiNode) && return true
    isa(stmt, Union{UpsilonNode, PhiCNode, SSAValue}) && return false
    isa(stmt, Expr) && return is_value_pos_expr_head(stmt.head)
    return true
end

function first_insert_for_bb(code::Vector{Any}, cfg::CFG, block::Int)
    stmts = cfg.blocks[block].stmts
    lastnonphiidx = first(stmts)
    for idx in stmts
        stmt = code[idx]
        if !isa(stmt, PhiNode)
            if !is_valid_phiblock_stmt(stmt)
                return lastnonphiidx
            end
        else
            lastnonphiidx = idx + 1
        end
    end
    if lastnonphiidx > last(stmts)
        error("any insert position isn't found")
    end
    return lastnonphiidx
end

# SSA values that need renaming
struct OldSSAValue
    id::Int
end

## TODO: This description currently omits the use of NewSSAValue during slot2ssa,
## which doesn't use IncrementalCompact, but does something similar and also uses
## NewSSAValue to refer to new_nodes. Ideally that use of NewSSAValue would go away
## during a refactor.
"""
    struct NewSSAValue

`NewSSAValue`s occur in the context of IncrementalCompact. Their meaning depends
on where they appear:

1. In already-compacted nodes,
    i. a `NewSSAValue` with positive `id` has the same meaning as a regular SSAValue.
    ii. a `NewSSAValue` with negative `id` refers to post-compaction `new_node` node.

2. In non-compacted nodes,
    i. a `NewSSAValue` with positive `id` refers to the index of an already-compacted instructions.
    ii. a `NewSSAValue` with negative `id` has the same meaning as in compacted nodes.
"""
struct NewSSAValue
    id::Int
end

const AnySSAValue = Union{SSAValue, OldSSAValue, NewSSAValue}


# SSA-indexed nodes
struct InstructionStream
    stmt::Vector{Any}
    type::Vector{Any}
    info::Vector{CallInfo}
    line::Vector{Int32}
    flag::Vector{UInt32}
end
function InstructionStream(len::Int)
    stmts = Vector{Any}(undef, len)
    types = Vector{Any}(undef, len)
    info = Vector{CallInfo}(undef, len)
    fill!(info, NoCallInfo())
    lines = fill(Int32(0), len)
    flags = fill(IR_FLAG_NULL, len)
    return InstructionStream(stmts, types, info, lines, flags)
end
InstructionStream() = InstructionStream(0)
length(is::InstructionStream) = length(is.stmt)
isempty(is::InstructionStream) = isempty(is.stmt)
function add_new_idx!(is::InstructionStream)
    ninst = length(is) + 1
    resize!(is, ninst)
    return ninst
end
function copy(is::InstructionStream)
    return InstructionStream(
        copy_exprargs(is.stmt),
        copy(is.type),
        copy(is.info),
        copy(is.line),
        copy(is.flag))
end
function resize!(stmts::InstructionStream, len)
    old_length = length(stmts)
    resize!(stmts.stmt, len)
    resize!(stmts.type, len)
    resize!(stmts.info, len)
    resize!(stmts.line, len)
    resize!(stmts.flag, len)
    for i in (old_length + 1):len
        stmts.line[i] = 0
        stmts.flag[i] = IR_FLAG_NULL
        stmts.info[i] = NoCallInfo()
    end
    return stmts
end
iterate(is::InstructionStream, st::Int=1) = (st <= length(is)) ? (is[st], st + 1) : nothing

struct Instruction
    data::InstructionStream
    idx::Int
end
Instruction(is::InstructionStream) = Instruction(is, add_new_idx!(is))

@inline function getindex(node::Instruction, fld::Symbol)
    (fld === :inst) && (fld = :stmt) # deprecated
    isdefined(node, fld) && return getfield(node, fld)
    return getfield(getfield(node, :data), fld)[getfield(node, :idx)]
end
@inline function setindex!(node::Instruction, @nospecialize(val), fld::Symbol)
    (fld === :inst) && (fld = :stmt) # deprecated
    getfield(getfield(node, :data), fld)[getfield(node, :idx)] = val
    return node
end

@inline getindex(is::InstructionStream, idx::Int) = Instruction(is, idx)
function setindex!(is::InstructionStream, newval::Instruction, idx::Int)
    is.stmt[idx] = newval[:stmt]
    is.type[idx] = newval[:type]
    is.info[idx] = newval[:info]
    is.line[idx] = newval[:line]
    is.flag[idx] = newval[:flag]
    return is
end
function setindex!(is::InstructionStream, newval::Union{AnySSAValue, Nothing}, idx::Int)
    is.stmt[idx] = newval
    return is
end
function setindex!(node::Instruction, newval::Instruction)
    node.data[node.idx] = newval
    return node
end

has_flag(inst::Instruction, flag::UInt32) = has_flag(inst[:flag], flag)
add_flag!(inst::Instruction, flag::UInt32) = inst[:flag] |= flag
sub_flag!(inst::Instruction, flag::UInt32) = inst[:flag] &= ~flag

struct NewNodeInfo
    # Insertion position (interpretation depends on which array this is in)
    pos::Int
    # Place the new instruction after this instruction (but in the same BB if this is an implicit terminator)
    attach_after::Bool
end
struct NewNodeStream
    stmts::InstructionStream
    info::Vector{NewNodeInfo}
end
NewNodeStream(len::Int=0) = NewNodeStream(InstructionStream(len), fill(NewNodeInfo(0, false), len))
length(new::NewNodeStream) = length(new.stmts)
isempty(new::NewNodeStream) = isempty(new.stmts)
function add_inst!(new::NewNodeStream, pos::Int, attach_after::Bool)
    push!(new.info, NewNodeInfo(pos, attach_after))
    return Instruction(new.stmts)
end
copy(nns::NewNodeStream) = NewNodeStream(copy(nns.stmts), copy(nns.info))

struct NewInstruction
    stmt::Any
    type::Any
    info::CallInfo
    line::Union{Int32,Nothing} # if nothing, copy the line from previous statement in the insertion location
    flag::Union{UInt32,Nothing} # if nothing, IR flags will be recomputed on insertion
    function NewInstruction(@nospecialize(stmt), @nospecialize(type), @nospecialize(info::CallInfo),
                            line::Union{Int32,Nothing}, flag::Union{UInt32,Nothing})
        return new(stmt, type, info, line, flag)
    end
end
function NewInstruction(@nospecialize(stmt), @nospecialize(type), line::Union{Int32,Nothing}=nothing)
    return NewInstruction(stmt, type, NoCallInfo(), line, nothing)
end
@nospecialize
function NewInstruction(newinst::NewInstruction;
    stmt::Any=newinst.stmt,
    type::Any=newinst.type,
    info::CallInfo=newinst.info,
    line::Union{Int32,Nothing}=newinst.line,
    flag::Union{UInt32,Nothing}=newinst.flag)
    return NewInstruction(stmt, type, info, line, flag)
end
function NewInstruction(inst::Instruction;
    stmt::Any=inst[:stmt],
    type::Any=inst[:type],
    info::CallInfo=inst[:info],
    line::Union{Int32,Nothing}=inst[:line],
    flag::Union{UInt32,Nothing}=inst[:flag])
    return NewInstruction(stmt, type, info, line, flag)
end
@specialize
effect_free_and_nothrow(newinst::NewInstruction) = add_flag(newinst, IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW)
function add_flag(newinst::NewInstruction, newflag::UInt32)
    flag = newinst.flag
    if flag === nothing
        flag = newflag
    else
        flag |= newflag
    end
    return NewInstruction(newinst; flag)
end
function sub_flag(newinst::NewInstruction, newflag::UInt32)
    flag = newinst.flag
    if flag === nothing
        flag = IR_FLAG_NULL
    else
        flag &= ~newflag
    end
    return NewInstruction(newinst; flag)
end

struct IRCode
    stmts::InstructionStream
    argtypes::Vector{Any}
    sptypes::Vector{VarState}
    linetable::Vector{LineInfoNode}
    cfg::CFG
    new_nodes::NewNodeStream
    meta::Vector{Expr}

    function IRCode(stmts::InstructionStream, cfg::CFG, linetable::Vector{LineInfoNode}, argtypes::Vector{Any}, meta::Vector{Expr}, sptypes::Vector{VarState})
        return new(stmts, argtypes, sptypes, linetable, cfg, NewNodeStream(), meta)
    end
    function IRCode(ir::IRCode, stmts::InstructionStream, cfg::CFG, new_nodes::NewNodeStream)
        return new(stmts, ir.argtypes, ir.sptypes, ir.linetable, cfg, new_nodes, ir.meta)
    end
    global copy(ir::IRCode) = new(copy(ir.stmts), copy(ir.argtypes), copy(ir.sptypes),
        copy(ir.linetable), copy(ir.cfg), copy(ir.new_nodes), copy(ir.meta))
end

"""
    IRCode()

Create an empty IRCode object with a single `return nothing` statement. This method is mostly intended
for debugging and unit testing of IRCode APIs. The compiler itself should generally obtain an IRCode
from the frontend or one of the caches.
"""
function IRCode()
    ir = IRCode(InstructionStream(1), CFG([BasicBlock(1:1, Int[], Int[])], Int[1]), LineInfoNode[], Any[], Expr[], VarState[])
    ir[SSAValue(1)][:stmt] = ReturnNode(nothing)
    ir[SSAValue(1)][:type] = Nothing
    ir[SSAValue(1)][:flag] = 0x00
    ir[SSAValue(1)][:line] = Int32(0)
    return ir
end

function block_for_inst(ir::IRCode, inst::Int)
    if inst > length(ir.stmts)
        inst = ir.new_nodes.info[inst - length(ir.stmts)].pos
    end
    block_for_inst(ir.cfg, inst)
end

function getindex(ir::IRCode, s::SSAValue)
    nstmts = length(ir.stmts)
    if s.id <= nstmts
        return ir.stmts[s.id]
    else
        return ir.new_nodes.stmts[s.id - nstmts]
    end
end

function setindex!(x::IRCode, repl::Union{Instruction, Nothing, AnySSAValue}, s::SSAValue)
    if s.id <= length(x.stmts)
        x.stmts[s.id] = repl
    else
        x.new_nodes.stmts[s.id - length(x.stmts)] = repl
    end
    return x
end

mutable struct UseRefIterator
    stmt::Any
    relevant::Bool
    UseRefIterator(@nospecialize(a), relevant::Bool) = new(a, relevant)
end
getindex(it::UseRefIterator) = it.stmt

struct UseRef
    urs::UseRefIterator
    op::Int
    UseRef(urs::UseRefIterator) = new(urs, 0)
    UseRef(urs::UseRefIterator, op::Int) = new(urs, op)
end

struct OOBToken end; const OOB_TOKEN = OOBToken()
struct UndefToken end; const UNDEF_TOKEN = UndefToken()

@noinline function _useref_getindex(@nospecialize(stmt), op::Int)
    if isa(stmt, Expr) && stmt.head === :(=)
        rhs = stmt.args[2]
        if isa(rhs, Expr)
            if is_relevant_expr(rhs)
                op > length(rhs.args) && return OOB_TOKEN
                return rhs.args[op]
            end
        end
        op == 1 || return OOB_TOKEN
        return rhs
    elseif isa(stmt, Expr) # @assert is_relevant_expr(stmt)
        op > length(stmt.args) && return OOB_TOKEN
        return stmt.args[op]
    elseif isa(stmt, GotoIfNot)
        op == 1 || return OOB_TOKEN
        return stmt.cond
    elseif isa(stmt, ReturnNode)
        isdefined(stmt, :val) || return OOB_TOKEN
        op == 1 || return OOB_TOKEN
        return stmt.val
    elseif isa(stmt, EnterNode)
        isdefined(stmt, :scope) || return OOB_TOKEN
        op == 1 || return OOB_TOKEN
        return stmt.scope
    elseif isa(stmt, PiNode)
        isdefined(stmt, :val) || return OOB_TOKEN
        op == 1 || return OOB_TOKEN
        return stmt.val
    elseif isa(stmt, Union{AnySSAValue, GlobalRef})
        op == 1 || return OOB_TOKEN
        return stmt
    elseif isa(stmt, UpsilonNode)
        isdefined(stmt, :val) || return OOB_TOKEN
        op == 1 || return OOB_TOKEN
        return stmt.val
    elseif isa(stmt, PhiNode)
        op > length(stmt.values) && return OOB_TOKEN
        isassigned(stmt.values, op) || return UNDEF_TOKEN
        return stmt.values[op]
    elseif isa(stmt, PhiCNode)
        op > length(stmt.values) && return OOB_TOKEN
        isassigned(stmt.values, op) || return UNDEF_TOKEN
        return stmt.values[op]
    else
        return OOB_TOKEN
    end
end
@inline getindex(x::UseRef) = _useref_getindex(x.urs.stmt, x.op)

function is_relevant_expr(e::Expr)
    return e.head in (:call, :invoke, :invoke_modify,
                      :new, :splatnew, :(=), :(&),
                      :gc_preserve_begin, :gc_preserve_end,
                      :foreigncall, :isdefined, :copyast,
                      :throw_undef_if_not,
                      :cfunction, :method, :pop_exception,
                      :leave,
                      :new_opaque_closure)
end

@noinline function _useref_setindex!(@nospecialize(stmt), op::Int, @nospecialize(v))
    if isa(stmt, Expr) && stmt.head === :(=)
        rhs = stmt.args[2]
        if isa(rhs, Expr)
            if is_relevant_expr(rhs)
                op > length(rhs.args) && throw(BoundsError())
                rhs.args[op] = v
                return stmt
            end
        end
        op == 1 || throw(BoundsError())
        stmt.args[2] = v
    elseif isa(stmt, Expr) # @assert is_relevant_expr(stmt)
        op > length(stmt.args) && throw(BoundsError())
        stmt.args[op] = v
    elseif isa(stmt, GotoIfNot)
        op == 1 || throw(BoundsError())
        stmt = GotoIfNot(v, stmt.dest)
    elseif isa(stmt, ReturnNode)
        op == 1 || throw(BoundsError())
        stmt = ReturnNode(v)
    elseif isa(stmt, EnterNode)
        op == 1 || throw(BoundsError())
        stmt = EnterNode(stmt.catch_dest, v)
    elseif isa(stmt, Union{AnySSAValue, GlobalRef})
        op == 1 || throw(BoundsError())
        stmt = v
    elseif isa(stmt, UpsilonNode)
        op == 1 || throw(BoundsError())
        stmt = UpsilonNode(v)
    elseif isa(stmt, PiNode)
        op == 1 || throw(BoundsError())
        stmt = PiNode(v, stmt.typ)
    elseif isa(stmt, PhiNode)
        op > length(stmt.values) && throw(BoundsError())
        isassigned(stmt.values, op) || throw(BoundsError())
        stmt.values[op] = v
    elseif isa(stmt, PhiCNode)
        op > length(stmt.values) && throw(BoundsError())
        isassigned(stmt.values, op) || throw(BoundsError())
        stmt.values[op] = v
    else
        throw(BoundsError())
    end
    return stmt
end

@inline function setindex!(x::UseRef, @nospecialize(v))
    x.urs.stmt = _useref_setindex!(x.urs.stmt, x.op, v)
    return x
end

function userefs(@nospecialize(x))
    relevant = (isa(x, Expr) && is_relevant_expr(x)) ||
        isa(x, GotoIfNot) || isa(x, ReturnNode) || isa(x, SSAValue) || isa(x, OldSSAValue) || isa(x, NewSSAValue) ||
        isa(x, PiNode) || isa(x, PhiNode) || isa(x, PhiCNode) || isa(x, UpsilonNode) || isa(x, EnterNode)
    return UseRefIterator(x, relevant)
end

@noinline function _advance(@nospecialize(stmt), op)
    while true
        op += 1
        y = _useref_getindex(stmt, op)
        y === OOB_TOKEN && return nothing
        y === UNDEF_TOKEN || return op
    end
end

@inline function iterate(it::UseRefIterator, op::Int=0)
    it.relevant || return nothing
    op = _advance(it.stmt, op)
    op === nothing && return nothing
    return (UseRef(it, op), op)
end

# This function is used from the show code, which may have a different
# `push!`/`used` type since it's in Base.
scan_ssa_use!(@specialize(push!), used, @nospecialize(stmt)) = foreachssa(ssa::SSAValue -> push!(used, ssa.id), stmt)

# Manually specialized copy of the above with push! === Compiler.push!
scan_ssa_use!(used::IdSet, @nospecialize(stmt)) = foreachssa(ssa::SSAValue -> push!(used, ssa.id), stmt)

function insert_node!(ir::IRCode, pos::SSAValue, newinst::NewInstruction, attach_after::Bool=false)
    posid = pos.id
    if pos.id > length(ir.stmts)
        if attach_after
            info = ir.new_nodes.info[pos.id-length(ir.stmts)];
            posid = info.pos
            attach_after = info.attach_after
        else
            error("Cannot attach before a pending node.")
        end
    end
    node = add_inst!(ir.new_nodes, posid, attach_after)
    newline = something(newinst.line, ir[pos][:line])
    newflag = recompute_inst_flag(newinst, ir)
    node = inst_from_newinst!(node, newinst, newline, newflag)
    return SSAValue(length(ir.stmts) + node.idx)
end
insert_node!(ir::IRCode, pos::Int, newinst::NewInstruction, attach_after::Bool=false) =
    insert_node!(ir, SSAValue(pos), newinst, attach_after)

struct CFGTransformState
    cfg_transforms_enabled::Bool
    fold_constant_branches::Bool
    result_bbs::Vector{BasicBlock}
    bb_rename_pred::Vector{Int}
    bb_rename_succ::Vector{Int}
    domtree::Union{Nothing, DomTree}
end

# N.B.: Takes ownership of the CFG array
function CFGTransformState!(blocks::Vector{BasicBlock}, allow_cfg_transforms::Bool=false)
    if allow_cfg_transforms
        bb_rename = Vector{Int}(undef, length(blocks))
        cur_bb = 1
        domtree = construct_domtree(blocks)
        for i = 1:length(bb_rename)
            if bb_unreachable(domtree, i)
                bb_rename[i] = -1
            else
                bb_rename[i] = cur_bb
                cur_bb += 1
            end
        end
        for i = 1:length(bb_rename)
            bb_rename[i] == -1 && continue
            preds, succs = blocks[i].preds, blocks[i].succs
            # Rename preds
            for j = 1:length(preds)
                if preds[j] != 0
                    preds[j] = bb_rename[preds[j]]
                end
            end
            # Dead blocks get removed from the predecessor list
            filter!(x::Int->x≠-1, preds)
            # Rename succs
            for j = 1:length(succs)
                succs[j] = bb_rename[succs[j]]
            end
        end
        let blocks = blocks, bb_rename = bb_rename
            result_bbs = BasicBlock[blocks[i] for i = 1:length(blocks) if bb_rename[i] != -1]
        end
        # TODO: This could be done by just renaming the domtree
        domtree = construct_domtree(result_bbs)
    else
        bb_rename = Vector{Int}()
        result_bbs = blocks
        domtree = nothing
    end
    return CFGTransformState(allow_cfg_transforms, allow_cfg_transforms, result_bbs, bb_rename, bb_rename, domtree)
end

mutable struct IncrementalCompact
    ir::IRCode
    result::InstructionStream

    cfg_transform::CFGTransformState
    ssa_rename::Vector{Any}

    used_ssas::Vector{Int}
    late_fixup::Vector{Int}
    perm::Vector{Int}
    new_nodes_idx::Int
    # This supports insertion while compacting
    new_new_nodes::NewNodeStream  # New nodes that were before the compaction point at insertion time
    new_new_used_ssas::Vector{Int}
    pending_nodes::NewNodeStream  # New nodes that were after the compaction point at insertion time
    pending_perm::Vector{Int} # pending_nodes.info[pending_perm] is in min-heap order by pos

    # State
    idx::Int
    result_idx::Int
    active_bb::Int
    active_result_bb::Int
    renamed_new_nodes::Bool

    function IncrementalCompact(code::IRCode, cfg_transform::CFGTransformState)
        # Sort by position with attach after nodes after regular ones
        info = code.new_nodes.info
        perm = sort!(collect(eachindex(info)); by=i::Int->(2info[i].pos+info[i].attach_after, i))
        new_len = length(code.stmts) + length(info)
        result = InstructionStream(new_len)
        used_ssas = fill(0, new_len)
        new_new_used_ssas = Vector{Int}()
        blocks = code.cfg.blocks
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        late_fixup = Vector{Int}()
        new_new_nodes = NewNodeStream()
        pending_nodes = NewNodeStream()
        pending_perm = Int[]
        return new(code, result, cfg_transform, ssa_rename, used_ssas, late_fixup, perm, 1,
            new_new_nodes, new_new_used_ssas, pending_nodes, pending_perm,
            1, 1, 1, 1, false)
    end

    # For inlining
    function IncrementalCompact(parent::IncrementalCompact, code::IRCode, result_offset)
        info = code.new_nodes.info
        perm = sort!(collect(eachindex(info)); by=i::Int->(info[i].pos, i))
        new_len = length(code.stmts) + length(info)
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        bb_rename = Vector{Int}()
        pending_nodes = NewNodeStream()
        pending_perm = Int[]
        return new(code, parent.result, CFGTransformState(false, false, parent.cfg_transform.result_bbs, bb_rename, bb_rename, nothing),
            ssa_rename, parent.used_ssas,
            parent.late_fixup, perm, 1,
            parent.new_new_nodes, parent.new_new_used_ssas, pending_nodes, pending_perm,
            1, result_offset, 1, parent.active_result_bb, false)
    end
end

function IncrementalCompact(code::IRCode, allow_cfg_transforms::Bool=false)
    return IncrementalCompact(code, CFGTransformState!(code.cfg.blocks, allow_cfg_transforms))
end

struct TypesView{T}
    ir::T # ::Union{IRCode, IncrementalCompact}
end
types(ir::Union{IRCode, IncrementalCompact}) = TypesView(ir)

function getindex(compact::IncrementalCompact, ssa::SSAValue)
    @assert ssa.id < compact.result_idx
    return compact.result[ssa.id]
end

function getindex(compact::IncrementalCompact, ssa::OldSSAValue)
    id = ssa.id
    if id < compact.idx
        new_idx = compact.ssa_rename[id]::Int
        return compact.result[new_idx]
    elseif id <= length(compact.ir.stmts)
        return compact.ir.stmts[id]
    end
    id -= length(compact.ir.stmts)
    if id <= length(compact.ir.new_nodes)
        return compact.ir.new_nodes.stmts[id]
    end
    id -= length(compact.ir.new_nodes)
    return compact.pending_nodes.stmts[id]
end

function getindex(compact::IncrementalCompact, ssa::NewSSAValue)
    if ssa.id < 0
        return compact.new_new_nodes.stmts[-ssa.id]
    else
        return compact[SSAValue(ssa.id)]
    end
end

function block_for_inst(compact::IncrementalCompact, idx::SSAValue)
    id = idx.id
    if id < compact.result_idx # if ssa within result
        return searchsortedfirst(compact.cfg_transform.result_bbs, BasicBlock(StmtRange(id, id)),
            1, compact.active_result_bb, bb_ordering())-1
    else
        return block_for_inst(compact.ir.cfg, id)
    end
end

function block_for_inst(compact::IncrementalCompact, idx::OldSSAValue)
    id = idx.id
    if id < compact.idx # if ssa within result
        id = compact.ssa_rename[id]::Int
        return block_for_inst(compact, SSAValue(id))
    else
        return block_for_inst(compact.ir.cfg, id)
    end
end

function block_for_inst(compact::IncrementalCompact, idx::NewSSAValue)
    if idx.id > 0
        @assert idx.id < compact.result_idx
        return block_for_inst(compact, SSAValue(idx.id))
    else
        return block_for_inst(compact, SSAValue(compact.new_new_nodes.info[-idx.id].pos))
    end
end

function dominates_ssa(compact::IncrementalCompact, domtree::DomTree, x::AnySSAValue, y::AnySSAValue)
    xb = block_for_inst(compact, x)
    yb = block_for_inst(compact, y)
    if xb == yb
        if isa(compact[x][:stmt], PhiNode)
            if isa(compact[y][:stmt], PhiNode)
                # A node dominates another only if it dominates all uses of that note.
                # Usually that is not a distinction. However, for phi nodes, the use
                # occurs on the edge to the predecessor block. Thus, by definition, for
                # any other PhiNode in the same BB there must be (at least) one edge
                # that this phi node does not dominate.
                return false
            end
        end
        xinfo = yinfo = nothing
        if isa(x, OldSSAValue)
            x′ = compact.ssa_rename[x.id]::SSAValue
        elseif isa(x, NewSSAValue)
            if x.id > 0
                x′ = SSAValue(x.id)
            else
                xinfo = compact.new_new_nodes.info[-x.id]
                x′ = SSAValue(xinfo.pos)
            end
        else
            x′ = x
        end
        if isa(y, OldSSAValue)
            y′ = compact.ssa_rename[y.id]::SSAValue
        elseif isa(y, NewSSAValue)
            if y.id > 0
                y′ = SSAValue(y.id)
            else
                yinfo = compact.new_new_nodes.info[-y.id]
                y′ = SSAValue(yinfo.pos)
            end
        else
            y′ = y
        end
        if x′.id == y′.id
            if xinfo !== nothing && yinfo !== nothing
                if xinfo.attach_after == yinfo.attach_after
                    return x.id < y.id
                end
                return yinfo.attach_after
            elseif xinfo !== nothing
                return !xinfo.attach_after
            elseif yinfo !== nothing
                return yinfo.attach_after
            end
        end
        return x′.id < y′.id
    end
    return dominates(domtree, xb, yb)
end

function _count_added_node!(compact::IncrementalCompact, @nospecialize(val))
    if isa(val, SSAValue)
        compact.used_ssas[val.id] += 1
        return false
    elseif isa(val, NewSSAValue)
        @assert val.id < 0 # Newly added nodes should be canonicalized
        compact.new_new_used_ssas[-val.id] += 1
        return true
    end
    return false
end

function count_added_node!(compact::IncrementalCompact, @nospecialize(v))
    needs_late_fixup = false
    for ops in userefs(v)
        needs_late_fixup |= _count_added_node!(compact, ops[])
    end
    return needs_late_fixup
end

function add_pending!(compact::IncrementalCompact, pos::Int, attach_after::Bool)
    node = add_inst!(compact.pending_nodes, pos, attach_after)
    heappush!(compact.pending_perm, length(compact.pending_nodes), By(x::Int->compact.pending_nodes.info[x].pos))
    return node
end

function inst_from_newinst!(node::Instruction, newinst::NewInstruction,
    newline::Int32=newinst.line::Int32, newflag::UInt32=newinst.flag::UInt32)
    node[:stmt] = newinst.stmt
    node[:type] = newinst.type
    node[:info] = newinst.info
    node[:line] = newline
    node[:flag] = newflag
    return node
end

function recompute_inst_flag(newinst::NewInstruction, src::Union{IRCode,IncrementalCompact})
    flag = newinst.flag
    flag !== nothing && return flag
    flag = IR_FLAG_NULL
    (consistent, effect_free_and_nothrow, nothrow) = stmt_effect_flags(
        fallback_lattice, newinst.stmt, newinst.type, src)
    if consistent
        flag |= IR_FLAG_CONSISTENT
    end
    if effect_free_and_nothrow
        flag |= IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW
    elseif nothrow
        flag |= IR_FLAG_NOTHROW
    end
    if !isexpr(newinst.stmt, :call) && !isexpr(newinst.stmt, :invoke)
        # See comment in check_effect_free!
        flag |= IR_FLAG_NOUB
    end
    return flag
end

function insert_node!(compact::IncrementalCompact, @nospecialize(before), newinst::NewInstruction, attach_after::Bool=false)
    newflag = recompute_inst_flag(newinst, compact)
    if isa(before, SSAValue)
        if before.id < compact.result_idx
            count_added_node!(compact, newinst.stmt)
            newline = something(newinst.line, compact.result[before.id][:line])
            node = add_inst!(compact.new_new_nodes, before.id, attach_after)
            node = inst_from_newinst!(node, newinst, newline, newflag)
            push!(compact.new_new_used_ssas, 0)
            return NewSSAValue(-node.idx)
        else
            newline = something(newinst.line, compact.ir.stmts[before.id][:line])
            node = add_pending!(compact, before.id, attach_after)
            node = inst_from_newinst!(node, newinst, newline, newflag)
            os = OldSSAValue(length(compact.ir.stmts) + length(compact.ir.new_nodes) + length(compact.pending_nodes))
            push!(compact.ssa_rename, os)
            push!(compact.used_ssas, 0)
            return os
        end
    elseif isa(before, OldSSAValue)
        pos = before.id
        if pos < compact.idx
            renamed = compact.ssa_rename[pos]::AnySSAValue
            count_added_node!(compact, newinst.stmt)
            newline = something(newinst.line, compact.result[renamed.id][:line])
            node = add_inst!(compact.new_new_nodes, renamed.id, attach_after)
            node = inst_from_newinst!(node, newinst, newline, newflag)
            push!(compact.new_new_used_ssas, 0)
            return NewSSAValue(-node.idx)
        else
            if pos > length(compact.ir.stmts)
                #@assert attach_after
                info = compact.pending_nodes.info[pos - length(compact.ir.stmts) - length(compact.ir.new_nodes)]
                pos, attach_after = info.pos, info.attach_after
            end
            newline = something(newinst.line, compact.ir.stmts[pos][:line])
            node = add_pending!(compact, pos, attach_after)
            node = inst_from_newinst!(node, newinst, newline, newflag)
            os = OldSSAValue(length(compact.ir.stmts) + length(compact.ir.new_nodes) + length(compact.pending_nodes))
            push!(compact.ssa_rename, os)
            push!(compact.used_ssas, 0)
            return os
        end
    elseif isa(before, NewSSAValue)
        # As above, new_new_nodes must get counted. We don't visit them during our compact,
        # so they're immediately considered reified.
        count_added_node!(compact, newinst.stmt)
        # TODO: This is incorrect and does not maintain ordering among the new nodes
        before_entry = compact.new_new_nodes.info[-before.id]
        newline = something(newinst.line, compact.new_new_nodes.stmts[-before.id][:line])
        new_entry = add_inst!(compact.new_new_nodes, before_entry.pos, attach_after)
        new_entry = inst_from_newinst!(new_entry, newinst, newline, newflag)
        push!(compact.new_new_used_ssas, 0)
        return NewSSAValue(-new_entry.idx)
    else
        error("Unsupported")
    end
end

function maybe_reopen_bb!(compact)
    result_idx = compact.result_idx
    result_bbs = compact.cfg_transform.result_bbs
    if (compact.active_result_bb == length(result_bbs) + 1) ||
        result_idx == first(result_bbs[compact.active_result_bb].stmts)
        compact.active_result_bb -= 1
        return true
    end
    return false
end

function insert_node_here!(compact::IncrementalCompact, newinst::NewInstruction, reverse_affinity::Bool=false)
    newline = newinst.line::Int32
    refinish = false
    result_idx = compact.result_idx
    result_bbs = compact.cfg_transform.result_bbs
    refinish = reverse_affinity && maybe_reopen_bb!(compact)
    if result_idx > length(compact.result)
        @assert result_idx == length(compact.result) + 1
        resize!(compact, result_idx)
    end
    newflag = recompute_inst_flag(newinst, compact)
    node = inst_from_newinst!(compact.result[result_idx], newinst, newline, newflag)
    count_added_node!(compact, newinst.stmt) && push!(compact.late_fixup, result_idx)
    compact.result_idx = result_idx + 1
    inst = SSAValue(result_idx)
    refinish && finish_current_bb!(compact, 0)
    return inst
end

function delete_inst_here!(compact::IncrementalCompact)
    # If we already closed this bb, reopen it for our modification
    refinish = maybe_reopen_bb!(compact)

    # Delete the statement, update refcounts etc
    compact[SSAValue(compact.result_idx-1)] = nothing

    # Pretend that we never compacted this statement in the first place
    compact.result_idx -= 1

    refinish && finish_current_bb!(compact, 0)

    return nothing
end

function getindex(view::TypesView, v::OldSSAValue)
    id = v.id
    ir = view.ir.ir
    stmts = ir.stmts
    if id <= length(stmts)
        return stmts[id][:type]
    end
    id -= length(stmts)
    if id <= length(ir.new_nodes)
        return ir.new_nodes.stmts[id][:type]
    end
    id -= length(ir.new_nodes)
    return view.ir.pending_nodes.stmts[id][:type]
end

function kill_current_use!(compact::IncrementalCompact, @nospecialize(val))
    if isa(val, SSAValue)
        @assert compact.used_ssas[val.id] >= 1
        compact.used_ssas[val.id] -= 1
    elseif isa(val, NewSSAValue)
        @assert val.id < 0
        @assert compact.new_new_used_ssas[-val.id] >= 1
        compact.new_new_used_ssas[-val.id] -= 1
    end
end

function kill_current_uses!(compact::IncrementalCompact, @nospecialize(stmt))
    for ops in userefs(stmt)
        kill_current_use!(compact, ops[])
    end
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), ssa::Union{SSAValue, NewSSAValue})
    (compact[ssa][:stmt] === v) && return compact
    # Kill count for current uses
    kill_current_uses!(compact, compact[ssa][:stmt])
    compact[ssa][:stmt] = v
    # Add count for new use
    count_added_node!(compact, v) && isa(ssa, SSAValue) && push!(compact.late_fixup, ssa.id)
    return compact
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::OldSSAValue)
    id = idx.id
    if id < compact.idx
        new_idx = compact.ssa_rename[id]::Int
        (compact.result[new_idx][:stmt] === v) && return compact
        kill_current_uses!(compact, compact.result[new_idx][:stmt])
        compact.result[new_idx][:stmt] = v
        count_added_node!(compact, v) && push!(compact.late_fixup, new_idx)
        return compact
    elseif id <= length(compact.ir.stmts)  # ir.stmts, new_nodes, and pending_nodes uses aren't counted yet, so no need to adjust
        compact.ir.stmts[id][:stmt] = v
        return compact
    end
    id -= length(compact.ir.stmts)
    if id <= length(compact.ir.new_nodes)
        compact.ir.new_nodes.stmts[id][:stmt] = v
        return compact
    end
    id -= length(compact.ir.new_nodes)
    compact.pending_nodes.stmts[id][:stmt] = v
    return compact
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::Int)
    if idx < compact.result_idx
        compact[SSAValue(idx)] = v
    else
        compact.ir.stmts[idx][:stmt] = v
    end
    return compact
end

__set_check_ssa_counts(onoff::Bool) = __check_ssa_counts__[] = onoff
const __check_ssa_counts__ = fill(false)

should_check_ssa_counts() = __check_ssa_counts__[]

# specifically meant to be used with body1 = compact.result and body2 = compact.new_new_nodes, with nvals == length(compact.used_ssas)
function find_ssavalue_uses1(compact::IncrementalCompact)
    body1, body2 = compact.result.stmt, compact.new_new_nodes.stmts.stmt
    nvals = length(compact.used_ssas)
    nvalsnew = length(compact.new_new_used_ssas)
    nbody1 = compact.result_idx
    nbody2 = length(body2)

    uses = zeros(Int, nvals)
    usesnew = zeros(Int, nvalsnew)
    function increment_uses(ssa::AnySSAValue)
        if isa(ssa, NewSSAValue)
            usesnew[-ssa.id] += 1
        elseif isa(ssa, SSAValue)
            uses[ssa.id] += 1
        end
    end

    for line in 1:(nbody1 + nbody2)
        # index into the right body
        if line <= nbody1
            isassigned(body1, line) || continue
            e = body1[line]
        else
            line -= nbody1
            isassigned(body2, line) || continue
            e = body2[line]
        end

        foreach_anyssa(increment_uses, e)
    end

    return (uses, usesnew)
end

function _oracle_check(compact::IncrementalCompact)
    (observed_used_ssas, observed_used_newssas) = Core.Compiler.find_ssavalue_uses1(compact)
    for i = 1:length(observed_used_ssas)
        if observed_used_ssas[i] != compact.used_ssas[i]
            return (observed_used_ssas, observed_used_newssas, SSAValue(i))
        end
    end
    for i = 1:length(observed_used_newssas)
        if observed_used_newssas[i] != compact.new_new_used_ssas[i]
            return (observed_used_ssas, observed_used_newssas, NewSSAValue(i))
        end
    end
    return (nothing, nothing, 0)
end

function oracle_check(compact::IncrementalCompact)
    (maybe_oracle_used_ssas, observed_used_newssas, oracle_error_ssa) = _oracle_check(compact)
    if maybe_oracle_used_ssas !== nothing
        @eval Main (compact = $compact; oracle_used_ssas = $maybe_oracle_used_ssas; observed_used_newssas = $observed_used_newssas; oracle_error_ssa = $(QuoteNode(oracle_error_ssa)))
        error("Oracle check failed, inspect Main.{compact, oracle_used_ssas, observed_used_newssas, oracle_error_ssa}")
    end
end

getindex(view::TypesView, idx::SSAValue) = getindex(view, idx.id)
function getindex(view::TypesView, idx::Int)
    if isa(view.ir, IncrementalCompact) && idx < view.ir.result_idx
        return view.ir.result[idx][:type]
    elseif isa(view.ir, IncrementalCompact) && view.ir.renamed_new_nodes
        if idx <= length(view.ir.result)
            return view.ir.result[idx][:type]
        else
            return view.ir.new_new_nodes.stmts[idx - length(view.ir.result)][:type]
        end
    else
        ir = isa(view.ir, IncrementalCompact) ? view.ir.ir : view.ir
        if idx <= length(ir.stmts)
            return ir.stmts[idx][:type]
        else
            return ir.new_nodes.stmts[idx - length(ir.stmts)][:type]
        end
    end
end

function getindex(view::TypesView, idx::NewSSAValue)
    return view.ir[idx][:type]
end

# N.B.: Don't make this <: Function to avoid ::Function deopt
struct Refiner
    result_flags::Vector{UInt32}
    result_idx::Int
end
(this::Refiner)() = (this.result_flags[this.result_idx] |= IR_FLAG_REFINED; nothing)

function process_phinode_values(old_values::Vector{Any}, late_fixup::Vector{Int},
                                already_inserted, result_idx::Int,
                                ssa_rename::Vector{Any}, used_ssas::Vector{Int},
                                new_new_used_ssas::Vector{Int},
                                do_rename_ssa::Bool,
                                mark_refined!::Union{Refiner, Nothing})
    values = Vector{Any}(undef, length(old_values))
    for i = 1:length(old_values)
        isassigned(old_values, i) || continue
        val = old_values[i]
        if isa(val, SSAValue)
            if do_rename_ssa
                if !already_inserted(i, OldSSAValue(val.id))
                    push!(late_fixup, result_idx)
                    val = OldSSAValue(val.id)
                else
                    val = renumber_ssa2(val, ssa_rename, used_ssas, new_new_used_ssas, do_rename_ssa, mark_refined!)
                end
            else
                used_ssas[val.id] += 1
            end
        elseif isa(val, OldSSAValue)
            if !already_inserted(i, val)
                push!(late_fixup, result_idx)
            else
                # Always renumber these. do_rename_ssa applies only to actual SSAValues
                val = renumber_ssa2(SSAValue(val.id), ssa_rename, used_ssas, new_new_used_ssas, true, mark_refined!)
            end
        elseif isa(val, NewSSAValue)
            if val.id < 0
                new_new_used_ssas[-val.id] += 1
            else
                @assert do_rename_ssa
                val = SSAValue(val.id)
            end
        end
        if isa(val, NewSSAValue)
            push!(late_fixup, result_idx)
        end
        values[i] = val
    end
    return values
end

function renumber_ssa2(val::SSAValue, ssanums::Vector{Any}, used_ssas::Vector{Int},
        new_new_used_ssas::Vector{Int}, do_rename_ssa::Bool, mark_refined!::Union{Refiner, Nothing})
    id = val.id
    if do_rename_ssa
        if id > length(ssanums)
            return val
        end
        val = ssanums[id]
    end
    if isa(val, Refined)
        val = val.val
        mark_refined! !== nothing && mark_refined!()
    end
    if isa(val, SSAValue)
        used_ssas[val.id] += 1
    elseif isa(val, NewSSAValue)
        @assert val.id < 0
        new_new_used_ssas[-val.id] += 1
    end
    return val
end

function renumber_ssa2(val::NewSSAValue, ssanums::Vector{Any}, used_ssas::Vector{Int},
        new_new_used_ssas::Vector{Int}, do_rename_ssa::Bool, mark_refined!::Union{Refiner, Nothing})
    if val.id < 0
        new_new_used_ssas[-val.id] += 1
        return val
    else
        used_ssas[val.id] += 1
        return SSAValue(val.id)
    end
end

function renumber_ssa2!(@nospecialize(stmt), ssanums::Vector{Any}, used_ssas::Vector{Int}, new_new_used_ssas::Vector{Int}, late_fixup::Vector{Int}, result_idx::Int, do_rename_ssa::Bool, mark_refined!::Union{Refiner, Nothing})
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, OldSSAValue) || isa(val, NewSSAValue)
            push!(late_fixup, result_idx)
        end
        if isa(val, Union{SSAValue, NewSSAValue})
            val = renumber_ssa2(val, ssanums, used_ssas, new_new_used_ssas, do_rename_ssa, mark_refined!)
        end
        if isa(val, OldSSAValue) || isa(val, NewSSAValue)
            push!(late_fixup, result_idx)
        end
        op[] = val
    end
    return urs[]
end

# Used in inlining before we start compacting - Only works at the CFG level
function kill_edge!(bbs::Vector{BasicBlock}, from::Int, to::Int, callback=nothing)
    preds, succs = bbs[to].preds, bbs[from].succs
    deleteat!(preds, findfirst(x::Int->x==from, preds)::Int)
    deleteat!(succs, findfirst(x::Int->x==to, succs)::Int)
    if length(preds) == 0
        for succ in copy(bbs[to].succs)
            kill_edge!(bbs, to, succ, callback)
        end
    end
    if callback !== nothing
        callback(from, to)
    end
end

function kill_edge!(ir::IRCode, from::Int, to::Int, callback=nothing)
    kill_edge!(ir.cfg.blocks, from, to, callback)
end

# N.B.: from and to are non-renamed indices
function kill_edge!(compact::IncrementalCompact, active_bb::Int, from::Int, to::Int)
    # Note: We recursively kill as many edges as are obviously dead.
    (; bb_rename_pred, bb_rename_succ, result_bbs, domtree) = compact.cfg_transform
    preds = result_bbs[bb_rename_succ[to]].preds
    succs = result_bbs[bb_rename_pred[from]].succs
    deleteat!(preds, findfirst(x::Int->x==bb_rename_pred[from], preds)::Int)
    deleteat!(succs, findfirst(x::Int->x==bb_rename_succ[to], succs)::Int)
    if domtree !== nothing
        domtree_delete_edge!(domtree, result_bbs, bb_rename_pred[from], bb_rename_succ[to])
    end
    # Check if the block is now dead
    if length(preds) == 0 || (domtree !== nothing && bb_unreachable(domtree, bb_rename_succ[to]))
        to_succs = result_bbs[bb_rename_succ[to]].succs
        for succ in copy(to_succs)
            new_succ = findfirst(x::Int->x==succ, bb_rename_pred)
            new_succ === nothing && continue
            kill_edge!(compact, active_bb, to, new_succ)
        end
        empty!(preds)
        empty!(to_succs)
        if to < active_bb
            # Kill all statements in the block
            stmts = result_bbs[bb_rename_succ[to]].stmts
            for stmt in stmts
                compact.result[stmt][:stmt] = nothing
            end
            compact.result[last(stmts)][:stmt] = ReturnNode()
        else
            # Tell compaction to not schedule this block. A value of -2 here
            # indicates that the block is not to be scheduled, but there should
            # still be an (unreachable) BB inserted into the final IR to avoid
            # disturbing the BB numbering.
            bb_rename_succ[to] = -2
        end
    else
        # Remove this edge from all phi nodes in `to` block
        # NOTE: It is possible for `to` to contain only `nothing` statements,
        #       so we must be careful to stop at its last statement
        if to < active_bb
            stmts = result_bbs[bb_rename_succ[to]].stmts
            idx = first(stmts)
            while idx <= last(stmts)
                stmt = compact.result[idx][:stmt]
                stmt === nothing && continue
                isa(stmt, PhiNode) || break
                i = findfirst(x::Int32->x==bb_rename_pred[from], stmt.edges)
                if i !== nothing
                    deleteat!(stmt.edges, i)
                    deleteat!(stmt.values, i)
                end
                idx += 1
            end
        else
            stmts = compact.ir.cfg.blocks[to].stmts
            for stmt in CompactPeekIterator(compact, first(stmts), last(stmts))
                stmt === nothing && continue
                isa(stmt, PhiNode) || break
                i = findfirst(x::Int32->x==from, stmt.edges)
                if i !== nothing
                    deleteat!(stmt.edges, i)
                    deleteat!(stmt.values, i)
                end
            end
        end
    end
    nothing
end

struct Refined
    val::Any
    Refined(@nospecialize(val)) = new(val)
end

function process_node!(compact::IncrementalCompact, result_idx::Int, inst::Instruction, idx::Int, processed_idx::Int, active_bb::Int, do_rename_ssa::Bool)
    stmt = inst[:stmt]
    (; result, ssa_rename, late_fixup, used_ssas, new_new_used_ssas) = compact
    (; cfg_transforms_enabled, fold_constant_branches, bb_rename_succ, bb_rename_pred, result_bbs) = compact.cfg_transform
    mark_refined! = Refiner(result.flag, result_idx)
    already_inserted = (::Int, ssa::OldSSAValue)->ssa.id <= processed_idx
    if stmt === nothing
        ssa_rename[idx] = stmt
    elseif isa(stmt, OldSSAValue)
        ssa_rename[idx] = ssa_rename[stmt.id]
    elseif isa(stmt, GotoNode) && cfg_transforms_enabled
        label = bb_rename_succ[stmt.label]
        @assert label > 0
        ssa_rename[idx] = SSAValue(result_idx)
        result[result_idx][:stmt] = GotoNode(label)
        result_idx += 1
    elseif isa(stmt, GlobalRef)
        total_flags = IR_FLAG_CONSISTENT | IR_FLAG_EFFECT_FREE
        flag = result[result_idx][:flag]
        if has_flag(flag, total_flags)
            ssa_rename[idx] = stmt
        else
            ssa_rename[idx] = SSAValue(result_idx)
            result[result_idx][:stmt] = stmt
            result_idx += 1
        end
    elseif isa(stmt, GotoNode)
        ssa_rename[idx] = SSAValue(result_idx)
        result[result_idx][:stmt] = stmt
        result_idx += 1
    elseif isa(stmt, GotoIfNot) && cfg_transforms_enabled
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa, mark_refined!)::GotoIfNot
        result[result_idx][:stmt] = stmt
        cond = stmt.cond
        if fold_constant_branches
            if !isa(cond, Bool)
                condT = widenconditional(argextype(cond, compact))
                isa(condT, Const) || @goto bail
                kill_current_use!(compact, cond)
                cond = condT.val
                isa(cond, Bool) || @goto bail
            end
            if cond
                ssa_rename[idx] = nothing
                result[result_idx][:stmt] = nothing
                kill_edge!(compact, active_bb, active_bb, stmt.dest)
                # Don't increment result_idx => Drop this statement
            else
                label = bb_rename_succ[stmt.dest]
                @assert label > 0
                ssa_rename[idx] = SSAValue(result_idx)
                result[result_idx][:stmt] = GotoNode(label)
                kill_edge!(compact, active_bb, active_bb, active_bb+1)
                result_idx += 1
            end
        else
            @label bail
            label = bb_rename_succ[stmt.dest]
            @assert label > 0
            ssa_rename[idx] = SSAValue(result_idx)
            result[result_idx][:stmt] = GotoIfNot(cond, label)
            result_idx += 1
        end
    elseif cfg_transforms_enabled && isa(stmt, EnterNode)
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa, mark_refined!)::EnterNode
        if stmt.catch_dest != 0
            label = bb_rename_succ[stmt.catch_dest]
            @assert label > 0
            ssa_rename[idx] = SSAValue(result_idx)
            result[result_idx][:stmt] = EnterNode(stmt, label)
        else
            result[result_idx][:stmt] = stmt
        end
        result_idx += 1
    elseif isa(stmt, Expr)
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa, mark_refined!)::Expr
        if isexpr(stmt, :throw_undef_if_not)
            cond = stmt.args[2]
            if isa(cond, Bool) && cond === true
                # cond was folded to true - this statement
                # is dead.
                ssa_rename[idx] = nothing
                return result_idx
            end
        elseif isexpr(stmt, :leave)
            let i = 1
                while i <= length(stmt.args)
                    if stmt.args[i] === nothing
                        deleteat!(stmt.args, i)
                    else
                        i += 1
                    end
                end
            end
            if isempty(stmt.args)
                # This :leave is dead
                ssa_rename[idx] = nothing
                return result_idx
            end
        end
        typ = inst[:type]
        if isa(typ, Const) && is_inlineable_constant(typ.val)
            ssa_rename[idx] = quoted(typ.val)
        else
            ssa_rename[idx] = SSAValue(result_idx)
        end
        result[result_idx][:stmt] = stmt
        result_idx += 1
    elseif isa(stmt, PiNode)
        # As an optimization, we eliminate any trivial pinodes. For performance, we use ===
        # type equality. We may want to consider using == in either a separate pass or if
        # performance turns out ok
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa, mark_refined!)::PiNode
        pi_val = stmt.val
        if isa(pi_val, SSAValue)
            if stmt.typ === result[pi_val.id][:type]
                used_ssas[pi_val.id] -= 1
                ssa_rename[idx] = pi_val
                return result_idx
            end
        elseif isa(pi_val, Argument)
            if stmt.typ === compact.ir.argtypes[pi_val.n]
                ssa_rename[idx] = pi_val
                return result_idx
            end
        elseif !isa(pi_val, AnySSAValue) && !isa(pi_val, GlobalRef)
            pi_val′ = isa(pi_val, QuoteNode) ? pi_val.value : pi_val
            stmttyp = stmt.typ
            if isa(stmttyp, Const) ? pi_val′ === stmttyp.val : typeof(pi_val′) === stmttyp
                ssa_rename[idx] = pi_val
                return result_idx
            end
        end
        ssa_rename[idx] = SSAValue(result_idx)
        result[result_idx][:stmt] = stmt
        result_idx += 1
    elseif isa(stmt, ReturnNode) || isa(stmt, UpsilonNode) || isa(stmt, GotoIfNot) || isa(stmt, EnterNode)
        ssa_rename[idx] = SSAValue(result_idx)
        result[result_idx][:stmt] = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa, mark_refined!)
        result_idx += 1
    elseif isa(stmt, PhiNode)
        # N.B.: For PhiNodes, this needs to be at the top, since PhiNodes
        # can self-reference.
        ssa_rename[idx] = SSAValue(result_idx)
        if cfg_transforms_enabled
            # Rename phi node edges
            let bb_rename_pred=bb_rename_pred
                map!(i::Int32->i == 0 ? 0 : bb_rename_pred[i], stmt.edges, stmt.edges)
            end

            # Remove edges and values associated with dead blocks. Entries in
            # `values` can be undefined when the phi node refers to something
            # that is not defined. (This is not a sign that the compiler is
            # unintentionally leaving some entries in `values` uninitialized.)
            # For example, consider a reference to a variable that is only
            # defined if some branch is taken.
            #
            # In order to leave undefined values undefined (undefined-ness is
            # not a value we can copy), we copy only the edges and (defined)
            # values we want to keep to new arrays initialized with undefined
            # elements.

            edges = Vector{Int32}(undef, length(stmt.edges))
            values = Vector{Any}(undef, length(stmt.values))
            new_index = 1
            for old_index in 1:length(stmt.edges)
                if stmt.edges[old_index] > 0
                    edges[new_index] = stmt.edges[old_index]
                    if isassigned(stmt.values, old_index)
                        values[new_index] = stmt.values[old_index]
                    end
                    new_index += 1
                end
            end
            resize!(edges, new_index-1)
            resize!(values, new_index-1)
        else
            edges = stmt.edges
            values = stmt.values
        end

        values = process_phinode_values(values, late_fixup, already_inserted, result_idx, ssa_rename, used_ssas, new_new_used_ssas, do_rename_ssa, mark_refined!)
        # Don't remove the phi node if it is before the definition of its value
        # because doing so can create forward references. This should only
        # happen with dead loops, but can cause problems when optimization
        # passes look at all code, dead or not. This check should be
        # unnecessary when DCE can remove those dead loops entirely, so this is
        # just to be safe.
        before_def = isassigned(values, 1) && (v = values[1]; isa(v, OldSSAValue)) && idx < v.id
        if length(edges) == 1 && isassigned(values, 1) && !before_def &&
                length(cfg_transforms_enabled ?
                    result_bbs[bb_rename_succ[active_bb]].preds :
                    compact.ir.cfg.blocks[active_bb].preds) == 1
            # There's only one predecessor left - just replace it
            v = values[1]
            @assert !isa(v, NewSSAValue)
            if isa(v, SSAValue)
                used_ssas[v.id] -= 1
            end
            ssa_rename[idx] = v
        else
            result[result_idx][:stmt] = PhiNode(edges, values)
            result_idx += 1
        end
    elseif isa(stmt, PhiCNode)
        ssa_rename[idx] = SSAValue(result_idx)
        values = stmt.values
        if cfg_transforms_enabled
            # Filter arguments that come from dead blocks
            values = Any[]
            for value in stmt.values
                if isa(value, SSAValue)
                    blk = block_for_inst(compact.ir.cfg, value.id)
                    if bb_rename_pred[blk] < 0
                        continue
                    end
                end
                push!(values, value)
            end
        end
        result[result_idx][:stmt] = PhiCNode(process_phinode_values(values, late_fixup, already_inserted, result_idx, ssa_rename, used_ssas, new_new_used_ssas, do_rename_ssa, mark_refined!))
        result_idx += 1
    else
        if isa(stmt, SSAValue)
            # identity assign, replace uses of this ssa value with its result
            if do_rename_ssa
                stmt = ssa_rename[stmt.id]
            end
        elseif isa(stmt, NewSSAValue)
            stmt = SSAValue(stmt.id)
        else
            # Constant assign, replace uses of this ssa value with its result
        end
        if has_flag(inst, IR_FLAG_REFINED) && !isa(stmt, Refined)
            # If we're compacting away an instruction that was marked as refined,
            # leave a marker in the ssa_rename, so we can taint any users.
            stmt = Refined(stmt)
        end
        ssa_rename[idx] = stmt
    end
    return result_idx
end

function resize!(compact::IncrementalCompact, nnewnodes::Int)
    old_length = length(compact.result)
    resize!(compact.result, nnewnodes)
    resize!(compact.used_ssas, nnewnodes)
    for i in (old_length + 1):nnewnodes
        compact.used_ssas[i] = 0
    end
    return compact
end

function finish_current_bb!(compact::IncrementalCompact, active_bb::Int,
                            old_result_idx::Int=compact.result_idx, unreachable::Bool=false)
    (;result_bbs, cfg_transforms_enabled, bb_rename_succ) = compact.cfg_transform
    if compact.active_result_bb > length(result_bbs)
        #@assert compact.bb_rename[active_bb] == -1
        return true
    end
    bb = result_bbs[compact.active_result_bb]
    # If this was the last statement in the BB and we decided to skip it, insert a
    # dummy `nothing` node, to prevent changing the structure of the CFG
    skipped = false
    if !cfg_transforms_enabled || active_bb == 0 || active_bb > length(bb_rename_succ) || bb_rename_succ[active_bb] != -1
        if compact.result_idx == first(bb.stmts)
            length(compact.result) < old_result_idx && resize!(compact, old_result_idx)
            node = compact.result[old_result_idx]
            if unreachable
                node[:stmt], node[:type], node[:line] = ReturnNode(), Union{}, 0
            else
                node[:stmt], node[:type], node[:line], node[:flag] = nothing, Nothing, 0, IR_FLAGS_EFFECTS
            end
            compact.result_idx = old_result_idx + 1
        elseif cfg_transforms_enabled && compact.result_idx - 1 == first(bb.stmts)
            # Optimization: If this BB consists of only a branch, eliminate this bb
        end
        result_bbs[compact.active_result_bb] = BasicBlock(bb, StmtRange(first(bb.stmts), compact.result_idx-1))
        compact.active_result_bb += 1
    else
        skipped = true
    end
    if compact.active_result_bb <= length(result_bbs)
        new_bb = result_bbs[compact.active_result_bb]
        result_bbs[compact.active_result_bb] = BasicBlock(new_bb,
            StmtRange(compact.result_idx, last(new_bb.stmts)))
    end
    return skipped
end

"""
    stmts_awaiting_insertion(compact::IncrementalCompact, idx::Int)

Returns true if there are new/pending instructions enqueued for insertion into
`compact` on any instruction in the range `1:idx`. Otherwise, returns false.
"""
function stmts_awaiting_insertion(compact::IncrementalCompact, idx::Int)

    new_node_waiting = compact.new_nodes_idx <= length(compact.perm) &&
        compact.ir.new_nodes.info[compact.perm[compact.new_nodes_idx]].pos <= idx
    pending_node_waiting = !isempty(compact.pending_perm) &&
        compact.pending_nodes.info[compact.pending_perm[1]].pos <= idx

    return new_node_waiting || pending_node_waiting
end

function process_newnode!(compact::IncrementalCompact, new_idx::Int, new_node_entry::Instruction, new_node_info::NewNodeInfo, idx::Int, active_bb::Int, do_rename_ssa::Bool)
    old_result_idx = compact.result_idx
    bb = compact.ir.cfg.blocks[active_bb]
    node = compact.result[old_result_idx]
    node[] = new_node_entry
    result_idx = process_node!(compact, old_result_idx, node, new_idx, idx - 1, active_bb, do_rename_ssa)
    compact.result_idx = result_idx
    # If this instruction has reverse affinity and we were at the end of a basic block,
    # finish it now.
    if new_node_info.attach_after && idx == last(bb.stmts)+1 && !stmts_awaiting_insertion(compact, idx-1)
        active_bb += 1
        finish_current_bb!(compact, active_bb, old_result_idx)
    end
    return (old_result_idx, result_idx, active_bb)
end

struct CompactPeekIterator
    compact::IncrementalCompact
    start_idx::Int
    end_idx::Int
end

function CompactPeekIterator(compact::IncrementalCompact, start_idx::Int)
    return CompactPeekIterator(compact, start_idx, 0)
end

entry_at_idx(entry::NewNodeInfo, idx::Int) = entry.attach_after ? entry.pos == idx - 1 : entry.pos == idx
function iterate(it::CompactPeekIterator, (idx, aidx, bidx)::NTuple{3, Int}=(it.start_idx, it.compact.new_nodes_idx, 1))
    if it.end_idx > 0 && idx > it.end_idx
        return nothing
    end

    # TODO: Take advantage of the fact that these arrays are sorted
    # TODO: this return value design is horrible
    compact = it.compact
    if compact.new_nodes_idx <= length(compact.perm)
        new_nodes = compact.ir.new_nodes
        for eidx in aidx:length(compact.perm)
            if entry_at_idx(new_nodes.info[compact.perm[eidx]], idx)
                entry = new_nodes.stmts[compact.perm[eidx]]
                return (entry[:stmt], (idx, eidx+1, bidx))
            end
        end
    end
    if !isempty(compact.pending_perm)
        for eidx in bidx:length(compact.pending_perm)
            if entry_at_idx(compact.pending_nodes.info[compact.pending_perm[eidx]], idx)
                entry = compact.pending_nodes.stmts[compact.pending_perm[eidx]]
                return (entry[:stmt], (idx, aidx, eidx+1))
            end
        end
    end
    idx > length(compact.ir.stmts) && return nothing
    return (compact.ir.stmts[idx][:stmt], (idx + 1, aidx, bidx))
end

# the returned Union{Nothing, Pair{Pair{Int,Int},Any}} cannot be stack allocated,
# so we inline this function into the caller
@inline function iterate(compact::IncrementalCompact, state=nothing)
    idxs = iterate_compact(compact)
    idxs === nothing && return nothing
    old_result_idx = idxs[2]
    return Pair{Pair{Int,Int},Any}(idxs, compact.result[old_result_idx][:stmt]), nothing
end

function iterate_compact(compact::IncrementalCompact)
    # Create label to dodge recursion so that we don't stack overflow
    @label restart

    idx = compact.idx
    active_bb = compact.active_bb

    old_result_idx = compact.result_idx
    if idx > length(compact.ir.stmts) && (compact.new_nodes_idx > length(compact.perm))
        return nothing
    end
    if length(compact.result) < old_result_idx
        resize!(compact, old_result_idx)
    end
    bb = compact.ir.cfg.blocks[active_bb]
    (; cfg_transforms_enabled, bb_rename_succ) = compact.cfg_transform
    if cfg_transforms_enabled && active_bb > 1 && active_bb <= length(bb_rename_succ) && bb_rename_succ[active_bb] <= -1
        # Dead block, so kill the entire block.
        compact.idx = last(bb.stmts)
        # Pop any remaining insertion nodes
        while compact.new_nodes_idx <= length(compact.perm)
            entry = compact.ir.new_nodes.info[compact.perm[compact.new_nodes_idx]]
            if !(entry.attach_after ? entry.pos <= compact.idx - 1 : entry.pos <= compact.idx)
                break
            end
            compact.new_nodes_idx += 1
        end
        while !isempty(compact.pending_perm)
            info = compact.pending_nodes.info[compact.pending_perm[1]];
            if !(info.attach_after ? info.pos <= compact.idx - 1 : info.pos <= compact.idx)
                break
            end
            heappop!(compact.pending_perm, By(x::Int -> compact.pending_nodes.info[x].pos))
        end
        # Move to next block
        compact.idx += 1
        compact.active_bb += 1
        if finish_current_bb!(compact, active_bb, old_result_idx, true)
            return iterate_compact(compact)
        else
            return Pair{Int,Int}(compact.idx-1, old_result_idx)
        end
    end
    if compact.new_nodes_idx <= length(compact.perm) &&
        (info = compact.ir.new_nodes.info[compact.perm[compact.new_nodes_idx]];
         info.attach_after ? info.pos == idx - 1 : info.pos == idx)
        new_idx = compact.perm[compact.new_nodes_idx]
        compact.new_nodes_idx += 1
        new_node_entry = compact.ir.new_nodes.stmts[new_idx]
        new_node_info = compact.ir.new_nodes.info[new_idx]
        new_idx += length(compact.ir.stmts)
        (old_result_idx, result_idx, active_bb) =
                process_newnode!(compact, new_idx, new_node_entry, new_node_info, idx, active_bb, true)
        compact.active_bb = active_bb
        old_result_idx == result_idx && @goto restart
        return Pair{Int,Int}(new_idx, old_result_idx)
    elseif !isempty(compact.pending_perm) &&
        (info = compact.pending_nodes.info[compact.pending_perm[1]];
         info.attach_after ? info.pos == idx - 1 : info.pos == idx)
        new_idx = heappop!(compact.pending_perm, By(x::Int -> compact.pending_nodes.info[x].pos))
        new_node_entry = compact.pending_nodes.stmts[new_idx]
        new_node_info = compact.pending_nodes.info[new_idx]
        new_idx += length(compact.ir.stmts) + length(compact.ir.new_nodes)
        (old_result_idx, result_idx, active_bb) =
                process_newnode!(compact, new_idx, new_node_entry, new_node_info, idx, active_bb, false)
        compact.active_bb = active_bb
        old_result_idx == result_idx && @goto restart
        return Pair{Int,Int}(new_idx, old_result_idx)
    end
    # This will get overwritten in future iterations if
    # result_idx is not, incremented, but that's ok and expected
    compact.result[old_result_idx] = compact.ir.stmts[idx]
    result_idx = process_node!(compact, old_result_idx, compact.ir.stmts[idx], idx, idx, active_bb, true)
    compact.result_idx = result_idx
    if idx == last(bb.stmts) && !stmts_awaiting_insertion(compact, idx)
        finish_current_bb!(compact, active_bb, old_result_idx)
        active_bb += 1
    end
    compact.idx = idx + 1
    compact.active_bb = active_bb
    if old_result_idx == compact.result_idx
        idx += 1
        @goto restart
    end
    @assert isassigned(compact.result.stmt, old_result_idx)
    return Pair{Int,Int}(compact.idx-1, old_result_idx)
end

maybe_erase_unused!(compact::IncrementalCompact, idx::Int, in_worklist::Bool, extra_worklist::Vector{Int}) =
    maybe_erase_unused!(null_dce_callback, compact, idx, in_worklist, extra_worklist)
function maybe_erase_unused!(callback::Function, compact::IncrementalCompact, idx::Int,
    in_worklist::Bool, extra_worklist::Vector{Int})
    nresult = length(compact.result)
    inst = idx ≤ nresult ? compact.result[idx] : compact.new_new_nodes.stmts[idx-nresult]
    stmt = inst[:stmt]
    stmt === nothing && return false
    inst[:type] === Bottom && return false
    effect_free = has_flag(inst, (IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW))
    effect_free || return false
    foreachssa(stmt) do val::SSAValue
        if compact.used_ssas[val.id] == 1
            if val.id < idx || in_worklist
                push!(extra_worklist, val.id)
            end
        end
        compact.used_ssas[val.id] -= 1
        callback(val)
    end
    inst[:stmt] = nothing
    return true
end

struct FixedNode
    node::Any
    needs_fixup::Bool
    FixedNode(@nospecialize(node), needs_fixup::Bool) = new(node, needs_fixup)
end

function fixup_phinode_values!(compact::IncrementalCompact, old_values::Vector{Any}, reify_new_nodes::Bool)
    values = Vector{Any}(undef, length(old_values))
    fixup = false
    for i = 1:length(old_values)
        isassigned(old_values, i) || continue
        (; node, needs_fixup) = fixup_node(compact, old_values[i], reify_new_nodes)
        fixup |= needs_fixup
        values[i] = node
    end
    return (values, fixup)
end

function fixup_node(compact::IncrementalCompact, @nospecialize(stmt), reify_new_nodes::Bool)
    if isa(stmt, PhiNode)
        (node, needs_fixup) = fixup_phinode_values!(compact, stmt.values, reify_new_nodes)
        return FixedNode(PhiNode(stmt.edges, node), needs_fixup)
    elseif isa(stmt, PhiCNode)
        (node, needs_fixup) = fixup_phinode_values!(compact, stmt.values, reify_new_nodes)
        return FixedNode(PhiCNode(node), needs_fixup)
    elseif isa(stmt, NewSSAValue)
        @assert stmt.id < 0
        if reify_new_nodes
            val = SSAValue(length(compact.result) - stmt.id)
            return FixedNode(val, false)
        else
            return FixedNode(stmt, true)
        end
    elseif isa(stmt, OldSSAValue)
        node = compact.ssa_rename[stmt.id]
        if isa(node, Refined)
            node = node.val
        end
        needs_fixup = false
        if isa(node, NewSSAValue)
            (;node, needs_fixup) = fixup_node(compact, node, reify_new_nodes)
        end
        if isa(node, SSAValue)
            compact.used_ssas[node.id] += 1
        elseif isa(node, NewSSAValue)
            compact.new_new_used_ssas[-node.id] += 1
        end
        return FixedNode(node, needs_fixup)
    else
        urs = userefs(stmt)
        fixup = false
        for ur in urs
            val = ur[]
            if isa(val, Union{NewSSAValue, OldSSAValue})
                (;node, needs_fixup) = fixup_node(compact, val, reify_new_nodes)
                fixup |= needs_fixup
                ur[] = node
            end
        end
        return FixedNode(urs[], fixup)
    end
end

function just_fixup!(compact::IncrementalCompact, new_new_nodes_offset::Union{Int, Nothing} = nothing, late_fixup_offset::Union{Int, Nothing} = nothing)
    if new_new_nodes_offset === late_fixup_offset === nothing # only do this appending in non_dce_finish!
        resize!(compact.used_ssas, length(compact.result))
        append!(compact.used_ssas, compact.new_new_used_ssas)
        empty!(compact.new_new_used_ssas)
    end
    off = late_fixup_offset === nothing ? 1 : (late_fixup_offset+1)
    set_off = off
    for i in off:length(compact.late_fixup)
        idx = compact.late_fixup[i]
        stmt = compact.result[idx][:stmt]
        (;node, needs_fixup) = fixup_node(compact, stmt, late_fixup_offset === nothing)
        (stmt === node) || (compact.result[idx][:stmt] = node)
        if needs_fixup
            compact.late_fixup[set_off] = idx
            set_off += 1
        end
    end
    if late_fixup_offset !== nothing
        resize!(compact.late_fixup, set_off-1)
    end
    off = new_new_nodes_offset === nothing ? 1 : (new_new_nodes_offset+1)
    for idx in off:length(compact.new_new_nodes)
        new_node = compact.new_new_nodes.stmts[idx]
        stmt = new_node[:stmt]
        (;node) = fixup_node(compact, stmt, late_fixup_offset === nothing)
        if node !== stmt
            new_node[:stmt] = node
        end
    end
end

simple_dce!(compact::IncrementalCompact) = simple_dce!(null_dce_callback, compact)
function simple_dce!(callback::Function, compact::IncrementalCompact)
    # Perform simple DCE for unused values
    @assert isempty(compact.new_new_used_ssas) # just_fixup! wasn't run?
    extra_worklist = Int[]
    for (idx, nused) in Iterators.enumerate(compact.used_ssas)
        nused == 0 || continue
        maybe_erase_unused!(callback, compact, idx, false, extra_worklist)
    end
    while !isempty(extra_worklist)
        maybe_erase_unused!(callback, compact, pop!(extra_worklist), true, extra_worklist)
    end
end

null_dce_callback(x::SSAValue) = return

function non_dce_finish!(compact::IncrementalCompact)
    result_idx = compact.result_idx
    resize!(compact.result, result_idx - 1)
    just_fixup!(compact)
    bb = compact.cfg_transform.result_bbs[end]
    compact.cfg_transform.result_bbs[end] = BasicBlock(bb,
                StmtRange(first(bb.stmts), result_idx-1))
    compact.renamed_new_nodes = true
    nothing
end

function finish(compact::IncrementalCompact)
    non_dce_finish!(compact)
    simple_dce!(compact)
    return complete(compact)
end

function complete(compact::IncrementalCompact)
    result_bbs = resize!(compact.cfg_transform.result_bbs, compact.active_result_bb-1)
    cfg = CFG(result_bbs, Int[first(result_bbs[i].stmts) for i in 2:length(result_bbs)])
    if should_check_ssa_counts()
        oracle_check(compact)
    end

    # trim trailing undefined statements due to copy propagation
    nundef = 0
    for i in length(compact.result):-1:1
        if isassigned(compact.result.stmt, i)
            break
        end
        nundef += 1
    end
    if nundef > 0
        resize!(compact.result, length(compact.result) - nundef)
    end

    return IRCode(compact.ir, compact.result, cfg, compact.new_new_nodes)
end

function compact!(code::IRCode, allow_cfg_transforms::Bool=false)
    compact = IncrementalCompact(code, allow_cfg_transforms)
    # Just run through the iterator without any processing
    for _ in compact; end # _ isa Pair{Int, Any}
    return finish(compact)
end

struct BBIdxIter
    ir::IRCode
end

bbidxiter(ir::IRCode) = BBIdxIter(ir)

function iterate(x::BBIdxIter, (idx, bb)::Tuple{Int, Int}=(1, 1))
    idx > length(x.ir.stmts) && return nothing
    active_bb = x.ir.cfg.blocks[bb]
    next_bb = bb
    if idx == last(active_bb.stmts)
        next_bb += 1
    end
    return (bb, idx), (idx + 1, next_bb)
end

# Inserters

abstract type Inserter; end

struct InsertHere <: Inserter
    compact::IncrementalCompact
end
(i::InsertHere)(newinst::NewInstruction) = insert_node_here!(i.compact, newinst)

struct InsertBefore{T<:Union{IRCode, IncrementalCompact}} <: Inserter
    src::T
    pos::SSAValue
end
(i::InsertBefore)(newinst::NewInstruction) = insert_node!(i.src, i.pos, newinst)

###
### CFG manipulation tools
###

"""
    NewBlocksInfo

Information on basic blocks newly allocated in `allocate_new_blocks!`.  See
[`allocate_new_blocks!`](@ref) for explanation on the properties.
"""
struct NewBlocksInfo
    positions_nblocks::Vector{Pair{Int,Int}}
    block_to_range::IdDict{Int,UnitRange{Int}}
    ssachangemap::Vector{Int}
    bbchangemap::Vector{Int}
end

"""
    allocate_new_blocks!(
        ir::IRCode,
        positions_nblocks::Vector{Pair{Int,Int}},
    ) -> info::NewBlocksInfo

For each `position => nblocks` in `positions_nblocks`, add new "singleton" `nblocks` blocks
(i.e., each BB contains a single dummy instruction) before the statement `position`.

The caller must ensure that:

    statement_positions = map(first, positions_nblocks)
    @assert all(>(0), diff(statement_positions))
    @assert all(1 <= p <= length(ir.stmts) for p in statement_positions)
    @assert all(nblocks > 0 for (_, nblocks) in positions_nblocks)

Note that this function does not wire up the CFG for newly created BBs. It just
inserts the dummy `GotoNode(0)` at the end of the new singleton BBs and the BB
_before_ (in terms of `ir.cfg.bocks`) it.  The predecessors of the BB just
before the newly added singleton BB and the successors of the BB just after the
newly added singleton BB are re-wired.  See `allocate_goto_sequence!` for
an example for creating a valid CFG.

For example, given an `ir` containing:

    #bb
        %1 = instruction_1
        %2 = instruction_2

`allocate_new_blocks!(ir, [2 => 1])` produces

    #bb′
        %1 = instruction_1
        goto #0               # dummy
    #new_bb_1
        goto #0               # dummy
    #new_bb_2
        %2 = instruction_2

The predecessors of `#bb′` are equivalent to the predecessors of `#bb`.  The successors of
`#new_bb_2` are equivalent to the successors of `#bb`.

The returned object `info::NewBlocksInfo` has the following properties:

* `positions_nblocks`: The second argument.
* `block_to_range`: A mapping from an old basic block index to the indices of
  `positions_nblocks` that contains the statements of the old basic block.
* `ssachangemap`: Given an original statement position `iold`, the new statement position is
  `ssachangemap[iold]`.
* `bbchangemap`: Given an original basic block id `iold`, the new basic block ID is
  `bbchangemap[iold]`.

Functions [`split_blocks`](@ref), [`split_positions`](@ref), and
[`inserted_blocks`](@ref) can be used to iterate over the newly added basic blocks.  These
functions support the following access patterns:

```julia
blocks::NewBlocksInfo
for sb in split_blocks(blocks)  # iterate over split BBs
    sb.oldbb::Int
    sb.indices::UnitRange{Int}

    for sp in split_positions(sb)  # iterate over split positions
        sp.oldbb::Int;   @assert sp.oldbb == sb.oldbb
        sp.prebb::Int    # BB before the split position (exclusive)
        sp.postbb::Int   # BB after the split position (inclusive)
        sp.index::Int;   @assert sp.index ∈ sb.indices

        position, nblocks = blocks.positions_nblocks[sp.index]
        for ib in inserted_blocks(sp)  # iterate over newly added BBs
            ib.oldbb::Int;  @assert ib.oldbb == sb.oldbb
            ib.newbb::Int;  @assert sp.prebb < ib.newbb < sp.postbb
            ib.index::Int;  @assert ib.index == ib.index
            ib.nth::Int;    @assert ib.nth ∈ 1:nblocks
        end
    end
end
```
"""
function allocate_new_blocks!(ir::IRCode, positions_nblocks::Vector{Pair{Int,Int}})
    @assert isincreasing(positions_nblocks; by = first)
    ssachangemap = Vector{Int}(undef, length(ir.stmts) + length(ir.new_nodes.stmts))
    let iold = 1, inew = 1
        for (pos, nblocks) in positions_nblocks
            @assert 1 <= pos <= length(ir.stmts)
            @assert nblocks >= 0
            for i in iold:pos-1
                ssachangemap[i] = inew
                inew += 1
            end
            inew += 1 + nblocks
            iold = pos
        end
        for i in iold:length(ssachangemap)
            ssachangemap[i] = inew
            inew += 1
        end
    end

    # For the original basic block index `ibb`, the pairs of position and the number of
    # blocks to be inserted can be obtained by:
    #     indices = block_to_range[ibb]::UnitRange
    #     positions_nblocks[indices]
    block_to_range = IdDict{Int,UnitRange{Int}}()

    # Two maps are used for relabeling BBs:
    # * `bbchangemap` maps each old BB index to the index of the BB that includes the *last*
    #   statement in the old BB.
    # * `gotolabelchangemap` maps each old BB index to the index of the BB that includes
    #   *first* statement in the old BB; i.e., it is used for fixing the labels in the
    #   goto-like nodes.
    bbchangemap = ones(Int, length(ir.cfg.blocks))
    gotolabelchangemap = ones(Int, length(ir.cfg.blocks) + 1)  # "+ 1" simplifies the code
    newblocks = 0

    let pre_index = 0, pre_ibb = 0
        for (i, (ipos, nblocks)) in pairs(positions_nblocks)
            ibb = block_for_inst(ir.cfg, ipos)

            if pre_ibb != ibb
                if pre_ibb != 0
                    block_to_range[pre_ibb] = pre_index:i-1
                end
                pre_index = i
                pre_ibb = ibb
            end

            bbchangemap[ibb] += 1 + nblocks
            gotolabelchangemap[ibb+1] += 1 + nblocks
            newblocks += 1 + nblocks
        end
        block_to_range[pre_ibb] = pre_index:length(positions_nblocks)
    end
    _cumsum!(bbchangemap)
    _cumsum!(gotolabelchangemap)

    # Insert `newblocks` new blocks:
    oldnblocks = length(ir.cfg.blocks)
    resize!(ir.cfg.blocks, oldnblocks + newblocks)
    # Copy pre-existing blocks:
    for iold in oldnblocks:-1:1
        bb = ir.cfg.blocks[iold]
        for labels in (bb.preds, bb.succs)
            for (i, l) in pairs(labels)
                labels[i] = bbchangemap[l]
            end
            # Note: Some labels that are referring to the split BBs are still incorrect
            # at this point.  These are copied to the new BBs in the next phase (and thus
            # relabeling here is still required).
        end
        start = ssachangemap[bb.stmts.start]
        stop = ssachangemap[bb.stmts.stop]
        ir.cfg.blocks[bbchangemap[iold]] = BasicBlock(bb, StmtRange(start, stop))
    end
    # Insert new blocks:
    for (iold, indices) in block_to_range
        ilst = bbchangemap[iold]  # using bbchangemap as it's already moved
        bblst = ir.cfg.blocks[ilst]

        # Assign `StmtRange`s to the new BBs (edges are handled later)
        prefirst = bblst.stmts.start  # already moved
        inew = get(bbchangemap, iold - 1, 0) + 1
        local oldpos
        for i in indices
            oldpos, nblocks = positions_nblocks[i]
            p = get(ssachangemap, oldpos - 1, 0) + 1
            ir.cfg.blocks[inew] = BasicBlock(StmtRange(min(prefirst, p), p))
            inew += 1
            p += 1
            for _ in 1:nblocks
                ir.cfg.blocks[inew] = BasicBlock(StmtRange(p, p))
                inew += 1
                p += 1
            end
            @assert p == ssachangemap[oldpos]
            prefirst = p
        end

        # Handle edges of the "head" and "tail" BBs
        ifst = get(bbchangemap, iold - 1, 0) + 1
        bbfst = ir.cfg.blocks[ifst]
        for p in bblst.preds
            k = findfirst(==(ilst), ir.cfg.blocks[p].succs)
            @assert k !== nothing
            ir.cfg.blocks[p].succs[k] = ifst
        end
        copy!(bbfst.preds, bblst.preds)
        empty!(bblst.preds)
        stmts = StmtRange(ssachangemap[oldpos], last(bblst.stmts))
        ir.cfg.blocks[bbchangemap[iold]] = BasicBlock(bblst, stmts)
        @assert !isempty(stmts)
    end
    for bb in ir.cfg.blocks
        @assert !isempty(bb.stmts)
    end
    cfg_reindex!(ir.cfg)

    on_ssavalue(v) = SSAValue(ssachangemap[v.id])
    on_phi_label(l) = bbchangemap[l]
    on_goto_label(l) = gotolabelchangemap[l]
    for stmts in (ir.stmts, ir.new_nodes.stmts)
        for i in 1:length(stmts)
            st = stmts[i]
            inst = ssamap(on_ssavalue, st[:inst])
            if inst isa PhiNode
                edges = inst.edges::Vector{Int32}
                for i in 1:length(edges)
                    edges[i] = on_phi_label(edges[i])
                end
            elseif inst isa GotoNode
                inst = GotoNode(on_goto_label(inst.label))
            elseif inst isa GotoIfNot
                inst = GotoIfNot(inst.cond, on_goto_label(inst.dest))
            elseif isexpr(inst, :enter)
                inst.args[1] = on_goto_label(inst.args[1]::Int)
            end
            st[:inst] = inst
        end
    end
    minpos, _ = positions_nblocks[1]  # it's sorted
    for (i, info) in pairs(ir.new_nodes.info)
        if info.pos >= minpos
            ir.new_nodes.info[i] = if info.attach_after
                NewNodeInfo(ssachangemap[info.pos], info.attach_after)
            else
                NewNodeInfo(get(ssachangemap, info.pos - 1, 0) + 1, info.attach_after)
            end
        end
    end

    # Fixup `ir.linetable` before mutating `ir.stmts.lines`:
    linetablechangemap = Vector{Int32}(undef, length(ir.linetable))
    fill!(linetablechangemap, 1)
    let lines = ir.stmts.line
        # Allocate spaces for newly inserted statements
        for (pos, nblocks) in positions_nblocks
            linetablechangemap[lines[pos]] += 1 + nblocks
        end
    end
    _cumsum!(linetablechangemap)
    let newlength = linetablechangemap[end], ilast = newlength + 1
        @assert newlength == length(ir.linetable) + newblocks
        resize!(ir.linetable, newlength)
        for iold in length(linetablechangemap):-1:1
            inew = linetablechangemap[iold]
            oldinfo = ir.linetable[iold]
            inlined_at = oldinfo.inlined_at
            if inlined_at != 0
                inlined_at = linetablechangemap[inlined_at]
            end
            newinfo = LineInfoNode(
                oldinfo.module,
                oldinfo.method,
                oldinfo.file,
                oldinfo.line,
                inlined_at,
            )
            for i in inew:ilast-1
                ir.linetable[i] = newinfo
            end
            ilast = inew
        end
    end

    # Fixup `ir.stmts.line`
    let lines = ir.stmts.line, iold = length(lines), inew = iold + newblocks

        resize!(lines, inew)
        for i in length(positions_nblocks):-1:1
            pos, nblocks = positions_nblocks[i]
            while pos <= iold
                lines[inew] = linetablechangemap[lines[iold]]
                iold -= 1
                inew -= 1
            end
            for _ in 1:1+nblocks
                lines[inew] = linetablechangemap[lines[iold+1]]
                inew -= 1
            end
        end
        @assert inew == iold
    end

    # Fixup `ir.new_nodes.stmts.line`
    let lines = ir.new_nodes.stmts.line
        for i in 1:length(lines)
            lines[i] = linetablechangemap[lines[i]]
        end
    end

    function allocate_stmts!(xs, filler)
        n = length(xs)
        resize!(xs, length(xs) + newblocks)
        for i in n:-1:1
            xs[ssachangemap[i]] = xs[i]
        end
        for i in 2:n
            for j in ssachangemap[i-1]+1:ssachangemap[i]-1
                xs[j] = filler
            end
        end
        for js in (1:ssachangemap[1]-1, ssachangemap[end]+1:length(xs))
            for j in js
                xs[j] = filler
            end
        end
    end

    allocate_stmts!(ir.stmts.stmt, GotoNode(0))  # dummy
    allocate_stmts!(ir.stmts.type, Any)
    allocate_stmts!(ir.stmts.info, NoCallInfo())
    allocate_stmts!(ir.stmts.flag, 0)

    return NewBlocksInfo(positions_nblocks, block_to_range, ssachangemap, bbchangemap)
end

"""
    split_blocks(blocks::NewBlocksInfo)

Iterate over old basic blocks that are split.

Each element `sb::SplitBlock` of the iterable returned from `split_blocks` has the following
properties:

* `blocks::NewBlocksInfo`
* `oldbb::Int`: Old index of a BB that is split.
* `indices`: For each `i` in `indices`, `blocks.positions_nblocks[i]` is the pair
  `position => nblocks` that specifies that `nblocks` new blocks are inserted at statement
  `position`.

Use `split_positions(sb::SplitBlock)` to iterate over the statement positions at which
the old basic blocks are split.
"""
function split_blocks end

"""
    split_positions(sb::SplitBlock)
    split_positions(blocks::NewBlocksInfo)

Iterate over the statement positions at which the old basic blocks are split.

Each element `sp::SplitPosition` of the iterable returned from `split_positions` has the
following properties:

* `blocks::NewBlocksInfo`
* `index`: `blocks.positions_nblocks[index]` is the pair `position => nblocks` that
  specifies that `nblocks` new blocks are inserted at statement `position`.
* `oldbb::Int`: Old index of a BB that is split.
* `prebb::Int`: New index of the BB before the split position (exclusive).
* `postbb::Int`: New index of the BB after the split position (inclusive).

Use `inserted_blocks(sp::SplitPosition)` to iterate over the newly added "singleton"
basic blocks.
"""
function split_positions end

"""
    inserted_blocks(sp::SplitPosition)
    inserted_blocks(sb::SplitBlock)
    inserted_blocks(blocks::NewBlocksInfo)

Iterate over the newly added basic blocks.

Each element `ib::InsertedBlock` of the iterable returned from `inserted_blocks` has the
following properties:

* `blocks::NewBlocksInfo`
* `index`: `blocks.positions_nblocks[index]` is the pair `position => nblocks` that
  specifies that `nblocks` new blocks are inserted at statement `position`.
* `oldbb::Int`: Old index of a BB that is split.
* `newbb::Int`: New index of this BB.
* `nth::Int`: This BB is the `nth` BB at this split position.
"""
function inserted_blocks end

struct SplitBlock
    blocks::NewBlocksInfo
    oldbb::Int
    indices::UnitRange{Int}
end

struct SplitPosition
    blocks::NewBlocksInfo
    oldbb::Int
    prebb::Int
    postbb::Int
    index::Int
end

struct InsertedBlock
    blocks::NewBlocksInfo
    oldbb::Int
    newbb::Int
    index::Int
    nth::Int
end

new_head_bb(sb::SplitBlock) = get(sb.blocks.bbchangemap, sb.oldbb - 1, 0) + 1
new_tail_bb(sb::SplitBlock) = sb.blocks.bbchangemap[sb.oldbb]

function split_blocks(blocks::NewBlocksInfo)
    oldblocks = sort!(collect(Int, keys(blocks.block_to_range)))
    Iterators.map(oldblocks) do oldbb
        indices = blocks.block_to_range[oldbb]
        SplitBlock(blocks, oldbb, indices)
    end
end

struct SplitPositionIterator
    split::SplitBlock
end

function iterate(
    it::SplitPositionIterator,
    (index, prevbb) = (first(it.split.indices), new_head_bb(it.split)),
)
    (; blocks, oldbb, indices) = it.split
    index > last(indices) && return nothing
    _pos, nblocks = blocks.positions_nblocks[index]
    postbb = prevbb + 1 + nblocks
    sp = SplitPosition(blocks, oldbb, prevbb, postbb, index)
    return (sp, (index + 1, postbb))
end

split_positions(sb::SplitBlock) = SplitPositionIterator(sb)
split_positions(blocks::NewBlocksInfo) =
    Iterators.flatmap(split_positions, split_blocks(blocks))

function inserted_blocks(sp::SplitPosition)
    (; blocks, index) = sp
    _pos, nblocks = blocks.positions_nblocks[index]
    Iterators.map(1:nblocks) do nth
        InsertedBlock(sp.blocks, sp.oldbb, sp.prebb + nth, sp.index, nth)
    end
end

inserted_blocks(x) = Iterators.flatmap(inserted_blocks, split_positions(x))

"""
    allocate_goto_sequence!(ir::IRCode, positions_nblocks) -> info::NewBlocksInfo

Add new BBs using `allocate_new_blocks!(ir, positions_nblocks)` and then connect them by
"no-op" `GotoNode` that jumps to the next BB.  Unlike `allocate_new_blocks!`, this function
results in an IR with valid CFG.

Read `allocate_new_blocks!` on the preconditions on `positions_nblocks`.

For example, given an `ir` containing:

    #bb
        %1 = instruction_1
        %2 = instruction_2

`allocate_new_blocks!(ir, [2 => 1])` produces

    #bb′
        %1 = instruction_1
        goto #new_bb_1
    #new_bb_1
        goto #new_bb_2
    #new_bb_2
        %2 = instruction_2
"""
function allocate_goto_sequence!(ir::IRCode, positions_nblocks)
    blocks = allocate_new_blocks!(ir, positions_nblocks)
    function set_goto(ibb1::Int)
        ibb2 = ibb1 + 1
        b1 = ir.cfg.blocks[ibb1]
        @assert ir.stmts.stmt[last(b1.stmts)] === GotoNode(0)
        ir.stmts.stmt[last(b1.stmts)] = GotoNode(ibb2)
        cfg_insert_edge!(ir.cfg, ibb1, ibb2)
    end
    for sp in split_positions(blocks)
        set_goto(sp.prebb)
        for block in inserted_blocks(sp)
            set_goto(block.newbb)
        end
    end
    return blocks
end
