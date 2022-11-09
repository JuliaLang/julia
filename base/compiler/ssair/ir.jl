# This file is a part of Julia. License is MIT: https://julialang.org/license

Core.PhiNode() = Core.PhiNode(Int32[], Any[])

isterminator(@nospecialize(stmt)) = isa(stmt, GotoNode) || isa(stmt, GotoIfNot) || isa(stmt, ReturnNode)

struct CFG
    blocks::Vector{BasicBlock}
    index::Vector{Int} # map from instruction => basic-block number
                       # TODO: make this O(1) instead of O(log(n_blocks))?
end

copy(c::CFG) = CFG(BasicBlock[copy(b) for b in c.blocks], copy(c.index))

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
    deleteat!(preds, findfirst(x->x === from, preds)::Int)
    deleteat!(succs, findfirst(x->x === to, succs)::Int)
    nothing
end

function bb_ordering()
    lt=(<=)
    by=x->first(x.stmts)
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
        elseif isa(stmt, Expr)
            if stmt.head === :leave
                # :leave terminates a BB
                push!(jump_dests, idx+1)
            elseif stmt.head === :enter
                # :enter starts/ends a BB
                push!(jump_dests, idx)
                push!(jump_dests, idx+1)
                # The catch block is a jump dest
                push!(jump_dests, stmt.args[1]::Int)
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
        elseif isexpr(terminator, :enter)
            # :enter gets a virtual edge to the exception handler and
            # the exception handler gets a virtual edge from outside
            # the function.
            block′ = block_for_inst(basic_block_index, terminator.args[1]::Int)
            push!(blocks[block′].preds, num)
            push!(blocks[block′].preds, 0)
            push!(b.succs, block′)
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
function first_insert_for_bb(code::Vector{Any}, cfg::CFG, block::Int)
    for idx in cfg.blocks[block].stmts
        stmt = code[idx]
        if !isa(stmt, PhiNode)
            return idx
        end
    end
    error("any insert position isn't found")
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
    inst::Vector{Any}
    type::Vector{Any}
    info::Vector{CallInfo}
    line::Vector{Int32}
    flag::Vector{UInt8}
end
function InstructionStream(len::Int)
    insts = Vector{Any}(undef, len)
    types = Vector{Any}(undef, len)
    info = Vector{CallInfo}(undef, len)
    fill!(info, NoCallInfo())
    lines = fill(Int32(0), len)
    flags = fill(IR_FLAG_NULL, len)
    return InstructionStream(insts, types, info, lines, flags)
end
InstructionStream() = InstructionStream(0)
length(is::InstructionStream) = length(is.inst)
isempty(is::InstructionStream) = isempty(is.inst)
function add_new_idx!(is::InstructionStream)
    ninst = length(is) + 1
    resize!(is, ninst)
    return ninst
end
function copy(is::InstructionStream)
    return InstructionStream(
        copy_exprargs(is.inst),
        copy(is.type),
        copy(is.info),
        copy(is.line),
        copy(is.flag))
end
function resize!(stmts::InstructionStream, len)
    old_length = length(stmts)
    resize!(stmts.inst, len)
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

struct Instruction
    data::InstructionStream
    idx::Int
end
Instruction(is::InstructionStream) = Instruction(is, add_new_idx!(is))

@inline function getindex(node::Instruction, fld::Symbol)
    isdefined(node, fld) && return getfield(node, fld)
    return getfield(getfield(node, :data), fld)[getfield(node, :idx)]
end
@inline function setindex!(node::Instruction, @nospecialize(val), fld::Symbol)
    getfield(getfield(node, :data), fld)[getfield(node, :idx)] = val
    return node
end

@inline getindex(is::InstructionStream, idx::Int) = Instruction(is, idx)
function setindex!(is::InstructionStream, newval::Instruction, idx::Int)
    is.inst[idx] = newval[:inst]
    is.type[idx] = newval[:type]
    is.info[idx] = newval[:info]
    is.line[idx] = newval[:line]
    is.flag[idx] = newval[:flag]
    return is
end
function setindex!(is::InstructionStream, newval::Union{AnySSAValue, Nothing}, idx::Int)
    is.inst[idx] = newval
    return is
end
function setindex!(node::Instruction, newval::Instruction)
    node.data[node.idx] = newval
    return node
end

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
    flag::Union{UInt8,Nothing} # if nothing, IR flags will be recomputed on insertion
    function NewInstruction(@nospecialize(stmt), @nospecialize(type), @nospecialize(info::CallInfo),
                            line::Union{Int32,Nothing}, flag::Union{UInt8,Nothing})
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
    flag::Union{UInt8,Nothing}=newinst.flag)
    return NewInstruction(stmt, type, info, line, flag)
end
function NewInstruction(inst::Instruction;
    stmt::Any=inst[:inst],
    type::Any=inst[:type],
    info::CallInfo=inst[:info],
    line::Union{Int32,Nothing}=inst[:line],
    flag::Union{UInt8,Nothing}=inst[:flag])
    return NewInstruction(stmt, type, info, line, flag)
end
@specialize
effect_free(newinst::NewInstruction) = NewInstruction(newinst; flag=add_flag(newinst, IR_FLAG_EFFECT_FREE))
non_effect_free(newinst::NewInstruction) = NewInstruction(newinst; flag=sub_flag(newinst, IR_FLAG_EFFECT_FREE))
with_flags(newinst::NewInstruction, flags::UInt8) = NewInstruction(newinst; flag=add_flag(newinst, flags))
without_flags(newinst::NewInstruction, flags::UInt8) = NewInstruction(newinst; flag=sub_flag(newinst, flags))
function add_flag(newinst::NewInstruction, newflag::UInt8)
    flag = newinst.flag
    flag === nothing && return newflag
    return flag | newflag
end
function sub_flag(newinst::NewInstruction, newflag::UInt8)
    flag = newinst.flag
    flag === nothing && return IR_FLAG_NULL
    return flag & ~newflag
end

struct IRCode
    stmts::InstructionStream
    argtypes::Vector{Any}
    sptypes::Vector{Any}
    linetable::Vector{LineInfoNode}
    cfg::CFG
    new_nodes::NewNodeStream
    meta::Vector{Expr}

    function IRCode(stmts::InstructionStream, cfg::CFG, linetable::Vector{LineInfoNode}, argtypes::Vector{Any}, meta::Vector{Expr}, sptypes::Vector{Any})
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
    ir = IRCode(InstructionStream(1), CFG([BasicBlock(1:1, Int[], Int[])], Int[1]), LineInfoNode[], Any[], Expr[], Any[])
    ir[SSAValue(1)][:inst] = ReturnNode(nothing)
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

function getindex(x::IRCode, s::SSAValue)
    if s.id <= length(x.stmts)
        return x.stmts[s.id]
    else
        return x.new_nodes.stmts[s.id - length(x.stmts)]
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
    elseif isa(stmt, PiNode)
        isdefined(stmt, :val) || return OOB_TOKEN
        op == 1 || return OOB_TOKEN
        return stmt.val
    elseif isa(stmt, Union{SSAValue, NewSSAValue, GlobalRef})
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
                      :undefcheck, :throw_undef_if_not,
                      :cfunction, :method, :pop_exception,
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
        stmt = typeof(stmt)(v)
    elseif isa(stmt, Union{SSAValue, NewSSAValue, GlobalRef})
        op == 1 || throw(BoundsError())
        stmt = v
    elseif isa(stmt, UpsilonNode)
        op == 1 || throw(BoundsError())
        stmt = typeof(stmt)(v)
    elseif isa(stmt, PiNode)
        op == 1 || throw(BoundsError())
        stmt = typeof(stmt)(v, stmt.typ)
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
        isa(x, GotoIfNot) || isa(x, ReturnNode) || isa(x, SSAValue) || isa(x, NewSSAValue) ||
        isa(x, PiNode) || isa(x, PhiNode) || isa(x, PhiCNode) || isa(x, UpsilonNode)
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


mutable struct IncrementalCompact
    ir::IRCode
    result::InstructionStream
    result_bbs::Vector{BasicBlock}

    ssa_rename::Vector{Any}
    bb_rename_pred::Vector{Int}
    bb_rename_succ::Vector{Int}

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
    cfg_transforms_enabled::Bool
    fold_constant_branches::Bool

    function IncrementalCompact(code::IRCode, allow_cfg_transforms::Bool=false)
        # Sort by position with attach after nodes after regular ones
        info = code.new_nodes.info
        perm = sort!(collect(eachindex(info)); by=i->(2info[i].pos+info[i].attach_after, i))
        new_len = length(code.stmts) + length(info)
        result = InstructionStream(new_len)
        used_ssas = fill(0, new_len)
        new_new_used_ssas = Vector{Int}()
        blocks = code.cfg.blocks
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
                filter!(x->x !== -1, preds)
                # Rename succs
                for j = 1:length(succs)
                    succs[j] = bb_rename[succs[j]]
                end
            end
            let blocks = blocks, bb_rename = bb_rename
                result_bbs = BasicBlock[blocks[i] for i = 1:length(blocks) if bb_rename[i] != -1]
            end
        else
            bb_rename = Vector{Int}()
            result_bbs = code.cfg.blocks
        end
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        late_fixup = Vector{Int}()
        new_new_nodes = NewNodeStream()
        pending_nodes = NewNodeStream()
        pending_perm = Int[]
        return new(code, result, result_bbs, ssa_rename, bb_rename, bb_rename, used_ssas, late_fixup, perm, 1,
            new_new_nodes, new_new_used_ssas, pending_nodes, pending_perm,
            1, 1, 1, 1, false, allow_cfg_transforms, allow_cfg_transforms)
    end

    # For inlining
    function IncrementalCompact(parent::IncrementalCompact, code::IRCode, result_offset)
        info = code.new_nodes.info
        perm = sort!(collect(eachindex(info)); by=i->(info[i].pos, i))
        new_len = length(code.stmts) + length(info)
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        bb_rename = Vector{Int}()
        pending_nodes = NewNodeStream()
        pending_perm = Int[]
        return new(code, parent.result,
            parent.result_bbs, ssa_rename, bb_rename, bb_rename, parent.used_ssas,
            parent.late_fixup, perm, 1,
            parent.new_new_nodes, parent.new_new_used_ssas, pending_nodes, pending_perm,
            1, result_offset, 1, parent.active_result_bb, false, false, false)
    end
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
        new_idx = compact.ssa_rename[id]
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
        return searchsortedfirst(compact.result_bbs, BasicBlock(StmtRange(id, id)),
            1, compact.active_result_bb, bb_ordering())-1
    else
        return block_for_inst(compact.ir.cfg, id)
    end
end

function block_for_inst(compact::IncrementalCompact, idx::OldSSAValue)
    id = idx.id
    if id < compact.idx # if ssa within result
        id = compact.ssa_rename[id]
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
        if x′.id == y′.id && (xinfo !== nothing || yinfo !== nothing)
            if xinfo !== nothing && yinfo !== nothing
                if xinfo.attach_after == yinfo.attach_after
                    return x.id < y.id
                end
                return yinfo.attach_after
            elseif xinfo !== nothing
                return !xinfo.attach_after
            else
                return (yinfo::NewNodeInfo).attach_after
            end
        end
        return x′.id < y′.id
    end
    return dominates(domtree, xb, yb)
end

function _count_added_node!(compact,  @nospecialize(val))
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
    heappush!(compact.pending_perm, length(compact.pending_nodes), By(x -> compact.pending_nodes.info[x].pos))
    return node
end

function inst_from_newinst!(node::Instruction, newinst::NewInstruction,
    newline::Int32=newinst.line::Int32, newflag::UInt8=newinst.flag::UInt8)
    node[:inst] = newinst.stmt
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

function insert_node_here!(compact::IncrementalCompact, newinst::NewInstruction, reverse_affinity::Bool=false)
    newline = newinst.line::Int32
    refinish = false
    result_idx = compact.result_idx
    if reverse_affinity &&
            ((compact.active_result_bb == length(compact.result_bbs) + 1) ||
             result_idx == first(compact.result_bbs[compact.active_result_bb].stmts))
        compact.active_result_bb -= 1
        refinish = true
    end
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

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::SSAValue)
    @assert idx.id < compact.result_idx
    (compact.result[idx.id][:inst] === v) && return
    # Kill count for current uses
    kill_current_uses!(compact, compact.result[idx.id][:inst])
    compact.result[idx.id][:inst] = v
    # Add count for new use
    count_added_node!(compact, v) && push!(compact.late_fixup, idx.id)
    return compact
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::OldSSAValue)
    id = idx.id
    if id < compact.idx
        new_idx = compact.ssa_rename[id]
        (compact.result[new_idx][:inst] === v) && return
        kill_current_uses!(compact, compact.result[new_idx][:inst])
        compact.result[new_idx][:inst] = v
        count_added_node!(compact, v) && push!(compact.late_fixup, new_idx)
        return compact
    elseif id <= length(compact.ir.stmts)  # ir.stmts, new_nodes, and pending_nodes uses aren't counted yet, so no need to adjust
        compact.ir.stmts[id][:inst] = v
        return compact
    end
    id -= length(compact.ir.stmts)
    if id <= length(compact.ir.new_nodes)
        compact.ir.new_nodes.stmts[id][:inst] = v
        return compact
    end
    id -= length(compact.ir.new_nodes)
    compact.pending_nodes.stmts[id][:inst] = v
    return compact
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::Int)
    if idx < compact.result_idx
        compact[SSAValue(idx)] = v
    else
        compact.ir.stmts[idx][:inst] = v
    end
    return compact
end

__set_check_ssa_counts(onoff::Bool) = __check_ssa_counts__[] = onoff
const __check_ssa_counts__ = fill(false)

should_check_ssa_counts() = __check_ssa_counts__[]

# specifically meant to be used with body1 = compact.result and body2 = compact.new_new_nodes, with nvals == length(compact.used_ssas)
function find_ssavalue_uses1(compact)
    body1, body2 = compact.result.inst, compact.new_new_nodes.stmts.inst
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

function process_phinode_values(old_values::Vector{Any}, late_fixup::Vector{Int},
                                processed_idx::Int, result_idx::Int,
                                ssa_rename::Vector{Any}, used_ssas::Vector{Int},
                                new_new_used_ssas::Vector{Int},
                                do_rename_ssa::Bool)
    values = Vector{Any}(undef, length(old_values))
    for i = 1:length(old_values)
        isassigned(old_values, i) || continue
        val = old_values[i]
        if isa(val, SSAValue)
            if do_rename_ssa
                if val.id > processed_idx
                    push!(late_fixup, result_idx)
                    val = OldSSAValue(val.id)
                else
                    val = renumber_ssa2(val, ssa_rename, used_ssas, new_new_used_ssas, do_rename_ssa)
                end
            else
                used_ssas[val.id] += 1
            end
        elseif isa(val, OldSSAValue)
            if val.id > processed_idx
                push!(late_fixup, result_idx)
            else
                # Always renumber these. do_rename_ssa applies only to actual SSAValues
                val = renumber_ssa2(SSAValue(val.id), ssa_rename, used_ssas, new_new_used_ssas, true)
            end
        elseif isa(val, NewSSAValue)
            if val.id < 0
                push!(late_fixup, result_idx)
                new_new_used_ssas[-val.id] += 1
            else
                @assert do_rename_ssa
                val = SSAValue(val.id)
            end
        end
        values[i] = val
    end
    return values
end

function renumber_ssa2(val::SSAValue, ssanums::Vector{Any}, used_ssas::Vector{Int},
        new_new_used_ssas::Vector{Int}, do_rename_ssa::Bool)
    id = val.id
    if id > length(ssanums)
        return val
    end
    if do_rename_ssa
        val = ssanums[id]
    end
    if isa(val, SSAValue)
        used_ssas[val.id] += 1
    end
    return val
end

function renumber_ssa2(val::NewSSAValue, ssanums::Vector{Any}, used_ssas::Vector{Int},
        new_new_used_ssas::Vector{Int}, do_rename_ssa::Bool)
    if val.id < 0
        new_new_used_ssas[-val.id] += 1
        return val
    else
        used_ssas[val.id] += 1
        return SSAValue(val.id)
    end
end

function renumber_ssa2!(@nospecialize(stmt), ssanums::Vector{Any}, used_ssas::Vector{Int}, new_new_used_ssas::Vector{Int}, late_fixup::Vector{Int}, result_idx::Int, do_rename_ssa::Bool)
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, OldSSAValue) || isa(val, NewSSAValue)
            push!(late_fixup, result_idx)
        end
        if isa(val, Union{SSAValue, NewSSAValue})
            val = renumber_ssa2(val, ssanums, used_ssas, new_new_used_ssas, do_rename_ssa)
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
    deleteat!(preds, findfirst(x->x === from, preds)::Int)
    deleteat!(succs, findfirst(x->x === to, succs)::Int)
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
    # Note: We recursively kill as many edges as are obviously dead. However, this
    # may leave dead loops in the IR. We kill these later in a CFG cleanup pass (or
    # worstcase during codegen).
    preds = compact.result_bbs[compact.bb_rename_succ[to]].preds
    succs = compact.result_bbs[compact.bb_rename_pred[from]].succs
    deleteat!(preds, findfirst(x->x === compact.bb_rename_pred[from], preds)::Int)
    deleteat!(succs, findfirst(x->x === compact.bb_rename_succ[to], succs)::Int)
    # Check if the block is now dead
    if length(preds) == 0
        for succ in copy(compact.result_bbs[compact.bb_rename_succ[to]].succs)
            kill_edge!(compact, active_bb, to, findfirst(x->x === succ, compact.bb_rename_pred)::Int)
        end
        if to < active_bb
            # Kill all statements in the block
            stmts = compact.result_bbs[compact.bb_rename_succ[to]].stmts
            for stmt in stmts
                compact.result[stmt][:inst] = nothing
            end
            compact.result[last(stmts)][:inst] = ReturnNode()
        end
    else
        # Remove this edge from all phi nodes in `to` block
        # NOTE: It is possible for `to` to contain only `nothing` statements,
        #       so we must be careful to stop at its last statement
        if to < active_bb
            stmts = compact.result_bbs[compact.bb_rename_succ[to]].stmts
            idx = first(stmts)
            while idx <= last(stmts)
                stmt = compact.result[idx][:inst]
                stmt === nothing && continue
                isa(stmt, PhiNode) || break
                i = findfirst(x-> x == compact.bb_rename_pred[from], stmt.edges)
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
                i = findfirst(x-> x == from, stmt.edges)
                if i !== nothing
                    deleteat!(stmt.edges, i)
                    deleteat!(stmt.values, i)
                end
            end
        end
    end
    nothing
end

function process_node!(compact::IncrementalCompact, result_idx::Int, inst::Instruction, idx::Int, processed_idx::Int, active_bb::Int, do_rename_ssa::Bool)
    stmt = inst[:inst]
    (; result, ssa_rename, late_fixup, used_ssas, new_new_used_ssas, cfg_transforms_enabled, fold_constant_branches) = compact
    ssa_rename[idx] = SSAValue(result_idx)
    if stmt === nothing
        ssa_rename[idx] = stmt
    elseif isa(stmt, OldSSAValue)
        ssa_rename[idx] = ssa_rename[stmt.id]
    elseif isa(stmt, GotoNode) && cfg_transforms_enabled
        label = compact.bb_rename_succ[stmt.label]
        @assert label > 0
        result[result_idx][:inst] = GotoNode(label)
        result_idx += 1
    elseif isa(stmt, GlobalRef)
        total_flags = IR_FLAG_CONSISTENT | IR_FLAG_EFFECT_FREE
        flag = result[result_idx][:flag]
        if (flag & total_flags) == total_flags
            ssa_rename[idx] = stmt
        else
            result[result_idx][:inst] = stmt
            result[result_idx][:type] = argextype(stmt, compact)
            result[result_idx][:flag] = flag
            result_idx += 1
        end
    elseif isa(stmt, GotoNode)
        result[result_idx][:inst] = stmt
        result_idx += 1
    elseif isa(stmt, GotoIfNot) && cfg_transforms_enabled
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa)::GotoIfNot
        result[result_idx][:inst] = stmt
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
                result[result_idx][:inst] = nothing
                kill_edge!(compact, active_bb, active_bb, stmt.dest)
                # Don't increment result_idx => Drop this statement
            else
                label = compact.bb_rename_succ[stmt.dest]
                @assert label > 0
                result[result_idx][:inst] = GotoNode(label)
                kill_edge!(compact, active_bb, active_bb, active_bb+1)
                result_idx += 1
            end
        else
            @label bail
            label = compact.bb_rename_succ[stmt.dest]
            @assert label > 0
            result[result_idx][:inst] = GotoIfNot(cond, label)
            result_idx += 1
        end
    elseif isa(stmt, Expr)
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa)::Expr
        if cfg_transforms_enabled && isexpr(stmt, :enter)
            label = compact.bb_rename_succ[stmt.args[1]::Int]
            @assert label > 0
            stmt.args[1] = label
        elseif isexpr(stmt, :throw_undef_if_not)
            cond = stmt.args[2]
            if isa(cond, Bool) && cond === true
                # cond was folded to true - this statement
                # is dead.
                return result_idx
            end
        end
        result[result_idx][:inst] = stmt
        result_idx += 1
    elseif isa(stmt, PiNode)
        # As an optimization, we eliminate any trivial pinodes. For performance, we use ===
        # type equality. We may want to consider using == in either a separate pass or if
        # performance turns out ok
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa)::PiNode
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
            valtyp = isa(pi_val, QuoteNode) ? typeof(pi_val.value) : typeof(pi_val)
            if valtyp === stmt.typ
                ssa_rename[idx] = pi_val
                return result_idx
            end
        end
        result[result_idx][:inst] = stmt
        result_idx += 1
    elseif isa(stmt, ReturnNode) || isa(stmt, UpsilonNode) || isa(stmt, GotoIfNot)
        result[result_idx][:inst] = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa)
        result_idx += 1
    elseif isa(stmt, PhiNode)
        if cfg_transforms_enabled
            # Rename phi node edges
            map!(i -> compact.bb_rename_pred[i], stmt.edges, stmt.edges)

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

        values = process_phinode_values(values, late_fixup, processed_idx, result_idx, ssa_rename, used_ssas, new_new_used_ssas, do_rename_ssa)
        # Don't remove the phi node if it is before the definition of its value
        # because doing so can create forward references. This should only
        # happen with dead loops, but can cause problems when optimization
        # passes look at all code, dead or not. This check should be
        # unnecessary when DCE can remove those dead loops entirely, so this is
        # just to be safe.
        before_def = isassigned(values, 1) && (v = values[1]; isa(v, OldSSAValue)) && idx < v.id
        if length(edges) == 1 && isassigned(values, 1) && !before_def &&
                length(cfg_transforms_enabled ?
                    compact.result_bbs[compact.bb_rename_succ[active_bb]].preds :
                    compact.ir.cfg.blocks[active_bb].preds) == 1
            # There's only one predecessor left - just replace it
            v = values[1]
            @assert !isa(v, NewSSAValue)
            if isa(v, SSAValue)
                used_ssas[v.id] -= 1
            end
            ssa_rename[idx] = v
        else
            result[result_idx][:inst] = PhiNode(edges, values)
            result_idx += 1
        end
    elseif isa(stmt, PhiCNode)
        result[result_idx][:inst] = PhiCNode(process_phinode_values(stmt.values, late_fixup, processed_idx, result_idx, ssa_rename, used_ssas, new_new_used_ssas, do_rename_ssa))
        result_idx += 1
    elseif isa(stmt, SSAValue)
        # identity assign, replace uses of this ssa value with its result
        if do_rename_ssa
            stmt = ssa_rename[stmt.id]
        end
        ssa_rename[idx] = stmt
    elseif isa(stmt, NewSSAValue)
        ssa_rename[idx] = SSAValue(stmt.id)
    else
        # Constant assign, replace uses of this ssa value with its result
        ssa_rename[idx] = stmt
    end
    return result_idx
end

function resize!(compact::IncrementalCompact, nnewnodes)
    old_length = length(compact.result)
    resize!(compact.result, nnewnodes)
    resize!(compact.used_ssas, nnewnodes)
    for i in (old_length + 1):nnewnodes
        compact.used_ssas[i] = 0
    end
    return compact
end

function finish_current_bb!(compact::IncrementalCompact, active_bb, old_result_idx=compact.result_idx, unreachable=false)
    if compact.active_result_bb > length(compact.result_bbs)
        #@assert compact.bb_rename[active_bb] == -1
        return true
    end
    bb = compact.result_bbs[compact.active_result_bb]
    # If this was the last statement in the BB and we decided to skip it, insert a
    # dummy `nothing` node, to prevent changing the structure of the CFG
    skipped = false
    if !compact.cfg_transforms_enabled || active_bb == 0 || active_bb > length(compact.bb_rename_succ) || compact.bb_rename_succ[active_bb] != -1
        if compact.result_idx == first(bb.stmts)
            length(compact.result) < old_result_idx && resize!(compact, old_result_idx)
            node = compact.result[old_result_idx]
            if unreachable
                node[:inst], node[:type], node[:line] = ReturnNode(), Union{}, 0
            else
                node[:inst], node[:type], node[:line] = nothing, Nothing, 0
            end
            compact.result_idx = old_result_idx + 1
        elseif compact.cfg_transforms_enabled && compact.result_idx - 1 == first(bb.stmts)
            # Optimization: If this BB consists of only a branch, eliminate this bb
        end
        compact.result_bbs[compact.active_result_bb] = BasicBlock(bb, StmtRange(first(bb.stmts), compact.result_idx-1))
        compact.active_result_bb += 1
    else
        skipped = true
    end
    if compact.active_result_bb <= length(compact.result_bbs)
        new_bb = compact.result_bbs[compact.active_result_bb]
        compact.result_bbs[compact.active_result_bb] = BasicBlock(new_bb,
            StmtRange(compact.result_idx, last(new_bb.stmts)))
    end
    return skipped
end

function attach_after_stmt_after(compact::IncrementalCompact, idx::Int)
    compact.new_nodes_idx > length(compact.perm) && return false
    entry = compact.ir.new_nodes.info[compact.perm[compact.new_nodes_idx]]
    return entry.pos == idx && entry.attach_after
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
    if new_node_info.attach_after && idx == last(bb.stmts)+1 && !attach_after_stmt_after(compact, idx-1)
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
                return (entry[:inst], (idx, eidx+1, bidx))
            end
        end
    end
    if !isempty(compact.pending_perm)
        for eidx in bidx:length(compact.pending_perm)
            if entry_at_idx(compact.pending_nodes.info[compact.pending_perm[eidx]], idx)
                entry = compact.pending_nodes.stmts[compact.pending_perm[eidx]]
                return (entry[:inst], (idx, aidx, eidx+1))
            end
        end
    end
    idx > length(compact.ir.stmts) && return nothing
    return (compact.ir.stmts[idx][:inst], (idx + 1, aidx, bidx))
end

# the returned Union{Nothing, Pair{Pair{Int,Int},Any}} cannot be stack allocated,
# so we inline this function into the caller
@inline function iterate(compact::IncrementalCompact, state=nothing)
    idxs = iterate_compact(compact)
    idxs === nothing && return nothing
    old_result_idx = idxs[2]
    return Pair{Pair{Int,Int},Any}(idxs, compact.result[old_result_idx][:inst]), nothing
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
    if compact.cfg_transforms_enabled && active_bb > 1 && active_bb <= length(compact.bb_rename_succ) && compact.bb_rename_succ[active_bb] == -1
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
            heappop!(compact.pending_perm, By(x -> compact.pending_nodes.info[x].pos))
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
        new_idx = heappop!(compact.pending_perm, By(x -> compact.pending_nodes.info[x].pos))
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
    if idx == last(bb.stmts) && !attach_after_stmt_after(compact, idx)
        finish_current_bb!(compact, active_bb, old_result_idx)
        active_bb += 1
    end
    compact.idx = idx + 1
    compact.active_bb = active_bb
    if old_result_idx == compact.result_idx
        idx += 1
        @goto restart
    end
    @assert isassigned(compact.result.inst, old_result_idx)
    return Pair{Int,Int}(compact.idx-1, old_result_idx)
end

function maybe_erase_unused!(
    extra_worklist::Vector{Int}, compact::IncrementalCompact, idx::Int, in_worklist::Bool,
    callback = null_dce_callback)

    inst = idx <= length(compact.result) ? compact.result[idx] :
        compact.new_new_nodes.stmts[idx - length(compact.result)]
    stmt = inst[:inst]
    stmt === nothing && return false
    if inst[:type] === Bottom
        effect_free = false
    else
        effect_free = inst[:flag] & IR_FLAG_EFFECT_FREE != 0
    end
    function kill_ssa_value(val::SSAValue)
        if compact.used_ssas[val.id] == 1
            if val.id < idx || in_worklist
                push!(extra_worklist, val.id)
            end
        end
        compact.used_ssas[val.id] -= 1
        callback(val)
    end
    if effect_free
        foreachssa(kill_ssa_value, stmt)
        inst[:inst] = nothing
        return true
    end
    return false
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
        val = compact.ssa_rename[stmt.id]
        if isa(val, SSAValue)
            compact.used_ssas[val.id] += 1
        end
        return FixedNode(val, false)
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
        stmt = compact.result[idx][:inst]
        (;node, needs_fixup) = fixup_node(compact, stmt, late_fixup_offset === nothing)
        (stmt === node) || (compact.result[idx][:inst] = node)
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
        stmt = new_node[:inst]
        (;node) = fixup_node(compact, stmt, late_fixup_offset === nothing)
        if node !== stmt
            new_node[:inst] = node
        end
    end
end

function simple_dce!(compact::IncrementalCompact, callback = null_dce_callback)
    # Perform simple DCE for unused values
    @assert isempty(compact.new_new_used_ssas) # just_fixup! wasn't run?
    extra_worklist = Int[]
    for (idx, nused) in Iterators.enumerate(compact.used_ssas)
        nused == 0 || continue
        maybe_erase_unused!(extra_worklist, compact, idx, false, callback)
    end
    while !isempty(extra_worklist)
        maybe_erase_unused!(extra_worklist, compact, pop!(extra_worklist), true, callback)
    end
end

null_dce_callback(x::SSAValue) = return

function non_dce_finish!(compact::IncrementalCompact)
    result_idx = compact.result_idx
    resize!(compact.result, result_idx - 1)
    just_fixup!(compact)
    bb = compact.result_bbs[end]
    compact.result_bbs[end] = BasicBlock(bb,
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
    result_bbs = resize!(compact.result_bbs, compact.active_result_bb-1)
    cfg = CFG(result_bbs, Int[first(result_bbs[i].stmts) for i in 2:length(result_bbs)])
    if should_check_ssa_counts()
        oracle_check(compact)
    end

    # trim trailing undefined statements due to copy propagation
    nundef = 0
    for i in length(compact.result):-1:1
        if isassigned(compact.result.inst, i)
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
