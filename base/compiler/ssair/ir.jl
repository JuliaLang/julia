# This file is a part of Julia. License is MIT: https://julialang.org/license

Core.PhiNode() = Core.PhiNode(Int32[], Any[])

isterminator(@nospecialize(stmt)) = isa(stmt, GotoNode) || isa(stmt, GotoIfNot) || isa(stmt, ReturnNode)

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
    deleteat!(preds, findfirst(x->x === from, preds)::Int)
    deleteat!(succs, findfirst(x->x === to, succs)::Int)
    nothing
end

function cfg_reindex!(cfg::CFG)
    resize!(cfg.index, length(cfg.blocks) - 1)
    for ibb in 2:length(cfg.blocks)
        cfg.index[ibb-1] = first(cfg.blocks[ibb].stmts)
    end
    return cfg
end

function block_for_inst(index::Vector{Int}, inst::Int)
    return searchsortedfirst(index, inst, lt=(<=))
end

function block_for_inst(index::Vector{BasicBlock}, inst::Int)
    return searchsortedfirst(index, BasicBlock(StmtRange(inst, inst)), by=x->first(x.stmts), lt=(<=))-1
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
function first_insert_for_bb(code, cfg::CFG, block::Int)
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

# SSA values that are in `new_new_nodes` of an `IncrementalCompact` and are to
# be actually inserted next time (they become `new_nodes` next time)
struct NewSSAValue
    id::Int
end

const AnySSAValue = Union{SSAValue, OldSSAValue, NewSSAValue}


# SSA-indexed nodes

struct NewInstruction
    stmt::Any
    type::Any
    info::Any
    # If nothing, copy the line from previous statement
    # in the insertion location
    line::Union{Int32, Nothing}
    flag::UInt8

    ## Insertion options

    # The IR_FLAG_EFFECT_FREE flag has already been computed (or forced).
    # Don't bother redoing so on insertion.
    effect_free_computed::Bool
    NewInstruction(@nospecialize(stmt), @nospecialize(type), @nospecialize(info),
            line::Union{Int32, Nothing}, flag::UInt8, effect_free_computed::Bool) =
        new(stmt, type, info, line, flag, effect_free_computed)
end
NewInstruction(@nospecialize(stmt), @nospecialize(type)) =
    NewInstruction(stmt, type, nothing)
NewInstruction(@nospecialize(stmt), @nospecialize(type), line::Union{Nothing, Int32}) =
    NewInstruction(stmt, type, nothing, line, IR_FLAG_NULL, false)

effect_free(inst::NewInstruction) =
    NewInstruction(inst.stmt, inst.type, inst.info, inst.line, inst.flag | IR_FLAG_EFFECT_FREE, true)
non_effect_free(inst::NewInstruction) =
    NewInstruction(inst.stmt, inst.type, inst.info, inst.line, inst.flag & ~IR_FLAG_EFFECT_FREE, true)


struct InstructionStream
    inst::Vector{Any}
    type::Vector{Any}
    info::Vector{Any}
    line::Vector{Int32}
    flag::Vector{UInt8}
end
function InstructionStream(len::Int)
    insts = Array{Any}(undef, len)
    types = Array{Any}(undef, len)
    info = Array{Any}(undef, len)
    fill!(info, nothing)
    lines = fill(Int32(0), len)
    flags = fill(IR_FLAG_NULL, len)
    return InstructionStream(insts, types, info, lines, flags)
end
InstructionStream() = InstructionStream(0)
length(is::InstructionStream) = length(is.inst)
isempty(is::InstructionStream) = isempty(is.inst)
function add!(is::InstructionStream)
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
        stmts.info[i] = nothing
    end
    return stmts
end
iterate(is::InstructionStream, st::Int=1) = (st <= length(is)) ? (is[st], st + 1) : nothing

struct Instruction
    data::InstructionStream
    idx::Int
end
Instruction(is::InstructionStream) = Instruction(is, add!(is))

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
function setindex!(is::InstructionStream, newval::AnySSAValue, idx::Int)
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
function add!(new::NewNodeStream, pos::Int, attach_after::Bool)
    push!(new.info, NewNodeInfo(pos, attach_after))
    return Instruction(new.stmts)
end
copy(nns::NewNodeStream) = NewNodeStream(copy(nns.stmts), copy(nns.info))

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
    global copy
    copy(ir::IRCode) = new(copy(ir.stmts), copy(ir.argtypes), copy(ir.sptypes),
        copy(ir.linetable), copy(ir.cfg), copy(ir.new_nodes), copy(ir.meta))
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

function setindex!(x::IRCode, repl::Union{Instruction, AnySSAValue}, s::SSAValue)
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
    elseif isa(stmt, Union{SSAValue, NewSSAValue})
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
    elseif isa(stmt, Union{SSAValue, NewSSAValue})
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
scan_ssa_use!(push!, used, @nospecialize(stmt)) = foreachssa(ssa -> push!(used, ssa.id), stmt)

# Manually specialized copy of the above with push! === Compiler.push!
scan_ssa_use!(used::IdSet, @nospecialize(stmt)) = foreachssa(ssa -> push!(used, ssa.id), stmt)

function insert_node!(ir::IRCode, pos::Int, inst::NewInstruction, attach_after::Bool=false)
    node = add!(ir.new_nodes, pos, attach_after)
    node[:line] = something(inst.line, ir.stmts[pos][:line])
    flag = inst.flag
    if !inst.effect_free_computed
        if stmt_effect_free(inst.stmt, inst.type, ir)
            flag |= IR_FLAG_EFFECT_FREE
        end
    end
    node[:inst], node[:type], node[:flag] = inst.stmt, inst.type, flag
    return SSAValue(length(ir.stmts) + node.idx)
end

# For bootstrapping
function my_sortperm(v)
    p = Vector{Int}(undef, length(v))
    for i = 1:length(v)
        p[i] = i
    end
    sort!(p, Sort.DEFAULT_UNSTABLE, Order.Perm(Sort.Forward,v))
    p
end

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
    # TODO: Switch these two to a min-heap of some sort
    pending_nodes::NewNodeStream  # New nodes that were after the compaction point at insertion time
    pending_perm::Vector{Int}

    # State
    idx::Int
    result_idx::Int
    active_result_bb::Int
    renamed_new_nodes::Bool
    cfg_transforms_enabled::Bool
    fold_constant_branches::Bool

    function IncrementalCompact(code::IRCode, allow_cfg_transforms::Bool=false)
        # Sort by position with attach after nodes after regular ones
        perm = my_sortperm(Int[let new_node = code.new_nodes.info[i]
            (new_node.pos * 2 + Int(new_node.attach_after))
            end for i in 1:length(code.new_nodes)])
        new_len = length(code.stmts) + length(code.new_nodes)
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
            1, 1, 1, false, allow_cfg_transforms, allow_cfg_transforms)
    end

    # For inlining
    function IncrementalCompact(parent::IncrementalCompact, code::IRCode, result_offset)
        perm = my_sortperm(Int[code.new_nodes.info[i].pos for i in 1:length(code.new_nodes)])
        new_len = length(code.stmts) + length(code.new_nodes)
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        new_new_used_ssas = Vector{Int}()
        late_fixup = Vector{Int}()
        bb_rename = Vector{Int}()
        new_new_nodes = NewNodeStream()
        pending_nodes = NewNodeStream()
        pending_perm = Int[]
        return new(code, parent.result,
            parent.result_bbs, ssa_rename, bb_rename, bb_rename, parent.used_ssas,
            late_fixup, perm, 1,
            new_new_nodes, new_new_used_ssas, pending_nodes, pending_perm,
            1, result_offset, parent.active_result_bb, false, false, false)
    end
end

struct TypesView{T}
    ir::T # ::Union{IRCode, IncrementalCompact}
end
types(ir::Union{IRCode, IncrementalCompact}) = TypesView(ir)

# TODO We can be a bit better about access here by using a pattern similar to InstructionStream
function getindex(compact::IncrementalCompact, idx::Int)
    if idx < compact.result_idx
        return compact.result[idx][:inst]
    else
        return compact.ir.stmts[idx][:inst]
    end
end

function getindex(compact::IncrementalCompact, ssa::SSAValue)
    @assert ssa.id < compact.result_idx
    return compact.result[ssa.id][:inst]
end

function getindex(compact::IncrementalCompact, ssa::OldSSAValue)
    id = ssa.id
    if id < compact.idx
        new_idx = compact.ssa_rename[id]
        return compact.result[new_idx][:inst]
    elseif id <= length(compact.ir.stmts)
        return compact.ir.stmts[id][:inst]
    end
    id -= length(compact.ir.stmts)
    if id <= length(compact.ir.new_nodes)
        return compact.ir.new_nodes.stmts[id][:inst]
    end
    id -= length(compact.ir.new_nodes)
    return compact.pending_nodes.stmts[id][:inst]
end

function getindex(compact::IncrementalCompact, ssa::NewSSAValue)
    return compact.new_new_nodes.stmts[ssa.id][:inst]
end

function block_for_inst(compact::IncrementalCompact, idx::SSAValue)
    id = idx.id
    if id < compact.result_idx # if ssa within result
        return block_for_inst(compact.result_bbs, id)
    else
        return block_for_inst(compact.ir.cfg, id)
    end
end

function block_for_inst(compact::IncrementalCompact, idx::OldSSAValue)
    id = idx.id
    if id < compact.idx # if ssa within result
        return block_for_inst(compact.result_bbs, compact.ssa_rename[id])
    else
        return block_for_inst(compact.ir.cfg, id)
    end
end

function block_for_inst(compact::IncrementalCompact, idx::NewSSAValue)
    block_for_inst(compact, SSAValue(compact.new_new_nodes.info[idx.id].pos))
end

function dominates_ssa(compact::IncrementalCompact, domtree::DomTree, x::AnySSAValue, y::AnySSAValue)
    xb = block_for_inst(compact, x)
    yb = block_for_inst(compact, y)
    if xb == yb
        xinfo = yinfo = nothing
        if isa(x, OldSSAValue)
            x′ = compact.ssa_rename[x.id]::SSAValue
        elseif isa(x, NewSSAValue)
            xinfo = compact.new_new_nodes.info[x.id]
            x′ = SSAValue(xinfo.pos)
        else
            x′ = x
        end
        if isa(y, OldSSAValue)
            y′ = compact.ssa_rename[y.id]::SSAValue
        elseif isa(y, NewSSAValue)
            yinfo = compact.new_new_nodes.info[y.id]
            y′ = SSAValue(yinfo.pos)
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
                return yinfo.attach_after
            end
        end
        return x′.id < y′.id
    end
    return dominates(domtree, xb, yb)
end

function count_added_node!(compact::IncrementalCompact, @nospecialize(v))
    needs_late_fixup = false
    for ops in userefs(v)
        val = ops[]
        if isa(val, SSAValue)
            compact.used_ssas[val.id] += 1
        elseif isa(val, NewSSAValue)
            compact.new_new_used_ssas[val.id] += 1
            needs_late_fixup = true
        end
    end
    return needs_late_fixup
end

function add_pending!(compact::IncrementalCompact, pos::Int, attach_after::Bool)
    node = add!(compact.pending_nodes, pos, attach_after)
    # TODO: switch this to `l = length(pending_nodes); splice!(pending_perm, searchsorted(pending_perm, l), l)`
    push!(compact.pending_perm, length(compact.pending_nodes))
    sort!(compact.pending_perm, DEFAULT_STABLE, Order.By(x->compact.pending_nodes.info[x].pos, Order.Forward))
    return node
end

function insert_node!(compact::IncrementalCompact, before, inst::NewInstruction, attach_after::Bool=false)
    @assert inst.effect_free_computed
    if isa(before, SSAValue)
        if before.id < compact.result_idx
            count_added_node!(compact, inst.stmt)
            line = something(inst.line, compact.result[before.id][:line])
            node = add!(compact.new_new_nodes, before.id, attach_after)
            push!(compact.new_new_used_ssas, 0)
            node[:inst], node[:type], node[:line], node[:flag] = inst.stmt, inst.type, line, inst.flag
            return NewSSAValue(node.idx)
        else
            line = something(inst.line, compact.ir.stmts[before.id][:line])
            node = add_pending!(compact, before.id, attach_after)
            node[:inst], node[:type], node[:line], node[:flag] = inst.stmt, inst.type, line, inst.flag
            os = OldSSAValue(length(compact.ir.stmts) + length(compact.ir.new_nodes) + length(compact.pending_nodes))
            push!(compact.ssa_rename, os)
            push!(compact.used_ssas, 0)
            return os
        end
    elseif isa(before, OldSSAValue)
        pos = before.id
        if pos < compact.idx
            renamed = compact.ssa_rename[pos]::AnySSAValue
            count_added_node!(compact, inst.stmt)
            line = something(inst.line, compact.result[renamed.id][:line])
            node = add!(compact.new_new_nodes, renamed.id, attach_after)
            push!(compact.new_new_used_ssas, 0)
            node[:inst], node[:type], node[:line], node[:flag] = inst.stmt, inst.type, line, inst.flag
            return NewSSAValue(node.idx)
        else
            if pos > length(compact.ir.stmts)
                #@assert attach_after
                info = compact.pending_nodes.info[pos - length(compact.ir.stmts) - length(compact.ir.new_nodes)]
                pos, attach_after = info.pos, info.attach_after
            end
            line = something(inst.line, compact.ir.stmts[pos][:line])
            node = add_pending!(compact, pos, attach_after)
            node[:inst], node[:type], node[:line], node[:flag] = inst.stmt, inst.type, line, inst.flag
            os = OldSSAValue(length(compact.ir.stmts) + length(compact.ir.new_nodes) + length(compact.pending_nodes))
            push!(compact.ssa_rename, os)
            push!(compact.used_ssas, 0)
            return os
        end
    elseif isa(before, NewSSAValue)
        before_entry = compact.new_new_nodes.info[before.id]
        line = something(inst.line, compact.new_new_nodes.stmts[before.id][:line])
        new_entry = add!(compact.new_new_nodes, before_entry.pos, attach_after)
        new_entry[:inst], new_entry[:type], new_entry[:line], new_entry[:flag] = inst.stmt, inst.type, line, inst.flag
        push!(compact.new_new_used_ssas, 0)
        return NewSSAValue(new_entry.idx)
    else
        error("Unsupported")
    end
end

function insert_node_here!(compact::IncrementalCompact, inst::NewInstruction, reverse_affinity::Bool=false)
    @assert inst.line !== nothing
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
    flag = inst.flag
    if !inst.effect_free_computed && stmt_effect_free(inst.stmt, inst.type, compact)
        flag |= IR_FLAG_EFFECT_FREE
    end
    node = compact.result[result_idx]
    node[:inst], node[:type], node[:line], node[:flag] = inst.stmt, inst.type, inst.line, flag
    count_added_node!(compact, inst.stmt) && push!(compact.late_fixup, result_idx)
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

function kill_current_uses(compact::IncrementalCompact, @nospecialize(stmt))
    for ops in userefs(stmt)
        val = ops[]
        if isa(val, SSAValue)
            @assert compact.used_ssas[val.id] >= 1
            compact.used_ssas[val.id] -= 1
        elseif isa(val, NewSSAValue)
            @assert compact.new_new_used_ssas[val.id] >= 1
            compact.new_new_used_ssas[val.id] -= 1
        end
    end
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::SSAValue)
    @assert idx.id < compact.result_idx
    (compact.result[idx.id][:inst] === v) && return
    # Kill count for current uses
    kill_current_uses(compact, compact.result[idx.id][:inst])
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
        kill_current_uses(compact, compact.result[new_idx][:inst])
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

function _oracle_check(compact::IncrementalCompact)
    observed_used_ssas = Core.Compiler.find_ssavalue_uses1(compact)
    for i = 1:length(observed_used_ssas)
        if observed_used_ssas[i] != compact.used_ssas[i]
            return observed_used_ssas
        end
    end
    return nothing
end

function oracle_check(compact::IncrementalCompact)
    maybe_oracle_used_ssas = _oracle_check(compact)
    if maybe_oracle_used_ssas !== nothing
        @eval Main (compact = $compact; oracle_used_ssas = $maybe_oracle_used_ssas)
        error("Oracle check failed, inspect Main.compact and Main.oracle_used_ssas")
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
    if isa(view.ir, IncrementalCompact)
        return view.ir.new_new_nodes.stmts[idx.id][:type]
    else
        return view.ir.new_nodes.stmts[idx.id][:type]
    end
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
            push!(late_fixup, result_idx)
            new_new_used_ssas[val.id] += 1
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
    new_new_used_ssas[val.id] += 1
    return val
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
function kill_edge!(bbs::Vector{BasicBlock}, from::Int, to::Int)
    preds, succs = bbs[to].preds, bbs[from].succs
    deleteat!(preds, findfirst(x->x === from, preds)::Int)
    deleteat!(succs, findfirst(x->x === to, succs)::Int)
    if length(preds) == 0
        for succ in copy(bbs[to].succs)
            kill_edge!(bbs, to, succ)
        end
    end
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
        result[result_idx][:inst] = GotoNode(compact.bb_rename_succ[stmt.label])
        result_idx += 1
    elseif isa(stmt, GlobalRef)
        result[result_idx][:inst] = stmt
        result[result_idx][:type] = argextype(stmt, compact)
        result_idx += 1
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
                cond = condT.val
                isa(cond, Bool) || @goto bail
            end
            if cond
                result[result_idx][:inst] = nothing
                kill_edge!(compact, active_bb, active_bb, stmt.dest)
                # Don't increment result_idx => Drop this statement
            else
                result[result_idx][:inst] = GotoNode(compact.bb_rename_succ[stmt.dest])
                kill_edge!(compact, active_bb, active_bb, active_bb+1)
                result_idx += 1
            end
        else
            @label bail
            result[result_idx][:inst] = GotoIfNot(cond, compact.bb_rename_succ[stmt.dest])
            result_idx += 1
        end
    elseif isa(stmt, Expr)
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, new_new_used_ssas, late_fixup, result_idx, do_rename_ssa)::Expr
        if cfg_transforms_enabled && isexpr(stmt, :enter)
            stmt.args[1] = compact.bb_rename_succ[stmt.args[1]::Int]
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
                if stmt.edges[old_index] != -1
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
    (old_result_idx == result_idx) && return iterate(compact, (idx, active_bb))
    return Pair{Pair{Int, Int}, Any}(
        Pair{Int,Int}(new_idx,old_result_idx),
        compact.result[old_result_idx][:inst]), (idx, active_bb)
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

function iterate(compact::IncrementalCompact, (idx, active_bb)::Tuple{Int, Int}=(compact.idx, 1))
    # Create label to dodge recursion so that we don't stack overflow
    @label restart

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
            popfirst!(compact.pending_perm)
        end
        # Move to next block
        compact.idx += 1
        if finish_current_bb!(compact, active_bb, old_result_idx, true)
            return iterate(compact, (compact.idx, active_bb + 1))
        else
            return Pair{Pair{Int, Int}, Any}(Pair{Int,Int}(compact.idx-1, old_result_idx), compact.result[old_result_idx][:inst]), (compact.idx, active_bb + 1)
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
        return process_newnode!(compact, new_idx, new_node_entry, new_node_info, idx, active_bb, true)
    elseif !isempty(compact.pending_perm) &&
        (info = compact.pending_nodes.info[compact.pending_perm[1]];
         info.attach_after ? info.pos == idx - 1 : info.pos == idx)
        new_idx = popfirst!(compact.pending_perm)
        new_node_entry = compact.pending_nodes.stmts[new_idx]
        new_node_info = compact.pending_nodes.info[new_idx]
        new_idx += length(compact.ir.stmts) + length(compact.ir.new_nodes)
        return process_newnode!(compact, new_idx, new_node_entry, new_node_info, idx, active_bb, false)
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
    if old_result_idx == compact.result_idx
        idx += 1
        @goto restart
    end
    @assert isassigned(compact.result.inst, old_result_idx)
    return Pair{Pair{Int,Int}, Any}(Pair{Int,Int}(compact.idx-1, old_result_idx),
        compact.result[old_result_idx][:inst]), (compact.idx, active_bb)
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

function fixup_phinode_values!(compact::IncrementalCompact, old_values::Vector{Any})
    values = Vector{Any}(undef, length(old_values))
    for i = 1:length(old_values)
        isassigned(old_values, i) || continue
        val = old_values[i]
        if isa(val, Union{OldSSAValue, NewSSAValue})
            val = fixup_node(compact, val)
        end
        values[i] = val
    end
    values
end

function fixup_node(compact::IncrementalCompact, @nospecialize(stmt))
    if isa(stmt, PhiNode)
        return PhiNode(stmt.edges, fixup_phinode_values!(compact, stmt.values))
    elseif isa(stmt, PhiCNode)
        return PhiCNode(fixup_phinode_values!(compact, stmt.values))
    elseif isa(stmt, NewSSAValue)
        return SSAValue(length(compact.result) + stmt.id)
    elseif isa(stmt, OldSSAValue)
        val = compact.ssa_rename[stmt.id]
        if isa(val, SSAValue)
            # If `val.id` is greater than the length of `compact.result` or
            # `compact.used_ssas`, this SSA value is in `new_new_nodes`, so
            # don't count the use
            compact.used_ssas[val.id] += 1
        end
        return val
    else
        urs = userefs(stmt)
        for ur in urs
            val = ur[]
            if isa(val, Union{NewSSAValue, OldSSAValue})
                ur[] = fixup_node(compact, val)
            end
        end
        return urs[]
    end
end

function just_fixup!(compact::IncrementalCompact)
    resize!(compact.used_ssas, length(compact.result))
    append!(compact.used_ssas, compact.new_new_used_ssas)
    empty!(compact.new_new_used_ssas)
    for idx in compact.late_fixup
        stmt = compact.result[idx][:inst]
        new_stmt = fixup_node(compact, stmt)
        (stmt === new_stmt) || (compact.result[idx][:inst] = new_stmt)
    end
    for idx in 1:length(compact.new_new_nodes)
        node = compact.new_new_nodes.stmts[idx]
        stmt = node[:inst]
        new_stmt = fixup_node(compact, stmt)
        if new_stmt !== stmt
            node[:inst] = new_stmt
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
    if __check_ssa_counts__[]
        oracle_check(compact)
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

    allocate_stmts!(ir.stmts.inst, GotoNode(0))  # dummy
    allocate_stmts!(ir.stmts.type, Any)
    allocate_stmts!(ir.stmts.info, nothing)
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
        @assert ir.stmts.inst[last(b1.stmts)] === GotoNode(0)
        ir.stmts.inst[last(b1.stmts)] = GotoNode(ibb2)
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
