# This file is a part of Julia. License is MIT: https://julialang.org/license

@inline isexpr(@nospecialize(stmt), head::Symbol) = isa(stmt, Expr) && stmt.head === head
@eval Core.UpsilonNode() = $(Expr(:new, Core.UpsilonNode))
Core.PhiNode() = Core.PhiNode(Any[], Any[])

struct Argument
    n::Int
end

struct GotoIfNot
    cond::Any
    dest::Int
    GotoIfNot(@nospecialize(cond), dest::Int) = new(cond, dest)
end

struct ReturnNode
    val::Any
    ReturnNode(@nospecialize(val)) = new(val)
    # unassigned val indicates unreachable
    ReturnNode() = new()
end

"""
Like UnitRange{Int}, but can handle the `last` field, being temporarily
< first (this can happen during compacting)
"""
struct StmtRange <: AbstractUnitRange{Int}
    start::Int
    stop::Int
end
first(r::StmtRange) = r.start
last(r::StmtRange) = r.stop
iterate(r::StmtRange, state=0) = (last(r) - first(r) < state) ? nothing : (first(r) + state, state + 1)

StmtRange(range::UnitRange{Int}) = StmtRange(first(range), last(range))

struct BasicBlock
    stmts::StmtRange
    preds::Vector{Int}
    succs::Vector{Int}
end
function BasicBlock(stmts::StmtRange)
    return BasicBlock(stmts, Int[], Int[])
end
function BasicBlock(old_bb, stmts)
    return BasicBlock(stmts, old_bb.preds, old_bb.succs)
end
copy(bb::BasicBlock) = BasicBlock(bb.stmts, copy(bb.preds), copy(bb.succs))

struct CFG
    blocks::Vector{BasicBlock}
    index::Vector{Int} # map from instruction => basic-block number
                       # TODO: make this O(1) instead of O(log(n_blocks))?
end
copy(c::CFG) = CFG(BasicBlock[copy(b) for b in c.blocks], copy(c.index))

function block_for_inst(index::Vector{Int}, inst::Int)
    return searchsortedfirst(index, inst, lt=(<=))
end
block_for_inst(cfg::CFG, inst::Int) = block_for_inst(cfg.index, inst)

function basic_blocks_starts(stmts::Vector{Any})
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
            elseif stmt.head === :gotoifnot
                # also tolerate expr form of IR
                push!(jump_dests, idx+1)
                push!(jump_dests, stmt.args[2]::Int)
            elseif stmt.head === :return
                # also tolerate expr form of IR
                # This is a fake dest to force the next stmt to start a bb
                idx < length(stmts) && push!(jump_dests, idx+1)
            end
        end
        if isa(stmt, PhiNode)
            for edge in stmt.edges
                if edge === idx - 1
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
    bb_starts = basic_blocks_starts(stmts)
    # Compute ranges
    pop!(bb_starts, 1)
    basic_block_index = collect(bb_starts)
    blocks = BasicBlock[]
    sizehint!(blocks, length(basic_block_index))
    let first = 1
        for last in basic_block_index
            push!(blocks, BasicBlock(StmtRange(first, last - 1)))
            first = last
        end
    end
    # Compute successors/predecessors
    for (num, b) in enumerate(blocks)
        terminator = stmts[last(b.stmts)]
        if isa(terminator, ReturnNode) || isexpr(terminator, :return)
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
        elseif isa(terminator, Expr)
            if terminator.head === :enter
                # :enter gets a virtual edge to the exception handler and
                # the exception handler gets a virtual edge from outside
                # the function.
                # See the devdocs on exception handling in SSA form (or
                # bug Keno to write them, if you're reading this and they
                # don't exist)
                block′ = block_for_inst(basic_block_index, terminator.args[1]::Int)
                push!(blocks[block′].preds, num)
                push!(blocks[block′].preds, 0)
                push!(b.succs, block′)
            elseif terminator.head === :gotoifnot
                block′ = block_for_inst(basic_block_index, terminator.args[2]::Int)
                if block′ == num + 1
                    # This GotoIfNot acts like a noop - treat it as such.
                    # We will drop it during SSA renaming
                else
                    push!(blocks[block′].preds, num)
                    push!(b.succs, block′)
                end
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

function first_insert_for_bb(code, cfg::CFG, block::Int)
    for idx in cfg.blocks[block].stmts
        stmt = code[idx]
        if !isa(stmt, PhiNode)
            return idx
        end
    end
end

struct NewNode
    # Insertion position (interpretation depends on which array this is in)
    pos::Int
    # Place the new instruction after this instruction (but in the same BB if this is an implicit terminator)
    attach_after::Bool
    # The type of the instruction to insert
    typ::Any
    # The node itself
    node::Any
    # The index into the line number table of this entry
    line::Int32

    NewNode(pos::Int, attach_after::Bool, @nospecialize(typ), @nospecialize(node), line::Int32) =
        new(pos, attach_after, typ, node, line)
end

struct IRCode
    stmts::Vector{Any}
    types::Vector{Any}
    lines::Vector{Int32}
    flags::Vector{UInt8}
    argtypes::Vector{Any}
    sptypes::Vector{Any}
    linetable::Vector{LineInfoNode}
    cfg::CFG
    new_nodes::Vector{NewNode}
    meta::Vector{Any}

    function IRCode(stmts::Vector{Any}, types::Vector{Any}, lines::Vector{Int32}, flags::Vector{UInt8},
            cfg::CFG, linetable::Vector{LineInfoNode}, argtypes::Vector{Any}, meta::Vector{Any},
            sptypes::Vector{Any})
        return new(stmts, types, lines, flags, argtypes, sptypes, linetable, cfg, NewNode[], meta)
    end
    function IRCode(ir::IRCode, stmts::Vector{Any}, types::Vector{Any}, lines::Vector{Int32}, flags::Vector{UInt8},
            cfg::CFG, new_nodes::Vector{NewNode})
        return new(stmts, types, lines, flags, ir.argtypes, ir.sptypes, ir.linetable, cfg, new_nodes, ir.meta)
    end
end
copy(code::IRCode) = IRCode(code, copy_exprargs(code.stmts), copy(code.types),
    copy(code.lines), copy(code.flags), copy(code.cfg), copy(code.new_nodes))

function getindex(x::IRCode, s::SSAValue)
    if s.id <= length(x.stmts)
        return x.stmts[s.id]
    else
        return x.new_nodes[s.id - length(x.stmts)].node
    end
end

function setindex!(x::IRCode, @nospecialize(repl), s::SSAValue)
    @assert s.id <= length(x.stmts)
    x.stmts[s.id] = repl
    nothing
end


struct OldSSAValue
    id::Int
end

struct NewSSAValue
    id::Int
end

const AnySSAValue = Union{SSAValue, OldSSAValue, NewSSAValue}

mutable struct UseRef
    stmt::Any
    op::Int
    UseRef(@nospecialize(a)) = new(a, 0)
end
struct UseRefIterator
    use::Tuple{UseRef, Nothing}
    relevant::Bool
    UseRefIterator(@nospecialize(a), relevant::Bool) = new((UseRef(a), nothing), relevant)
end
getindex(it::UseRefIterator) = it.use[1].stmt

# TODO: stack-allocation
#struct UseRef
#    urs::UseRefIterator
#    use::Int
#end

struct OOBToken
end

struct UndefToken
end

function getindex(x::UseRef)
    stmt = x.stmt
    if isa(stmt, Expr) && stmt.head === :(=)
        rhs = stmt.args[2]
        if isa(rhs, Expr)
            if is_relevant_expr(rhs)
                x.op > length(rhs.args) && return OOBToken()
                return rhs.args[x.op]
            end
        end
        x.op == 1 || return OOBToken()
        return rhs
    elseif isa(stmt, Expr) # @assert is_relevant_expr(stmt)
        x.op > length(stmt.args) && return OOBToken()
        return stmt.args[x.op]
    elseif isa(stmt, GotoIfNot)
        x.op == 1 || return OOBToken()
        return stmt.cond
    elseif isa(stmt, ReturnNode)
        isdefined(stmt, :val) || return OOBToken()
        x.op == 1 || return OOBToken()
        return stmt.val
    elseif isa(stmt, PiNode)
        isdefined(stmt, :val) || return OOBToken()
        x.op == 1 || return OOBToken()
        return stmt.val
    elseif isa(stmt, UpsilonNode)
        isdefined(stmt, :val) || return OOBToken()
        x.op == 1 || return OOBToken()
        return stmt.val
    elseif isa(stmt, PhiNode)
        x.op > length(stmt.values) && return OOBToken()
        isassigned(stmt.values, x.op) || return UndefToken()
        return stmt.values[x.op]
    elseif isa(stmt, PhiCNode)
        x.op > length(stmt.values) && return OOBToken()
        isassigned(stmt.values, x.op) || return UndefToken()
        return stmt.values[x.op]
    else
        return OOBToken()
    end
end

function is_relevant_expr(e::Expr)
    return e.head in (:call, :invoke, :new, :splatnew, :(=), :(&),
                      :gc_preserve_begin, :gc_preserve_end,
                      :foreigncall, :isdefined, :copyast,
                      :undefcheck, :throw_undef_if_not,
                      :cfunction, :method, :pop_exception,
                      #=legacy IR format support=# :gotoifnot, :return)
end

function setindex!(x::UseRef, @nospecialize(v))
    stmt = x.stmt
    if isa(stmt, Expr) && stmt.head === :(=)
        rhs = stmt.args[2]
        if isa(rhs, Expr)
            if is_relevant_expr(rhs)
                x.op > length(rhs.args) && throw(BoundsError())
                rhs.args[x.op] = v
                return v
            end
        end
        x.op == 1 || throw(BoundsError())
        stmt.args[2] = v
    elseif isa(stmt, Expr) # @assert is_relevant_expr(stmt)
        x.op > length(stmt.args) && throw(BoundsError())
        stmt.args[x.op] = v
    elseif isa(stmt, GotoIfNot)
        x.op == 1 || throw(BoundsError())
        x.stmt = GotoIfNot(v, stmt.dest)
    elseif isa(stmt, ReturnNode)
        x.op == 1 || throw(BoundsError())
        x.stmt = typeof(stmt)(v)
    elseif isa(stmt, UpsilonNode)
        x.op == 1 || throw(BoundsError())
        x.stmt = typeof(stmt)(v)
    elseif isa(stmt, PiNode)
        x.op == 1 || throw(BoundsError())
        x.stmt = typeof(stmt)(v, stmt.typ)
    elseif isa(stmt, PhiNode)
        x.op > length(stmt.values) && throw(BoundsError())
        isassigned(stmt.values, x.op) || throw(BoundsError())
        stmt.values[x.op] = v
    elseif isa(stmt, PhiCNode)
        x.op > length(stmt.values) && throw(BoundsError())
        isassigned(stmt.values, x.op) || throw(BoundsError())
        stmt.values[x.op] = v
    else
        throw(BoundsError())
    end
    return x
end

function userefs(@nospecialize(x))
    relevant = (isa(x, Expr) && is_relevant_expr(x)) ||
        isa(x, GotoIfNot) || isa(x, ReturnNode) ||
        isa(x, PiNode) || isa(x, PhiNode) || isa(x, PhiCNode) || isa(x, UpsilonNode)
    return UseRefIterator(x, relevant)
end

iterate(it::UseRefIterator) = (it.use[1].op = 0; iterate(it, nothing))
@noinline function iterate(it::UseRefIterator, ::Nothing)
    it.relevant || return nothing
    use = it.use[1]
    while true
        use.op += 1
        y = use[]
        y === OOBToken() && return nothing
        y === UndefToken() || return it.use
    end
end

# This function is used from the show code, which may have a different
# `push!`/`used` type since it's in Base.
function scan_ssa_use!(push!, used, @nospecialize(stmt))
    if isa(stmt, SSAValue)
        push!(used, stmt.id)
    end
    for useref in userefs(stmt)
        val = useref[]
        if isa(val, SSAValue)
            push!(used, val.id)
        end
    end
end

# Manually specialized copy of the above with push! === Compiler.push!
function scan_ssa_use!(used::IdSet, @nospecialize(stmt))
    if isa(stmt, SSAValue)
        push!(used, stmt.id)
    end
    for useref in userefs(stmt)
        val = useref[]
        if isa(val, SSAValue)
            push!(used, val.id)
        end
    end
end

function ssamap(f, @nospecialize(stmt))
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, SSAValue)
            op[] = f(val)
        end
    end
    return urs[]
end

function foreachssa(f, @nospecialize(stmt))
    for op in userefs(stmt)
        val = op[]
        if isa(val, SSAValue)
            f(val)
        end
    end
end

function insert_node!(ir::IRCode, pos::Int, @nospecialize(typ), @nospecialize(val), attach_after::Bool=false)
    line = ir.lines[pos]
    push!(ir.new_nodes, NewNode(pos, attach_after, typ, val, line))
    return SSAValue(length(ir.stmts) + length(ir.new_nodes))
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
    result::Vector{Any}
    result_types::Vector{Any}
    result_lines::Vector{Int32}
    result_flags::Vector{UInt8}
    result_bbs::Vector{BasicBlock}
    ssa_rename::Vector{Any}
    bb_rename_pred::Vector{Int}
    bb_rename_succ::Vector{Int}
    used_ssas::Vector{Int}
    late_fixup::Vector{Int}
    # This could be Stateful, but bootstrapping doesn't like that
    perm::Vector{Int}
    new_nodes_idx::Int
    # This supports insertion while compacting
    new_new_nodes::Vector{NewNode}  # New nodes that were before the compaction point at insertion time
    # TODO: Switch these two to a min-heap of some sort
    pending_nodes::Vector{NewNode}  # New nodes that were after the compaction point at insertion time
    pending_perm::Vector{Int}
    # State
    idx::Int
    result_idx::Int
    active_result_bb::Int
    renamed_new_nodes::Bool
    cfg_transforms_enabled::Bool
    fold_constant_branches::Bool
    function IncrementalCompact(code::IRCode, allow_cfg_transforms::Bool=false)
        # Sort by position with attach after nodes affter regular ones
        perm = my_sortperm(Int[(code.new_nodes[i].pos*2 + Int(code.new_nodes[i].attach_after)) for i in 1:length(code.new_nodes)])
        new_len = length(code.stmts) + length(code.new_nodes)
        result = Array{Any}(undef, new_len)
        result_types = Array{Any}(undef, new_len)
        result_lines = fill(Int32(0), new_len)
        result_flags = fill(0x00, new_len)
        used_ssas = fill(0, new_len)
        blocks = code.cfg.blocks
        if allow_cfg_transforms
            bb_rename = Vector{Int}(undef, length(blocks))
            cur_bb = 1
            for i = 1:length(bb_rename)
                if i != 1 && length(blocks[i].preds) == 0
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
                for j = 1:length(succs); succs[j] = bb_rename[succs[j]]; end
            end
            let blocks=blocks
                result_bbs = BasicBlock[blocks[i] for i = 1:length(blocks) if bb_rename[i] != -1]
            end
        else
            bb_rename = Vector{Int}()
            result_bbs = code.cfg.blocks
        end
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        late_fixup = Vector{Int}()
        new_new_nodes = NewNode[]
        pending_nodes = NewNode[]
        pending_perm = Int[]
        return new(code, result, result_types, result_lines, result_flags, result_bbs, ssa_rename, bb_rename, bb_rename, used_ssas, late_fixup, perm, 1,
            new_new_nodes, pending_nodes, pending_perm,
            1, 1, 1, false, allow_cfg_transforms, allow_cfg_transforms)
    end

    # For inlining
    function IncrementalCompact(parent::IncrementalCompact, code::IRCode, result_offset)
        perm = my_sortperm(Int[code.new_nodes[i].pos for i in 1:length(code.new_nodes)])
        new_len = length(code.stmts) + length(code.new_nodes)
        ssa_rename = Any[SSAValue(i) for i = 1:new_len]
        used_ssas = fill(0, new_len)
        late_fixup = Vector{Int}()
        bb_rename = Vector{Int}()
        new_new_nodes = NewNode[]
        pending_nodes = NewNode[]
        pending_perm = Int[]
        return new(code, parent.result, parent.result_types, parent.result_lines, parent.result_flags,
            parent.result_bbs, ssa_rename, bb_rename, bb_rename, parent.used_ssas,
            late_fixup, perm, 1,
            new_new_nodes, pending_nodes, pending_perm,
            1, result_offset, parent.active_result_bb, false, false, false)
    end
end

struct TypesView
    ir::Union{IRCode, IncrementalCompact}
end
types(ir::Union{IRCode, IncrementalCompact}) = TypesView(ir)

function getindex(compact::IncrementalCompact, idx::Int)
    if idx < compact.result_idx
        return compact.result[idx]
    else
        return compact.ir.stmts[idx]
    end
end

function getindex(compact::IncrementalCompact, ssa::SSAValue)
    @assert ssa.id < compact.result_idx
    return compact.result[ssa.id]
end

function getindex(compact::IncrementalCompact, ssa::OldSSAValue)
    id = ssa.id
    if id <= length(compact.ir.stmts)
        return compact.ir.stmts[id]
    end
    id -= length(compact.ir.stmts)
    if id <= length(compact.ir.new_nodes)
        return compact.ir.new_nodes[id].node
    end
    id -= length(compact.ir.new_nodes)
    return compact.pending_nodes[id].node
end

function getindex(compact::IncrementalCompact, ssa::NewSSAValue)
    return compact.new_new_nodes[ssa.id].node
end

function count_added_node!(compact::IncrementalCompact, @nospecialize(v))
    needs_late_fixup = isa(v, NewSSAValue)
    if isa(v, SSAValue)
        compact.used_ssas[v.id] += 1
    else
        for ops in userefs(v)
            val = ops[]
            if isa(val, SSAValue)
                compact.used_ssas[val.id] += 1
            elseif isa(val, NewSSAValue)
                needs_late_fixup = true
            end
        end
    end
    needs_late_fixup
end

function resort_pending!(compact)
    sort!(compact.pending_perm, DEFAULT_STABLE, Order.By(x->compact.pending_nodes[x].pos, Order.Forward))
end

function insert_node!(compact::IncrementalCompact, before, @nospecialize(typ), @nospecialize(val), attach_after::Bool=false)
    if isa(before, SSAValue)
        if before.id < compact.result_idx
            count_added_node!(compact, val)
            line = compact.result_lines[before.id]
            push!(compact.new_new_nodes, NewNode(before.id, attach_after, typ, val, line))
            return NewSSAValue(length(compact.new_new_nodes))
        else
            line = compact.ir.lines[before.id]
            push!(compact.pending_nodes, NewNode(before.id, attach_after, typ, val, line))
            push!(compact.pending_perm, length(compact.pending_nodes))
            resort_pending!(compact)
            os = OldSSAValue(length(compact.ir.stmts) + length(compact.ir.new_nodes) + length(compact.pending_nodes))
            push!(compact.ssa_rename, os)
            push!(compact.used_ssas, 0)
            return os
        end
    elseif isa(before, OldSSAValue)
        pos = before.id
        if pos > length(compact.ir.stmts)
            #@assert attach_after
            entry = compact.pending_nodes[pos - length(compact.ir.stmts) - length(compact.ir.new_nodes)]
            pos, attach_after = entry.pos, entry.attach_after
        end
        line = compact.ir.lines[pos]
        push!(compact.pending_nodes, NewNode(pos, attach_after, typ, val, line))
        push!(compact.pending_perm, length(compact.pending_nodes))
        resort_pending!(compact)
        os = OldSSAValue(length(compact.ir.stmts) + length(compact.ir.new_nodes) + length(compact.pending_nodes))
        push!(compact.ssa_rename, os)
        push!(compact.used_ssas, 0)
        return os
    elseif isa(before, NewSSAValue)
        before_entry = compact.new_new_nodes[before.id]
        push!(compact.new_new_nodes, NewNode(before_entry.pos, attach_after, typ, val, before_entry.line))
        return NewSSAValue(length(compact.new_new_nodes))
    else
        error("Unsupported")
    end
end

function append_node!(ir, @nospecialize(typ), @nospecialize(node), line)
    push!(ir.stmts, node)
    push!(ir.types, typ)
    push!(ir.lines, line)
    push!(ir.flags, 0)
    last_bb = ir.cfg.blocks[end]
    ir.cfg.blocks[end] = BasicBlock(first(last_bb.stmts):length(ir.stmts),
        last_bb.preds,
        last_bb.succs)
    return SSAValue(length(ir.stmts))
end

function insert_node_here!(compact::IncrementalCompact, @nospecialize(val), @nospecialize(typ), ltable_idx::Int32, reverse_affinity::Bool=false)
    if compact.result_idx > length(compact.result)
        @assert compact.result_idx == length(compact.result) + 1
        resize!(compact, compact.result_idx)
    end
    refinish = false
    if compact.result_idx == first(compact.result_bbs[compact.active_result_bb].stmts) && reverse_affinity
        compact.active_result_bb -= 1
        refinish = true
    end
    compact.result[compact.result_idx] = val
    compact.result_types[compact.result_idx] = typ
    compact.result_lines[compact.result_idx] = ltable_idx
    compact.result_flags[compact.result_idx] = 0x00
    if count_added_node!(compact, val)
        push!(compact.late_fixup, compact.result_idx)
    end
    ret = SSAValue(compact.result_idx)
    compact.result_idx += 1
    refinish && finish_current_bb!(compact, 0)
    ret
end

function getindex(view::TypesView, v::OldSSAValue)
    id = v.id
    if id <= length(view.ir.ir.types)
        return view.ir.ir.types[id]
    end
    id -= length(view.ir.ir.types)
    if id <= length(view.ir.ir.new_nodes)
        return view.ir.ir.new_nodes[id].typ
    end
    id -= length(view.ir.ir.new_nodes)
    return view.ir.pending_nodes[id].typ
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::SSAValue)
    @assert idx.id < compact.result_idx
    (compact.result[idx.id] === v) && return
    # Kill count for current uses
    for ops in userefs(compact.result[idx.id])
        val = ops[]
        if isa(val, SSAValue)
            @assert compact.used_ssas[val.id] >= 1
            compact.used_ssas[val.id] -= 1
        end
    end
    compact.result[idx.id] = v
    # Add count for new use
    if count_added_node!(compact, v)
        push!(compact.late_fixup, idx.id)
    end
end

function setindex!(compact::IncrementalCompact, @nospecialize(v), idx::Int)
    if idx < compact.result_idx
        compact[SSAValue(idx)] = v
    else
        compact.ir.stmts[idx] = v
    end
    return nothing
end

function getindex(view::TypesView, idx)
    isa(idx, SSAValue) && (idx = idx.id)
    if isa(view.ir, IncrementalCompact) && idx < view.ir.result_idx
        return view.ir.result_types[idx]
    elseif isa(view.ir, IncrementalCompact) && view.ir.renamed_new_nodes
        if idx <= length(view.ir.result_types)
            return view.ir.result_types[idx]
        else
            return view.ir.new_new_nodes[idx - length(view.ir.result_types)].typ
        end
    else
        ir = isa(view.ir, IncrementalCompact) ? view.ir.ir : view.ir
        if idx <= length(ir.types)
            return ir.types[idx]
        else
            return ir.new_nodes[idx - length(ir.types)].typ
        end
    end
end

function getindex(view::TypesView, idx::NewSSAValue)
    if isa(view.ir, IncrementalCompact)
        compact = view.ir
        compact.new_new_nodes[idx.id].typ
    else
        view.ir.new_nodes[idx.id].typ
    end
end

function process_phinode_values(old_values::Vector{Any}, late_fixup::Vector{Int},
                                processed_idx::Int, result_idx::Int,
                                ssa_rename::Vector{Any}, used_ssas::Vector{Int},
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
                    val = renumber_ssa2(val, ssa_rename, used_ssas, do_rename_ssa)
                end
            else
                used_ssas[val.id] += 1
            end
        elseif isa(val, OldSSAValue)
            if val.id > processed_idx
                push!(late_fixup, result_idx)
            else
                # Always renumber these. do_rename_ssa applies only to actual SSAValues
                val = renumber_ssa2(SSAValue(val.id), ssa_rename, used_ssas, true)
            end
        elseif isa(val, NewSSAValue)
            push!(late_fixup, result_idx)
        end
        values[i] = val
    end
    return values
end

function renumber_ssa2(val::SSAValue, ssanums::Vector{Any}, used_ssa::Vector{Int}, do_rename_ssa::Bool)
    id = val.id
    if id > length(ssanums)
        return val
    end
    if do_rename_ssa
        val = ssanums[id]
    end
    if isa(val, SSAValue)
        if used_ssa !== nothing
            used_ssa[val.id] += 1
        end
    end
    return val
end

function renumber_ssa2!(@nospecialize(stmt), ssanums::Vector{Any}, used_ssa::Vector{Int}, late_fixup::Vector{Int}, result_idx::Int, do_rename_ssa::Bool)
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, OldSSAValue) || isa(val, NewSSAValue)
            push!(late_fixup, result_idx)
        end
        if isa(val, SSAValue)
            val = renumber_ssa2(val, ssanums, used_ssa, do_rename_ssa)
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
    preds, succs = compact.result_bbs[compact.bb_rename_succ[to]].preds, compact.result_bbs[compact.bb_rename_pred[from]].succs
    deleteat!(preds, findfirst(x->x === compact.bb_rename_pred[from], preds)::Int)
    deleteat!(succs, findfirst(x->x === compact.bb_rename_succ[to], succs)::Int)
    # Check if the block is now dead
    if length(preds) == 0
        for succ in copy(compact.result_bbs[compact.bb_rename_succ[to]].succs)
            kill_edge!(compact, active_bb, to, findfirst(x->x === succ, compact.bb_rename_pred))
        end
        if to < active_bb
            # Kill all statements in the block
            stmts = compact.result_bbs[compact.bb_rename_succ[to]].stmts
            for stmt in stmts
                compact.result[stmt] = nothing
            end
            compact.result[last(stmts)] = ReturnNode()
        end
    else
        # We need to remove this edge from any phi nodes
        if to < active_bb
            idx = first(compact.result_bbs[compact.bb_rename_succ[to]].stmts)
            while idx < length(compact.result)
                stmt = compact.result[idx]
                stmt === nothing && continue
                isa(stmt, PhiNode) || break
                i = findfirst(x-> x === compact.bb_rename_pred[from], stmt.edges)
                if i !== nothing
                    deleteat!(stmt.edges, i)
                    deleteat!(stmt.values, i)
                end
                idx += 1
            end
        else
            idx = first(compact.ir.cfg.blocks[to].stmts)
            for stmt in CompactPeekIterator(compact, idx)
                stmt === nothing && continue
                isa(stmt, PhiNode) || break
                i = findfirst(x-> x === from, stmt.edges)
                if i !== nothing
                    deleteat!(stmt.edges, i)
                    deleteat!(stmt.values, i)
                end
            end
        end
    end
    nothing
end

function process_node!(compact::IncrementalCompact, result::Vector{Any},
        result_idx::Int, ssa_rename::Vector{Any},
        late_fixup::Vector{Int}, used_ssas::Vector{Int}, @nospecialize(stmt),
        idx::Int, processed_idx::Int, active_bb::Int, do_rename_ssa::Bool)
    ssa_rename[idx] = SSAValue(result_idx)
    if stmt === nothing
        ssa_rename[idx] = stmt
    elseif isa(stmt, OldSSAValue)
        ssa_rename[idx] = ssa_rename[stmt.id]
    elseif isa(stmt, GotoNode) && compact.cfg_transforms_enabled
        result[result_idx] = GotoNode(compact.bb_rename_succ[stmt.label])
        result_idx += 1
    elseif isa(stmt, GlobalRef) || isa(stmt, GotoNode)
        result[result_idx] = stmt
        result_idx += 1
    elseif isa(stmt, GotoIfNot) && compact.cfg_transforms_enabled
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, late_fixup, result_idx, do_rename_ssa)::GotoIfNot
        result[result_idx] = stmt
        cond = stmt.cond
        if isa(cond, Bool) && compact.fold_constant_branches
            if cond
                result[result_idx] = nothing
                kill_edge!(compact, active_bb, active_bb, stmt.dest)
                # Don't increment result_idx => Drop this statement
            else
                result[result_idx] = GotoNode(compact.bb_rename_succ[stmt.dest])
                kill_edge!(compact, active_bb, active_bb, active_bb+1)
                result_idx += 1
            end
        else
            result[result_idx] = GotoIfNot(cond, compact.bb_rename_succ[stmt.dest])
            result_idx += 1
        end
    elseif isa(stmt, Expr)
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, late_fixup, result_idx, do_rename_ssa)::Expr
        if compact.cfg_transforms_enabled && isexpr(stmt, :enter)
            stmt.args[1] = compact.bb_rename_succ[stmt.args[1]::Int]
        end
        result[result_idx] = stmt
        result_idx += 1
    elseif isa(stmt, PiNode)
        # As an optimization, we eliminate any trivial pinodes. For performance, we use ===
        # type equality. We may want to consider using == in either a separate pass or if
        # performance turns out ok
        stmt = renumber_ssa2!(stmt, ssa_rename, used_ssas, late_fixup, result_idx, do_rename_ssa)::PiNode
        pi_val = stmt.val
        if isa(pi_val, SSAValue)
            if stmt.typ === compact.result_types[pi_val.id]
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
        result[result_idx] = stmt
        result_idx += 1
    elseif isa(stmt, ReturnNode) || isa(stmt, UpsilonNode) || isa(stmt, GotoIfNot)
        result[result_idx] = renumber_ssa2!(stmt, ssa_rename, used_ssas, late_fixup, result_idx, do_rename_ssa)
        result_idx += 1
    elseif isa(stmt, PhiNode)
        values = process_phinode_values(stmt.values, late_fixup, processed_idx, result_idx, ssa_rename, used_ssas, do_rename_ssa)
        if length(stmt.edges) == 1 && isassigned(values, 1) &&
                length(compact.cfg_transforms_enabled ?
                    compact.result_bbs[compact.bb_rename_succ[active_bb]].preds :
                    compact.ir.cfg.blocks[active_bb].preds) == 1
            # There's only one predecessor left - just replace it
            ssa_rename[idx] = values[1]
        else
            edges = compact.cfg_transforms_enabled ? map!(i->compact.bb_rename_pred[i], stmt.edges, stmt.edges) : stmt.edges
            result[result_idx] = PhiNode(edges, values)
            result_idx += 1
        end
    elseif isa(stmt, PhiCNode)
        result[result_idx] = PhiCNode(process_phinode_values(stmt.values, late_fixup, processed_idx, result_idx, ssa_rename, used_ssas, do_rename_ssa))
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

function process_node!(compact::IncrementalCompact, result_idx::Int, @nospecialize(stmt), idx::Int, processed_idx::Int, active_bb::Int, do_rename_ssa::Bool)
    return process_node!(compact, compact.result, result_idx, compact.ssa_rename,
        compact.late_fixup, compact.used_ssas, stmt, idx, processed_idx, active_bb,
        do_rename_ssa)
end

function resize!(compact::IncrementalCompact, nnewnodes)
    old_length = length(compact.result)
    resize!(compact.result, nnewnodes)
    resize!(compact.result_types, nnewnodes)
    resize!(compact.result_lines, nnewnodes)
    resize!(compact.result_flags, nnewnodes)
    resize!(compact.used_ssas, nnewnodes)
    for i in (old_length+1):nnewnodes
        compact.used_ssas[i] = 0
    end
    nothing
end

function finish_current_bb!(compact, active_bb, old_result_idx=compact.result_idx, unreachable=false)
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
            if unreachable
                compact.result[old_result_idx] = ReturnNode()
                compact.result_types[old_result_idx] = Union{}
            else
                compact.result[old_result_idx] = nothing
                compact.result_types[old_result_idx] = Nothing
            end
            compact.result_lines[old_result_idx] = 0
            compact.result_flags[old_result_idx] = 0x00
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
    entry = compact.ir.new_nodes[compact.perm[compact.new_nodes_idx]]
    entry.pos == idx && entry.attach_after
end

function process_newnode!(compact, new_idx, new_node_entry, idx, active_bb, do_rename_ssa)
    old_result_idx = compact.result_idx
    bb = compact.ir.cfg.blocks[active_bb]
    compact.result_types[old_result_idx] = new_node_entry.typ
    compact.result_lines[old_result_idx] = new_node_entry.line
    result_idx = process_node!(compact, old_result_idx, new_node_entry.node, new_idx, idx - 1, active_bb, do_rename_ssa)
    compact.result_idx = result_idx
    # If this instruction has reverse affinity and we were at the end of a basic block,
    # finish it now.
    if new_node_entry.attach_after && idx == last(bb.stmts)+1 && !attach_after_stmt_after(compact, idx-1)
        active_bb += 1
        finish_current_bb!(compact, active_bb, old_result_idx)
    end
    (old_result_idx == result_idx) && return iterate(compact, (idx, active_bb))
    return Pair{Int, Any}(old_result_idx, compact.result[old_result_idx]), (idx, active_bb)
end

struct CompactPeekIterator
    compact::IncrementalCompact
    start_idx::Int
end

entry_at_idx(entry, idx) = entry.attach_after ? entry.pos == idx - 1 : entry.pos == idx
function iterate(it::CompactPeekIterator, (idx, aidx, bidx)::NTuple{3, Int}=(it.start_idx,it.compact.new_nodes_idx,1))
    # TODO: Take advantage of the fact that these arrays are sorted
    compact = it.compact
    if compact.new_nodes_idx <= length(compact.perm)
        for eidx in aidx:length(compact.perm)
            if entry_at_idx(compact.ir.new_nodes[compact.perm[eidx]], idx)
                entry = compact.ir.new_nodes[compact.perm[eidx]]
                return (entry.node, (idx, eidx+1, bidx))
            end
        end
    end
    if !isempty(compact.pending_perm)
        for eidx in bidx:length(compact.pending_perm)
            if entry_at_idx(compact.pending_nodes[compact.pending_perm[eidx]], idx)
                entry = compact.pending_nodes[compact.compact.pending_perm[eidx]]
                return (entry.node, (idx, aidx, eidx+1))
            end
        end
    end
    idx > length(compact.ir.stmts) && return nothing
    return (compact.ir.stmts[idx], (idx + 1, aidx, bidx))
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
    if compact.cfg_transforms_enabled && active_bb > 1 && active_bb <= length(compact.bb_rename_succ) && length(bb.preds) == 0
        # No predecessors, kill the entire block.
        compact.idx = last(bb.stmts)
        # Pop any remaining insertion nodes
        while compact.new_nodes_idx <= length(compact.perm)
            entry = compact.ir.new_nodes[compact.perm[compact.new_nodes_idx]]
            if !(entry.attach_after ? entry.pos <= compact.idx - 1 : entry.pos <= compact.idx)
                break
            end
            compact.new_nodes_idx += 1
        end
        while !isempty(compact.pending_perm)
            entry = compact.pending_nodes[compact.pending_perm[1]];
            if !(entry.attach_after ? entry.pos <= compact.idx - 1 : entry.pos <= compact.idx)
                break
            end
            popfirst!(compact.pending_perm)
        end
        # Move to next block
        compact.idx += 1
        if finish_current_bb!(compact, active_bb, old_result_idx, true)
            return iterate(compact, (compact.idx, active_bb + 1))
        else
            return Pair{Int, Any}(old_result_idx, compact.result[old_result_idx]), (compact.idx, active_bb + 1)
        end
    end
    if compact.new_nodes_idx <= length(compact.perm) &&
        (entry =  compact.ir.new_nodes[compact.perm[compact.new_nodes_idx]];
         entry.attach_after ? entry.pos == idx - 1 : entry.pos == idx)
        new_idx = compact.perm[compact.new_nodes_idx]
        compact.new_nodes_idx += 1
        new_node_entry = compact.ir.new_nodes[new_idx]
        new_idx += length(compact.ir.stmts)
        return process_newnode!(compact, new_idx, new_node_entry, idx, active_bb, true)
    elseif !isempty(compact.pending_perm) &&
        (entry = compact.pending_nodes[compact.pending_perm[1]];
         entry.attach_after ? entry.pos == idx - 1 : entry.pos == idx)
        new_idx = popfirst!(compact.pending_perm)
        new_node_entry = compact.pending_nodes[new_idx]
        new_idx += length(compact.ir.stmts) + length(compact.ir.new_nodes)
        return process_newnode!(compact, new_idx, new_node_entry, idx, active_bb, false)
    end
    # This will get overwritten in future iterations if
    # result_idx is not, incremented, but that's ok and expected
    compact.result_types[old_result_idx] = compact.ir.types[idx]
    compact.result_lines[old_result_idx] = compact.ir.lines[idx]
    compact.result_flags[old_result_idx] = compact.ir.flags[idx]
    result_idx = process_node!(compact, old_result_idx, compact.ir.stmts[idx], idx, idx, active_bb, true)
    stmt_if_any = old_result_idx == result_idx ? nothing : compact.result[old_result_idx]
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
    if !isassigned(compact.result, old_result_idx)
        @assert false
    end
    return Pair{Int, Any}(old_result_idx, compact.result[old_result_idx]), (compact.idx, active_bb)
end

function maybe_erase_unused!(extra_worklist, compact, idx, callback = x->nothing)
    stmt = compact.result[idx]
    stmt === nothing && return false
    if compact_exprtype(compact, SSAValue(idx)) === Bottom
        effect_free = false
    else
        effect_free = stmt_effect_free(stmt, compact.result_types[idx], compact, compact.ir.sptypes)
    end
    if effect_free
        for ops in userefs(stmt)
            val = ops[]
            # If the pass we ran inserted new nodes, it's possible for those
            # to be outside our used_ssas count.
            if isa(val, SSAValue) && val.id <= length(compact.used_ssas)
                if compact.used_ssas[val.id] == 1
                    if val.id < idx
                        push!(extra_worklist, val.id)
                    end
                end
                compact.used_ssas[val.id] -= 1
                callback(val)
            end
        end
        compact.result[idx] = nothing
        return true
    end
    return false
end

function fixup_phinode_values!(compact::IncrementalCompact, old_values::Vector{Any})
    values = Vector{Any}(undef, length(old_values))
    for i = 1:length(old_values)
        isassigned(old_values, i) || continue
        val = old_values[i]
        if isa(val, OldSSAValue)
            val = compact.ssa_rename[val.id]
            if isa(val, SSAValue)
                compact.used_ssas[val.id] += 1
            end
        elseif isa(val, NewSSAValue)
            val = SSAValue(length(compact.result) + val.id)
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
        return compact.ssa_rename[stmt.id]
    else
        urs = userefs(stmt)
        for ur in urs
            val = ur[]
            if isa(val, NewSSAValue)
                ur[] = SSAValue(length(compact.result) + val.id)
            elseif isa(val, OldSSAValue)
                ur[] = compact.ssa_rename[val.id]
            end
        end
        return urs[]
    end
end

function just_fixup!(compact::IncrementalCompact)
    for idx in compact.late_fixup
        stmt = compact.result[idx]
        new_stmt = fixup_node(compact, stmt)
        (stmt !== new_stmt) && (compact.result[idx] = new_stmt)
    end
    for idx in 1:length(compact.new_new_nodes)
        node = compact.new_new_nodes[idx]
        new_stmt = fixup_node(compact, node.node)
        if node.node !== new_stmt
            compact.new_new_nodes[idx] = NewNode(
                node.pos, node.attach_after, node.typ,
                new_stmt, node.line)
        end
    end
end

function simple_dce!(compact::IncrementalCompact)
    # Perform simple DCE for unused values
    extra_worklist = Int[]
    for (idx, nused) in Iterators.enumerate(compact.used_ssas)
        idx >= compact.result_idx && break
        nused == 0 || continue
        maybe_erase_unused!(extra_worklist, compact, idx)
    end
    while !isempty(extra_worklist)
        maybe_erase_unused!(extra_worklist, compact, pop!(extra_worklist))
    end
end

function non_dce_finish!(compact::IncrementalCompact)
    result_idx = compact.result_idx
    resize!(compact.result, result_idx-1)
    resize!(compact.result_types, result_idx-1)
    resize!(compact.result_lines, result_idx-1)
    resize!(compact.result_flags, result_idx-1)
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
    return IRCode(compact.ir, compact.result, compact.result_types, compact.result_lines, compact.result_flags, cfg, compact.new_new_nodes)
end

function compact!(code::IRCode, allow_cfg_transforms=false)
    compact = IncrementalCompact(code, allow_cfg_transforms)
    # Just run through the iterator without any processing
    foreach(x -> nothing, compact) # x isa Pair{Int, Any}
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
