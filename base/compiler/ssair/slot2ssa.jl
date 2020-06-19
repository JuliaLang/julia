# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct SlotInfo
    defs::Vector{Int}
    uses::Vector{Int}
    any_newvar::Bool
end
SlotInfo() = SlotInfo(Int[], Int[], false)

function scan_entry!(result::Vector{SlotInfo}, idx::Int, @nospecialize(stmt))
    # NewVarNodes count as defs for the purpose
    # of liveness analysis (i.e. they kill use chains)
    if isa(stmt, NewvarNode)
        result[slot_id(stmt.slot)].any_newvar = true
        push!(result[slot_id(stmt.slot)].defs, idx)
        return
    elseif isexpr(stmt, :(=))
        if isa(stmt.args[1], SlotNumber)
            push!(result[slot_id(stmt.args[1])].defs, idx)
        end
        stmt = stmt.args[2]
    end
    if isa(stmt, Union{SlotNumber, TypedSlot})
        push!(result[slot_id(stmt)].uses, idx)
        return
    end
    for op in userefs(stmt)
        val = op[]
        if isa(val, Union{SlotNumber, TypedSlot})
            push!(result[slot_id(val)].uses, idx)
        end
    end
end


function lift_defuse(cfg::CFG, defuse)
    map(defuse) do slot
        SlotInfo(
            Int[block_for_inst(cfg, x) for x in slot.defs],
            Int[block_for_inst(cfg, x) for x in slot.uses],
            slot.any_newvar
        )
    end
end

function scan_slot_def_use(nargs::Int, ci::CodeInfo, code::Vector{Any})
    nslots = length(ci.slotflags)
    result = SlotInfo[SlotInfo() for i = 1:nslots]
    # Set defs for arguments
    for var in result[1:(1+nargs)]
        push!(var.defs, 0)
    end
    for idx in 1:length(code)
        stmt = code[idx]
        scan_entry!(result, idx, stmt)
    end
    result
end

function renumber_ssa(stmt::SSAValue, ssanums::Vector{Any}, new_ssa::Bool=false, used_ssa::Union{Nothing, Vector{Int}}=nothing)
    id = stmt.id
    if id > length(ssanums)
        return stmt
    end
    val = ssanums[id]
    if isa(val, SSAValue) && used_ssa !== nothing
        used_ssa[val.id] += 1
    end
    return val
end

function renumber_ssa!(@nospecialize(stmt), ssanums::Vector{Any}, new_ssa::Bool=false, used_ssa::Union{Nothing, Vector{Int}}=nothing)
    isa(stmt, SSAValue) && return renumber_ssa(stmt, ssanums, new_ssa, used_ssa)
    return ssamap(val->renumber_ssa(val, ssanums, new_ssa, used_ssa), stmt)
end

function make_ssa!(ci::CodeInfo, code::Vector{Any}, idx, slot, @nospecialize(typ))
    (idx == 0) && return Argument(slot)
    stmt = code[idx]
    @assert isexpr(stmt, :(=))
    code[idx] = stmt.args[2]
    ci.ssavaluetypes[idx] = typ
    idx
end

struct UndefToken
end
const undef_token = UndefToken()

function new_to_regular(@nospecialize(stmt), new_offset::Int)
    if isa(stmt, NewSSAValue)
        return SSAValue(stmt.id + new_offset)
    end
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, NewSSAValue)
            op[] = SSAValue(val.id + new_offset)
        end
    end
    return urs[]
end

function fixup_slot!(ir::IRCode, ci::CodeInfo, idx::Int, slot::Int, @nospecialize(stmt::Union{SlotNumber, TypedSlot}), @nospecialize(ssa))
    # We don't really have the information here to get rid of these.
    # We'll do so later
    if ssa === undef_token
        insert_node!(ir, idx, Any, Expr(:throw_undef_if_not, ci.slotnames[slot], false))
        return undef_token
    end
    if !isa(ssa, Argument) && !(ssa === nothing) && ((ci.slotflags[slot] & SLOT_USEDUNDEF) != 0)
        insert_node!(ir, idx, Any, Expr(:undefcheck, ci.slotnames[slot], ssa))
    end
    if isa(stmt, SlotNumber)
        return ssa
    elseif isa(stmt, TypedSlot)
        return NewSSAValue(insert_node!(ir, idx, stmt.typ, PiNode(ssa, stmt.typ)).id - length(ir.stmts))
    end
    @assert false # unreachable
end

function fixemup!(cond, rename, ir::IRCode, ci::CodeInfo, idx::Int, @nospecialize(stmt))
    if isa(stmt, Union{SlotNumber, TypedSlot}) && cond(stmt)
        return fixup_slot!(ir, ci, idx, slot_id(stmt), stmt, rename(stmt))
    end
    if isexpr(stmt, :(=))
        stmt.args[2] = fixemup!(cond, rename, ir, ci, idx, stmt.args[2])
        return stmt
    end
    if isa(stmt, PhiNode)
        for i = 1:length(stmt.edges)
            isassigned(stmt.values, i) || continue
            val = stmt.values[i]
            isa(val, Union{SlotNumber, TypedSlot}) || continue
            cond(val) || continue
            bb_idx = block_for_inst(ir.cfg, stmt.edges[i])
            from_bb_terminator = last(ir.cfg.blocks[bb_idx].stmts)
            stmt.values[i] = fixup_slot!(ir, ci, from_bb_terminator, slot_id(val), val, rename(val))
        end
        return stmt
    end
    if isexpr(stmt, :isdefined)
        val = stmt.args[1]
        if isa(val, Union{SlotNumber, TypedSlot})
            slot = slot_id(val)
            if (ci.slotflags[slot] & SLOT_USEDUNDEF) == 0
                return true
            else
                ssa = rename(val)
                if ssa === undef_token
                    return false
                elseif !isa(ssa, SSAValue) && !isa(ssa, NewSSAValue)
                    return true
                end
            end
            stmt.args[1] = ssa
        end
        return stmt
    end
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, Union{SlotNumber, TypedSlot}) && cond(val)
            x = fixup_slot!(ir, ci, idx, slot_id(val), val, rename(val))
            # We inserted an undef error node. Delete subsequent statement
            # to avoid confusing the optimizer
            if x === undef_token
                return nothing
            end
            op[] = x
        end
    end
    return urs[]
end

function fixup_uses!(ir::IRCode, ci::CodeInfo, code::Vector{Any}, uses::Vector{Int}, slot::Int, @nospecialize(ssa))
    for use in uses
        code[use] = fixemup!(stmt->slot_id(stmt)==slot, stmt->ssa, ir, ci, use, code[use])
    end
end

function rename_uses!(ir::IRCode, ci::CodeInfo, idx::Int, @nospecialize(stmt), renames::Vector{Any})
    return fixemup!(stmt->true, stmt->renames[slot_id(stmt)], ir, ci, idx, stmt)
end

function strip_trailing_junk!(ci::CodeInfo, code::Vector{Any}, flags::Vector{UInt8})
    # Remove `nothing`s at the end, we don't handle them well
    # (we expect the last instruction to be a terminator)
    for i = length(code):-1:1
        if code[i] !== nothing
            resize!(code, i)
            resize!(ci.ssavaluetypes, i)
            resize!(ci.codelocs, i)
            resize!(flags, i)
            break
        end
    end
    # If the last instruction is not a terminator, add one. This can
    # happen for implicit return on dead branches.
    term = code[end]
    if !isa(term, GotoIfNot) && !isa(term, GotoNode) && !isa(term, ReturnNode)
        push!(code, ReturnNode())
        push!(ci.ssavaluetypes, Union{})
        push!(ci.codelocs, 0)
        push!(flags, 0x00)
    end
    nothing
end

struct DelayedTyp
    phi::NewSSAValue
end

# maybe use expr_type?
function typ_for_val(@nospecialize(x), ci::CodeInfo, sptypes::Vector{Any}, idx::Int, slottypes::Vector{Any})
    if isa(x, Expr)
        if x.head === :static_parameter
            return sptypes[x.args[1]]
        elseif x.head === :boundscheck
            return Bool
        elseif x.head === :copyast
            return typ_for_val(x.args[1], ci, sptypes, idx, slottypes)
        end
        return ci.ssavaluetypes[idx]
    end
    isa(x, GlobalRef) && return abstract_eval_global(x.mod, x.name)
    isa(x, SSAValue) && return ci.ssavaluetypes[x.id]
    isa(x, Argument) && return slottypes[x.n]
    isa(x, NewSSAValue) && return DelayedTyp(x)
    isa(x, QuoteNode) && return Const(x.value)
    isa(x, Union{Symbol, PiNode, PhiNode, SlotNumber, TypedSlot}) && error("unexpected val type")
    return Const(x)
end

struct BlockLiveness
    def_bbs::Vector{Int}
    live_in_bbs::Vector{Int}
end

# Run iterated dominance frontier
#
# The algorithm we have here essentially follows LLVM, which itself is a
# a cleaned up version of the linear-time algorithm described in
#
#  A Linear Time Algorithm for Placing phi-Nodes (by Sreedhar and Gao)
#
# The algorithm here, is quite straightforward. Suppose we have a CFG:
#
# A -> B -> D -> F
#  \-> C -------/
#
# and a corresponding dominator tree:
#
# A
# |- B - D
# |- C
# |- F
#
# Now, for every definition of our slot, we simply walk down the dominator
# tree and look for any edges that leave the sub-domtree rooted by our definition.
#
# E.g. in our example above, if we have a definition in `B`, we look at its successors,
#      which is only `D`, which is dominated by `B` and hence doesn't need a phi node.
#      Then we descend down the subtree rooted at `B` and end up in `D`. `D` has a successor
#      `F`, which is not part of the current subtree, (i.e. not dominated by `B`), so it
#      needs a phi node.
#
# Now, the key insight of that algorithm is that we have two defs, in blocks `A` and `B`,
# and `A` dominates `B`, then we do not need to recurse into `B`, because the set of
# potential backedges from a subtree rooted at `B` (to outside the subtree) is a strict
# subset of those backedges from a subtree rooted at `A` (out outside the subtree rooted
# at `A`). Note however that this does not work the other way. Thus, the algorithm
# needs to make sure that we always visit `B` before `A`.
function idf(cfg::CFG, liveness::BlockLiveness, domtree::DomTree)
    # This should be a priority queue, but TODO - sorted array for now
    defs = liveness.def_bbs
    pq = Tuple{Int, Int}[(defs[i], domtree.nodes[defs[i]].level) for i in 1:length(defs)]
    sort!(pq, by=x->x[2])
    phiblocks = Int[]
    # This bitset makes sure we only add a phi node to a given block once.
    processed = BitSet()
    # This bitset implements the `key insight` mentioned above. In particular, it prevents
    # us from visiting a subtree that we have already visited before.
    visited = BitSet()
    while !isempty(pq)
        # We pop from the end of the array - i.e. the element with the highest level.
        node, level = pop!(pq)
        worklist = Int[]
        push!(worklist, node)
        while !isempty(worklist)
            active = pop!(worklist)
            for succ in cfg.blocks[active].succs
                # Check whether the current root (`node`) dominates succ.
                # We are guaranteed that `node` dominates `active`, since
                # we've arrived at `active` by following dominator tree edges.
                # If `succ`'s level is less than or equal to that of `node`,
                # it cannot possibly be dominated by `node`. On the other hand,
                # since at this point we know that there is an edge from `node`'s
                # subtree to `succ`, we know that if succ's level is greater than
                # that of `node`, it must be dominated by `node`.
                succ_level = domtree.nodes[succ].level
                succ_level > level && continue
                # We don't dominate succ. We need to place a phinode,
                # unless liveness said otherwise.
                succ in processed && continue
                push!(processed, succ)
                if !(succ in liveness.live_in_bbs)
                    continue
                end
                push!(phiblocks, succ)
                # Basically: Consider the phi node we just added another
                # def of this value. N.B.: This needs to retain the invariant that it
                # is processed before any of its parents in the dom tree. This is guaranteed,
                # because succ_level <= level, which is the greatest level we have currently
                # processed. Thus, we have not yet processed any subtrees of level < succ_level.
                if !(succ in defs)
                    push!(pq, (succ, succ_level))
                    sort!(pq, by=x->x[2])
                end
            end
            # Recurse down the current subtree
            for child in domtree.nodes[active].children
                child in visited && continue
                push!(visited, child)
                push!(worklist, child)
            end
        end
    end
    phiblocks
end

function rename_incoming_edge(old_edge, old_to, result_order, bb_rename)
    new_edge_from = bb_rename[old_edge]
    if old_edge == old_to - 1
        # Could have been a crit edge break
        if new_edge_from < length(result_order) && result_order[new_edge_from + 1] == 0
            new_edge_from += 1
        end
    end
    new_edge_from
end

function rename_outgoing_edge(old_to, old_from, result_order, bb_rename)
    new_edge_to = bb_rename[old_to]
    if old_from == old_to - 1
        # Could have been a crit edge break
        if bb_rename[old_from] < length(result_order) && result_order[bb_rename[old_from]+1] == 0
            new_edge_to = bb_rename[old_from] + 1
        end
    end
    new_edge_to
end

function rename_phinode_edges(node, bb, result_order, bb_rename)
    new_values = Any[]
    new_edges = Any[]
    for (idx, edge) in pairs(node.edges)
        (edge == 0 || haskey(bb_rename, edge)) || continue
        new_edge_from = edge == 0 ? 0 : rename_incoming_edge(edge, bb, result_order, bb_rename)
        push!(new_edges, new_edge_from)
        if isassigned(node.values, idx)
            push!(new_values, node.values[idx])
        else
            resize!(new_values, length(new_values)+1)
        end
    end
    return PhiNode(new_edges, new_values)
end

"""
    Sort the basic blocks in `ir` into domtree order (i.e. if bb`` is higher in
    the domtree than bb2, it will come first in the linear order). The resulting
    ir has the property that a linear traversal of basic blocks will also be a
    RPO traversal and in particular, any use of an SSA value must come after (by linear
    order) its definition.
"""
function domsort_ssa!(ir::IRCode, domtree::DomTree)
    # First compute the new order of basic blocks
    result_order = Int[]
    stack = Int[]
    node = 1
    ncritbreaks = 0
    nnewfallthroughs = 0
    while node !== -1
        push!(result_order, node)
        cs = domtree.nodes[node].children
        terminator = ir.stmts[last(ir.cfg.blocks[node].stmts)][:inst]
        iscondbr = isa(terminator, GotoIfNot)
        let old_node = node + 1
            if length(cs) >= 1
                # Adding the nodes in reverse sorted order attempts to retain
                # the original source order of the nodes as much as possible.
                # This is not required for correctness, but is easier on the humans
                if old_node in cs
                    # Schedule the fall through node first,
                    # so we can retain the fall through
                    append!(stack, reverse(sort(filter(x -> (x != old_node), cs))))
                    node = node + 1
                else
                    append!(stack, reverse(sort(cs)))
                    node = pop!(stack)
                end
            else
                if isempty(stack)
                    node = -1
                else
                    node = pop!(stack)
                end
            end
            if node != old_node && !isa(terminator, Union{GotoNode, ReturnNode})
                if isa(terminator, GotoIfNot)
                    # Need to break the critical edge
                    ncritbreaks += 1
                    push!(result_order, 0)
                else
                    nnewfallthroughs += 1
                end
            end
        end
    end
    bb_rename = IdDict{Int,Int}(i=>x for (x, i) in pairs(result_order) if i !== 0)
    new_bbs = Vector{BasicBlock}(undef, length(result_order))
    nstmts = 0
    for i in result_order
        if i !== 0
            nstmts += length(ir.cfg.blocks[i].stmts)
        end
    end
    result = InstructionStream(nstmts + ncritbreaks + nnewfallthroughs)
    inst_rename = Vector{Any}(undef, length(ir.stmts) + length(ir.new_nodes))
    for i = 1:length(ir.new_nodes)
        inst_rename[i + length(ir.stmts)] = SSAValue(i + length(result))
    end
    bb_start_off = 0
    crit_edge_breaks_fixup = Tuple{Int, Int}[]
    for (new_bb, bb) in pairs(result_order)
        if bb == 0
            nidx = bb_start_off + 1
            inst = result[nidx][:inst]
            @assert isa(inst, GotoNode)
            # N.B.: The .label has already been renamed when it was created.
            new_bbs[new_bb] = BasicBlock(nidx:nidx, [new_bb - 1], [inst.label])
            bb_start_off += 1
            continue
        end
        old_inst_range = ir.cfg.blocks[bb].stmts
        inst_range = (bb_start_off+1):(bb_start_off+length(old_inst_range))
        for (nidx, idx) in zip(inst_range, old_inst_range)
            inst_rename[idx] = SSAValue(nidx)
            @assert !isassigned(result.inst, nidx)
            node = result[nidx]
            node[] = ir.stmts[idx]
            inst = node[:inst]
            if isa(inst, PhiNode)
                node[:inst] = rename_phinode_edges(inst, bb, result_order, bb_rename)
            end
        end
        # Now fix up the terminator
        terminator = result[inst_range[end]][:inst]
        if isa(terminator, GotoNode)
            # Convert to implicit fall through
            if bb_rename[terminator.label] == new_bb + 1
                result[inst_range[end]][:inst] = nothing
            else
                result[inst_range[end]][:inst] = GotoNode(bb_rename[terminator.label])
            end
        elseif isa(terminator, GotoIfNot)
            # Check if we need to break the critical edge
            if bb_rename[bb + 1] != new_bb + 1
                @assert result_order[new_bb + 1] == 0
                # Add an explicit goto node in the next basic block (we accounted for this above)
                nidx = inst_range[end] + 1
                node = result[nidx]
                node[:inst], node[:type], node[:line] = GotoNode(bb_rename[bb + 1]), Any, 0
            end
            result[inst_range[end]][:inst] = GotoIfNot(terminator.cond, bb_rename[terminator.dest])
        elseif !isa(terminator, ReturnNode)
            if isa(terminator, Expr)
                if terminator.head == :enter
                    terminator.args[1] = bb_rename[terminator.args[1]]
                end
            end
            if bb_rename[bb + 1] != new_bb + 1
                # Add an explicit goto node
                nidx = inst_range[end] + 1
                node = result[nidx]
                node[:inst], node[:type], node[:line] = GotoNode(bb_rename[bb + 1]), Any, 0
                inst_range = first(inst_range):(last(inst_range) + 1)
            end
        end
        bb_start_off += length(inst_range)
        local new_preds, new_succs
        let bb = bb, bb_rename = bb_rename, result_order = result_order
            new_preds = Int[rename_incoming_edge(i, bb, result_order, bb_rename) for i in ir.cfg.blocks[bb].preds if haskey(bb_rename, i)]
            new_succs = Int[rename_outgoing_edge(i, bb, result_order, bb_rename) for i in ir.cfg.blocks[bb].succs if haskey(bb_rename, i)]
        end
        new_bbs[new_bb] = BasicBlock(inst_range, new_preds, new_succs)
    end
    for i in 1:length(result)
        result[i][:inst] = renumber_ssa!(result[i][:inst], inst_rename, true)
    end
    cfg = CFG(new_bbs, Int[first(bb.stmts) for bb in new_bbs[2:end]])
    new_new_nodes = NewNodeStream(length(ir.new_nodes))
    for i = 1:length(ir.new_nodes)
        new_info = ir.new_nodes.info[i]
        new_new_info = NewNodeInfo(inst_rename[new_info.pos].id, new_info.attach_after)
        new_new_nodes.info[i] = new_new_info
        new_node = new_new_nodes.stmts[i]
        new_node[] = ir.new_nodes.stmts[i]
        new_node_inst = new_node[:inst]
        if isa(new_node_inst, PhiNode)
            new_node_inst = rename_phinode_edges(new_node_inst, block_for_inst(ir.cfg, new_info.pos), result_order, bb_rename)
        end
        new_node[:inst] = renumber_ssa!(new_node_inst, inst_rename, true)
    end
    new_ir = IRCode(ir, result, cfg, new_new_nodes)
    return new_ir
end

function compute_live_ins(cfg::CFG, defuse)
    # We remove from `uses` any block where all uses are dominated
    # by a def. This prevents insertion of dead phi nodes at the top
    # of such a block if that block happens to be in a loop
    ordered = Tuple{Int, Int, Bool}[(x, block_for_inst(cfg, x), true) for x in defuse.uses]
    for x in defuse.defs
        push!(ordered, (x, block_for_inst(cfg, x), false))
    end
    ordered = sort(ordered, by=x->x[1])
    bb_defs = Int[]
    bb_uses = Int[]
    last_bb = last_def_bb = 0
    for (_, bb, is_use) in ordered
        if bb != last_bb && is_use
            push!(bb_uses, bb)
        end
        last_bb = bb
        if last_def_bb != bb && !is_use
            push!(bb_defs, bb)
            last_def_bb = bb
        end
    end
    # To obtain live ins from bb_uses, recursively add predecessors
    extra_liveins = BitSet()
    worklist = Int[]
    for bb in bb_uses
        append!(worklist, filter(p->p != 0 && !(p in bb_defs), cfg.blocks[bb].preds))
    end
    while !isempty(worklist)
        elem = pop!(worklist)
        (elem in bb_uses || elem in extra_liveins) && continue
        push!(extra_liveins, elem)
        append!(worklist, filter(p->p != 0 && !(p in bb_defs), cfg.blocks[elem].preds))
    end
    append!(bb_uses, extra_liveins)
    BlockLiveness(bb_defs, bb_uses)
end

function recompute_type(node::Union{PhiNode, PhiCNode}, ci::CodeInfo, ir::IRCode, sptypes::Vector{Any}, slottypes::Vector{Any})
    new_typ = Union{}
    for i = 1:length(node.values)
        if isa(node, PhiNode) && !isassigned(node.values, i)
            if !isa(new_typ, MaybeUndef)
                new_typ = MaybeUndef(new_typ)
            end
            continue
        end
        typ = typ_for_val(node.values[i], ci, sptypes, -1, slottypes)
        was_maybe_undef = false
        if isa(typ, MaybeUndef)
            typ = typ.typ
            was_maybe_undef = true
        end
        @assert !isa(typ, MaybeUndef)
        while isa(typ, DelayedTyp)
            typ = types(ir)[typ.phi::NewSSAValue]
        end
        new_typ = tmerge(new_typ, was_maybe_undef ? MaybeUndef(typ) : typ)
    end
    return new_typ
end

function construct_ssa!(ci::CodeInfo, ir::IRCode, domtree::DomTree, defuse, nargs::Int, sptypes::Vector{Any},
                        slottypes::Vector{Any})
    code = ir.stmts.inst
    cfg = ir.cfg
    left = Int[]
    catch_entry_blocks = Tuple{Int, Int}[]
    for idx in 1:length(code)
        stmt = code[idx]
        if isexpr(stmt, :enter)
            push!(catch_entry_blocks, (block_for_inst(cfg, idx), block_for_inst(cfg, stmt.args[1])))
        end
    end

    exc_handlers = IdDict{Int, Tuple{Int, Int}}()
    # Record the correct exception handler for all cricitcal sections
    for (enter_block, exc) in catch_entry_blocks
        exc_handlers[enter_block+1] = (enter_block, exc)
        # TODO: Cut off here if the terminator is a leave corresponding to this enter
        for block in dominated(domtree, enter_block+1)
            exc_handlers[block] = (enter_block, exc)
        end
    end

    phi_slots = Vector{Int}[Vector{Int}() for _ = 1:length(ir.cfg.blocks)]
    phi_nodes = Vector{Pair{NewSSAValue,PhiNode}}[Vector{Pair{NewSSAValue,PhiNode}}() for _ = 1:length(cfg.blocks)]
    phi_ssas = SSAValue[]
    phicnodes = IdDict{Int, Vector{Tuple{SlotNumber, NewSSAValue, PhiCNode}}}()
    for (_, exc) in catch_entry_blocks
        phicnodes[exc] = Vector{Tuple{SlotNumber, NewSSAValue, PhiCNode}}()
    end
    @timeit "idf" for (idx, slot) in Iterators.enumerate(defuse)
        # No uses => no need for phi nodes
        isempty(slot.uses) && continue
        # TODO: Restore this optimization
        if false # length(slot.defs) == 1 && slot.any_newvar
            if slot.defs[] == 0
                typ = slottypes[idx]
                ssaval = Argument(idx)
                fixup_uses!(ir, ci, code, slot.uses, idx, ssaval)
            elseif isa(code[slot.defs[]], NewvarNode)
                typ = MaybeUndef(Union{})
                ssaval = nothing
                for use in slot.uses[]
                    insert_node!(ir, use, Union{}, Expr(:throw_undef_if_not, ci.slotnames[idx], false))
                end
                fixup_uses!(ir, ci, code, slot.uses, idx, nothing)
            else
                val = code[slot.defs[]].args[2]
                typ = typ_for_val(val, ci, sptypes, slot.defs[], slottypes)
                ssaval = SSAValue(make_ssa!(ci, code, slot.defs[], idx, typ))
                fixup_uses!(ir, ci, code, slot.uses, idx, ssaval)
            end
            continue
        end
        @timeit "liveness" (live = compute_live_ins(cfg, slot))
        for li in live.live_in_bbs
            cidx = findfirst(x->x[2] == li, catch_entry_blocks)
            if cidx !== nothing
                # The slot is live-in into this block. We need to
                # Create a PhiC node in the catch entry block and
                # an upsilon node in the corresponding enter block
                node = PhiCNode(Any[])
                phic_ssa = NewSSAValue(insert_node!(ir, first_insert_for_bb(code, cfg, li), Union{}, node).id - length(ir.stmts))
                push!(phicnodes[li], (SlotNumber(idx), phic_ssa, node))
                # Inform IDF that we now have a def in the catch block
                if !(li in live.def_bbs)
                    push!(live.def_bbs, li)
                end
            end
        end
        phiblocks = idf(cfg, live, domtree)
        for block in phiblocks
            push!(phi_slots[block], idx)
            node = PhiNode()
            ssa = NewSSAValue(insert_node!(ir, first_insert_for_bb(code, cfg, block), Union{}, node).id - length(ir.stmts))
            push!(phi_nodes[block], ssa=>node)
        end
        push!(left, idx)
    end
    # Perform SSA renaming
    initial_incoming_vals = Any[
        if 0 in defuse[x].defs
            Argument(x)
        elseif !defuse[x].any_newvar
            undef_token
        else
            SSAValue(-2)
        end for x in 1:length(ci.slotflags)
    ]
    worklist = Tuple{Int, Int, Vector{Any}}[(1, 0, initial_incoming_vals)]
    visited = BitSet()
    type_refine_phi = BitSet()
    new_nodes = ir.new_nodes
    @timeit "SSA Rename" while !isempty(worklist)
        (item::Int, pred, incoming_vals) = pop!(worklist)
        # Rename existing phi nodes first, because their uses occur on the edge
        # TODO: This isn't necessary if inlining stops replacing arguments by slots.
        for idx in cfg.blocks[item].stmts
            stmt = code[idx]
            if isexpr(stmt, :(=))
                stmt = stmt.args[2]
            end
            isa(stmt, PhiNode) || continue
            for (edgeidx, edge) in pairs(stmt.edges)
                from_bb = edge == 0 ? 0 : block_for_inst(cfg, edge)
                from_bb == pred || continue
                isassigned(stmt.values, edgeidx) || break
                stmt.values[edgeidx] = rename_uses!(ir, ci, edge, stmt.values[edgeidx], incoming_vals)
                break
            end
        end
        # Insert phi nodes if necessary
        for (idx, slot) in Iterators.enumerate(phi_slots[item])
            ssaval, node = phi_nodes[item][idx]
            incoming_val = incoming_vals[slot]
            if incoming_val == SSAValue(-1)
                # Optimistically omit this path.
                # Liveness analysis would probably have prevented us from inserting this phi node
                continue
            end
            push!(node.edges, pred)
            if incoming_val == undef_token
                resize!(node.values, length(node.values)+1)
            else
                push!(node.values, incoming_val)
            end
            # TODO: Remove the next line, it shouldn't be necessary
            push!(type_refine_phi, ssaval.id)
            if isa(incoming_val, NewSSAValue)
                push!(type_refine_phi, ssaval.id)
            end
            typ = incoming_val == undef_token ? MaybeUndef(Union{}) : typ_for_val(incoming_val, ci, sptypes, -1, slottypes)
            old_entry = new_nodes.stmts[ssaval.id]
            if isa(typ, DelayedTyp)
                push!(type_refine_phi, ssaval.id)
            end
            new_typ = isa(typ, DelayedTyp) ? Union{} : tmerge(old_entry[:type], typ)
            old_entry[:type] = new_typ
            old_entry[:inst] = node
            incoming_vals[slot] = ssaval
        end
        (item in visited) && continue
        # Record phi_C nodes if necessary
        if haskey(phicnodes, item)
            for (slot, ssa, _) in phicnodes[item]
                incoming_vals[slot_id(slot)] = ssa
            end
        end
        # Record initial upsilon nodes if necessary
        eidx = findfirst(x->x[1] == item, catch_entry_blocks)
        if eidx !== nothing
            for (slot, _, node) in phicnodes[catch_entry_blocks[eidx][2]]
                ival = incoming_vals[slot_id(slot)]
                ivalundef = ival === undef_token
                unode = ivalundef ? UpsilonNode() : UpsilonNode(ival)
                typ = ivalundef ? MaybeUndef(Union{}) : typ_for_val(ival, ci, sptypes, -1, slottypes)
                push!(node.values,
                    NewSSAValue(insert_node!(ir, first_insert_for_bb(code, cfg, item),
                                 typ, unode, true).id - length(ir.stmts)))
            end
        end
        push!(visited, item)
        for idx in cfg.blocks[item].stmts
            stmt = code[idx]
            (isa(stmt, PhiNode) || (isexpr(stmt, :(=)) && isa(stmt.args[2], PhiNode))) && continue
            if isa(stmt, NewvarNode)
                incoming_vals[slot_id(stmt.slot)] = undef_token
                code[idx] = nothing
            else
                stmt = rename_uses!(ir, ci, idx, stmt, incoming_vals)
                if stmt === nothing && isa(code[idx], Union{ReturnNode, GotoIfNot}) && idx == last(cfg.blocks[item].stmts)
                    # preserve the CFG
                    stmt = ReturnNode()
                end
                code[idx] = stmt
                # Record a store
                if isexpr(stmt, :(=)) && isa(stmt.args[1], SlotNumber)
                    id = slot_id(stmt.args[1])
                    val = stmt.args[2]
                    typ = typ_for_val(val, ci, sptypes, idx, slottypes)
                    # Having undef_token appear on the RHS is possible if we're on a dead branch.
                    # Do something reasonable here, by marking the LHS as undef as well.
                    if val !== undef_token
                        incoming_vals[id] = SSAValue(make_ssa!(ci, code, idx, id, typ))
                    else
                        code[idx] = nothing
                        incoming_vals[id] = undef_token
                    end
                    eidx = item
                    while haskey(exc_handlers, eidx)
                        (eidx, exc) = exc_handlers[eidx]
                        cidx = findfirst(x->slot_id(x[1]) == id, phicnodes[exc])
                        if cidx !== nothing
                            node = UpsilonNode(incoming_vals[id])
                            if incoming_vals[id] === undef_token
                                node = UpsilonNode()
                                typ = MaybeUndef(Union{})
                            end
                            push!(phicnodes[exc][cidx][3].values,
                                NewSSAValue(insert_node!(ir, idx, typ, node, true).id - length(ir.stmts)))
                        end
                    end
                end
            end
        end
        for succ in cfg.blocks[item].succs
            push!(worklist, (succ, item, copy(incoming_vals)))
        end
    end
    # Delete any instruction in unreachable blocks (except for terminators)
    for bb in setdiff(BitSet(1:length(cfg.blocks)), visited)
        for idx in cfg.blocks[bb].stmts
            if isa(code[idx], Union{GotoNode, GotoIfNot, ReturnNode})
                code[idx] = ReturnNode()
            else
                code[idx] = nothing
            end
        end
    end
    # Convert into IRCode form
    nstmts = length(ir.stmts)
    new_code = Vector{Any}(undef, nstmts)
    ssavalmap = Any[SSAValue(-1) for _ in 0:length(ci.ssavaluetypes)]
    result_types = Any[Any for _ in 1:nstmts]
    # Detect statement positions for assignments and construct array
    for (bb, idx) in bbidxiter(ir)
        stmt = code[idx]
        # Convert GotoNode/GotoIfNot/PhiNode to BB addressing
        if isa(stmt, GotoNode)
            new_code[idx] = GotoNode(block_for_inst(cfg, stmt.label))
        elseif isa(stmt, GotoIfNot)
            new_dest = block_for_inst(cfg, stmt.dest)
            if new_dest == bb+1
                # Drop this node - it's a noop
                new_code[idx] = stmt.cond
            else
                new_code[idx] = GotoIfNot(stmt.cond, new_dest)
            end
        elseif isexpr(stmt, :enter)
            new_code[idx] = Expr(:enter, block_for_inst(cfg, stmt.args[1]))
            ssavalmap[idx] = SSAValue(idx) # Slot to store token for pop_exception
        elseif isexpr(stmt, :leave) || isexpr(stmt, :(=)) || isa(stmt, ReturnNode) ||
            isexpr(stmt, :meta) || isa(stmt, NewvarNode)
            new_code[idx] = stmt
        else
            ssavalmap[idx] = SSAValue(idx)
            result_types[idx] = ci.ssavaluetypes[idx]
            if isa(stmt, PhiNode)
                edges = Any[edge == 0 ? 0 : block_for_inst(cfg, edge) for edge in stmt.edges]
                new_code[idx] = PhiNode(edges, stmt.values)
            else
                new_code[idx] = stmt
            end
        end
    end
    for (_, nodes) in phicnodes
        for (_, ssa, node) in nodes
            new_typ = Union{}
            # TODO: This could just be the ones that depend on other phis
            push!(type_refine_phi, ssa.id)
            new_idx = ssa.id
            node = new_nodes.stmts[new_idx]
            phic_values = (node[:inst]::PhiCNode).values
            for i = 1:length(phic_values)
                orig_typ = typ = typ_for_val(phic_values[i], ci, sptypes, -1, slottypes)
                @assert !isa(typ, MaybeUndef)
                while isa(typ, DelayedTyp)
                    typ = types(ir)[typ.phi::NewSSAValue]
                end
                new_typ = tmerge(new_typ, typ)
            end
            node[:type] = new_typ
        end
    end
    # This is a bit awkward, because it basically duplicates what type
    # inference does. Ideally, we'd just use this representation earlier
    # to make sure phi nodes have accurate types
    changed = true
    while changed
        changed = false
        for new_idx in type_refine_phi
            node = new_nodes.stmts[new_idx]
            new_typ = recompute_type(node[:inst], ci, ir, sptypes, slottypes)
            if !(node[:type] ⊑ new_typ) || !(new_typ ⊑ node[:type])
                node[:type] = new_typ
                changed = true
            end
        end
    end
    for i in 1:length(result_types)
        rt_i = result_types[i]
        if rt_i isa DelayedTyp
            result_types[i] = types(ir)[rt_i.phi::NewSSAValue]
        end
    end
    for i = 1:length(new_nodes)
        local node = new_nodes.stmts[i]
        local typ = node[:type]
        if isa(typ, DelayedTyp)
            node[:type] = types(ir)[typ.phi::NewSSAValue]
        end
    end
    # Renumber SSA values
    @assert isempty(ir.stmts.type)
    resize!(ir.stmts.type, nstmts)
    for i in 1:nstmts
        local node = ir.stmts[i]
        node[:inst] = new_to_regular(renumber_ssa!(new_code[i], ssavalmap), nstmts)
        node[:type] = result_types[i]
    end
    for i = 1:length(new_nodes)
        local node = new_nodes.stmts[i]
        node[:inst] = new_to_regular(renumber_ssa!(node[:inst], ssavalmap), nstmts)
    end
    @timeit "domsort" ir = domsort_ssa!(ir, domtree)
    return ir
end
