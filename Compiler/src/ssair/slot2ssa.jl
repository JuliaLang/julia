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
        arg1 = stmt.args[1]
        if isa(arg1, SlotNumber)
            push!(result[slot_id(arg1)].defs, idx)
        end
        stmt = stmt.args[2]
    end
    if isa(stmt, SlotNumber)
        push!(result[slot_id(stmt)].uses, idx)
        return
    end
    for op in userefs(stmt)
        val = op[]
        if isa(val, SlotNumber)
            push!(result[slot_id(val)].uses, idx)
        end
    end
end

function scan_slot_def_use(nargs::Int, ci::CodeInfo, code::Vector{Any})
    nslots = length(ci.slotflags)
    result = SlotInfo[SlotInfo() for i = 1:nslots]
    # Set defs for arguments
    for var in result[1:nargs]
        push!(var.defs, 0)
    end
    for idx in 1:length(code)
        stmt = code[idx]
        scan_entry!(result, idx, stmt)
    end
    result
end

function renumber_ssa(stmt::SSAValue, ssanums::Vector{SSAValue}, new_ssa::Bool=false)
    id = stmt.id
    if id > length(ssanums)
        return stmt
    end
    val = ssanums[id]
    @assert val.id > 0
    return val
end

function renumber_ssa!(@nospecialize(stmt), ssanums::Vector{SSAValue}, new_ssa::Bool=false)
    isa(stmt, SSAValue) && return renumber_ssa(stmt, ssanums, new_ssa)
    return ssamap(val->renumber_ssa(val, ssanums, new_ssa), stmt)
end

function make_ssa!(ci::CodeInfo, code::Vector{Any}, idx::Int, @nospecialize(typ))
    stmt = code[idx]
    @assert isexpr(stmt, :(=))
    code[idx] = stmt.args[2]
    (ci.ssavaluetypes::Vector{Any})[idx] = typ
    return SSAValue(idx)
end

function new_to_regular(@nospecialize(stmt), new_offset::Int)
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, NewSSAValue)
            op[] = SSAValue(val.id + new_offset)
        end
    end
    return urs[]
end

function fixup_slot!(ir::IRCode, ci::CodeInfo, idx::Int, slot::Int, @nospecialize(ssa), @nospecialize(def_ssa))
    # We don't really have the information here to get rid of these.
    # We'll do so later
    if ssa === UNDEF_TOKEN
        insert_node!(ir, idx, NewInstruction(
            Expr(:throw_undef_if_not, ci.slotnames[slot], false), Any))
        return UNDEF_TOKEN
    elseif has_flag(ir.stmts[idx], IR_FLAG_NOTHROW)
        # if the `isdefined`-ness of this slot is guaranteed by abstract interpretation,
        # there is no need to form a `:throw_undef_if_not`
    elseif def_ssa !== true
        insert_node!(ir, idx, NewInstruction(
            Expr(:throw_undef_if_not, ci.slotnames[slot], def_ssa), Any))
    end
    return ssa
end

function fixemup!(@specialize(slot_filter), @specialize(rename_slot), ir::IRCode, ci::CodeInfo, idx::Int, @nospecialize(stmt))
    if isa(stmt, SlotNumber) && slot_filter(stmt)
        return fixup_slot!(ir, ci, idx, slot_id(stmt), rename_slot(stmt)...)
    end
    if isexpr(stmt, :(=))
        stmt.args[2] = fixemup!(slot_filter, rename_slot, ir, ci, idx, stmt.args[2])
        return stmt
    end
    if isa(stmt, PhiNode)
        for i = 1:length(stmt.edges)
            isassigned(stmt.values, i) || continue
            val = stmt.values[i]
            isa(val, SlotNumber) || continue
            slot_filter(val) || continue
            bb_idx = block_for_inst(ir.cfg, Int(stmt.edges[i]))
            from_bb_terminator = last(ir.cfg.blocks[bb_idx].stmts)
            stmt.values[i] = fixup_slot!(ir, ci, from_bb_terminator, slot_id(val), rename_slot(val)...)
        end
        return stmt
    end
    if isexpr(stmt, :isdefined)
        val = stmt.args[1]
        if isa(val, SlotNumber)
            ssa, undef_ssa = rename_slot(val)
            return undef_ssa
        end
        return stmt
    end
    urs = userefs(stmt)
    for op in urs
        val = op[]
        if isa(val, SlotNumber) && slot_filter(val)
            x = fixup_slot!(ir, ci, idx, slot_id(val), rename_slot(val)...)
            # We inserted an undef error node. Delete subsequent statement
            # to avoid confusing the optimizer
            if x === UNDEF_TOKEN
                return nothing
            end
            op[] = x
        elseif isexpr(val, :static_parameter)
            ty = typ_for_val(val, ci, ir, idx, Any[])
            if isa(ty, Const)
                inst = NewInstruction(quoted(ty.val), ty)
            else
                inst = NewInstruction(val, ty)
            end
            op[] = NewSSAValue(insert_node!(ir, idx, inst).id - length(ir.stmts))
        end
    end
    return urs[]
end

function fixup_uses!(ir::IRCode, ci::CodeInfo, code::Vector{Any}, uses::Vector{Int}, slot::Int, @nospecialize(ssa))
    for use in uses
        code[use] = fixemup!(x::SlotNumber->slot_id(x)==slot, ::SlotNumber->Pair{Any,Any}(ssa, true), ir, ci, use, code[use])
    end
end

function rename_uses!(ir::IRCode, ci::CodeInfo, idx::Int, @nospecialize(stmt), renames::Vector{Pair{Any, Any}})
    return fixemup!(::SlotNumber->true, x::SlotNumber->renames[slot_id(x)], ir, ci, idx, stmt)
end

# maybe use expr_type?
function typ_for_val(@nospecialize(x), ci::CodeInfo, ir::IRCode, idx::Int, slottypes::Vector{Any})
    if isa(x, Expr)
        if x.head === :static_parameter
            return ir.sptypes[x.args[1]::Int].typ
        elseif x.head === :boundscheck
            return Bool
        elseif x.head === :copyast
            return typ_for_val(x.args[1], ci, ir, idx, slottypes)
        end
        return (ci.ssavaluetypes::Vector{Any})[idx]
    end
    isa(x, GlobalRef) && return abstract_eval_globalref_type(x, ci)
    isa(x, SSAValue) && return (ci.ssavaluetypes::Vector{Any})[x.id]
    isa(x, Argument) && return slottypes[x.n]
    isa(x, NewSSAValue) && return types(ir)[new_to_regular(x, length(ir.stmts))]
    isa(x, QuoteNode) && return Const(x.value)
    isa(x, Union{Symbol, PiNode, PhiNode, SlotNumber}) && error("unexpected val type")
    return Const(x)
end

struct BlockLiveness
    def_bbs::Vector{Int}
    live_in_bbs::Union{Vector{Int}, Nothing}
end

"""
    iterated_dominance_frontier(cfg::CFG, liveness::BlockLiveness, domtree::DomTree)
        -> phinodes::Vector{Int}

Run iterated dominance frontier.
The algorithm we have here essentially follows LLVM, which itself is a
a cleaned up version of the linear-time algorithm described in [^SG95].

The algorithm here is quite straightforward. Suppose we have a CFG:

    A -> B -> D -> F
     \\-> C ------>/

and a corresponding dominator tree:

    A
    |- B - D
    |- C
    |- F

Now, for every definition of our slot, we simply walk down the dominator
tree and look for any edges that leave the sub-domtree rooted by our definition.

In our example above, if we have a definition in `B`, we look at its successors,
which is only `D`, which is dominated by `B` and hence doesn't need a ϕ-node.
Then we descend down the subtree rooted at `B` and end up in `D`. `D` has a successor
`F`, which is not part of the current subtree, (i.e. not dominated by `B`),
so it needs a ϕ-node.

Now, the key insight of that algorithm is that we have two defs, in blocks `A` and `B`,
and `A` dominates `B`, then we do not need to recurse into `B`, because the set of
potential backedges from a subtree rooted at `B` (to outside the subtree) is a strict
subset of those backedges from a subtree rooted at `A` (out outside the subtree rooted
at `A`). Note however that this does not work the other way. Thus, the algorithm
needs to make sure that we always visit `B` before `A`.

[^SG95]: Vugranam C. Sreedhar and Guang R. Gao. 1995.
         A linear time algorithm for placing φ-nodes.
         In Proceedings of the 22nd ACM SIGPLAN-SIGACT symposium on Principles of programming languages (POPL '95).
         Association for Computing Machinery, New York, NY, USA, 62–73.
         DOI: <https://doi.org/10.1145/199448.199464>.
"""
function iterated_dominance_frontier(cfg::CFG, liveness::BlockLiveness, domtree::DomTree)
    defs = liveness.def_bbs
    heap = Tuple{Int, Int}[(defs[i], domtree.nodes[defs[i]].level) for i in 1:length(defs)]
    heap_order = By(x -> -x[2])
    heapify!(heap, heap_order)
    phiblocks = Int[]
    # This bitset makes sure we only add a phi node to a given block once.
    processed = BitSet()
    # This bitset implements the `key insight` mentioned above. In particular, it prevents
    # us from visiting a subtree that we have already visited before.
    visited = BitSet()
    while !isempty(heap)
        # We pop from the end of the array - i.e. the element with the highest level.
        node, level = heappop!(heap, heap_order)
        worklist = Int[]
        push!(worklist, node)
        while !isempty(worklist)
            active = pop!(worklist)
            succs = cfg.blocks[active].succs
            for succ in succs
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
                if liveness.live_in_bbs !== nothing && !(succ in liveness.live_in_bbs)
                    continue
                end
                push!(phiblocks, succ)
                # Basically: Consider the phi node we just added another
                # def of this value. N.B.: This needs to retain the invariant that it
                # is processed before any of its parents in the dom tree. This is guaranteed,
                # because succ_level <= level, which is the greatest level we have currently
                # processed. Thus, we have not yet processed any subtrees of level < succ_level.
                if !(succ in defs)
                    heappush!(heap, (succ, succ_level), heap_order)
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

function rename_incoming_edge(old_edge::Int, old_to::Int, result_order::Vector{Int}, bb_rename::Vector{Int})
    old_edge == 0 && return 0
    new_edge_from = bb_rename[old_edge]
    new_edge_from < 0 && return new_edge_from
    if old_edge == old_to - 1
        # Could have been a crit edge break
        if new_edge_from < length(result_order) && result_order[new_edge_from + 1] == 0
            new_edge_from += 1
        end
    end
    new_edge_from
end

function rename_outgoing_edge(old_to::Int, old_from::Int, result_order::Vector{Int}, bb_rename::Vector{Int})
    new_edge_to = bb_rename[old_to]
    if old_from == old_to - 1
        # Could have been a crit edge break
        if bb_rename[old_from] < length(result_order) && result_order[bb_rename[old_from]+1] == 0
            new_edge_to = bb_rename[old_from] + 1
        end
    end
    new_edge_to
end

function rename_phinode_edges(node::PhiNode, bb::Int, result_order::Vector{Int}, bb_rename::Vector{Int})
    new_values = Any[]
    new_edges = Int32[]
    for (idx, edge) in pairs(node.edges)
        edge = Int(edge)
        (edge == 0 || bb_rename[edge] != -1) || continue
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
Sort the basic blocks in `ir` into domtree order (i.e. if `bb1` is higher in
the domtree than `bb2`, it will come first in the linear order). The resulting
`ir` has the property that a linear traversal of basic blocks will also be a
RPO traversal and in particular, any use of an SSA value must come after
(by linear order) its definition.
"""
function domsort_ssa!(ir::IRCode, domtree::DomTree)
    # Mapping from new → old BB index
    # An "old" index of 0 means that this was a BB inserted as part of a fixup (see below)
    result_order = Int[]

    # Mapping from old → new BB index
    bb_rename = fill(-1, length(ir.cfg.blocks))

    # The number of GotoNodes we need to insert to preserve control-flow after sorting
    nfixupstmts = 0

    # node queued up for scheduling (-1 === nothing)
    node_to_schedule = 1
    worklist = Int[]
    while node_to_schedule !== -1
        # First assign a new BB index to `node_to_schedule`
        push!(result_order, node_to_schedule)
        bb_rename[node_to_schedule] = length(result_order)
        cs = domtree.nodes[node_to_schedule].children
        terminator = ir[SSAValue(last(ir.cfg.blocks[node_to_schedule].stmts))][:stmt]
        fallthrough = node_to_schedule + 1
        node_to_schedule = -1

        # Adding the nodes in reverse sorted order attempts to retain
        # the original source order of the nodes as much as possible.
        # This is not required for correctness, but is easier on the humans
        for node in Iterators.Reverse(cs)
            if node == fallthrough
                # Schedule the fall through node first,
                # so we can retain the fall through
                node_to_schedule = node
            else
                push!(worklist, node)
            end
        end
        if node_to_schedule == -1 && !isempty(worklist)
            node_to_schedule = pop!(worklist)
        end
        # If a fallthrough successor is no longer the fallthrough after sorting, we need to
        # add a GotoNode (and either extend or split the basic block as necessary)
        if node_to_schedule != fallthrough && !isa(terminator, Union{GotoNode, ReturnNode})
            if isa(terminator, GotoIfNot)
                # Need to break the critical edge
                push!(result_order, 0)
            elseif isa(terminator, EnterNode) || isexpr(terminator, :leave)
                # Cannot extend the BasicBlock with a goto, have to split it
                push!(result_order, 0)
            else
                # No need for a new block, just extend
                @assert !isterminator(terminator)
            end
            # Reserve space for the fixup goto
            nfixupstmts += 1
        end
    end
    new_bbs = Vector{BasicBlock}(undef, length(result_order))
    nstmts = 0
    for i in result_order
        if i !== 0
            nstmts += length(ir.cfg.blocks[i].stmts)
        end
    end
    result = InstructionStream(nstmts + nfixupstmts)
    inst_rename = Vector{SSAValue}(undef, length(ir.stmts) + length(ir.new_nodes))
    @inbounds for i = 1:length(ir.stmts)
        inst_rename[i] = SSAValue(-1)
    end
    @inbounds for i = 1:length(ir.new_nodes)
        inst_rename[i + length(ir.stmts)] = SSAValue(i + length(result))
    end
    bb_start_off = 0
    for (new_bb, bb) in pairs(result_order)
        if bb == 0
            nidx = bb_start_off + 1
            stmt = result[nidx][:stmt]
            @assert isa(stmt, GotoNode)
            # N.B.: The .label has already been renamed when it was created.
            new_bbs[new_bb] = BasicBlock(nidx:nidx, [new_bb - 1], [stmt.label])
            bb_start_off += 1
            continue
        end
        old_inst_range = ir.cfg.blocks[bb].stmts
        inst_range = (bb_start_off+1):(bb_start_off+length(old_inst_range))
        for (nidx, idx) in zip(inst_range, old_inst_range)
            inst_rename[idx] = SSAValue(nidx)
            @assert !isassigned(result.stmt, nidx)
            node = result[nidx]
            node[] = ir.stmts[idx]
            stmt = node[:stmt]
            if isa(stmt, PhiNode)
                node[:stmt] = rename_phinode_edges(stmt, bb, result_order, bb_rename)
            end
        end
        # Now fix up the terminator
        terminator = result[inst_range[end]][:stmt]
        if isa(terminator, GotoNode)
            # Convert to implicit fall through
            if bb_rename[terminator.label] == new_bb + 1
                result[inst_range[end]][:stmt] = nothing
            else
                result[inst_range[end]][:stmt] = GotoNode(bb_rename[terminator.label])
            end
        elseif isa(terminator, GotoIfNot) || isa(terminator, EnterNode) || isexpr(terminator, :leave)
            # Check if we need to break the critical edge or split the block
            if bb_rename[bb + 1] != new_bb + 1
                @assert result_order[new_bb + 1] == 0
                # Add an explicit goto node in the next basic block (we accounted for this above)
                nidx = inst_range[end] + 1
                node = result[nidx]
                node[:stmt], node[:type], node[:line] = GotoNode(bb_rename[bb + 1]), Any, NoLineUpdate
            end
            if isa(terminator, GotoIfNot)
                result[inst_range[end]][:stmt] = GotoIfNot(terminator.cond, bb_rename[terminator.dest])
            elseif isa(terminator, EnterNode)
                result[inst_range[end]][:stmt] = EnterNode(terminator, terminator.catch_dest == 0 ? 0 : bb_rename[terminator.catch_dest])
            else
                @assert isexpr(terminator, :leave)
            end
        elseif !isa(terminator, ReturnNode)
            if bb_rename[bb + 1] != new_bb + 1
                # Add an explicit goto node
                nidx = inst_range[end] + 1
                node = result[nidx]
                node[:stmt], node[:type], node[:line] = GotoNode(bb_rename[bb + 1]), Any, NoLineUpdate
                inst_range = first(inst_range):(last(inst_range) + 1)
            end
        end
        bb_start_off += length(inst_range)
        local new_preds, new_succs
        let bb = bb, bb_rename = bb_rename, result_order = result_order
            new_preds = Int[bb for bb in (rename_incoming_edge(i, bb, result_order, bb_rename) for i in ir.cfg.blocks[bb].preds) if bb != -1]
            new_succs = Int[              rename_outgoing_edge(i, bb, result_order, bb_rename) for i in ir.cfg.blocks[bb].succs]
        end
        new_bbs[new_bb] = BasicBlock(inst_range, new_preds, new_succs)
    end
    for i in 1:length(result)
        result[i][:stmt] = renumber_ssa!(result[i][:stmt], inst_rename, true)
    end
    cfg = CFG(new_bbs, Int[first(bb.stmts) for bb in new_bbs[2:end]])
    new_new_nodes = NewNodeStream(length(ir.new_nodes))
    for i = 1:length(ir.new_nodes)
        new_info = ir.new_nodes.info[i]
        new_new_info = NewNodeInfo(inst_rename[new_info.pos].id, new_info.attach_after)
        new_new_nodes.info[i] = new_new_info
        new_node = new_new_nodes.stmts[i]
        new_node[] = ir.new_nodes.stmts[i]
        new_node_inst = new_node[:stmt]
        if isa(new_node_inst, PhiNode)
            new_node_inst = rename_phinode_edges(new_node_inst, block_for_inst(ir.cfg, new_info.pos), result_order, bb_rename)
        end
        new_node[:stmt] = renumber_ssa!(new_node_inst, inst_rename, true)
    end
    ir.debuginfo.codelocs = result.line
    new_ir = IRCode(ir, result, cfg, new_new_nodes)
    return new_ir
end

compute_live_ins(cfg::CFG, slot::SlotInfo) = compute_live_ins(cfg, slot.defs, slot.uses)

function compute_live_ins(cfg::CFG, defs::Vector{Int}, uses::Vector{Int})
    # We remove from `uses` any block where all uses are dominated
    # by a def. This prevents insertion of dead phi nodes at the top
    # of such a block if that block happens to be in a loop
    bb_defs = Int[] # blocks with a def
    bb_uses = Int[] # blocks with a use that is not dominated by a def

    # We do a sorted joint iteration over the instructions listed
    # in defs and uses following a pattern similar to mergesort
    last_block, block_has_def = 0, false
    defs_i = uses_i = 1
    while defs_i <= lastindex(defs) || uses_i <= lastindex(uses)
        is_def = uses_i > lastindex(uses) || defs_i <= lastindex(defs) && defs[defs_i] < uses[uses_i]
        block = block_for_inst(cfg, is_def ? defs[defs_i] : uses[uses_i])
        defs_i += is_def
        uses_i += !is_def
        if last_block != block || is_def && !block_has_def
            push!(is_def ? bb_defs : bb_uses, block)
            block_has_def = is_def
        end
        last_block = block
    end
    # To obtain live ins from bb_uses, recursively add predecessors
    extra_liveins = BitSet()
    worklist = Int[]
    for bb in bb_uses
        append!(worklist, Iterators.filter(p->p != 0 && !(p in bb_defs), cfg.blocks[bb].preds))
    end
    while !isempty(worklist)
        elem = pop!(worklist)
        (elem in bb_uses || elem in extra_liveins) && continue
        push!(extra_liveins, elem)
        append!(worklist, Iterators.filter(p->p != 0 && !(p in bb_defs), cfg.blocks[elem].preds))
    end
    append!(bb_uses, extra_liveins)
    BlockLiveness(bb_defs, bb_uses)
end

struct TryCatchRegion
    enter_block::Int
    leave_block::Int
end
struct NewSlotPhi{Phi}
    ssaval::NewSSAValue
    node::Phi
    undef_ssaval::Union{NewSSAValue, Bool}
    undef_node::Union{Phi, Nothing}
end

const NewPhiNode2 = NewSlotPhi{PhiNode}

struct NewPhiCNode2
    slot::SlotNumber
    insert::NewSlotPhi{PhiCNode}
end

function construct_ssa!(ci::CodeInfo, ir::IRCode, sv::OptimizationState,
                        domtree::DomTree, defuses::Vector{SlotInfo},
                        𝕃ₒ::AbstractLattice)
    code = ir.stmts.stmt
    cfg = ir.cfg
    catch_entry_blocks = TryCatchRegion[]
    for idx in 1:length(code)
        stmt = code[idx]
        if isa(stmt, EnterNode)
            push!(catch_entry_blocks, TryCatchRegion(
                block_for_inst(cfg, idx),
                block_for_inst(cfg, stmt.catch_dest)))
        end
    end

    # Record the correct exception handler for all critical sections
    handler_info = compute_trycatch(code)

    phi_slots = Vector{Int}[Int[] for _ = 1:length(ir.cfg.blocks)]
    live_slots = Vector{Int}[Int[] for _ = 1:length(ir.cfg.blocks)]
    new_phi_nodes = Vector{NewPhiNode2}[NewPhiNode2[] for _ = 1:length(cfg.blocks)]
    new_phic_nodes = IdDict{Int, Vector{NewPhiCNode2}}()
    for (; leave_block) in catch_entry_blocks
        new_phic_nodes[leave_block] = NewPhiCNode2[]
    end
    @timeit "idf" for (idx, slot) in Iterators.enumerate(defuses)
        # No uses => no need for phi nodes
        isempty(slot.uses) && continue
        # TODO: Restore this optimization
        if false # length(slot.defs) == 1 && slot.any_newvar
            if slot.defs[] == 0
                typ = sv.slottypes[idx]
                ssaval = Argument(idx)
                fixup_uses!(ir, ci, code, slot.uses, idx, ssaval)
            elseif isa(code[slot.defs[]], NewvarNode)
                typ = Union{}
                ssaval = nothing
                for use in slot.uses[]
                    insert_node!(ir, use,
                        NewInstruction(Expr(:throw_undef_if_not, ci.slotnames[idx], false), Union{}))
                end
                fixup_uses!(ir, ci, code, slot.uses, idx, nothing)
            else
                val = code[slot.defs[]].args[2]
                typ = typ_for_val(val, ci, ir, slot.defs[], sv.slottypes)
                ssaval = make_ssa!(ci, code, slot.defs[], typ)
                fixup_uses!(ir, ci, code, slot.uses, idx, ssaval)
            end
            continue
        end

        @timeit "liveness" (live = compute_live_ins(cfg, slot))
        for li in live.live_in_bbs
            push!(live_slots[li], idx)
            cidx = findfirst(x::TryCatchRegion->x.leave_block==li, catch_entry_blocks)
            if cidx !== nothing
                # The slot is live-in into this block. We need to
                # Create a PhiC node in the catch entry block and
                # an upsilon node in the corresponding enter block
                varstate = sv.bb_vartables[li]
                if varstate === nothing
                    continue
                end
                node = PhiCNode(Any[])
                insertpoint = first_insert_for_bb(code, cfg, li)
                vt = varstate[idx]
                phic_ssa = NewSSAValue(
                    insert_node!(ir, insertpoint,
                        NewInstruction(node, vt.typ)).id - length(ir.stmts))
                undef_node = undef_ssaval = nothing
                if vt.typ === Union{}
                    undef_ssaval = false
                elseif !vt.undef
                    undef_ssaval = true
                else
                    undef_node = PhiCNode(Any[])
                    undef_ssaval = NewSSAValue(insert_node!(ir,
                        insertpoint, NewInstruction(undef_node, Bool)).id - length(ir.stmts))
                end
                push!(new_phic_nodes[li], NewPhiCNode2(SlotNumber(idx),
                    NewSlotPhi{PhiCNode}(phic_ssa, node, undef_ssaval, undef_node)))
                # Inform IDF that we now have a def in the catch block
                if !(li in live.def_bbs)
                    push!(live.def_bbs, li)
                end
            end
        end
        phiblocks = iterated_dominance_frontier(cfg, live, domtree)
        for block in phiblocks
            push!(phi_slots[block], idx)
            node = PhiNode()
            varstate = sv.bb_vartables[block]
            @assert varstate !== nothing
            vt = varstate[idx]
            ssaval = NewSSAValue(insert_node!(ir,
                first_insert_for_bb(code, cfg, block), NewInstruction(node, vt.typ)).id - length(ir.stmts))
            undef_node = undef_ssaval = nothing
            if vt.typ === Union{}
                undef_ssaval = false
            elseif !vt.undef
                undef_ssaval = true
            else
                undef_node = PhiNode()
                undef_ssaval = NewSSAValue(insert_node!(ir,
                    first_insert_for_bb(code, cfg, block), NewInstruction(undef_node, Bool)).id - length(ir.stmts))
            end
            push!(new_phi_nodes[block], NewPhiNode2(ssaval, node, undef_ssaval, undef_node))
        end
    end
    # Perform SSA renaming
    initial_incoming_vals = Pair{Any, Any}[
        if 0 in defuses[x].defs
            Pair{Any, Any}(Argument(x), true)
        elseif !defuses[x].any_newvar
            Pair{Any, Any}(UNDEF_TOKEN, false)
        else
            Pair{Any, Any}(SSAValue(-2), false)
        end for x in 1:length(ci.slotflags)
    ]
    worklist = Tuple{Int, Int, Vector{Pair{Any, Any}}}[(1, 0, initial_incoming_vals)]
    visited = BitSet()
    new_nodes = ir.new_nodes
    @timeit "SSA Rename" while !isempty(worklist)
        (item, pred, incoming_vals) = pop!(worklist)
        if sv.bb_vartables[item] === nothing
            continue
        end
        # Rename existing phi nodes first, because their uses occur on the edge
        # TODO: This isn't necessary if inlining stops replacing arguments by slots.
        for idx in cfg.blocks[item].stmts
            stmt = code[idx]
            if isexpr(stmt, :(=))
                stmt = stmt.args[2]
            end
            isa(stmt, PhiNode) || continue
            for (edgeidx, edge) in pairs(stmt.edges)
                from_bb = edge == 0 ? 0 : block_for_inst(cfg, Int(edge))
                from_bb == pred || continue
                isassigned(stmt.values, edgeidx) || break
                stmt.values[edgeidx] = rename_uses!(ir, ci, Int(edge), stmt.values[edgeidx], incoming_vals)
                break
            end
        end
        # Insert phi nodes if necessary
        for (idx, slot) in Iterators.enumerate(phi_slots[item])
            (; ssaval, node, undef_ssaval, undef_node) = new_phi_nodes[item][idx]
            (incoming_val, incoming_def) = incoming_vals[slot]
            if incoming_val === SSAValue(-1)
                # Optimistically omit this path.
                # Liveness analysis would probably have prevented us from inserting this phi node
                continue
            end
            push!(node.edges, pred)
            if incoming_val === UNDEF_TOKEN
                resize!(node.values, length(node.values)+1)
            else
                push!(node.values, incoming_val)
            end
            if undef_node !== nothing
                push!(undef_node.edges, pred)
                push!(undef_node.values, incoming_def)
            end

            incoming_vals[slot] = Pair{Any, Any}(ssaval, undef_ssaval)
        end
        (item in visited) && continue
        # Record phi_C nodes if necessary
        if haskey(new_phic_nodes, item)
            for (; slot, insert) in new_phic_nodes[item]
                (; ssaval, undef_ssaval) = insert
                incoming_vals[slot_id(slot)] = Pair{Any, Any}(ssaval, undef_ssaval)
            end
        end
        # Record Pi nodes if necessary
        has_pinode = fill(false, length(sv.slottypes))
        for slot in live_slots[item]
            (ival, idef) = incoming_vals[slot]
            (ival === SSAValue(-1)) && continue
            (ival === SSAValue(-2)) && continue
            (ival === UNDEF_TOKEN) && continue

            varstate = sv.bb_vartables[item]
            @assert varstate !== nothing
            typ = varstate[slot].typ
            if !⊑(𝕃ₒ, sv.slottypes[slot], typ)
                node = PiNode(ival, typ)
                ival = NewSSAValue(insert_node!(ir,
                    first_insert_for_bb(code, cfg, item), NewInstruction(node, typ)).id - length(ir.stmts))
                incoming_vals[slot] = Pair{Any, Any}(ival, idef)
                has_pinode[slot] = true
            end
        end
        # Record initial upsilon nodes if necessary
        eidx = findfirst((; enter_block)::TryCatchRegion->enter_block==item, catch_entry_blocks)
        if eidx !== nothing
            for (; slot, insert) in new_phic_nodes[catch_entry_blocks[eidx].leave_block]
                (; node, undef_node) = insert
                (ival, idef) = incoming_vals[slot_id(slot)]
                ivalundef = ival === UNDEF_TOKEN
                Υ = NewInstruction(ivalundef ? UpsilonNode() : UpsilonNode(ival),
                                   ivalundef ? Union{} : typ_for_val(ival, ci, ir, -1, sv.slottypes))
                insertpos = first_insert_for_bb(code, cfg, item)
                # insert `UpsilonNode` immediately before the `:enter` expression
                Υssa = insert_node!(ir, insertpos, Υ)
                push!(node.values, NewSSAValue(Υssa.id - length(ir.stmts)))
                if undef_node !== nothing
                    Υundef = NewInstruction(UpsilonNode(idef), Bool)
                    Υssaundef = insert_node!(ir, insertpos, Υundef)
                    push!(undef_node.values, NewSSAValue(Υssaundef.id - length(ir.stmts)))
                end
            end
        end
        push!(visited, item)
        for idx in cfg.blocks[item].stmts
            stmt = code[idx]
            (isa(stmt, PhiNode) || (isexpr(stmt, :(=)) && isa(stmt.args[2], PhiNode))) && continue
            if isa(stmt, NewvarNode)
                incoming_vals[slot_id(stmt.slot)] = Pair{Any, Any}(UNDEF_TOKEN, false)
                has_pinode[slot_id(stmt.slot)] = false
                code[idx] = nothing
            else
                stmt = rename_uses!(ir, ci, idx, stmt, incoming_vals)
                if stmt === nothing && isa(code[idx], Union{ReturnNode, GotoIfNot}) && idx == last(cfg.blocks[item].stmts)
                    # preserve the CFG
                    stmt = ReturnNode()
                end
                code[idx] = stmt
                # Record a store
                if isexpr(stmt, :(=))
                    arg1 = stmt.args[1]
                    if isa(arg1, SlotNumber)
                        id = slot_id(arg1)
                        val = stmt.args[2]
                        typ = typ_for_val(val, ci, ir, idx, sv.slottypes)
                        # Having UNDEF_TOKEN appear on the RHS is possible if we're on a dead branch.
                        # Do something reasonable here, by marking the LHS as undef as well.
                        if val !== UNDEF_TOKEN
                            thisdef = true
                            thisval = make_ssa!(ci, code, idx, typ)
                        else
                            code[idx] = nothing
                            thisval = UNDEF_TOKEN
                            thisdef = false
                        end
                        incoming_vals[id] = Pair{Any, Any}(thisval, thisdef)
                        has_pinode[id] = false
                        enter_idx = idx
                        while (handler = gethandler(handler_info, enter_idx)) !== nothing
                            enter_idx = get_enter_idx(handler)
                            enter_node = code[enter_idx]::EnterNode
                            leave_block = block_for_inst(cfg, enter_node.catch_dest)
                            cidx = findfirst((; slot)::NewPhiCNode2->slot_id(slot)==id,
                                new_phic_nodes[leave_block])
                            if cidx !== nothing
                                node = thisdef ? UpsilonNode(thisval) : UpsilonNode()
                                if incoming_vals[id] === UNDEF_TOKEN
                                    node = UpsilonNode()
                                    typ = Union{}
                                end
                                insert = new_phic_nodes[leave_block][cidx].insert
                                push!(insert.node.values,
                                      NewSSAValue(insert_node!(ir, idx, NewInstruction(node, typ), true).id - length(ir.stmts)))
                                if insert.undef_node !== nothing
                                    push!(insert.undef_node.values,
                                          NewSSAValue(insert_node!(ir, idx, NewInstruction(UpsilonNode(thisdef), Bool), true).id - length(ir.stmts)))
                                end
                            end
                        end
                    end
                end
            end
        end
        # Unwrap any PiNodes before continuing, since they weren't considered during our
        # dominance frontier calculation and so have to be used locally in each BB.
        for (i, (ival, idef)) in enumerate(incoming_vals)
            if has_pinode[i]
                stmt = ir[new_to_regular(ival::NewSSAValue, length(ir.stmts))][:stmt]
                incoming_vals[i] = Pair{Any, Any}(stmt.val, idef)
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
    ssavaluetypes = ci.ssavaluetypes::Vector{Any}
    nstmts = length(ir.stmts)
    new_code = Vector{Any}(undef, nstmts)
    ssavalmap = fill(SSAValue(-1), length(ssavaluetypes) + 1)
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
                new_code[idx] = Expr(:call, GlobalRef(Core, :typeassert), stmt.cond, GlobalRef(Core, :Bool))
            else
                new_code[idx] = GotoIfNot(stmt.cond, new_dest)
            end
        elseif isa(stmt, EnterNode)
            except_bb = stmt.catch_dest == 0 ? 0 : block_for_inst(cfg, stmt.catch_dest)
            new_code[idx] = EnterNode(stmt, except_bb)
            ssavalmap[idx] = SSAValue(idx) # Slot to store token for pop_exception
        elseif isexpr(stmt, :leave) || isexpr(stmt, :(=)) || isa(stmt, ReturnNode) ||
            isexpr(stmt, :meta) || isa(stmt, NewvarNode)
            new_code[idx] = stmt
        else
            ssavalmap[idx] = SSAValue(idx)
            if isa(stmt, PhiNode)
                edges = Int32[edge == 0 ? 0 : block_for_inst(cfg, Int(edge)) for edge in stmt.edges]
                new_code[idx] = PhiNode(edges, stmt.values)
            else
                new_code[idx] = stmt
            end
        end
    end
    # Renumber SSA values
    @assert isempty(ir.stmts.type)
    resize!(ir.stmts.type, nstmts)
    for i in 1:nstmts
        local node = ir.stmts[i]
        node[:stmt] = new_to_regular(renumber_ssa!(new_code[i], ssavalmap), nstmts)
        node[:type] = ssavaluetypes[i]
    end
    for i = 1:length(new_nodes)
        local node = new_nodes.stmts[i]
        node[:stmt] = new_to_regular(renumber_ssa!(node[:stmt], ssavalmap), nstmts)
    end
    @timeit "domsort" ir = domsort_ssa!(ir, domtree)
    return ir
end
