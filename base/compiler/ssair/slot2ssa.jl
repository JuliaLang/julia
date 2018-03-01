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

@inline slot_id(s) = isa(s, SlotNumber) ? (s::SlotNumber).id : (s::TypedSlot).id
function scan_slot_def_use(nargs, ci::CodeInfo)
    nslots = length(ci.slotnames)
    result = SlotInfo[SlotInfo() for i = 1:nslots]
    # Set defs for arguments
    for var in result[1:(1+nargs)]
        push!(var.defs, 0)
    end
    for (idx, stmt) in Iterators.enumerate(ci.code)
        scan_entry!(result, idx, stmt)
    end
    result
end

function renumber_ssa(stmt::SSAValue, ssanums::Vector{Any}, new_ssa::Bool=false, used_ssa::Union{Nothing, Vector{Int}}=nothing)
    id = stmt.id + (new_ssa ? 0 : 1)
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

function make_ssa!(ci::CodeInfo, idx, slot, @nospecialize(typ))
    (idx == 0) && return Argument(slot)
    stmt = ci.code[idx]
    @assert isexpr(stmt, :(=))
    push!(ci.ssavaluetypes, typ)
    ssa = length(ci.ssavaluetypes)-1
    stmt.args[1] = SSAValue(ssa)
    ssa
end

struct UndefToken
end
const undef_token = UndefToken()

function new_to_regular(@nospecialize(stmt))
    if isa(stmt, NewSSAValue)
        return SSAValue(stmt.id)
    end
    urs = userefs(stmt)
    urs === () && return stmt
    for op in urs
        val = op[]
        if isa(val, NewSSAValue)
            op[] = SSAValue(val.id)
        end
    end
    urs[]
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
        return NewSSAValue(insert_node!(ir, idx, stmt.typ, PiNode(ssa, stmt.typ)).id)
    end
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
    urs === () && return stmt
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
    urs[]
end

function fixup_uses!(ir::IRCode, ci::CodeInfo, uses::Vector{Int}, slot, @nospecialize(ssa))
    for use in uses
        ci.code[use] = fixemup!(stmt->slot_id(stmt)==slot, stmt->ssa, ir, ci, use, ci.code[use])
    end
end

function rename_uses!(ir::IRCode, ci::CodeInfo, idx::Int, @nospecialize(stmt), renames::Vector{Any})
    return fixemup!(stmt->true, stmt->renames[slot_id(stmt)], ir, ci, idx, stmt)
end

function strip_trailing_junk!(code::Vector{Any}, lines::Vector{Int})
    # Remove `nothing`s at the end, we don't handle them well
    # (we expect the last instruction to be a terminator)
    for i = length(code):-1:1
        if code[i] !== nothing
            resize!(code, i)
            resize!(lines, i)
            break
        end
    end
    # If the last instruction is not a terminator, add one. This can
    # happen for implicit return on dead branches.
    term = code[end]
    if !isa(term, GotoIfNot) && !isa(term, GotoNode) && !isa(term, ReturnNode)
        push!(code, ReturnNode{Any}())
        push!(lines, 0)
    end
    return code
end

struct DelayedTyp
    phi::NewSSAValue
end

# maybe use expr_type?
function typ_for_val(@nospecialize(val), ci::CodeInfo)
    isa(val, Expr) && return val.typ
    isa(val, GlobalRef) && return abstract_eval_global(val.mod, val.name)
    isa(val, SSAValue) && return ci.ssavaluetypes[val.id+1]
    isa(val, Argument) && return ci.slottypes[val.n]
    isa(val, NewSSAValue) && return DelayedTyp(val)
    isa(val, QuoteNode) && return Const(val.value)
    isa(val, Union{Symbol, PiNode, PhiNode, SlotNumber, TypedSlot}) && error("unexpected val type")
    return Const(val)
end

# Run iterated dominance frontier
function idf(cfg::CFG, defuse, domtree::DomTree, slot::Int)
    # This should be a priority queue, but TODO - sorted array for now
    defs = defuse[slot].defs
    pq = Tuple{Int, Int}[(defs[i], domtree.nodes[defs[i]].level) for i in 1:length(defs)]
    sort!(pq, by=x->x[2])
    phiblocks = Int[]
    processed = IdSet{Int}()
    while !isempty(pq)
        node, level = pop!(pq)
        worklist = Int[]
        visited = IdSet{Int}()
        push!(worklist, node)
        while !isempty(worklist)
            active = pop!(worklist)
            for succ in cfg.blocks[active].succs
                succ_level = domtree.nodes[succ].level
                succ_level > level && continue
                succ in processed && continue
                push!(processed, succ)
                # <- TODO: Use liveness here
                push!(phiblocks, succ)
                if !(succ in defuse[slot].defs)
                    push!(pq, (succ, succ_level))
                    sort!(pq, by=x->x[2])
                end
            end

            for child in domtree.nodes[active].children
                child in visited && continue
                push!(visited, child)
                push!(worklist, child)
            end
        end
    end
    phiblocks
end

function construct_ssa!(ci::CodeInfo, ir::IRCode, domtree::DomTree, defuse, nargs::Int)
    cfg = ir.cfg
    left = Int[]
    defuse_blocks = lift_defuse(ir.cfg, defuse)
    phi_slots = Vector{Int}[Vector{Int}() for _ = 1:length(ir.cfg.blocks)]
    phi_nodes = Vector{Pair{Int,PhiNode}}[Vector{Pair{Int,PhiNode}}() for _ = 1:length(cfg.blocks)]
    phi_ssas = SSAValue[]
    for (idx, slot) in Iterators.enumerate(defuse)
        # No uses => no need for phi nodes
        isempty(slot.uses) && continue
        # TODO: Restore this optimization
        if false # length(slot.defs) == 1 && slot.any_newvar
            if slot.defs[] == 0
                typ = ci.slottypes[idx]
                ssaval = Argument(idx)
                fixup_uses!(ir, ci, slot.uses, idx, ssaval)
            elseif isa(ci.code[slot.defs[]], NewvarNode)
                typ = MaybeUndef(Union{})
                ssaval = nothing
                for use in slot.uses[]
                    insert_node!(ir, use, Union{}, Expr(:throw_undef_if_not, ci.slotnames[idx], false))
                end
                fixup_uses!(ir, ci, slot.uses, idx, nothing)
            else
                val = ci.code[slot.defs[]].args[2]
                typ = typ_for_val(val, ci)
                ssaval = SSAValue(make_ssa!(ci, slot.defs[], idx, typ))
                fixup_uses!(ir, ci, slot.uses, idx, ssaval)
            end
            continue
        end
        # TODO: Perform liveness here to eliminate dead phi nodes
        phiblocks = idf(cfg, defuse_blocks, domtree, idx)
        for block in phiblocks
            push!(phi_slots[block], idx)
            node = PhiNode()
            ssa = insert_node!(ir, first_insert_for_bb(ci.code, cfg, block), Union{}, node)
            push!(phi_nodes[block], ssa.id=>node)
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
            SSAValue(-1)
        end for x in 1:length(ci.slotnames)
    ]
    worklist = Any[(1, 0, initial_incoming_vals)]
    visited = IdSet{Int}()
    type_refine_phi = IdSet{Int}()
    while !isempty(worklist)
        (item, pred, incoming_vals) = pop!(worklist)
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
            push!(type_refine_phi, ssaval)
            if isa(incoming_val, NewSSAValue)
                push!(type_refine_phi, ssaval)
            end
            typ = incoming_val == undef_token ? MaybeUndef(Union{}) : typ_for_val(incoming_val, ci)
            new_node_id = ssaval - length(ir.stmts)
            old_insert, old_typ, _, old_line = ir.new_nodes[new_node_id]
            if isa(typ, DelayedTyp)
                push!(type_refine_phi, ssaval)
            end
            new_typ = isa(typ, DelayedTyp) ? Union{} : tmerge(old_typ, typ)
            ir.new_nodes[new_node_id] = (old_insert, new_typ, node, old_line)
            incoming_vals[slot] = NewSSAValue(ssaval)
        end
        (item in visited) && continue
        push!(visited, item)
        for idx in cfg.blocks[item].stmts
            stmt = ci.code[idx]
            if isa(stmt, NewvarNode)
                incoming_vals[slot_id(stmt.slot)] = undef_token
                ci.code[idx] = nothing
            else
                stmt = rename_uses!(ir, ci, idx, stmt, incoming_vals)
                if stmt === nothing && idx == last(cfg.blocks[item].stmts)
                    # preserve the CFG
                    stmt = ReturnNode{Any}()
                end
                ci.code[idx] = stmt
                # Record a store
                if isexpr(stmt, :(=)) && isa(stmt.args[1], SlotNumber)
                    id = slot_id(stmt.args[1])
                    val = stmt.args[2]
                    typ = typ_for_val(val, ci)
                    incoming_vals[id] = SSAValue(make_ssa!(ci, idx, id, typ))
                end
            end
        end
        for succ in cfg.blocks[item].succs
            push!(worklist, (succ, item, copy(incoming_vals)))
        end
    end
    # Delete any instruction in unreachable blocks
    for bb in setdiff(IdSet{Int}(1:length(cfg.blocks)), visited)
        for idx in cfg.blocks[bb].stmts
            ci.code[idx] = nothing
        end
    end
    # Convert into IRCode form
    code = ir.stmts
    ssavalmap = Any[SSAValue(-1) for _ in 1:(length(ci.ssavaluetypes)+1)]
    types = Any[Any for _ in 1:length(code)]
    # Detect statement positions for assignments and construct array
    for (idx, stmt) in Iterators.enumerate(ci.code)
        if isexpr(stmt, :(=)) && isa(stmt.args[1], SSAValue)
            ssavalmap[stmt.args[1].id + 1] = SSAValue(idx)
            types[idx] = ci.ssavaluetypes[stmt.args[1].id + 1]
            stmt = stmt.args[2]
            if isa(stmt, PhiNode)
                edges = Any[block_for_inst(cfg, edge) for edge in stmt.edges]
                code[idx] = PhiNode(edges, stmt.values)
            else
                code[idx] = stmt
            end
        # Convert GotoNode/GotoIfNot/PhiNode to BB addressing
        elseif isa(stmt, GotoNode)
            code[idx] = GotoNode(block_for_inst(cfg, stmt.label))
        elseif isa(stmt, GotoIfNot)
            code[idx] = GotoIfNot(stmt.cond, block_for_inst(cfg, stmt.dest))
        else
            code[idx] = stmt
        end
    end
    # This is a bit awkward, because it basically duplicates what type
    # inference does. Ideally, we'd just use this representation earlier
    # to make sure phi nodes have accurate types
    changed = true
    while changed
        changed = false
        for phi in type_refine_phi
            new_idx = phi - length(ir.stmts)
            old_insert, old_typ, node, old_line = ir.new_nodes[new_idx]
            new_typ = Union{}
            for i = 1:length(node.values)
                if !isassigned(node.values, i)
                    if !isa(new_typ, MaybeUndef)
                        new_typ = MaybeUndef(new_typ)
                    end
                    continue
                end
                typ = typ_for_val(node.values[i], ci)
                if isa(typ, DelayedTyp)
                    typ = ir.new_nodes[typ.phi.id - length(ir.stmts)][2]
                end
                new_typ = tmerge(new_typ, typ)
            end
            if !(old_typ ⊑ new_typ) || !(new_typ ⊑ old_typ)
                ir.new_nodes[new_idx] = (old_insert, new_typ, node, old_line)
                changed = true
            end
        end
    end
    types = Any[isa(types[i], DelayedTyp) ? ir.new_nodes[types[i].phi.id - length(ir.stmts)][2] : types[i] for i in 1:length(types)]
    new_nodes = NewNode[let (pos, typ, node, line) = ir.new_nodes[i]
            typ = isa(typ, DelayedTyp) ? ir.new_nodes[typ.phi.id - length(ir.stmts)][2] : typ
            (pos, typ, node, line)
        end for i in 1:length(ir.new_nodes)]
    # Renumber SSA values
    code = Any[new_to_regular(renumber_ssa!(code[i], ssavalmap)) for i in 1:length(code)]
    new_nodes = NewNode[let (pt, typ, stmt, line) = new_nodes[i]
            (pt, typ, new_to_regular(renumber_ssa!(stmt, ssavalmap)), line)
        end for i in 1:length(new_nodes)]
    return IRCode(ir, code, types, ir.lines, ir.cfg, new_nodes)
end
