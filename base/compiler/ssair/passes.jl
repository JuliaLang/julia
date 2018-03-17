function compact_exprtype(compact, value)
    if isa(value, Union{SSAValue, OldSSAValue})
        return types(compact)[value]
    elseif isa(value, Argument)
        return compact.ir.argtypes[value.n]
    end
    exprtype(value, compact.ir, compact.ir.mod)
end

function getfield_elim_pass!(ir::IRCode)
    compact = IncrementalCompact(ir)
    insertions = Vector{Any}()
    for (idx, stmt) in compact
        isa(stmt, Expr) || continue
        is_known_call(stmt, getfield, ir, ir.mod) || continue
        isa(stmt.args[2], SSAValue) || continue
        field = stmt.args[3]
        isa(field, QuoteNode) && (field = field.value)
        isa(field, Union{Int, Symbol}) || continue
        orig_defidx = defidx = stmt.args[2].id
        def = compact[defidx]
        typeconstraint = types(compact)[defidx]
        phi_locs = Tuple{Int, Int}[]
        while true
            if isa(def, PiNode)
                typeconstraint = typeintersect(typeconstraint, def.typ)
                if isa(def.val, SSAValue)
                    defidx = def.val.id
                    def = compact[defidx]
                else
                    def = def.val
                end
                continue
            elseif isa(def, PhiNode)
                possible_predecessors = collect(Iterators.filter(1:length(def.edges)) do n
                    isassigned(def.values, n) || return false
                    value = def.values[n]
                    edge_typ = compact_exprtype(compact, value)
                    return edge_typ ⊑ typeconstraint
                end)
                # For now, only look at unique predecessors
                if length(possible_predecessors) == 1
                    n = possible_predecessors[1]
                    pred = def.edges[n]
                    val = def.values[n]
                    if isa(val, SSAValue)
                        push!(phi_locs, (pred, defidx))
                        defidx = val.id
                        def = compact[defidx]
                    elseif def == val
                        # This shouldn't really ever happen, but
                        # patterns like this can occur in dead code,
                        # so bail out.
                        break
                    else
                        def = val
                    end
                    continue
                end
            end
            break
        end
        if isa(def, Expr) && is_known_call(def, tuple, ir, ir.mod) && isa(field, Int) && 1 <= field < length(def.args)
            forwarded = def.args[1+field]
        elseif isexpr(def, :new)
            typ = def.typ
            if isa(typ, UnionAll)
                typ = unwrap_unionall(typ)
            end
            isa(typ, DataType) || continue
            !typ.mutable || continue
            if isa(field, Symbol)
                field = fieldindex(typ, field, false)
                field == 0 && continue
            elseif isa(field, Integer)
                (1 <= field <= fieldcount(typ)) || continue
            end
            forwarded = def.args[1+field]
        else
            continue
        end
        if !isempty(phi_locs) && isa(forwarded, SSAValue)
            # TODO: We have have to use BB ids for phi_locs
            # to avoid index invalidation.
            push!(insertions, (idx, phi_locs))
        end
        compact[idx] = forwarded
    end
    ir = finish(compact)
    for (idx, phi_locs) in insertions
        # For non-dominating load-store forward, we may have to insert extra phi nodes
        # TODO: Can use the domtree to eliminate unnecessary phis, but ok for now
        forwarded = ir.stmts[idx]
        if isa(forwarded, SSAValue)
            forwarded_typ = ir.types[forwarded.id]
            for (pred, pos) in reverse!(phi_locs)
                node = PhiNode()
                push!(node.edges, pred)
                push!(node.values, forwarded)
                forwarded = insert_node!(ir, pos, forwarded_typ, node)
            end
        end
        ir.stmts[idx] = forwarded
    end
    ir
end

function type_lift_pass!(ir::IRCode)
    type_ctx_uses = Vector{Vector{Int}}[]
    has_non_type_ctx_uses = IdSet{Int}()
    lifted_undef = IdDict{Int, SSAValue}()
    for (idx, stmt) in pairs(ir.stmts)
        if stmt isa Expr && (stmt.head === :isdefined || stmt.head === :undefcheck)
            val = (stmt.head === :isdefined) ? stmt.args[1] : stmt.args[2]
            # undef can only show up by being introduced in a phi
            # node, so lift all phi nodes that have maybe undef values
            processed = IdDict{Int, SSAValue}()
            if !isa(val, SSAValue)
                if stmt.head === :undefcheck
                    ir.stmts[idx] = nothing
                end
                continue
            end
            worklist = Tuple{Int, SSAValue, Int}[(val.id, SSAValue(0), 0)]
            stmt_id = val.id
            while isa(ir.stmts[stmt_id], PiNode)
                stmt_id = ir.stmts[stmt_id].val.id
            end
            def = ir.stmts[stmt_id]
            if !isa(def, PhiNode)
                if stmt.head === :isdefined
                    ir.stmts[idx] = true
                else
                    ir.stmts[idx] = nothing
                end
                continue
            end
            if !haskey(lifted_undef, stmt_id)
                first = true
                while !isempty(worklist)
                    item, which, use = pop!(worklist)
                    def = ir.stmts[item]
                    edges = copy(def.edges)
                    values = Vector{Any}(undef, length(edges))
                    new_phi = length(values) == 0 ? false : insert_node!(ir, item, Bool, PhiNode(edges, values))
                    processed[item] = new_phi
                    if first
                        lifted_undef[stmt_id] = new_phi
                        first = false
                    end
                    for i = 1:length(edges)
                        if !isassigned(def.values, i)
                            val = false
                        elseif !isa(def.values[i], SSAValue)
                            val = true
                        else
                            id = def.values[i].id
                            if !isa(ir.types[id], MaybeUndef)
                                val = true
                            else
                                while isa(ir.stmts[id], PiNode)
                                    id = ir.stmts[id].val.id
                                end
                                if isa(ir.stmts[id], PhiNode)
                                    if haskey(processed, id)
                                        val = processed[id]
                                    else
                                        push!(worklist, (id, new_phi, i))
                                        continue
                                    end
                                else
                                    val = true
                                end
                            end
                        end
                        values[i] = val
                    end
                    if which !== SSAValue(0)
                        ir[which].values[use] = new_phi
                    end
                end
            end
            if stmt.head === :isdefined
                ir.stmts[idx] = lifted_undef[stmt_id]
            else
                ir.stmts[idx] = Expr(:throw_undef_if_not, stmt.args[1], lifted_undef[stmt_id])
            end
        end
    end
    ir
end