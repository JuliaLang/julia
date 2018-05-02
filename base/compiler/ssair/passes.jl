"""
    This struct keeps track of all uses of some mutable struct allocated
    in the current function. `uses` are all instances of `getfield` on the
    struct. `defs` are all instances of `setfield!` on the struct. The terminology
    refers to the uses/defs of the ``slot bundle'' that the mutable struct represents.

    In addition we keep track of all instances of a foreigncall preserve of this mutable
    struct. Somewhat counterintuitively, we don't actually need to make sure that the
    struct itself is live (or even allocated) at a ccall site. If there are no other places
    where the struct escapes (and thus e.g. where its address is taken), it need not be
    allocated. We do however, need to make sure to preserve any elments of this struct.
"""
struct SSADefUse
    uses::Vector{Int}
    defs::Vector{Int}
    ccall_preserve_uses::Vector{Int}
end
SSADefUse() = SSADefUse(Int[], Int[], Int[])

function try_compute_fieldidx_expr(@nospecialize(typ), @nospecialize(use_expr))
    field = use_expr.args[3]
    isa(field, QuoteNode) && (field = field.value)
    isa(field, Union{Int, Symbol}) || return nothing
    return try_compute_fieldidx(typ, field)
end

function lift_defuse(cfg::CFG, ssa::SSADefUse)
    # We remove from `uses` any block where all uses are dominated
    # by a def. This prevents insertion of dead phi nodes at the top
    # of such a block if that block happens to be in a loop
    ordered = Tuple{Int, Int, Bool}[(x, block_for_inst(cfg, x), true) for x in ssa.uses]
    for x in ssa.defs
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
    SSADefUse(bb_uses, bb_defs, Int[])
end

function find_curblock(domtree::DomTree, allblocks, curblock::Int)
    # TODO: This can be much faster by looking at current level and only
    # searching for those blocks in a sorted order
    while !(curblock in allblocks)
        curblock = domtree.idoms[curblock]
    end
    return curblock
end

function val_for_def_expr(ir::IRCode, def::Int, fidx::Int)
    if isexpr(ir[SSAValue(def)], :new)
        return ir[SSAValue(def)].args[1+fidx]
    else
        # The use is whatever the setfield was
        return ir[SSAValue(def)].args[4]
    end
end

function compute_value_for_block(ir::IRCode, domtree::DomTree, allblocks, du, phinodes, fidx, curblock)
    curblock = find_curblock(domtree, allblocks, curblock)
    def = reduce(max, 0, stmt for stmt in du.defs if block_for_inst(ir.cfg, stmt) == curblock)
    def == 0 ? phinodes[curblock] : val_for_def_expr(ir, def, fidx)
end

function compute_value_for_use(ir::IRCode, domtree::DomTree, allblocks, du, phinodes, fidx, use_idx)
    # Find the first dominating def
    curblock = stmtblock = block_for_inst(ir.cfg, use_idx)
    curblock = find_curblock(domtree, allblocks, curblock)
    defblockdefs = [stmt for stmt in du.defs if block_for_inst(ir.cfg, stmt) == curblock]
    def = 0
    if !isempty(defblockdefs)
        if curblock != stmtblock
            # Find the last def in this block
            def = maximum(defblockdefs)
        else
            # Find the last def before our use
            def = mapreduce(x->x >= use_idx ? 0 : x, max, defblockdefs)
        end
    end
    if def == 0
        if !haskey(phinodes, curblock)
            # If this happens, we need to search the predecessors for defs. Which
            # one doesn't matter - if it did, we'd have had a phinode
            return compute_value_for_block(ir, domtree, allblocks, du, phinodes, fidx, first(ir.cfg.blocks[stmtblock].preds))
        end
        # The use is the phinode
        return phinodes[curblock]
    else
        return val_for_def_expr(ir, def, fidx)
    end
end

function walk_to_def(compact::IncrementalCompact, @nospecialize(def), intermediaries=IdSet{Int}(), allow_phinode::Bool=true, phi_locs=Tuple{Int, Int}[])
    if !isa(def, SSAValue)
        return (def, 0)
    end
    orig_defidx = defidx = def.id
    # Step 2: Figure out what the struct is defined as
    def = compact[defidx]
    typeconstraint = types(compact)[defidx]
    ## Track definitions through PiNode/PhiNode
    found_def = false
    ## Track which PhiNodes, SSAValue intermediaries
    ## we forwarded through.
    while true
        if isa(def, PiNode)
            push!(intermediaries, defidx)
            typeconstraint = typeintersect(typeconstraint, def.typ)
            if isa(def.val, SSAValue)
                defidx = def.val.id
                def = compact[defidx]
            else
                def = def.val
            end
            continue
        elseif isa(def, FastForward)
            append!(phi_locs, def.phi_locs)
            def = def.to
        elseif isa(def, PhiNode)
            # For now, we don't track setfields structs through phi nodes
            allow_phinode || break
            push!(intermediaries, defidx)
            possible_predecessors = let def=def, typeconstraint=typeconstraint
                collect(Iterators.filter(1:length(def.edges)) do n
                    isassigned(def.values, n) || return false
                    value = def.values[n]
                    edge_typ = widenconst(compact_exprtype(compact, value))
                    return typeintersect(edge_typ, typeconstraint) !== Union{}
                end)
            end
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
        elseif isa(def, SSAValue)
            push!(intermediaries, defidx)
            defidx = def.id
            def = compact[def.id]
            continue
        end
        found_def = true
        break
    end
    found_def ? (def, defidx) : nothing
end

function process_immutable_preserve(new_preserves::Vector{Any}, compact::IncrementalCompact, def::Expr)
    for arg in (isexpr(def, :new) ? def.args : def.args[2:end])
        if !isbitstype(widenconst(compact_exprtype(compact, arg)))
            push!(new_preserves, arg)
        end
    end
end

struct FastForward
    to::SSAValue
    phi_locs::Vector{Tuple{Int64, Int64}}
end

function getfield_elim_pass!(ir::IRCode, domtree::DomTree)
    compact = IncrementalCompact(ir)
    insertions = Vector{Any}()
    defuses = IdDict{Int, Tuple{IdSet{Int}, SSADefUse}}()
    for (idx, stmt) in compact
        isa(stmt, Expr) || continue
        is_getfield = false
        is_ccall = false
        # Step 1: Check whether the statement we're looking at is a getfield/setfield!
        if is_known_call(stmt, setfield!, compact)
            is_setfield = true
        elseif is_known_call(stmt, getfield, compact)
            is_getfield = true
        elseif isexpr(stmt, :foreigncall)
            nccallargs = stmt.args[5]
            new_preserves = Any[]
            old_preserves = stmt.args[(6+nccallargs):end]
            for (pidx, preserved_arg) in enumerate(old_preserves)
                intermediaries = IdSet()
                isa(preserved_arg, SSAValue) || continue
                def = walk_to_def(compact, preserved_arg, intermediaries, false)
                def !== nothing || continue
                (def, defidx) = def
                if is_tuple_call(compact, def)
                    process_immutable_preserve(new_preserves, compact, def)
                    old_preserves[pidx] = nothing
                    continue
                elseif isexpr(def, :new)
                    typ = def.typ
                    if isa(typ, UnionAll)
                        typ = unwrap_unionall(typ)
                    end
                    if !typ.mutable
                        process_immutable_preserve(new_preserves, compact, def)
                        old_preserves[pidx] = nothing
                        continue
                    end
                else
                    continue
                end
                mid, defuse = get!(defuses, defidx, (IdSet{Int}(), SSADefUse()))
                push!(defuse.ccall_preserve_uses, idx)
                union!(mid, intermediaries)
                continue
            end
            if !isempty(new_preserves)
                old_preserves = filter(ssa->ssa !== nothing, old_preserves)
                new_expr = Expr(:foreigncall, stmt.args[1:(6+nccallargs-1)]...,
                    old_preserves..., new_preserves...)
                new_expr.typ = stmt.typ
                compact[idx] = new_expr
            end
            continue
        else
            continue
        end
        ## Normalize the field argument to getfield/setfield
        field = stmt.args[3]
        isa(field, QuoteNode) && (field = field.value)
        isa(field, Union{Int, Symbol}) || continue

        intermediaries = IdSet()
        phi_locs = Tuple{Int, Int}[]
        def = walk_to_def(compact, stmt.args[2], intermediaries, is_getfield, phi_locs)
        def !== nothing || continue
        (def, defidx) = def

        if !is_getfield
            (defidx == 0) && continue
            mid, defuse = get!(defuses, defidx, (IdSet{Int}(), SSADefUse()))
            push!(defuse.defs, idx)
            union!(mid, intermediaries)
            continue
        end
        # Step 3: Check if the definition we eventually end up at is either
        # a tuple(...) call or Expr(:new) and perform replacement.
        if is_tuple_call(compact, def) && isa(field, Int) && 1 <= field < length(def.args)
            forwarded = def.args[1+field]
        elseif isexpr(def, :new)
            typ = def.typ
            if isa(typ, UnionAll)
                typ = unwrap_unionall(typ)
            end
            isa(typ, DataType) || continue
            if typ.mutable
                @assert defidx != 0
                mid, defuse = get!(defuses, defidx, (IdSet{Int}(), SSADefUse()))
                push!(defuse.uses, idx)
                union!(mid, intermediaries)
                continue
            end
            field = try_compute_fieldidx_expr(typ, stmt)
            field === nothing && continue
            forwarded = def.args[1+field]
        else
            obj = compact_exprtype(compact, def)
            isa(obj, Const) || continue
            obj = obj.val
            isimmutable(obj) || continue
            field = try_compute_fieldidx_expr(typeof(obj), stmt)
            field === nothing && continue
            isdefined(obj, field) || continue
            val = getfield(obj, field)
            is_inlineable_constant(val) || continue
            forwarded = quoted(val)
        end
        # Step 4: Remember any phinodes we need to insert
        if !isempty(phi_locs) && isa(forwarded, SSAValue)
            # TODO: We have have to use BB ids for phi_locs
            # to avoid index invalidation.
            push!(insertions, idx)
            compact[idx] = FastForward(forwarded, phi_locs)
        else
            compact[idx] = forwarded
        end
    end
    ir = finish(compact)
    # Now go through any mutable structs and see which ones we can eliminate
    for (idx, (intermediaries, defuse)) in defuses
        intermediaries = collect(intermediaries)
        # Check if there are any uses we did not account for. If so, the variable
        # escapes and we cannot eliminate the allocation. This works, because we're guaranteed
        # not to include any intermediaries that have dead uses. As a result, missing uses will only ever
        # show up in the nuses_total count.
        nleaves = length(defuse.uses) + length(defuse.defs) + length(defuse.ccall_preserve_uses)
        nuses_total = compact.used_ssas[idx] + mapreduce(idx->compact.used_ssas[idx], +, 0, intermediaries) - length(intermediaries)
        nleaves == nuses_total || continue
        # Find the type for this allocation
        defexpr = ir[SSAValue(idx)]
        isexpr(defexpr, :new) || continue
        typ = defexpr.typ
        if isa(typ, UnionAll)
            typ = unwrap_unionall(typ)
        end
        # Could still end up here if we tried to setfield! and immutable, which would
        # error at runtime, but is not illegal to have in the IR.
        typ.mutable || continue
        # Partition defuses by field
        fielddefuse = SSADefUse[SSADefUse() for _ = 1:fieldcount(typ)]
        ok = true
        for use in defuse.uses
            field = try_compute_fieldidx_expr(typ, ir[SSAValue(use)])
            field === nothing && (ok = false; break)
            push!(fielddefuse[field].uses, use)
        end
        ok || continue
        for use in defuse.defs
            field = try_compute_fieldidx_expr(typ, ir[SSAValue(use)])
            field === nothing && (ok = false; break)
            push!(fielddefuse[field].defs, use)
        end
        ok || continue
        preserve_uses = IdDict{Int, Vector{Any}}((idx=>Any[] for idx in IdSet{Int}(defuse.ccall_preserve_uses)))
        # Everything accounted for. Go field by field and perform idf
        for (fidx, du) in pairs(fielddefuse)
            ftyp = fieldtype(typ, fidx)
            if !isempty(du.uses)
                push!(du.defs, idx)
                ldu = compute_live_ins(ir.cfg, du)
                phiblocks = []
                if !isempty(ldu.live_in_bbs)
                    phiblocks = idf(ir.cfg, ldu, domtree)
                end
                phinodes = IdDict{Int, SSAValue}()
                for b in phiblocks
                    n = PhiNode()
                    phinodes[b] = insert_node!(ir, first(ir.cfg.blocks[b].stmts), ftyp, n)
                end
                # Now go through all uses and rewrite them
                allblocks = sort(vcat(phiblocks, ldu.def_bbs))
                for stmt in du.uses
                    ir[SSAValue(stmt)] = compute_value_for_use(ir, domtree, allblocks, du, phinodes, fidx, stmt)
                end
                if !isbitstype(fieldtype(typ, fidx))
                    for (use, list) in preserve_uses
                        push!(list, compute_value_for_use(ir, domtree, allblocks, du, phinodes, fidx, use))
                    end
                end
                for b in phiblocks
                    for p in ir.cfg.blocks[b].preds
                        n = ir[phinodes[b]]
                        push!(n.edges, p)
                        push!(n.values, compute_value_for_block(ir, domtree,
                            allblocks, du, phinodes, fidx, p))
                    end
                end
            end
            for stmt in du.defs
                stmt == idx && continue
                ir[SSAValue(stmt)] = nothing
            end
            continue
        end
        isempty(defuse.ccall_preserve_uses) && continue
        push!(intermediaries, idx)
        # Insert the new preserves
        for (use, new_preserves) in preserve_uses
            useexpr = ir[SSAValue(use)]
            nccallargs = useexpr.args[5]
            old_preserves = filter(ssa->!isa(ssa, SSAValue) || !(ssa.id in intermediaries), useexpr.args[(6+nccallargs):end])
            new_expr = Expr(:foreigncall, useexpr.args[1:(6+nccallargs-1)]...,
                old_preserves..., new_preserves...)
            new_expr.typ = useexpr.typ
            ir[SSAValue(use)] = new_expr
        end
    end
    for idx in insertions
        # For non-dominating load-store forward, we may have to insert extra phi nodes
        # TODO: Can use the domtree to eliminate unnecessary phis, but ok for now
        ff = ir.stmts[idx]
        ff === nothing && continue # May have been DCE'd if there were no more uses
        ff = ff::FastForward
        forwarded = ff.to
        if isa(forwarded, SSAValue)
            forwarded_typ = ir.types[forwarded.id]
            for (pred, pos) in reverse!(ff.phi_locs)
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
    lifted_undef = IdDict{Int, Any}()
    for (idx, stmt) in pairs(ir.stmts)
        if stmt isa Expr && (stmt.head === :isdefined || stmt.head === :undefcheck)
            val = (stmt.head === :isdefined) ? stmt.args[1] : stmt.args[2]
            # undef can only show up by being introduced in a phi
            # node (or an UpsilonNode() argument to a PhiC node),
            # so lift all these nodes that have maybe undef values
            processed = IdDict{Int, Union{SSAValue, Bool}}()
            if !isa(val, SSAValue)
                if stmt.head === :undefcheck
                    ir.stmts[idx] = nothing
                end
                continue
            end
            worklist = Tuple{Int, Int, SSAValue, Int}[(val.id, 0, SSAValue(0), 0)]
            stmt_id = val.id
            while isa(ir.stmts[stmt_id], PiNode)
                stmt_id = ir.stmts[stmt_id].val.id
            end
            def = ir.stmts[stmt_id]
            if !isa(def, PhiNode) && !isa(def, PhiCNode)
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
                    item, w_up_id, which, use = pop!(worklist)
                    def = ir.stmts[item]
                    if isa(def, PhiNode)
                        edges = copy(def.edges)
                        values = Vector{Any}(undef, length(edges))
                        new_phi = length(values) == 0 ? false : insert_node!(ir, item, Bool, PhiNode(edges, values))
                    else
                        values = Vector{Any}(undef, length(def.values))
                        new_phi = length(values) == 0 ? false : insert_node!(ir, item, Bool, PhiCNode(values))
                    end
                    processed[item] = new_phi
                    if first
                        lifted_undef[stmt_id] = new_phi
                        first = false
                    end
                    local id::Int = 0
                    for i = 1:length(values)
                        if !isassigned(def.values, i)
                            val = false
                        elseif !isa(def.values[i], SSAValue)
                            val = true
                        else
                            up_id = id = def.values[i].id
                            @label restart
                            if !isa(ir.types[id], MaybeUndef)
                                val = true
                            else
                                if isa(ir.stmts[id], UpsilonNode)
                                    up = ir.stmts[id]
                                    if !isdefined(up, :val)
                                        val = false
                                    elseif !isa(up.val, SSAValue)
                                        val = true
                                    else
                                        id = up.val.id
                                        @goto restart
                                    end
                                else
                                    while isa(ir.stmts[id], PiNode)
                                        id = ir.stmts[id].val.id
                                    end
                                    if isa(ir.stmts[id], Union{PhiNode, PhiCNode})
                                        if haskey(processed, id)
                                            val = processed[id]
                                        else
                                            push!(worklist, (id, up_id, new_phi, i))
                                            continue
                                        end
                                    else
                                        val = true
                                    end
                                end
                            end
                        end
                        if isa(def, PhiNode)
                            values[i] = val
                        else
                            values[i] = insert_node!(ir, up_id, Bool, UpsilonNode(val))
                        end
                    end
                    if which !== SSAValue(0)
                        phi = ir[which]
                        if isa(phi, PhiNode)
                            phi.values[use] = new_phi
                        else
                            phi = phi::PhiCNode
                            ir[which].values[use] = insert_node!(ir, w_up_id, Bool, UpsilonNode(new_phi))
                        end
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
