# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    This struct keeps track of all uses of some mutable struct allocated
    in the current function. `uses` are all instances of `getfield` on the
    struct. `defs` are all instances of `setfield!` on the struct. The terminology
    refers to the uses/defs of the ``slot bundle'' that the mutable struct represents.

    In addition we keep track of all instances of a foreigncall preserve of this mutable
    struct. Somewhat counterintuitively, we don't actually need to make sure that the
    struct itself is live (or even allocated) at a ccall site. If there are no other places
    where the struct escapes (and thus e.g. where its address is taken), it need not be
    allocated. We do however, need to make sure to preserve any elements of this struct.
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

function find_curblock(domtree::DomTree, allblocks::Vector{Int}, curblock::Int)
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

function compute_value_for_block(ir::IRCode, domtree::DomTree, allblocks::Vector{Int}, du::SSADefUse, phinodes::IdDict{Int, SSAValue}, fidx::Int, curblock::Int)
    curblock = find_curblock(domtree, allblocks, curblock)
    def = 0
    for stmt in du.defs
        if block_for_inst(ir.cfg, stmt) == curblock
            def = max(def, stmt)
        end
    end
    def == 0 ? phinodes[curblock] : val_for_def_expr(ir, def, fidx)
end

function compute_value_for_use(ir::IRCode, domtree::DomTree, allblocks::Vector{Int}, du::SSADefUse, phinodes::IdDict{Int, SSAValue}, fidx::Int, use_idx::Int)
    # Find the first dominating def
    curblock = stmtblock = block_for_inst(ir.cfg, use_idx)
    curblock = find_curblock(domtree, allblocks, curblock)
    defblockdefs = Int[stmt for stmt in du.defs if block_for_inst(ir.cfg, stmt) == curblock]
    def = 0
    if !isempty(defblockdefs)
        if curblock != stmtblock
            # Find the last def in this block
            def = 0
            for x in defblockdefs
                def = max(def, x)
            end
        else
            # Find the last def before our use
            def = 0
            for x in defblockdefs
                def = max(def, x >= use_idx ? 0 : x)
            end
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

function simple_walk(compact::IncrementalCompact, @nospecialize(defssa#=::AnySSAValue=#), pi_callback=(pi, idx)->false)
    while true
        if isa(defssa, OldSSAValue) && already_inserted(compact, defssa)
            rename = compact.ssa_rename[defssa.id]
            if isa(rename, AnySSAValue)
                defssa = rename
                continue
            end
            return rename
        end
        def = compact[defssa]
        if isa(def, PiNode)
            if pi_callback(def, defssa)
                return defssa
            end
            if isa(def.val, SSAValue)
                if is_old(compact, defssa)
                    defssa = OldSSAValue(def.val.id)
                else
                    defssa = def.val
                end
            else
                return def.val
            end
        elseif isa(def, AnySSAValue)
            pi_callback(def, defssa)
            if isa(def, SSAValue) && is_old(compact, defssa)
                defssa = OldSSAValue(def.id)
            else
                defssa = def
            end
        elseif isa(def, Union{PhiNode, PhiCNode, Expr, GlobalRef})
            return defssa
        else
            return def
        end
    end
end

function simple_walk_constraint(compact::IncrementalCompact, @nospecialize(defidx), @nospecialize(typeconstraint) = types(compact)[defidx])
    callback = function (@nospecialize(pi), @nospecialize(idx))
        isa(pi, PiNode) && (typeconstraint = typeintersect(typeconstraint, widenconst(pi.typ)))
        return false
    end
    def = simple_walk(compact, defidx, callback)
    return Pair{Any, Any}(def, typeconstraint)
end

"""
    walk_to_defs(compact, val, intermediaries)

Starting at `val` walk use-def chains to get all the leaves feeding into
this val (pruning those leaves rules out by path conditions).
"""
function walk_to_defs(compact::IncrementalCompact, @nospecialize(defssa), @nospecialize(typeconstraint), visited_phinodes::Vector{Any}=Any[])
    if !isa(defssa, AnySSAValue) || !isa(compact[defssa], PhiNode)
        return Any[defssa]
    end
    # Step 2: Figure out what the struct is defined as
    def = compact[defssa]
    ## Track definitions through PiNode/PhiNode
    found_def = false
    ## Track which PhiNodes, SSAValue intermediaries
    ## we forwarded through.
    visited = IdDict{Any, Any}()
    worklist_defs = Any[]
    worklist_constraints = Any[]
    leaves = Any[]
    push!(worklist_defs, defssa)
    push!(worklist_constraints, typeconstraint)
    while !isempty(worklist_defs)
        defssa = pop!(worklist_defs)
        typeconstraint = pop!(worklist_constraints)
        visited[defssa] = typeconstraint
        def = compact[defssa]
        if isa(def, PhiNode)
            push!(visited_phinodes, defssa)
            possible_predecessors = Int[]
            for n in 1:length(def.edges)
                isassigned(def.values, n) || continue
                val = def.values[n]
                if is_old(compact, defssa) && isa(val, SSAValue)
                    val = OldSSAValue(val.id)
                end
                edge_typ = widenconst(compact_exprtype(compact, val))
                typeintersect(edge_typ, typeconstraint) === Union{} && continue
                push!(possible_predecessors, n)
            end
            for n in possible_predecessors
                pred = def.edges[n]
                val = def.values[n]
                if is_old(compact, defssa) && isa(val, SSAValue)
                    val = OldSSAValue(val.id)
                end
                if isa(val, AnySSAValue)
                    new_def, new_constraint = simple_walk_constraint(compact, val, typeconstraint)
                    if isa(new_def, AnySSAValue)
                        if !haskey(visited, new_def)
                            push!(worklist_defs, new_def)
                            push!(worklist_constraints, new_constraint)
                        elseif !(new_constraint <: visited[new_def])
                            # We have reached the same definition via a different
                            # path, with a different type constraint. We may have
                            # to redo some work here with the wider typeconstraint
                            push!(worklist_defs, new_def)
                            push!(worklist_constraints, tmerge(new_constraint, visited[new_def]))
                        end
                        continue
                    end
                    val = new_def
                end
                if def == val
                    # This shouldn't really ever happen, but
                    # patterns like this can occur in dead code,
                    # so bail out.
                    break
                else
                    push!(leaves, val)
                end
                continue
            end
        else
            push!(leaves, defssa)
        end
    end
    leaves
end

function process_immutable_preserve(new_preserves::Vector{Any}, compact::IncrementalCompact, def::Expr)
    for arg in (isexpr(def, :new) ? def.args : def.args[2:end])
        if !isbitstype(widenconst(compact_exprtype(compact, arg)))
            push!(new_preserves, arg)
        end
    end
end

function already_inserted(compact::IncrementalCompact, old::OldSSAValue)
    id = old.id
    if id < length(compact.ir.stmts)
        return id < compact.idx
    end
    id -= length(compact.ir.stmts)
    if id < length(compact.ir.new_nodes)
        error()
    end
    id -= length(compact.ir.new_nodes)
    @assert id <= length(compact.pending_nodes)
    return !(id in compact.pending_perm)
end

function is_pending(compact::IncrementalCompact, old::OldSSAValue)
    return old.id > length(compact.ir.stmts) + length(compact.ir.new_nodes)
end

function lift_leaves(compact::IncrementalCompact, @nospecialize(stmt),
        @nospecialize(result_t), field::Int, leaves::Vector{Any})
    # For every leaf, the lifted value
    lifted_leaves = IdDict{Any, Any}()
    maybe_undef = false
    for leaf in leaves
        leaf_key = leaf
        if isa(leaf, AnySSAValue)
            if isa(leaf, OldSSAValue) && already_inserted(compact, leaf)
                leaf = compact.ssa_rename[leaf.id]
                if isa(leaf, AnySSAValue)
                    leaf = simple_walk(compact, leaf)
                end
                if isa(leaf, AnySSAValue)
                    def = compact[leaf]
                else
                    def = leaf
                end
            else
                def = compact[leaf]
            end
            if is_tuple_call(compact, def) && isa(field, Int) && 1 <= field < length(def.args)
                lifted = def.args[1+field]
                if is_old(compact, leaf) && isa(lifted, SSAValue)
                    lifted = OldSSAValue(lifted.id)
                end
                if isa(lifted, GlobalRef) || isa(lifted, Expr)
                    lifted = insert_node!(compact, leaf, compact_exprtype(compact, lifted), lifted)
                    def.args[1+field] = lifted
                    (isa(leaf, SSAValue) && (leaf.id < compact.result_idx)) && push!(compact.late_fixup, leaf.id)
                end
                lifted_leaves[leaf_key] = RefValue{Any}(lifted)
                continue
            elseif isexpr(def, :new)
                typ = widenconst(types(compact)[leaf])
                if isa(typ, UnionAll)
                    typ = unwrap_unionall(typ)
                end
                (isa(typ, DataType) && (!typ.abstract)) || return nothing
                @assert !typ.mutable
                field = try_compute_fieldidx_expr(typ, stmt)
                field === nothing && return nothing
                if length(def.args) < 1 + field
                    ftyp = fieldtype(typ, field)
                    if !isbitstype(ftyp)
                        # On this branch, this will be a guaranteed UndefRefError.
                        # We use the regular undef mechanic to lift this to a boolean slot
                        maybe_undef = true
                        lifted_leaves[leaf_key] = nothing
                        continue
                    end
                    return nothing
                    # Expand the Expr(:new) to include it's element Expr(:new) nodes up until the one we want
                    compact[leaf] = nothing
                    for i = (length(def.args) + 1):(1+field)
                        ftyp = fieldtype(typ, i - 1)
                        isbits(ftyp) || return nothing
                        push!(def.args, insert_node!(compact, leaf, result_t, Expr(:new, ftyp)))
                    end
                    compact[leaf] = def
                end
                lifted = def.args[1+field]
                if is_old(compact, leaf) && isa(lifted, SSAValue)
                    lifted = OldSSAValue(lifted.id)
                end
                if isa(lifted, GlobalRef) || isa(lifted, Expr)
                    lifted = insert_node!(compact, leaf, compact_exprtype(compact, lifted), lifted)
                    def.args[1+field] = lifted
                    (isa(leaf, SSAValue) && (leaf.id < compact.result_idx)) && push!(compact.late_fixup, leaf.id)
                end
                lifted_leaves[leaf_key] = RefValue{Any}(lifted)
                continue
            else
                typ = compact_exprtype(compact, leaf)
                if !isa(typ, Const)
                    # If the leaf is an old ssa value, insert a getfield here
                    # We will revisit this getfield later when compaction gets
                    # to the appropriate point.
                    # N.B.: This can be a bit dangerous because it can lead to
                    # infinite loops if we accidentally insert a node just ahead
                    # of where we are
                    if is_old(compact, leaf) && (isa(field, Int) || isa(field, Symbol))
                        (isa(typ, DataType) && (!typ.abstract)) || return nothing
                        @assert !typ.mutable
                        # If there's the potential for an undefref error on access, we cannot insert a getfield
                        if field > typ.ninitialized && !isbits(fieldtype(typ, field))
                            return nothing
                            lifted_leaves[leaf] = RefValue{Any}(insert_node!(compact, leaf, make_MaybeUndef(result_t), Expr(:call, :unchecked_getfield, SSAValue(leaf.id), field), true))
                            maybe_undef = true
                        else
                            return nothing
                            lifted_leaves[leaf] = RefValue{Any}(insert_node!(compact, leaf, result_t, Expr(:call, getfield, SSAValue(leaf.id), field), true))
                        end
                        continue
                    end
                    return nothing
                end
                leaf = typ.val
                # Fall through to below
            end
        elseif isa(leaf, QuoteNode)
            leaf = leaf.value
        elseif isa(leaf, GlobalRef)
            mod, name = leaf.mod, leaf.name
            if isdefined(mod, name) && isconst(mod, name)
                leaf = getfield(mod, name)
            else
                return nothing
            end
        elseif isa(leaf, Union{Argument, Expr})
            return nothing
        end
        !ismutable(leaf) || return nothing
        isdefined(leaf, field) || return nothing
        val = getfield(leaf, field)
        is_inlineable_constant(val) || return nothing
        lifted_leaves[leaf_key] = RefValue{Any}(quoted(val))
    end
    lifted_leaves, maybe_undef
end

make_MaybeUndef(@nospecialize(typ)) = isa(typ, MaybeUndef) ? typ : MaybeUndef(typ)

function lift_comparison!(compact::IncrementalCompact, idx::Int,
        @nospecialize(c1), @nospecialize(c2), stmt::Expr,
        lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue})
    if isa(c1, Const)
        cmp = c1
        typeconstraint = widenconst(c2)
        val = stmt.args[3]
    else
        cmp = c2
        typeconstraint = widenconst(c1)
        val = stmt.args[2]
    end

    is_type_only = isdefined(typeof(cmp), :instance)

    if isa(val, Union{OldSSAValue, SSAValue})
        val, typeconstraint = simple_walk_constraint(compact, val, typeconstraint)
    end

    visited_phinodes = Any[]
    leaves = walk_to_defs(compact, val, typeconstraint, visited_phinodes)

    # Let's check if we evaluate the comparison for each one of the leaves
    lifted_leaves = IdDict{Any, Any}()
    for leaf in leaves
        r = egal_tfunc(compact_exprtype(compact, leaf), cmp)
        if isa(r, Const)
            lifted_leaves[leaf] = RefValue{Any}(r.val)
        else
            # TODO: In some cases it might be profitable to hoist the ===
            # here.
            return
        end
    end

    lifted_val = perform_lifting!(compact, visited_phinodes, cmp, lifting_cache, Bool, lifted_leaves, val)
    @assert lifted_val !== nothing

    #global assertion_counter
    #assertion_counter::Int += 1
    #insert_node_here!(compact, Expr(:assert_egal, Symbol(string("assert_egal_", assertion_counter)), SSAValue(idx), lifted_val), nothing, 0, true)
    #return
    compact[idx] = lifted_val.x
end

struct LiftedPhi
    ssa::AnySSAValue
    node::Any
    need_argupdate::Bool
end

function is_old(compact, @nospecialize(old_node_ssa))
    isa(old_node_ssa, OldSSAValue) &&
        !is_pending(compact, old_node_ssa) &&
        !already_inserted(compact, old_node_ssa)
end

function perform_lifting!(compact::IncrementalCompact,
        visited_phinodes::Vector{Any}, @nospecialize(cache_key),
        lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue},
        @nospecialize(result_t), lifted_leaves::IdDict{Any, Any}, @nospecialize(stmt_val))
    reverse_mapping = IdDict{Any, Any}(ssa => id for (id, ssa) in enumerate(visited_phinodes))

    # Insert PhiNodes
    lifted_phis = LiftedPhi[]
    for item in visited_phinodes
        if (item, cache_key) in keys(lifting_cache)
            ssa = lifting_cache[Pair{AnySSAValue, Any}(item, cache_key)]
            push!(lifted_phis, LiftedPhi(ssa, compact[ssa], false))
            continue
        end
        n = PhiNode()
        ssa = insert_node!(compact, item, result_t, n)
        lifting_cache[Pair{AnySSAValue, Any}(item, cache_key)] = ssa
        push!(lifted_phis, LiftedPhi(ssa, n, true))
    end

    # Fix up arguments
    for (old_node_ssa, lf) in zip(visited_phinodes, lifted_phis)
        old_node = compact[old_node_ssa]
        new_node = lf.node
        lf.need_argupdate || continue
        for i = 1:length(old_node.edges)
            edge = old_node.edges[i]
            isassigned(old_node.values, i) || continue
            val = old_node.values[i]
            orig_val = val
            if is_old(compact, old_node_ssa) && isa(val, SSAValue)
                val = OldSSAValue(val.id)
            end
            if isa(val, Union{NewSSAValue, SSAValue, OldSSAValue})
                val = simple_walk(compact, val)
            end
            if val in keys(lifted_leaves)
                push!(new_node.edges, edge)
                lifted_val = lifted_leaves[val]
                if lifted_val === nothing
                    resize!(new_node.values, length(new_node.values)+1)
                    continue
                end
                lifted_val = lifted_val.x
                if isa(lifted_val, Union{NewSSAValue, SSAValue, OldSSAValue})
                    lifted_val = simple_walk(compact, lifted_val, (pi, idx)->true)
                end
                push!(new_node.values, lifted_val)
            elseif isa(val, Union{NewSSAValue, SSAValue, OldSSAValue}) && val in keys(reverse_mapping)
                push!(new_node.edges, edge)
                push!(new_node.values, lifted_phis[reverse_mapping[val]].ssa)
            else
                # Probably ignored by path condition, skip this
            end
        end
    end

    for lf in lifted_phis
        count_added_node!(compact, lf.node)
    end

    # Fixup the stmt itself
    if isa(stmt_val, Union{SSAValue, OldSSAValue})
        stmt_val = simple_walk(compact, stmt_val)
    end

    if stmt_val in keys(lifted_leaves)
        stmt_val = lifted_leaves[stmt_val]
    else
        isa(stmt_val, Union{SSAValue, OldSSAValue}) && stmt_val in keys(reverse_mapping)
        stmt_val = RefValue{Any}(lifted_phis[reverse_mapping[stmt_val]].ssa)
    end

    return stmt_val
end

assertion_counter = 0
function getfield_elim_pass!(ir::IRCode, domtree::DomTree)
    compact = IncrementalCompact(ir)
    insertions = Vector{Any}()
    defuses = IdDict{Int, Tuple{IdSet{Int}, SSADefUse}}()
    lifting_cache = IdDict{Pair{AnySSAValue, Any}, AnySSAValue}()
    revisit_worklist = Int[]
    #ndone, nmax = 0, 200
    for (idx, stmt) in compact
        isa(stmt, Expr) || continue
        #ndone >= nmax && continue
        #ndone += 1
        result_t = compact_exprtype(compact, SSAValue(idx))
        is_getfield = is_setfield = false
        is_ccall = false
        is_unchecked = false
        # Step 1: Check whether the statement we're looking at is a getfield/setfield!
        if is_known_call(stmt, setfield!, compact)
            is_setfield = true
            4 <= length(stmt.args) <= 5 || continue
        elseif is_known_call(stmt, getfield, compact)
            is_getfield = true
            3 <= length(stmt.args) <= 4 || continue
        elseif is_known_call(stmt, isa, compact)
            # TODO
            continue
        elseif is_known_call(stmt, typeassert, compact)
            # Canonicalize
            #   X = typeassert(Y, T)::S
            # into
            #   typeassert(Y, T)
            #   X = PiNode(Y, S)
            # N.B.: Inference may have a more precise type for `S`, than
            #       just T, but from here on out, there's no problem with
            #       using just using that.
            # so subsequent analysis only has to deal with the latter
            # form. TODO: This isn't the best place to put this.
            # Also, we should probably have a version of typeassert
            # that's defined not to return its value to make life easier
            # for the backend.
            pi = insert_node_here!(compact,
                PiNode(stmt.args[2], compact.result_types[idx]), compact.result_types[idx],
                compact.result_lines[idx], true)
            compact.ssa_rename[compact.idx-1] = pi
            continue
        elseif is_known_call(stmt, (===), compact)
            c1 = compact_exprtype(compact, stmt.args[2])
            c2 = compact_exprtype(compact, stmt.args[3])
            if !(isa(c1, Const) || isa(c2, Const))
                continue
            end
            (isa(c1, Const) && isa(c2, Const)) && continue
            lift_comparison!(compact, idx, c1, c2, stmt, lifting_cache)
            continue
        elseif isexpr(stmt, :call) && stmt.args[1] === :unchecked_getfield
            is_getfield = true
            is_unchecked = true
        elseif isexpr(stmt, :foreigncall)
            nccallargs = length(stmt.args[3]::SimpleVector)
            new_preserves = Any[]
            old_preserves = stmt.args[(6+nccallargs):end]
            for (pidx, preserved_arg) in enumerate(old_preserves)
                isa(preserved_arg, SSAValue) || continue
                let intermediaries = IdSet()
                    callback = function(@nospecialize(pi), ssa::AnySSAValue)
                        push!(intermediaries, ssa.id)
                        return false
                    end
                    def = simple_walk(compact, preserved_arg, callback)
                    isa(def, SSAValue) || continue
                    defidx = def.id
                    def = compact[defidx]
                    if is_tuple_call(compact, def)
                        process_immutable_preserve(new_preserves, compact, def)
                        old_preserves[pidx] = nothing
                        continue
                    elseif isexpr(def, :new)
                        typ = widenconst(compact_exprtype(compact, SSAValue(defidx)))
                        if isa(typ, UnionAll)
                            typ = unwrap_unionall(typ)
                        end
                        if typ isa DataType && !typ.mutable
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
                end
                continue
            end
            if !isempty(new_preserves)
                old_preserves = filter(ssa->ssa !== nothing, old_preserves)
                new_expr = Expr(:foreigncall, stmt.args[1:(6+nccallargs-1)]...,
                    old_preserves..., new_preserves...)
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

        struct_typ = unwrap_unionall(widenconst(compact_exprtype(compact, stmt.args[2])))
        isa(struct_typ, DataType) || continue

        def, typeconstraint = stmt.args[2], struct_typ

        if struct_typ.mutable
            isa(def, SSAValue) || continue
            let intermediaries = IdSet()
                callback = function(@nospecialize(pi), ssa::AnySSAValue)
                    push!(intermediaries, ssa.id)
                    return false
                end
                def = simple_walk(compact, def, callback)
                # Mutable stuff here
                isa(def, SSAValue) || continue
                mid, defuse = get!(defuses, def.id, (IdSet{Int}(), SSADefUse()))
                if is_setfield
                    push!(defuse.defs, idx)
                else
                    push!(defuse.uses, idx)
                end
                union!(mid, intermediaries)
            end
            continue
        elseif is_setfield
            continue
        end

        if isa(def, Union{OldSSAValue, SSAValue})
            def, typeconstraint = simple_walk_constraint(compact, def, typeconstraint)
        end

        visited_phinodes = Any[]
        leaves = walk_to_defs(compact, def, typeconstraint, visited_phinodes)

        isempty(leaves) && continue

        field = try_compute_fieldidx_expr(struct_typ, stmt)
        field === nothing && continue

        r = lift_leaves(compact, stmt, result_t, field, leaves)
        r === nothing && continue
        lifted_leaves, any_undef = r

        if any_undef
            result_t = make_MaybeUndef(result_t)
        end

#        @Base.show result_t
#        @Base.show stmt
#        for (k,v) in lifted_leaves
#            @Base.show (k, v)
#            if isa(k, AnySSAValue)
#                @Base.show compact[k]
#            end
#            if isa(v, RefValue) && isa(v.x, AnySSAValue)
#                @Base.show compact[v.x]
#            end
#        end
        val = perform_lifting!(compact, visited_phinodes, field, lifting_cache, result_t, lifted_leaves, stmt.args[2])

        # Insert the undef check if necessary
        if any_undef && !is_unchecked
            if val === nothing
                insert_node!(compact, SSAValue(idx), Nothing, Expr(:throw_undef_if_not, Symbol("##getfield##"), false))
            else
                insert_node!(compact, SSAValue(idx), Nothing, Expr(:undefcheck, Symbol("##getfield##"), val.x))
            end
        else
            @assert val !== nothing
        end

        global assertion_counter
        assertion_counter::Int += 1
        #insert_node_here!(compact, Expr(:assert_egal, Symbol(string("assert_egal_", assertion_counter)), SSAValue(idx), val), nothing, 0, true)
        #continue
        compact[idx] = val === nothing ? nothing : val.x
    end


    non_dce_finish!(compact)
    # Copy the use count, `simple_dce!` may modify it and for our predicate
    # below we need it consistent with the state of the IR here (after tracking
    # phi node arguments, but before dce).
    used_ssas = copy(compact.used_ssas)
    simple_dce!(compact)
    ir = complete(compact)
    # Now go through any mutable structs and see which ones we can eliminate
    for (idx, (intermediaries, defuse)) in defuses
        intermediaries = collect(intermediaries)
        # Check if there are any uses we did not account for. If so, the variable
        # escapes and we cannot eliminate the allocation. This works, because we're guaranteed
        # not to include any intermediaries that have dead uses. As a result, missing uses will only ever
        # show up in the nuses_total count.
        nleaves = length(defuse.uses) + length(defuse.defs) + length(defuse.ccall_preserve_uses)
        nuses = 0
        for idx in intermediaries
            nuses += used_ssas[idx]
        end
        nuses_total = used_ssas[idx] + nuses - length(intermediaries)
        nleaves == nuses_total || continue
        # Find the type for this allocation
        defexpr = ir[SSAValue(idx)]
        isexpr(defexpr, :new) || continue
        typ = ir.types[idx]
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
            stmt = ir[SSAValue(use)]
            # We may have discovered above that this use is dead
            # after the getfield elim of immutables. In that case,
            # it would have been deleted. That's fine, just ignore
            # the use in that case.
            stmt === nothing && continue
            field = try_compute_fieldidx_expr(typ, stmt)
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
        # Check that the defexpr has defined values for all the fields
        # we're accessing. In the future, we may want to relax this,
        # but we should come up with semantics for well defined semantics
        # for uninitialized fields first.
        for (fidx, du) in pairs(fielddefuse)
            isempty(du.uses) && continue
            if fidx + 1 > length(defexpr.args)
                ok = false
                break
            end
        end
        ok || continue
        preserve_uses = IdDict{Int, Vector{Any}}((idx=>Any[] for idx in IdSet{Int}(defuse.ccall_preserve_uses)))
        # Everything accounted for. Go field by field and perform idf
        for (fidx, du) in pairs(fielddefuse)
            ftyp = fieldtype(typ, fidx)
            if !isempty(du.uses)
                push!(du.defs, idx)
                ldu = compute_live_ins(ir.cfg, du)
                phiblocks = Int[]
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
            nccallargs = length(useexpr.args[3]::SimpleVector)
            old_preserves = filter(ssa->!isa(ssa, SSAValue) || !(ssa.id in intermediaries), useexpr.args[(6+nccallargs):end])
            new_expr = Expr(:foreigncall, useexpr.args[1:(6+nccallargs-1)]...,
                old_preserves..., new_preserves...)
            ir[SSAValue(use)] = new_expr
        end
    end
    ir
end

function adce_erase!(phi_uses, extra_worklist, compact, idx)
    if isa(compact.result[idx], PhiNode)
        maybe_erase_unused!(extra_worklist, compact, idx, val->phi_uses[val.id]-=1)
    else
        maybe_erase_unused!(extra_worklist, compact, idx)
    end
end

function count_uses(@nospecialize(stmt), uses::Vector{Int})
    for ur in userefs(stmt)
        use = ur[]
        if isa(use, SSAValue)
            uses[use.id] += 1
        end
    end
end

function mark_phi_cycles(compact::IncrementalCompact, safe_phis::BitSet, phi::Int)
    worklist = Int[]
    push!(worklist, phi)
    while !isempty(worklist)
        phi = pop!(worklist)
        push!(safe_phis, phi)
        for ur in userefs(compact.result[phi])
            val = ur[]
            isa(val, SSAValue) || continue
            isa(compact[val], PhiNode) || continue
            (val.id in safe_phis) && continue
            push!(worklist, val.id)
        end
    end
end

function adce_pass!(ir::IRCode)
    phi_uses = fill(0, length(ir.stmts) + length(ir.new_nodes))
    all_phis = Int[]
    compact = IncrementalCompact(ir)
    for (idx, stmt) in compact
        if isa(stmt, PhiNode)
            push!(all_phis, idx)
        end
    end
    non_dce_finish!(compact)
    for phi in all_phis
        count_uses(compact.result[phi]::PhiNode, phi_uses)
    end
    # Perform simple DCE for unused values
    extra_worklist = Int[]
    for (idx, nused) in Iterators.enumerate(compact.used_ssas)
        idx >= compact.result_idx && break
        nused == 0 || continue
        adce_erase!(phi_uses, extra_worklist, compact, idx)
    end
    while !isempty(extra_worklist)
        adce_erase!(phi_uses, extra_worklist, compact, pop!(extra_worklist))
    end
    # Go back and erase any phi cycles
    changed = true
    while changed
        changed = false
        safe_phis = BitSet()
        for phi in all_phis
            # Save any phi cycles that have non-phi uses
            if compact.used_ssas[phi] - phi_uses[phi] != 0
                mark_phi_cycles(compact, safe_phis, phi)
            end
        end
        for phi in all_phis
            if !(phi in safe_phis)
                push!(extra_worklist, phi)
            end
        end
        while !isempty(extra_worklist)
            if adce_erase!(phi_uses, extra_worklist, compact, pop!(extra_worklist))
                changed = true
            end
        end
    end
    return complete(compact)
end

function type_lift_pass!(ir::IRCode)
    type_ctx_uses = Vector{Vector{Int}}[]
    has_non_type_ctx_uses = IdSet{Int}()
    lifted_undef = IdDict{Int, Any}()
    for (idx, stmt) in pairs(ir.stmts)
        stmt isa Expr || continue
        if (stmt.head === :isdefined || stmt.head === :undefcheck)
            val = (stmt.head === :isdefined) ? stmt.args[1] : stmt.args[2]
            # undef can only show up by being introduced in a phi
            # node (or an UpsilonNode() argument to a PhiC node),
            # so lift all these nodes that have maybe undef values
            processed = IdDict{Int, Union{SSAValue, Bool}}()
            while isa(val, SSAValue) && isa(ir.stmts[val.id], PiNode)
                val = ir.stmts[val.id].val
            end
            if !isa(val, SSAValue) || (!isa(ir.stmts[val.id], PhiNode) && !isa(ir.stmts[val.id], PhiCNode))
                (isa(val, GlobalRef) || isexpr(val, :static_parameter)) && continue
                if stmt.head === :undefcheck
                    ir.stmts[idx] = nothing
                else
                    ir.stmts[idx] = true
                end
                continue
            end
            stmt_id = val.id
            worklist = Tuple{Int, Int, SSAValue, Int}[(stmt_id, 0, SSAValue(0), 0)]
            def = ir.stmts[stmt_id]
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

function cfg_simplify!(ir::IRCode)
    bbs = ir.cfg.blocks
    merge_into = zeros(Int, length(bbs))
    merged_succ = zeros(Int, length(bbs))

    # Walk the CFG at from the entry block and aggressively combine blocks
    for (idx, bb) in enumerate(bbs)
        if length(bb.succs) == 1
            succ = bb.succs[1]
            if length(bbs[succ].preds) == 1
                merge_into[succ] = idx
                merged_succ[idx] = succ
            end
        end
    end
    max_bb_num = 1
    bb_rename_succ = zeros(Int, length(bbs))
    # Lay out the basic blocks
    for i = 1:length(bbs)
        if merge_into[i] != 0
            bb_rename_succ[i] = -1
            continue
        end
        # Drop unreachable blocks
        if i != 1 && length(ir.cfg.blocks[i].preds) == 0
            bb_rename_succ[i] = -1
        end
        bb_rename_succ[i] != 0 && continue
        curr = i
        while true
            bb_rename_succ[curr] = max_bb_num
            max_bb_num += 1
            # Now walk the chain of blocks we merged.
            # If we end in something that may fall through,
            # we have to schedule that block next
            while merged_succ[curr] != 0
                curr = merged_succ[curr]
            end
            terminator = ir.stmts[ir.cfg.blocks[curr].stmts[end]]
            if isa(terminator, GotoNode) || isa(terminator, ReturnNode)
                break
            end
            curr += 1
        end
    end
    bb_rename_pred = zeros(Int, length(bbs))
    for i = 1:length(bbs)
        if merged_succ[i] != 0
            bb_rename_pred[i] = -1
            continue
        end
        bbnum = i
        while merge_into[bbnum] != 0
            bbnum = merge_into[bbnum]
        end
        bb_rename_pred[i] = bb_rename_succ[bbnum]
    end
    result_bbs = Int[findfirst(j->i==j, bb_rename_succ) for i = 1:max_bb_num-1]
    result_bbs_lengths = zeros(Int, max_bb_num-1)
    for (idx, orig_bb) in enumerate(result_bbs)
        ms = orig_bb
        while ms != 0
            result_bbs_lengths[idx] += length(bbs[ms].stmts)
            ms = merged_succ[ms]
        end
    end
    bb_starts = Vector{Int}(undef, 1+length(result_bbs_lengths))
    bb_starts[1] = 1
    for i = 1:length(result_bbs_lengths)
        bb_starts[i+1] = bb_starts[i] + result_bbs_lengths[i]
    end
    # Look at the original successor
    function compute_succs(i)
        orig_bb = result_bbs[i]
        while merged_succ[orig_bb] != 0
            orig_bb = merged_succ[orig_bb]
        end
        map(i->bb_rename_succ[i], bbs[orig_bb].succs)
    end

    function compute_preds(i)
        orig_bb = result_bbs[i]
        preds = bbs[orig_bb].preds
        map(preds) do pred
            while merge_into[pred] != 0
                pred = merge_into[pred]
            end
            bb_rename_succ[pred]
        end
    end
    cresult_bbs = BasicBlock[BasicBlock(
        StmtRange(bb_starts[i], i+1 > length(bb_starts) ? length(compact.result) : bb_starts[i+1]-1),
        compute_preds(i), compute_succs(i)) for i = 1:length(result_bbs)]
    compact = IncrementalCompact(ir, true)
    # We're messing with the CFG. We don't want compaction to do
    # so independently
    compact.fold_constant_branches = false
    compact.bb_rename_succ = bb_rename_succ
    compact.bb_rename_pred = bb_rename_pred
    compact.result_bbs = cresult_bbs
    result_idx = 1
    for (idx, orig_bb) in enumerate(result_bbs)
        ms = orig_bb
        while ms != 0
            for i in bbs[ms].stmts
                stmt = ir.stmts[i]
                compact.result[compact.result_idx] = nothing
                compact.result_types[compact.result_idx] = ir.types[i]
                compact.result_lines[compact.result_idx] = ir.lines[i]
                compact.result_flags[compact.result_idx] = ir.flags[i]
                # If we merged a basic block, we need remove the trailing GotoNode (if any)
                if isa(stmt, GotoNode) && merged_succ[ms] != 0
                    # Do nothing
                else
                    process_node!(compact, compact.result_idx, stmt, i, i, ms, true)
                end
                # We always increase the result index to ensure a predicatable
                # placement of the resulting nodes.
                compact.result_idx += 1
            end
            ms = merged_succ[ms]
        end
    end

    compact.active_result_bb = length(bb_starts)
    return finish(compact)
end
