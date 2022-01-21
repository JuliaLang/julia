# This file is a part of Julia. License is MIT: https://julialang.org/license

function is_known_call(@nospecialize(x), @nospecialize(func), ir::Union{IRCode,IncrementalCompact})
    isexpr(x, :call) || return false
    ft = argextype(x.args[1], ir)
    return singleton_type(ft) === func
end

# assume `stmt == getfield(obj, field, ...)` or `stmt == setfield!(obj, field, val, ...)`
try_compute_field_stmt(ir::Union{IncrementalCompact,IRCode}, stmt::Expr) =
    try_compute_field(ir, stmt.args[3])

function try_compute_field(ir::Union{IncrementalCompact,IRCode}, @nospecialize(field))
    # fields are usually literals, handle them manually
    if isa(field, QuoteNode)
        field = field.value
    elseif isa(field, Int) || isa(field, Symbol)
    else
        # try to resolve other constants, e.g. global reference
        field = argextype(field, ir)
        if isa(field, Const)
            field = field.val
        else
            return nothing
        end
    end
    return isa(field, Union{Int, Symbol}) ? field : nothing
end

function try_compute_fieldidx_stmt(ir::Union{IncrementalCompact,IRCode}, stmt::Expr, typ::DataType)
    field = try_compute_field_stmt(ir, stmt)
    return try_compute_fieldidx(typ, field)
end

function collect_leaves(compact::IncrementalCompact, @nospecialize(val), @nospecialize(typeconstraint))
    if isa(val, Union{OldSSAValue, SSAValue})
        val, typeconstraint = simple_walk_constraint(compact, val, typeconstraint)
    end
    return walk_to_defs(compact, val, typeconstraint)
end

function simple_walk(compact::IncrementalCompact, @nospecialize(defssa#=::AnySSAValue=#),
                     callback = (@nospecialize(pi), @nospecialize(idx)) -> false)
    while true
        if isa(defssa, OldSSAValue)
            if already_inserted(compact, defssa)
                rename = compact.ssa_rename[defssa.id]
                if isa(rename, AnySSAValue)
                    defssa = rename
                    continue
                end
                return rename
            end
        end
        def = compact[defssa]
        if isa(def, PiNode)
            if callback(def, defssa)
                return defssa
            end
            def = def.val
            if isa(def, SSAValue)
                is_old(compact, defssa) && (def = OldSSAValue(def.id))
            else
                return def
            end
            defssa = def
        elseif isa(def, AnySSAValue)
            callback(def, defssa)
            if isa(def, SSAValue)
                is_old(compact, defssa) && (def = OldSSAValue(def.id))
            end
            defssa = def
        elseif isa(def, Union{PhiNode, PhiCNode, Expr, GlobalRef})
            return defssa
        else
            return def
        end
    end
end

function simple_walk_constraint(compact::IncrementalCompact, @nospecialize(defssa#=::AnySSAValue=#),
                                @nospecialize(typeconstraint))
    callback = function (@nospecialize(pi), @nospecialize(idx))
        if isa(pi, PiNode)
            typeconstraint = typeintersect(typeconstraint, widenconst(pi.typ))
        end
        return false
    end
    def = simple_walk(compact, defssa, callback)
    return Pair{Any, Any}(def, typeconstraint)
end

"""
    walk_to_defs(compact, val, typeconstraint)

Starting at `val` walk use-def chains to get all the leaves feeding into this `val`
(pruning those leaves rules out by path conditions).
"""
function walk_to_defs(compact::IncrementalCompact, @nospecialize(defssa), @nospecialize(typeconstraint))
    visited_phinodes = AnySSAValue[]
    isa(defssa, AnySSAValue) || return Any[defssa], visited_phinodes
    def = compact[defssa]
    isa(def, PhiNode) || return Any[defssa], visited_phinodes
    visited_constraints = IdDict{AnySSAValue, Any}()
    worklist_defs = AnySSAValue[]
    worklist_constraints = Any[]
    leaves = Any[]
    push!(worklist_defs, defssa)
    push!(worklist_constraints, typeconstraint)
    while !isempty(worklist_defs)
        defssa = pop!(worklist_defs)
        typeconstraint = pop!(worklist_constraints)
        visited_constraints[defssa] = typeconstraint
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
                edge_typ = widenconst(argextype(val, compact))
                hasintersect(edge_typ, typeconstraint) || continue
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
                        if !haskey(visited_constraints, new_def)
                            push!(worklist_defs, new_def)
                            push!(worklist_constraints, new_constraint)
                        elseif !(new_constraint <: visited_constraints[new_def])
                            # We have reached the same definition via a different
                            # path, with a different type constraint. We may have
                            # to redo some work here with the wider typeconstraint
                            push!(worklist_defs, new_def)
                            push!(worklist_constraints, tmerge(new_constraint, visited_constraints[new_def]))
                        end
                        continue
                    end
                    val = new_def
                end
                if def === val
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
    return leaves, visited_phinodes
end

function record_immutable_preserve!(new_preserves::Vector{Any}, def::Expr, compact::IncrementalCompact)
    args = isexpr(def, :new) ? def.args : def.args[2:end]
    for i = 1:length(args)
        arg = args[i]
        if !isbitstype(widenconst(argextype(arg, compact)))
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
        error("")
    end
    id -= length(compact.ir.new_nodes)
    @assert id <= length(compact.pending_nodes)
    return !(id in compact.pending_perm)
end

function is_pending(compact::IncrementalCompact, old::OldSSAValue)
    return old.id > length(compact.ir.stmts) + length(compact.ir.new_nodes)
end

function is_getfield_captures(@nospecialize(def), compact::IncrementalCompact)
    isa(def, Expr) || return false
    length(def.args) >= 3 || return false
    is_known_call(def, getfield, compact) || return false
    which = argextype(def.args[3], compact)
    isa(which, Const) || return false
    which.val === :captures || return false
    oc = argextype(def.args[2], compact)
    return oc ⊑ Core.OpaqueClosure
end

struct LiftedValue
    x
    LiftedValue(@nospecialize x) = new(x)
end
const LiftedLeaves = IdDict{Any, Union{Nothing,LiftedValue}}

# try to compute lifted values that can replace `getfield(x, field)` call
# where `x` is an immutable struct that are defined at any of `leaves`
function lift_leaves(compact::IncrementalCompact,
                     @nospecialize(result_t), field::Int, leaves::Vector{Any})
    # For every leaf, the lifted value
    lifted_leaves = LiftedLeaves()
    maybe_undef = false
    for i = 1:length(leaves)
        leaf = leaves[i]
        cache_key = leaf
        if isa(leaf, AnySSAValue)
            (def, leaf) = walk_to_def(compact, leaf)
            if is_known_call(def, tuple, compact) && 1 ≤ field < length(def.args)
                lift_arg!(compact, leaf, cache_key, def, 1+field, lifted_leaves)
                continue
            elseif isexpr(def, :new)
                typ = widenconst(types(compact)[leaf])
                if isa(typ, UnionAll)
                    typ = unwrap_unionall(typ)
                end
                (isa(typ, DataType) && !isabstracttype(typ)) || return nothing
                @assert !ismutabletype(typ)
                if length(def.args) < 1+field
                    if field > fieldcount(typ)
                        return nothing
                    end
                    ftyp = fieldtype(typ, field)
                    if !isbitstype(ftyp)
                        # On this branch, this will be a guaranteed UndefRefError.
                        # We use the regular undef mechanic to lift this to a boolean slot
                        maybe_undef = true
                        lifted_leaves[cache_key] = nothing
                        continue
                    end
                    return nothing
                    # Expand the Expr(:new) to include it's element Expr(:new) nodes up until the one we want
                    compact[leaf] = nothing
                    for i = (length(def.args) + 1):(1+field)
                        ftyp = fieldtype(typ, i - 1)
                        isbitstype(ftyp) || return nothing
                        ninst = effect_free(NewInstruction(Expr(:new, ftyp), result_t))
                        push!(def.args, insert_node!(compact, leaf, ninst))
                    end
                    compact[leaf] = def
                end
                lift_arg!(compact, leaf, cache_key, def, 1+field, lifted_leaves)
                continue
            elseif is_getfield_captures(def, compact)
                # Walk to new_opaque_closure
                ocleaf = def.args[2]
                if isa(ocleaf, AnySSAValue)
                    ocleaf = simple_walk(compact, ocleaf)
                end
                ocdef, _ = walk_to_def(compact, ocleaf)
                if isexpr(ocdef, :new_opaque_closure) && isa(field, Int) && 1 ≤ field ≤ length(ocdef.args)-5
                    lift_arg!(compact, leaf, cache_key, ocdef, 5+field, lifted_leaves)
                    continue
                end
                return nothing
            else
                typ = argextype(leaf, compact)
                if !isa(typ, Const)
                    # TODO: (disabled since #27126)
                    # If the leaf is an old ssa value, insert a getfield here
                    # We will revisit this getfield later when compaction gets
                    # to the appropriate point.
                    # N.B.: This can be a bit dangerous because it can lead to
                    # infinite loops if we accidentally insert a node just ahead
                    # of where we are
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
        elseif isa(leaf, Argument) || isa(leaf, Expr)
            return nothing
        end
        ismutable(leaf) && return nothing
        isdefined(leaf, field) || return nothing
        val = getfield(leaf, field)
        is_inlineable_constant(val) || return nothing
        lifted_leaves[cache_key] = LiftedValue(quoted(val))
    end
    return lifted_leaves, maybe_undef
end

function lift_arg!(
    compact::IncrementalCompact, @nospecialize(leaf), @nospecialize(cache_key),
    stmt::Expr, argidx::Int, lifted_leaves::LiftedLeaves)
    lifted = stmt.args[argidx]
    if is_old(compact, leaf) && isa(lifted, SSAValue)
        lifted = OldSSAValue(lifted.id)
    end
    if isa(lifted, GlobalRef) || isa(lifted, Expr)
        lifted = insert_node!(compact, leaf, effect_free(NewInstruction(lifted, argextype(lifted, compact))))
        stmt.args[argidx] = lifted
        if isa(leaf, SSAValue) && leaf.id < compact.result_idx
            push!(compact.late_fixup, leaf.id)
        end
    end
    lifted_leaves[cache_key] = LiftedValue(lifted)
    nothing
end

function walk_to_def(compact::IncrementalCompact, @nospecialize(leaf))
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
    elseif isa(leaf, AnySSAValue)
        def = compact[leaf]
    else
        def = leaf
    end
    return Pair{Any, Any}(def, leaf)
end

make_MaybeUndef(@nospecialize(typ)) = isa(typ, MaybeUndef) ? typ : MaybeUndef(typ)

"""
    lift_comparison!(cmp, compact::IncrementalCompact, idx::Int, stmt::Expr)

Replaces `cmp(φ(x, y)::Union{X,Y}, constant)` by `φ(cmp(x, constant), cmp(y, constant))`,
where `cmp(x, constant)` and `cmp(y, constant)` can be replaced with constant `Bool`eans.
It helps codegen avoid generating expensive code for `cmp` with `Union` types.
In particular, this is supposed to improve the performance of the iteration protocol:
```julia
while x !== nothing
    x = iterate(...)::Union{Nothing,Tuple{Any,Any}}
end
```
"""
function lift_comparison! end

function lift_comparison!(::typeof(===), compact::IncrementalCompact,
    idx::Int, stmt::Expr, lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue})
    args = stmt.args
    length(args) == 3 || return
    lhs, rhs = args[2], args[3]
    vl = argextype(lhs, compact)
    vr = argextype(rhs, compact)
    if isa(vl, Const)
        isa(vr, Const) && return
        val = rhs
        cmp = vl
    elseif isa(vr, Const)
        val = lhs
        cmp = vr
    else
        return
    end
    lift_comparison_leaves!(egal_tfunc, compact, val, cmp, lifting_cache, idx)
end

function lift_comparison!(::typeof(isa), compact::IncrementalCompact,
    idx::Int, stmt::Expr, lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue})
    args = stmt.args
    length(args) == 3 || return
    cmp = argextype(args[3], compact)
    val = args[2]
    lift_comparison_leaves!(isa_tfunc, compact, val, cmp, lifting_cache, idx)
end

function lift_comparison!(::typeof(isdefined), compact::IncrementalCompact,
    idx::Int, stmt::Expr, lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue})
    args = stmt.args
    length(args) == 3 || return
    cmp = argextype(args[3], compact)
    isa(cmp, Const) || return # `isdefined_tfunc` won't return Const
    val = args[2]
    lift_comparison_leaves!(isdefined_tfunc, compact, val, cmp, lifting_cache, idx)
end

function lift_comparison_leaves!(@specialize(tfunc),
    compact::IncrementalCompact, @nospecialize(val), @nospecialize(cmp),
    lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue}, idx::Int)
    typeconstraint = widenconst(argextype(val, compact))
    if isa(val, Union{OldSSAValue, SSAValue})
        val, typeconstraint = simple_walk_constraint(compact, val, typeconstraint)
    end
    isa(typeconstraint, Union) || return # bail out if there won't be a good chance for lifting
    leaves, visited_phinodes = collect_leaves(compact, val, typeconstraint)
    length(leaves) ≤ 1 && return # bail out if we don't have multiple leaves

    # check if we can evaluate the comparison for each one of the leaves
    lifted_leaves = nothing
    for i = 1:length(leaves)
        leaf = leaves[i]
        result = tfunc(argextype(leaf, compact), cmp)
        if isa(result, Const)
            if lifted_leaves === nothing
                lifted_leaves = LiftedLeaves()
            end
            lifted_leaves[leaf] = LiftedValue(result.val)
        else
            return # TODO In some cases it might be profitable to hoist the comparison here
        end
    end

    # perform lifting
    lifted_val = perform_lifting!(compact,
        visited_phinodes, cmp, lifting_cache, Bool,
        lifted_leaves::LiftedLeaves, val)::LiftedValue

    compact[idx] = lifted_val.x
end

struct LiftedPhi
    ssa::AnySSAValue
    node::PhiNode
    need_argupdate::Bool
end

function is_old(compact, @nospecialize(old_node_ssa))
    isa(old_node_ssa, OldSSAValue) &&
        !is_pending(compact, old_node_ssa) &&
        !already_inserted(compact, old_node_ssa)
end

function perform_lifting!(compact::IncrementalCompact,
    visited_phinodes::Vector{AnySSAValue}, @nospecialize(cache_key),
    lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue},
    @nospecialize(result_t), lifted_leaves::LiftedLeaves, @nospecialize(stmt_val))
    reverse_mapping = IdDict{AnySSAValue, Int}(ssa => id for (id, ssa) in enumerate(visited_phinodes))

    # Insert PhiNodes
    lifted_phis = LiftedPhi[]
    for item in visited_phinodes
        # FIXME this cache is broken somehow
        # ckey = Pair{AnySSAValue, Any}(item, cache_key)
        # cached = ckey in keys(lifting_cache)
        cached = false
        if cached
            ssa = lifting_cache[ckey]
            push!(lifted_phis, LiftedPhi(ssa, compact[ssa]::PhiNode, false))
            continue
        end
        n = PhiNode()
        ssa = insert_node!(compact, item, effect_free(NewInstruction(n, result_t)))
        # lifting_cache[ckey] = ssa
        push!(lifted_phis, LiftedPhi(ssa, n, true))
    end

    # Fix up arguments
    for (old_node_ssa, lf) in zip(visited_phinodes, lifted_phis)
        old_node = compact[old_node_ssa]::PhiNode
        new_node = lf.node
        lf.need_argupdate || continue
        for i = 1:length(old_node.edges)
            edge = old_node.edges[i]
            isassigned(old_node.values, i) || continue
            val = old_node.values[i]
            if is_old(compact, old_node_ssa) && isa(val, SSAValue)
                val = OldSSAValue(val.id)
            end
            if isa(val, AnySSAValue)
                val = simple_walk(compact, val)
            end
            if val in keys(lifted_leaves)
                push!(new_node.edges, edge)
                lifted_val = lifted_leaves[val]
                if lifted_val === nothing
                    resize!(new_node.values, length(new_node.values)+1)
                    continue
                end
                val = lifted_val.x
                if isa(val, AnySSAValue)
                    callback = (@nospecialize(pi), @nospecialize(idx)) -> true
                    val = simple_walk(compact, val, callback)
                end
                push!(new_node.values, val)
            elseif isa(val, AnySSAValue) && val in keys(reverse_mapping)
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
        return lifted_leaves[stmt_val]
    elseif isa(stmt_val, AnySSAValue) && stmt_val in keys(reverse_mapping)
        return LiftedValue(lifted_phis[reverse_mapping[stmt_val]].ssa)
    end

    return stmt_val # N.B. should never happen
end

# NOTE we use `IdSet{Int}` instead of `BitSet` for in these passes since they work on IR after inlining,
# which can be very large sometimes, and program counters in question are often very sparse
const SPCSet = IdSet{Int}

"""
    linear_pass!(ir::IRCode) -> (newir::IRCode, memory_opt::Bool)

This pass consists of the following optimizations that can be performed by
a single linear traversal over IR statements:
- load forwarding of immutables (`getfield` elimination): immutable allocations whose
  loads are all eliminated by this pass may be erased entirely as a result of succeeding
  dead code elimination (this allocation elimination is called "SROA", Scalar Replacements of Aggregates)
- lifting of builtin comparisons: see [`lift_comparison!`](@ref)
- canonicalization of `typeassert` calls: see [`canonicalize_typeassert!`](@ref)

In addition to performing the optimizations above, the linear traversal also examines each
statement and checks if there is any profitability of running [`memory_opt_pass!`](@ref) pass.
In such cases `memory_opt` is flagged on and it indicates `ir` may be further optimized by
running `memory_opt_pass!(ir, estate::EscapeState)`.
"""
function linear_pass!(ir::IRCode)
    compact = IncrementalCompact(ir)
    lifting_cache = IdDict{Pair{AnySSAValue, Any}, AnySSAValue}()
    local memory_opt = false # whether or not to run the memory_opt_pass! pass later
    for ((_, idx), stmt) in compact
        isa(stmt, Expr) || continue
        field_ordering = :unspecified
        if isexpr(stmt, :new)
            typ = unwrap_unionall(widenconst(argextype(SSAValue(idx), compact)))
            if ismutabletype(typ)
                # mutable SROA may eliminate this eliminate this allocation, mark it now
                memory_opt = true
            end
            continue
        elseif is_known_call(stmt, getfield, compact)
            3 <= length(stmt.args) <= 5 || continue
            if length(stmt.args) == 5
                field_ordering = argextype(stmt.args[5], compact)
            elseif length(stmt.args) == 4
                field_ordering = argextype(stmt.args[4], compact)
                widenconst(field_ordering) === Bool && (field_ordering = :unspecified)
            end
        elseif isexpr(stmt, :foreigncall)
            nccallargs = length(stmt.args[3]::SimpleVector)
            preserved = Int[]
            new_preserves = Any[]
            for pidx in (6+nccallargs):length(stmt.args)
                preserved_arg = stmt.args[pidx]
                isa(preserved_arg, SSAValue) || continue
                def = simple_walk(compact, preserved_arg)
                isa(def, SSAValue) || continue
                defidx = def.id
                def = compact[defidx]
                if is_known_call(def, tuple, compact)
                    record_immutable_preserve!(new_preserves, def, compact)
                    push!(preserved, preserved_arg.id)
                elseif isexpr(def, :new)
                    typ = unwrap_unionall(widenconst(argextype(SSAValue(defidx), compact)))
                    if typ isa DataType
                        ismutabletype(typ) && continue # mutable SROA is performed later
                        record_immutable_preserve!(new_preserves, def, compact)
                        push!(preserved, preserved_arg.id)
                    end
                end
            end
            if !isempty(new_preserves)
                compact[idx] = form_new_preserves(stmt, preserved, new_preserves)
            end
            continue
        else # TODO: This isn't the best place to put these
            if is_known_call(stmt, typeassert, compact)
                canonicalize_typeassert!(compact, idx, stmt)
            elseif is_known_call(stmt, (===), compact)
                lift_comparison!(===, compact, idx, stmt, lifting_cache)
            elseif is_known_call(stmt, isa, compact)
                lift_comparison!(isa, compact, idx, stmt, lifting_cache)
            elseif is_known_call(stmt, isdefined, compact)
                lift_comparison!(isdefined, compact, idx, stmt, lifting_cache)
            end
            continue
        end

        # analyze this `getfield` call

        field = try_compute_field_stmt(compact, stmt)
        field === nothing && continue

        val = stmt.args[2]

        struct_typ = unwrap_unionall(widenconst(argextype(val, compact)))
        if isa(struct_typ, Union) && struct_typ <: Tuple
            struct_typ = unswitchtupleunion(struct_typ)
        end
        isa(struct_typ, DataType) || continue

        struct_typ.name.atomicfields == C_NULL || continue # TODO: handle more
        if !(field_ordering === :unspecified || (field_ordering isa Const && field_ordering.val === :not_atomic))
            continue
        end

        ismutabletype(struct_typ) && continue # mutable SROA is performed later

        # perform SROA on immutable structs here on

        field = try_compute_fieldidx(struct_typ, field)
        field === nothing && continue

        leaves, visited_phinodes = collect_leaves(compact, val, struct_typ)
        isempty(leaves) && continue

        result_t = argextype(SSAValue(idx), compact)
        lifted_result = lift_leaves(compact, result_t, field, leaves)
        lifted_result === nothing && continue
        lifted_leaves, any_undef = lifted_result

        if any_undef
            result_t = make_MaybeUndef(result_t)
        end

        val = perform_lifting!(compact,
            visited_phinodes, field, lifting_cache, result_t, lifted_leaves, val)

        # Insert the undef check if necessary
        if any_undef
            if val === nothing
                insert_node!(compact, SSAValue(idx),
                    non_effect_free(NewInstruction(Expr(:throw_undef_if_not, Symbol("##getfield##"), false), Nothing)))
            else
                # val must be defined
            end
        else
            @assert val !== nothing
        end

        compact[idx] = val === nothing ? nothing : val.x
    end

    non_dce_finish!(compact)
    simple_dce!(compact)
    ir = complete(compact)
    return ir, memory_opt
end

function form_new_preserves(origex::Expr, intermediates::Vector{Int}, new_preserves::Vector{Any})
    newex = Expr(:foreigncall)
    nccallargs = length(origex.args[3]::SimpleVector)
    for i in 1:(6+nccallargs-1)
        push!(newex.args, origex.args[i])
    end
    for i in (6+nccallargs):length(origex.args)
        x = origex.args[i]
        # don't need to preserve intermediaries
        if isa(x, SSAValue) && x.id in intermediates
            continue
        end
        push!(newex.args, x)
    end
    for i in 1:length(new_preserves)
        push!(newex.args, new_preserves[i])
    end
    return newex
end

import .EscapeAnalysis:
    EscapeState, EscapeInfo, IndexableFields, LivenessSet, getaliases, LocalUse, LocalDef

"""
    memory_opt_pass!(ir::IRCode, estate::EscapeState) -> newir::IRCode

Performs memory optimizations using escape information analyzed by `EscapeAnalysis`.
Specifically, this optimization pass does SROA of mutable allocations.

`estate::EscapeState` is expected to be a result of `analyze_escapes(ir, ...)`.
Since the computational cost of running `analyze_escapes` can be relatively expensive,
it is recommended to run this pass "selectively" i.e. only when there seems to be
a profitability for the memory optimizations.
"""
function memory_opt_pass!(ir::IRCode, estate::EscapeState)
    # Compute domtree now, needed below, now that we have finished compacting the IR.
    # This needs to be after we iterate through the IR with `IncrementalCompact`
    # because removing dead blocks can invalidate the domtree.
    # TODO initialization of the domtree can be delayed to avoid the expensive computation
    # in cases when there are no loads to be forwarded
    @timeit "domtree 2" domtree = construct_domtree(ir.cfg.blocks)
    wset = BitSet(1:length(ir.stmts)+length(ir.new_nodes.stmts))
    eliminated = BitSet()
    revisit = Tuple{#=related=#Vector{SSAValue}, #=Liveness=#LivenessSet}[]
    all_preserved = true
    newpreserves = nothing
    while !isempty(wset)
        idx = pop!(wset)
        ssa = SSAValue(idx)
        stmt = ir[ssa][:inst]
        isexpr(stmt, :new) || continue
        einfo = estate[ssa]
        is_load_forwardable(einfo) || continue
        aliases = getaliases(ssa, estate)
        if aliases === nothing
            related = SSAValue[ssa]
        else
            related = SSAValue[]
            for alias in aliases
                @assert isa(alias, SSAValue) "invalid escape analysis"
                push!(related, alias)
                delete!(wset, alias.id)
            end
        end
        finfos = (einfo.AliasInfo::IndexableFields).infos
        nfields = length(finfos)

        # Partition defuses by field
        fdefuses = Vector{FieldDefUse}(undef, nfields)
        for i = 1:nfields
            finfo = finfos[i]
            fdu = FieldDefUse()
            for fx in finfo
                if isa(fx, LocalUse)
                    push!(fdu.uses, GetfieldLoad(fx.idx))  # use (getfield call)
                else
                    @assert isa(fx, LocalDef)
                    push!(fdu.defs, fx.idx) # def (setfield! call or :new expression)
                end
            end
            fdefuses[i] = fdu
        end

        Liveness = einfo.Liveness
        for livepc in Liveness
            livestmt = ir[SSAValue(livepc)][:inst]
            if is_known_call(livestmt, Core.ifelse, ir)
                # the succeeding domination analysis doesn't account for conditional branching
                # by ifelse branching at this moment
                @goto next_itr
            elseif is_known_call(livestmt, isdefined, ir)
                args = livestmt.args
                length(args) ≥ 3 || continue
                obj = args[2]
                isa(obj, SSAValue) || continue
                obj in related || continue
                fld = args[3]
                fldval = try_compute_field(ir, fld)
                fldval === nothing && continue
                typ = unwrap_unionall(widenconst(argextype(obj, ir)))
                isa(typ, DataType) || continue
                fldidx = try_compute_fieldidx(typ, fldval)
                fldidx === nothing && continue
                push!(fdefuses[fldidx].uses, IsdefinedUse(livepc))
            elseif isexpr(livestmt, :foreigncall)
                # we shouldn't eliminate this use if it's used as a direct argument
                args = livestmt.args
                nccallargs = length(args[3]::SimpleVector)
                for i = 6:(5+nccallargs)
                    arg = args[i]
                    isa(arg, SSAValue) && arg in related && @goto next_liveness
                end
                # this use is preserve, and may be eliminable
                for fidx in 1:nfields
                    push!(fdefuses[fidx].uses, PreserveUse(livepc))
                end
            end
            @label next_liveness
        end

        for fidx in 1:nfields
            fdu = fdefuses[fidx]
            isempty(fdu.uses) && @goto next_use
            # check if all uses have safe definitions first, otherwise we should bail out
            # since then we may fail to form new ϕ-nodes
            ldu = compute_live_ins(ir.cfg, fdu)
            if isempty(ldu.live_in_bbs)
                phiblocks = Int[]
            else
                phiblocks = iterated_dominance_frontier(ir.cfg, ldu, domtree)
            end
            allblocks = sort!(vcat(phiblocks, ldu.def_bbs))
            for use in fdu.uses
                isa(use, IsdefinedUse) && continue
                if isa(use, PreserveUse) && isempty(fdu.defs)
                    # nothing to preserve, just ignore this use (may happen when there are unintialized fields)
                    continue
                end
                if !has_safe_def(ir, domtree, allblocks, fdu, getuseidx(use))
                    all_preserved = false
                    @goto next_use
                end
            end
            phinodes = IdDict{Int, SSAValue}()
            for b in phiblocks
                phinodes[b] = insert_node!(ir, first(ir.cfg.blocks[b].stmts),
                    NewInstruction(PhiNode(), Any))
            end
            # Now go through all uses and rewrite them
            for use in fdu.uses
                if isa(use, GetfieldLoad)
                    use = getuseidx(use)
                    ir[SSAValue(use)][:inst] = compute_value_for_use(
                        ir, domtree, allblocks, fdu, phinodes, fidx, use)
                    push!(eliminated, use)
                elseif all_preserved && isa(use, PreserveUse)
                    if newpreserves === nothing
                        newpreserves = IdDict{Int,Vector{Any}}()
                    end
                    # record this `use` as replaceable no matter if we preserve new value or not
                    use = getuseidx(use)
                    newvalues = get!(()->Any[], newpreserves, use)
                    isempty(fdu.defs) && continue # nothing to preserve (may happen when there are unintialized fields)
                    newval = compute_value_for_use(
                        ir, domtree, allblocks, fdu, phinodes, fidx, use)
                    if !isbitstype(widenconst(argextype(newval, ir)))
                        push!(newvalues, newval)
                    end
                elseif isa(use, IsdefinedUse)
                    use = getuseidx(use)
                    if has_safe_def(ir, domtree, allblocks, fdu, use)
                        ir[SSAValue(use)][:inst] = true
                        push!(eliminated, use)
                    end
                else
                    throw("unexpected use")
                end
            end
            for b in phiblocks
                ϕssa = phinodes[b]
                n = ir[ϕssa][:inst]::PhiNode
                t = Bottom
                for p in ir.cfg.blocks[b].preds
                    push!(n.edges, p)
                    v = compute_value_for_block(ir, domtree, allblocks, fdu, phinodes, fidx, p)
                    push!(n.values, v)
                    if t !== Any
                        t = tmerge(t, argextype(v, ir))
                    end
                end
                ir[ϕssa][:type] = t
            end
            @label next_use
        end
        push!(revisit, (related, Liveness))
        @label next_itr
    end

    # remove dead setfield! and :new allocs
    deadssas = IdSet{SSAValue}()
    if all_preserved && newpreserves !== nothing
        preserved = keys(newpreserves)
    else
        preserved = EMPTY_PRESERVED_SSAS
    end
    mark_dead_ssas!(ir, deadssas, revisit, eliminated, preserved)
    for ssa in deadssas
        ir[ssa][:inst] = nothing
    end
    if all_preserved && newpreserves !== nothing
        deadssas = Int[ssa.id for ssa in deadssas]
        for (idx, newuses) in newpreserves
            ir[SSAValue(idx)][:inst] = form_new_preserves(
                ir[SSAValue(idx)][:inst]::Expr, deadssas, newuses)
        end
    end

    return ir
end

const EMPTY_PRESERVED_SSAS = keys(IdDict{Int,Vector{Any}}())
const PreservedSets = typeof(EMPTY_PRESERVED_SSAS)

function is_load_forwardable(x::EscapeInfo)
    AliasInfo = x.AliasInfo
    return isa(AliasInfo, IndexableFields)
end

struct FieldDefUse
    uses::Vector{Any}
    defs::Vector{Int}
end
FieldDefUse() = FieldDefUse(Any[], Int[])
struct GetfieldLoad
    idx::Int
end
struct PreserveUse
    idx::Int
end
struct IsdefinedUse
    idx::Int
end
function getuseidx(@nospecialize use)
    if isa(use, GetfieldLoad)
        return use.idx
    elseif isa(use, PreserveUse)
        return use.idx
    elseif isa(use, IsdefinedUse)
        return use.idx
    end
    throw("getuseidx: unexpected use")
end

function compute_live_ins(cfg::CFG, fdu::FieldDefUse)
    uses = Int[]
    for use in fdu.uses
        isa(use, IsdefinedUse) && continue
        push!(uses, getuseidx(use))
    end
    return compute_live_ins(cfg, fdu.defs, uses)
end

# even when the allocation contains an uninitialized field, we try an extra effort to check
# if this load at `idx` have any "safe" `setfield!` calls that define the field
# try to find
function has_safe_def(ir::IRCode, domtree::DomTree, allblocks::Vector{Int},
    fdu::FieldDefUse, use::Int)
    dfu = find_def_for_use(ir, domtree, allblocks, fdu, use)
    dfu === nothing && return false
    def = dfu[1]
    def ≠ 0 && return true # found a "safe" definition
    # we may still be able to replace this load with `PhiNode` -- examine if all predecessors of
    # this `block` have any "safe" definition
    block = block_for_inst(ir, use)
    seen = BitSet(block)
    worklist = BitSet(ir.cfg.blocks[block].preds)
    isempty(worklist) && return false
    while !isempty(worklist)
        pred = pop!(worklist)
        # if this block has already been examined, bail out to avoid infinite cycles
        pred in seen && return false
        use = last(ir.cfg.blocks[pred].stmts)
        # NOTE this `use` isn't a load, and so the inclusive condition can be used
        dfu = find_def_for_use(ir, domtree, allblocks, fdu, use, true)
        dfu === nothing && return false
        def = dfu[1]
        push!(seen, pred)
        def ≠ 0 && continue # found a "safe" definition for this predecessor
        # if not, check for the predecessors of this predecessor
        for newpred in ir.cfg.blocks[pred].preds
            push!(worklist, newpred)
        end
    end
    return true
end

# find the first dominating def for the given use
function find_def_for_use(ir::IRCode, domtree::DomTree, allblocks::Vector{Int},
    fdu::FieldDefUse, use::Int, inclusive::Bool=false)
    useblock = block_for_inst(ir.cfg, use)
    curblock = find_curblock(domtree, allblocks, useblock)
    curblock === nothing && return nothing
    local def = 0
    for idx in fdu.defs
        if block_for_inst(ir.cfg, idx) == curblock
            if curblock != useblock
                # Find the last def in this block
                def = max(def, idx)
            else
                # Find the last def before our use
                if inclusive
                    def = max(def, idx ≤ use ? idx : 0)
                else
                    def = max(def, idx < use ? idx : 0)
                end
            end
        end
    end
    return def, useblock, curblock
end

function find_curblock(domtree::DomTree, allblocks::Vector{Int}, curblock::Int)
    # TODO: This can be much faster by looking at current level and only
    # searching for those blocks in a sorted order
    while !(curblock in allblocks)
        curblock = domtree.idoms_bb[curblock]
        curblock == 0 && return nothing
    end
    return curblock
end

function compute_value_for_use(ir::IRCode, domtree::DomTree, allblocks::Vector{Int},
    fdu::FieldDefUse, phinodes::IdDict{Int, SSAValue}, fidx::Int, use::Int)
    dfu = find_def_for_use(ir, domtree, allblocks, fdu, use)
    @assert dfu !== nothing "has_safe_def condition unsatisfied"
    def, useblock, curblock = dfu
    if def == 0
        if !haskey(phinodes, curblock)
            # If this happens, we need to search the predecessors for defs. Which
            # one doesn't matter - if it did, we'd have had a phinode
            return compute_value_for_block(ir, domtree, allblocks, fdu, phinodes, fidx, first(ir.cfg.blocks[useblock].preds))
        end
        # The use is the phinode
        return phinodes[curblock]
    else
        return val_for_def_expr(ir, def, fidx)
    end
end

function compute_value_for_block(ir::IRCode, domtree::DomTree, allblocks::Vector{Int},
    fdu::FieldDefUse, phinodes::IdDict{Int, SSAValue}, fidx::Int, curblock::Int)
    curblock = find_curblock(domtree, allblocks, curblock)
    @assert curblock !== nothing "has_safe_def condition unsatisfied"
    def = 0
    for stmt in fdu.defs
        if block_for_inst(ir.cfg, stmt) == curblock
            def = max(def, stmt)
        end
    end
    return def == 0 ? phinodes[curblock] : val_for_def_expr(ir, def, fidx)
end

function val_for_def_expr(ir::IRCode, def::Int, fidx::Int)
    ex = ir[SSAValue(def)][:inst]
    if isexpr(ex, :new)
        return ex.args[1+fidx]
    else
        @assert is_known_call(ex, setfield!, ir) "invalid load forwarding"
        return ex.args[4]
    end
end

function mark_dead_ssas!(ir::IRCode, deadssas::IdSet{SSAValue},
    revisit::Vector{Tuple{Vector{SSAValue},LivenessSet}}, eliminated::BitSet,
    preserved::PreservedSets)
    wset = BitSet(1:length(revisit))
    while !isempty(wset)
        revisit_idx = pop!(wset)
        mark_dead_ssas!(ir, deadssas, revisit, eliminated, preserved, wset, revisit_idx)
    end
end

function mark_dead_ssas!(ir::IRCode, deadssas::IdSet{SSAValue},
    revisit::Vector{Tuple{Vector{SSAValue},LivenessSet}}, eliminated::BitSet,
    preserved::PreservedSets, wset::BitSet, revisit_idx::Int)
    related, Liveness = revisit[revisit_idx]
    eliminable = SSAValue[]
    for livepc in Liveness
        livepc in eliminated && @goto next_live
        ssa = SSAValue(livepc)
        stmt = ir[ssa][:inst]
        if isexpr(stmt, :new)
            ssa in deadssas && @goto next_live
            for new_revisit_idx in wset
                if ssa in revisit[new_revisit_idx][1]
                    delete!(wset, new_revisit_idx)
                    if mark_dead_ssas!(ir, deadssas,
                            revisit, eliminated,
                            preserved, wset, new_revisit_idx)
                        push!(eliminable, ssa)
                        @goto next_live
                    else
                        return false
                    end
                end
            end
            return false
        elseif is_known_call(stmt, setfield!, ir)
            @assert length(stmt.args) ≥ 4 "invalid escape analysis"
            obj = stmt.args[2]
            val = stmt.args[4]
            if isa(obj, SSAValue)
                if obj in related
                    push!(eliminable, ssa)
                    @goto next_live
                end
                if isa(val, SSAValue) && val in related
                    if obj in deadssas
                        push!(eliminable, ssa)
                        @goto next_live
                    end
                    for new_revisit_idx in wset
                        if obj in revisit[new_revisit_idx][1]
                            delete!(wset, new_revisit_idx)
                            if mark_dead_ssas!(ir, deadssas,
                                    revisit, eliminated,
                                    preserved, wset, new_revisit_idx)
                                push!(eliminable, ssa)
                                @goto next_live
                            else
                                return false
                            end
                        end
                    end
                end
            end
            return false
        elseif isexpr(stmt, :foreigncall)
            livepc in preserved && @goto next_live
            return false
        else
            return false
        end
        @label next_live
    end
    for ssa in related; push!(deadssas, ssa); end
    for ssa in eliminable; push!(deadssas, ssa); end
    return true
end

"""
    canonicalize_typeassert!(compact::IncrementalCompact, idx::Int, stmt::Expr)

Canonicalizes `X = typeassert(Y, T)::S` into `typeassert(Y, T); X = π(Y, S)`
so that subsequent analysis only has to deal with the latter form.

N.B. Inference may have a more precise type for `S`, than just `T`, but from here on out,
there's no problem with just using that.
We should probably have a version of `typeassert` that's defined not to return its value to
make life easier for the backend.
"""
function canonicalize_typeassert!(compact::IncrementalCompact, idx::Int, stmt::Expr)
    length(stmt.args) == 3 || return
    pi = insert_node_here!(compact,
        NewInstruction(
            PiNode(stmt.args[2], compact.result[idx][:type]),
            compact.result[idx][:type],
            compact.result[idx][:line]), true)
    compact.ssa_rename[compact.idx-1] = pi
end

function adce_erase!(phi_uses::Vector{Int}, extra_worklist::Vector{Int}, compact::IncrementalCompact, idx::Int)
    # return whether this made a change
    if isa(compact.result[idx][:inst], PhiNode)
        return maybe_erase_unused!(extra_worklist, compact, idx, val::SSAValue -> phi_uses[val.id] -= 1)
    else
        return maybe_erase_unused!(extra_worklist, compact, idx)
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

function mark_phi_cycles!(compact::IncrementalCompact, safe_phis::SPCSet, phi::Int)
    worklist = Int[]
    push!(worklist, phi)
    while !isempty(worklist)
        phi = pop!(worklist)
        push!(safe_phis, phi)
        for ur in userefs(compact.result[phi][:inst])
            val = ur[]
            isa(val, SSAValue) || continue
            isa(compact[val], PhiNode) || continue
            (val.id in safe_phis) && continue
            push!(worklist, val.id)
        end
    end
end

function is_some_union(@nospecialize(t))
    isa(t, MaybeUndef) && (t = t.typ)
    return isa(t, Union)
end

function is_union_phi(compact::IncrementalCompact, idx::Int)
    inst = compact.result[idx]
    return isa(inst[:inst], PhiNode) && is_some_union(inst[:type])
end

"""
    adce_pass!(ir::IRCode) -> newir::IRCode

Aggressive Dead Code Elimination pass.

In addition to a simple DCE for unused values and allocations,
this pass also nullifies `typeassert` calls that can be proved to be no-op,
in order to allow LLVM to emit simpler code down the road.

Note that this pass is more effective after SROA optimization (i.e. `linear_pass!`),
since SROA often allows this pass to:
- eliminate allocation of object whose field references are all replaced with scalar values, and
- nullify `typeassert` call whose first operand has been replaced with a scalar value
  (, which may have introduced new type information that inference did not understand)

Also note that currently this pass _needs_ to run after `linear_pass!`, because
the `typeassert` elimination depends on the transformation by `canonicalize_typeassert!` done
within `linear_pass!` which redirects references of `typeassert`ed value to the corresponding `PiNode`.
"""
function adce_pass!(ir::IRCode)
    phi_uses = fill(0, length(ir.stmts) + length(ir.new_nodes))
    all_phis = Int[]
    unionphis = Pair{Int,Any}[] # sorted
    compact = IncrementalCompact(ir)
    for ((_, idx), stmt) in compact
        if isa(stmt, PhiNode)
            push!(all_phis, idx)
            if is_some_union(compact.result[idx][:type])
                push!(unionphis, Pair{Int,Any}(idx, Union{}))
            end
        elseif isa(stmt, PiNode)
            val = stmt.val
            if isa(val, SSAValue) && is_union_phi(compact, val.id)
                r = searchsorted(unionphis, val.id; by = first)
                if !isempty(r)
                    unionphi = unionphis[first(r)]
                    t = tmerge(unionphi[2], stmt.typ)
                    unionphis[first(r)] = Pair{Int,Any}(unionphi[1], t)
                end
            end
        else
            if is_known_call(stmt, typeassert, compact) && length(stmt.args) == 3
                # nullify safe `typeassert` calls
                ty, isexact = instanceof_tfunc(argextype(stmt.args[3], compact))
                if isexact && argextype(stmt.args[2], compact) ⊑ ty
                    compact[idx] = nothing
                    continue
                end
            end
            for ur in userefs(stmt)
                use = ur[]
                if isa(use, SSAValue) && is_union_phi(compact, use.id)
                    r = searchsorted(unionphis, use.id; by = first)
                    if !isempty(r)
                        deleteat!(unionphis, first(r))
                    end
                end
            end
        end
    end
    non_dce_finish!(compact)
    for phi in all_phis
        inst = compact.result[phi]
        for ur in userefs(inst[:inst]::PhiNode)
            use = ur[]
            if isa(use, SSAValue)
                phi_uses[use.id] += 1
                stmt = compact.result[use.id][:inst]
                if isa(stmt, PhiNode)
                    r = searchsorted(unionphis, use.id; by=first)
                    if !isempty(r)
                        unionphi = unionphis[first(r)]
                        unionphis[first(r)] = Pair{Int,Any}(unionphi[1],
                            tmerge(unionphi[2], inst[:type]))
                    end
                end
            end
        end
    end
    # Narrow any union phi nodes that have unused branches
    for i = 1:length(unionphis)
        unionphi = unionphis[i]
        phi = unionphi[1]
        t = unionphi[2]
        if t === Union{}
            compact.result[phi][:inst] = nothing
            continue
        elseif t === Any
            continue
        elseif compact.result[phi][:type] ⊑ t
            continue
        end
        to_drop = Int[]
        stmt = compact[phi]
        stmt === nothing && continue
        for i = 1:length(stmt.values)
            if !isassigned(stmt.values, i)
                # Should be impossible to have something used only by PiNodes that's undef
                push!(to_drop, i)
            elseif !hasintersect(widenconst(argextype(stmt.values[i], compact)),
                                 widenconst(t))
                push!(to_drop, i)
            end
        end
        compact.result[phi][:type] = t
        isempty(to_drop) && continue
        deleteat!(stmt.values, to_drop)
        deleteat!(stmt.edges, to_drop)
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
        safe_phis = SPCSet()
        for phi in all_phis
            # Save any phi cycles that have non-phi uses
            if compact.used_ssas[phi] - phi_uses[phi] != 0
                mark_phi_cycles!(compact, safe_phis, phi)
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
    lifted_undef = IdDict{Int, Any}()
    insts = ir.stmts
    for idx in 1:length(insts)
        stmt = insts[idx][:inst]
        stmt isa Expr || continue
        if (stmt.head === :isdefined || stmt.head === :undefcheck)
            # after optimization, undef can only show up by being introduced in
            # a phi node (or an UpsilonNode() argument to a PhiC node), so lift
            # all these nodes that have maybe undef values
            val = stmt.args[(stmt.head === :isdefined) ? 1 : 2]
            if stmt.head === :isdefined && (val isa Slot || val isa GlobalRef ||
                    isexpr(val, :static_parameter) || val isa Argument || val isa Symbol)
                # this is a legal node, so assume it was not introduced by
                # slot2ssa (at worst, we might leave in a runtime check that
                # shouldn't have been there)
                continue
            end
            # otherwise, we definitely have a corrupt node from slot2ssa, and
            # must fix or delete that now
            processed = IdDict{Int, Union{SSAValue, Bool}}()
            def = val
            while true
                # peek through PiNodes
                isa(val, SSAValue) || break
                def = insts[val.id][:inst]
                isa(def, PiNode) || break
                val = def.val
            end
            if !isa(val, SSAValue) || (!isa(def, PhiNode) && !isa(def, PhiCNode))
                # in most cases, reaching this statement implies we had a value
                if stmt.head === :undefcheck
                    insts[idx][:inst] = nothing
                else
                    insts[idx][:inst] = true
                end
                continue
            end
            stmt_id = val.id
            worklist = Tuple{Int, Int, SSAValue, Int}[(stmt_id, 0, SSAValue(0), 0)]
            if !haskey(lifted_undef, stmt_id)
                first = true
                while !isempty(worklist)
                    item, w_up_id, which, use = pop!(worklist)
                    def = insts[item][:inst]
                    if isa(def, PhiNode)
                        edges = copy(def.edges)
                        values = Vector{Any}(undef, length(edges))
                        new_phi = if length(values) == 0
                            false
                        else
                            insert_node!(ir, item, NewInstruction(PhiNode(edges, values), Bool))
                        end
                    else
                        def = def::PhiCNode
                        values = Vector{Any}(undef, length(def.values))
                        new_phi = if length(values) == 0
                            false
                        else
                            insert_node!(ir, item, NewInstruction(PhiCNode(values), Bool))
                        end
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
                            up_id = id = (def.values[i]::SSAValue).id
                            @label restart
                            if !isa(ir.stmts[id][:type], MaybeUndef)
                                val = true
                            else
                                node = insts[id][:inst]
                                if isa(node, UpsilonNode)
                                    if !isdefined(node, :val)
                                        val = false
                                    elseif !isa(node.val, SSAValue)
                                        val = true
                                    else
                                        id = (node.val::SSAValue).id
                                        @goto restart
                                    end
                                else
                                    while isa(node, PiNode)
                                        id = node.val.id
                                        node = insts[id][:inst]
                                    end
                                    if isa(node, Union{PhiNode, PhiCNode})
                                        if haskey(processed, id)
                                            val = processed[id]
                                        else
                                            push!(worklist, (id, up_id, new_phi::SSAValue, i))
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
                            values[i] = insert_node!(ir, up_id, NewInstruction(UpsilonNode(val), Bool))
                        end
                    end
                    if which !== SSAValue(0)
                        phi = ir[which][:inst]
                        if isa(phi, PhiNode)
                            phi.values[use] = new_phi
                        else
                            phi = phi::PhiCNode
                            phi.values[use] = insert_node!(ir, w_up_id, NewInstruction(UpsilonNode(new_phi), Bool))
                        end
                    end
                end
            end
            inst = lifted_undef[stmt_id]
            if stmt.head === :undefcheck
                inst = Expr(:throw_undef_if_not, stmt.args[1], inst)
            end
            insts[idx][:inst] = inst
        end
    end
    ir
end

function is_bb_empty(ir::IRCode, bb::BasicBlock)
    isempty(bb.stmts) && return true
    if length(bb.stmts) == 1
        stmt = ir[SSAValue(first(bb.stmts))][:inst]
        return stmt === nothing || isa(stmt, GotoNode)
    end
    return false
end

# TODO: This is terrible, we should change the IR for GotoIfNot to gain an else case
function is_legal_bb_drop(ir::IRCode, bbidx::Int, bb::BasicBlock)
    # If the block we're going to is the same as the fallthrow, it's always legal to drop
    # the block.
    length(bb.stmts) == 0 && return true
    if length(bb.stmts) == 1
        stmt = ir[SSAValue(first(bb.stmts))][:inst]
        stmt === nothing && return true
        ((stmt::GotoNode).label == bbidx + 1) && return true
    end
    # Otherwise make sure we're not the fallthrough case of any predecessor
    for pred in bb.preds
        if pred == bbidx - 1
            terminator = ir[SSAValue(first(bb.stmts)-1)][:inst]
            if isa(terminator, GotoIfNot)
                if terminator.dest != bbidx
                    return false
                end
            end
            break
        end
    end
    return true
end

function cfg_simplify!(ir::IRCode)
    bbs = ir.cfg.blocks
    merge_into = zeros(Int, length(bbs))
    merged_succ = zeros(Int, length(bbs))
    dropped_bbs = Vector{Int}() # sorted
    function follow_merge_into(idx::Int)
        while merge_into[idx] != 0
            idx = merge_into[idx]
        end
        return idx
    end
    function follow_merged_succ(idx::Int)
        while merged_succ[idx] != 0
            idx = merged_succ[idx]
        end
        return idx
    end

    # Walk the CFG from the entry block and aggressively combine blocks
    for (idx, bb) in enumerate(bbs)
        if length(bb.succs) == 1
            succ = bb.succs[1]
            if length(bbs[succ].preds) == 1
                # Prevent cycles by making sure we don't end up back at `idx`
                # by following what is to be merged into `succ`
                if follow_merged_succ(succ) != idx
                    merge_into[succ] = idx
                    merged_succ[idx] = succ
                end
            elseif is_bb_empty(ir, bb) && is_legal_bb_drop(ir, idx, bb)
                # If this BB is empty, we can still merge it as long as none of our successor's phi nodes
                # reference our predecessors.
                found_interference = false
                for idx in bbs[succ].stmts
                    stmt = ir[SSAValue(idx)][:inst]
                    stmt === nothing && continue
                    isa(stmt, PhiNode) || break
                    for edge in stmt.edges
                        for pred in bb.preds
                            if pred == edge
                                found_interference = true
                                @goto done
                            end
                        end
                    end
                end
                @label done
                if !found_interference
                    push!(dropped_bbs, idx)
                end
            end
        end
    end

    # Assign new BB numbers
    max_bb_num = 1
    bb_rename_succ = zeros(Int, length(bbs))
    for i = 1:length(bbs)
        # Drop blocks that will be merged away
        if merge_into[i] != 0
            bb_rename_succ[i] = -1
        end
        # Drop blocks with no predecessors
        if i != 1 && length(ir.cfg.blocks[i].preds) == 0
            bb_rename_succ[i] = -1
        end
        # Mark dropped blocks for fixup
        if !isempty(searchsorted(dropped_bbs, i))
            bb_rename_succ[i] = -bbs[i].succs[1]
        end

        bb_rename_succ[i] != 0 && continue

        curr = i
        while true
            bb_rename_succ[curr] = max_bb_num
            max_bb_num += 1
            # Now walk the chain of blocks we merged.
            # If we end in something that may fall through,
            # we have to schedule that block next
            curr = follow_merged_succ(curr)
            terminator = ir.stmts[ir.cfg.blocks[curr].stmts[end]][:inst]
            if isa(terminator, GotoNode) || isa(terminator, ReturnNode)
                break
            end
            curr += 1
            if !isempty(searchsorted(dropped_bbs, curr))
                break
            end
        end
    end

    # Compute map from new to old blocks
    result_bbs = Int[findfirst(j->i==j, bb_rename_succ) for i = 1:max_bb_num-1]

    # Fixup dropped BBs
    resolved_all = false
    while !resolved_all
        # TODO: There are faster ways to do this
        resolved_all = true
        for bb in dropped_bbs
            obb = bb_rename_succ[bb]
            if obb < -1
                nsucc = bb_rename_succ[-obb]
                if nsucc == -1
                    nsucc = -merge_into[-obb]
                end
                bb_rename_succ[bb] = nsucc
                resolved_all = false
            end
        end
    end

    # Figure out how predecessors should be renamed
    bb_rename_pred = zeros(Int, length(bbs))
    for i = 1:length(bbs)
        if merged_succ[i] != 0
            # Block `i` should no longer be a predecessor (before renaming)
            # because it is being merged with its sole successor
            bb_rename_pred[i] = -1
            continue
        end
        pred = i
        while pred !== 1 && !isempty(searchsorted(dropped_bbs, pred))
            pred = bbs[pred].preds[1]
        end
        bbnum = follow_merge_into(pred)
        bb_rename_pred[i] = bb_rename_succ[bbnum]
    end

    # Compute new block lengths
    result_bbs_lengths = zeros(Int, max_bb_num-1)
    for (idx, orig_bb) in enumerate(result_bbs)
        ms = orig_bb
        while ms != 0
            result_bbs_lengths[idx] += length(bbs[ms].stmts)
            ms = merged_succ[ms]
        end
    end

    # Compute statement indices the new blocks start at
    bb_starts = Vector{Int}(undef, 1+length(result_bbs_lengths))
    bb_starts[1] = 1
    for i = 1:length(result_bbs_lengths)
        bb_starts[i+1] = bb_starts[i] + result_bbs_lengths[i]
    end

    cresult_bbs = let result_bbs = result_bbs,
                      merged_succ = merged_succ,
                      merge_into = merge_into,
                      bbs = bbs,
                      bb_rename_succ = bb_rename_succ

        # Compute (renamed) successors and predecessors given (renamed) block
        function compute_succs(i)
            orig_bb = follow_merged_succ(result_bbs[i])
            return Int[bb_rename_succ[i] for i in bbs[orig_bb].succs]
        end
        function compute_preds(i)
            orig_bb = result_bbs[i]
            preds = bbs[orig_bb].preds
            return Int[bb_rename_pred[pred] for pred in preds]
        end

        BasicBlock[
            BasicBlock(StmtRange(bb_starts[i],
                                 i+1 > length(bb_starts) ?
                                    length(compact.result) : bb_starts[i+1]-1),
                       compute_preds(i),
                       compute_succs(i))
            for i = 1:length(result_bbs)]
    end

    # Fixup terminators for any blocks that would have caused double edges
    for (bbidx, (new_bb, old_bb)) in enumerate(zip(cresult_bbs, result_bbs))
        @assert length(new_bb.succs) <= 2
        length(new_bb.succs) <= 1 && continue
        if new_bb.succs[1] == new_bb.succs[2]
            terminator = ir[SSAValue(last(bbs[old_bb].stmts))]
            @assert isa(terminator[:inst], GotoIfNot)
            terminator[:inst] = GotoNode(terminator[:inst].dest)
            pop!(new_bb.succs)
            new_succ = cresult_bbs[new_bb.succs[1]]
            for (i, nsp) in enumerate(new_succ.preds)
                if nsp == bbidx
                    deleteat!(new_succ.preds, i)
                    break
                end
            end
        end
    end

    compact = IncrementalCompact(ir, true)
    # Run instruction compaction to produce the result,
    # but we're messing with the CFG
    # so we don't want compaction to do so independently
    compact.fold_constant_branches = false
    compact.bb_rename_succ = bb_rename_succ
    compact.bb_rename_pred = bb_rename_pred
    compact.result_bbs = cresult_bbs
    result_idx = 1
    for (idx, orig_bb) in enumerate(result_bbs)
        ms = orig_bb
        while ms != 0
            for i in bbs[ms].stmts
                node = ir.stmts[i]
                compact.result[compact.result_idx] = node
                if isa(node[:inst], GotoNode) && merged_succ[ms] != 0
                    # If we merged a basic block, we need remove the trailing GotoNode (if any)
                    compact.result[compact.result_idx][:inst] = nothing
                else
                    process_node!(compact, compact.result_idx, node, i, i, ms, true)
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
