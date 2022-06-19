# This file is a part of Julia. License is MIT: https://julialang.org/license

function is_known_call(@nospecialize(x), @nospecialize(func), ir::Union{IRCode,IncrementalCompact})
    isexpr(x, :call) || return false
    ft = argextype(x.args[1], ir)
    return singleton_type(ft) === func
end

struct SSAUse
    kind::Symbol
    idx::Int
end
GetfieldUse(idx::Int)  = SSAUse(:getfield, idx)
PreserveUse(idx::Int)  = SSAUse(:preserve, idx)
NoPreserve()           = SSAUse(:nopreserve, 0)
IsdefinedUse(idx::Int) = SSAUse(:isdefined, idx)
AddFinalizerUse(idx::Int) = SSAUse(:add_finalizer, idx)

"""
    du::SSADefUse

This struct keeps track of all uses of some mutable struct allocated in the current function:
- `du.uses::Vector{SSAUse}` are some "usages" (like `getfield`) of the struct
- `du.defs::Vector{Int}` are all instances of `setfield!` on the struct
The terminology refers to the uses/defs of the "slot bundle" that the mutable struct represents.

`du.uses` tracks all instances of `getfield` and `isdefined` calls on the struct.
Additionally it also tracks all instances of a `:foreigncall` that preserves of this mutable
struct. Somewhat counterintuitively, we don't actually need to make sure that the struct
itself is live (or even allocated) at a `ccall` site. If there are no other places where
the struct escapes (and thus e.g. where its address is taken), it need not be allocated.
We do however, need to make sure to preserve any elements of this struct.
"""
struct SSADefUse
    uses::Vector{SSAUse}
    defs::Vector{Int}
end
SSADefUse() = SSADefUse(SSAUse[], Int[])

function compute_live_ins(cfg::CFG, du::SSADefUse)
    uses = Int[]
    for use in du.uses
        use.kind === :isdefined && continue # filter out `isdefined` usages
        push!(uses, use.idx)
    end
    compute_live_ins(cfg, du.defs, uses)
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

function find_curblock(domtree::DomTree, allblocks::Vector{Int}, curblock::Int)
    # TODO: This can be much faster by looking at current level and only
    # searching for those blocks in a sorted order
    while !(curblock in allblocks)
        curblock = domtree.idoms_bb[curblock]
    end
    return curblock
end

function val_for_def_expr(ir::IRCode, def::Int, fidx::Int)
    ex = ir[SSAValue(def)][:inst]
    if isexpr(ex, :new)
        return ex.args[1+fidx]
    else
        @assert isa(ex, Expr)
        # The use is whatever the setfield was
        return ex.args[4]
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

function compute_value_for_use(ir::IRCode, domtree::DomTree, allblocks::Vector{Int},
    du::SSADefUse, phinodes::IdDict{Int, SSAValue}, fidx::Int, use::Int)
    def, useblock, curblock = find_def_for_use(ir, domtree, allblocks, du, use)
    if def == 0
        if !haskey(phinodes, curblock)
            # If this happens, we need to search the predecessors for defs. Which
            # one doesn't matter - if it did, we'd have had a phinode
            return compute_value_for_block(ir, domtree, allblocks, du, phinodes, fidx, first(ir.cfg.blocks[useblock].preds))
        end
        # The use is the phinode
        return phinodes[curblock]
    else
        return val_for_def_expr(ir, def, fidx)
    end
end

# even when the allocation contains an uninitialized field, we try an extra effort to check
# if this load at `idx` have any "safe" `setfield!` calls that define the field
function has_safe_def(
    ir::IRCode, domtree::DomTree, allblocks::Vector{Int}, du::SSADefUse,
    newidx::Int, idx::Int)
    def, _, _ = find_def_for_use(ir, domtree, allblocks, du, idx)
    # will throw since we already checked this `:new` site doesn't define this field
    def == newidx && return false
    # found a "safe" definition
    def ≠ 0 && return true
    # we may still be able to replace this load with `PhiNode`
    # examine if all predecessors of `block` have any "safe" definition
    block = block_for_inst(ir, idx)
    seen = BitSet(block)
    worklist = BitSet(ir.cfg.blocks[block].preds)
    isempty(worklist) && return false
    while !isempty(worklist)
        pred = pop!(worklist)
        # if this block has already been examined, bail out to avoid infinite cycles
        pred in seen && return false
        idx = last(ir.cfg.blocks[pred].stmts)
        # NOTE `idx` isn't a load, thus we can use inclusive coondition within the `find_def_for_use`
        def, _, _ = find_def_for_use(ir, domtree, allblocks, du, idx, true)
        # will throw since we already checked this `:new` site doesn't define this field
        def == newidx && return false
        push!(seen, pred)
        # found a "safe" definition for this predecessor
        def ≠ 0 && continue
        # check for the predecessors of this predecessor
        for newpred in ir.cfg.blocks[pred].preds
            push!(worklist, newpred)
        end
    end
    return true
end

# find the first dominating def for the given use
function find_def_for_use(
    ir::IRCode, domtree::DomTree, allblocks::Vector{Int}, du::SSADefUse, use::Int, inclusive::Bool=false)
    useblock = block_for_inst(ir.cfg, use)
    curblock = find_curblock(domtree, allblocks, useblock)
    local def = 0
    for idx in du.defs
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
        def = compact[defssa][:inst]
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
    def = compact[defssa][:inst]
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
        def = compact[defssa][:inst]
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
                typ = unwrap_unionall(widenconst(types(compact)[leaf]))
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
                if isexpr(ocdef, :new_opaque_closure) && isa(field, Int) && 1 ≤ field ≤ length(ocdef.args)-4
                    lift_arg!(compact, leaf, cache_key, ocdef, 4+field, lifted_leaves)
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
                leaf = getglobal(mod, name)
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
        if already_inserted(compact, lifted)
            lifted = compact.ssa_rename[lifted.id]
        end
    end
    if isa(lifted, GlobalRef) || isa(lifted, Expr)
        lifted = insert_node!(compact, leaf, effect_free(NewInstruction(lifted, argextype(lifted, compact))))
        compact[leaf] = nothing
        stmt.args[argidx] = lifted
        compact[leaf] = stmt
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
            def = compact[leaf][:inst]
        else
            def = leaf
        end
    elseif isa(leaf, AnySSAValue)
        def = compact[leaf][:inst]
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
        lifted_leaves::LiftedLeaves, val, nothing)::LiftedValue

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

mutable struct LazyDomtree
    ir::IRCode
    domtree::DomTree
    LazyDomtree(ir::IRCode) = new(ir)
end
function get(x::LazyDomtree)
    isdefined(x, :domtree) && return x.domtree
    return @timeit "domtree 2" x.domtree = construct_domtree(x.ir.cfg.blocks)
end

function perform_lifting!(compact::IncrementalCompact,
    visited_phinodes::Vector{AnySSAValue}, @nospecialize(cache_key),
    lifting_cache::IdDict{Pair{AnySSAValue, Any}, AnySSAValue},
    @nospecialize(result_t), lifted_leaves::LiftedLeaves, @nospecialize(stmt_val),
    lazydomtree::Union{LazyDomtree,Nothing})
    reverse_mapping = IdDict{AnySSAValue, Int}(ssa => id for (id, ssa) in enumerate(visited_phinodes))

    # Check if all the lifted leaves are the same
    local the_leaf
    all_same = true
    for (_, val) in lifted_leaves
        if !@isdefined(the_leaf)
            the_leaf = val
            continue
        end
        if val !== the_leaf
            all_same = false
        end
    end

    the_leaf_val = isa(the_leaf, LiftedValue) ? the_leaf.x : nothing
    if !isa(the_leaf_val, SSAValue)
        all_same = false
    end

    if all_same
        dominates_all = true
        if lazydomtree !== nothing
            domtree = get(lazydomtree)
            for item in visited_phinodes
                if !dominates_ssa(compact, domtree, the_leaf_val, item)
                    dominates_all = false
                    break
                end
            end
            if dominates_all
                return the_leaf
            end
        end
    end

    # Insert PhiNodes
    lifted_phis = LiftedPhi[]
    for item in visited_phinodes
        # FIXME this cache is broken somehow
        # ckey = Pair{AnySSAValue, Any}(item, cache_key)
        # cached = ckey in keys(lifting_cache)
        cached = false
        if cached
            ssa = lifting_cache[ckey]
            push!(lifted_phis, LiftedPhi(ssa, compact[ssa][:inst]::PhiNode, false))
            continue
        end
        n = PhiNode()
        ssa = insert_node!(compact, item, effect_free(NewInstruction(n, result_t)))
        # lifting_cache[ckey] = ssa
        push!(lifted_phis, LiftedPhi(ssa, n, true))
    end

    # Fix up arguments
    for (old_node_ssa, lf) in zip(visited_phinodes, lifted_phis)
        old_node = compact[old_node_ssa][:inst]::PhiNode
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
        count_added_node!(compact, new_node)
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
    sroa_pass!(ir::IRCode) -> newir::IRCode

`getfield` elimination pass, a.k.a. Scalar Replacements of Aggregates optimization.

This pass is based on a local field analysis by def-use chain walking.
It looks for struct allocation sites ("definitions"), and `getfield` calls as well as
`:foreigncall`s that preserve the structs ("usages"). If "definitions" have enough information,
then this pass will replace corresponding usages with forwarded values.
`mutable struct`s require additional cares and need to be handled separately from immutables.
For `mutable struct`s, `setfield!` calls account for "definitions" also, and the pass should
give up the lifting conservatively when there are any "intermediate usages" that may escape
the mutable struct (e.g. non-inlined generic function call that takes the mutable struct as
its argument).

In a case when all usages are fully eliminated, `struct` allocation may also be erased as
a result of succeeding dead code elimination.
"""
function sroa_pass!(ir::IRCode, inlining::Union{Nothing, InliningState} = nothing)
    compact = IncrementalCompact(ir)
    defuses = nothing # will be initialized once we encounter mutability in order to reduce dynamic allocations
    lifting_cache = IdDict{Pair{AnySSAValue, Any}, AnySSAValue}()
    # initialization of domtree is delayed to avoid the expensive computation in many cases
    lazydomtree = LazyDomtree(ir)
    for ((_, idx), stmt) in compact
        # check whether this statement is `getfield` / `setfield!` (or other "interesting" statement)
        isa(stmt, Expr) || continue
        is_setfield = is_isdefined = is_finalizer = false
        field_ordering = :unspecified
        if is_known_call(stmt, setfield!, compact)
            4 <= length(stmt.args) <= 5 || continue
            is_setfield = true
            if length(stmt.args) == 5
                field_ordering = argextype(stmt.args[5], compact)
            end
        elseif is_known_call(stmt, getfield, compact)
            3 <= length(stmt.args) <= 5 || continue
            if length(stmt.args) == 5
                field_ordering = argextype(stmt.args[5], compact)
            elseif length(stmt.args) == 4
                field_ordering = argextype(stmt.args[4], compact)
                widenconst(field_ordering) === Bool && (field_ordering = :unspecified)
            end
        elseif is_known_call(stmt, isdefined, compact)
            3 <= length(stmt.args) <= 4 || continue
            is_isdefined = true
            if length(stmt.args) == 4
                field_ordering = argextype(stmt.args[4], compact)
                widenconst(field_ordering) === Bool && (field_ordering = :unspecified)
            end
        elseif is_known_call(stmt, Core.finalizer, compact)
            3 <= length(stmt.args) <= 5 || continue
            # Inlining performs legality checks on the finalizer to determine
            # whether or not we may inline it. If so, it appends extra arguments
            # at the end of the intrinsic. Detect that here.
            length(stmt.args) == 5 || continue
            is_finalizer = true
        elseif isexpr(stmt, :foreigncall)
            nccallargs = length(stmt.args[3]::SimpleVector)
            preserved = Int[]
            new_preserves = Any[]
            for pidx in (6+nccallargs):length(stmt.args)
                preserved_arg = stmt.args[pidx]
                isa(preserved_arg, SSAValue) || continue
                let intermediaries = SPCSet()
                    callback = function (@nospecialize(pi), @nospecialize(ssa))
                        push!(intermediaries, ssa.id)
                        return false
                    end
                    def = simple_walk(compact, preserved_arg, callback)
                    isa(def, SSAValue) || continue
                    defidx = def.id
                    def = compact[def][:inst]
                    if is_known_call(def, tuple, compact)
                        record_immutable_preserve!(new_preserves, def, compact)
                        push!(preserved, preserved_arg.id)
                        continue
                    elseif isexpr(def, :new)
                        typ = unwrap_unionall(widenconst(argextype(SSAValue(defidx), compact)))
                        if typ isa DataType && !ismutabletype(typ)
                            record_immutable_preserve!(new_preserves, def, compact)
                            push!(preserved, preserved_arg.id)
                            continue
                        end
                    else
                        continue
                    end
                    if defuses === nothing
                        defuses = IdDict{Int, Tuple{SPCSet, SSADefUse}}()
                    end
                    mid, defuse = get!(()->(SPCSet(),SSADefUse()), defuses, defidx)
                    push!(defuse.uses, PreserveUse(idx))
                    union!(mid, intermediaries)
                end
                continue
            end
            if !isempty(new_preserves)
                compact[idx] = nothing
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
            end
            continue
        end

        # analyze this `getfield` / `isdefined` / `setfield!` call

        if !is_finalizer
            field = try_compute_field_stmt(compact, stmt)
            field === nothing && continue
            val = stmt.args[2]
        else
            val = stmt.args[3]
        end

        struct_typ = unwrap_unionall(widenconst(argextype(val, compact)))
        if isa(struct_typ, Union) && struct_typ <: Tuple
            struct_typ = unswitchtupleunion(struct_typ)
        end
        if isa(struct_typ, Union) && is_isdefined
            lift_comparison!(isdefined, compact, idx, stmt, lifting_cache)
            continue
        end
        isa(struct_typ, DataType) || continue

        struct_typ.name.atomicfields == C_NULL || continue # TODO: handle more
        if !((field_ordering === :unspecified) ||
             (field_ordering isa Const && field_ordering.val === :not_atomic))
            continue
        end

        # analyze this mutable struct here for the later pass
        if ismutabletype(struct_typ)
            isa(val, SSAValue) || continue
            let intermediaries = SPCSet()
                callback = function (@nospecialize(pi), @nospecialize(ssa))
                    push!(intermediaries, ssa.id)
                    return false
                end
                def = simple_walk(compact, val, callback)
                # Mutable stuff here
                isa(def, SSAValue) || continue
                if defuses === nothing
                    defuses = IdDict{Int, Tuple{SPCSet, SSADefUse}}()
                end
                mid, defuse = get!(()->(SPCSet(),SSADefUse()), defuses, def.id)
                if is_setfield
                    push!(defuse.defs, idx)
                elseif is_isdefined
                    push!(defuse.uses, IsdefinedUse(idx))
                elseif is_finalizer
                    push!(defuse.uses, AddFinalizerUse(idx))
                else
                    push!(defuse.uses, GetfieldUse(idx))
                end
                union!(mid, intermediaries)
            end
            continue
        elseif is_setfield || is_finalizer
            continue # invalid `setfield!` or `Core.finalizer` call, but just ignore here
        elseif is_isdefined
            continue # TODO?
        end

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
            visited_phinodes, field, lifting_cache, result_t, lifted_leaves, val, lazydomtree)

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
    if defuses !== nothing
        # now go through analyzed mutable structs and see which ones we can eliminate
        # NOTE copy the use count here, because `simple_dce!` may modify it and we need it
        # consistent with the state of the IR here (after tracking `PhiNode` arguments,
        # but before the DCE) for our predicate within `sroa_mutables!`, but we also
        # try an extra effort using a callback so that reference counts are updated
        used_ssas = copy(compact.used_ssas)
        simple_dce!(compact, (x::SSAValue) -> used_ssas[x.id] -= 1)
        ir = complete(compact)
        sroa_mutables!(ir, defuses, used_ssas, lazydomtree, inlining)
        return ir
    else
        simple_dce!(compact)
        return complete(compact)
    end
end

function try_inline_finalizer!(ir::IRCode, argexprs::Vector{Any}, idx::Int, mi::MethodInstance, inlining::InliningState)
    code = get(inlining.mi_cache, mi, nothing)
    if code isa CodeInstance
        if use_const_api(code)
            # No code in the function - Nothing to do
            inlining.et !== nothing && push!(inlining.et, mi)
            return true
        end
        src = code.inferred
    else
        src = code
    end

    src = inlining_policy(inlining.interp, src, IR_FLAG_NULL, mi, Any[])
    src === nothing && return false
    src = retrieve_ir_for_inlining(mi, src)

    # For now: Require finalizer to only have one basic block
    length(src.cfg.blocks) == 1 || return false

    # Ok, we're committed to inlining the finalizer
    inlining.et !== nothing && push!(inlining.et, mi)

    linetable_offset, extra_coverage_line = ir_inline_linetable!(ir.linetable, src, mi.def, ir[SSAValue(idx)][:line])
    if extra_coverage_line != 0
        insert_node!(ir, idx, NewInstruction(Expr(:code_coverage_effect), Nothing, extra_coverage_line))
    end

    # TODO: Use the actual inliner here rather than open coding this special
    # purpose inliner.
    spvals = mi.sparam_vals
    ssa_rename = Vector{Any}(undef, length(src.stmts))
    for idx′ = 1:length(src.stmts)
        urs = userefs(src[SSAValue(idx′)][:inst])
        for ur in urs
            if isa(ur[], SSAValue)
                ur[] = ssa_rename[ur[].id]
            elseif isa(ur[], Argument)
                ur[] = argexprs[ur[].n]
            elseif isexpr(ur[], :static_parameter)
                ur[] = spvals[ur[].args[1]]
            end
        end
        # TODO: Scan newly added statement into the sroa defuse struct
        stmt = urs[]
        isa(stmt, ReturnNode) && continue
        inst = src[SSAValue(idx′)]
        ssa_rename[idx′] = insert_node!(ir, idx, NewInstruction(stmt, inst; line = inst[:line] + linetable_offset), true)
    end
    return true
end

is_nothrow(ir::IRCode, pc::Int) = ir.stmts[pc][:flag] & (IR_FLAG_EFFECT_FREE | IR_FLAG_NOTHROW) ≠ 0
function sroa_mutables!(ir::IRCode, defuses::IdDict{Int, Tuple{SPCSet, SSADefUse}}, used_ssas::Vector{Int}, lazydomtree::LazyDomtree, inlining::Union{Nothing, InliningState})
    for (idx, (intermediaries, defuse)) in defuses
        intermediaries = collect(intermediaries)
        # Check if there are any uses we did not account for. If so, the variable
        # escapes and we cannot eliminate the allocation. This works, because we're guaranteed
        # not to include any intermediaries that have dead uses. As a result, missing uses will only ever
        # show up in the nuses_total count.
        nleaves = length(defuse.uses) + length(defuse.defs)
        nuses = 0
        for idx in intermediaries
            nuses += used_ssas[idx]
        end
        nuses_total = used_ssas[idx] + nuses - length(intermediaries)
        nleaves == nuses_total || continue
        # Find the type for this allocation
        defexpr = ir[SSAValue(idx)][:inst]
        isexpr(defexpr, :new) || continue
        newidx = idx
        typ = unwrap_unionall(ir.stmts[newidx][:type])
        # Could still end up here if we tried to setfield! on an immutable, which would
        # error at runtime, but is not illegal to have in the IR.
        ismutabletype(typ) || continue
        typ = typ::DataType
        # First check for any add_finalizer calls
        add_finalizer_idx = nothing
        for use in defuse.uses
            if use.kind === :add_finalizer
                # For now: Only allow one add_finalizer per allocation
                add_finalizer_idx !== nothing && @goto skip
                add_finalizer_idx = use.idx
            end
        end
        if add_finalizer_idx !== nothing
            # For now: Require that all uses and defs are in the same basic block,
            # so that live range calculations are easy.
            bb = ir.cfg.blocks[block_for_inst(ir.cfg, first(defuse.uses).idx)]
            minval::Int = typemax(Int)
            maxval::Int = 0

            check_in_range(defuse) = check_in_range(defuse.idx)
            function check_in_range(didx::Int)
                didx in bb.stmts || return false
                if didx < minval
                    minval = didx
                end
                if didx > maxval
                    maxval = didx
                end
                return true
            end

            check_in_range(idx) || continue
            _all(check_in_range, defuse.uses) || continue
            _all(check_in_range, defuse.defs) || continue

            # For now: Require all statements in the basic block range to be
            # nothrow.
            all_nothrow = _all(idx->is_nothrow(ir, idx) || idx == add_finalizer_idx, minval:maxval)
            all_nothrow || continue

            # Ok, finalizer rewrite is legal.
            add_finalizer_stmt = ir[SSAValue(add_finalizer_idx)][:inst]
            argexprs = Any[add_finalizer_stmt.args[2], add_finalizer_stmt.args[3]]
            may_inline = add_finalizer_stmt.args[4]::Bool
            mi = add_finalizer_stmt.args[5]::Union{MethodInstance, Nothing}
            if may_inline && mi !== nothing
                if try_inline_finalizer!(ir, argexprs, maxval, add_finalizer_stmt.args[5], inlining)
                    @goto done_finalizer
                end
                mi = compileable_specialization(inlining.et, mi, Effects()).invoke
            end
            if mi !== nothing
                insert_node!(ir, maxval,
                    NewInstruction(Expr(:invoke, mi, argexprs...), Nothing),
                    true)
            else
                insert_node!(ir, maxval,
                    NewInstruction(Expr(:call, argexprs...), Nothing),
                    true)
            end
            @label done_finalizer
            # Erase call to add_finalizer
            ir[SSAValue(add_finalizer_idx)][:inst] = nothing
            continue
        end
        # Partition defuses by field
        fielddefuse = SSADefUse[SSADefUse() for _ = 1:fieldcount(typ)]
        all_eliminated = all_forwarded = true
        has_finalizer = false
        for use in defuse.uses
            if use.kind === :preserve
                for du in fielddefuse
                    push!(du.uses, use)
                end
                continue
            end
            stmt = ir[SSAValue(use.idx)][:inst] # == `getfield`/`isdefined` call
            # We may have discovered above that this use is dead
            # after the getfield elim of immutables. In that case,
            # it would have been deleted. That's fine, just ignore
            # the use in that case.
            if stmt === nothing
                all_forwarded = false
                continue
            end
            field = try_compute_fieldidx_stmt(ir, stmt::Expr, typ)
            field === nothing && @goto skip
            push!(fielddefuse[field].uses, use)
        end
        for def in defuse.defs
            stmt = ir[SSAValue(def)][:inst]::Expr # == `setfield!` call
            field = try_compute_fieldidx_stmt(ir, stmt, typ)
            field === nothing && @goto skip
            isconst(typ, field) && @goto skip # we discovered an attempt to mutate a const field, which must error
            push!(fielddefuse[field].defs, def)
        end
        # Check that the defexpr has defined values for all the fields
        # we're accessing. In the future, we may want to relax this,
        # but we should come up with semantics for well defined semantics
        # for uninitialized fields first.
        ndefuse = length(fielddefuse)
        blocks = Vector{Tuple{#=phiblocks=# Vector{Int}, #=allblocks=# Vector{Int}}}(undef, ndefuse)
        for fidx in 1:ndefuse
            du = fielddefuse[fidx]
            isempty(du.uses) && continue
            push!(du.defs, newidx)
            ldu = compute_live_ins(ir.cfg, du)
            if isempty(ldu.live_in_bbs)
                phiblocks = Int[]
            else
                phiblocks = iterated_dominance_frontier(ir.cfg, ldu, get(lazydomtree))
            end
            allblocks = sort!(vcat(phiblocks, ldu.def_bbs); alg=QuickSort)
            blocks[fidx] = phiblocks, allblocks
            if fidx + 1 > length(defexpr.args)
                for i = 1:length(du.uses)
                    use = du.uses[i]
                    if use.kind === :isdefined
                        if has_safe_def(ir, get(lazydomtree), allblocks, du, newidx, use.idx)
                            ir[SSAValue(use.idx)][:inst] = true
                        else
                            all_eliminated = false
                        end
                        continue
                    elseif use.kind === :preserve
                        if length(du.defs) == 1 # allocation with this field unintialized
                            # there is nothing to preserve, just ignore this use
                            du.uses[i] = NoPreserve()
                            continue
                        end
                    end
                    has_safe_def(ir, get(lazydomtree), allblocks, du, newidx, use.idx) || @goto skip
                end
            else # always have some definition at the allocation site
                for i = 1:length(du.uses)
                    use = du.uses[i]
                    if use.kind === :isdefined
                        ir[SSAValue(use.idx)][:inst] = true
                    end
                end
            end
        end
        # Everything accounted for. Go field by field and perform idf:
        # Compute domtree now, needed below, now that we have finished compacting the IR.
        # This needs to be after we iterate through the IR with `IncrementalCompact`
        # because removing dead blocks can invalidate the domtree.
        domtree = get(lazydomtree)
        local preserve_uses = nothing
        for fidx in 1:ndefuse
            du = fielddefuse[fidx]
            ftyp = fieldtype(typ, fidx)
            if !isempty(du.uses)
                phiblocks, allblocks = blocks[fidx]
                phinodes = IdDict{Int, SSAValue}()
                for b in phiblocks
                    phinodes[b] = insert_node!(ir, first(ir.cfg.blocks[b].stmts),
                        NewInstruction(PhiNode(), ftyp))
                end
                # Now go through all uses and rewrite them
                for use in du.uses
                    if use.kind === :getfield
                        ir[SSAValue(use.idx)][:inst] = compute_value_for_use(ir, domtree, allblocks,
                            du, phinodes, fidx, use.idx)
                    elseif use.kind === :isdefined
                        continue # already rewritten if possible
                    elseif use.kind === :nopreserve
                        continue # nothing to preserve (may happen when there are unintialized fields)
                    elseif use.kind === :preserve
                        newval = compute_value_for_use(ir, domtree, allblocks,
                            du, phinodes, fidx, use.idx)
                        if !isbitstype(widenconst(argextype(newval, ir)))
                            if preserve_uses === nothing
                                preserve_uses = IdDict{Int, Vector{Any}}()
                            end
                            push!(get!(()->Any[], preserve_uses, use.idx), newval)
                        end
                    else
                        @assert false "sroa_mutables!: unexpected use"
                    end
                end
                for b in phiblocks
                    n = ir[phinodes[b]][:inst]::PhiNode
                    for p in ir.cfg.blocks[b].preds
                        push!(n.edges, p)
                        push!(n.values, compute_value_for_block(ir, domtree,
                            allblocks, du, phinodes, fidx, p))
                    end
                end
            end
            all_eliminated || continue
            # all "usages" (i.e. `getfield` and `isdefined` calls) are eliminated,
            # now eliminate "definitions" (`setfield!`) calls
            # (NOTE the allocation itself will be eliminated by DCE pass later)
            for stmt in du.defs
                stmt == newidx && continue
                ir[SSAValue(stmt)][:inst] = nothing
            end
        end
        preserve_uses === nothing && continue
        if all_forwarded
            # this means all ccall preserves have been replaced with forwarded loads
            # so we can potentially eliminate the allocation, otherwise we must preserve
            # the whole allocation.
            push!(intermediaries, newidx)
        end
        # Insert the new preserves
        for (useidx, new_preserves) in preserve_uses
            ir[SSAValue(useidx)][:inst] = form_new_preserves(ir[SSAValue(useidx)][:inst]::Expr,
                intermediaries, new_preserves)
        end

        @label skip
    end
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

function adce_erase!(phi_uses::Vector{Int}, extra_worklist::Vector{Int}, compact::IncrementalCompact, idx::Int, in_worklist::Bool)
    # return whether this made a change
    if isa(compact.result[idx][:inst], PhiNode)
        return maybe_erase_unused!(extra_worklist, compact, idx, in_worklist, val::SSAValue -> phi_uses[val.id] -= 1)
    else
        return maybe_erase_unused!(extra_worklist, compact, idx, in_worklist)
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
            isa(compact[val][:inst], PhiNode) || continue
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

Note that this pass is more effective after SROA optimization (i.e. `sroa_pass!`),
since SROA often allows this pass to:
- eliminate allocation of object whose field references are all replaced with scalar values, and
- nullify `typeassert` call whose first operand has been replaced with a scalar value
  (, which may have introduced new type information that inference did not understand)

Also note that currently this pass _needs_ to run after `sroa_pass!`, because
the `typeassert` elimination depends on the transformation by `canonicalize_typeassert!` done
within `sroa_pass!` which redirects references of `typeassert`ed value to the corresponding `PiNode`.
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
        stmt = compact[SSAValue(phi)][:inst]
        stmt === nothing && continue
        stmt = stmt::PhiNode
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
        adce_erase!(phi_uses, extra_worklist, compact, idx, false)
    end
    while !isempty(extra_worklist)
        adce_erase!(phi_uses, extra_worklist, compact, pop!(extra_worklist), true)
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
            if adce_erase!(phi_uses, extra_worklist, compact, pop!(extra_worklist), true)
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
                                        id = (node.val::SSAValue).id
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
