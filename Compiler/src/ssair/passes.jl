# This file is a part of Julia. License is MIT: https://julialang.org/license

function is_known_call(@nospecialize(x), @nospecialize(func), ir::Union{IRCode,IncrementalCompact})
    isexpr(x, :call) || return false
    ft = argextype(x.args[1], ir)
    return singleton_type(ft) === func
end

function is_known_invoke_or_call(@nospecialize(x), @nospecialize(func), ir::Union{IRCode,IncrementalCompact})
    isinvoke = isexpr(x, :invoke)
    (isinvoke || isexpr(x, :call)) || return false
    narg = isinvoke ? 2 : 1
    length(x.args) < narg && return false
    ft = argextype(x.args[narg], ir)
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
FinalizerUse(idx::Int) = SSAUse(:finalizer, idx)

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
    compute_live_ins(cfg, sort!(du.defs), uses)
end

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

# assume `stmt` is a call of `getfield`/`setfield!`/`isdefined`
function try_compute_fieldidx_stmt(ir::Union{IncrementalCompact,IRCode}, stmt::Expr, @nospecialize(typ))
    field = try_compute_field(ir, stmt.args[3])
    return try_compute_fieldidx(typ, field)
end

function find_curblock(domtree::DomTree, allblocks::BitSet, curblock::Int)
    # TODO: This can be much faster by looking at current level and only
    # searching for those blocks in a sorted order
    while curblock ∉ allblocks && curblock ≠ 0
        curblock = domtree.idoms_bb[curblock]
    end
    return curblock
end

function val_for_def_expr(ir::IRCode, def::Int, fidx::Int)
    ex = ir[SSAValue(def)][:stmt]
    if isexpr(ex, :new)
        return ex.args[1+fidx]
    else
        @assert is_known_call(ex, setfield!, ir)
        return ex.args[4]
    end
end

function compute_value_for_block(ir::IRCode, domtree::DomTree, allblocks::BitSet, du::SSADefUse, phinodes::IdDict{Int, SSAValue}, fidx::Int, curblock::Int)
    curblock = find_curblock(domtree, allblocks, curblock)
    def = 0
    for stmt in du.defs
        if block_for_inst(ir.cfg, stmt) == curblock
            def = max(def, stmt)
        end
    end
    def == 0 ? phinodes[curblock] : val_for_def_expr(ir, def, fidx)
end

function compute_value_for_use(ir::IRCode, domtree::DomTree, allblocks::BitSet,
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
    ir::IRCode, domtree::DomTree, allblocks::BitSet, du::SSADefUse,
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
        # NOTE `idx` isn't a load, thus we can use inclusive condition within the `find_def_for_use`
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
    ir::IRCode, domtree::DomTree, allblocks::BitSet, du::SSADefUse, use::Int, inclusive::Bool=false)
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

function collect_leaves(compact::IncrementalCompact, @nospecialize(val), @nospecialize(typeconstraint), 𝕃ₒ::AbstractLattice,
                        predecessors::Pre = ((@nospecialize(def), compact::IncrementalCompact) -> isa(def, PhiNode) ? def.values : nothing)) where {Pre}
    if isa(val, Union{OldSSAValue, SSAValue})
        val, typeconstraint = simple_walk_constraint(compact, val, typeconstraint)
    end
    return walk_to_defs(compact, val, typeconstraint, predecessors, 𝕃ₒ)
end

abstract type WalkerCallback end

struct TrivialWalker <: WalkerCallback end
(::TrivialWalker)(@nospecialize(def), @nospecialize(defssa::AnySSAValue)) = nothing

struct PiWalker <: WalkerCallback end
function (::PiWalker)(@nospecialize(def), @nospecialize(defssa::AnySSAValue))
    if isa(def, PiNode)
        return LiftedValue(def.val)
    end
    return nothing
end

function simple_walk(compact::IncrementalCompact, @nospecialize(defssa::AnySSAValue),
                     walker_callback::WalkerCallback=TrivialWalker())
    while true
        if isa(defssa, OldSSAValue)
            if already_inserted(compact, defssa)
                rename = compact.ssa_rename[defssa.id]
                if isa(rename, Refined)
                    rename = rename.val
                end
                if isa(rename, AnySSAValue)
                    defssa = rename
                    continue
                end
                return rename
            end
        end
        def = compact[defssa][:stmt]
        if isa(def, AnySSAValue)
            walker_callback(def, defssa)
            if isa(def, SSAValue)
                is_old(compact, defssa) && (def = OldSSAValue(def.id))
            end
            defssa = def
        elseif isa(def, Union{PhiNode, PhiCNode, GlobalRef})
            return defssa
        else
            new_def = walker_callback(def, defssa)
            if new_def === nothing
                return defssa
            end
            new_def = new_def.val
            if !isa(new_def, AnySSAValue)
                return new_def
            elseif isa(new_def, SSAValue)
                is_old(compact, defssa) && (new_def = OldSSAValue(new_def.id))
            end
            defssa = new_def
        end
    end
end

mutable struct TypeConstrainingWalker <: WalkerCallback
    typeconstraint::Any
    TypeConstrainingWalker(@nospecialize(typeconstraint::Any)) = new(typeconstraint)
end
function (walker_callback::TypeConstrainingWalker)(@nospecialize(def), @nospecialize(defssa::AnySSAValue))
    if isa(def, PiNode)
        walker_callback.typeconstraint =
            typeintersect(walker_callback.typeconstraint, widenconst(def.typ))
        return LiftedValue(def.val)
    end
    return nothing
end
function simple_walk_constraint(compact::IncrementalCompact, @nospecialize(val::AnySSAValue),
                                @nospecialize(typeconstraint))
    def = simple_walk(compact, val, TypeConstrainingWalker(typeconstraint))
    return Pair{Any, Any}(def, typeconstraint)
end

"""
    walk_to_defs(compact, val, typeconstraint, predecessors)

Starting at `val` walk use-def chains to get all the leaves feeding into this `val`
(pruning those leaves ruled out by path conditions).

`predecessors(def, compact)` is a callback which should return the set of possible
predecessors for a "phi-like" node (PhiNode or Core.ifelse) or `nothing` otherwise.
"""
function walk_to_defs(compact::IncrementalCompact, @nospecialize(defssa), @nospecialize(typeconstraint), predecessors::Pre, 𝕃ₒ::AbstractLattice) where {Pre}
    visited_philikes = AnySSAValue[]
    isa(defssa, AnySSAValue) || return Any[defssa], visited_philikes
    def = compact[defssa][:stmt]
    if predecessors(def, compact) === nothing
        return Any[defssa], visited_philikes
    end
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
        def = compact[defssa][:stmt]
        values = predecessors(def, compact)
        if values !== nothing
            if isa(def, PhiNode) || length(values) > 1
                push!(visited_philikes, defssa)
            end
            possible_predecessors = Int[]

            for n in 1:length(values)
                isassigned(values, n) || continue
                val = values[n]
                if is_old(compact, defssa) && isa(val, SSAValue)
                    val = OldSSAValue(val.id)
                end
                edge_typ = widenconst(argextype(val, compact))
                hasintersect(edge_typ, typeconstraint) || continue
                push!(possible_predecessors, n)
            end
            for n in possible_predecessors
                val = values[n]
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
                            push!(worklist_constraints, tmerge(𝕃ₒ, new_constraint, visited_constraints[new_def]))
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
    return leaves, visited_philikes
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
    already_inserted_ssa(compact, compact.idx-1)(0, old)
end

function already_inserted_ssa(compact::IncrementalCompact, processed_idx::Int)
    return function did_already_insert(phi_arg::Int, old::OldSSAValue)
        id = old.id
        if id <= length(compact.ir.stmts)
            return id <= processed_idx
        end
        id -= length(compact.ir.stmts)
        if id <= length(compact.ir.new_nodes)
            return did_already_insert(phi_arg, OldSSAValue(compact.ir.new_nodes.info[id].pos))
        end
        id -= length(compact.ir.new_nodes)
        @assert id <= length(compact.pending_nodes)
        return !(id in compact.pending_perm)
    end
end

function is_pending(compact::IncrementalCompact, old::OldSSAValue)
    return old.id > length(compact.ir.stmts) + length(compact.ir.new_nodes)
end

function is_getfield_captures(@nospecialize(def), compact::IncrementalCompact, 𝕃ₒ::AbstractLattice)
    isa(def, Expr) || return false
    length(def.args) >= 3 || return false
    is_known_call(def, getfield, compact) || return false
    which = argextype(def.args[3], compact)
    isa(which, Const) || return false
    which.val === :captures || return false
    oc = argextype(def.args[2], compact)
    return ⊑(𝕃ₒ, oc, Core.OpaqueClosure)
end

struct LiftedValue
    val
    LiftedValue(@nospecialize val) = new(val)
end
const LiftedLeaves = IdDict{Any, Union{Nothing,LiftedValue}}
const LiftedDefs = IdDict{Any, Bool}

# try to compute lifted values that can replace `getfield(x, field)` call
# where `x` is an immutable struct that are defined at any of `leaves`
function lift_leaves(compact::IncrementalCompact, field::Int,
                     leaves::Vector{Any}, 𝕃ₒ::AbstractLattice)
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
                if ismutabletype(typ)
                    isconst(typ, field) || return nothing
                end
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
                end
                lift_arg!(compact, leaf, cache_key, def, 1+field, lifted_leaves)
                continue
            # NOTE we can enable this, but most `:splatnew` expressions are transformed into
            #      `:new` expressions by the inlinear
            # elseif isexpr(def, :splatnew) && length(def.args) == 2 && isa(def.args[2], AnySSAValue)
            #     tplssa = def.args[2]::AnySSAValue
            #     tplexpr = compact[tplssa][:stmt]
            #     if is_known_call(tplexpr, tuple, compact) && 1 ≤ field < length(tplexpr.args)
            #         lift_arg!(compact, tplssa, cache_key, tplexpr, 1+field, lifted_leaves)
            #         continue
            #     end
            #     return nothing
            elseif is_getfield_captures(def, compact, 𝕃ₒ)
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
            typ = argextype(leaf, compact)
            if isa(typ, Const)
                leaf = typ.val
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
            new_lifted = compact.ssa_rename[lifted.id]
            if isa(new_lifted, Refined)
                new_lifted = new_lifted.val
            end
            # Special case: If lifted happens to be the statement we're currently processing,
            # leave it as old SSAValue in case we decide to handle this in the renamer
            if !isa(new_lifted, SSAValue) || new_lifted != SSAValue(compact.result_idx-1)
                lifted = new_lifted
            end
        end
    end
    lifted_leaves[cache_key] = LiftedValue(lifted)
    return nothing
end

function walk_to_def(compact::IncrementalCompact, @nospecialize(leaf))
    if isa(leaf, OldSSAValue) && already_inserted(compact, leaf)
        leaf = compact.ssa_rename[leaf.id]
        if isa(leaf, Refined)
            leaf = leaf.val
        end
        if isa(leaf, AnySSAValue)
            leaf = simple_walk(compact, leaf)
        end
        if isa(leaf, AnySSAValue)
            def = compact[leaf][:stmt]
        else
            def = leaf
        end
    elseif isa(leaf, AnySSAValue)
        def = compact[leaf][:stmt]
    else
        def = leaf
    end
    return Pair{Any, Any}(def, leaf)
end

"""
    lift_comparison!(cmp, compact::IncrementalCompact, idx::Int, stmt::Expr, 𝕃ₒ::AbstractLattice)

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
    idx::Int, stmt::Expr, 𝕃ₒ::AbstractLattice)
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
    lift_comparison_leaves!(egal_tfunc, compact, val, cmp, idx, 𝕃ₒ)
end

function lift_comparison!(::typeof(isa), compact::IncrementalCompact,
    idx::Int, stmt::Expr, 𝕃ₒ::AbstractLattice)
    args = stmt.args
    length(args) == 3 || return
    cmp = argextype(args[3], compact)
    val = args[2]
    lift_comparison_leaves!(isa_tfunc, compact, val, cmp, idx, 𝕃ₒ)
end

function lift_comparison!(::typeof(isdefined), compact::IncrementalCompact,
    idx::Int, stmt::Expr, 𝕃ₒ::AbstractLattice)
    args = stmt.args
    length(args) == 3 || return
    cmp = argextype(args[3], compact)
    isa(cmp, Const) || return # `isdefined_tfunc` won't return Const
    val = args[2]
    lift_comparison_leaves!(isdefined_tfunc, compact, val, cmp, idx, 𝕃ₒ)
end

function phi_or_ifelse_predecessors(@nospecialize(def), compact::IncrementalCompact)
    isa(def, PhiNode) && return def.values
    is_known_call(def, Core.ifelse, compact) && return def.args[3:4]
    return nothing
end

function lift_comparison_leaves!(@specialize(tfunc),
    compact::IncrementalCompact, @nospecialize(val), @nospecialize(cmp),
    idx::Int, 𝕃ₒ::AbstractLattice)
    typeconstraint = widenconst(argextype(val, compact))
    if isa(val, Union{OldSSAValue, SSAValue})
        val, typeconstraint = simple_walk_constraint(compact, val, typeconstraint)
    end
    isa(typeconstraint, Union) || return # bail out if there won't be a good chance for lifting

    leaves, visited_philikes = collect_leaves(compact, val, typeconstraint, 𝕃ₒ, phi_or_ifelse_predecessors)
    length(leaves) ≤ 1 && return # bail out if we don't have multiple leaves

    # check if we can evaluate the comparison for each one of the leaves
    lifted_leaves = nothing
    for i = 1:length(leaves)
        leaf = leaves[i]
        result = tfunc(𝕃ₒ, argextype(leaf, compact), cmp)
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
    (lifted_val, nest) = perform_lifting!(compact,
        visited_philikes, cmp, Bool, lifted_leaves::LiftedLeaves, val, nothing)

    compact[idx] = (lifted_val::LiftedValue).val

    finish_phi_nest!(compact, nest)
end

struct IfElseCall
    call::Expr
end

# An intermediate data structure used for lifting expressions through a
# "phi-like" instruction (either a PhiNode or a call to Core.ifelse)
struct LiftedPhilike
    ssa::AnySSAValue
    node::Union{PhiNode,IfElseCall}
    need_argupdate::Bool
end

struct SkipToken end; const SKIP_TOKEN = SkipToken()

function lifted_value(compact::IncrementalCompact, @nospecialize(old_node_ssa::AnySSAValue),
                      @nospecialize(old_value), lifted_philikes::Vector{LiftedPhilike},
                      lifted_leaves::Union{LiftedLeaves, LiftedDefs},
                      reverse_mapping::IdDict{AnySSAValue, Int},
                      walker_callback::WalkerCallback)
    val = old_value
    if is_old(compact, old_node_ssa) && isa(val, SSAValue)
        val = OldSSAValue(val.id)
    end
    if isa(val, AnySSAValue)
        val = simple_walk(compact, val, LiftedLeaveWalker(lifted_leaves, reverse_mapping, walker_callback))
    end
    if val in keys(lifted_leaves)
        lifted_val = lifted_leaves[val]
        if isa(lifted_leaves, LiftedDefs)
            return lifted_val
        end
        lifted_val === nothing && return UNDEF_TOKEN
        val = lifted_val.val
        if isa(val, AnySSAValue)
            val = simple_walk(compact, val, PiWalker())
        end
        return val
    elseif isa(val, AnySSAValue) && val in keys(reverse_mapping)
        return lifted_philikes[reverse_mapping[val]].ssa
    else
        return SKIP_TOKEN # Probably ignored by path condition, skip this
    end
end

function is_old(compact, @nospecialize(old_node_ssa))
    isa(old_node_ssa, OldSSAValue) || return false
    is_pending(compact, old_node_ssa) && return false
    already_inserted(compact, old_node_ssa) && return false
    return true
end

struct PhiNest{C<:WalkerCallback}
    visited_philikes::Vector{AnySSAValue}
    lifted_philikes::Vector{LiftedPhilike}
    lifted_leaves::Union{LiftedLeaves, LiftedDefs}
    reverse_mapping::IdDict{AnySSAValue, Int}
    walker_callback::C
end

function finish_phi_nest!(compact::IncrementalCompact, nest::PhiNest)
    (;visited_philikes, lifted_philikes, lifted_leaves, reverse_mapping, walker_callback) = nest
    nphilikes = length(lifted_philikes)
    # Fix up arguments
    for i = 1:nphilikes
        (old_node_ssa, lf) = visited_philikes[i], lifted_philikes[i]
        lf.need_argupdate || continue
        should_count = !isa(lf.ssa, OldSSAValue) || already_inserted(compact, lf.ssa)

        lfnode = lf.node
        if isa(lfnode, PhiNode)
            old_node = compact[old_node_ssa][:stmt]::PhiNode
            new_node = lfnode
            for i = 1:length(old_node.values)
                isassigned(old_node.values, i) || continue
                val = lifted_value(compact, old_node_ssa, old_node.values[i],
                                   lifted_philikes, lifted_leaves, reverse_mapping, walker_callback)
                val !== SKIP_TOKEN && push!(new_node.edges, old_node.edges[i])
                if val === UNDEF_TOKEN
                    resize!(new_node.values, length(new_node.values)+1)
                elseif val !== SKIP_TOKEN
                    should_count && _count_added_node!(compact, val)
                    push!(new_node.values, val)
                end
            end
        elseif isa(lfnode, IfElseCall)
            old_node = compact[old_node_ssa][:stmt]::Expr
            then_result, else_result = old_node.args[3], old_node.args[4]

            then_result = lifted_value(compact, old_node_ssa, then_result,
                                       lifted_philikes, lifted_leaves, reverse_mapping, walker_callback)
            else_result = lifted_value(compact, old_node_ssa, else_result,
                                       lifted_philikes, lifted_leaves, reverse_mapping, walker_callback)

            # In cases where the Core.ifelse condition is statically-known, e.g., thanks
            # to a PiNode from a guarding conditional, replace with the remaining branch.
            if then_result === SKIP_TOKEN || else_result === SKIP_TOKEN
                only_result = (then_result === SKIP_TOKEN) ? else_result : then_result

                # Replace Core.ifelse(%cond, %a, %b) with %a
                compact[lf.ssa] = only_result

                # Note: Core.ifelse(%cond, %a, %b) has observable effects (!nothrow), but since
                # we have not deleted the preceding statement that this was derived from, this
                # replacement is safe, i.e. it will not affect the effects observed.
                continue
            end

            @assert then_result !== SKIP_TOKEN && then_result !== UNDEF_TOKEN
            @assert else_result !== SKIP_TOKEN && else_result !== UNDEF_TOKEN

            if should_count
                _count_added_node!(compact, then_result)
                _count_added_node!(compact, else_result)
            end

            push!(lfnode.call.args, then_result)
            push!(lfnode.call.args, else_result)
        end
    end
end

struct LiftedLeaveWalker{C<:WalkerCallback} <: WalkerCallback
    lifted_leaves::Union{LiftedLeaves, LiftedDefs}
    reverse_mapping::IdDict{AnySSAValue, Int}
    inner_walker_callback::C
    function LiftedLeaveWalker(@nospecialize(lifted_leaves::Union{LiftedLeaves, LiftedDefs}),
                               @nospecialize(reverse_mapping::IdDict{AnySSAValue, Int}),
                               inner_walker_callback::C) where C<:WalkerCallback
        return new{C}(lifted_leaves, reverse_mapping, inner_walker_callback)
    end
end
function (walker_callback::LiftedLeaveWalker)(@nospecialize(def), @nospecialize(defssa::AnySSAValue))
    (; lifted_leaves, reverse_mapping, inner_walker_callback) = walker_callback
    if defssa in keys(lifted_leaves) || defssa in keys(reverse_mapping)
        return nothing
    end
    isa(def, PiNode) && return LiftedValue(def.val)
    return inner_walker_callback(def, defssa)
end

function perform_lifting!(compact::IncrementalCompact,
        visited_philikes::Vector{AnySSAValue}, @nospecialize(cache_key),
        @nospecialize(result_t), lifted_leaves::Union{LiftedLeaves, LiftedDefs}, @nospecialize(stmt_val),
        lazydomtree::Union{LazyDomtree,Nothing}, walker_callback::WalkerCallback = TrivialWalker())
    reverse_mapping = IdDict{AnySSAValue, Int}()
    for id in 1:length(visited_philikes)
        reverse_mapping[visited_philikes[id]] = id
    end

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

    if all_same && isa(the_leaf, LiftedValue)
        dominates_all = true
        the_leaf_val = the_leaf.val
        if isa(the_leaf_val, AnySSAValue)
            if lazydomtree === nothing
                # Must conservatively assume this
                dominates_all = false
            else
                # This code guards against the possibility of accidentally forwarding a value from a
                # previous iteration. Consider for example:
                #
                # %p = phi(%arg, %t)
                # %b = <...>
                # %c = getfield(%p, 1)
                # %t = tuple(%b)
                #
                # It would be incorrect to replace `%c` by `%b`, because that would read the value of
                # `%b` in the *current* iteration, while the value of `%b` that comes in via `%p` is
                # that of the previous iteration.
                domtree = get!(lazydomtree)
                for item in visited_philikes
                    if !dominates_ssa(compact, domtree, the_leaf_val, item)
                        dominates_all = false
                        break
                    end
                end
            end
        end
        if dominates_all
            if isa(the_leaf_val, OldSSAValue)
                the_leaf = LiftedValue(simple_walk(compact, the_leaf_val))
            end
            return Pair{Any, PhiNest}(the_leaf, PhiNest(visited_philikes, Vector{LiftedPhilike}(undef, 0), lifted_leaves, reverse_mapping, walker_callback))
        end
    end

    # Insert PhiNodes
    nphilikes = length(visited_philikes)
    lifted_philikes = Vector{LiftedPhilike}(undef, nphilikes)
    for i = 1:nphilikes
        old_ssa = visited_philikes[i]
        old_inst = compact[old_ssa]
        old_node = old_inst[:stmt]::Union{PhiNode,Expr}
        if isa(old_node, PhiNode)
            new_node = PhiNode()
            ssa = insert_node!(compact, old_ssa, removable_if_unused(NewInstruction(new_node, result_t)))
            lifted_philikes[i] = LiftedPhilike(ssa, new_node, true)
        else
            @assert is_known_call(old_node, Core.ifelse, compact)
            ifelse_func, condition = old_node.args
            if is_old(compact, old_ssa) && isa(condition, SSAValue)
                condition = OldSSAValue(condition.id)
            end

            new_node = Expr(:call, ifelse_func, condition) # Renamed then_result, else_result added below
            new_inst = NewInstruction(new_node, result_t, NoCallInfo(), old_inst[:line], old_inst[:flag])

            ssa = insert_node!(compact, old_ssa, new_inst, #= attach_after =# true)
            lifted_philikes[i] = LiftedPhilike(ssa, IfElseCall(new_node), true)
        end
    end

    # Fixup the stmt itself
    if isa(stmt_val, Union{SSAValue, OldSSAValue})
        stmt_val = simple_walk(compact, stmt_val, LiftedLeaveWalker(lifted_leaves, reverse_mapping, walker_callback))
    end

    if stmt_val in keys(lifted_leaves)
        stmt_val = lifted_leaves[stmt_val]
    elseif isa(stmt_val, AnySSAValue) && stmt_val in keys(reverse_mapping)
        stmt_val = LiftedValue(lifted_philikes[reverse_mapping[stmt_val]].ssa)
    else
        error()
    end

    return Pair{Any, PhiNest}(stmt_val, PhiNest(visited_philikes, lifted_philikes, lifted_leaves, reverse_mapping, walker_callback))
end

# Handle _apply_iterate calls: convert arguments to use `Core.svec`.
# The behavior of `Core.svec` (with boxing) better matches the ABI of codegen.
function lift_apply_args!(compact::IncrementalCompact, idx::Int, stmt::Expr)
    compact[idx] = nothing
    for i in 4:length(stmt.args) # Skip `_apply_iterate`, `iterate`, and the function
        arg = stmt.args[i]
        arg_type = widenconst(argextype(arg, compact))
        if isa(arg_type, DataType) && arg_type.name === Tuple.name
            svec_args = nothing
            if isa(arg, SSAValue)
                arg_stmt = compact[arg][:stmt]
                if is_known_call(arg_stmt, Core.tuple, compact)
                    svec_args = copy(arg_stmt.args)
                end
            end
            if svec_args === nothing
                # Fallback path: generate getfield calls for tuple elements
                tuple_length = length(arg_type.parameters)
                if tuple_length > 0 && !isvarargtype(arg_type.parameters[tuple_length])
                    svec_args = Vector{Any}(undef, tuple_length + 1)
                    for j in 1:tuple_length
                        getfield_call = Expr(:call, GlobalRef(Core, :getfield), arg, j)
                        getfield_type = arg_type.parameters[j]
                        inst = compact[SSAValue(idx)]
                        getfield_ssa = insert_node!(compact, SSAValue(idx), NewInstruction(getfield_call, getfield_type, NoCallInfo(), inst[:line], inst[:flag]))
                        svec_args[j + 1] = getfield_ssa
                    end
                end
            end
            if svec_args !== nothing
                svec_args[1] = GlobalRef(Core, :svec)
                new_svec_call = Expr(:call)
                new_svec_call.args = svec_args
                inst = compact[SSAValue(idx)]
                new_svec_ssa = insert_node!(compact, SSAValue(idx), NewInstruction(new_svec_call, SimpleVector, NoCallInfo(), inst[:line], inst[:flag]))
                stmt.args[i] = new_svec_ssa
            end
        end
    end
    compact[idx] = stmt
    nothing
end

function lift_svec_ref!(compact::IncrementalCompact, idx::Int, stmt::Expr)
    length(stmt.args) != 3 && return

    vec = stmt.args[2]
    val = stmt.args[3]
    valT = argextype(val, compact)
    (isa(valT, Const) && isa(valT.val, Int)) || return
    valI = valT.val::Int
    valI >= 1 || return

    if isa(vec, SimpleVector)
        valI <= length(vec) || return
        compact[idx] = quoted(vec[valI])
    elseif isa(vec, SSAValue)
        def = compact[vec][:stmt]
        if is_known_call(def, Core.svec, compact)
            valI <= length(def.args) - 1 || return
            compact[idx] = def.args[valI+1]
        elseif is_known_call(def, Core._compute_sparams, compact)
            valI != 1 && return # TODO generalize this for more values of valI
            res = _lift_svec_ref(def, compact)
            res === nothing && return
            compact[idx] = res.val
        end
    end
    return
end

function lift_leaves_keyvalue(compact::IncrementalCompact, @nospecialize(key),
                             leaves::Vector{Any}, 𝕃ₒ::AbstractLattice)
    # For every leaf, the lifted value
    lifted_leaves = LiftedLeaves()
    for i = 1:length(leaves)
        leaf = leaves[i]
        cache_key = leaf
        if isa(leaf, AnySSAValue)
            (def, leaf) = walk_to_def(compact, leaf)
            if is_known_invoke_or_call(def, Core.OptimizedGenerics.KeyValue.set, compact)
                @assert isexpr(def, :invoke)
                if length(def.args) in (5, 6)
                    set_key = def.args[end-1]
                    set_val_idx = length(def.args)
                elseif length(def.args) == 4
                    # Key is deleted
                    # TODO: Model this
                    return nothing
                elseif length(def.args) == 3
                    # The whole collection is deleted
                    # TODO: Model this
                    return nothing
                else
                    return nothing
                end
                if set_key === key || (egal_tfunc(𝕃ₒ, argextype(key, compact), argextype(set_key, compact)) == Const(true))
                    lift_arg!(compact, leaf, cache_key, def, set_val_idx, lifted_leaves)
                    continue
                end
            end
        end
        return nothing
    end
    return lifted_leaves
end

function keyvalue_predecessors(@nospecialize(key), 𝕃ₒ::AbstractLattice)
    function(@nospecialize(def), compact::IncrementalCompact)
        if is_known_invoke_or_call(def, Core.OptimizedGenerics.KeyValue.set, compact)
            @assert isexpr(def, :invoke)
            if length(def.args) in (5, 6)
                collection = def.args[end-2]
                set_key = def.args[end-1]
                set_val_idx = length(def.args)
            elseif length(def.args) == 4
                collection = def.args[end-1]
                # Key is deleted
                # TODO: Model this
                return nothing
            elseif length(def.args) == 3
                collection = def.args[end]
                # The whole collection is deleted
                # TODO: Model this
                return nothing
            else
                return nothing
            end
            if set_key === key || (egal_tfunc(𝕃ₒ, argextype(key, compact), argextype(set_key, compact)) == Const(true))
                # This is an actual def
                return nothing
            end
            return Any[collection]
        end
        return phi_or_ifelse_predecessors(def, compact)
    end
end

struct KeyValueWalker <: WalkerCallback
    compact::IncrementalCompact
end
function (walker_callback::KeyValueWalker)(@nospecialize(def), @nospecialize(defssa::AnySSAValue))
    if is_known_invoke_or_call(def, Core.OptimizedGenerics.KeyValue.set, walker_callback.compact)
        @assert length(def.args) in (5, 6)
        return LiftedValue(def.args[end-2])
    end
    return nothing
end

function lift_keyvalue_get!(compact::IncrementalCompact, idx::Int, stmt::Expr, 𝕃ₒ::AbstractLattice)
    collection = stmt.args[end-1]
    key = stmt.args[end]

    leaves, visited_philikes = collect_leaves(compact, collection, Any, 𝕃ₒ, keyvalue_predecessors(key, 𝕃ₒ))
    isempty(leaves) && return

    lifted_leaves = lift_leaves_keyvalue(compact, key, leaves, 𝕃ₒ)
    lifted_leaves === nothing && return

    result_t = Union{}
    for v in values(lifted_leaves)
        v === nothing && return
        result_t = tmerge(𝕃ₒ, result_t, argextype(v.val, compact))
    end

    (lifted_val, nest) = perform_lifting!(compact,
        visited_philikes, key, result_t, lifted_leaves, collection, nothing,
        KeyValueWalker(compact))

    compact[idx] = lifted_val === nothing ? nothing : Expr(:call, GlobalRef(Core, :tuple), lifted_val.val)
    finish_phi_nest!(compact, nest)
    if lifted_val !== nothing
        if !⊑(𝕃ₒ, compact[SSAValue(idx)][:type], tuple_tfunc(𝕃ₒ, Any[result_t]))
            add_flag!(compact[SSAValue(idx)], IR_FLAG_REFINED)
        end
    end

    return
end

# TODO: We could do the whole lifting machinery here, but really all
# we want to do is clean this up when it got inserted by inlining,
# which always targets simple `svec` call or `_compute_sparams`,
# so this specialized lifting would be enough
@inline function _lift_svec_ref(def::Expr, compact::IncrementalCompact)
    length(def.args) >= 3 || return nothing
    m = argextype(def.args[2], compact)
    isa(m, Const) || return nothing
    m = m.val
    isa(m, Method) || return nothing

    # TODO: More general structural analysis of the intersection
    sig = m.sig
    isa(sig, UnionAll) || return nothing
    tvar = sig.var
    sig = sig.body
    isa(sig, DataType) || return nothing
    sig.name === Tuple.name || return nothing
    sig_parameters = sig.parameters::SimpleVector
    length_sig_parameters = length(sig_parameters)
    length_sig_parameters >= 1 || return nothing

    function has_typevar_closure(j::Int)
        has_typevar(sig_parameters[j], tvar)
    end

    i = findfirst(has_typevar_closure, 1:length_sig_parameters)
    i === nothing && return nothing
    any(has_typevar_closure, i+1:length_sig_parameters) && return nothing

    arg = sig_parameters[i]

    rarg = def.args[2 + i]
    isa(rarg, SSAValue) || return nothing
    argdef = compact[rarg][:stmt]
    if isexpr(argdef, :new)
        rarg = argdef.args[1]
        isa(rarg, SSAValue) || return nothing
        argdef = compact[rarg][:stmt]
    else
        isType(arg) || return nothing
        arg = arg.parameters[1]
    end

    is_known_call(argdef, Core.apply_type, compact) || return nothing
    length(argdef.args) == 3 || return nothing

    applyT = argextype(argdef.args[2], compact)
    isa(applyT, Const) || return nothing
    applyT = applyT.val

    isa(applyT, UnionAll) || return nothing
    # N.B.: At the moment we only lift the valI == 1 case, so we
    # only need to look at the outermost tvar.
    applyTvar = applyT.var
    applyTbody = applyT.body

    arg = unwrap_unionall(arg)
    applyTbody = unwrap_unionall(applyTbody)

    (isa(arg, DataType) && isa(applyTbody, DataType)) || return nothing
    applyTbody.name === arg.name || return nothing
    length(applyTbody.parameters) == length(arg.parameters) || return nothing
    for i = 1:length(applyTbody.parameters)
        if applyTbody.parameters[i] === applyTvar && arg.parameters[i] === tvar
            return LiftedValue(argdef.args[3])
        end
    end
    return nothing
end

struct IsEgal <: Function
    x::Any
    IsEgal(@nospecialize(x)) = new(x)
end
(x::IsEgal)(@nospecialize(y)) = x.x === y

# This tries to match patterns of the form
#  %ft   = typeof(%farg)
#  %Targ = apply_type(Foo, ft)
#  %x    = new(%Targ, %farg)
#
# and if possible refines the nothrowness of the new expr based on it.
function pattern_match_typeof(compact::IncrementalCompact, typ::DataType, fidx::Int,
                              @nospecialize(Targ), @nospecialize(farg))
    isa(Targ, SSAValue) || return false

    Tdef = compact[Targ][:stmt]
    is_known_call(Tdef, Core.apply_type, compact) || return false
    length(Tdef.args) ≥ 2 || return false

    applyT = argextype(Tdef.args[2], compact)
    isa(applyT, Const) || return false

    applyT = applyT.val
    tvars = Any[]
    while isa(applyT, UnionAll)
        applyTvar = applyT.var
        applyT = applyT.body
        push!(tvars, applyTvar)
    end

    @assert applyT.name === typ.name
    fT = fieldtype(applyT, fidx)
    idx = findfirst(IsEgal(fT), tvars)
    idx === nothing && return false
    checkbounds(Bool, Tdef.args, 2+idx) || return false
    valarg = Tdef.args[2+idx]
    isa(valarg, SSAValue) || return false
    valdef = compact[valarg][:stmt]
    is_known_call(valdef, typeof, compact) || return false

    return valdef.args[2] === farg
end

function refine_new_effects!(𝕃ₒ::AbstractLattice, compact::IncrementalCompact, idx::Int, stmt::Expr)
    inst = compact[SSAValue(idx)]
    if has_flag(inst, IR_FLAGS_REMOVABLE)
        return # already accurate
    end
    (consistent, removable, nothrow) = new_expr_effect_flags(𝕃ₒ, stmt.args, compact, pattern_match_typeof)
    if consistent
        add_flag!(inst, IR_FLAG_CONSISTENT)
    end
    if removable
        add_flag!(inst, IR_FLAGS_REMOVABLE)
    elseif nothrow
        add_flag!(inst, IR_FLAG_NOTHROW)
    end
    return nothing
end

function fold_ifelse!(compact::IncrementalCompact, idx::Int, stmt::Expr, 𝕃ₒ::AbstractLattice)
    length(stmt.args) == 4 || return false
    condarg = stmt.args[2]
    condtyp = argextype(condarg, compact)
    if isa(condtyp, Const)
        if condtyp.val === true
            compact[idx] = stmt.args[3]
            return true
        elseif condtyp.val === false
            compact[idx] = stmt.args[4]
            return true
        end
    elseif ⊑(𝕃ₒ, condtyp, Bool) && stmt.args[3] === stmt.args[4]
        compact[idx] = stmt.args[3]
        return true
    end
    return false
end

# NOTE we use `IdSet{Int}` instead of `BitSet` for in these passes since they work on IR after inlining,
# which can be very large sometimes, and program counters in question are often very sparse
const SPCSet = IdSet{Int}

struct IntermediaryCollector <: WalkerCallback
    intermediaries::SPCSet
end
function (walker_callback::IntermediaryCollector)(@nospecialize(def), @nospecialize(defssa::AnySSAValue))
    if !(def isa Expr)
        push!(walker_callback.intermediaries, defssa.id)
        if def isa PiNode
            return LiftedValue(def.val)
        end
    end
    return nothing
end

function update_scope_mapping!(scope_mapping, bb, val)
    current_mapping = scope_mapping[bb]
    if current_mapping != SSAValue(0)
        if val == SSAValue(0)
            # Unreachable bbs will have SSAValue(0), but can branch into
            # try/catch regions. We could validate with the domtree, but that's
            # quite expensive for a debug check, so simply allow this without
            # making any changes to mapping.
            return
        end
        @assert current_mapping == val
        return
    end
    scope_mapping[bb] = val
end

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
function sroa_pass!(ir::IRCode, inlining::Union{Nothing,InliningState}=nothing)
    𝕃ₒ = inlining === nothing ? SimpleInferenceLattice.instance : optimizer_lattice(inlining.interp)
    compact = IncrementalCompact(ir)
    defuses = nothing # will be initialized once we encounter mutability in order to reduce dynamic allocations
    # initialization of domtree is delayed to avoid the expensive computation in many cases
    lazydomtree = LazyDomtree(ir)
    scope_mapping::Union{Vector{SSAValue}, Nothing} = nothing
    for ((old_idx, idx), stmt) in compact
        # If we encounter any EnterNode with set :scope, propagate the current scope for all basic blocks, so
        # we have easy access for current_scope folding below.
        if !isa(stmt, Expr)
            bb = compact.active_result_bb - 1
            if scope_mapping !== nothing && did_just_finish_bb(compact)
                this_scope = scope_mapping[bb]
                if isa(stmt, GotoIfNot)
                    update_scope_mapping!(scope_mapping, stmt.dest, this_scope)
                    update_scope_mapping!(scope_mapping, bb+1, this_scope)
                elseif isa(stmt, GotoNode)
                    update_scope_mapping!(scope_mapping, stmt.label, this_scope)
                elseif isa(stmt, EnterNode)
                    if stmt.catch_dest != 0
                        update_scope_mapping!(scope_mapping, stmt.catch_dest, this_scope)
                    end
                    isdefined(stmt, :scope) || update_scope_mapping!(scope_mapping, bb+1, this_scope)
                elseif !isa(stmt, ReturnNode)
                    update_scope_mapping!(scope_mapping, bb+1, this_scope)
                end
            end
            if isa(stmt, EnterNode)
                if isdefined(stmt, :scope)
                    if scope_mapping === nothing
                        scope_mapping = SSAValue[SSAValue(0) for i = 1:length(compact.cfg_transform.result_bbs)]
                    end
                    update_scope_mapping!(scope_mapping, bb+1, SSAValue(idx))
                end
            end
            continue
        end
        if scope_mapping !== nothing && did_just_finish_bb(compact)
            bb = compact.active_result_bb - 1
            bbs = scope_mapping[bb]
            if isexpr(stmt, :leave) && bbs != SSAValue(0)
                # Here we want to count the number of scopes that we're leaving,
                # which is the same as the number of EnterNodes being referenced
                # by `stmt.args`. Which have :scope set. In practice, the frontend
                # does emit these in order, so we could simply go to the last one,
                # but we want to avoid making that semantic assumption.
                for i = 1:length(stmt.args)
                    scope = stmt.args[i]
                    scope === nothing && continue
                    enter = compact[scope][:inst]
                    @assert isa(enter, EnterNode)
                    isdefined(enter, :scope) || continue
                    bbs = scope_mapping[block_for_inst(compact, bbs)]
                end
            end
            update_scope_mapping!(scope_mapping, bb+1, bbs)
        end
        # check whether this statement is `getfield` / `setfield!` (or other "interesting" statement)
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
            info = compact[SSAValue(idx)][:info]
            if isa(info, FinalizerInfo)
                is_finalizer_inlineable(info.effects) || continue
            else
                # Inlining performs legality checks on the finalizer to determine
                # whether or not we may inline it. If so, it appends extra arguments
                # at the end of the intrinsic. Detect that here.
                if length(stmt.args) == 4 && stmt.args[4] === nothing
                    # constant case
                elseif length(stmt.args) == 5 && stmt.args[4] isa Bool && stmt.args[5] isa Core.CodeInstance
                    # inlining case
                else
                    continue
                end
            end
            is_finalizer = true
        elseif isexpr(stmt, :foreigncall)
            nccallargs = length(stmt.args[3]::SimpleVector)
            preserved = Int[]
            new_preserves = Any[]
            for pidx in (6+nccallargs):length(stmt.args)
                preserved_arg = stmt.args[pidx]
                isa(preserved_arg, SSAValue) || continue
                let intermediaries = SPCSet()
                    def = simple_walk(compact, preserved_arg, IntermediaryCollector(intermediaries))
                    isa(def, SSAValue) || continue
                    defidx = def.id
                    def = compact[def][:stmt]
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
            elseif is_known_call(stmt, Core._svec_ref, compact)
                lift_svec_ref!(compact, idx, stmt)
            elseif is_known_call(stmt, (===), compact)
                lift_comparison!(===, compact, idx, stmt, 𝕃ₒ)
            elseif is_known_call(stmt, isa, compact)
                lift_comparison!(isa, compact, idx, stmt, 𝕃ₒ)
            elseif is_known_call(stmt, Core.ifelse, compact)
                fold_ifelse!(compact, idx, stmt, 𝕃ₒ)
            elseif is_known_invoke_or_call(stmt, Core.OptimizedGenerics.KeyValue.get, compact)
                2 == (length(stmt.args) - (isexpr(stmt, :invoke) ? 2 : 1)) || continue
                lift_keyvalue_get!(compact, idx, stmt, 𝕃ₒ)
            elseif is_known_call(stmt, Core.current_scope, compact)
                length(stmt.args) == 1 || continue
                scope_mapping !== nothing || continue
                bb = compact.active_result_bb
                did_just_finish_bb(compact) && (bb -= 1)
                enter_ssa = scope_mapping[bb]
                enter_ssa == SSAValue(0) && continue
                compact[SSAValue(idx)] = (compact[enter_ssa][:stmt]::EnterNode).scope
            elseif isexpr(stmt, :new)
                refine_new_effects!(𝕃ₒ, compact, idx, stmt)
            elseif is_known_call(stmt, Core._apply_iterate, compact)
                length(stmt.args) >= 4 || continue
                lift_apply_args!(compact, idx, stmt)
            end
            continue
        end

        if is_finalizer
            val = stmt.args[3]
        else
            # analyze `getfield` / `isdefined` / `setfield!` call
            val = stmt.args[2]
        end
        struct_typ = widenconst(argextype(val, compact))
        struct_argtyp = argument_datatype(struct_typ)
        if struct_argtyp === nothing
            if isa(struct_typ, Union) && is_isdefined
                lift_comparison!(isdefined, compact, idx, stmt, 𝕃ₒ)
            end
            continue
        end
        struct_typ_name = struct_argtyp.name

        struct_typ_name.atomicfields == C_NULL || continue # TODO: handle more
        if !((field_ordering === :unspecified) ||
             (field_ordering isa Const && field_ordering.val === :not_atomic))
            continue
        end

        # analyze this mutable struct here for the later pass
        if ismutabletypename(struct_typ_name)
            isa(val, SSAValue) || continue
            let intermediaries = SPCSet()
                def = simple_walk(compact, val, IntermediaryCollector(intermediaries))
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
                    push!(defuse.uses, FinalizerUse(idx))
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
        field = try_compute_fieldidx_stmt(compact, stmt, struct_typ)
        field === nothing && continue

        leaves, visited_philikes = collect_leaves(compact, val, struct_typ, 𝕃ₒ, phi_or_ifelse_predecessors)
        isempty(leaves) && continue

        lifted_result = lift_leaves(compact, field, leaves, 𝕃ₒ)
        lifted_result === nothing && continue
        lifted_leaves, any_undef = lifted_result

        result_t = Union{}
        for v in values(lifted_leaves)
            v === nothing && continue
            result_t = tmerge(𝕃ₒ, result_t, argextype(v.val, compact))
        end

        (lifted_val, nest) = perform_lifting!(compact,
            visited_philikes, field, result_t, lifted_leaves, val, lazydomtree)

        should_delete_node = false
        line = compact[SSAValue(idx)][:line]
        if lifted_val !== nothing && !⊑(𝕃ₒ, compact[SSAValue(idx)][:type], result_t)
            compact[idx] = lifted_val === nothing ? nothing : lifted_val.val
            add_flag!(compact[SSAValue(idx)], IR_FLAG_REFINED)
        elseif lifted_val === nothing || isa(lifted_val.val, AnySSAValue)
            # Save some work in a later compaction, by inserting this into the renamer now,
            # but only do this if we didn't set the REFINED flag, to save work for irinterp
            # in revisiting only the renamings that came through *this* idx.
            compact.ssa_rename[old_idx] = lifted_val === nothing ? nothing : lifted_val.val
            should_delete_node = true
        else
            compact[idx] = lifted_val === nothing ? nothing : lifted_val.val
        end

        finish_phi_nest!(compact, nest)

        # Insert the undef check if necessary
        if any_undef
            if lifted_val === nothing
                def_val = false
            else
                lifted_leaves_def = LiftedDefs()
                for (k, v) in pairs(lifted_leaves)
                    lifted_leaves_def[k] = v === nothing ? false : true
                end
                (def_val, nest) = perform_lifting!(compact,
                    visited_philikes, field, Bool, lifted_leaves_def, val, lazydomtree)
                def_val = (def_val::LiftedValue).val
                finish_phi_nest!(compact, nest)
            end
            throw_expr = Expr(:throw_undef_if_not, Symbol("##getfield##"), def_val)
            if should_delete_node
                # Replace the node we already have rather than deleting/re-inserting.
                # This way it is easier to handle BB boundary corner cases.
                compact[SSAValue(idx)] = throw_expr
                compact[SSAValue(idx)][:type] = Nothing
                compact[SSAValue(idx)][:flag] = IR_FLAG_EFFECT_FREE | IR_FLAG_CONSISTENT | IR_FLAG_NOUB
                should_delete_node = false
            else
                ni = NewInstruction(throw_expr, Nothing, line)
                insert_node!(compact, SSAValue(idx), ni)
            end
        else
            # val must be defined
            @assert lifted_val !== nothing
        end

        should_delete_node && delete_inst_here!(compact)
    end

    non_dce_finish!(compact)
    if defuses !== nothing
        # now go through analyzed mutable structs and see which ones we can eliminate
        # NOTE copy the use count here, because `simple_dce!` may modify it and we need it
        # consistent with the state of the IR here (after tracking `PhiNode` arguments,
        # but before the DCE) for our predicate within `sroa_mutables!`, but we also
        # try an extra effort using a callback so that reference counts are updated
        used_ssas = copy(compact.used_ssas)
        simple_dce!(compact) do x::SSAValue
            used_ssas[x.id] -= 1
        end
        ir = complete(compact)
        # remove any use that has been optimized away by the DCE
        for (intermediaries, defuse) in values(defuses)
            filter!(x -> ir[SSAValue(x.idx)][:stmt] !== nothing, defuse.uses)
        end
        sroa_mutables!(ir, defuses, used_ssas, lazydomtree, inlining)
        return ir
    else
        simple_dce!(compact)
        return complete(compact)
    end
end

# NOTE we resolve the inlining source here as we don't want to serialize `Core.Compiler`
# data structure into the global cache (see the comment in `handle_finalizer_call!`)
function try_inline_finalizer!(ir::IRCode, argexprs::Vector{Any}, idx::Int,
    code::CodeInstance, @nospecialize(info::CallInfo), inlining::InliningState,
    attach_after::Bool)
    mi = get_ci_mi(code)
    et = InliningEdgeTracker(inlining)
    if code isa CodeInstance
        if use_const_api(code)
            # No code in the function - Nothing to do
            add_inlining_edge!(et, code)
            return true
        end
        src = ci_get_source(inlining.interp, code)
    else
        return false
    end

    src_inlining_policy(inlining.interp, mi, src, info, IR_FLAG_NULL) || return false
    src, spec_info, di = retrieve_ir_for_inlining(code, src)

    # For now: Require finalizer to only have one basic block
    length(src.cfg.blocks) == 1 || return false

    # Ok, we're committed to inlining the finalizer
    add_inlining_edge!(et, code)

    # TODO: Should there be a special line number node for inlined finalizers?
    inline_at = ir[SSAValue(idx)][:line]
    ssa_substitute = ir_prepare_inlining!(InsertBefore(ir, SSAValue(idx)), ir, src, spec_info, di, mi, inline_at, argexprs)

    # TODO: Use the actual inliner here rather than open coding this special purpose inliner.
    ssa_rename = Vector{Any}(undef, length(src.stmts))
    for idx′ = 1:length(src.stmts)
        inst = src[SSAValue(idx′)]
        stmt′ = inst[:stmt]
        isa(stmt′, ReturnNode) && continue
        stmt′ = ssamap(stmt′) do ssa::SSAValue
            ssa_rename[ssa.id]
        end
        stmt′ = ssa_substitute_op!(InsertBefore(ir, SSAValue(idx)), inst, stmt′, ssa_substitute)
        ssa_rename[idx′] = insert_node!(ir, idx,
            NewInstruction(inst; stmt=stmt′, line=(ssa_substitute.inlined_at[1], ssa_substitute.inlined_at[2], Int32(idx′))),
            attach_after)
    end

    return true
end

is_nothrow(ir::IRCode, ssa::SSAValue) = has_flag(ir[ssa], IR_FLAG_NOTHROW)

function reachable_blocks(cfg::CFG, from_bb::Int, to_bb::Int)
    worklist = Int[from_bb]
    visited = BitSet(from_bb)
    if to_bb == from_bb
        return visited
    else
        push!(visited, to_bb)
    end
    function visit!(bb::Int)
        if bb ∉ visited
            push!(visited, bb)
            push!(worklist, bb)
        end
    end
    while !isempty(worklist)
        foreach(visit!, cfg.blocks[pop!(worklist)].succs)
    end
    return visited
end

function try_resolve_finalizer!(ir::IRCode, alloc_idx::Int, finalizer_idx::Int, defuse::SSADefUse,
        inlining::InliningState, lazydomtree::LazyDomtree,
        lazypostdomtree::LazyPostDomtree, @nospecialize(info::CallInfo))
    # For now, require that:
    # 1. The allocation dominates the finalizer registration
    # 2. The insertion block for the finalizer is the post-dominator of all
    #    uses (including the finalizer registration).
    # 3. The path from the finalizer registration to the finalizer inlining
    #    location is nothrow
    #
    # TODO: We could relax the check 2, by inlining the finalizer multiple times.

    # Check #1: The allocation dominates the finalizer registration
    domtree = get!(lazydomtree)
    finalizer_bb = block_for_inst(ir, finalizer_idx)
    alloc_bb = block_for_inst(ir, alloc_idx)
    dominates(domtree, alloc_bb, finalizer_bb) || return nothing

    # Check #2: The insertion block for the finalizer is the post-dominator of all uses
    insert_bb::Int = finalizer_bb
    insert_idx::Union{Int,Nothing} = finalizer_idx
    function note_defuse!(x::Union{Int,SSAUse})
        defuse_idx = x isa SSAUse ? x.idx : x
        defuse_idx == finalizer_idx && return nothing
        defuse_bb = block_for_inst(ir, defuse_idx)
        new_insert_bb = nearest_common_dominator(get!(lazypostdomtree),
            insert_bb, defuse_bb)
        if new_insert_bb == insert_bb && insert_idx !== nothing
            insert_idx = max(insert_idx::Int, defuse_idx)
        elseif new_insert_bb == defuse_bb
            insert_idx = defuse_idx
        else
            insert_idx = nothing
        end
        insert_bb = new_insert_bb
        nothing
    end
    foreach(note_defuse!, defuse.uses)
    foreach(note_defuse!, defuse.defs)
    insert_bb != 0 || return nothing # verify post-dominator of all uses exists

    if !OptimizationParams(inlining.interp).assume_fatal_throw
        # Collect all reachable blocks between the finalizer registration and the
        # insertion point
        blocks = reachable_blocks(ir.cfg, finalizer_bb, insert_bb)

        # Check #3
        function check_range_nothrow(s::Int, e::Int)
            return all(s:e) do sidx::Int
                sidx == finalizer_idx && return true
                sidx == alloc_idx && return true
                return is_nothrow(ir, SSAValue(sidx))
            end
        end
        for bb in blocks
            range = ir.cfg.blocks[bb].stmts
            s, e = first(range), last(range)
            if bb == insert_bb
                insert_idx === nothing && continue
                e = insert_idx
            end
            if bb == finalizer_bb
                s = finalizer_idx
            end
            check_range_nothrow(s, e) || return nothing
        end
    end

    # Ok, legality check complete. Figure out the exact statement where we're
    # going to inline the finalizer.
    loc = insert_idx === nothing ? first(ir.cfg.blocks[insert_bb].stmts) : insert_idx::Int
    attach_after = insert_idx !== nothing

    finalizer_stmt = ir[SSAValue(finalizer_idx)][:stmt]
    argexprs = Any[finalizer_stmt.args[2], finalizer_stmt.args[3]]
    flag = info isa FinalizerInfo ? flags_for_effects(info.effects) : IR_FLAG_NULL
    if length(finalizer_stmt.args) >= 4
        inline = finalizer_stmt.args[4]
        if inline === nothing
            # No code in the function - Nothing to do
        else
            ci = finalizer_stmt.args[5]::CodeInstance
            if inline::Bool && try_inline_finalizer!(ir, argexprs, loc, ci, info, inlining, attach_after)
                # the finalizer body has been inlined
            else
                newinst = add_flag(NewInstruction(Expr(:invoke, ci, argexprs...), Nothing), flag)
                insert_node!(ir, loc, newinst, attach_after)
            end
        end
    else
        newinst = add_flag(NewInstruction(Expr(:call, argexprs...), Nothing), flag)
        insert_node!(ir, loc, newinst, attach_after)
    end
    # Erase the call to `finalizer`
    ir[SSAValue(finalizer_idx)][:stmt] = nothing
    return nothing
end

function sroa_mutables!(ir::IRCode, defuses::IdDict{Int,Tuple{SPCSet,SSADefUse}}, used_ssas::Vector{Int}, lazydomtree::LazyDomtree, inlining::Union{Nothing,InliningState})
    𝕃ₒ = inlining === nothing ? SimpleInferenceLattice.instance : optimizer_lattice(inlining.interp)
    lazypostdomtree = LazyPostDomtree(ir)
    function find_finalizer_useidx(defuse::SSADefUse)
        finalizer_useidx = nothing
        for (useidx, use) in enumerate(defuse.uses)
            if use.kind === :finalizer
                # For now: Only allow one finalizer per allocation
                finalizer_useidx !== nothing && return false
                finalizer_useidx = useidx
            end
        end
        if finalizer_useidx === nothing || inlining === nothing
            return true
        end
        return finalizer_useidx
    end
    for (defidx, (intermediaries, defuse)) in defuses
        # Find the type for this allocation
        defexpr = ir[SSAValue(defidx)][:stmt]
        isexpr(defexpr, :new) || continue
        typ = unwrap_unionall(ir.stmts[defidx][:type])
        # Could still end up here if we tried to setfield! on an immutable, which would
        # error at runtime, but is not illegal to have in the IR.
        typ = widenconst(typ)
        ismutabletype(typ) || continue
        typ = typ::DataType
        # Check if there are any uses we did not account for. If so, the variable
        # escapes and we cannot eliminate the allocation. This works, because we're guaranteed
        # not to include any intermediaries that have dead uses. As a result, missing uses will only ever
        # show up in the nuses_total count.
        nleaves = length(defuse.uses) + length(defuse.defs)
        nuses = 0
        for iidx in intermediaries
            nuses += used_ssas[iidx]
        end
        nuses_total = used_ssas[defidx] + nuses - length(intermediaries)
        all_eliminated = all_forwarded = true
        if nleaves ≠ nuses_total
            finalizer_useidx = find_finalizer_useidx(defuse)
            if finalizer_useidx isa Int
                nargs = length(ir.argtypes) # COMBAK this might need to be `Int(opt.src.nargs)`
                estate = EscapeAnalysis.analyze_escapes(ir, nargs, 𝕃ₒ, get_escape_cache(inlining.interp))
                # disable finalizer inlining when this allocation is aliased to somewhere,
                # mostly likely to edges of `PhiNode`
                hasaliases = EscapeAnalysis.getaliases(SSAValue(defidx), estate) !== nothing
                einfo = estate[SSAValue(defidx)]
                if !hasaliases && EscapeAnalysis.has_no_escape(einfo)
                    already = BitSet(use.idx for use in defuse.uses)
                    for idx = einfo.Liveness
                        if idx ∉ already
                            push!(defuse.uses, SSAUse(:EALiveness, idx))
                        end
                    end
                    finalizer_idx = defuse.uses[finalizer_useidx].idx
                    try_resolve_finalizer!(ir, defidx, finalizer_idx, defuse, inlining::InliningState,
                        lazydomtree, lazypostdomtree, ir[SSAValue(finalizer_idx)][:info])
                end
            end
            continue
        else
            finalizer_useidx = find_finalizer_useidx(defuse)
            if finalizer_useidx isa Int
                finalizer_idx = defuse.uses[finalizer_useidx].idx
                try_resolve_finalizer!(ir, defidx, finalizer_idx, defuse, inlining::InliningState,
                    lazydomtree, lazypostdomtree, ir[SSAValue(finalizer_idx)][:info])
                deleteat!(defuse.uses, finalizer_useidx)
                all_eliminated = all_forwarded = false # can't eliminate `setfield!` calls safely
            elseif !finalizer_useidx
                continue
            end
        end
        # Partition defuses by field
        fielddefuse = SSADefUse[SSADefUse() for _ = 1:fieldcount(typ)]
        for use in defuse.uses
            if use.kind === :preserve
                for du in fielddefuse
                    push!(du.uses, use)
                end
                continue
            end
            stmt = ir[SSAValue(use.idx)][:stmt] # == `getfield`/`isdefined` call
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
            stmt = ir[SSAValue(def)][:stmt]::Expr # == `setfield!` call
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
        blocks = Vector{Tuple{#=phiblocks=#Vector{Int},#=allblocks=#BitSet}}(undef, ndefuse)
        for fidx in 1:ndefuse
            du = fielddefuse[fidx]
            isempty(du.uses) && continue
            push!(du.defs, defidx)
            ldu = compute_live_ins(ir.cfg, du)
            if isempty(ldu.live_in_bbs)
                phiblocks = Int[]
            else
                phiblocks = iterated_dominance_frontier(ir.cfg, ldu, get!(lazydomtree))
            end
            allblocks = union!(BitSet(phiblocks), ldu.def_bbs)
            blocks[fidx] = phiblocks, allblocks
            if fidx + 1 > length(defexpr.args)
                for i = 1:length(du.uses)
                    use = du.uses[i]
                    if use.kind === :isdefined
                        if has_safe_def(ir, get!(lazydomtree), allblocks, du, defidx, use.idx)
                            ir[SSAValue(use.idx)][:stmt] = true
                        else
                            all_eliminated = false
                        end
                        continue
                    elseif use.kind === :preserve
                        if length(du.defs) == 1 # allocation with this field uninitialized
                            # there is nothing to preserve, just ignore this use
                            du.uses[i] = NoPreserve()
                            continue
                        end
                    end
                    has_safe_def(ir, get!(lazydomtree), allblocks, du, defidx, use.idx) || @goto skip
                end
            else # always have some definition at the allocation site
                for i = 1:length(du.uses)
                    use = du.uses[i]
                    if use.kind === :isdefined
                        ir[SSAValue(use.idx)][:stmt] = true
                    end
                end
            end
        end
        # Everything accounted for. Go field by field and perform idf:
        # Compute domtree now, needed below, now that we have finished compacting the IR.
        # This needs to be after we iterate through the IR with `IncrementalCompact`
        # because removing dead blocks can invalidate the domtree.
        domtree = get!(lazydomtree)
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
                        inst = ir[SSAValue(use.idx)]
                        inst[:stmt] = compute_value_for_use(ir, domtree, allblocks,
                            du, phinodes, fidx, use.idx)
                        add_flag!(inst, IR_FLAG_REFINED)
                    elseif use.kind === :isdefined
                        continue # already rewritten if possible
                    elseif use.kind === :nopreserve
                        continue # nothing to preserve (may happen when there are uninitialized fields)
                    elseif use.kind === :preserve
                        newval = compute_value_for_use(ir, domtree, allblocks,
                            du, phinodes, fidx, use.idx)
                        if !isbitstype(widenconst(argextype(newval, ir)))
                            if preserve_uses === nothing
                                preserve_uses = IdDict{Int, Vector{Any}}()
                            end
                            push!(get!(Vector{Any}, preserve_uses, use.idx), newval)
                        end
                    else
                        @assert false "sroa_mutables!: unexpected use"
                    end
                end
                for b in phiblocks
                    n = ir[phinodes[b]][:stmt]::PhiNode
                    result_t = Bottom
                    for p in ir.cfg.blocks[b].preds
                        push!(n.edges, p)
                        v = compute_value_for_block(ir, domtree, allblocks, du, phinodes, fidx, p)
                        push!(n.values, v)
                        result_t = tmerge(𝕃ₒ, result_t, argextype(v, ir))
                    end
                    ir[phinodes[b]][:type] = result_t
                end
            end
            all_eliminated || continue
            # all "usages" (i.e. `getfield` and `isdefined` calls) are eliminated,
            # now eliminate "definitions" (i.e. `setfield!`) calls
            # (NOTE the allocation itself will be eliminated by DCE pass later)
            for didx in du.defs
                didx == defidx && continue # this is allocation
                # verify this statement won't throw, otherwise it can't be eliminated safely
                setfield_ssa = SSAValue(didx)
                if is_nothrow(ir, setfield_ssa)
                    ir[setfield_ssa][:stmt] = nothing
                else
                    # We can't eliminate this statement, because it might still
                    # throw an error, but we can mark it as effect-free since we
                    # know we have removed all uses of the mutable allocation.
                    # As a result, if we ever do prove nothrow, we can delete
                    # this statement then.
                    add_flag!(ir[setfield_ssa], IR_FLAG_EFFECT_FREE)
                end
            end
        end
        preserve_uses === nothing && continue
        if all_forwarded
            # this means all ccall preserves have been replaced with forwarded loads
            # so we can potentially eliminate the allocation, otherwise we must preserve
            # the whole allocation.
            push!(intermediaries, defidx)
        end
        # Insert the new preserves
        for (useidx, new_preserves) in preserve_uses
            ir[SSAValue(useidx)][:stmt] = form_new_preserves(ir[SSAValue(useidx)][:stmt]::Expr,
                intermediaries, new_preserves)
        end

        @label skip
    end
end

function form_new_preserves(origex::Expr, intermediaries::Union{Vector{Int},SPCSet}, new_preserves::Vector{Any})
    newex = Expr(:foreigncall)
    nccallargs = length(origex.args[3]::SimpleVector)
    for i in 1:(6+nccallargs-1)
        push!(newex.args, origex.args[i])
    end
    for i in (6+nccallargs):length(origex.args)
        x = origex.args[i]
        # don't need to preserve intermediaries
        if isa(x, SSAValue) && x.id in intermediaries
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
            compact.result[idx][:line]), #=reverse_affinity=#true)
    compact.ssa_rename[compact.idx-1] = pi
end

function adce_erase!(phi_uses::Vector{Int}, extra_worklist::Vector{Int}, compact::IncrementalCompact, idx::Int, in_worklist::Bool)
    # return whether this made a change
    if isa(compact.result[idx][:stmt], PhiNode)
        return maybe_erase_unused!(compact, idx, in_worklist, extra_worklist) do val::SSAValue
            phi_uses[val.id] -= 1
        end
    else
        return maybe_erase_unused!(compact, idx, in_worklist, extra_worklist)
    end
end

function mark_phi_cycles!(compact::IncrementalCompact, safe_phis::SPCSet, phi::Int)
    worklist = Int[]
    push!(worklist, phi)
    while !isempty(worklist)
        phi = pop!(worklist)
        push!(safe_phis, phi)
        for ur in userefs(compact.result[phi][:stmt])
            val = ur[]
            isa(val, SSAValue) || continue
            isa(compact[val][:stmt], PhiNode) || continue
            (val.id in safe_phis) && continue
            push!(worklist, val.id)
        end
    end
end

function is_some_union(@nospecialize(t))
    return isa(t, Union)
end

function is_union_phi(compact::IncrementalCompact, idx::Int)
    inst = compact.result[idx]
    isa(inst[:stmt], PhiNode) || return false
    return is_some_union(inst[:type])
end

function kill_phi!(compact::IncrementalCompact, phi_uses::Vector{Int},
                    to_drop::Union{Vector{Int}, UnitRange{Int}},
                    ssa::SSAValue, phi::PhiNode, delete_inst::Bool = false)
    for d in to_drop
        if isassigned(phi.values, d)
            val = phi.values[d]
            if !delete_inst
                # Deleting the inst will update compact's use count, so
                # don't do it here.
                kill_current_use!(compact, val)
            end
            if isa(val, SSAValue)
                phi_uses[val.id] -= 1
            end
        end
    end
    if delete_inst
        compact[ssa] = nothing
    elseif !isempty(to_drop)
        deleteat!(phi.values, to_drop)
        deleteat!(phi.edges, to_drop)
    end
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
function adce_pass!(ir::IRCode, inlining::Union{Nothing,InliningState}=nothing)
    𝕃ₒ = inlining === nothing ? SimpleInferenceLattice.instance : optimizer_lattice(inlining.interp)
    phi_uses = fill(0, length(ir.stmts) + length(ir.new_nodes))
    all_phis = Int[]
    unionphis = Pair{Int,Any}[] # sorted
    compact = IncrementalCompact(ir, true)
    made_changes = false
    for ((old_idx, idx), stmt) in compact
        if isa(stmt, PhiNode)
            if reprocess_phi_node!(𝕃ₒ, compact, stmt, old_idx)
                # Phi node has a single predecessor and was deleted
                made_changes = true
                continue
            end
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
                    t = tmerge(𝕃ₒ, unionphi[2], stmt.typ)
                    unionphis[first(r)] = Pair{Int,Any}(unionphi[1], t)
                end
            end
        else
            if is_known_call(stmt, typeassert, compact) && length(stmt.args) == 3
                # nullify safe `typeassert` calls
                ty, isexact = instanceof_tfunc(argextype(stmt.args[3], compact), true)
                if isexact && ⊑(𝕃ₒ, argextype(stmt.args[2], compact), ty)
                    delete_inst_here!(compact)
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
        for ur in userefs(inst[:stmt]::PhiNode)
            use = ur[]
            if isa(use, SSAValue)
                phi_uses[use.id] += 1
                stmt = compact.result[use.id][:stmt]
                if isa(stmt, PhiNode)
                    r = searchsorted(unionphis, use.id; by=first)
                    if !isempty(r)
                        unionphi = unionphis[first(r)]
                        unionphis[first(r)] = Pair{Int,Any}(unionphi[1],
                            tmerge(𝕃ₒ, unionphi[2], inst[:type]))
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
        inst = compact.result[phi]
        if t === Union{}
            stmt = inst[:stmt]::PhiNode
            kill_phi!(compact, phi_uses, 1:length(stmt.values), SSAValue(phi), stmt, true)
            made_changes = true
            continue
        elseif t === Any
            continue
        end
        ⊏ = strictpartialorder(𝕃ₒ)
        t ⊏ inst[:type] || continue
        to_drop = Int[]
        stmt = inst[:stmt]
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
        inst[:type] = t
        add_flag!(inst, IR_FLAG_REFINED) # t ⊏ inst[:type]
        kill_phi!(compact, phi_uses, to_drop, SSAValue(phi), stmt, false)
        made_changes = true
    end
    # Perform simple DCE for unused values
    extra_worklist = Int[]
    for (idx, nused) in Iterators.enumerate(compact.used_ssas)
        idx >= compact.result_idx && break
        nused == 0 || continue
        made_changes |= adce_erase!(phi_uses, extra_worklist, compact, idx, false)
    end
    while !isempty(extra_worklist)
        made_changes |= adce_erase!(phi_uses, extra_worklist, compact, pop!(extra_worklist), true)
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
                made_changes = true
            end
        end
    end

    return Pair{IRCode, Bool}(complete(compact), made_changes)
end

function is_bb_empty(ir::IRCode, bb::BasicBlock)
    isempty(bb.stmts) && return true
    if length(bb.stmts) == 1
        stmt = ir[SSAValue(first(bb.stmts))][:stmt]
        return stmt === nothing || isa(stmt, GotoNode)
    end
    return false
end

# TODO: This is terrible, we should change the IR for GotoIfNot to gain an else case
function is_legal_bb_drop(ir::IRCode, bbidx::Int, bb::BasicBlock)
    # For the time being, don't drop the first bb, because it has special predecessor semantics.
    bbidx == 1 && return false
    return true
end

function legalize_bb_drop_pred!(ir::IRCode, bb::BasicBlock, bbidx::Int, bbs::Vector{BasicBlock}, dropped_bbs::Vector{Int})
    (bbidx-1) in bb.preds || return true
    last_fallthrough = bbidx-1
    dbi = length(dropped_bbs)
    while dbi != 0 && dropped_bbs[dbi] == last_fallthrough && (last_fallthrough-1 in bbs[last_fallthrough].preds)
        last_fallthrough -= 1
        dbi -= 1
    end
    last_fallthrough_term_ssa = SSAValue(last(bbs[last_fallthrough].stmts))
    terminator = ir[last_fallthrough_term_ssa][:stmt]
    if isa(terminator, GotoIfNot)
        if terminator.dest != bbidx
            # The previous terminator's destination matches our fallthrough.
            # If we're also a fallthrough terminator, then we just have
            # to delete the GotoIfNot.
            our_terminator = ir[SSAValue(last(bb.stmts))][:stmt]
            if terminator.dest != (isa(our_terminator, GotoNode) ? our_terminator.label : bbidx + 1)
                return false
            end
        end
        ir[last_fallthrough_term_ssa] = nothing
        kill_edge!(bbs, last_fallthrough, terminator.dest)
    elseif isa(terminator, GotoNode)
        return true
    elseif isterminator(terminator)
        return false
    end
    # Hack, but effective. If we have a predecessor with a fall-through terminator, change the
    # instruction numbering to merge the blocks now such that below processing will properly
    # update it.
    bbs[last_fallthrough] = BasicBlock(first(bbs[last_fallthrough].stmts):last(bb.stmts), bbs[last_fallthrough].preds, bbs[last_fallthrough].succs)
    return true
end

function follow_map(map::Vector{Int}, idx::Int)
    while map[idx] ≠ 0
        idx = map[idx]
    end
    return idx
end

function ascend_eliminated_preds(bbs::Vector{BasicBlock}, pred::Int)
    pred == 0 && return pred
    while pred != 1 && length(bbs[pred].preds) == 1 && length(bbs[pred].succs) == 1
        pred = bbs[pred].preds[1]
    end
    return pred
end

# Compute (renamed) successors and predecessors given (renamed) block
function compute_succs(merged_succ::Vector{Int}, bbs::Vector{BasicBlock}, result_bbs::Vector{Int}, bb_rename_succ::Vector{Int}, i::Int)
    orig_bb = follow_map(merged_succ, result_bbs[i])
    return Int[bb_rename_succ[i] for i in bbs[orig_bb].succs]
end

function compute_preds(bbs::Vector{BasicBlock}, result_bbs::Vector{Int}, bb_rename_pred::Vector{Int}, i::Int)
    orig_bb = result_bbs[i]
    preds = copy(bbs[orig_bb].preds)
    res = Int[]
    while !isempty(preds)
        pred = popfirst!(preds)
        if pred == 0
            push!(res, 0)
            continue
        end
        r = bb_rename_pred[pred]
        (r == -2 || r == -1) && continue
        if r == -3
            prepend!(preds, bbs[pred].preds)
        else
            push!(res, r)
        end
    end
    return res
end

function add_preds!(all_new_preds::Vector{Int32}, bbs::Vector{BasicBlock}, bb_rename_pred::Vector{Int}, old_edge::Int32)
    preds = copy(bbs[old_edge].preds)
    while !isempty(preds)
        old_edge′ = popfirst!(preds)
        if old_edge′ == 0
            push!(all_new_preds, old_edge′)
            continue
        end
        new_edge = bb_rename_pred[old_edge′]
        if new_edge > 0 && new_edge ∉ all_new_preds
            push!(all_new_preds, Int32(new_edge))
        elseif new_edge == -3
            prepend!(preds, bbs[old_edge′].preds)
        end
    end
end

function cfg_simplify!(ir::IRCode)
    bbs = ir.cfg.blocks
    merge_into = zeros(Int, length(bbs))
    merged_succ = zeros(Int, length(bbs))
    dropped_bbs = Vector{Int}() # sorted

    # Walk the CFG from the entry block and aggressively combine blocks
    for (idx, bb) in enumerate(bbs)
        if length(bb.succs) == 1
            succ = bb.succs[1]
            if length(bbs[succ].preds) == 1 && succ != 1
                # Can't merge blocks with a non-GotoNode terminator, even if they
                # only have one successor, because it would not be legal to have that
                # terminator in the middle of a basic block.
                terminator = ir[SSAValue(last(bb.stmts))][:stmt]
                if !isa(terminator, GotoNode) && isterminator(terminator)
                    continue
                end
                # Prevent cycles by making sure we don't end up back at `idx`
                # by following what is to be merged into `succ`
                if follow_map(merged_succ, succ) != idx
                    merge_into[succ] = idx
                    merged_succ[idx] = succ
                end
            elseif merge_into[idx] == 0 && is_bb_empty(ir, bb) && is_legal_bb_drop(ir, idx, bb)
                # If this BB is empty, we can still merge it as long as none of our successor's phi nodes
                # reference our predecessors.
                #
                # This is for situations like:
                #   #1 - ...
                #        goto #3 if not ...
                #   #2 - (empty)
                #   #3 - ϕ(#2 => true, #1 => false)
                #
                # where we rely on the empty basic block to disambiguate the ϕ-node's value

                found_interference = false
                preds = Int[ascend_eliminated_preds(bbs, pred) for pred in bb.preds]
                for idx in bbs[succ].stmts
                    stmt = ir[SSAValue(idx)][:stmt]
                    stmt === nothing && continue
                    isa(stmt, PhiNode) || break
                    for edge in stmt.edges
                        edge = ascend_eliminated_preds(bbs, Int(edge))
                        for pred in preds
                            if pred == edge
                                found_interference = true
                                @goto done
                            end
                        end
                    end
                end
                @label done
                found_interference && continue
                legalize_bb_drop_pred!(ir, bb, idx, bbs, dropped_bbs) || continue
                push!(dropped_bbs, idx)
            end
        end
    end

    # Assign new BB numbers in DFS order, dropping unreachable blocks
    max_bb_num = 1
    bb_rename_succ = zeros(Int, length(bbs))
    worklist = BitSetBoundedMinPrioritySet(length(bbs))
    push!(worklist, 1)
    while !isempty(worklist)
        i = popfirst!(worklist)
        # Drop blocks that will be merged away
        if merge_into[i] != 0
            bb_rename_succ[i] = typemin(Int)
        end
        # Mark dropped blocks for fixup
        if !isempty(searchsorted(dropped_bbs, i))
            succ = bbs[i].succs[1]
            push!(worklist, succ)
            bb_rename_succ[i] = -succ
        end

        if bb_rename_succ[i] == 0
            curr = i
            while true
                @assert bb_rename_succ[curr] == 0
                bb_rename_succ[curr] = max_bb_num
                max_bb_num += 1
                # Now walk the chain of blocks we merged.
                # If we end in something that may fall through,
                # we have to schedule that block next
                while merged_succ[curr] != 0
                    if bb_rename_succ[curr] == 0
                        bb_rename_succ[curr] = typemin(Int)
                    end
                    curr = merged_succ[curr]
                end
                terminator = ir[SSAValue(bbs[curr].stmts[end])][:stmt]

                if isa(terminator, GotoIfNot)
                    if bb_rename_succ[terminator.dest] == 0
                        push!(worklist, terminator.dest)
                    end
                elseif isa(terminator, EnterNode)
                    catchbb = terminator.catch_dest
                    if catchbb ≠ 0
                        if bb_rename_succ[catchbb] == 0
                            push!(worklist, catchbb)
                        end
                    end
                elseif isa(terminator, GotoNode) || isa(terminator, ReturnNode)
                    # No implicit fall through. Schedule from work list.
                    break
                else
                    is_bottom = ir[SSAValue(bbs[curr].stmts[end])][:type] === Union{}
                    if is_bottom && !isa(terminator, PhiNode) && terminator !== nothing
                        # If this is a regular statement (not PhiNode/GotoNode/GotoIfNot
                        # or the `nothing` special case deletion marker),
                        # and the type is Union{}, then this may be a terminator.
                        # Ordinarily we normalize with ReturnNode(), but this is not
                        # required. In any case, we do not fall through, so we
                        # do not need to schedule the fall-through block.
                        break
                    end
                end
                ncurr = curr + 1
                while !isempty(searchsorted(dropped_bbs, ncurr))
                    bb_rename_succ[ncurr] = -bbs[ncurr].succs[1]
                    ncurr += 1
                end
                curr = ncurr
            end

            for succ in bbs[curr].succs
                if bb_rename_succ[succ] == 0
                    push!(worklist, succ)
                end
            end
        end
    end

    # Fixup dropped BBs
    resolved_all = false
    while !resolved_all
        # TODO: There are faster ways to do this
        resolved_all = true
        for bb in dropped_bbs
            obb = bb_rename_succ[bb]
            if obb < 0 && obb != typemin(Int)
                nsucc = bb_rename_succ[-obb]
                if nsucc == typemin(Int)
                    nsucc = -merge_into[-obb]
                end
                bb_rename_succ[bb] = nsucc
                resolved_all = false
            end
        end
    end

    # Drop remaining unvisited bbs
    bb_rename_pred = zeros(Int, length(bbs))
    for i = 1:length(bbs)
        if bb_rename_succ[i] == 0
            bb_rename_succ[i] = -2
            bb_rename_pred[i] = -2
        elseif bb_rename_succ[i] == typemin(Int)
            bb_rename_succ[i] = -2
        end
    end

    # Compute map from new to old blocks
    result_bbs = zeros(Int, max_bb_num-1)
    for (o, bb) in enumerate(bb_rename_succ)
        bb > 0 || continue
        isempty(searchsorted(dropped_bbs, o)) || continue
        result_bbs[bb] = o
    end

    # Figure out how predecessors should be renamed
    for i = 1:length(bbs)
        if merged_succ[i] != 0
            # Block `i` should no longer be a predecessor (before renaming)
            # because it is being merged with its sole successor
            bb_rename_pred[i] = -1
            continue
        end
        pred = i
        is_unreachable = false
        is_multi = false
        while pred !== 1 && !isempty(searchsorted(dropped_bbs, pred))
            preds = bbs[pred].preds
            if length(preds) == 0
                is_unreachable = true
                break
            elseif length(preds) > 1
                # This block has multiple predecessors - the only way this is
                # legal is if we proved above that our successors don't have
                # any phi nodes that would interfere with the renaming. Mark
                # this specially.
                is_multi = true
                break
            end
            @assert length(preds) == 1
            pred = preds[1]
        end
        if is_unreachable
            @assert bb_rename_pred[i] == -2
        elseif is_multi
            bb_rename_pred[i] = -3
        else
            bbnum = follow_map(merge_into, pred)
            bb_rename_pred[i] = bb_rename_succ[bbnum]
        end
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

    cresult_bbs = BasicBlock[
        BasicBlock(StmtRange(bb_starts[i],
                             i+1 > length(bb_starts) ? length(compact.result) : bb_starts[i+1]-1),
                   compute_preds(bbs, result_bbs, bb_rename_pred, i),
                   compute_succs(merged_succ, bbs, result_bbs, bb_rename_succ, i))
        for i = 1:length(result_bbs)]

    # Fixup terminators for any blocks that would have caused double edges
    for (bbidx, (new_bb, old_bb)) in enumerate(zip(cresult_bbs, result_bbs))
        @assert length(new_bb.succs) <= 2
        length(new_bb.succs) <= 1 && continue
        if new_bb.succs[1] == new_bb.succs[2]
            old_bb2 = findfirst(x::Int->x==bbidx, bb_rename_pred)::Int
            terminator = ir[SSAValue(last(bbs[old_bb2].stmts))]
            @assert terminator[:stmt] isa GotoIfNot
            # N.B.: The dest will be renamed in process_node! below
            terminator[:stmt] = GotoNode(terminator[:stmt].dest::Int)
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

    # Run instruction compaction to produce the result,
    # but we're messing with the CFG
    # so we don't want compaction to do so independently
    compact = IncrementalCompact(ir, CFGTransformState(true, false, cresult_bbs, bb_rename_pred, bb_rename_succ, nothing))
    result_idx = 1
    for (idx, orig_bb) in enumerate(result_bbs)
        ms = orig_bb
        bb_start = true
        while ms != 0
            old_bb_stmts = bbs[ms].stmts
            for i in old_bb_stmts
                node = ir.stmts[i]
                compact.result[compact.result_idx] = node
                stmt = node[:stmt]
                if isa(stmt, GotoNode) && merged_succ[ms] != 0
                    # If we merged a basic block, we need remove the trailing GotoNode (if any)
                    compact.result[compact.result_idx][:stmt] = nothing
                elseif isa(stmt, PhiNode)
                    phi = stmt
                    values = phi.values
                    (; ssa_rename, late_fixup, used_ssas, new_new_used_ssas) = compact
                    ssa_rename[i] = SSAValue(compact.result_idx)
                    already_inserted = function (branch::Int, val::OldSSAValue)
                        if val.id in old_bb_stmts
                            return val.id <= i
                        end
                        return 0 < bb_rename_pred[phi.edges[branch]] < idx
                    end
                    edges = Int32[]
                    values = Any[]
                    sizehint!(edges, length(phi.edges)); sizehint!(values, length(phi.values))
                    for old_index in 1:length(phi.edges)
                        old_edge = phi.edges[old_index]
                        new_edge = bb_rename_pred[old_edge]
                        if new_edge > 0
                            push!(edges, new_edge)
                            if isassigned(phi.values, old_index)
                                val = process_phinode_value(phi.values, old_index, late_fixup, already_inserted, compact.result_idx, ssa_rename, used_ssas, new_new_used_ssas, true, nothing)
                                push!(values, val)
                            else
                                resize!(values, length(values)+1)
                            end
                        elseif new_edge == -1
                            @assert length(phi.edges) == 1
                            if isassigned(phi.values, old_index)
                                val = process_phinode_value(phi.values, old_index, late_fixup, already_inserted, compact.result_idx, ssa_rename, used_ssas, new_new_used_ssas, true, nothing)
                                push!(edges, -1)
                                push!(values, val)
                            end
                        elseif new_edge == -3
                            # Multiple predecessors, we need to expand out this phi
                            all_new_preds = Int32[]
                            add_preds!(all_new_preds, bbs, bb_rename_pred, old_edge)
                            append!(edges, all_new_preds)
                            np = length(all_new_preds)
                            if np > 0
                                if isassigned(phi.values, old_index)
                                    val = process_phinode_value(phi.values, old_index, late_fixup, already_inserted, compact.result_idx, ssa_rename, used_ssas, new_new_used_ssas, true, nothing)
                                    for p in 1:np
                                        push!(values, val)
                                        p > 2 && count_added_node!(compact, val)
                                    end
                                else
                                    resize!(values, length(values)+np)
                                end
                            end
                        end
                    end
                    if length(edges) == 0 || (length(edges) == 1 && !isassigned(values, 1))
                        compact.result[compact.result_idx][:stmt] = nothing
                    elseif length(edges) == 1 && !bb_start
                        compact.result[compact.result_idx][:stmt] = values[1]
                    else
                        @assert bb_start
                        compact.result[compact.result_idx][:stmt] = PhiNode(edges, values)
                    end
                else
                    ri = process_node!(compact, compact.result_idx, node, i, i, ms, true)
                    if ri == compact.result_idx
                        # process_node! wanted this statement dropped. We don't do this,
                        # but we still need to erase the node
                        compact.result[compact.result_idx][:stmt] = nothing
                    end
                end
                # We always increase the result index to ensure a predicatable
                # placement of the resulting nodes.
                compact.result_idx += 1
            end
            ms = merged_succ[ms]
            bb_start = false
        end
    end
    compact.idx = length(ir.stmts)
    compact.active_result_bb = length(bb_starts)
    return finish(compact)
end
