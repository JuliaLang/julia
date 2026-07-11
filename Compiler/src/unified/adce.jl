# ADCE parity (§10.4; stock reference: Compiler/src/ssair/passes.jl
# `adce_pass!` — the CASES, not the mechanics). `UnifiedIR.dce!` already
# removes unused REMOVABLE plain statements; this file extends the cleanup to
# structure:
#
#   * `fold_island_branches!` — `br_if` on a constant condition becomes a
#     `goto` (edge killing inside islands; companion of the structured
#     `fold_constant_branches!`).
#   * `drop_unreachable_blocks!` — island blocks with no in-edges that are not
#     the entry are dropped via `kill_region!`.
#   * `merge_goto_chains!` — a block whose sole global predecessor edge is an
#     unconditional `goto` is merged into its predecessor (jump threading for
#     the plain-statement case).
#   * `dissolve_islands!` — a `cfg` op reduced to a single yield-terminated
#     block is dissolved into its parent region (`inline_region!`).
#   * `adce_region_ops!` — an `if`/`try`/acyclic `cfg`/single-trip `loop`
#     whose result is unused and whose regions contain only REMOVABLE
#     statements (and no exits escaping the op) is killed whole, including
#     the empty-arms case.
#
# All passes here run in one editable session inside `optimize_ir!`.

const _EDGE_KINDS = (K"goto", K"br_if", K"switch", K"await")

is_edge_kind(k::UnifiedIR.Kind) =
    k === K"goto" || k === K"br_if" || k === K"switch" || k === K"await"

"Constant Bool of an operand, consulting the type column for statements."
function const_bool_of(ir::UnifiedIR.IR, o::UnifiedIR.Operand)
    v = static_operand_value(ir, o)
    return v isa Bool ? v : nothing
end

"""
    fold_island_branches!(ir) -> Int

`br_if` whose condition is a known constant becomes an unconditional `goto`
carrying the kept edge bundle (footprint-preserving terminator rewrite).
"""
function fold_island_branches!(ir::UnifiedIR.IR)
    n = 0
    for s in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) === K"br_if" || continue
        cv = const_bool_of(ir, UnifiedIR.getop(ir, s, 1))
        cv isa Bool || continue
        bs = UnifiedIR.edge_bundles(ir, s)
        dest, args = bs[cv ? 1 : 2]
        UnifiedIR.replace_stmt!(ir, s, K"goto", UnifiedIR.op_block(dest),
                                UnifiedIR.op_inline(length(args)), args...)
        n += 1
    end
    return n
end

# In-edge counts for every block region, over ALL edge bundles in the IR
# (covers sealed cross-island gotos, §5.5).
function block_in_edges(ir::UnifiedIR.IR)
    tgt = Dict{Int32,Int}()
    for s in UnifiedIR.each_stmt(ir)
        is_edge_kind(UnifiedIR.stmt_kind(ir, s)) || continue
        for (dest, _) in UnifiedIR.edge_bundles(ir, s)
            tgt[dest.id] = get(tgt, dest.id, 0) + 1
        end
    end
    return tgt
end

"""
    drop_unreachable_blocks!(ir) -> Int

Kill island blocks with no predecessors that are not their island's entry
(first live owned region), to a fixpoint. Editable state.
"""
function drop_unreachable_blocks!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "drop_unreachable_blocks!")
    removed = 0
    changed = true
    while changed
        changed = false
        tgt = block_in_edges(ir)
        for s in collect(UnifiedIR.each_stmt(ir))
            UnifiedIR.is_tombstone(ir, s) && continue
            UnifiedIR.stmt_kind(ir, s) === K"cfg" || continue
            rs = UnifiedIR.live_owned_regions(ir, s)
            for (i, rid) in enumerate(rs)
                i == 1 && continue                    # entry block
                get(tgt, rid.id, 0) == 0 || continue
                UnifiedIR.kill_region!(ir, rid)
                removed += 1
                changed = true
            end
        end
    end
    return removed
end

"""
    merge_goto_chains!(ir) -> Int

If block `B` ends in an argument-free-or-matching `goto ^C`, `C` is not an
entry block, `C`'s only global in-edge is that goto, and `C` contains no
region-owning statements: clone `C`'s statements into `B` (block args replaced
by the edge arguments), retarget uses, replace `B`'s goto with `C`'s
terminator, and kill `C`. Editable state.
"""
function merge_goto_chains!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "merge_goto_chains!")
    merged = 0
    changed = true
    while changed
        changed = false
        tgt = block_in_edges(ir)
        for s in collect(UnifiedIR.each_stmt(ir))
            UnifiedIR.is_tombstone(ir, s) && continue
            UnifiedIR.stmt_kind(ir, s) === K"goto" || continue
            breg = UnifiedIR.stmt_region(ir, s)
            UnifiedIR.getregion(ir, breg).kind === UnifiedIR.REGION_BLOCK || continue
            dest, eargs = UnifiedIR.edge_bundles(ir, s)[1]
            dest == breg && continue                              # self loop
            dreg = UnifiedIR.getregion(ir, dest)
            dreg.dead && continue
            dreg.owner == UnifiedIR.getregion(ir, breg).owner || continue  # same island
            get(tgt, dest.id, 0) == 1 || continue                 # sole predecessor
            rs = UnifiedIR.live_owned_regions(ir, dreg.owner)
            (!isempty(rs) && rs[1] == dest) && continue           # never merge the entry away
            members = UnifiedIR.region_stmts(ir, dest)
            any(m -> UnifiedIR.owns_regions(UnifiedIR.stmt_kind(ir, m)), members) && continue
            isempty(members) && continue
            term = members[end]
            UnifiedIR.is_terminator(UnifiedIR.stmt_kind(ir, term)) || continue
            # clone C's body into B before the goto, remapping args and locals
            opmap = Dict{Int32,UnifiedIR.Operand}()
            for (i, a) in enumerate(dreg.args)
                i <= length(eargs) || (empty!(opmap); break)
                opmap[a.id] = eargs[i]
            end
            length(dreg.args) <= length(eargs) || continue
            remap(o::UnifiedIR.Operand) = begin
                UnifiedIR.optag(o) == UnifiedIR.TAG_STMT || return o
                get(opmap, UnifiedIR.asstmt(o).id, o)
            end
            lastclone = NULL_STMT
            for m in members
                mk = UnifiedIR.stmt_kind(ir, m)
                mk === K"region_arg" && continue
                m == term && break
                ops = UnifiedIR.Operand[remap(UnifiedIR.getop(ir, m, i))
                                        for i in 1:UnifiedIR.nops(ir, m)]
                c = UnifiedIR.insert_before!(ir, s, mk, ops...;
                                             type = UnifiedIR.stmt_type(ir, m),
                                             flag = UnifiedIR.stmt_flag(ir, m))
                opmap[m.id] = UnifiedIR.op_stmt(c)
                lastclone = c
            end
            # uses of C's defs in blocks C dominated now refer to the clones
            for m in members
                haskey(opmap, m.id) || continue
                UnifiedIR.replace_uses_where!(u -> !UnifiedIR.is_tombstone(ir, u), ir,
                                              m => opmap[m.id])
            end
            tops = UnifiedIR.Operand[remap(UnifiedIR.getop(ir, term, i))
                                     for i in 1:UnifiedIR.nops(ir, term)]
            UnifiedIR.replace_stmt!(ir, s, UnifiedIR.stmt_kind(ir, term), tops...;
                                    type = UnifiedIR.stmt_type(ir, term))
            UnifiedIR.kill_region!(ir, dest)
            merged += 1
            changed = true
            break   # region table changed; recompute in-edges
        end
    end
    return merged
end

"""
    dissolve_islands!(ir) -> Int

A `cfg` op with exactly one live block whose terminator is `yield` dissolves
into the parent region: entry block args are rewritten to the op's operands,
then `inline_region!` splices the contents and forwards the yield value to the
result's uses. Editable state.
"""
function dissolve_islands!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "dissolve_islands!")
    n = 0
    for s in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) === K"cfg" || continue
        rs = UnifiedIR.live_owned_regions(ir, s)
        length(rs) == 1 || continue
        blk = rs[1]
        term = UnifiedIR.region_terminator(ir, blk)
        term === nothing && continue
        UnifiedIR.stmt_kind(ir, term) === K"yield" || continue
        breg = UnifiedIR.getregion(ir, blk)
        if !isempty(breg.args)
            UnifiedIR.nops(ir, s) >= length(breg.args) || continue
            for (i, a) in enumerate(copy(breg.args))
                UnifiedIR.replace_uses_where!(_ -> true, ir, a => UnifiedIR.getop(ir, s, i))
                UnifiedIR.kill_stmt!(ir, a)
            end
            empty!(breg.args)
        end
        UnifiedIR.inline_region!(ir, s, blk)
        n += 1
    end
    return n
end

# Is region `r` inside the subtree of regions owned (transitively) by `op`?
# (Any ancestor region on r's parent chain whose owner is `op`.)
function region_within_op(ir::UnifiedIR.IR, r::RegionId, op::StmtId)
    seen = 0
    while !UnifiedIR.isnull(r) && (seen += 1) <= UnifiedIR.nregions(ir) + 1
        reg = UnifiedIR.getregion(ir, r)
        reg.owner == op && return true
        r = reg.parent
    end
    return false
end

# Would removing `op` (a region-owning statement) delete any observable
# behavior? True = safe to remove when the result is unused.
function removable_subtree(ir::UnifiedIR.IR, op::StmtId)
    k = UnifiedIR.stmt_kind(ir, op)
    work = RegionId[r for r in UnifiedIR.live_owned_regions(ir, op)]
    while !isempty(work)
        r = pop!(work)
        reg = UnifiedIR.getregion(ir, r)
        reg.activation === UnifiedIR.ACT_IMMEDIATE || return false  # deferred: §3.3
        for st in UnifiedIR.region_stmts(ir, r)
            sk = UnifiedIR.stmt_kind(ir, st)
            if sk === K"region_arg" || sk === K"yield"
                continue
            elseif sk === K"cell_set" || sk === K"cell_new"
                # cell stores carry pure-ish flags (§6: cells are IR-internal
                # memory), but deleting a store to a cell that outlives the op
                # changes what later reads observe — only stores to cells
                # defined inside the subtree die with it
                c = UnifiedIR.asstmt(UnifiedIR.getop(ir, st, 1))
                region_within_op(ir, UnifiedIR.stmt_region(ir, c), op) || return false
            elseif sk === K"break" || sk === K"continue"
                tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, st, 1))
                region_within_op(ir, tgt, op) || return false
            elseif sk === K"goto" || sk === K"br_if" || sk === K"switch"
                for (dest, _) in UnifiedIR.edge_bundles(ir, st)
                    region_within_op(ir, dest, op) || return false
                end
            elseif sk === K"return" || sk === K"unreachable" || sk === K"await"
                return false
            elseif UnifiedIR.owns_regions(sk)
                if sk === K"loop"
                    single_trip_loop(ir, st) || return false
                elseif sk === K"cfg"
                    island_acyclic(ir, st) || return false
                end
                append!(work, UnifiedIR.live_owned_regions(ir, st))
            else
                flg = UnifiedIR.stmt_flag(ir, st)
                flg & UnifiedIR.FLAG_REMOVABLE == UnifiedIR.FLAG_REMOVABLE || return false
            end
        end
    end
    if k === K"loop"
        single_trip_loop(ir, op) || return false
    elseif k === K"cfg"
        island_acyclic(ir, op) || return false
    end
    return true
end

# Termination evidence for a loop: the body's terminator is a `break` (the
# single-trip wrapper shape), or a `continue` with a const-false condition.
function single_trip_loop(ir::UnifiedIR.IR, op::StmtId)
    rs = UnifiedIR.live_owned_regions(ir, op)
    isempty(rs) && return false
    t = UnifiedIR.region_terminator(ir, rs[1])
    t === nothing && return false
    tk = UnifiedIR.stmt_kind(ir, t)
    tk === K"break" && return true
    if tk === K"continue"
        cv = const_bool_of(ir, UnifiedIR.getop(ir, t, 2))
        return cv === false
    end
    return false
end

# Termination evidence for an island: its block graph (own blocks only) is
# acyclic — every branch makes forward progress.
function island_acyclic(ir::UnifiedIR.IR, op::StmtId)
    rs = UnifiedIR.live_owned_regions(ir, op)
    own = Set{Int32}(r.id for r in rs)
    color = Dict{Int32,Int}()
    function visit(r::RegionId)
        c = get(color, r.id, 0)
        c == 2 && return true
        c == 1 && return false
        color[r.id] = 1
        t = UnifiedIR.region_terminator(ir, r)
        if t !== nothing && is_edge_kind(UnifiedIR.stmt_kind(ir, t))
            for (dest, _) in UnifiedIR.edge_bundles(ir, t)
                dest.id in own || continue
                visit(dest) || return false
            end
        end
        color[r.id] = 2
        return true
    end
    for r in rs
        visit(r) || return false
    end
    return true
end

"""
    adce_region_ops!(ir) -> Int

Kill region-owning ops (`if`/`try`/`cfg`/`loop`) whose result is unused and
whose region subtree contains only REMOVABLE statements with no exits
escaping the op and termination evidence (single-trip loops, acyclic
islands). Covers empty arms as the degenerate case. Editable state.
"""
function adce_region_ops!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "adce_region_ops!")
    removed = 0
    changed = true
    while changed
        changed = false
        counts = UnifiedIR.use_counts(ir)
        for s in collect(UnifiedIR.each_stmt(ir))
            UnifiedIR.is_tombstone(ir, s) && continue
            k = UnifiedIR.stmt_kind(ir, s)
            (k === K"if" || k === K"try" || k === K"cfg" || k === K"loop") || continue
            counts[s.id] == 0 || continue
            removable_subtree(ir, s) || continue
            UnifiedIR.kill_stmt!(ir, s)
            removed += 1
            changed = true
        end
    end
    return removed
end
