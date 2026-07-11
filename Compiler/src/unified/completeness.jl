# Join-point completeness verification (§6 "Join-point completeness" in the
# design doc): machinery substantiating the claim that, for slot-class
# cells, {if-results, loop region args, loop break-values, island phis} are
# jointly complete join points — the projection of iterated dominance
# frontiers onto the region tree — with machine-checkable exception classes.
#
# Three legs (driven from Compiler/test/unified/completeness.jl and
# Compiler/bench/unified_completeness.jl):
#   (a) residual classifier + stock oracle over real corpus bodies,
#   (b) structured fuzzing with semantic differentials,
#   (c) dominance-frontier correspondence: our placements vs the STOCK
#       slot2ssa iterated-DF computation on the flattened body.

# ---------------------------------------------------------------------------
# The joint promotion fixpoint (the object under verification)
# ---------------------------------------------------------------------------

"""
    structure_prep!(ir) -> ir

Bring a cfg-wrapped (entry-converted) body to structured region form without
any cell promotion: the pre-promotion input for `df_correspondence` and
`promotion_fixpoint!` (mirrors optimize_ir!'s structural sub-passes).
"""
function structure_prep!(ir::UnifiedIR.IR)
    while true
        UnifiedIR.editable(ir)
        c = fold_island_branches!(ir)
        c += drop_unreachable_blocks!(ir)
        c += merge_goto_chains!(ir)
        c += structurize!(ir)
        c += dissolve_islands!(ir)
        ir, _ = UnifiedIR.compact!(ir)
        c == 0 && break
    end
    return ir
end

"""
    promotion_fixpoint!(ir) -> ir

Run the cell-promotion passes — `promote_cells!` (dominating),
`promote_block_cells!` (single-region, covers cfg-island blocks),
`promote_arm_cells!` (if-join sinking), `promote_island_cells!` (island
phis), `promote_loop_cells!` (loop carrying) — to a joint fixpoint. This is
the §6 promotion pipeline in isolation (optimize_ir! interleaves the same
passes with its other rounds).
"""
function promotion_fixpoint!(ir::UnifiedIR.IR)
    while true
        c = UnifiedIR.promote_cells!(ir)
        c += promote_block_cells!(ir)
        c += drop_dead_cells!(ir)
        UnifiedIR.editable(ir)
        c += promote_arm_cells!(ir)
        c += promote_island_cells!(ir)
        c += promote_loop_cells!(ir)
        ir, _ = UnifiedIR.compact!(ir)
        c == 0 && break
    end
    return ir
end

"""
    drop_dead_cells!(ir) -> Int

Delete unobserved frame cells: no `cell_get`/`cell_isdefined`/escaping use —
stores into a never-read frame cell are unobservable (the dead-store side of
mem2reg; in the full optimizer DCE covers this, but the isolated promotion
fixpoint needs it so a store-only cell is not mistaken for a residual).
Dense state.
"""
function drop_dead_cells!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_DENSE, "drop_dead_cells!")
    UnifiedIR.flush_renames!(ir)
    dropped = 0
    for c in UnifiedIR.each_stmt(ir)
        UnifiedIR.is_tombstone(ir, c) && continue
        UnifiedIR.stmt_kind(ir, c) === K"cell" || continue
        sets = StmtId[]; news = StmtId[]
        observed = false
        UnifiedIR.each_ssa_use(ir) do site, used
            (used == c && !observed) || return
            site isa UnifiedIR.StmtOperand || (observed = true; return)
            u = site.user
            UnifiedIR.is_tombstone(ir, u) && return
            k = UnifiedIR.stmt_kind(ir, u)
            if k === K"cell_set" && site.opidx == 1
                push!(sets, u)
            elseif k === K"cell_new"
                push!(news, u)
            else
                observed = true          # get / isdefined / escape
            end
        end
        observed && continue
        for st in sets
            UnifiedIR.delete_stmt!(ir, st)
        end
        for nw in news
            UnifiedIR.delete_stmt!(ir, nw)
        end
        UnifiedIR.delete_stmt!(ir, c)
        dropped += 1
    end
    UnifiedIR.flush_renames!(ir)
    return dropped
end

# ---------------------------------------------------------------------------
# (a) Residual classifier
# ---------------------------------------------------------------------------

const RESIDUAL_REASONS = (:escape_or_token, :throw_edge_handler,
                          :maybe_undef_read, :island,
                          :refused_multilevel_exit, :UNCLASSIFIED)

"Does the region ancestry of `s` cross a `try`-owned region that does not
also enclose `anchor`? (Throw-edge boundary relative to the cell.)"
function _crosses_try(ir::UnifiedIR.IR, s::StmtId, anchor::StmtId)
    ar = UnifiedIR.stmt_region(ir, anchor)
    r = UnifiedIR.stmt_region(ir, s)
    while !UnifiedIR.isnull(r)
        r == ar && return false
        reg = UnifiedIR.getregion(ir, r)
        if !UnifiedIR.isnull(reg.owner) &&
           UnifiedIR.stmt_kind(ir, reg.owner) === K"try" &&
           !UnifiedIR.is_ancestor(ir, r, ar)
            return true
        end
        r = reg.parent
    end
    return false
end

"""
    classify_residual_cells(ir) -> Vector{Pair{StmtId,Symbol}}

Classify every cell remaining after promotion with a machine-checkable
reason (`RESIDUAL_REASONS`). `:UNCLASSIFIED` is the completeness-bug signal:
a residual cell none of the documented exception classes explains.
"""
function classify_residual_cells(ir::UnifiedIR.IR)
    out = Pair{StmtId,Symbol}[]
    for c in UnifiedIR.each_stmt(ir)
        UnifiedIR.is_tombstone(ir, c) && continue
        k = UnifiedIR.stmt_kind(ir, c)
        (k === K"cell" || k === K"cell_shared") || continue
        push!(out, c => _classify_cell(ir, c))
    end
    return out
end

function _classify_cell(ir::UnifiedIR.IR, cell::StmtId)
    UnifiedIR.stmt_kind(ir, cell) === K"cell_shared" && return :escape_or_token
    sets = StmtId[]; gets = StmtId[]; isdefs = StmtId[]; news = StmtId[]
    escaped = false
    UnifiedIR.each_ssa_use(ir) do site, used
        (used == cell && !escaped) || return
        site isa UnifiedIR.StmtOperand || (escaped = true; return)
        u = site.user
        UnifiedIR.is_tombstone(ir, u) && return
        k = UnifiedIR.stmt_kind(ir, u)
        if k === K"cell_set" && site.opidx == 1
            push!(sets, u)
        elseif k === K"cell_get"
            push!(gets, u)
        elseif k === K"cell_isdefined"
            push!(isdefs, u)
        elseif k === K"cell_new"
            push!(news, u)
        else
            escaped = true
        end
    end
    escaped && return :escape_or_token
    # gc_preserve token cells: the stored value is a preserve token
    for st in sets
        v = UnifiedIR.getop(ir, st, 2)
        if UnifiedIR.optag(v) == UnifiedIR.TAG_STMT &&
           UnifiedIR.stmt_kind(ir, UnifiedIR.asstmt(v)) === K"gc_preserve_begin"
            return :escape_or_token
        end
    end
    # throw-edge observability: reads/queries in handlers, or any use across
    # a try boundary relative to the cell
    (any(u -> _in_handler(ir, u), gets) || any(u -> _in_handler(ir, u), isdefs)) &&
        return :throw_edge_handler
    any(u -> _crosses_try(ir, u, cell), Iterators.flatten((sets, gets, isdefs))) &&
        return :throw_edge_handler
    # island-resident uses (cfg form: island mem2reg's domain; residual =
    # cross-block case it refused)
    (UnifiedIR.inside_island(ir, cell) ||
     any(u -> UnifiedIR.inside_island(ir, u),
         Iterators.flatten((sets, gets, isdefs)))) && return :island
    # maybe-undef: some read/query no store dominates, or a non-declaration
    # cell_new (re-undefine)
    dominated(u) = any(st -> st.id < u.id && UnifiedIR.dominates_for_cell(ir, st, u), sets)
    (any(d -> !dominated(d), isdefs) || any(g -> !dominated(g), gets) ||
     !all(nw -> all(st -> nw.id < st.id, sets), news)) && return :maybe_undef_read
    # loop-boundary refusals (the loop pass's conservative cases: multi-level
    # exit-value threading, ambiguous body reaching, chain shapes): a store
    # inside a loop body whose value is observable outside that body, or
    # backedge-carried reads
    for st in sets, g in gets
        if UnifiedIR.shares_loop(ir, st, g) &&
           !UnifiedIR.is_ancestor(ir, UnifiedIR.stmt_region(ir, st), UnifiedIR.stmt_region(ir, g))
            return :refused_multilevel_exit
        end
        st.id > g.id && UnifiedIR.shares_loop(ir, st, g) && return :refused_multilevel_exit
    end
    for st in sets
        r = UnifiedIR.stmt_region(ir, st)
        while !UnifiedIR.isnull(r)
            reg = UnifiedIR.getregion(ir, r)
            if reg.kind === UnifiedIR.REGION_LOOP_BODY &&
               !all(g -> UnifiedIR.is_ancestor(ir, r, UnifiedIR.stmt_region(ir, g)), gets)
                return :refused_multilevel_exit
            end
            r = reg.parent
        end
    end
    return :UNCLASSIFIED
end

# ---------------------------------------------------------------------------
# (c) Dominance-frontier correspondence
# ---------------------------------------------------------------------------

"""
    df_correspondence(ir0) -> (; results, ok)

For every frame cell of the (pre-promotion, dense) body `ir0`: flatten the
body through the exit converter, compute the STOCK slot2ssa iterated-DF phi
placement (liveness-pruned, `Compiler.iterated_dominance_frontier`) for the
cell's store set, run the promotion fixpoint with the placement trace on,
and compare block sets. Per cell:

    (; cell, status, expected, ours, extra)

status ∈ (:match, :missing, :residual_classified, :residual_unclassified).
`:missing` (expected DF blocks our promotion placed no join value at) is a
completeness bug. `extra` counts placements beyond pruned IDF — harmless
but suboptimal (arm sinking is unpruned; region-form threading through
enclosing arms has no classical-phi counterpart).
"""
function df_correspondence(ir0::UnifiedIR.IR)
    ci, cx = ir_to_codeinfo_ctx(ir0)
    code = ci.code::Vector{Any}
    cfg = Compiler.compute_basic_blocks(code)
    domtree = Compiler.construct_domtree(cfg.blocks)
    blockof(pc::Int) = Compiler.block_for_inst(cfg, pc)
    labelblock(key) = haskey(cx.labels, key) ?
        blockof(min(cx.labels[key], length(code))) : nothing
    # statically dead code (e.g. statements after an if whose arms all exit —
    # `continue` is a region terminator) flattens to unreachable blocks, where
    # dominance is undefined (idom 0) and IDF is meaningless: the classical
    # comparison only exists over the reachable subgraph, so filter both sides
    reachable(b::Int) = b == 1 || domtree.idoms_bb[b] != 0

    cells = StmtId[]
    for s in UnifiedIR.each_stmt(ir0)
        UnifiedIR.stmt_kind(ir0, s) === K"cell" && push!(cells, s)
    end
    # stock expectation per cell (by slot)
    expected = Dict{Int,Vector{Int}}()   # cell id -> sorted DF blocks
    for c in cells
        slot = get(cx.slotof, Int32(c.id), nothing)
        slot === nothing && continue
        defs = Int[]; uses = Int[]
        for (pc, st) in enumerate(code)
            reachable(blockof(pc)) || continue
            if st isa Expr && st.head === :(=) && st.args[1] == Core.SlotNumber(slot)
                push!(defs, pc)
            end
            _uses_slot(st, slot) && push!(uses, pc)
        end
        isempty(defs) && continue
        liveness = Compiler.compute_live_ins(cfg, sort!(defs), uses)
        idf = Compiler.iterated_dominance_frontier(cfg, liveness, domtree)
        expected[Int(c.id)] = sort!(idf)
    end

    # our placements: run the fixpoint round by round, back-translating each
    # round's trace ids to ORIGINAL ir0 ids through the compaction RemapSets
    # (anchors — if/loop stmts — and cells always pre-exist, so they are
    # translatable; statements the passes insert are not anchors)
    trace = Tuple{Symbol,Int,Int}[]
    ir = ir0
    inv = collect(1:UnifiedIR.nstmts(ir))          # current id -> original id
    rinv = collect(1:UnifiedIR.nregions(ir))       # current region -> original
    getinv(i::Int) = 1 <= i <= length(inv) ? inv[i] : 0
    getrinv(i::Int) = 1 <= i <= length(rinv) ? rinv[i] : 0
    while true
        tr = Tuple{Symbol,Int,Int}[]
        PROMOTION_TRACE[] = tr
        c = 0
        rs = try
            c += UnifiedIR.promote_cells!(ir)
            c += promote_block_cells!(ir)
            c += drop_dead_cells!(ir)
            UnifiedIR.editable(ir)
            c += promote_arm_cells!(ir)
            c += promote_island_cells!(ir)
            c += promote_loop_cells!(ir)
            ir, rs_ = UnifiedIR.compact!(ir)
            rs_
        finally
            PROMOTION_TRACE[] = nothing
        end
        for (k, a, cid) in tr
            # :island_phi anchors are region ids; all others are stmt ids
            oa = k === :island_phi ? getrinv(a) : getinv(a)
            ocid = getinv(cid)
            (oa == 0 || ocid == 0) || push!(trace, (k, oa, ocid))
        end
        newinv = zeros(Int, UnifiedIR.nstmts(ir))
        for o in 1:length(rs.stmt)
            n = Int(rs.stmt[o])
            n != 0 && (newinv[n] = getinv(o))
        end
        inv = newinv
        newrinv = zeros(Int, UnifiedIR.nregions(ir))
        for o in 1:length(rs.region)
            n = Int(rs.region[o])
            n != 0 && (newrinv[n] = getrinv(o))
        end
        rinv = newrinv
        c == 0 && break
    end
    residual = Dict{Int,Symbol}()
    for (c, r) in classify_residual_cells(ir)
        oc = getinv(Int(c.id))
        oc != 0 && (residual[oc] = r)
    end
    ours = Dict{Int,Vector{Int}}(); extra = Dict{Int,Int}()
    for (kind, anchor, cellid) in trace
        b = kind === :if_join ? labelblock((:join, Int32(anchor))) :
            kind === :loop_header ? labelblock((:head, Int32(anchor))) :
            kind === :loop_break ? labelblock((:brk, Int32(anchor))) :
            kind === :island_phi ? labelblock((:blk, Int32(anchor))) :
            nothing                      # :if_thread = structural extra
        if b === nothing
            extra[cellid] = get(extra, cellid, 0) + 1
        elseif reachable(b)              # placements in dead code have no
            push!(get!(() -> Int[], ours, cellid), b)  # classical counterpart
        end
    end
    results = NamedTuple[]
    ok = true
    for c in cells
        cid = Int(c.id)
        haskey(expected, cid) || continue
        exp = expected[cid]
        got = sort!(unique(get(ours, cid, Int[])))
        ex = get(extra, cid, 0) + length(setdiff(got, exp))
        status = if haskey(residual, cid)
            residual[cid] === :UNCLASSIFIED ? :residual_unclassified : :residual_classified
        elseif isempty(setdiff(exp, got))
            :match
        else
            :missing
        end
        status in (:missing, :residual_unclassified) && (ok = false)
        push!(results, (; cell = cid, status, expected = exp, ours = got, extra = ex))
    end
    return (; results, ok)
end

_uses_slot(@nospecialize(st), slot::Int) = _scan_slot_use(st, slot)
function _scan_slot_use(@nospecialize(x), slot::Int)
    if x isa Core.SlotNumber
        return x.id == slot
    elseif x isa Expr
        if x.head === :(=)
            return _scan_slot_use(x.args[2], slot)
        end
        return any(a -> _scan_slot_use(a, slot), x.args)
    elseif x isa Core.GotoIfNot
        return _scan_slot_use(x.cond, slot)
    elseif x isa Core.ReturnNode
        return isdefined(x, :val) && _scan_slot_use(x.val, slot)
    end
    return false
end
