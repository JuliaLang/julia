# Generic passes over the core structure: DCE, constant-branch folding, cell
# promotion (§6 v1 policy). Julia-semantic passes live provider-side.

"""
    dce!(ir) -> Int

Dense-state DCE: delete unused plain statements whose flags satisfy the
REMOVABLE mask (§8.2). Region-owning ops are handled by
`fold_constant_branches!`/editable surgery, not here — with one exception:
an unused `closure` is removable on its CREATION-site flags alone, because
effect composition is activation-mode-aware (§3.3): the deferred body's
effects do not count at the creation site (they surface at call sites), and
no exit can escape the boundary (L1). Its subtree is tombstoned and the dead
regions are dropped at the next `compact!`. Returns count removed.
"""
function dce!(ir::IR)
    check_state(ir, LAYOUT_DENSE, "dce!")
    flush_renames!(ir)
    body = ir.body
    n = Int(body.len)
    removed = 0
    # unused closures first (killing a subtree frees uses of captured values,
    # which the plain worklist below then collects)
    changed = true
    while changed
        changed = false
        counts0 = use_counts(ir)
        for i in 1:n
            body.kind[i] === K"closure" || continue
            s = StmtId(i)
            counts0[s.id] == 0 || continue
            stmt_flag(ir, s) & FLAG_REMOVABLE == FLAG_REMOVABLE || continue
            kill_stmt!(ir, s)
            removed += 1
            changed = true
        end
    end
    counts = use_counts(ir)
    work = StmtId[]
    for i in n:-1:1
        body.kind[i] === KIND_DELETED && continue
        counts[i] == 0 && push!(work, StmtId(i))
    end
    while !isempty(work)
        s = pop!(work)
        body.kind[s.id] === KIND_DELETED && continue
        counts[s.id] == 0 || continue
        k = body.kind[s.id]
        result_arity(k) == 0 && continue           # terminators, stores, …
        owns_regions(k) && continue
        k === K"region_arg" && continue
        stmt_flag(ir, s) & FLAG_REMOVABLE == FLAG_REMOVABLE || continue
        isguardcond = false
        for reg in ir.regions
            is_guard(reg) && reg.cond == s && (isguardcond = true)
        end
        isguardcond && continue
        # decrement uses of operands, requeue newly dead
        for j in 1:nops(ir, s)
            o = getop(ir, s, j)
            if optag(o) == TAG_STMT
                d = asstmt(o)
                counts[d.id] -= 1
                counts[d.id] == 0 && push!(work, d)
            end
        end
        delete_stmt!(ir, s)
        removed += 1
    end
    return removed
end

"""
    fold_constant_branches!(ir) -> (ir, Int)

Editable-surgery pass: `if` ops whose condition is a constant are dissolved
via `inline_region!` (irinterp's edge-killing as constrained surgery, §10.3).
Expects editable state; returns fold count.
"""
function fold_constant_branches!(ir::IR)
    check_state(ir, LAYOUT_EDITABLE, "fold_constant_branches!")
    folded = 0
    changed = true
    while changed
        changed = false
        for s in collect(each_stmt(ir))
            is_tombstone(ir, s) && continue
            stmt_kind(ir, s) === K"if" || continue
            o = getop(ir, s, 1)
            cv = nothing
            if optag(o) == TAG_INLINE
                cv = imm_value(o)
            elseif optag(o) == TAG_CONST
                cv = getconst(ir, o)
            end
            cv isa Bool || continue
            regions = owned_regions(ir, s)
            filter!(r -> !getregion(ir, r).dead, regions)
            keep = if cv
                regions[1]
            elseif length(regions) >= 2
                regions[2]
            else
                nothing
            end
            if keep === nothing
                # if without else on false condition: whole op disappears
                for r in regions
                    kill_region!(ir, r)
                end
                kill_stmt!(ir, s)
            else
                # NB. only join (result-terminated) arms may inline: splicing
                # a DIVERGING arm mid-region would leave two terminators, and
                # deleting the tail behind it silences the verifier exactly
                # when the fold acted on unstable mid-round inference — keep
                # such folds for the round where the diverge is structural
                term = region_terminator(ir, keep)
                if term !== nothing && stmt_kind(ir, term) !== K"result"
                    continue
                end
                inline_region!(ir, s, keep)
            end
            folded += 1
            changed = true
        end
    end
    return (ir, folded)
end

# ---------------------------------------------------------------------------
# Cell promotion (§6 v1 policy): mem2reg over the region tree, promoting
# frame-class cells whose every use is reached by an unconditional dominating
# store within the same activation, never across a throw edge and never
# across an `await`/cfg island.
# ---------------------------------------------------------------------------

"""
    promote_cells!(ir) -> Int

Returns the number of cells fully promoted. Dense state.
"""
function promote_cells!(ir::IR)
    check_state(ir, LAYOUT_DENSE, "promote_cells!")
    flush_renames!(ir)
    body = ir.body
    n = Int(body.len)
    promoted = 0
    for ci in 1:n
        body.kind[ci] === K"cell" || continue     # frame-class only (§6)
        cell = StmtId(ci)
        stores = StmtId[]; gets = StmtId[]; isdefs = StmtId[]; news = StmtId[]
        escaped = false
        for i in 1:n
            body.kind[i] === KIND_DELETED && continue
            s = StmtId(i)
            for j in 1:nops(ir, s)
                o = getop(ir, s, j)
                optag(o) == TAG_STMT && asstmt(o) == cell || continue
                k = body.kind[i]
                if k === K"cell_set" && j == 1
                    push!(stores, s)
                elseif k === K"cell_get"
                    push!(gets, s)
                elseif k === K"cell_isdefined"
                    push!(isdefs, s)
                elseif k === K"cell_new"
                    push!(news, s)
                else
                    escaped = true
                end
            end
        end
        (escaped || isempty(stores)) && continue
        # cell_new re-undefines: only news preceding every store (the newvar
        # declaration pattern) are harmless; anything else keeps memory form
        firststore = minimum(s -> s.id, stores)
        all(nw -> nw.id < firststore, news) || continue
        # Island uses are fine for the DOMINATING case: `dominates_for_cell`
        # only walks region nesting, so a store proves dominance either from
        # outside the island (executes before the whole cfg) or from the same
        # block subtree (blocks execute their members in order, and re-entry
        # re-executes from the top, so the store re-executes before the get
        # on every iteration). Cross-block flow never establishes dominance.
        # The one island hazard is handled below: a LATER store can reach an
        # earlier get through island back edges (`shares_island`).
        # all stores must dominate all gets uniformly: every store's region is
        # ancestor-or-self of every get's region, with no handler boundary
        # between (throw-edge rule), and stores precede gets they reach.
        ok = true
        for g in gets
            reaching = NULL_STMT
            for st in stores
                st.id < g.id || continue
                dominates_for_cell(ir, st, g) || continue
                (isnull(reaching) || reaching.id < st.id) && (reaching = st)
            end
            if isnull(reaching)
                ok = false; break
            end
            # no other store on a path between: any store in a region NOT
            # ancestor-of-g between reaching and g kills the promotion
            for st in stores
                st == reaching && continue
                if reaching.id < st.id < g.id && !dominates_for_cell(ir, st, g)
                    ok = false; break
                end
                # backedge reach: a store at-or-after the get inside a shared
                # loop — or anywhere in the same cfg island, whose edges may
                # loop — reaches the get on the next iteration. Both hazards
                # are SHADOWED when the reaching store re-executes before the
                # get on every such re-entry: for loops, when it sits inside
                # the innermost loop the interfering store shares with the
                # get; for islands, when it lives in the island (dominance
                # already put it in the get's own block subtree).
                if st.id > g.id
                    X = _innermost_shared_loop(ir, st, g)
                    if !isnull(X) && !is_ancestor(ir, X, stmt_region(ir, reaching))
                        ok = false; break
                    end
                    # innermost-ISLAND rule, exactly like the loop one: the
                    # backedge that carries st's value to g belongs to the
                    # innermost cfg they share — the reaching store shadows
                    # it only from inside that same cfg (dominance then puts
                    # it in g's own block subtree, re-executed on re-entry)
                    own = _innermost_shared_island(ir, st, g)
                    if !isnull(own) && !_inside_cfg(ir, reaching, own)
                        ok = false; break
                    end
                end
            end
            ok || break
        end
        # §6 v1 policy: EVERY use must be reached by an unconditional dominating
        # store within the same activation, never across a throw edge. A
        # `cell_isdefined` without a dominating store (e.g. in a handler
        # observing a possibly-unstored cell) keeps the whole cell in memory
        # form — partial promotion that deletes stores would change what the
        # handler observes.
        for d in isdefs
            any(st -> st.id < d.id && dominates_for_cell(ir, st, d), stores) ||
                (ok = false; break)
        end
        ok || continue
        # rewrite: each get takes its reaching store's value operand
        for g in gets
            reaching = NULL_STMT
            for st in stores
                st.id < g.id && dominates_for_cell(ir, st, g) &&
                    (isnull(reaching) || reaching.id < st.id) && (reaching = st)
            end
            v = getop(ir, reaching, 2)
            replace_uses_where!(_ -> true, ir, g => v)
            delete_stmt!(ir, g)
        end
        for d in isdefs
            # every isdefined site is dominated by a store (checked above)
            replace_uses_where!(_ -> true, ir, d => op_inline(true))
            delete_stmt!(ir, d)
        end
        # remove stores, declaration news, and the cell when unreferenced
        for st in stores
            delete_stmt!(ir, st)
        end
        for nw in news
            delete_stmt!(ir, nw)
        end
        counts = use_counts(ir)
        counts[cell.id] == 0 && delete_stmt!(ir, cell)
        promoted += 1
    end
    flush_renames!(ir)
    return promoted
end

"Do two statements share an enclosing loop body (backedge-reach hazard)?"
function shares_loop(ir::IR, a::StmtId, b::StmtId)
    r = stmt_region(ir, a)
    while !isnull(r)
        reg = getregion(ir, r)
        if reg.kind === REGION_LOOP_BODY && is_ancestor(ir, r, stmt_region(ir, b))
            return true
        end
        r = reg.parent
    end
    return false
end

"The innermost loop body on `a`'s region chain that contains `b` — the
tightest backedge able to carry `a`'s stored value around to `b`."
function _innermost_shared_loop(ir::IR, a::StmtId, b::StmtId)
    r = stmt_region(ir, a)
    while !isnull(r)
        reg = getregion(ir, r)
        if reg.kind === REGION_LOOP_BODY && is_ancestor(ir, r, stmt_region(ir, b))
            return r
        end
        r = reg.parent
    end
    return NULL_REGION
end

"The INNERMOST cfg op whose blocks contain both `a` and `b` — its edges
form the tightest re-entry able to carry `a`'s stored value around to `b`.
NULL when they share no island."
function _innermost_shared_island(ir::IR, a::StmtId, b::StmtId)
    r = stmt_region(ir, a)
    while !isnull(r)
        reg = getregion(ir, r)
        if reg.kind === REGION_BLOCK
            own = reg.owner
            rb = stmt_region(ir, b)
            while !isnull(rb)
                regb = getregion(ir, rb)
                regb.kind === REGION_BLOCK && regb.owner == own && return own
                rb = regb.parent
            end
        end
        r = reg.parent
    end
    return NULL_STMT
end

"Does `s` live under some block of cfg op `own`?"
function _inside_cfg(ir::IR, s::StmtId, own::StmtId)
    r = stmt_region(ir, s)
    while !isnull(r)
        reg = getregion(ir, r)
        reg.kind === REGION_BLOCK && reg.owner == own && return true
        r = reg.parent
    end
    return false
end

"Do `a` and `b` live under blocks of the same cfg island? (Island edges may
loop, so a positionally-later store can reach an earlier get across
iterations — the conservative backedge-reach test for goto-land.)"
function shares_island(ir::IR, a::StmtId, b::StmtId)
    r = stmt_region(ir, a)
    while !isnull(r)
        reg = getregion(ir, r)
        if reg.kind === REGION_BLOCK
            own = reg.owner
            rb = stmt_region(ir, b)
            while !isnull(rb)
                regb = getregion(ir, rb)
                regb.kind === REGION_BLOCK && regb.owner == own && return true
                rb = regb.parent
            end
        end
        r = reg.parent
    end
    return false
end

"Is `s` inside a cfg island (REGION_BLOCK on its region ancestry)?"
function inside_island(ir::IR, s::StmtId)
    r = stmt_region(ir, s)
    while !isnull(r)
        getregion(ir, r).kind === REGION_BLOCK && return true
        r = getregion(ir, r).parent
    end
    return false
end

# Store dominates use for cell promotion: region-ancestry + order, with no
# handler region strictly between the use and the store's region (the §6
# never-across-a-throw-edge rule), same activation.
function dominates_for_cell(ir::IR, st::StmtId, use::StmtId)
    sr = stmt_region(ir, st)
    r = stmt_region(ir, use)
    seen = false
    while !isnull(r)
        if r == sr
            seen = true
            break
        end
        reg = getregion(ir, r)
        reg.kind === REGION_HANDLER && return false
        reg.activation === ACT_IMMEDIATE || return false
        r = reg.parent
    end
    seen || return false
    return st.id < use.id
end
