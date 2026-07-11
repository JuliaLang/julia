# SROA on UnifiedIR (§10.4; stock reference: Compiler/src/ssair/passes.jl
# `sroa_pass!`/`sroa_mutables!` — the CASES, not the mechanics):
#
#   1. Immutable-struct SROA: `extract` (canonicalized getfield) of a locally
#      constructed `Core.tuple`/`K"new"` of a concrete immutable type forwards
#      the field value, following `refine` chains; legality of the forwarded
#      operand at the use site is checked with `UnifiedIR.visible`.
#      (Extension of `forward_extracts!`, which lives in optimize.jl.)
#   2. If-result forwarding: an `if` whose live arms all yield the same
#      operand forwards that operand to the result's uses (the phi-of-one-
#      value case of stock SROA lifting).
#   3. Mutable-struct SROA: a `new` of a mutable struct that never escapes
#      (uses are only getfield/extract loads and setfield! stores with
#      constant fields) becomes per-field cells (K"cell" + cell_set/cell_get);
#      `UnifiedIR.promote_cells!` + `dce!` then clean up.
#   4. Dead `new` elimination falls out of `dce!` once `refine_effects!`
#      marks nothrow constructions REMOVABLE (optimize.jl).

"Field index (1-based) for a constant field designator (Int or Symbol), or nothing."
function field_index_of(@nospecialize(T), @nospecialize(fld))
    T isa DataType || return nothing
    if fld isa Int
        1 <= fld <= fieldcount(T) || return nothing
        return fld
    elseif fld isa Symbol
        fi = Base.fieldindex(T, fld, false)
        return fi == 0 ? nothing : fi
    end
    return nothing
end

"Concrete DataType of a lattice element/operand type, or nothing."
function concrete_datatype(@nospecialize(tl))
    T = tl isa CC.Const ? tl.val : CC.singleton_type(CC.widenconst(tl))
    if T === nothing
        wt = CC.widenconst(tl)
        wt isa DataType && isconcretetype(wt) && (T = wt)
    end
    T isa DataType && isconcretetype(T) || return nothing
    return T
end

"Skip through K\"refine\" chains to the underlying definition."
function skip_refines(ir::UnifiedIR.IR, def::StmtId)
    steps = 0
    while UnifiedIR.stmt_kind(ir, def) === K"refine" && (steps += 1) <= 32
        o = UnifiedIR.getop(ir, def, 1)
        UnifiedIR.optag(o) == UnifiedIR.TAG_STMT || break
        def = UnifiedIR.asstmt(o)
    end
    return def
end

"""
    forward_if_results!(ir) -> Int

`if` ops (two live arms) whose arms each yield exactly one operand and all
yield the *same* operand forward it to the result's uses. Statement operands
must be visible at the `if` itself (hence at every use of the result). The
`if` op remains for its arm effects; `adce_region_ops!` removes it when pure.
Dense state.
"""
function forward_if_results!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_DENSE, "forward_if_results!")
    n = 0
    counts = UnifiedIR.use_counts(ir)
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.stmt_kind(ir, s) === K"if" || continue
        counts[s.id] > 0 || continue
        rs = UnifiedIR.live_owned_regions(ir, s)
        length(rs) == 2 || continue    # one-armed if: not-taken result is nothing
        yop = nothing
        ok = true
        for r in rs
            t = UnifiedIR.region_terminator(ir, r)
            (t !== nothing && UnifiedIR.stmt_kind(ir, t) === K"yield" &&
             UnifiedIR.nops(ir, t) == 1) || (ok = false; break)
            o = UnifiedIR.getop(ir, t, 1)
            if yop === nothing
                yop = o
            elseif yop.bits != o.bits
                ok = false
                break
            end
        end
        (ok && yop !== nothing) || continue
        yo = yop::UnifiedIR.Operand
        if UnifiedIR.optag(yo) == UnifiedIR.TAG_STMT
            UnifiedIR.visible(ir, UnifiedIR.asstmt(yo), s) || continue
        end
        UnifiedIR.replace_uses!(ir, s => yo)
        n += 1
    end
    n > 0 && UnifiedIR.flush_renames!(ir)
    return n
end

"""
    promote_block_cells!(ir) -> Int

Single-region mem2reg — the SROA-adjacent cleanup for spliced callee frame
cells: a frame-class cell whose every use is a direct member of *one* region
(including cfg island blocks, which the §6 `promote_cells!` policy refuses
wholesale) is promoted when every read and every definedness test is preceded
in region order by a store. Within one region's direct members execution is
sequential, so the last preceding store is the reaching definition on every
path — regardless of how the region itself is entered or re-entered (loop
backedges re-execute the store before the read). Dense state.
"""
function promote_block_cells!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_DENSE, "promote_block_cells!")
    UnifiedIR.flush_renames!(ir)
    promoted = 0
    for ci in 1:UnifiedIR.nstmts(ir)
        ir.body.kind[ci] === K"cell" || continue     # frame-class only (§6)
        cell = StmtId(Int32(ci))
        R = NULL_REGION
        stores = StmtId[]; gets = StmtId[]; news = StmtId[]; isdefs = StmtId[]
        ok = true
        UnifiedIR.each_ssa_use(ir) do site, used
            (ok && used == cell) || return
            site isa UnifiedIR.StmtOperand || (ok = false; return)
            u = site.user
            ur = UnifiedIR.stmt_region(ir, u)
            if R == NULL_REGION
                R = ur
            elseif R != ur
                ok = false
                return
            end
            k = UnifiedIR.stmt_kind(ir, u)
            if k === K"cell_set" && site.opidx == 1
                push!(stores, u)
            elseif k === K"cell_get"
                push!(gets, u)
            elseif k === K"cell_new"
                push!(news, u)
            elseif k === K"cell_isdefined"
                push!(isdefs, u)
            else
                ok = false          # escape: value use, store of the cell, …
            end
        end
        (ok && !isempty(stores)) || continue
        firststore = minimum(s -> s.id, stores)
        all(nw -> nw.id < firststore, news) || continue   # declaration news only
        reaching(g) = begin
            best = NULL_STMT
            for st in stores
                st.id < g.id && (UnifiedIR.isnull(best) || best.id < st.id) && (best = st)
            end
            best
        end
        all(g -> !UnifiedIR.isnull(reaching(g)), gets) || continue
        all(d -> !UnifiedIR.isnull(reaching(d)), isdefs) || continue
        for g in gets
            v = UnifiedIR.getop(ir, reaching(g), 2)
            UnifiedIR.replace_uses_where!(_ -> true, ir, g => v)
            UnifiedIR.delete_stmt!(ir, g)
        end
        for d in isdefs
            UnifiedIR.replace_uses_where!(_ -> true, ir, d => UnifiedIR.op_inline(true))
            UnifiedIR.delete_stmt!(ir, d)
        end
        for st in stores
            UnifiedIR.delete_stmt!(ir, st)
        end
        for nw in news
            UnifiedIR.delete_stmt!(ir, nw)
        end
        counts = UnifiedIR.use_counts(ir)
        counts[cell.id] == 0 && UnifiedIR.delete_stmt!(ir, cell)
        promoted += 1
    end
    UnifiedIR.flush_renames!(ir)
    return promoted
end

# ---------------------------------------------------------------------------
# Loop-carried cell promotion (§5.3 / §6): the loop-crossing mem2reg cases
# that `promote_cells!`/`promote_block_cells!` refuse.
# ---------------------------------------------------------------------------

# Store dominates a use site for promotion purposes: the site's region
# ancestry reaches the store's region crossing only immediate, non-handler
# (§6 throw-edge rule), non-island regions, and the store precedes the site.
function _cell_dominates_ed(ir::UnifiedIR.IR, st::StmtId, site::StmtId)
    sr = UnifiedIR.stmt_region(ir, st)
    r = UnifiedIR.stmt_region(ir, site)
    while r != sr
        reg = UnifiedIR.getregion(ir, r)
        (reg.kind === UnifiedIR.REGION_HANDLER || reg.kind === UnifiedIR.REGION_BLOCK ||
         reg.kind === UnifiedIR.REGION_GUARD) && return false
        reg.activation === UnifiedIR.ACT_IMMEDIATE || return false
        UnifiedIR.isnull(reg.parent) && return false
        r = reg.parent
    end
    return UnifiedIR.comes_before(ir, st, site)
end

# Can store `t` execute on some path that then reaches `site`? False only
# when t and site sit in sibling arms of one `if` (mutually exclusive).
function _may_reach(ir::UnifiedIR.IR, t::StmtId, site::StmtId)
    A = RegionId[]
    r = UnifiedIR.stmt_region(ir, t)
    while !UnifiedIR.isnull(r)
        push!(A, r)
        r = UnifiedIR.getregion(ir, r).parent
    end
    idxof = Dict{Int32,Int}(a.id => i for (i, a) in enumerate(A))
    r = UnifiedIR.stmt_region(ir, site)
    prev = NULL_REGION
    while !UnifiedIR.isnull(r)
        if haskey(idxof, r.id)
            i = idxof[r.id]
            (i == 1 || UnifiedIR.isnull(prev)) && return true
            tchild = A[i - 1]
            tc = UnifiedIR.getregion(ir, tchild)
            pc = UnifiedIR.getregion(ir, prev)
            if tchild != prev && !UnifiedIR.isnull(tc.owner) && tc.owner == pc.owner &&
               tc.kind === UnifiedIR.REGION_ARM && pc.kind === UnifiedIR.REGION_ARM
                return false
            end
            return true
        end
        prev = r
        r = UnifiedIR.getregion(ir, r).parent
    end
    return true
end

# Reaching store for `site` among `stores` (editable state, okey order):
# (:store, st) — unambiguous dominating reaching definition;
# (:none, _)   — no store can reach (value is the incoming one);
# (:ambig, _)  — a non-dominating store may reach: refuse promotion.
function _reach_ed(ir::UnifiedIR.IR, stores::Vector{StmtId}, site::StmtId)
    best = NULL_STMT
    for st in stores
        UnifiedIR.comes_before(ir, st, site) || continue
        _cell_dominates_ed(ir, st, site) || continue
        (UnifiedIR.isnull(best) || UnifiedIR.comes_before(ir, best, st)) && (best = st)
    end
    for t in stores
        t == best && continue
        UnifiedIR.comes_before(ir, t, site) || continue
        (UnifiedIR.isnull(best) || UnifiedIR.comes_before(ir, best, t)) || continue
        _may_reach(ir, t, site) && return (:ambig, NULL_STMT)
    end
    return UnifiedIR.isnull(best) ? (:none, NULL_STMT) : (:store, best)
end

# Path from a body-site's region up to the loop body region: stores may cross
# only `if` arms (their reaching values stay statically resolvable); reads may
# additionally sit inside nested loop bodies (the carried value is invariant
# there). Anything else (islands, handlers, try bodies) keeps memory form.
function _body_path_ok(ir::UnifiedIR.IR, u::StmtId, bodyr::RegionId; reads::Bool)
    r = UnifiedIR.stmt_region(ir, u)
    while r != bodyr
        reg = UnifiedIR.getregion(ir, r)
        okkind = reg.kind === UnifiedIR.REGION_ARM ||
                 (reads && reg.kind === UnifiedIR.REGION_LOOP_BODY)
        okkind || return false
        reg.activation === UnifiedIR.ACT_IMMEDIATE || return false
        UnifiedIR.isnull(reg.parent) && return false
        r = reg.parent
    end
    return true
end

# Direct anchor of `g` in region `rtop` (the transitive owner that is a direct
# member of rtop), refusing handler/island/guard/deferred crossings.
function _post_anchor(ir::UnifiedIR.IR, g::StmtId, rtop::RegionId)
    x = g
    steps = 0
    while UnifiedIR.stmt_region(ir, x) != rtop
        reg = UnifiedIR.getregion(ir, UnifiedIR.stmt_region(ir, x))
        (reg.kind === UnifiedIR.REGION_HANDLER || reg.kind === UnifiedIR.REGION_BLOCK ||
         reg.kind === UnifiedIR.REGION_GUARD) && return NULL_STMT
        reg.activation === UnifiedIR.ACT_IMMEDIATE || return NULL_STMT
        UnifiedIR.isnull(reg.owner) && return NULL_STMT
        x = reg.owner
        (steps += 1) > UnifiedIR.nregions(ir) && return NULL_STMT
    end
    return x
end

struct LoopCellPlan
    cell::StmtId
    prestores::Vector{StmtId}
    bodystores::Vector{StmtId}
    pregets::Vector{Pair{StmtId,StmtId}}      # get => reaching pre store
    bodygets::Vector{Pair{StmtId,StmtId}}     # get => reaching body store (NULL ⇒ arg)
    postgets::Vector{StmtId}
    news::Vector{StmtId}
    init::UnifiedIR.Operand
end

"""
    promote_loop_cells!(ir) -> Int

Loop-carried cell promotion (mem2reg across `loop` regions, §5.3): a frame
cell stored on the straight line before a loop and re-stored in its body
becomes a carried region arg — pre-store value → loop init operand, body
reads → the region arg (or the dominating body store's value), body values at
each exit → the `continue`/`break` carried operands, post-loop reads → the
loop's result (threaded out through enclosing `if` arms when the loop sits
under a guard, with the pre value yielded on the not-taken arms).

Soundness gates (§6): every access is classified pre / body / post relative
to ONE loop; body stores cross only `if` arms; no handler, island, or
deferred region on any access path; reaching definitions are unambiguous
(sibling-arm stores are exclusive, everything else refuses); the backedge
rule is satisfied by construction — body reads before the first dominating
body store take the carried arg, never the pre-loop store. Editable state.
"""
function promote_loop_cells!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "promote_loop_cells!")
    UnifiedIR.flush_renames!(ir)
    promoted = 0
    for L in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, L) && continue
        UnifiedIR.stmt_kind(ir, L) === K"loop" || continue
        UnifiedIR.nops(ir, L) == 0 || continue          # no existing carried values
        rs = UnifiedIR.live_owned_regions(ir, L)
        length(rs) == 1 || continue
        bodyr = rs[1]
        bodyreg = UnifiedIR.getregion(ir, bodyr)
        isempty(bodyreg.args) || continue
        promoted += _promote_cells_of_loop!(ir, L, bodyr)
    end
    return promoted
end

function _promote_cells_of_loop!(ir::UnifiedIR.IR, L::StmtId, bodyr::RegionId)
    before(a, b) = UnifiedIR.comes_before(ir, a, b)
    # --- the loop's chain up to the region holding post reads ---------------
    chain = StmtId[]                       # enclosing `if` ops, inner → outer
    rtop = UnifiedIR.stmt_region(ir, L)
    while true
        reg = UnifiedIR.getregion(ir, rtop)
        reg.activation === UnifiedIR.ACT_IMMEDIATE || return 0
        reg.kind === UnifiedIR.REGION_ARM || break
        ow = reg.owner
        UnifiedIR.stmt_kind(ir, ow) === K"if" || return 0
        push!(chain, ow)
        rtop = UnifiedIR.stmt_region(ir, ow)
    end
    top = isempty(chain) ? L : chain[end]
    # --- exits ---------------------------------------------------------------
    conts = StmtId[]; brks = StmtId[]
    for x in UnifiedIR.each_stmt(ir)
        k = UnifiedIR.stmt_kind(ir, x)
        (k === K"continue" || k === K"break") || continue
        UnifiedIR.asregion(UnifiedIR.getop(ir, x, 1)) == bodyr || continue
        _body_path_ok(ir, x, bodyr; reads = false) || return 0
        k === K"continue" ? push!(conts, x) : push!(brks, x)
    end
    all(c -> UnifiedIR.nops(ir, c) == 2, conts) || return 0
    all(b -> UnifiedIR.nops(ir, b) == 1, brks) || return 0
    anycontexit = any(c -> static_operand_value(ir, UnifiedIR.getop(ir, c, 2)) !== true,
                      conts)
    exits = vcat(conts, brks)
    counts = UnifiedIR.use_counts(ir)
    # --- per-cell analysis ----------------------------------------------------
    plans = LoopCellPlan[]
    for ci in 1:UnifiedIR.nstmts(ir)
        ir.body.kind[ci] === K"cell" || continue
        cell = StmtId(Int32(ci))
        sets = StmtId[]; gets = StmtId[]; news = StmtId[]
        ok = true
        UnifiedIR.each_ssa_use(ir) do site, used
            (ok && used == cell) || return
            site isa UnifiedIR.StmtOperand || (ok = false; return)
            u = site.user
            UnifiedIR.is_tombstone(ir, u) && return
            k = UnifiedIR.stmt_kind(ir, u)
            if k === K"cell_set" && site.opidx == 1
                push!(sets, u)
            elseif k === K"cell_get"
                push!(gets, u)
            elseif k === K"cell_new"
                push!(news, u)
            else
                ok = false        # escapes / cell_isdefined / value use
            end
        end
        ok || continue
        inbody(u) = UnifiedIR.is_ancestor(ir, bodyr, UnifiedIR.stmt_region(ir, u))
        prestores = StmtId[]; bodystores = StmtId[]
        for st in sets
            if inbody(st)
                _body_path_ok(ir, st, bodyr; reads = false) || (ok = false; break)
                push!(bodystores, st)
            elseif UnifiedIR.is_ancestor(ir, UnifiedIR.stmt_region(ir, st),
                                         UnifiedIR.stmt_region(ir, L)) && before(st, L)
                push!(prestores, st)
            else
                ok = false; break                # post/aside store: refuse
            end
        end
        (ok && !isempty(bodystores) && !isempty(prestores)) || continue
        # declaration news only, all outside the loop
        ok = all(nw -> !inbody(nw) && all(st -> before(nw, st), sets), news)
        ok || continue
        # init must be a clean reaching pre store at the loop
        kind0, init_st = _reach_ed(ir, prestores, L)
        kind0 === :store || continue
        pregets = Pair{StmtId,StmtId}[]
        bodygets = Pair{StmtId,StmtId}[]
        postgets = StmtId[]
        for g in gets
            if inbody(g)
                _body_path_ok(ir, g, bodyr; reads = true) || (ok = false; break)
                rk, rst = _reach_ed(ir, bodystores, g)
                rk === :ambig && (ok = false; break)
                push!(bodygets, g => rst)          # NULL ⇒ carried arg
            elseif before(g, L)
                rk, rst = _reach_ed(ir, prestores, g)
                rk === :store || (ok = false; break)
                # backedge hazard: a later store sharing an enclosing loop
                # reaches this read on the next iteration
                for t in sets
                    if before(g, t) && UnifiedIR.shares_loop(ir, t, g)
                        ok = false; break
                    end
                end
                ok || break
                push!(pregets, g => rst)
            else
                anchor = _post_anchor(ir, g, rtop)
                if UnifiedIR.isnull(anchor) || anchor == L || any(==(anchor), chain) ||
                   !before(top, anchor)
                    ok = false
                    break
                end
                push!(postgets, g)
            end
        end
        ok || continue
        # exit-site values must be resolvable
        for x in exits
            rk, _ = _reach_ed(ir, bodystores, x)
            rk === :ambig && (ok = false; break)
        end
        ok || continue
        push!(plans, LoopCellPlan(cell, prestores, bodystores, pregets, bodygets,
                                  postgets, news,
                                  UnifiedIR.getop(ir, init_st, 2)))
    end
    isempty(plans) && return 0
    # --- result set and chain validation -------------------------------------
    R = [i for (i, p) in enumerate(plans) if !isempty(p.postgets)]
    if !isempty(R)
        chainok = true
        for ifop in chain
            counts[ifop.id] == 0 || (chainok = false; break)
            arms = UnifiedIR.live_owned_regions(ir, ifop)
            1 <= length(arms) <= 2 || (chainok = false; break)
            carm = findfirst(a -> UnifiedIR.is_ancestor(ir, a, UnifiedIR.stmt_region(ir, L)), arms)
            carm === nothing && (chainok = false; break)
            ct = UnifiedIR.region_terminator(ir, arms[carm])
            (ct !== nothing && UnifiedIR.stmt_kind(ir, ct) === K"yield" &&
             UnifiedIR.nops(ir, ct) == 0) || (chainok = false; break)
            for (ai, arm) in enumerate(arms)
                ai == carm && continue
                ot = UnifiedIR.region_terminator(ir, arm)
                ot === nothing && (chainok = false; break)
                otk = UnifiedIR.stmt_kind(ir, ot)
                (otk === K"yield" && UnifiedIR.nops(ir, ot) != 0) && (chainok = false; break)
                (otk === K"yield" || is_diverge_kind(otk)) || (chainok = false; break)
            end
            chainok || break
        end
        if !chainok
            # cannot thread results out: drop the post-reading cells
            keep = [p for (i, p) in enumerate(plans) if !(i in R)]
            plans = keep
            R = Int[]
        end
        isempty(plans) && return 0
        # also: the not-taken value at every chain yield must resolve
        if !isempty(R)
            anycontexit && (R = collect(1:length(plans)))
            for ifop in chain, p in plans[R]
                rk, _ = _reach_ed(ir, p.prestores, ifop)
                rk === :store || return 0
            end
        end
    end
    k = length(plans)
    # --- rewrite --------------------------------------------------------------
    bodyreg = UnifiedIR.getregion(ir, bodyr)
    firstm = StmtId(bodyreg.first.id)
    args = StmtId[]
    for _ in 1:k
        a = UnifiedIR.insert_before!(ir, firstm, K"region_arg"; type = Any)
        push!(args, a)
        push!(bodyreg.args, a)
    end
    UnifiedIR.store_ops!(ir, L, UnifiedIR.Operand[p.init for p in plans])
    valat(p, i, site) = begin
        rk, rst = _reach_ed(ir, p.bodystores, site)
        rk === :store ? UnifiedIR.getop(ir, rst, 2) : UnifiedIR.op_stmt(args[i])
    end
    for (i, p) in enumerate(plans)
        for (g, rst) in p.bodygets
            v = UnifiedIR.isnull(rst) ? UnifiedIR.op_stmt(args[i]) : UnifiedIR.getop(ir, rst, 2)
            UnifiedIR.replace_uses_where!(_ -> true, ir, g => v)
            UnifiedIR.delete_stmt!(ir, g)
        end
        for (g, rst) in p.pregets
            UnifiedIR.replace_uses_where!(_ -> true, ir, g => UnifiedIR.getop(ir, rst, 2))
            UnifiedIR.delete_stmt!(ir, g)
        end
    end
    for c in conts
        vals = UnifiedIR.Operand[valat(p, i, c) for (i, p) in enumerate(plans)]
        UnifiedIR.replace_stmt!(ir, c, K"continue", UnifiedIR.getop(ir, c, 1),
                                UnifiedIR.getop(ir, c, 2), vals...)
    end
    if !isempty(R)
        for b in brks
            vals = UnifiedIR.Operand[valat(plans[i], i, b) for i in R]
            UnifiedIR.replace_stmt!(ir, b, K"break", UnifiedIR.getop(ir, b, 1), vals...)
        end
        # thread the exit values out through the chain
        UnifiedIR.set_type!(ir, L, Any)
        curvals = UnifiedIR.Operand[]
        if length(R) == 1
            push!(curvals, UnifiedIR.op_stmt(L))
        else
            anchor = L
            for j in 1:length(R)
                e = UnifiedIR.insert_after!(ir, anchor, K"extract", UnifiedIR.op_stmt(L),
                                            UnifiedIR.op_inline(j - 1); type = Any)
                push!(curvals, UnifiedIR.op_stmt(e))
                anchor = e
            end
        end
        for ifop in chain
            arms = UnifiedIR.live_owned_regions(ir, ifop)
            carm = findfirst(a -> UnifiedIR.is_ancestor(ir, a, UnifiedIR.stmt_region(ir, L)), arms)
            ct = UnifiedIR.region_terminator(ir, arms[carm])
            UnifiedIR.replace_stmt!(ir, ct, K"yield", curvals...)
            prevals = UnifiedIR.Operand[]
            for i in R
                _, pst = _reach_ed(ir, plans[i].prestores, ifop)
                push!(prevals, UnifiedIR.getop(ir, pst, 2))
            end
            handled_else = false
            for (ai, arm) in enumerate(arms)
                ai == carm && continue
                ot = UnifiedIR.region_terminator(ir, arm)
                if UnifiedIR.stmt_kind(ir, ot) === K"yield"
                    UnifiedIR.replace_stmt!(ir, ot, K"yield", prevals...)
                end
                handled_else = true
            end
            if !handled_else && length(arms) == 1
                er = UnifiedIR.new_region!(ir, ifop, UnifiedIR.REGION_ARM)
                UnifiedIR.push_stmt!(ir, er, K"yield", prevals...)
            end
            UnifiedIR.set_type!(ir, ifop, Any)
            newvals = UnifiedIR.Operand[]
            if length(R) == 1
                push!(newvals, UnifiedIR.op_stmt(ifop))
            else
                anchor = ifop
                for j in 1:length(R)
                    e = UnifiedIR.insert_after!(ir, anchor, K"extract", UnifiedIR.op_stmt(ifop),
                                                UnifiedIR.op_inline(j - 1); type = Any)
                    push!(newvals, UnifiedIR.op_stmt(e))
                    anchor = e
                end
            end
            curvals = newvals
        end
        for (ridx, i) in enumerate(R)
            for g in plans[i].postgets
                UnifiedIR.replace_uses_where!(_ -> true, ir, g => curvals[ridx])
                UnifiedIR.delete_stmt!(ir, g)
            end
        end
    end
    for p in plans
        for st in p.prestores
            UnifiedIR.delete_stmt!(ir, st)
        end
        for st in p.bodystores
            UnifiedIR.delete_stmt!(ir, st)
        end
        for nw in p.news
            UnifiedIR.delete_stmt!(ir, nw)
        end
    end
    counts2 = UnifiedIR.use_counts(ir)
    for p in plans
        counts2[p.cell.id] == 0 && UnifiedIR.delete_stmt!(ir, p.cell)
    end
    return length(plans)
end

"""
    sroa_mutables!(ir) -> Int

Mutable-struct SROA (§10.4 / stock `sroa_mutables!` cases): fully-initialized
`new` of a concrete mutable struct whose value never escapes — every use is
`extract`/`getfield(it, const fld)` or `setfield!(it, const fld, v)` — is
replaced by per-field frame cells. Loads become `cell_get`, stores become
`cell_set` (+ a `refine` carrying setfield!'s value result). Editable state;
`promote_cells!`/`dce!` finish the job on the next dense round.
"""
function sroa_mutables!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "sroa_mutables!")
    promoted = 0
    for s in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) === K"new" || continue
        T = concrete_datatype(stmt_lattice(ir, UnifiedIR.getop(ir, s, 1)))
        (T isa DataType && ismutabletype(T)) || continue
        nf = fieldcount(T)
        UnifiedIR.nops(ir, s) - 1 == nf || continue   # fully-initialized only (v1)
        any(i -> Base.isfieldatomic(T, i), 1:nf) && continue
        # inside a cfg island the replacement cells could never promote
        # (promote_cells! §6 policy refuses island cells) — a pure
        # pessimization; leave the allocation in memory form there
        UnifiedIR.inside_island(ir, s) && continue
        # collect uses; any non-load/store use disqualifies (escape check)
        loads = Tuple{StmtId,Int}[]
        stores = Tuple{StmtId,Int}[]
        ok = true
        UnifiedIR.each_ssa_use(ir) do site, used
            (ok && used == s) || return
            site isa UnifiedIR.StmtOperand || (ok = false; return)
            u = site.user
            UnifiedIR.is_tombstone(ir, u) && return
            uk = UnifiedIR.stmt_kind(ir, u)
            if uk === K"extract" && site.opidx == 1
                idx = Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, u, 2))::Int64) + 1
                1 <= idx <= nf ? push!(loads, (u, idx)) : (ok = false)
            elseif uk === K"call"
                callee = static_operand_value(ir, UnifiedIR.getop(ir, u, 1))
                nopu = UnifiedIR.nops(ir, u)
                if (callee === Core.getfield || callee === Base.getfield) &&
                   site.opidx == 2 && (nopu == 3 || nopu == 4)
                    fld = field_index_of(T, static_operand_value(ir, UnifiedIR.getop(ir, u, 3)))
                    fld === nothing ? (ok = false) : push!(loads, (u, fld))
                elseif (callee === Core.setfield! || callee === Base.setfield!) &&
                       site.opidx == 2 && nopu == 4
                    fld = field_index_of(T, static_operand_value(ir, UnifiedIR.getop(ir, u, 3)))
                    fld === nothing ? (ok = false) : push!(stores, (u, fld))
                else
                    ok = false
                end
            else
                ok = false   # cell_set, return, yield, phi-ish, nested call arg, …
            end
        end
        ok || continue
        # rewrite: per-field cells + initial stores, placed just before the new
        cells = StmtId[]
        for i in 1:nf
            ft = fieldtype(T, i)
            c = UnifiedIR.insert_before!(ir, s, K"cell", UnifiedIR.vop(ir, ft); type = ft)
            push!(cells, c)
            UnifiedIR.insert_before!(ir, s, K"cell_set", UnifiedIR.op_stmt(c),
                                     UnifiedIR.getop(ir, s, i + 1))
        end
        for (u, fld) in loads
            UnifiedIR.replace_stmt!(ir, u, K"cell_get", UnifiedIR.op_stmt(cells[fld]);
                                    type = UnifiedIR.stmt_type(ir, u))
        end
        for (u, fld) in stores
            vo = UnifiedIR.getop(ir, u, 4)
            UnifiedIR.insert_before!(ir, u, K"cell_set", UnifiedIR.op_stmt(cells[fld]), vo)
            # setfield! evaluates to the stored value; keep that result shape
            UnifiedIR.replace_stmt!(ir, u, K"refine", vo; type = UnifiedIR.stmt_type(ir, u))
        end
        UnifiedIR.kill_stmt!(ir, s)
        promoted += 1
    end
    return promoted
end
