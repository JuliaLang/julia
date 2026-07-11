# SROA on UnifiedIR (§10.4; stock reference: Compiler/src/ssair/passes.jl
# `sroa_pass!`/`sroa_mutables!` — the CASES, not the mechanics):
#
#   1. Immutable-struct SROA: `extract` (canonicalized getfield) of a locally
#      constructed `Core.tuple`/`K"new"` of a concrete immutable type forwards
#      the field value, following `refine` chains; legality of the forwarded
#      operand at the use site is checked with `UnifiedIR.visible`.
#      (Extension of `forward_extracts!`, which lives in optimize.jl.)
#   2. If-result forwarding: an `if` whose live arms all produce the same
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

`if` ops (two live arms) whose arms each produce exactly one result operand and all
produce the *same* operand forward it to the result's uses. Statement operands
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
            (t !== nothing && UnifiedIR.stmt_kind(ir, t) === K"result" &&
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
function _may_reach(ir::UnifiedIR.IR, t::StmtId, site::StmtId; iteration_local::Bool = false)
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
            i == 1 && return true
            # PER-ITERATION reach only (the loop pass, where the carried arg
            # covers backedge flow): a diverging terminator on the store's
            # side of the chain cuts fall-through — the value cannot reach a
            # later `site` within this activation of the common region.
            # MEMORY reach (everyone else) must keep the store visible: the
            # diverged store persists in the cell across backedges.
            if iteration_local
                for j in 1:i-1
                    tt = UnifiedIR.region_terminator(ir, A[j])
                    tt === nothing && continue
                    is_diverge_kind(UnifiedIR.stmt_kind(ir, tt)) && return false
                end
            end
            UnifiedIR.isnull(prev) && return true
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
function _reach_ed(ir::UnifiedIR.IR, stores::Vector{StmtId}, site::StmtId;
                   iteration_local::Bool = false)
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
        _may_reach(ir, t, site; iteration_local) && return (:ambig, NULL_STMT)
    end
    return UnifiedIR.isnull(best) ? (:none, NULL_STMT) : (:store, best)
end

# §6 throw-edge rule for store motion: when `anchor` (the region being
# rewritten — an if, loop, or cfg op) lies inside a `try` body while the cell
# has uses outside that try, a swallowed exception exposes the cell's
# mid-try memory to those outside readers — deleting or moving the stores
# would change what they observe. Refuse such cells wholesale.
function _sink_crosses_try(ir::UnifiedIR.IR, anchor::StmtId, uses)
    r = UnifiedIR.stmt_region(ir, anchor)
    while !UnifiedIR.isnull(r)
        reg = UnifiedIR.getregion(ir, r)
        own = reg.owner
        if !UnifiedIR.isnull(own) && UnifiedIR.stmt_kind(ir, own) === K"try" &&
           reg.kind !== UnifiedIR.REGION_HANDLER
            for u in uses
                UnifiedIR.is_ancestor(ir, r, UnifiedIR.stmt_region(ir, u)) ||
                    return true
            end
        end
        r = UnifiedIR.isnull(own) ? UnifiedIR.NULL_REGION :
            UnifiedIR.stmt_region(ir, own)
    end
    return false
end

# The innermost loop body on `t`'s region chain that contains `site` — the
# tightest backedge that can carry t's stored value around to `site` on a
# later iteration. NULL when they share no loop.
function _innermost_shared_body(ir::UnifiedIR.IR, t::StmtId, site::StmtId)
    r = UnifiedIR.stmt_region(ir, t)
    while !UnifiedIR.isnull(r)
        reg = UnifiedIR.getregion(ir, r)
        if reg.kind === UnifiedIR.REGION_LOOP_BODY &&
           UnifiedIR.is_ancestor(ir, r, UnifiedIR.stmt_region(ir, site))
            return r
        end
        r = reg.parent
    end
    return UnifiedIR.NULL_REGION
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
    # STORE-SINKING mode: instead of rewriting post-loop reads to threaded
    # exit values, one unconditional `cell_set` of the exit value lands right
    # after the loop; post-loop uses stay memory ops. This is the
    # compositional half of multi-level exit threading: the next fixpoint
    # round sees an ordinary store at the enclosing level (arm stores sink
    # the same way). Chosen when post-loop reads cannot anchor for direct
    # threading or post-loop stores exist; pre-loop stores are then KEPT
    # (paths that skip the loop must still find the incoming value in
    # memory).
    sink::Bool
end

"""
    promote_loop_cells!(ir) -> Int

Loop-carried cell promotion (mem2reg across `loop` regions, §5.3): a frame
cell stored on the straight line before a loop and re-stored in its body
becomes a carried region arg — pre-store value → loop init operand, body
reads → the region arg (or the dominating body store's value), body values at
each exit → the `continue`/`break` carried operands, post-loop reads → the
loop's result (threaded out through enclosing `if` arms when the loop sits
under a guard, with the pre value produced on the not-taken arms).

Soundness gates (§6): every access is classified pre / body / post relative
to ONE loop; body stores cross only `if` arms; no handler, island, or
deferred region on any access path; reaching definitions are unambiguous
(sibling-arm stores are exclusive, everything else refuses); the backedge
rule is satisfied by construction — body reads before the first dominating
body store take the carried arg, never the pre-loop store. Editable state.
"""
# Optional placement trace for the completeness harness (completeness.jl):
# when set, the promotion passes record every join-value placement as
# (kind, anchor stmt id, cell stmt id) with kind ∈ (:if_join, :loop_header,
# :loop_break, :if_thread) — :if_thread marks structural value threading
# through enclosing arms (region-form plumbing with no classical-phi
# counterpart; expected to be "extra" relative to iterated-DF placement).
const PROMOTION_TRACE = Ref{Union{Nothing,Vector{Tuple{Symbol,Int,Int}}}}(nothing)
@inline _trace!(kind::Symbol, anchor::StmtId, cell::StmtId) =
    (t = PROMOTION_TRACE[]; t === nothing || push!(t, (kind, Int(anchor.id), Int(cell.id))); nothing)
# :island_phi anchors are REGION ids (the phi block), not stmt ids — the
# harness back-translates them through RemapSet.region instead of .stmt
@inline _trace!(kind::Symbol, anchor::UnifiedIR.RegionId, cell::StmtId) =
    (t = PROMOTION_TRACE[]; t === nothing || push!(t, (kind, Int(anchor.id), Int(cell.id))); nothing)

function promote_loop_cells!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "promote_loop_cells!")
    UnifiedIR.flush_renames!(ir)
    promoted = 0
    for L in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, L) && continue
        UnifiedIR.stmt_kind(ir, L) === K"loop" || continue
        rs = UnifiedIR.live_owned_regions(ir, L)
        length(rs) == 1 || continue
        # loops that already carry values (from an earlier round of this pass
        # or from island exit threading) compose: new args/inits/continue
        # values APPEND after the existing ones
        UnifiedIR.nops(ir, L) == length(UnifiedIR.getregion(ir, rs[1]).args) || continue
        promoted += _promote_cells_of_loop!(ir, L, rs[1])
    end
    return promoted
end

function _promote_cells_of_loop!(ir::UnifiedIR.IR, L::StmtId, bodyr::RegionId)
    before(a, b) = UnifiedIR.comes_before(ir, a, b)
    bodyreg0 = UnifiedIR.getregion(ir, bodyr)
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
    # continues carry cond + one value per existing arg; breaks carry the
    # loop's existing result. With existing args only pure carried promotion
    # runs (no break rewriting), so the break arity is unconstrained then.
    nexist0 = length(bodyreg0.args)
    all(c -> UnifiedIR.nops(ir, c) == 2 + nexist0, conts) || return 0
    (nexist0 > 0 || all(b -> UnifiedIR.nops(ir, b) == 1, brks)) || return 0
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
        _sink_crosses_try(ir, L, Iterators.flatten((sets, gets, news))) && continue
        inbody(u) = UnifiedIR.is_ancestor(ir, bodyr, UnifiedIR.stmt_region(ir, u))
        prestores = StmtId[]; bodystores = StmtId[]
        sink = false
        for st in sets
            if inbody(st)
                _body_path_ok(ir, st, bodyr; reads = false) || (ok = false; break)
                push!(bodystores, st)
            elseif UnifiedIR.is_ancestor(ir, UnifiedIR.stmt_region(ir, st),
                                         UnifiedIR.stmt_region(ir, L)) && before(st, L)
                push!(prestores, st)
            elseif before(L, st)
                sink = true                      # post store: sink the exit value
            else
                ok = false; break                # aside store (may reach L): refuse
            end
        end
        (ok && !isempty(bodystores) && !isempty(prestores)) || continue
        # declaration news only, all outside the loop
        ok = all(nw -> !inbody(nw) && all(st -> before(nw, st), sets), news)
        ok || continue
        # init must be a clean reaching pre store at the loop
        kind0, init_st = _reach_ed(ir, prestores, L)
        kind0 === :store || continue
        # init backedge hazard (site = the loop op): a store sharing an
        # enclosing loop with L reaches L across that backedge, making the
        # region-static init stale from iteration 2 on — unless the init
        # re-executes inside the INNERMOST loop that store shares with L
        # (then every such backedge re-runs the init before L; an init
        # inside the innermost is inside every outer one too)
        hz = false
        for t in sets
            t == init_st && continue
            X = _innermost_shared_body(ir, t, L)
            UnifiedIR.isnull(X) && continue
            UnifiedIR.is_ancestor(ir, X, UnifiedIR.stmt_region(ir, init_st)) ||
                (hz = true; break)
        end
        hz && continue
        pregets = Pair{StmtId,StmtId}[]
        bodygets = Pair{StmtId,StmtId}[]
        postgets = StmtId[]
        for g in gets
            if inbody(g)
                _body_path_ok(ir, g, bodyr; reads = true) || (ok = false; break)
                rk, rst = _reach_ed(ir, bodystores, g; iteration_local = true)
                rk === :ambig && (ok = false; break)
                push!(bodygets, g => rst)          # NULL ⇒ carried arg
            elseif before(g, L)
                rk, rst = _reach_ed(ir, prestores, g)
                rk === :store || (ok = false; break)
                # backedge hazard: a later store sharing an enclosing loop
                # reaches this read on the next iteration — unless the
                # reaching store re-executes inside the INNERMOST loop that
                # store shares with the read (then every such backedge
                # re-runs it before the read, shadowing the carried value)
                for t in sets
                    before(g, t) || continue
                    X = _innermost_shared_body(ir, t, g)
                    UnifiedIR.isnull(X) && continue
                    UnifiedIR.is_ancestor(ir, X, UnifiedIR.stmt_region(ir, rst)) ||
                        (ok = false; break)
                end
                ok || break
                push!(pregets, g => rst)
            else
                anchor = _post_anchor(ir, g, rtop)
                if UnifiedIR.isnull(anchor) || anchor == L || any(==(anchor), chain) ||
                   !before(top, anchor)
                    sink = true                  # cannot thread directly: sink
                else
                    push!(postgets, g)
                end
            end
        end
        ok || continue
        sink && empty!(postgets)                 # sink mode: post reads stay memory
        # exit-site values must be resolvable (per-iteration: the carried
        # arg covers the paths no body store falls through to)
        for x in exits
            rk, _ = _reach_ed(ir, bodystores, x; iteration_local = true)
            rk === :ambig && (ok = false; break)
        end
        ok || continue
        push!(plans, LoopCellPlan(cell, prestores, bodystores, pregets, bodygets,
                                  postgets, news,
                                  UnifiedIR.getop(ir, init_st, 2), sink))
    end
    isempty(plans) && return 0
    # loops already carrying values (earlier rounds / island threading) only
    # take pure carried cells: exit values would have to renumber the
    # existing result tuple
    # sinking composes with chain nesting by NOT threading through the chain
    # (the sunk store is ordinary memory there); mixing chain threading and
    # sinking in one tuple would need not-taken values for sunk slots, so a
    # chain demotes everything to sinking
    if !isempty(chain) && any(p -> p.sink, plans) &&
       any(p -> !isempty(p.postgets), plans)
        plans = [p.sink ? p :
                 LoopCellPlan(p.cell, p.prestores, p.bodystores, p.pregets,
                              p.bodygets, StmtId[], p.news, p.init,
                              !isempty(p.postgets)) for p in plans]
    end
    # --- result set and chain validation -------------------------------------
    R = [i for (i, p) in enumerate(plans) if !isempty(p.postgets) || p.sink]
    # exit-value threading repurposes the loop's result; a result some user
    # already consumes must keep its shape (carried-only promotion is fine:
    # only the continues change, and their exit binding was never read)
    if !isempty(R) && counts[L.id] != 0
        plans = [p for p in plans if isempty(p.postgets) && !p.sink]
        isempty(plans) && return 0
        R = Int[]
    end
    if !isempty(R)
        chainok = true
        for ifop in chain
            counts[ifop.id] == 0 || (chainok = false; break)
            arms = UnifiedIR.live_owned_regions(ir, ifop)
            1 <= length(arms) <= 2 || (chainok = false; break)
            carm = findfirst(a -> UnifiedIR.is_ancestor(ir, a, UnifiedIR.stmt_region(ir, L)), arms)
            carm === nothing && (chainok = false; break)
            ct = UnifiedIR.region_terminator(ir, arms[carm])
            (ct !== nothing && UnifiedIR.stmt_kind(ir, ct) === K"result" &&
             UnifiedIR.nops(ir, ct) == 0) || (chainok = false; break)
            for (ai, arm) in enumerate(arms)
                ai == carm && continue
                ot = UnifiedIR.region_terminator(ir, arm)
                ot === nothing && (chainok = false; break)
                otk = UnifiedIR.stmt_kind(ir, ot)
                (otk === K"result" && UnifiedIR.nops(ir, ot) != 0) && (chainok = false; break)
                (otk === K"result" || is_diverge_kind(otk)) || (chainok = false; break)
            end
            chainok || break
        end
        if !chainok
            # cannot thread results through the chain: sink instead
            plans = [LoopCellPlan(p.cell, p.prestores, p.bodystores, p.pregets,
                                  p.bodygets, StmtId[], p.news, p.init,
                                  p.sink || !isempty(p.postgets)) for p in plans]
            R = [i for (i, p) in enumerate(plans) if p.sink]
        end
        isempty(plans) && return 0
        if !isempty(R)
            anycontexit && (R = collect(1:length(plans)))
            # chain threading (direct plans only) needs the not-taken value
            # at every chain result; sunk cells keep their pre stores in
            # memory instead
            if any(i -> !isempty(plans[i].postgets), R)
                for ifop in chain, p in plans[R]
                    rk, _ = _reach_ed(ir, p.prestores, ifop)
                    rk === :store || return 0
                end
            end
        end
    end
    k = length(plans)
    # --- rewrite --------------------------------------------------------------
    bodyreg = UnifiedIR.getregion(ir, bodyr)
    for p in plans
        _trace!(:loop_header, L, p.cell)
    end
    for i in R
        _trace!(:loop_break, L, plans[i].cell)
    end
    bms = UnifiedIR.region_stmts(ir, bodyr)
    firstm = bms[length(bodyreg.args) + 1]     # first non-arg member
    args = StmtId[]
    for _ in 1:k
        a = UnifiedIR.insert_before!(ir, firstm, K"region_arg"; type = Any)
        push!(args, a)
        push!(bodyreg.args, a)
    end
    newops = UnifiedIR.Operand[UnifiedIR.getop(ir, L, i) for i in 1:UnifiedIR.nops(ir, L)]
    append!(newops, UnifiedIR.Operand[p.init for p in plans])
    UnifiedIR.store_ops!(ir, L, newops)
    valat(p, i, site) = begin
        rk, rst = _reach_ed(ir, p.bodystores, site; iteration_local = true)
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
        keep = UnifiedIR.Operand[UnifiedIR.getop(ir, c, i) for i in 1:UnifiedIR.nops(ir, c)]
        UnifiedIR.replace_stmt!(ir, c, K"continue", keep..., vals...)
    end
    if !isempty(R)
        # exit-tuple base: result slots already claimed by an earlier
        # promotion of this loop. On continue-exits the result is the FULL
        # carried tuple (existing args lead), so new values index past them;
        # breaks keep their existing values, padded up to the base with
        # `nothing` (nothing consumes the pure-carried lead slots)
        base = anycontexit ? nexist0 :
               (isempty(brks) ? nexist0 : UnifiedIR.nops(ir, first(brks)) - 1)
        all(b -> UnifiedIR.nops(ir, b) == 1 + base,
            (anycontexit || isempty(brks)) ? StmtId[] : brks) || return 0
        for b in brks
            vals = UnifiedIR.Operand[valat(plans[i], i, b) for i in R]
            keep = UnifiedIR.Operand[UnifiedIR.getop(ir, b, i)
                                     for i in 1:UnifiedIR.nops(ir, b)]
            npad = 1 + base - length(keep)
            npad >= 0 || return 0
            pad = UnifiedIR.Operand[UnifiedIR.vop(ir, nothing) for _ in 1:npad]
            UnifiedIR.replace_stmt!(ir, b, K"break", keep..., pad..., vals...)
        end
        # materialize the exit values right after the loop
        UnifiedIR.set_type!(ir, L, Any)
        curvals = UnifiedIR.Operand[]
        anchor = L
        if base == 0 && length(R) == 1
            push!(curvals, UnifiedIR.op_stmt(L))
        else
            for j in 1:length(R)
                e = UnifiedIR.insert_after!(ir, anchor, K"extract", UnifiedIR.op_stmt(L),
                                            UnifiedIR.op_inline(base + j); type = Any)
                push!(curvals, UnifiedIR.op_stmt(e))
                anchor = e
            end
        end
        # sunk cells: one unconditional store of the exit value, in program
        # order right after the loop (post uses keep reading memory)
        for (ridx, i) in enumerate(R)
            plans[i].sink || continue
            anchor = UnifiedIR.insert_after!(ir, anchor, K"cell_set",
                                             UnifiedIR.op_stmt(plans[i].cell),
                                             curvals[ridx]; type = Nothing)
        end
        dodirect = any(i -> !isempty(plans[i].postgets), R)
        for ifop in (dodirect ? chain : StmtId[])
            for i in R
                _trace!(:if_thread, ifop, plans[i].cell)
            end
            arms = UnifiedIR.live_owned_regions(ir, ifop)
            carm = findfirst(a -> UnifiedIR.is_ancestor(ir, a, UnifiedIR.stmt_region(ir, L)), arms)
            ct = UnifiedIR.region_terminator(ir, arms[carm])
            UnifiedIR.replace_stmt!(ir, ct, K"result", curvals...)
            prevals = UnifiedIR.Operand[]
            for i in R
                _, pst = _reach_ed(ir, plans[i].prestores, ifop)
                push!(prevals, UnifiedIR.getop(ir, pst, 2))
            end
            handled_else = false
            for (ai, arm) in enumerate(arms)
                ai == carm && continue
                ot = UnifiedIR.region_terminator(ir, arm)
                if UnifiedIR.stmt_kind(ir, ot) === K"result"
                    UnifiedIR.replace_stmt!(ir, ot, K"result", prevals...)
                end
                handled_else = true
            end
            if !handled_else && length(arms) == 1
                er = UnifiedIR.new_region!(ir, ifop, UnifiedIR.REGION_ARM)
                UnifiedIR.push_stmt!(ir, er, K"result", prevals...)
            end
            UnifiedIR.set_type!(ir, ifop, Any)
            newvals = UnifiedIR.Operand[]
            if length(R) == 1
                push!(newvals, UnifiedIR.op_stmt(ifop))
            else
                anchor = ifop
                for j in 1:length(R)
                    e = UnifiedIR.insert_after!(ir, anchor, K"extract", UnifiedIR.op_stmt(ifop),
                                                UnifiedIR.op_inline(j); type = Any)
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
        if !p.sink
            for st in p.prestores
                UnifiedIR.delete_stmt!(ir, st)
            end
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
                idx = Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, u, 2))::Int64)
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
                ok = false   # cell_set, return, result, phi-ish, nested call arg, …
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

# ---------------------------------------------------------------------------
# Arm-join cell promotion (§6 / docs "Join completeness"): the if-join case
# of mem2reg. A frame cell stored in sibling arms of an `if` has its
# CONDITIONAL arm stores replaced by ONE UNCONDITIONAL `cell_set` right
# after the join: every joining arm's `result` carries the arm's outgoing
# value — its last direct store's value, or the cell's incoming value
# (materialized as a `cell_get` just before the `if`) for joining arms that
# do not store — and the post-join store takes the `if`'s (possibly tupled;
# extract-indexed) result. This is store SINKING, not full promotion: reads
# are left as `cell_get`s, and the now-unconditional store is exactly what
# `promote_cells!` (dominating case) and `promote_loop_cells!` (carried
# case) consume, so the joint fixpoint composes these into complete mem2reg
# over nested joins (`a, b = b, a` under `if` — the gcd shape).
#
# Soundness rules (§6 inviolable):
#   - frame-class `K"cell"` only; escaped/value-used cells refused;
#   - cells with any get/isdefined inside a handler region stay memory-form
#     (throw-edge observability — and the sink moves stores later, which a
#     handler could otherwise observe);
#   - cells touched inside cfg islands refused (island mem2reg's domain);
#   - the incoming `cell_get` is only inserted under definite assignment
#     (some store dominates the `if`); otherwise the cell keeps memory form
#     (maybe-undef: the get could observe undef on a path where the original
#     program read nothing);
#   - stores in EXITING arms (break/continue/return/unreachable) are left
#     untouched: those paths never reach the join, and their effect
#     legitimately flows out through memory (multi-level exits need no
#     refusal under the sinking formulation);
#   - stores nested DEEPER than the arm itself (under an inner if/loop/try)
#     are not "the arm's outgoing value": the cell is skipped at this `if`
#     until inner promotion (inside-out order + the fixpoint) flattens them;
#   - in-arm gets/isdefineds reached by a direct arm store are rewritten to
#     the store's value / `true` (the store is about to be deleted);
#   - `cell_new` only in declaration position (before every store), as in
#     `promote_cells!`.

"Is `s` (or any region on its ancestry) inside a handler region?"
function _in_handler(ir::UnifiedIR.IR, s::StmtId)
    r = UnifiedIR.stmt_region(ir, s)
    while !UnifiedIR.isnull(r)
        reg = UnifiedIR.getregion(ir, r)
        reg.kind === UnifiedIR.REGION_HANDLER && return true
        r = reg.parent
    end
    return false
end

"""
    promote_arm_cells!(ir) -> Int

Sink conditional sibling-arm `cell_set`s to a single unconditional post-join
store carried through the `if`'s results (see the block comment above).
Processes `if`s inside-out; run in the joint promotion fixpoint with
`promote_cells!` and `promote_loop_cells!`. Editable state. Returns the
number of (cell, if) sinkings performed.
"""
function promote_arm_cells!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "promote_arm_cells!")
    UnifiedIR.flush_renames!(ir)
    ifs = StmtId[]
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) === K"if" || continue
        push!(ifs, s)
    end
    # inside-out: deepest region first, so an inner if's post-join store is a
    # direct arm store by the time its enclosing if is processed
    sort!(ifs; by = s -> UnifiedIR.region_depth(ir, UnifiedIR.stmt_region(ir, s)), rev = true)
    total = 0
    for I in ifs
        total += _promote_arm_cells_at!(ir, I)
    end
    UnifiedIR.flush_renames!(ir)
    return total
end

function _promote_arm_cells_at!(ir::UnifiedIR.IR, I::StmtId)
    arms = UnifiedIR.live_owned_regions(ir, I)
    (1 <= length(arms) <= 2) || return 0
    joins = RegionId[]
    for a in arms
        t = UnifiedIR.region_terminator(ir, a)
        t === nothing && return 0
        tk = UnifiedIR.stmt_kind(ir, t)
        if tk === K"result"
            push!(joins, a)
        elseif !is_diverge_kind(tk)
            return 0
        end
    end
    isempty(joins) && return 0            # no arm reaches the join
    # a joining arm can still LEAK mid-arm through a nested sealed exit
    # (continue/break/return under a nested if): executions taking that path
    # ran the arm's stores but never reach the join, so the sunk post-join
    # store would not fire for them — such arms keep their stores in memory
    # (the post-join store then merely re-stores the same value on the join
    # path)
    leaky = Set{Int32}()
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.is_tombstone(ir, s) && continue
        k = UnifiedIR.stmt_kind(ir, s)
        # only exits that leave the arm while staying in the frame leak:
        # a continue/break to a loop INSIDE the arm is arm-internal, and a
        # return/unreachable ends the frame (no later observer of the cell)
        (k === K"continue" || k === K"break") || continue
        tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, s, 1))
        for a in joins
            if UnifiedIR.stmt_region(ir, s) != a &&
               UnifiedIR.is_ancestor(ir, a, UnifiedIR.stmt_region(ir, s)) &&
               !UnifiedIR.is_ancestor(ir, a, tgt)
                push!(leaky, a.id)
            end
        end
    end
    exist_arity = UnifiedIR.nops(ir, UnifiedIR.region_terminator(ir, joins[1]))
    all(a -> UnifiedIR.nops(ir, UnifiedIR.region_terminator(ir, a)) == exist_arity, joins) ||
        return 0
    onearmed = length(arms) == 1
    counts = UnifiedIR.use_counts(ir)
    # synthesizing an else arm (or re-tupling an in-use single result) only
    # supported for the common shapes; a one-armed if with existing results
    # has not-taken-value semantics we must not disturb
    onearmed && (exist_arity != 0 || counts[I.id] != 0) && return 0
    exist_arity == 0 && counts[I.id] != 0 && return 0

    before(a, b) = UnifiedIR.comes_before(ir, a, b)
    injoinarm(s) = any(a -> UnifiedIR.is_ancestor(ir, a, UnifiedIR.stmt_region(ir, s)), joins)

    # ---- candidate cells ----------------------------------------------------
    plans = Vector{Tuple{StmtId,                 # cell
                         Vector{StmtId},          # direct arm stores (to delete)
                         Dict{RegionId,StmtId},   # joining arm -> last direct store (or absent)
                         Vector{Pair{StmtId,StmtId}},  # in-arm get -> reaching store
                         Vector{StmtId},          # in-arm isdefined -> true rewrites
                         Bool}}()                 # needs incoming value
    for c in UnifiedIR.each_stmt(ir)
        UnifiedIR.is_tombstone(ir, c) && continue
        UnifiedIR.stmt_kind(ir, c) === K"cell" || continue
        cell = c
        sets = StmtId[]; gets = StmtId[]; isdefs = StmtId[]; news = StmtId[]
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
            elseif k === K"cell_isdefined"
                push!(isdefs, u)
            elseif k === K"cell_new"
                push!(news, u)
            else
                ok = false                        # escape / value use / token pairing
            end
        end
        ok || continue
        # §6: throw-edge observability keeps memory form. (Island uses are
        # fine: the analysis and transform are local to the if's region
        # subtree, and the post-join store this pass creates is exactly what
        # promote_island_cells!/promote_block_cells! consume next round.)
        any(u -> _in_handler(ir, u), gets) && continue
        any(u -> _in_handler(ir, u), isdefs) && continue
        _sink_crosses_try(ir, I, Iterators.flatten((sets, gets, isdefs))) && continue
        # cell_new in declaration position only
        all(nw -> all(st -> before(nw, st), sets), news) || continue

        armlast = Dict{RegionId,StmtId}()
        direct = StmtId[]                     # sinkable (non-leaky) arm stores
        allarm = StmtId[]                     # every direct arm store (reach)
        deeper = false
        for st in sets
            sr = UnifiedIR.stmt_region(ir, st)
            ai = findfirst(==(sr), joins)
            if ai !== nothing
                push!(allarm, st)
                # leaky arms keep their stores (mid-arm sealed exits observe
                # memory); the post-join store re-stores the same value on
                # the join path, so the arm still contributes its out below
                sr.id in leaky || push!(direct, st)
                prev = get(armlast, sr, UnifiedIR.NULL_STMT)
                (UnifiedIR.isnull(prev) || before(prev, st)) && (armlast[sr] = st)
            elseif injoinarm(st)
                deeper = true; break              # not flattened yet: wait for fixpoint
            end
            # stores in exiting arms / elsewhere: untouched, irrelevant here
        end
        (deeper || isempty(direct)) && continue

        # in-arm gets/isdefineds: rewrite those the direct arm stores reach.
        # Record the REACHING STORE, not its value: another cell's plan may
        # rewrite-and-delete the get this store's value currently names; the
        # value is re-read fresh at rewrite time (store operands are updated
        # by the earlier rewrites), which makes the transform order-invariant.
        getrw = Pair{StmtId,StmtId}[]
        isdefrw = StmtId[]
        for g in gets
            injoinarm(g) || continue
            rk, rst = _reach_ed(ir, allarm, g)
            rk === :ambig && (ok = false; break)
            rk === :store && push!(getrw, g => rst)
        end
        ok || continue
        for d in isdefs
            injoinarm(d) || continue
            rk, _ = _reach_ed(ir, allarm, d)
            rk === :ambig && (ok = false; break)
            rk === :store && push!(isdefrw, d)
        end
        ok || continue

        needs_in = onearmed || any(a -> !haskey(armlast, a), joins)
        if needs_in
            # definite assignment at the if: some store outside I dominates it
            outside = [st for st in sets if !any(a -> UnifiedIR.is_ancestor(ir, a, UnifiedIR.stmt_region(ir, st)), arms)]
            any(st -> before(st, I) && _cell_dominates_ed(ir, st, I), outside) || continue
        end
        push!(plans, (cell, direct, armlast, getrw, isdefrw, needs_in))
    end
    isempty(plans) && return 0

    # ---- transform ----------------------------------------------------------
    m = length(plans)
    for (cell, _, _, _, _, _) in plans
        _trace!(:if_join, I, cell)
    end
    # (1) in-arm reads/queries the (about to be deleted) arm stores reached —
    # FIRST, with values read fresh per rewrite (see the plan comment)
    for (_, _, _, getrw, isdefrw, _) in plans
        for (g, rst) in getrw
            v = UnifiedIR.getop(ir, rst, 2)
            UnifiedIR.replace_uses_where!(_ -> true, ir, g => v)
            UnifiedIR.delete_stmt!(ir, g)
        end
        for d in isdefrw
            UnifiedIR.replace_uses_where!(_ -> true, ir, d => UnifiedIR.op_inline(true))
            UnifiedIR.delete_stmt!(ir, d)
        end
    end
    # incoming values: one cell_get per needs-incoming cell, just before I
    incoming = Dict{Int,UnifiedIR.Operand}()
    for (i, (cell, _, _, _, _, needs_in)) in enumerate(plans)
        needs_in || continue
        g = UnifiedIR.insert_before!(ir, I, K"cell_get", UnifiedIR.op_stmt(cell); type = Any)
        incoming[i] = UnifiedIR.op_stmt(g)
    end
    # per-arm outgoing values appended to each joining result
    for a in joins
        t = UnifiedIR.region_terminator(ir, a)
        outs = UnifiedIR.Operand[]
        for (i, (_, _, armlast, _, _, _)) in enumerate(plans)
            st = get(armlast, a, UnifiedIR.NULL_STMT)
            push!(outs, UnifiedIR.isnull(st) ? incoming[i] : UnifiedIR.getop(ir, st, 2))
        end
        old = UnifiedIR.operands(ir, t)
        UnifiedIR.replace_stmt!(ir, t, K"result", old..., outs...)
    end
    if onearmed
        er = UnifiedIR.new_region!(ir, I, UnifiedIR.REGION_ARM)
        outs = UnifiedIR.Operand[incoming[i] for i in 1:m]   # needs_in guaranteed for all
        UnifiedIR.push_stmt!(ir, er, K"result", outs...)
    end
    UnifiedIR.set_type!(ir, I, Any)
    # post-join: extracts (when tupled) + the unconditional stores
    total_vals = exist_arity + m
    anchor = I
    if exist_arity == 1 && counts[I.id] != 0
        e0 = UnifiedIR.insert_after!(ir, anchor, K"extract", UnifiedIR.op_stmt(I),
                                     UnifiedIR.op_inline(1); type = Any)
        UnifiedIR.replace_uses_where!(u -> u != e0, ir, I => UnifiedIR.op_stmt(e0))
        anchor = e0
    end
    for (i, (cell, _, _, _, _, _)) in enumerate(plans)
        v = if total_vals == 1
            UnifiedIR.op_stmt(I)
        else
            e = UnifiedIR.insert_after!(ir, anchor, K"extract", UnifiedIR.op_stmt(I),
                                        UnifiedIR.op_inline(exist_arity + i); type = Any)
            anchor = e
            UnifiedIR.op_stmt(e)
        end
        anchor = UnifiedIR.insert_after!(ir, anchor, K"cell_set",
                                         UnifiedIR.op_stmt(cell), v; type = Nothing)
    end
    # delete the (now sunk) conditional arm stores (leaky-arm stores are
    # never in `direct`: mid-arm sealed exits still observe their memory)
    for (_, direct, _, _, _, _) in plans
        for st in direct
            UnifiedIR.delete_stmt!(ir, st)
        end
    end
    return m
end

# ---------------------------------------------------------------------------
# Island mem2reg (§6, the fourth join-point class: island phis)
# ---------------------------------------------------------------------------

"""
    promote_island_cells!(ir) -> Int

Pruned SSA construction for frame cells over cfg-op block graphs. A cell whose
island stores are all DIRECT block members and whose island reads/queries are
definitely assigned is rewritten to block args ("island phis", placed on the
liveness-pruned iterated dominance frontier of the store blocks) plus
straight-line reaching definitions. The incoming value is a `cell_get`
inserted before the cfg op under definite assignment (a dominating outside
store) — left for the dominating-store pass to fold, exactly like the arm
pass's incoming values.

v1 refusals (cells stay memory; the classifier keeps them `:island`):
  - island stores nested below block level (the arm/loop passes sink what is
    legal to sink; the rest is genuinely conditional at block granularity)
  - uses after the cfg op (exit threading through island results)
  - outside uses other than stores dominating the cfg op
  - reads/queries not definitely assigned (§6: maybe-undef stays memory)
  - uses under handler regions, escapes, `cell_new`, token values (§6)
  - islands with cross-island edges (sealed exits, §5.5) or edges from
    unreachable blocks into phi blocks
Editable state.
"""
function promote_island_cells!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "promote_island_cells!")
    UnifiedIR.flush_renames!(ir)
    cfgs = StmtId[]
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) === K"cfg" && push!(cfgs, s)
    end
    isempty(cfgs) && return 0
    sort!(cfgs; by = s -> UnifiedIR.region_depth(ir, UnifiedIR.stmt_region(ir, s)), rev = true)
    promoted = 0
    for I in cfgs
        UnifiedIR.is_tombstone(ir, I) && continue
        promoted += _promote_island_cells_at!(ir, I)
    end
    return promoted
end

const _EDGE_KINDS = (K"goto", K"br_if", K"switch", K"await")

# containing island block of s: (block index, direct member, crossed-handler?)
# or nothing when s is not nested inside one of the island's blocks
function _island_container(ir::UnifiedIR.IR, bidx::Dict{Int32,Int}, s::StmtId)
    cur = s
    hnd = false
    while true
        r = UnifiedIR.stmt_region(ir, cur)
        UnifiedIR.isnull(r) && return nothing
        haskey(bidx, r.id) && return (bidx[r.id], cur, hnd)
        reg = UnifiedIR.getregion(ir, r)
        reg.kind === UnifiedIR.REGION_HANDLER && (hnd = true)
        UnifiedIR.isnull(reg.owner) && return nothing
        cur = reg.owner
    end
end

# rebuild terminator t, appending `val` to every bundle whose destination
# region id is in `want` (bundle encoding as in edge_bundles, §5.5)
function _extend_bundles!(ir::UnifiedIR.IR, t::StmtId, want::Set{Int32}, val::UnifiedIR.Operand)
    k = UnifiedIR.stmt_kind(ir, t)
    old = UnifiedIR.Operand[UnifiedIR.getop(ir, t, i) for i in 1:UnifiedIR.nops(ir, t)]
    new = UnifiedIR.Operand[]
    i = 1
    function copybundle!()
        dest = old[i]; i += 1
        argc = Int(UnifiedIR.imm_value(old[i])::Int64); i += 1
        hit = UnifiedIR.asregion(dest).id in want
        push!(new, dest)
        push!(new, UnifiedIR.op_inline(Int64(argc + (hit ? 1 : 0))))
        for _ in 1:argc
            push!(new, old[i]); i += 1
        end
        hit && push!(new, val)
        nothing
    end
    if k === K"goto"
        copybundle!()
    elseif k === K"br_if"
        push!(new, old[i]); i += 1               # condition
        copybundle!(); copybundle!()
    elseif k === K"switch"
        push!(new, old[i]); i += 1               # scrutinee
        nc = Int(UnifiedIR.imm_value(old[i])::Int64)
        push!(new, old[i]); i += 1               # case count
        for _ in 1:nc
            push!(new, old[i]); i += 1           # case value
            copybundle!()
        end
        copybundle!()                             # default
    elseif k === K"await"
        push!(new, old[i]); i += 1               # flags
        copybundle!(); copybundle!()
    else
        return
    end
    UnifiedIR.store_ops!(ir, t, new)
    nothing
end

function _promote_island_cells_at!(ir::UnifiedIR.IR, I::StmtId)
    blocks = UnifiedIR.live_owned_regions(ir, I)
    n = length(blocks)
    n == 0 && return 0
    bidx = Dict{Int32,Int}(r.id => i for (i, r) in enumerate(blocks))
    # edges: (from, to, bundle-carrying stmt, direct member the edge leaves
    # from — NULL for block terminators, the containing member for sealed
    # exits of nested islands (§5.5) that land on our blocks mid-block).
    # Bundles leaving the island (sealed exits of OUR blocks) are exits:
    # they contribute no successor.
    edges = Tuple{Int,Int,StmtId,StmtId}[]
    terms = Vector{StmtId}(undef, n)
    for (i, r) in enumerate(blocks)
        ms = UnifiedIR.region_stmts(ir, r)
        isempty(ms) && return 0
        t = ms[end]
        terms[i] = t
        for (dest, _) in UnifiedIR.edge_bundles(ir, t)
            j = get(bidx, dest.id, 0)
            j == 0 && continue               # exit to an outer island
            push!(edges, (i, j, t, UnifiedIR.NULL_STMT))
        end
    end
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) in _EDGE_KINDS || continue
        haskey(bidx, UnifiedIR.stmt_region(ir, s).id) && continue
        for (dest, _) in UnifiedIR.edge_bundles(ir, s)
            j = get(bidx, dest.id, 0)
            j == 0 && continue
            c = _island_container(ir, bidx, s)
            # entries from wholly outside the island cannot be modeled
            c === nothing && return 0
            # NB. an edge from a handler nested under member m is fine for
            # candidate cells: their island stores are all direct members, so
            # none can execute mid-try — the reaching definition at the
            # handler entry is exactly the one at m (the mid-edge formula);
            # §6 protects handler READS of the cell, which refuse per-use
            push!(edges, (c[1], j, s, c[2]))
        end
    end
    succs = [Int[] for _ in 1:n]; preds = [Int[] for _ in 1:n]
    for (i, j, _, _) in edges
        j in succs[i] || push!(succs[i], j)
        i in preds[j] || push!(preds[j], i)
    end
    # reachability + reverse postorder from the entry block (owned region 1)
    seen = falses(n); post = Int[]
    stk = Tuple{Int,Int}[(1, 1)]; seen[1] = true
    while !isempty(stk)
        (b, ci) = stk[end]
        if ci <= length(succs[b])
            stk[end] = (b, ci + 1)
            c = succs[b][ci]
            seen[c] || (seen[c] = true; push!(stk, (c, 1)))
        else
            push!(post, b); pop!(stk)
        end
    end
    rpo = reverse(post)
    rpon = zeros(Int, n)
    for (i, b) in enumerate(rpo); rpon[b] = i; end
    # iterative idoms (Cooper–Harvey–Kennedy)
    idom = zeros(Int, n); idom[1] = 1
    function inter(a::Int, b::Int)
        while a != b
            while rpon[a] > rpon[b]; a = idom[a]; end
            while rpon[b] > rpon[a]; b = idom[b]; end
        end
        return a
    end
    changed = true
    while changed
        changed = false
        for b in rpo
            b == 1 && continue
            ni = 0
            for p in preds[b]
                (seen[p] && idom[p] != 0) || continue
                ni = ni == 0 ? p : inter(ni, p)
            end
            ni == 0 && continue
            idom[b] != ni && (idom[b] = ni; changed = true)
        end
    end
    # dominance frontiers. NB. the entry block always has one IMPLICIT
    # in-edge (the cfg op's entry), so a single internal pred already makes
    # it a join — the merge of the incoming value with the backedge
    df = [Int[] for _ in 1:n]
    for b in 1:n
        (seen[b] && length(preds[b]) + (b == 1 ? 1 : 0) >= 2) || continue
        for p in preds[b]
            (seen[p] && idom[p] != 0) || continue
            r = p
            while r != idom[b]
                b in df[r] || push!(df[r], b)
                r == idom[r] && break
                r = idom[r]
            end
        end
    end
    graph = (; blocks, bidx, succs, preds, edges, terms, seen, rpo, idom, df)
    promoted = 0
    for c in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, c) && continue
        UnifiedIR.stmt_kind(ir, c) === K"cell" || continue
        promoted += _promote_one_island_cell!(ir, I, graph, c)
    end
    return promoted
end

function _promote_one_island_cell!(ir::UnifiedIR.IR, I::StmtId, graph, cell::StmtId)
    (; blocks, bidx, succs, preds, edges, terms, seen, rpo, idom, df) = graph
    nb = length(blocks)
    instores = [StmtId[] for _ in 1:nb]
    newsin = [StmtId[] for _ in 1:nb]
    reads = Tuple{Int,StmtId,StmtId}[]       # (block, direct member, get)
    isdefs = Tuple{Int,StmtId,StmtId}[]
    outstores = StmtId[]
    outother = StmtId[]                      # dominating outside reads/queries
    outnews = StmtId[]                       # dominating outside cell_news
    outnd = StmtId[]                         # non-dominating outside uses
    touched = false
    ok = true
    UnifiedIR.each_ssa_use(ir) do site, used
        (ok && used == cell) || return
        site isa UnifiedIR.StmtOperand || (ok = false; return)
        u = site.user
        UnifiedIR.is_tombstone(ir, u) && return
        k = UnifiedIR.stmt_kind(ir, u)
        c = _island_container(ir, bidx, u)
        if c === nothing
            # outside: stores dominating the cfg op feed the incoming value,
            # and reads/queries dominating it never observe island stores;
            # anything else (in particular any use AFTER the island, which
            # would observe the stores this pass deletes) refuses
            if site.opidx != 1
                ok = false                   # value use of the cell: escape
            elseif k === K"cell_set" && _cell_dominates_ed(ir, u, I)
                push!(outstores, u)
            elseif (k === K"cell_get" || k === K"cell_isdefined") &&
                   _cell_dominates_ed(ir, u, I)
                push!(outother, u)           # stays a memory read (outside)
            elseif k === K"cell_new" && _cell_dominates_ed(ir, u, I)
                push!(outnews, u)            # declaration pattern checked below
            elseif k === K"cell_set" || k === K"cell_get" || k === K"cell_isdefined"
                push!(outnd, u)              # position checked against the loop
            else
                ok = false
            end
            return
        end
        touched = true
        (bi, m, hnd) = c
        seen[bi] || (ok = false; return)     # use in unreachable island block
        hnd && (ok = false; return)          # §6: handler uses stay memory
        if k === K"cell_set" && site.opidx == 1
            u == m || (ok = false; return)   # store below block level
            v = UnifiedIR.getop(ir, u, 2)
            if UnifiedIR.optag(v) === UnifiedIR.TAG_STMT &&
               UnifiedIR.stmt_kind(ir, UnifiedIR.asstmt(v)) === K"gc_preserve_begin"
                ok = false; return           # §6: tokens never promote
            end
            push!(instores[bi], u)
        elseif k === K"cell_get" && site.opidx == 1
            push!(reads, (bi, m, u))
        elseif k === K"cell_isdefined" && site.opidx == 1
            push!(isdefs, (bi, m, u))
        elseif k === K"cell_new" && site.opidx == 1
            # NewvarNode-equivalent: re-undefines the cell at this island
            # point. Direct members only; the dataflow treats them as KILL
            # events (reads/queries/exit values resolving through one refuse)
            u == m || (ok = false; return)
            push!(newsin[bi], u)
        else
            ok = false                       # escape / value use
        end
        return
    end
    (ok && touched) || return 0
    isempty(reads) && isempty(isdefs) && return 0   # store-only: dead-store side
    # outside news only in the declaration pattern (before every dominating
    # store): the incoming state at the cfg op is then store-defined
    all(nw -> all(st -> nw.id < st.id, outstores), outnews) || return 0
    if _sink_crosses_try(ir, I,
            Iterators.flatten((outstores, outother, outnews, outnd,
                               (u for (_, _, u) in reads), (u for (_, _, u) in isdefs))))
        return 0
    end
    # Backedge staleness / exit-value threading: when a loop body lies
    # strictly between the cfg op and the cell declaration, the cell memory
    # is carried ACROSS iterations (island exits reach the loop backedge; the
    # next iteration re-reads the cell before re-entering the island), so the
    # island stores may only be deleted if the carried value THREADS: the
    # loop grows a carried arg (init = the store reaching the loop), every
    # `continue` targeting it appends the reaching value at its exit point,
    # and the island's incoming value becomes the arg. Threading conditions
    # (each failure keeps the staleness refusal — the soundness sentinel):
    #   - exactly ONE loop body between (multi-loop lifetimes stay memory)
    #   - no outside use inside that loop body (a read there would observe
    #     the deleted stores on iterations >= 2; a store would break the
    #     arg's iteration-start invariant)
    #   - a store reaches the loop op definitively (the carried init)
    #   - every continue targeting the loop leaves from inside this island
    #     (its reaching value is this pass's per-block dataflow)
    thloop = UnifiedIR.NULL_STMT
    thbody = UnifiedIR.NULL_REGION
    thinit = UnifiedIR.NULL_STMT
    thconts = StmtId[]
    if !all(isempty, instores)
        declreg = UnifiedIR.stmt_region(ir, cell)
        r = UnifiedIR.stmt_region(ir, I)
        nloops = 0
        while r != declreg && !UnifiedIR.isnull(r)
            reg = UnifiedIR.getregion(ir, r)
            if reg.kind === UnifiedIR.REGION_LOOP_BODY
                nloops += 1
                thbody = r
                thloop = reg.owner
            end
            r = UnifiedIR.isnull(reg.owner) ? UnifiedIR.NULL_REGION :
                UnifiedIR.stmt_region(ir, reg.owner)
        end
        nloops > 1 && return 0
        if nloops == 1
            for u in Iterators.flatten((outstores, outother))
                UnifiedIR.is_ancestor(ir, thbody, UnifiedIR.stmt_region(ir, u)) &&
                    return 0
            end
            rk, rst = _reach_ed(ir, outstores, thloop)
            rk === :store && (thinit = rst)
            contsok = true
            for cs in UnifiedIR.each_stmt(ir)
                UnifiedIR.is_tombstone(ir, cs) && continue
                UnifiedIR.stmt_kind(ir, cs) === K"continue" || continue
                UnifiedIR.asregion(UnifiedIR.getop(ir, cs, 1)) == thbody || continue
                _island_container(ir, bidx, cs) === nothing && (contsok = false; break)
                push!(thconts, cs)
            end
            contsok || (thconts = StmtId[]; nothing)
            thcontsok = contsok
        else
            thcontsok = true
        end
    else
        thcontsok = true
    end
    loopbetween = !UnifiedIR.isnull(thloop)
    # non-dominating outside uses: only reads/queries/stores POSITIONED AFTER
    # a threaded loop are compatible — reads there stay memory, fed by one
    # unconditional store of the loop's exit value (the post-set below);
    # anything else would observe deleted stores
    needpost = false
    if !isempty(outnd)
        loopbetween || return 0
        for u in outnd
            UnifiedIR.comes_before(ir, thloop, u) || return 0
            UnifiedIR.stmt_kind(ir, u) === K"cell_set" || (needpost = true)
        end
    end
    if needpost
        # the post-set consumes the loop's exit result: the result must be
        # otherwise unconsumed, and no break may bind a different shape
        UnifiedIR.use_counts(ir)[thloop.id] == 0 || return 0
        for bs in UnifiedIR.each_stmt(ir)
            UnifiedIR.is_tombstone(ir, bs) && continue
            UnifiedIR.stmt_kind(ir, bs) === K"break" || continue
            UnifiedIR.asregion(UnifiedIR.getop(ir, bs, 1)) == thbody && return 0
        end
    end
    # whether threading is REQUIRED (the entry value is observed) is decided
    # after phi placement; these are the raw facts
    thinitok = !UnifiedIR.isnull(thinit)
    # member positions (region_stmts order; only relative order is used)
    pos = Dict{Int32,Int}()
    for r in blocks
        for (p, m) in enumerate(UnifiedIR.region_stmts(ir, r))
            pos[m.id] = p
        end
    end
    for bi in 1:nb
        sort!(instores[bi]; by = st -> pos[st.id])
        sort!(newsin[bi]; by = nw -> pos[nw.id])
    end
    hasst = [!isempty(instores[bi]) for bi in 1:nb]
    firstpos(bi) = isempty(instores[bi]) ? typemax(Int) : pos[instores[bi][1].id]
    # reaching value at a position within a block (nothing = flows in)
    function storebefore(bi::Int, p::Int)
        for st in Iterators.reverse(instores[bi])
            pos[st.id] < p && return st
        end
        return UnifiedIR.NULL_STMT
    end
    inedges = [Tuple{Int,StmtId,StmtId}[] for _ in 1:nb]   # (from, stmt, mid)
    for (f, t, st, mid) in edges
        push!(inedges[t], (f, st, mid))
    end
    # last event strictly before position p: :store, :new (killed), or :none
    function lastevent(bi::Int, p::Int)
        st = storebefore(bi, p)
        sp = UnifiedIR.isnull(st) ? 0 : pos[st.id]
        np = 0
        for nw in Iterators.reverse(newsin[bi])
            pos[nw.id] < p && (np = pos[nw.id]; break)
        end
        np > sp && return :new
        sp > 0 && return :store
        return :none
    end
    # block-level liveness (backward), for phi pruning
    upx = falses(nb)
    for (bi, m, _) in Iterators.flatten((reads, isdefs))
        pos[m.id] < firstpos(bi) && (upx[bi] = true)
    end
    livein = falses(nb)
    changed = true
    while changed
        changed = false
        for bi in nb:-1:1
            seen[bi] || continue
            li = upx[bi]
            if !li
                for (f, t, _, mid) in edges
                    f == bi || continue
                    livein[t] || continue
                    # terminator edges expose through the whole block; a
                    # mid-block sealed exit exposes through positions < mid
                    if UnifiedIR.isnull(mid) ? !hasst[bi] :
                       UnifiedIR.isnull(storebefore(bi, pos[mid.id]))
                        li = true
                        break
                    end
                end
            end
            li != livein[bi] && (livein[bi] = li; changed = true)
        end
    end
    # definite assignment (forward, optimistic init, AND meet); the virtual
    # entry edge is defined iff its would-be source is definite — a store
    # reaching the loop op when a loop lies between (the carried init), a
    # dominating outside store otherwise — and no entry `cell_new`
    # re-undefines the cell
    vin_ok = loopbetween ? thinitok : !isempty(outstores)
    defin = trues(nb); defout = trues(nb)
    changed = true
    while changed
        changed = false
        for bi in rpo
            di = bi == 1 ? vin_ok : true
            for (f, st, mid) in inedges[bi]
                seen[f] || continue
                edgedef = UnifiedIR.isnull(mid) ? defout[f] :
                    (!UnifiedIR.isnull(storebefore(f, pos[mid.id])) || defin[f])
                edgedef || (di = false; break)
            end
            ev = lastevent(bi, typemax(Int))
            do_ = ev === :store ? true : ev === :new ? false : di
            (di != defin[bi] || do_ != defout[bi]) &&
                (defin[bi] = di; defout[bi] = do_; changed = true)
        end
    end
    for (bi, m, _) in Iterators.flatten((reads, isdefs))
        ev = lastevent(bi, pos[m.id])
        # §6: maybe-undef reads/queries stay memory — killed by a preceding
        # cell_new, or upward-exposed without a definite incoming value
        (ev === :new || (ev === :none && !defin[bi])) && return 0
    end
    # liveness-pruned iterated dominance frontier of the store blocks (the
    # incoming def acts through the entry block when it exists)
    inphi = falses(nb)
    wl = Int[bi for bi in 1:nb if seen[bi] && hasst[bi]]
    vin_ok && push!(wl, 1)
    while !isempty(wl)
        b = pop!(wl)
        for f in df[b]
            (livein[f] && !inphi[f]) || continue
            inphi[f] = true
            push!(wl, f)
        end
    end
    # does anything observe the island-entry value? (symbolic resolution:
    # 1 = a concrete island value, 2 = the entry value)
    invsym = zeros(Int, nb); outsym = zeros(Int, nb)
    function outsym_of(bi::Int)
        outsym[bi] != 0 && return outsym[bi]
        outsym[bi] = hasst[bi] ? 1 : invsym_of(bi)
    end
    function invsym_of(bi::Int)
        invsym[bi] != 0 && return invsym[bi]
        invsym[bi] = inphi[bi] ? 1 : (bi == 1 ? 2 : outsym_of(idom[bi]))
    end
    needvin = inphi[1]                # an entry phi consumes the entry edge
    if !needvin
        for (bi, m, _) in reads
            UnifiedIR.isnull(storebefore(bi, pos[m.id])) || continue
            invsym_of(bi) == 2 && (needvin = true; break)
        end
    end
    if !needvin
        for (f, t, _, mid) in edges
            inphi[t] || continue
            sym = UnifiedIR.isnull(mid) ? outsym_of(f) :
                  (UnifiedIR.isnull(storebefore(f, pos[mid.id])) ? invsym_of(f) : 1)
            sym == 2 && (needvin = true; break)
        end
    end
    # a killed (post-`cell_new`) state may not flow out along any CONSUMED
    # edge: live phi edges and threaded backedges must carry real values
    for (f, t, _, mid) in edges
        inphi[t] || continue
        ev = UnifiedIR.isnull(mid) ? lastevent(f, typemax(Int)) : lastevent(f, pos[mid.id])
        ev === :new && return 0
    end
    if loopbetween
        for cont in thconts
            cc = _island_container(ir, bidx, cont)::Tuple{Int,StmtId,Bool}
            ev = cc[2] == cont ? lastevent(cc[1], typemax(Int)) :
                                 lastevent(cc[1], pos[cc[2].id])
            ev === :new && return 0
        end
    end
    # threading decision: with a loop between, an OBSERVED entry value must
    # thread (carried arg + init + every backedge carries its exit value);
    # an unobserved one needs nothing — every read is iteration-local, so
    # deleting the stores is already sound
    threading = false
    if loopbetween && (needvin || needpost)
        (thinitok && thcontsok) || return 0    # the staleness sentinel
        threading = true
    end
    want = Set{Int32}(blocks[bi].id for bi in 1:nb if inphi[bi])
    if !isempty(want)
        # every edge into a phi block must be extendable: edges from
        # unreachable blocks have no reaching value — refuse (verify checks
        # bundle arity on all edges, reachable or not)
        for (f, t, _, _) in edges
            !seen[f] && inphi[t] && return 0
        end
    end
    # ---- rewrite (all refusals are behind us) --------------------------------
    vin = Ref{Union{Nothing,UnifiedIR.Operand}}(nothing)
    if threading
        # the loop grows a carried arg; its init is the reaching store value
        breg = UnifiedIR.getregion(ir, thbody)
        bms = UnifiedIR.region_stmts(ir, thbody)
        ba = UnifiedIR.insert_before!(ir, bms[length(breg.args) + 1],
                                      K"region_arg"; type = Any)
        push!(breg.args, ba)
        lops = UnifiedIR.Operand[UnifiedIR.getop(ir, thloop, i)
                                 for i in 1:UnifiedIR.nops(ir, thloop)]
        push!(lops, UnifiedIR.getop(ir, thinit, 2))
        UnifiedIR.store_ops!(ir, thloop, lops)
        vin[] = UnifiedIR.op_stmt(ba)
        _trace!(:loop_header, thloop, cell)
        if needpost
            # one unconditional store of the exit value right after the
            # loop; post-loop reads keep reading memory (the loop's result
            # on a continue-exit is the full carried tuple, our slot last)
            UnifiedIR.set_type!(ir, thloop, Any)
            total = length(breg.args)
            v = if total == 1
                UnifiedIR.op_stmt(thloop)
            else
                e = UnifiedIR.insert_after!(ir, thloop, K"extract",
                                            UnifiedIR.op_stmt(thloop),
                                            UnifiedIR.op_inline(Int64(total)); type = Any)
                UnifiedIR.op_stmt(e)
            end
            at = total == 1 ? thloop : UnifiedIR.asstmt(v)
            UnifiedIR.insert_after!(ir, at, K"cell_set",
                                    UnifiedIR.op_stmt(cell), v; type = Nothing)
            _trace!(:loop_break, thloop, cell)
        end
    end
    getvin() = begin
        if vin[] === nothing
            g = UnifiedIR.insert_before!(ir, I, K"cell_get", UnifiedIR.op_stmt(cell); type = Any)
            vin[] = UnifiedIR.op_stmt(g)
        end
        vin[]::UnifiedIR.Operand
    end
    phiarg = Dict{Int,UnifiedIR.Operand}()
    for bi in 1:nb
        inphi[bi] || continue
        _trace!(:island_phi, blocks[bi], cell)
        reg = UnifiedIR.getregion(ir, blocks[bi])
        ms = UnifiedIR.region_stmts(ir, blocks[bi])
        at = ms[length(reg.args) + 1]        # first non-arg member
        a = UnifiedIR.insert_before!(ir, at, K"region_arg"; type = Any)
        push!(reg.args, a)
        phiarg[bi] = UnifiedIR.op_stmt(a)
    end
    function inval(bi::Int)
        haskey(phiarg, bi) && return phiarg[bi]
        bi == 1 && return getvin()
        return outval(idom[bi])
    end
    function outval(bi::Int)
        hasst[bi] && return UnifiedIR.getop(ir, instores[bi][end], 2)
        return inval(bi)
    end
    done = Set{Int32}()                       # extend edges into phi blocks
    for (f, t, st, mid) in edges
        inphi[t] || continue
        st.id in done && continue
        push!(done, st.id)
        v = if UnifiedIR.isnull(mid)
            outval(f)
        else
            rst = storebefore(f, pos[mid.id])
            UnifiedIR.isnull(rst) ? inval(f) : UnifiedIR.getop(ir, rst, 2)
        end
        _extend_bundles!(ir, st, want, v)
    end
    for cont in (threading ? thconts : StmtId[])  # backedges carry the exit value
        c = _island_container(ir, bidx, cont)::Tuple{Int,StmtId,Bool}
        v = if c[2] == cont
            outval(c[1])
        else
            # sealed exit nested below block level (inner island, if, or a
            # handler): the reaching value at its containing member position
            rst = storebefore(c[1], pos[c[2].id])
            UnifiedIR.isnull(rst) ? inval(c[1]) : UnifiedIR.getop(ir, rst, 2)
        end
        keep = UnifiedIR.Operand[UnifiedIR.getop(ir, cont, i)
                                 for i in 1:UnifiedIR.nops(ir, cont)]
        UnifiedIR.replace_stmt!(ir, cont, K"continue", keep..., v)
    end
    if inphi[1]                               # the island's own entry edge
        ops = UnifiedIR.Operand[UnifiedIR.getop(ir, I, i) for i in 1:UnifiedIR.nops(ir, I)]
        push!(ops, getvin())
        UnifiedIR.store_ops!(ir, I, ops)
    end
    for (bi, m, g) in reads
        v = nothing
        for st in Iterators.reverse(instores[bi])
            if pos[st.id] < pos[m.id]
                v = UnifiedIR.getop(ir, st, 2)
                break
            end
        end
        v === nothing && (v = inval(bi))
        UnifiedIR.replace_uses_where!(_ -> true, ir, g => v)
        UnifiedIR.delete_stmt!(ir, g)
    end
    for (_, _, d) in isdefs
        UnifiedIR.replace_uses_where!(_ -> true, ir, d => UnifiedIR.op_inline(true))
        UnifiedIR.delete_stmt!(ir, d)
    end
    for bi in 1:nb, st in instores[bi]
        UnifiedIR.delete_stmt!(ir, st)
    end
    for bi in 1:nb, nw in newsin[bi]
        UnifiedIR.delete_stmt!(ir, nw)
    end
    # the cell and its dominating outside stores remain; when the incoming
    # `cell_get` was materialized the dominating-store pass folds it (and the
    # dead-store sweep then drops the cell), same as the arm pass's incoming
    return 1
end
