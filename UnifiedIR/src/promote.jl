# Cell promotion: the mem2reg suite over the region tree (§6 join-point
# completeness). Substrate-level and Julia-semantics-free: shared verbatim by
# the compiler port (Compiler.Unified drives it from `optimize_ir!`) and by
# lowering (JuliaLowering's closure-capture analysis drives it through
# `promote_fixpoint!`) — one promotion machinery, two consumers.
#
# The dominating-store base case (`promote_cells!`, §6 v1 policy) lives in
# passes.jl; this file holds the join-point passes moved out of
# Compiler/src/unified/sroa.jl:
#   - forward_if_results!      if ops whose arms produce the same operand
#   - promote_block_cells!     single-region mem2reg (sequential reaching defs)
#   - promote_arm_cells!       sibling-arm store sinking through if results
#   - promote_loop_cells!      loop-carried args (incl. sealed-exit threading)
#   - promote_island_cells!    island phis on liveness-pruned iterated DFs
#   - promote_undef_cells!     definedness-as-data for maybe-undef cells
# plus the `promote_fixpoint!` driver composing them to quiescence.

"Terminator kinds that leave the enclosing region without joining its result."
is_diverge_kind(k::Kind) =
    k === K"return" || k === K"unreachable" || k === K"break" || k === K"continue"

"""
    operand_static_value(ir, o; stmt_value = nothing) -> value or nothing

Constant value of an operand when statically known (inline immediates, pool
constants, defined-const globals). For statement operands the substrate has no
type lattice; a provider may supply `stmt_value(ir, stmt)::Union{Nothing,Any}`
(Compiler.Unified passes its inferred-Const reader) — `nothing` means unknown.
"""
function operand_static_value(ir::IR, o::Operand; stmt_value = nothing)
    t = optag(o)
    if t == TAG_INLINE
        return imm_value(o)
    elseif t == TAG_CONST
        return ir.body.constants[payload(o)]
    elseif t == TAG_GLOBAL
        g = ir.body.globals[payload(o)]
        (isconst(g.mod, g.name) && isdefined(g.mod, g.name)) && return getglobal(g.mod, g.name)
        return nothing
    elseif t == TAG_STMT && stmt_value !== nothing
        return stmt_value(ir, asstmt(o))
    end
    return nothing
end

"""
    forward_if_results!(ir) -> Int

`if` ops (two live arms) whose arms each produce exactly one result operand and all
produce the *same* operand forward it to the result's uses. Statement operands
must be visible at the `if` itself (hence at every use of the result). The
`if` op remains for its arm effects; `adce_region_ops!` removes it when pure.
Dense state.
"""
function forward_if_results!(ir::IR)
    check_state(ir, LAYOUT_DENSE, "forward_if_results!")
    n = 0
    counts = use_counts(ir)
    for s in each_stmt(ir)
        stmt_kind(ir, s) === K"if" || continue
        counts[s.id] > 0 || continue
        rs = live_owned_regions(ir, s)
        length(rs) == 2 || continue    # one-armed if: not-taken result is nothing
        yop = nothing
        ok = true
        for r in rs
            t = region_terminator(ir, r)
            (t !== nothing && stmt_kind(ir, t) === K"result" &&
             nops(ir, t) == 1) || (ok = false; break)
            o = getop(ir, t, 1)
            if yop === nothing
                yop = o
            elseif yop.bits != o.bits
                ok = false
                break
            end
        end
        (ok && yop !== nothing) || continue
        yo = yop::Operand
        if optag(yo) == TAG_STMT
            visible(ir, asstmt(yo), s) || continue
        end
        replace_uses!(ir, s => yo)
        n += 1
    end
    n > 0 && flush_renames!(ir)
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
function promote_block_cells!(ir::IR)
    check_state(ir, LAYOUT_DENSE, "promote_block_cells!")
    flush_renames!(ir)
    promoted = 0
    for ci in 1:nstmts(ir)
        ir.body.kind[ci] === K"cell" || continue     # frame-class only (§6)
        cell = StmtId(Int32(ci))
        R = NULL_REGION
        stores = StmtId[]; gets = StmtId[]; news = StmtId[]; isdefs = StmtId[]
        ok = true
        each_ssa_use(ir) do site, used
            (ok && used == cell) || return
            site isa StmtOperand || (ok = false; return)
            u = site.user
            ur = stmt_region(ir, u)
            if R == NULL_REGION
                R = ur
            elseif R != ur
                ok = false
                return
            end
            k = stmt_kind(ir, u)
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
                st.id < g.id && (isnull(best) || best.id < st.id) && (best = st)
            end
            best
        end
        all(g -> !isnull(reaching(g)), gets) || continue
        all(d -> !isnull(reaching(d)), isdefs) || continue
        for g in gets
            v = getop(ir, reaching(g), 2)
            replace_uses_where!(_ -> true, ir, g => v)
            delete_stmt!(ir, g)
        end
        for d in isdefs
            replace_uses_where!(_ -> true, ir, d => op_inline(true))
            delete_stmt!(ir, d)
        end
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

# ---------------------------------------------------------------------------
# Loop-carried cell promotion (§5.3 / §6): the loop-crossing mem2reg cases
# that `promote_cells!`/`promote_block_cells!` refuse.
# ---------------------------------------------------------------------------

# Store dominates a use site for promotion purposes: the site's region
# ancestry reaches the store's region crossing only immediate, non-handler
# (§6 throw-edge rule), non-guard regions, and the store precedes the site.
# Island BLOCK crossings are fine: regions are only entered from above, so
# an ancestor store precedes every execution of the site — including block
# re-entry, which re-executes from the cfg's entry, after the store. Cross-
# block interference between STORES is the callers' problem (_may_reach
# reports :ambig; the island pass runs its own block dataflow).
function _cell_dominates_ed(ir::IR, st::StmtId, site::StmtId)
    sr = stmt_region(ir, st)
    r = stmt_region(ir, site)
    while r != sr
        reg = getregion(ir, r)
        # crossing INTO a guard region is fine for store→site dominance: a
        # guard may be skipped, but when the site executes at all, the whole
        # guard evaluation happens after the store. Handlers stay refused
        # (§6 throw edge), as do non-immediate activations.
        reg.kind === REGION_HANDLER && return false
        reg.activation === ACT_IMMEDIATE || return false
        isnull(reg.parent) && return false
        r = reg.parent
    end
    return comes_before(ir, st, site)
end

# Can store `t` execute on some path that then reaches `site`? False only
# when t and site sit in sibling arms of one `if` (mutually exclusive).
function _may_reach(ir::IR, t::StmtId, site::StmtId; iteration_local::Bool = false)
    A = RegionId[]
    r = stmt_region(ir, t)
    while !isnull(r)
        push!(A, r)
        r = getregion(ir, r).parent
    end
    idxof = Dict{Int32,Int}(a.id => i for (i, a) in enumerate(A))
    r = stmt_region(ir, site)
    prev = NULL_REGION
    while !isnull(r)
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
                    tt = region_terminator(ir, A[j])
                    tt === nothing && continue
                    is_diverge_kind(stmt_kind(ir, tt)) && return false
                end
            end
            isnull(prev) && return true
            tchild = A[i - 1]
            tc = getregion(ir, tchild)
            pc = getregion(ir, prev)
            if tchild != prev && !isnull(tc.owner) && tc.owner == pc.owner &&
               tc.kind === REGION_ARM && pc.kind === REGION_ARM
                return false
            end
            return true
        end
        prev = r
        r = getregion(ir, r).parent
    end
    return true
end

# Reaching store for `site` among `stores` (editable state, okey order):
# (:store, st) — unambiguous dominating reaching definition;
# (:none, _)   — no store can reach (value is the incoming one);
# (:ambig, _)  — a non-dominating store may reach: refuse promotion.
function _reach_ed(ir::IR, stores::Vector{StmtId}, site::StmtId;
                   iteration_local::Bool = false)
    best = NULL_STMT
    for st in stores
        comes_before(ir, st, site) || continue
        _cell_dominates_ed(ir, st, site) || continue
        (isnull(best) || comes_before(ir, best, st)) && (best = st)
    end
    for t in stores
        t == best && continue
        comes_before(ir, t, site) || continue
        (isnull(best) || comes_before(ir, best, t)) || continue
        _may_reach(ir, t, site; iteration_local) && return (:ambig, NULL_STMT)
    end
    return isnull(best) ? (:none, NULL_STMT) : (:store, best)
end

# §6 throw-edge rule for store motion: when `anchor` (the region being
# rewritten — an if, loop, or cfg op) lies inside a `try` body while the cell
# has uses outside that try, a swallowed exception exposes the cell's
# mid-try memory to those outside readers — deleting or moving the stores
# would change what they observe. Refuse such cells wholesale.
function _sink_crosses_try(ir::IR, anchor::StmtId, uses)
    r = stmt_region(ir, anchor)
    while !isnull(r)
        reg = getregion(ir, r)
        own = reg.owner
        if !isnull(own) && stmt_kind(ir, own) === K"try" &&
           reg.kind !== REGION_HANDLER
            # the exceptional path only reaches this frame's post-try code
            # when some handler JOINS (falls through / results); a handler
            # that diverges (rethrow, return, unreachable) never exposes the
            # mid-try state to outside uses
            joins = false
            for h in live_owned_regions(ir, own)[2:end]
                t = region_terminator(ir, h)
                (t === nothing || stmt_kind(ir, t) === K"result") &&
                    (joins = true; break)
            end
            if joins
                for u in uses
                    is_ancestor(ir, r, stmt_region(ir, u)) ||
                        return true
                end
            end
        end
        r = isnull(own) ? NULL_REGION :
            stmt_region(ir, own)
    end
    return false
end

# The innermost loop body on `t`'s region chain that contains `site` — the
# tightest backedge that can carry t's stored value around to `site` on a
# later iteration. NULL when they share no loop.
function _innermost_shared_body(ir::IR, t::StmtId, site::StmtId)
    r = stmt_region(ir, t)
    while !isnull(r)
        reg = getregion(ir, r)
        if reg.kind === REGION_LOOP_BODY &&
           is_ancestor(ir, r, stmt_region(ir, site))
            return r
        end
        r = reg.parent
    end
    return NULL_REGION
end

# Path from a body-site's region up to the loop body region: stores may cross
# only `if` arms (their reaching values stay statically resolvable); reads may
# additionally sit inside nested loop bodies (the carried value is invariant
# there). Anything else (islands, handlers, try bodies) keeps memory form.
function _body_path_ok(ir::IR, u::StmtId, bodyr::RegionId; reads::Bool)
    r = stmt_region(ir, u)
    while r != bodyr
        reg = getregion(ir, r)
        okkind = reg.kind === REGION_ARM ||
                 (reads && reg.kind === REGION_LOOP_BODY)
        okkind || return false
        reg.activation === ACT_IMMEDIATE || return false
        isnull(reg.parent) && return false
        r = reg.parent
    end
    return true
end

# Direct anchor of `g` in region `rtop` (the transitive owner that is a direct
# member of rtop), refusing handler/island/guard/deferred crossings.
function _post_anchor(ir::IR, g::StmtId, rtop::RegionId)
    x = g
    steps = 0
    while stmt_region(ir, x) != rtop
        reg = getregion(ir, stmt_region(ir, x))
        (reg.kind === REGION_HANDLER || reg.kind === REGION_BLOCK ||
         reg.kind === REGION_GUARD) && return NULL_STMT
        reg.activation === ACT_IMMEDIATE || return NULL_STMT
        isnull(reg.owner) && return NULL_STMT
        x = reg.owner
        (steps += 1) > nregions(ir) && return NULL_STMT
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
    init::Operand
    # MEMORY-INIT mode: the reaching pre-store is stale across an enclosing
    # backedge (the init backedge hazard), so the carried init becomes a
    # fresh `cell_get` inserted right before the loop — a §6 memory read of
    # the per-enclosing-iteration state. Pre-stores are kept, and sinking is
    # forced so the exit store keeps that memory current for the NEXT
    # enclosing iteration; the enclosing loop's own promotion then consumes
    # the get/store pair as ordinary body traffic (nested carried args, one
    # fixpoint round per nesting level).
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
    memini::Bool
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
@inline _trace!(kind::Symbol, anchor::RegionId, cell::StmtId) =
    (t = PROMOTION_TRACE[]; t === nothing || push!(t, (kind, Int(anchor.id), Int(cell.id))); nothing)

function promote_loop_cells!(ir::IR; stmt_value = nothing)
    check_state(ir, LAYOUT_EDITABLE, "promote_loop_cells!")
    flush_renames!(ir)
    promoted = 0
    for L in collect(each_stmt(ir))
        is_tombstone(ir, L) && continue
        stmt_kind(ir, L) === K"loop" || continue
        rs = live_owned_regions(ir, L)
        length(rs) == 1 || continue
        # loops that already carry values (from an earlier round of this pass
        # or from island exit threading) compose: new args/inits/continue
        # values APPEND after the existing ones
        nops(ir, L) == length(getregion(ir, rs[1]).args) || continue
        promoted += _promote_cells_of_loop!(ir, L, rs[1]; stmt_value)
    end
    return promoted
end

function _promote_cells_of_loop!(ir::IR, L::StmtId, bodyr::RegionId; stmt_value = nothing)
    before(a, b) = comes_before(ir, a, b)
    bodyreg0 = getregion(ir, bodyr)
    # --- the loop's chain up to the region holding post reads ---------------
    chain = StmtId[]                       # enclosing `if` ops, inner → outer
    rtop = stmt_region(ir, L)
    while true
        reg = getregion(ir, rtop)
        reg.kind === REGION_ARM || break
        # the gate applies to regions the chain CROSSES, not to the loop's
        # own altitude: a loop directly inside a deferred body promotes its
        # activation-LOCAL cells normally (§5.1 audit; arms are immediate in
        # valid IR — only closures own deferred regions — so this is
        # defensive)
        reg.activation === ACT_IMMEDIATE || return 0
        ow = reg.owner
        stmt_kind(ir, ow) === K"if" || return 0
        push!(chain, ow)
        rtop = stmt_region(ir, ow)
    end
    top = isempty(chain) ? L : chain[end]
    # --- exits ---------------------------------------------------------------
    conts = StmtId[]; brks = StmtId[]
    for x in each_stmt(ir)
        k = stmt_kind(ir, x)
        (k === K"continue" || k === K"break") || continue
        asregion(getop(ir, x, 1)) == bodyr || continue
        _body_path_ok(ir, x, bodyr; reads = false) || return 0
        k === K"continue" ? push!(conts, x) : push!(brks, x)
    end
    # continues carry cond + one value per existing arg; breaks carry the
    # loop's existing result. With existing args only pure carried promotion
    # runs (no break rewriting), so the break arity is unconstrained then.
    nexist0 = length(bodyreg0.args)
    all(c -> nops(ir, c) == 2 + nexist0, conts) || return 0
    (nexist0 > 0 || all(b -> nops(ir, b) == 1, brks)) || return 0
    anycontexit = any(c -> operand_static_value(ir, getop(ir, c, 2); stmt_value) !== true,
                      conts)
    exits = vcat(conts, brks)
    counts = use_counts(ir)
    # --- per-cell analysis ----------------------------------------------------
    plans = LoopCellPlan[]
    for ci in 1:nstmts(ir)
        ir.body.kind[ci] === K"cell" || continue
        cell = StmtId(Int32(ci))
        sets = StmtId[]; gets = StmtId[]; news = StmtId[]
        ok = true
        each_ssa_use(ir) do site, used
            (ok && used == cell) || return
            site isa StmtOperand || (ok = false; return)
            u = site.user
            is_tombstone(ir, u) && return
            k = stmt_kind(ir, u)
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
        inbody(u) = is_ancestor(ir, bodyr, stmt_region(ir, u))
        prestores = StmtId[]; bodystores = StmtId[]
        sink = false
        for st in sets
            if inbody(st)
                _body_path_ok(ir, st, bodyr; reads = false) || (ok = false; break)
                push!(bodystores, st)
            elseif is_ancestor(ir, stmt_region(ir, st),
                                         stmt_region(ir, L)) && before(st, L)
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
        # inside the innermost is inside every outer one too). A hazardous
        # init switches the plan to MEMORY-INIT mode instead of refusing.
        memini = false
        for t in sets
            t == init_st && continue
            X = _innermost_shared_body(ir, t, L)
            isnull(X) && continue
            is_ancestor(ir, X, stmt_region(ir, init_st)) ||
                (memini = true; break)
        end
        if memini
            # the memory round trip (get before L, exit store after L) only
            # keeps the enclosing iteration current when EVERY exit path
            # passes the sink: a continue/break inside the body targeting an
            # ENCLOSING loop skips it, leaving stale memory for the next
            # enclosing iteration — refuse those (multi-level exit values)
            for x in each_stmt(ir)
                is_tombstone(ir, x) && continue
                k = stmt_kind(ir, x)
                (k === K"continue" || k === K"break") || continue
                inbody(x) || continue
                tgt = asregion(getop(ir, x, 1))
                tgt == bodyr && continue
                is_ancestor(ir, bodyr, tgt) && continue
                ok = false
                break
            end
            ok || continue
            sink = true                  # the exit store keeps memory current
        end
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
                    isnull(X) && continue
                    is_ancestor(ir, X, stmt_region(ir, rst)) ||
                        (ok = false; break)
                end
                ok || break
                push!(pregets, g => rst)
            else
                anchor = _post_anchor(ir, g, rtop)
                if isnull(anchor) || anchor == L || any(==(anchor), chain) ||
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
                                  getop(ir, init_st, 2), sink, memini))
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
                              !isempty(p.postgets), p.memini) for p in plans]
    end
    # --- result set and chain validation -------------------------------------
    R = [i for (i, p) in enumerate(plans) if !isempty(p.postgets) || p.sink]
    # exit-value threading repurposes the loop's result. Existing consumers
    # survive an APPEND: extract users keep their indices (the tuple grows at
    # the end), and a scalar-consumed single-value result is rebased through
    # extract(L, 1) below before the tuple grows. Consumers of a loop that
    # never carried an exit value (base 0) see `nothing` today and a real
    # tuple after appending — refuse those.
    rebase_scalar = false
    if !isempty(R) && counts[L.id] != 0
        b0 = anycontexit ? length(bodyreg0.args) :
             (isempty(brks) ? length(bodyreg0.args) : nops(ir, first(brks)) - 1)
        if b0 == 1
            rebase_scalar = true
        elseif b0 == 0
            plans = [p for p in plans if isempty(p.postgets) && !p.sink]
            isempty(plans) && return 0
            R = Int[]
        end
        # b0 >= 2: users are extracts; append keeps them valid
    end
    if !isempty(R)
        chainok = true
        for ifop in chain
            counts[ifop.id] == 0 || (chainok = false; break)
            arms = live_owned_regions(ir, ifop)
            1 <= length(arms) <= 2 || (chainok = false; break)
            carm = findfirst(a -> is_ancestor(ir, a, stmt_region(ir, L)), arms)
            carm === nothing && (chainok = false; break)
            ct = region_terminator(ir, arms[carm])
            (ct !== nothing && stmt_kind(ir, ct) === K"result" &&
             nops(ir, ct) == 0) || (chainok = false; break)
            for (ai, arm) in enumerate(arms)
                ai == carm && continue
                ot = region_terminator(ir, arm)
                ot === nothing && (chainok = false; break)
                otk = stmt_kind(ir, ot)
                (otk === K"result" && nops(ir, ot) != 0) && (chainok = false; break)
                (otk === K"result" || is_diverge_kind(otk)) || (chainok = false; break)
            end
            chainok || break
        end
        if !chainok
            # cannot thread results through the chain: sink instead
            plans = [LoopCellPlan(p.cell, p.prestores, p.bodystores, p.pregets,
                                  p.bodygets, StmtId[], p.news, p.init,
                                  p.sink || !isempty(p.postgets), p.memini) for p in plans]
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
    bodyreg = getregion(ir, bodyr)
    for p in plans
        _trace!(:loop_header, L, p.cell)
    end
    for i in R
        _trace!(:loop_break, L, plans[i].cell)
    end
    bms = region_stmts(ir, bodyr)
    firstm = bms[length(bodyreg.args) + 1]     # first non-arg member
    args = StmtId[]
    for _ in 1:k
        a = insert_before!(ir, firstm, K"region_arg"; type = Any)
        push!(args, a)
        push!(bodyreg.args, a)
    end
    newops = Operand[getop(ir, L, i) for i in 1:nops(ir, L)]
    for p in plans
        if p.memini
            g = insert_before!(ir, L, K"cell_get",
                                         op_stmt(p.cell); type = Any)
            push!(newops, op_stmt(g))
        else
            push!(newops, p.init)
        end
    end
    store_ops!(ir, L, newops)
    valat(p, i, site) = begin
        rk, rst = _reach_ed(ir, p.bodystores, site; iteration_local = true)
        rk === :store ? getop(ir, rst, 2) : op_stmt(args[i])
    end
    for (i, p) in enumerate(plans)
        for (g, rst) in p.bodygets
            v = isnull(rst) ? op_stmt(args[i]) : getop(ir, rst, 2)
            replace_uses_where!(_ -> true, ir, g => v)
            delete_stmt!(ir, g)
        end
        for (g, rst) in p.pregets
            replace_uses_where!(_ -> true, ir, g => getop(ir, rst, 2))
            delete_stmt!(ir, g)
        end
    end
    for c in conts
        vals = Operand[valat(p, i, c) for (i, p) in enumerate(plans)]
        keep = Operand[getop(ir, c, i) for i in 1:nops(ir, c)]
        replace_stmt!(ir, c, K"continue", keep..., vals...)
    end
    if !isempty(R)
        # exit-tuple base: result slots already claimed by an earlier
        # promotion of this loop. On continue-exits the result is the FULL
        # carried tuple (existing args lead), so new values index past them;
        # breaks keep their existing values, padded up to the base with
        # `nothing` (nothing consumes the pure-carried lead slots)
        base = anycontexit ? nexist0 :
               (isempty(brks) ? nexist0 : nops(ir, first(brks)) - 1)
        all(b -> nops(ir, b) == 1 + base,
            (anycontexit || isempty(brks)) ? StmtId[] : brks) || return 0
        for b in brks
            vals = Operand[valat(plans[i], i, b) for i in R]
            keep = Operand[getop(ir, b, i)
                                     for i in 1:nops(ir, b)]
            npad = 1 + base - length(keep)
            npad >= 0 || return 0
            pad = Operand[vop(ir, nothing) for _ in 1:npad]
            replace_stmt!(ir, b, K"break", keep..., pad..., vals...)
        end
        # materialize the exit values right after the loop
        set_type!(ir, L, Any)
        if rebase_scalar
            # the prior single exit value was consumed as a scalar; appending
            # turns the result into a tuple, so reroute old consumers
            # through extract(L, 1)
            e1 = insert_after!(ir, L, K"extract", op_stmt(L),
                                         op_inline(Int64(1)); type = Any)
            replace_uses_where!(u -> u != e1, ir, L => op_stmt(e1))
        end
        curvals = Operand[]
        anchor = L
        if base == 0 && length(R) == 1
            push!(curvals, op_stmt(L))
        else
            for j in 1:length(R)
                e = insert_after!(ir, anchor, K"extract", op_stmt(L),
                                            op_inline(base + j); type = Any)
                push!(curvals, op_stmt(e))
                anchor = e
            end
        end
        # sunk cells: one unconditional store of the exit value, in program
        # order right after the loop (post uses keep reading memory)
        for (ridx, i) in enumerate(R)
            plans[i].sink || continue
            anchor = insert_after!(ir, anchor, K"cell_set",
                                             op_stmt(plans[i].cell),
                                             curvals[ridx]; type = Nothing)
        end
        dodirect = any(i -> !isempty(plans[i].postgets), R)
        for ifop in (dodirect ? chain : StmtId[])
            for i in R
                _trace!(:if_thread, ifop, plans[i].cell)
            end
            arms = live_owned_regions(ir, ifop)
            carm = findfirst(a -> is_ancestor(ir, a, stmt_region(ir, L)), arms)
            ct = region_terminator(ir, arms[carm])
            replace_stmt!(ir, ct, K"result", curvals...)
            prevals = Operand[]
            for i in R
                _, pst = _reach_ed(ir, plans[i].prestores, ifop)
                push!(prevals, getop(ir, pst, 2))
            end
            handled_else = false
            for (ai, arm) in enumerate(arms)
                ai == carm && continue
                ot = region_terminator(ir, arm)
                if stmt_kind(ir, ot) === K"result"
                    replace_stmt!(ir, ot, K"result", prevals...)
                end
                handled_else = true
            end
            if !handled_else && length(arms) == 1
                er = new_region!(ir, ifop, REGION_ARM)
                push_stmt!(ir, er, K"result", prevals...)
            end
            set_type!(ir, ifop, Any)
            newvals = Operand[]
            if length(R) == 1
                push!(newvals, op_stmt(ifop))
            else
                anchor = ifop
                for j in 1:length(R)
                    e = insert_after!(ir, anchor, K"extract", op_stmt(ifop),
                                                op_inline(j); type = Any)
                    push!(newvals, op_stmt(e))
                    anchor = e
                end
            end
            curvals = newvals
        end
        for (ridx, i) in enumerate(R)
            for g in plans[i].postgets
                replace_uses_where!(_ -> true, ir, g => curvals[ridx])
                delete_stmt!(ir, g)
            end
        end
    end
    for p in plans
        if !p.sink && !p.memini
            for st in p.prestores
                delete_stmt!(ir, st)
            end
        end
        for st in p.bodystores
            delete_stmt!(ir, st)
        end
        for nw in p.news
            delete_stmt!(ir, nw)
        end
    end
    counts2 = use_counts(ir)
    for p in plans
        counts2[p.cell.id] == 0 && delete_stmt!(ir, p.cell)
    end
    return length(plans)
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
function _in_handler(ir::IR, s::StmtId)
    r = stmt_region(ir, s)
    while !isnull(r)
        reg = getregion(ir, r)
        reg.kind === REGION_HANDLER && return true
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
function promote_arm_cells!(ir::IR)
    check_state(ir, LAYOUT_EDITABLE, "promote_arm_cells!")
    flush_renames!(ir)
    ifs = StmtId[]
    for s in each_stmt(ir)
        is_tombstone(ir, s) && continue
        stmt_kind(ir, s) === K"if" || continue
        push!(ifs, s)
    end
    # inside-out: deepest region first, so an inner if's post-join store is a
    # direct arm store by the time its enclosing if is processed
    sort!(ifs; by = s -> region_depth(ir, stmt_region(ir, s)), rev = true)
    total = 0
    for I in ifs
        total += _promote_arm_cells_at!(ir, I)
    end
    flush_renames!(ir)
    return total
end

function _promote_arm_cells_at!(ir::IR, I::StmtId)
    arms = live_owned_regions(ir, I)
    (1 <= length(arms) <= 2) || return 0
    joins = RegionId[]
    for a in arms
        t = region_terminator(ir, a)
        t === nothing && return 0
        tk = stmt_kind(ir, t)
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
    for s in each_stmt(ir)
        is_tombstone(ir, s) && continue
        k = stmt_kind(ir, s)
        # only exits that leave the arm while staying in the frame leak:
        # a continue/break to a loop INSIDE the arm is arm-internal, and a
        # return/unreachable ends the frame (no later observer of the cell)
        (k === K"continue" || k === K"break") || continue
        tgt = asregion(getop(ir, s, 1))
        for a in joins
            if stmt_region(ir, s) != a &&
               is_ancestor(ir, a, stmt_region(ir, s)) &&
               !is_ancestor(ir, a, tgt)
                push!(leaky, a.id)
            end
        end
    end
    exist_arity = nops(ir, region_terminator(ir, joins[1]))
    all(a -> nops(ir, region_terminator(ir, a)) == exist_arity, joins) ||
        return 0
    onearmed = length(arms) == 1
    counts = use_counts(ir)
    # synthesizing an else arm (or re-tupling an in-use single result) only
    # supported for the common shapes; a one-armed if with existing results
    # has not-taken-value semantics we must not disturb
    onearmed && (exist_arity != 0 || counts[I.id] != 0) && return 0
    exist_arity == 0 && counts[I.id] != 0 && return 0

    before(a, b) = comes_before(ir, a, b)
    injoinarm(s) = any(a -> is_ancestor(ir, a, stmt_region(ir, s)), joins)

    # ---- candidate cells ----------------------------------------------------
    plans = Vector{Tuple{StmtId,                 # cell
                         Vector{StmtId},          # direct arm stores (to delete)
                         Dict{RegionId,StmtId},   # joining arm -> last direct store (or absent)
                         Vector{Pair{StmtId,StmtId}},  # in-arm get -> reaching store
                         Vector{StmtId},          # in-arm isdefined -> true rewrites
                         Bool}}()                 # needs incoming value
    for c in each_stmt(ir)
        is_tombstone(ir, c) && continue
        stmt_kind(ir, c) === K"cell" || continue
        cell = c
        sets = StmtId[]; gets = StmtId[]; isdefs = StmtId[]; news = StmtId[]
        ok = true
        each_ssa_use(ir) do site, used
            (ok && used == cell) || return
            site isa StmtOperand || (ok = false; return)
            u = site.user
            is_tombstone(ir, u) && return
            k = stmt_kind(ir, u)
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
            sr = stmt_region(ir, st)
            ai = findfirst(==(sr), joins)
            if ai !== nothing
                push!(allarm, st)
                # leaky arms keep their stores (mid-arm sealed exits observe
                # memory); the post-join store re-stores the same value on
                # the join path, so the arm still contributes its out below
                sr.id in leaky || push!(direct, st)
                prev = get(armlast, sr, NULL_STMT)
                (isnull(prev) || before(prev, st)) && (armlast[sr] = st)
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
            outside = [st for st in sets if !any(a -> is_ancestor(ir, a, stmt_region(ir, st)), arms)]
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
            v = getop(ir, rst, 2)
            replace_uses_where!(_ -> true, ir, g => v)
            delete_stmt!(ir, g)
        end
        for d in isdefrw
            replace_uses_where!(_ -> true, ir, d => op_inline(true))
            delete_stmt!(ir, d)
        end
    end
    # incoming values: one cell_get per needs-incoming cell, just before I
    incoming = Dict{Int,Operand}()
    for (i, (cell, _, _, _, _, needs_in)) in enumerate(plans)
        needs_in || continue
        g = insert_before!(ir, I, K"cell_get", op_stmt(cell); type = Any)
        incoming[i] = op_stmt(g)
    end
    # per-arm outgoing values appended to each joining result
    for a in joins
        t = region_terminator(ir, a)
        outs = Operand[]
        for (i, (_, _, armlast, _, _, _)) in enumerate(plans)
            st = get(armlast, a, NULL_STMT)
            push!(outs, isnull(st) ? incoming[i] : getop(ir, st, 2))
        end
        old = operands(ir, t)
        replace_stmt!(ir, t, K"result", old..., outs...)
    end
    if onearmed
        er = new_region!(ir, I, REGION_ARM)
        outs = Operand[incoming[i] for i in 1:m]   # needs_in guaranteed for all
        push_stmt!(ir, er, K"result", outs...)
    end
    set_type!(ir, I, Any)
    # post-join: extracts (when tupled) + the unconditional stores
    total_vals = exist_arity + m
    anchor = I
    if exist_arity == 1 && counts[I.id] != 0
        e0 = insert_after!(ir, anchor, K"extract", op_stmt(I),
                                     op_inline(1); type = Any)
        replace_uses_where!(u -> u != e0, ir, I => op_stmt(e0))
        anchor = e0
    end
    for (i, (cell, _, _, _, _, _)) in enumerate(plans)
        v = if total_vals == 1
            op_stmt(I)
        else
            e = insert_after!(ir, anchor, K"extract", op_stmt(I),
                                        op_inline(exist_arity + i); type = Any)
            anchor = e
            op_stmt(e)
        end
        anchor = insert_after!(ir, anchor, K"cell_set",
                                         op_stmt(cell), v; type = Nothing)
    end
    # delete the (now sunk) conditional arm stores (leaky-arm stores are
    # never in `direct`: mid-arm sealed exits still observe their memory)
    for (_, direct, _, _, _, _) in plans
        for st in direct
            delete_stmt!(ir, st)
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
function promote_island_cells!(ir::IR)
    check_state(ir, LAYOUT_EDITABLE, "promote_island_cells!")
    flush_renames!(ir)
    cfgs = StmtId[]
    for s in each_stmt(ir)
        is_tombstone(ir, s) && continue
        stmt_kind(ir, s) === K"cfg" && push!(cfgs, s)
    end
    isempty(cfgs) && return 0
    sort!(cfgs; by = s -> region_depth(ir, stmt_region(ir, s)), rev = true)
    promoted = 0
    for I in cfgs
        is_tombstone(ir, I) && continue
        promoted += _promote_island_cells_at!(ir, I)
    end
    return promoted
end

const _EDGE_KINDS = (K"goto", K"br_if", K"switch", K"await")

# containing island block of s: (block index, direct member, crossed-handler?)
# or nothing when s is not nested inside one of the island's blocks
function _island_container(ir::IR, bidx::Dict{Int32,Int}, s::StmtId)
    cur = s
    hnd = false
    while true
        r = stmt_region(ir, cur)
        isnull(r) && return nothing
        haskey(bidx, r.id) && return (bidx[r.id], cur, hnd)
        reg = getregion(ir, r)
        reg.kind === REGION_HANDLER && (hnd = true)
        isnull(reg.owner) && return nothing
        cur = reg.owner
    end
end

# rebuild terminator t, appending `val` to every bundle whose destination
# region id is in `want` (bundle encoding as in edge_bundles, §5.5)
function _extend_bundles!(ir::IR, t::StmtId, want::Set{Int32}, val::Operand)
    k = stmt_kind(ir, t)
    old = Operand[getop(ir, t, i) for i in 1:nops(ir, t)]
    new = Operand[]
    i = 1
    function copybundle!()
        dest = old[i]; i += 1
        argc = Int(imm_value(old[i])::Int64); i += 1
        hit = asregion(dest).id in want
        push!(new, dest)
        push!(new, op_inline(Int64(argc + (hit ? 1 : 0))))
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
        nc = Int(imm_value(old[i])::Int64)
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
    store_ops!(ir, t, new)
    nothing
end

function _promote_island_cells_at!(ir::IR, I::StmtId)
    blocks = live_owned_regions(ir, I)
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
        ms = region_stmts(ir, r)
        isempty(ms) && return 0
        t = ms[end]
        terms[i] = t
        for (dest, _) in edge_bundles(ir, t)
            j = get(bidx, dest.id, 0)
            j == 0 && continue               # exit to an outer island
            push!(edges, (i, j, t, NULL_STMT))
        end
    end
    for s in each_stmt(ir)
        is_tombstone(ir, s) && continue
        stmt_kind(ir, s) in _EDGE_KINDS || continue
        haskey(bidx, stmt_region(ir, s).id) && continue
        for (dest, _) in edge_bundles(ir, s)
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
    for c in collect(each_stmt(ir))
        is_tombstone(ir, c) && continue
        stmt_kind(ir, c) === K"cell" || continue
        promoted += _promote_one_island_cell!(ir, I, graph, c)
    end
    return promoted
end

function _promote_one_island_cell!(ir::IR, I::StmtId, graph, cell::StmtId)
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
    each_ssa_use(ir) do site, used
        (ok && used == cell) || return
        site isa StmtOperand || (ok = false; return)
        u = site.user
        is_tombstone(ir, u) && return
        k = stmt_kind(ir, u)
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
            v = getop(ir, u, 2)
            if optag(v) === TAG_STMT &&
               stmt_kind(ir, asstmt(v)) === K"gc_preserve_begin"
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
    thloop = NULL_STMT
    thbody = NULL_REGION
    thinit = NULL_STMT
    thconts = StmtId[]
    if !all(isempty, instores)
        declreg = stmt_region(ir, cell)
        r = stmt_region(ir, I)
        nloops = 0
        while r != declreg && !isnull(r)
            reg = getregion(ir, r)
            if reg.kind === REGION_LOOP_BODY
                nloops += 1
                thbody = r
                thloop = reg.owner
            end
            r = isnull(reg.owner) ? NULL_REGION :
                stmt_region(ir, reg.owner)
        end
        nloops > 1 && return 0
        if nloops == 1
            for u in Iterators.flatten((outstores, outother))
                is_ancestor(ir, thbody, stmt_region(ir, u)) &&
                    return 0
            end
            rk, rst = _reach_ed(ir, outstores, thloop)
            rk === :store && (thinit = rst)
            contsok = true
            for cs in each_stmt(ir)
                is_tombstone(ir, cs) && continue
                stmt_kind(ir, cs) === K"continue" || continue
                asregion(getop(ir, cs, 1)) == thbody || continue
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
    loopbetween = !isnull(thloop)
    # non-dominating outside uses: only reads/queries/stores POSITIONED AFTER
    # a threaded loop are compatible — reads there stay memory, fed by one
    # unconditional store of the loop's exit value (the post-set below);
    # anything else would observe deleted stores
    needpost = false
    if !isempty(outnd)
        loopbetween || return 0
        for u in outnd
            if comes_before(ir, thloop, u)
                # after the loop: reads stay memory, fed by the post-set
                stmt_kind(ir, u) === K"cell_set" || (needpost = true)
            elseif comes_before(ir, u, thloop)
                # before the loop (with exactly one loop between, such a use
                # runs at most once, before any island store): reads are
                # plain memory reads; a conditional store makes the carried
                # init ambiguous — poison it (threading then requires the
                # entry value to be unobserved)
                stmt_kind(ir, u) === K"cell_set" &&
                    (thinit = NULL_STMT)
            else
                return 0
            end
        end
    end
    if needpost
        # the post-set consumes the loop's exit result: the result must be
        # otherwise unconsumed, and no break may bind a different shape
        use_counts(ir)[thloop.id] == 0 || return 0
        for bs in each_stmt(ir)
            is_tombstone(ir, bs) && continue
            stmt_kind(ir, bs) === K"break" || continue
            asregion(getop(ir, bs, 1)) == thbody && return 0
        end
    end
    # whether threading is REQUIRED (the entry value is observed) is decided
    # after phi placement; these are the raw facts
    thinitok = !isnull(thinit)
    # member positions (region_stmts order; only relative order is used)
    pos = Dict{Int32,Int}()
    for r in blocks
        for (p, m) in enumerate(region_stmts(ir, r))
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
        return NULL_STMT
    end
    inedges = [Tuple{Int,StmtId,StmtId}[] for _ in 1:nb]   # (from, stmt, mid)
    for (f, t, st, mid) in edges
        push!(inedges[t], (f, st, mid))
    end
    # last event strictly before position p: :store, :new (killed), or :none
    function lastevent(bi::Int, p::Int)
        st = storebefore(bi, p)
        sp = isnull(st) ? 0 : pos[st.id]
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
                    if isnull(mid) ? !hasst[bi] :
                       isnull(storebefore(bi, pos[mid.id]))
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
                edgedef = isnull(mid) ? defout[f] :
                    (!isnull(storebefore(f, pos[mid.id])) || defin[f])
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
            isnull(storebefore(bi, pos[m.id])) || continue
            invsym_of(bi) == 2 && (needvin = true; break)
        end
    end
    if !needvin
        for (f, t, _, mid) in edges
            inphi[t] || continue
            sym = isnull(mid) ? outsym_of(f) :
                  (isnull(storebefore(f, pos[mid.id])) ? invsym_of(f) : 1)
            sym == 2 && (needvin = true; break)
        end
    end
    # a killed (post-`cell_new`) state may not flow out along any CONSUMED
    # edge: live phi edges and threaded backedges must carry real values
    for (f, t, _, mid) in edges
        inphi[t] || continue
        ev = isnull(mid) ? lastevent(f, typemax(Int)) : lastevent(f, pos[mid.id])
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
    vin = Ref{Union{Nothing,Operand}}(nothing)
    if threading
        # the loop grows a carried arg; its init is the reaching store value
        breg = getregion(ir, thbody)
        bms = region_stmts(ir, thbody)
        ba = insert_before!(ir, bms[length(breg.args) + 1],
                                      K"region_arg"; type = Any)
        push!(breg.args, ba)
        lops = Operand[getop(ir, thloop, i)
                                 for i in 1:nops(ir, thloop)]
        push!(lops, getop(ir, thinit, 2))
        store_ops!(ir, thloop, lops)
        vin[] = op_stmt(ba)
        _trace!(:loop_header, thloop, cell)
        if needpost
            # one unconditional store of the exit value right after the
            # loop; post-loop reads keep reading memory (the loop's result
            # on a continue-exit is the full carried tuple, our slot last)
            set_type!(ir, thloop, Any)
            total = length(breg.args)
            v = if total == 1
                op_stmt(thloop)
            else
                e = insert_after!(ir, thloop, K"extract",
                                            op_stmt(thloop),
                                            op_inline(Int64(total)); type = Any)
                op_stmt(e)
            end
            at = total == 1 ? thloop : asstmt(v)
            insert_after!(ir, at, K"cell_set",
                                    op_stmt(cell), v; type = Nothing)
            _trace!(:loop_break, thloop, cell)
        end
    end
    getvin() = begin
        if vin[] === nothing
            g = insert_before!(ir, I, K"cell_get", op_stmt(cell); type = Any)
            vin[] = op_stmt(g)
        end
        vin[]::Operand
    end
    phiarg = Dict{Int,Operand}()
    for bi in 1:nb
        inphi[bi] || continue
        _trace!(:island_phi, blocks[bi], cell)
        reg = getregion(ir, blocks[bi])
        ms = region_stmts(ir, blocks[bi])
        at = ms[length(reg.args) + 1]        # first non-arg member
        a = insert_before!(ir, at, K"region_arg"; type = Any)
        push!(reg.args, a)
        phiarg[bi] = op_stmt(a)
    end
    function inval(bi::Int)
        haskey(phiarg, bi) && return phiarg[bi]
        bi == 1 && return getvin()
        return outval(idom[bi])
    end
    function outval(bi::Int)
        hasst[bi] && return getop(ir, instores[bi][end], 2)
        return inval(bi)
    end
    done = Set{Int32}()                       # extend edges into phi blocks
    for (f, t, st, mid) in edges
        inphi[t] || continue
        st.id in done && continue
        push!(done, st.id)
        v = if isnull(mid)
            outval(f)
        else
            rst = storebefore(f, pos[mid.id])
            isnull(rst) ? inval(f) : getop(ir, rst, 2)
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
            isnull(rst) ? inval(c[1]) : getop(ir, rst, 2)
        end
        keep = Operand[getop(ir, cont, i)
                                 for i in 1:nops(ir, cont)]
        replace_stmt!(ir, cont, K"continue", keep..., v)
    end
    if inphi[1]                               # the island's own entry edge
        ops = Operand[getop(ir, I, i) for i in 1:nops(ir, I)]
        push!(ops, getvin())
        store_ops!(ir, I, ops)
    end
    for (bi, m, g) in reads
        v = nothing
        for st in Iterators.reverse(instores[bi])
            if pos[st.id] < pos[m.id]
                v = getop(ir, st, 2)
                break
            end
        end
        v === nothing && (v = inval(bi))
        replace_uses_where!(_ -> true, ir, g => v)
        delete_stmt!(ir, g)
    end
    for (_, _, d) in isdefs
        replace_uses_where!(_ -> true, ir, d => op_inline(true))
        delete_stmt!(ir, d)
    end
    for bi in 1:nb, st in instores[bi]
        delete_stmt!(ir, st)
    end
    for bi in 1:nb, nw in newsin[bi]
        delete_stmt!(ir, nw)
    end
    # the cell and its dominating outside stores remain; when the incoming
    # `cell_get` was materialized the dominating-store pass folds it (and the
    # dead-store sweep then drops the cell), same as the arm pass's incoming
    return 1
end

# ---------------------------------------------------------------------------
# Definedness as data (§6: maybe-undef cells promote like everything else)
# ---------------------------------------------------------------------------

"""
    promote_undef_cells!(ir) -> Int

Rewrite maybe-undef frame cells so definedness becomes DATA, the way stock
slot2ssa handles maybe-undef slots (there is no slot left behind — the phi
carries a possibly-undef value and definedness rides `throw_undef_if_not`):

    %c = cell T                 %c = cell T ; cell_set %c, nothing
                                %d = cell Bool ; cell_set %d, false
    cell_set %c, v              cell_set %c, v ; cell_set %d, true
    cell_new %c                 cell_set %c, nothing ; cell_set %d, false
    %g = cell_get %c            (undominated only:)
                                %b = cell_get %d
                                if !%b { throw(UndefVarError(:cell)); unreachable }
                                %g = cell_get %c
    %i = cell_isdefined %c      %i = cell_get %d

Both cells are then DEFINITELY ASSIGNED, so the ordinary promotion passes
dissolve them through the normal join machinery — the Bool joins land on
exactly the same dominance-frontier points as the value joins, and guards
constant-fold wherever definedness is provable (fold_constant_branches!
kills `if false` arms).

Cells whose reads/queries sit in handlers keep memory form (§6 v1: cells
are the stand-in representation for exception-SSA PhiC/Upsilon); escaping
and token cells are untouched. Editable state.
"""
function promote_undef_cells!(ir::IR)
    check_state(ir, LAYOUT_EDITABLE, "promote_undef_cells!")
    flush_renames!(ir)
    rewritten = 0
    for cell in collect(each_stmt(ir))
        is_tombstone(ir, cell) && continue
        stmt_kind(ir, cell) === K"cell" || continue
        sets = StmtId[]; gets = StmtId[]; isdefs = StmtId[]; news = StmtId[]
        ok = true
        each_ssa_use(ir) do site, used
            (ok && used == cell) || return
            site isa StmtOperand || (ok = false; return)
            u = site.user
            is_tombstone(ir, u) && return
            k = stmt_kind(ir, u)
            if k === K"cell_set" && site.opidx == 1
                v = getop(ir, u, 2)
                if optag(v) === TAG_STMT &&
                   stmt_kind(ir, asstmt(v)) === K"gc_preserve_begin"
                    ok = false; return               # token cell (§6)
                end
                push!(sets, u)
            elseif k === K"cell_get" && site.opidx == 1
                push!(gets, u)
            elseif k === K"cell_isdefined" && site.opidx == 1
                push!(isdefs, u)
            elseif k === K"cell_new" && site.opidx == 1
                push!(news, u)
            else
                ok = false                            # escape (§6)
            end
            return
        end
        ok || continue
        # §6 v1 representation: handler-observed cells stay memory (PhiC/
        # Upsilon stand-in)
        any(u -> _in_handler(ir, u), gets) && continue
        any(u -> _in_handler(ir, u), isdefs) && continue
        # trigger: some read/query lacks a dominating store, or a cell_new
        # re-undefines past the declaration cluster. NB. editable state:
        # dominance and order come from the edit list (statement ids are NOT
        # positional here — a dense-id comparison would keep re-triggering
        # on our own freshly inserted initializer)
        dominated(u) = any(st -> _cell_dominates_ed(ir, st, u), sets)
        declnews = all(nw -> all(st -> comes_before(ir, nw, st), sets), news)
        # a store-dominated read is only definitely assigned when no
        # cell_new can intervene: with non-declaration news, EVERY read gets
        # the guard (redundant ones read a constant-true Bool and fold away)
        undom = declnews ? [g for g in gets if !dominated(g)] : copy(gets)
        (isempty(undom) && isempty(isdefs) && declnews) && continue
        # idempotence/expressibility: splitting only helps when the dummy
        # initializer AT THE DECLARATION will dominate every rewritten use —
        # otherwise the same reads re-trigger forever (each split minting a
        # fresh Bool cell) while nothing ever becomes promotable
        all(u -> _cell_dominates_ed(ir, cell, u),
            Iterators.flatten((undom, isdefs))) || continue

        # ---- rewrite -----------------------------------------------------
        dcell = insert_after!(ir, cell, K"cell",
                                        vop(ir, Bool); type = Any)
        # the definedness Bool is HALF of the slot's classical phi: the DF
        # harness credits the def-cell's join placements to the original
        _trace!(:undef_split, dcell, cell)
        dinit = insert_after!(ir, dcell, K"cell_set",
                                        op_stmt(dcell),
                                        vop(ir, false); type = Nothing)
        insert_after!(ir, dinit, K"cell_set", op_stmt(cell),
                                vop(ir, nothing); type = Nothing)
        for st in sets
            insert_after!(ir, st, K"cell_set", op_stmt(dcell),
                                    vop(ir, true); type = Nothing)
        end
        for nw in news
            insert_after!(ir, nw, K"cell_set", op_stmt(dcell),
                                    vop(ir, false); type = Nothing)
            insert_after!(ir, nw, K"cell_set", op_stmt(cell),
                                    vop(ir, nothing); type = Nothing)
            delete_stmt!(ir, nw)
        end
        for g in undom
            b = insert_before!(ir, g, K"cell_get",
                                         op_stmt(dcell); type = Bool)
            nb = insert_before!(ir, g, K"call",
                                          vop(ir, Core.Intrinsics.not_int),
                                          op_stmt(b); type = Bool)
            guard = insert_before!(ir, g, K"if",
                                             op_stmt(nb); type = Any)
            garm = new_region!(ir, guard, REGION_ARM)
            push_stmt!(ir, garm, K"call", vop(ir, Core.throw),
                                 vop(ir, UndefVarError(:cell)))
            # the arm JOINS (the throw never returns at runtime): a diverging
            # terminator here would block constant folds of provably-defined
            # guards, and a mid-region splice of it would strand the tail
            push_stmt!(ir, garm, K"result")
            garm2 = new_region!(ir, guard, REGION_ARM)
            push_stmt!(ir, garm2, K"result")
        end
        for d in isdefs
            b = insert_before!(ir, d, K"cell_get",
                                         op_stmt(dcell); type = Bool)
            replace_uses_where!(_ -> true, ir, d => op_stmt(b))
            delete_stmt!(ir, d)
        end
        rewritten += 1
    end
    return rewritten
end


# ---------------------------------------------------------------------------
# Try-join cell promotion (§6, the exception-join slice of PhiC/Upsilon):
# a frame cell stored in a `try`'s body and/or handler gains ONE additional
# unconditional post-try store of the try's threaded result. Nothing is
# deleted or moved: on every path that reaches the join, the threaded value
# equals the cell's memory at that point (the last direct arm store executed
# — or the incoming value when the joining BODY never stores), so the new
# store is a semantic no-op whose only effect is giving `promote_cells!` a
# dominating definition below the throw edge. The §6 "never promote across a
# throw edge" rule is preserved: reads inside handlers keep observing memory,
# and the arm stores stay exactly where they were.
#
# Refusals (cell skipped at this try):
#   - non-2-region try, or handler region not REGION_HANDLER;
#   - escapes / value uses (as everywhere);
#   - any store inside the try NOT a direct member of body/handler (deeper
#     conditional stores wait for the inner passes to flatten them — the
#     joint fixpoint composes, exactly like the arm pass);
#   - `cell_new` inside the try, or non-declaration news outside;
#   - a JOINING handler without a direct store while the body stores: an
#     exception between body stores and the body terminator would reach the
#     join with mid-body memory the thread cannot name;
#   - a joining storeless BODY without a store dominating the try (the
#     incoming `cell_get` requires definite assignment, as in the arm pass);
#   - no post-try read/query that lacks a post-try dominating store (the
#     idempotence trigger: firing would change no reaching relation).
# ---------------------------------------------------------------------------

"""
    promote_try_cells!(ir) -> Int

Thread try-body/handler stores of frame cells into the `try`'s results and
add one unconditional post-try store (see the block comment: a provable
no-op store that unlocks the dominating-store pass below the throw edge).
Editable state; returns the number of (cell, try) threadings.
"""
function promote_try_cells!(ir::IR)
    check_state(ir, LAYOUT_EDITABLE, "promote_try_cells!")
    flush_renames!(ir)
    trys = StmtId[]
    for s in each_stmt(ir)
        is_tombstone(ir, s) && continue
        stmt_kind(ir, s) === K"try" || continue
        push!(trys, s)
    end
    sort!(trys; by = s -> region_depth(ir, stmt_region(ir, s)), rev = true)
    total = 0
    for T in trys
        total += _promote_try_cells_at!(ir, T)
    end
    flush_renames!(ir)
    return total
end

function _promote_try_cells_at!(ir::IR, T::StmtId)
    rs = live_owned_regions(ir, T)
    length(rs) == 2 || return 0
    B, H = rs[1], rs[2]
    getregion(ir, H).kind === REGION_HANDLER || return 0
    tB = region_terminator(ir, B)
    tH = region_terminator(ir, H)
    (tB === nothing || tH === nothing) && return 0
    kB = stmt_kind(ir, tB)
    kH = stmt_kind(ir, tH)
    joinB = kB === K"result"
    joinH = kH === K"result"
    (joinB || is_diverge_kind(kB)) || return 0
    (joinH || is_diverge_kind(kH)) || return 0
    (joinB || joinH) || return 0                    # join unreachable
    exist_arity = joinB ? nops(ir, tB) : nops(ir, tH)
    (joinB && joinH && nops(ir, tB) != nops(ir, tH)) && return 0
    counts = use_counts(ir)
    # result-appending only supported for the common shapes (as the arm pass)
    exist_arity == 0 && counts[T.id] != 0 && return 0
    before(a, b) = comes_before(ir, a, b)
    intry(u) = is_ancestor(ir, B, stmt_region(ir, u)) ||
               is_ancestor(ir, H, stmt_region(ir, u))

    # ---- candidate cells ----------------------------------------------------
    # plan: (cell, lastB, lastH, needs_in) — NULL_STMT marks "no direct store"
    plans = Vector{Tuple{StmtId,StmtId,StmtId,Bool}}()
    for c in each_stmt(ir)
        is_tombstone(ir, c) && continue
        stmt_kind(ir, c) === K"cell" || continue
        cell = c
        sets = StmtId[]; gets = StmtId[]; isdefs = StmtId[]; news = StmtId[]
        ok = true
        each_ssa_use(ir) do site, used
            (ok && used == cell) || return
            site isa StmtOperand || (ok = false; return)
            u = site.user
            is_tombstone(ir, u) && return
            k = stmt_kind(ir, u)
            if k === K"cell_set" && site.opidx == 1
                push!(sets, u)
            elseif k === K"cell_get"
                push!(gets, u)
            elseif k === K"cell_isdefined"
                push!(isdefs, u)
            elseif k === K"cell_new"
                push!(news, u)
            else
                ok = false                        # escape / value use / token
            end
        end
        ok || continue
        # news: declaration position only, none inside the try
        any(nw -> intry(nw), news) && continue
        all(nw -> all(st -> before(nw, st), sets), news) || continue
        # in-try stores must be DIRECT members of body/handler
        lastB = NULL_STMT
        lastH = NULL_STMT
        deeper = false
        for st in sets
            sr = stmt_region(ir, st)
            if sr == B
                (isnull(lastB) || before(lastB, st)) && (lastB = st)
            elseif sr == H
                (isnull(lastH) || before(lastH, st)) && (lastH = st)
            elseif intry(st)
                deeper = true
                break
            end
        end
        deeper && continue
        (isnull(lastB) && isnull(lastH)) && continue      # no in-try stores
        # a joining handler without a direct store cannot name mid-body memory
        joinH && isnull(lastH) && !isnull(lastB) && continue
        # storeless joining body threads the incoming value: definite
        # assignment at the try required
        needs_in = joinB && isnull(lastB)
        if needs_in
            any(st -> !intry(st) && before(st, T) && _cell_dominates_ed(ir, st, T),
                sets) || continue
        end
        # idempotence trigger: some post-try read/query that the inserted
        # store WOULD dominate (`T` stands in for it: same region, just
        # before) and that no existing post-try store already dominates.
        # Requiring dominance-by-the-new-store guarantees one firing per
        # (try, cell): reads the new store cannot dominate (e.g. inside a
        # later handler) must never re-arm the trigger.
        fired = false
        for u in Iterators.flatten((gets, isdefs))
            (before(T, u) && !intry(u)) || continue
            _cell_dominates_ed(ir, T, u) || continue
            any(st -> !intry(st) && before(T, st) &&
                      _cell_dominates_ed(ir, st, u), sets) && continue
            fired = true
            break
        end
        fired || continue
        push!(plans, (cell, lastB, lastH, needs_in))
    end
    isempty(plans) && return 0

    # ---- transform ----------------------------------------------------------
    m = length(plans)
    for (cell, _, _, _) in plans
        _trace!(:try_join, T, cell)
    end
    # incoming values (storeless joining body): one cell_get just before T
    incoming = Dict{Int,Operand}()
    for (i, (cell, _, _, needs_in)) in enumerate(plans)
        needs_in || continue
        g = insert_before!(ir, T, K"cell_get", op_stmt(cell); type = Any)
        incoming[i] = op_stmt(g)
    end
    # append each plan's outgoing value to the joining terminators
    for (arm_t, isjoin, lastof) in ((tB, joinB, 2), (tH, joinH, 3))
        isjoin || continue
        outs = Operand[]
        for (i, plan) in enumerate(plans)
            st = plan[lastof]
            push!(outs, isnull(st) ? incoming[i] : getop(ir, st, 2))
        end
        old = operands(ir, arm_t)
        replace_stmt!(ir, arm_t, K"result", old..., outs...)
    end
    set_type!(ir, T, Any)
    # post-try: extracts (when tupled) + the unconditional no-op stores
    total_vals = exist_arity + m
    anchor = T
    if exist_arity == 1 && counts[T.id] != 0
        e0 = insert_after!(ir, anchor, K"extract", op_stmt(T), op_inline(1);
                           type = Any)
        replace_uses_where!(u -> u != e0, ir, T => op_stmt(e0))
        anchor = e0
    end
    for (i, (cell, _, _, _)) in enumerate(plans)
        v = if total_vals == 1
            op_stmt(T)
        else
            e = insert_after!(ir, anchor, K"extract", op_stmt(T),
                              op_inline(exist_arity + i); type = Any)
            anchor = e
            op_stmt(e)
        end
        anchor = insert_after!(ir, anchor, K"cell_set", op_stmt(cell), v;
                               type = Nothing)
    end
    return m
end

# ---------------------------------------------------------------------------
# Capture promotion (§5.7): shared cells read inside deferred regions.
#
# A `cell_shared` cell whose reads cross a deferred activation boundary is a
# CAPTURE of the variable; value-capture (rewriting the in-deferred reads to
# the value the cell holds at each closure-creation site) is legal iff
#
#   (a) no write (`cell_set`/`cell_new`) to the cell inside ANY deferred
#       region — a lambda storing to its capture needs the shared container;
#   (b) for EACH home-frame closure site C whose subtree reads the cell: no
#       write can execute after C — same-activation forward order
#       (`comes_before` + `_may_reach`, sibling-if-arms exclusive) plus the
#       multi-shot backedge rule (a write sharing a loop with C executes
#       again on the next iteration and IS observable) — cancelled only when
#       the CELL ITSELF is declared inside that shared loop (a fresh cell —
#       hence a fresh shared box — per iteration; this is the structural
#       form of the sidecar's "re-declared inside the loop" rule. A
#       `cell_new` does NOT cancel: on a shared cell it re-undefines the ONE
#       box every extant closure aliases, so it is itself an observable
#       write and is treated as one);
#   (c) a single defined value reaches each C — joins included — judged by
#       the STANDARD fixpoint itself: the candidate is speculatively demoted
#       to a frame cell on a throwaway copy of the IR, one probe `cell_get`
#       per site is planted at the creation point (held by a recognizable
#       marker call, exactly the sidecar's device relocated onto real IR),
#       and the fixpoint runs WITHOUT definedness-as-data. A probe the
#       passes resolve to a reaching definition proves definite assignment
#       at C and names the value; an unresolved probe means maybe-undef or
#       an ambiguous reaching store — the cell keeps the shared container
#       and `UndefVarError` stays a use-time error.
#
# The commit then mirrors the judged copy on the real IR: one `cell_get`
# probe before each site, every in-deferred read rewritten to its site's
# probe (a legal by-visibility capture — the interpreter/materializer
# snapshot the probe's value), and the cell demoted to frame class. The
# NEXT fixpoint rounds resolve the probes through the identical machinery
# that judged them, and a fully-resolved cell disappears. Residual
# `cell_shared` cells are true shared captures.
#
# `boundary` is the await seam: `:resume` capture legality is a LIVENESS
# question (the frame snapshot at the suspension point — values live across
# the resume edge — instead of the temporal no-store-after-creation rule
# above); it shares (c)'s judge but replaces (b). Documented, not
# implemented.
# ---------------------------------------------------------------------------

"Marker callee holding capture probes in the judgment copy (never emitted
into committed IR; unknown effects, so no pass deletes or reorders it)."
struct CaptureProbe end
const CAPTURE_PROBE = CaptureProbe()

# The home-frame closure site containing in-deferred use `g` of a cell whose
# activation root is `home`: the owner of the OUTERMOST deferred region on
# g's region ancestry strictly below `home`. NULL when the walk meets a
# non-deferred activation boundary or never reaches `home`.
function _home_site(ir::IR, g::StmtId, home::RegionId)
    r = stmt_region(ir, g)
    site = NULL_STMT
    while !isnull(r)
        r == home && return site
        reg = getregion(ir, r)
        if reg.activation === ACT_DEFERRED
            site = reg.owner
        elseif reg.activation !== ACT_IMMEDIATE
            return NULL_STMT             # resume boundary: not this pass's domain
        end
        isnull(reg.parent) && return NULL_STMT
        r = reg.parent
    end
    return NULL_STMT
end

struct CapturePlan
    cell::StmtId
    sites::Vector{StmtId}                       # home-frame closure ops
    indef::Vector{Pair{StmtId,Int}}             # in-deferred get => site index
end

# Scratch copy for the (c) judgment: same statement/region/constant ids, no
# extension columns (the judge needs none, and provenance-class columns can
# reference foreign graphs a deep copy must not drag along).
function _capture_scratch(ir::IR)
    b = ir.body
    nb = IRBody(NOCOLS)
    nb.len = Int(b.len)
    nb.kind = copy(b.kind); nb.ops = copy(b.ops); nb.operands = copy(b.operands)
    nb.type = copy(b.type); nb.flag = copy(b.flag); nb.debug = copy(b.debug)
    nb.region = copy(b.region)
    nb.constants = copy(b.constants); nb.constmap = copy(b.constmap)
    nb.globals = copy(b.globals); nb.globalmap = copy(b.globalmap)
    regs = Region[Region(r.kind, r.activation, r.owner, r.parent, copy(r.args),
                         r.cond, r.negated, r.first, r.last, r.dead)
                  for r in ir.regions]
    e = ir.edit
    return IR{typeof(NOCOLS)}(BodyOwner(layout(ir), 0), nb, regs,
                              copy(ir.argtypes), copy(ir.sptypes), ir.valid_worlds,
                              e === nothing ? nothing :
                                  EditState(copy(e.next), copy(e.prev), copy(e.okey)),
                              Pair{StmtId,Operand}[], AnalysisCache(),
                              Dict{Symbol,Any}(:name => get(ir.meta, :name, :capture_judge)))
end

"""
    promote_capture_cells!(ir; boundary = :deferred) -> Int

Value-capture promotion for `cell_shared` cells read inside deferred regions
(see the block comment above: criteria (a)/(b) checked structurally, (c)
judged by the standard fixpoint on a scratch copy without
definedness-as-data). Commits by planting one probe `cell_get` before each
closure site, rewriting the in-deferred reads to it, and demoting the cell
to frame class; later fixpoint rounds finish the home promotion. Editable
state. Returns the number of cells demoted.
"""
function promote_capture_cells!(ir::IR; boundary::Symbol = :deferred)
    check_state(ir, LAYOUT_EDITABLE, "promote_capture_cells!")
    boundary === :deferred ||
        error("promote_capture_cells!: only the :deferred boundary is implemented ",
              "(:resume = live-at-suspension snapshot, the await seam)")
    flush_renames!(ir)
    # fast no-op on closure-free IR: no deferred region, no candidates
    any(r -> !r.dead && r.activation === ACT_DEFERRED, ir.regions) || return 0

    # ---- candidates: (a), site mapping, (b) ---------------------------------
    plans = CapturePlan[]
    for c in collect(each_stmt(ir))
        is_tombstone(ir, c) && continue
        stmt_kind(ir, c) === K"cell_shared" || continue
        home = activation_root(ir, stmt_region(ir, c))
        writes = StmtId[]                  # home cell_set / cell_new
        indefgets = StmtId[]
        ok = true
        each_ssa_use(ir) do site, used
            (ok && used == c) || return
            site isa StmtOperand || (ok = false; return)
            u = site.user
            is_tombstone(ir, u) && return
            k = stmt_kind(ir, u)
            crossing = activation_root(ir, stmt_region(ir, u)) != home
            if !(k === K"cell_set" || k === K"cell_get" ||
                 k === K"cell_new" || k === K"cell_isdefined") || site.opidx != 1
                ok = false                 # escape / value use
            elseif k === K"cell_get"
                crossing ? push!(indefgets, u) : nothing
            elseif k === K"cell_isdefined"
                crossing && (ok = false)   # call-time definedness observation
            else # cell_set / cell_new
                crossing && (ok = false)   # criterion (a)
                push!(writes, u)
            end
            return
        end
        (ok && !isempty(indefgets)) || continue
        # site per in-deferred read; refuse on non-deferred boundaries
        sites = StmtId[]
        indef = Pair{StmtId,Int}[]
        for g in indefgets
            C = _home_site(ir, g, home)
            isnull(C) && (ok = false; break)
            si = findfirst(==(C), sites)
            si === nothing && (push!(sites, C); si = length(sites))
            push!(indef, g => si)
        end
        ok || continue
        # criterion (b) at every site, against every home write
        declr = stmt_region(ir, c)
        for C in sites
            for st in writes
                X = _innermost_shared_body(ir, st, C)
                if !isnull(X) && !is_ancestor(ir, X, declr)
                    ok = false; break      # backedge hazard, no fresh cell in X
                end
                if comes_before(ir, C, st) && _may_reach(ir, C, st)
                    ok = false; break      # a write can execute after C
                end
            end
            ok || break
        end
        ok || continue
        push!(plans, CapturePlan(c, sites, indef))
    end
    isempty(plans) && return 0

    # ---- criterion (c): the fixpoint judges a scratch copy ------------------
    work = _capture_scratch(ir)
    for (ci, p) in enumerate(plans)
        for (g, _) in p.indef
            replace_uses_where!(_ -> true, work, g => vop(work, nothing))
            delete_stmt!(work, g)
        end
        replace_stmt!(work, p.cell, K"cell", operands(work, p.cell)...)
        for (si, C) in enumerate(p.sites)
            pg = insert_before!(work, C, K"cell_get", op_stmt(p.cell); type = Any)
            insert_before!(work, C, K"call", vop(work, CAPTURE_PROBE),
                           vop(work, (ci, si)), op_stmt(pg); type = Any)
        end
    end
    compact!(work)
    promote_fixpoint!(work; include_undef = false, capture = false)
    unresolved = Set{Tuple{Int,Int}}()     # probes deleted with dead arms are vacuous
    for s in each_stmt(work)
        stmt_kind(work, s) === K"call" || continue
        nops(work, s) == 3 || continue
        operand_static_value(work, getop(work, s, 1)) === CAPTURE_PROBE || continue
        key = operand_static_value(work, getop(work, s, 2))::Tuple{Int,Int}
        o = getop(work, s, 3)
        if optag(o) == TAG_STMT && stmt_kind(work, asstmt(o)) === K"cell_get"
            push!(unresolved, key)
        end
    end

    # ---- commit --------------------------------------------------------------
    promoted = 0
    for (ci, p) in enumerate(plans)
        any(si -> (ci, si) in unresolved, 1:length(p.sites)) && continue
        probes = StmtId[insert_before!(ir, C, K"cell_get", op_stmt(p.cell); type = Any)
                        for C in p.sites]
        for (g, si) in p.indef
            replace_uses_where!(_ -> true, ir, g => op_stmt(probes[si]))
            delete_stmt!(ir, g)
        end
        replace_stmt!(ir, p.cell, K"cell", operands(ir, p.cell)...)
        _trace!(:capture_value, p.cell, p.cell)
        promoted += 1
    end
    return promoted
end

"""
    promote_fixpoint!(ir; stmt_value = nothing, include_undef = true,
                      capture = true) -> Int

The joint cell-promotion fixpoint (§6 join completeness): dominating-store
promotion, single-region promotion, then the editable join passes
(definedness-as-data, arm-join sinking, island phis, loop carrying, capture
promotion) with a `compact!` per round, iterated to quiescence. Returns
total promotions.

`include_undef = false` skips `promote_undef_cells!`: consumers for whom
maybe-undef memory must stay OBSERVABLY memory (lowering's closure-capture
analysis — a capture of a maybe-undef variable must keep the shared cell so
UndefVarError surfaces at use time) run the fixpoint without the
definedness-as-data split; every remaining pass only ever rewrites reads
covered by stores on all paths.

`capture = false` skips `promote_capture_cells!` — used by that pass's own
scratch-copy judgment (whose candidates are already demoted, so the nested
run would find none; the flag makes the non-recursion explicit).
"""
function promote_fixpoint!(ir::IR; stmt_value = nothing, include_undef::Bool = true,
                           capture::Bool = true)
    total = 0
    while true
        c = promote_cells!(ir)
        c += promote_block_cells!(ir)
        editable(ir)
        include_undef && (c += promote_undef_cells!(ir))
        c += promote_arm_cells!(ir)
        c += promote_try_cells!(ir)
        c += promote_island_cells!(ir)
        c += promote_loop_cells!(ir; stmt_value)
        capture && (c += promote_capture_cells!(ir))
        compact!(ir)
        total += c
        c == 0 && break
    end
    return total
end
