# Precise closure-capture analysis (the julia#15276 class): decide
# boxed-vs-value capture per variable with the SHARED UnifiedIR mem2reg
# machinery instead of the syntactic assigned-once rule.
#
# `analyze_def_and_use!` (binding_analysis.jl, flisp parity) runs first and
# keeps its verdicts; this pass runs after it and only WIDENS `unboxed` — a
# monotone improvement over stock, never a regression. The enclosing body is
# lowered to throwaway analysis IR with REAL `closure` region ops (each
# creation site's deferred body is its capture footprint: candidate reads,
# plus synthetic stores for lambda-written candidates), candidates as
# `cell_shared` cells, and the shared fixpoint
# (`UnifiedIR.promote_fixpoint!`, the same passes `Compiler.Unified` runs)
# decides captures STRUCTURALLY through `promote_capture_cells!` — the
# criterion (a)/(b)/(c) machinery lives inside that pass now, not in a
# sidecar pre-check. The per-variable verdict is read off the promoted IR:
# a candidate whose `cell_shared` survives with in-deferred uses must stay
# shared; a resolved candidate's reads were rewritten to value captures.
# Maybe-undef variables keep the shared container (the fixpoint runs WITHOUT
# definedness-as-data), preserving use-time UndefVarError semantics.
#
# Any unsupported form bails to the stock verdicts (`UnsupportedForm` —
# fidelity first).

"Rethrow analysis-internal errors instead of falling back (test/debug hook)."
const ACP_STRICT = Ref(false)

"""
Observation hook for the analysis IR (demo/debug; `nothing` = off). When set
to a callable it receives `(phase, lam, ir)` with `phase in (:before,
:after)` — the capture-analysis UnifiedIR of `lam` as emitted, and again
after the shared mem2reg fixpoint has run (site markers resolved or left as
`cell_get`s). See UnifiedIR/demo/capture_zoo.jl.
"""
const ACP_TRACE = Ref{Any}(nothing)

# the UnifiedIR instance the backend emits into (module constant: the
# `UIR.K"..."` kind literals below resolve at macro-expansion time)
const UIR = UnifiedBackend.UnifiedIR

"""
    analyze_captures_precise!(ctx::VariableAnalysisContext, ex)

Entry point, called from `resolve_scopes` after `analyze_def_and_use!`.
Walks every lambda (each is "home" to its native locals) and widens
`BindingInfo.unboxed` where the mem2reg verdict proves value capture legal.
"""
function analyze_captures_precise!(ctx, ex)
    isempty(ctx.closure_bindings) && return nothing
    _acp_lambda!(ctx, ex)
    return nothing
end

function _acp_lambda!(ctx, lam)
    _acp_nested!(ctx, lam[3])              # inner lambdas: their own homes
    lam.is_toplevel_thunk && return nothing
    try
        _acp_analyze!(ctx, lam)
    catch err
        # Fidelity rule: analysis failure must never fail (or change)
        # lowering. UnsupportedForm is the legitimate bail (the emitter met a
        # form it does not model — syntactic verdicts stand); anything else
        # is an internal error, surfaced under ACP_STRICT (the test suite).
        if !(err isa UnifiedBackend.UnsupportedForm)
            ACP_STRICT[] && rethrow()
            @debug("capture analysis bailed", err)
        end
    end
    return nothing
end

function _acp_nested!(ctx, ex)
    k = kind(ex)
    if k == K"lambda"
        _acp_lambda!(ctx, ex)
    elseif !is_leaf(ex) && !is_quoted(ex)
        for c in children(ex)
            _acp_nested!(ctx, c)
        end
    end
    return nothing
end

# closure-creation sites directly in this frame (not under a nested lambda)
function _acp_sites!(ctx, ex, out::Vector{Int})
    k = kind(ex)
    if k == K"function_decl" || k == K"_opaque_closure"
        kind(ex[1]) == K"BindingId" && push!(out, Int(ex[1].var_id))
        for i in 2:numchildren(ex)
            kind(ex[i]) == K"lambda" || _acp_sites!(ctx, ex[i], out)
        end
    elseif k == K"lambda"
        return nothing
    elseif !is_leaf(ex) && !is_quoted(ex)
        for c in children(ex)
            _acp_sites!(ctx, c, out)
        end
    end
    return nothing
end

# criterion (a): drop candidates assigned anywhere under a nested lambda
function _acp_closure_stores!(ctx, ex, cand::Set{Int}, inlambda::Bool)
    k = kind(ex)
    if k == K"lambda"
        for c in children(ex)
            _acp_closure_stores!(ctx, c, cand, true)
        end
    elseif k == K"=" && numchildren(ex) == 2
        lhs = ex[1]
        inlambda && kind(lhs) == K"BindingId" && delete!(cand, Int(lhs.var_id))
        _acp_closure_stores!(ctx, ex[2], cand, inlambda)
    elseif k == K"function_decl" || k == K"_opaque_closure"
        inlambda && kind(ex[1]) == K"BindingId" && delete!(cand, Int(ex[1].var_id))
        for i in 2:numchildren(ex)
            _acp_closure_stores!(ctx, ex[i], cand, inlambda)
        end
    elseif !is_leaf(ex) && !is_quoted(ex)
        for c in children(ex)
            _acp_closure_stores!(ctx, c, cand, inlambda)
        end
    end
    return nothing
end

function _acp_analyze!(ctx, lam)
    body = lam[3]
    lb = lam.lambda_bindings
    siteids = Int[]
    _acp_sites!(ctx, body, siteids)
    isempty(siteids) && return nothing

    # candidates: boxed native captured locals/arguments of THIS frame
    cand = Set{Int}()
    for sid in siteids
        cb = get(ctx.closure_bindings, sid, nothing)
        cb === nothing && continue
        for lbm in cb.lambdas, (id, capt) in lbm.locals_capt
            capt || continue
            get(lb.locals_capt, id, true) === false || continue   # native here
            binfo = get_binding(ctx, id)
            binfo.kind in (:local, :argument) || continue
            is_boxed(binfo) || continue      # already unboxed: nothing to widen
            push!(cand, Int(id))
        end
    end
    isempty(cand) && return nothing
    # criterion (a) is a tree fact that feeds the EMISSION: candidates some
    # capturing lambda stores to get a synthetic in-deferred store in their
    # sites' footprint bodies, which promote_capture_cells! refuses
    # structurally. They remain in the analysis as cells — the same IR
    # decides their TYPED-container eligibility below.
    writable = copy(cand)
    _acp_closure_stores!(ctx, body, writable, false)   # removes survivors
    closure_written = setdiff(cand, writable)

    capture_sets = Dict{Int,Vector{Int}}()
    for sid in siteids
        cb = get(ctx.closure_bindings, sid, nothing)
        cb === nothing && continue
        s = Int[]
        for lbm in cb.lambdas, (id, capt) in lbm.locals_capt
            capt && Int(id) in cand && push!(s, Int(id))
        end
        capture_sets[sid] = sort!(unique!(s))
    end
    foreign = Set{Int}(Int(id) for (id, capt) in lb.locals_capt if capt)
    ana = UnifiedBackend.AnalysisState(cand, foreign, capture_sets, closure_written)
    ir, _, _, ectx = UnifiedBackend.emit_lambda(ctx, lam, :capture_analysis;
                                                analysis = ana)

    # undef-safety for typed containers, on the pristine (pre-fixpoint) IR:
    # v may use a container whose empty state is unobservable iff no read
    # can see undef — every home read/query is store-dominated (the §6 dense
    # checker `dominates_for_cell`, shared again), every capture is
    # store-dominated AT ITS CREATION SITE (in-deferred reads execute after
    # the site, and home stores never un-define), and every `cell_new` is in
    # declaration position. Only home-side stores count (the synthetic
    # in-deferred stores model criterion (a), not definedness).
    cellstmt2var = Dict{Int,Int}(Int(c.id) => v for (v, c) in ectx.cellmap
                                 if v in cand)
    cellhome = Dict{Int,UnifiedBackend.RegionId}(
        Int(c.id) => UIR.activation_root(ir, UIR.stmt_region(ir, c))
        for (v, c) in ectx.cellmap if v in cand)
    athome(u, cid) = UIR.activation_root(ir, UIR.stmt_region(ir, u)) == cellhome[cid]
    sets = Dict{Int,Vector{UnifiedBackend.StmtId}}(v => UnifiedBackend.StmtId[]
                                                   for v in cand)
    reads = Dict{Int,Vector{UnifiedBackend.StmtId}}(v => UnifiedBackend.StmtId[]
                                                    for v in cand)
    news = Dict{Int,Vector{UnifiedBackend.StmtId}}(v => UnifiedBackend.StmtId[]
                                                   for v in cand)
    sitereads = Dict{Int,Vector{UnifiedBackend.StmtId}}(v => UnifiedBackend.StmtId[]
                                                        for v in cand)
    for s in UIR.each_stmt(ir)
        k = UIR.stmt_kind(ir, s)
        (k === UIR.K"cell_set" || k === UIR.K"cell_get" ||
         k === UIR.K"cell_isdefined" || k === UIR.K"cell_new") || continue
        o = UIR.getop(ir, s, 1)
        UIR.optag(o) == UIR.TAG_STMT || continue
        cid = Int(UIR.asstmt(o).id)
        v = get(cellstmt2var, cid, nothing)
        v === nothing && continue
        if k === UIR.K"cell_set"
            athome(s, cid) && push!(sets[v], s)
        elseif k === UIR.K"cell_new"
            push!(news[v], s)
        else
            athome(s, cid) ? push!(reads[v], s) : push!(sitereads[v], s)
        end
    end
    # in-deferred reads are dominated iff a home store dominates the SITE
    # closure op containing them
    site_of = Dict{Int,UnifiedBackend.StmtId}()
    for (si, caps) in enumerate(ana.site_caps)
        C = ana.site_stmts[si]
        rs = UIR.owned_regions(ir, C)
        for r in rs
            for m in UIR.region_stmts(ir, r)
                site_of[Int(m.id)] = C
            end
        end
        _ = caps
    end
    undef_safe = Dict{Int,Bool}()
    for v in cand
        dominated(u) = any(st -> UIR.dominates_for_cell(ir, st, u), sets[v])
        ok = all(dominated, reads[v]) &&
             all(nw -> all(st -> nw.id < st.id, sets[v]), news[v])
        if ok
            for g in sitereads[v]
                C = get(site_of, Int(g.id), nothing)
                (C !== nothing && dominated(C)) || (ok = false; break)
            end
        end
        undef_safe[v] = ok
    end

    tr = ACP_TRACE[]
    tr === nothing || tr(:before, lam, ir)

    # criterion (c): the shared fixpoint (same machinery as Compiler.Unified)
    # runs promote_capture_cells! on the real closure regions, without
    # definedness-as-data — maybe-undef captures must stay memory
    UIR.promote_fixpoint!(ir; include_undef = false)

    tr === nothing || tr(:after, lam, ir)

    # verdicts, structurally: a candidate stays shared iff its cell_shared
    # SURVIVED the fixpoint with an in-deferred use (an unresolved capture
    # read/query). Resolved candidates were demoted and their in-deferred
    # reads rewritten to value captures; sites folded away with dead arms
    # impose no constraint (that closure is never created).
    cellcol = UIR.getattr(ir, :cellbind)
    value_ok = Dict{Int,Bool}(v => true for v in cand)
    for s in UIR.each_stmt(ir)
        UIR.stmt_kind(ir, s) === UIR.K"cell_shared" || continue
        v = cellcol[s]
        (v isa Int && v in cand) || continue
        home = UIR.activation_root(ir, UIR.stmt_region(ir, s))
        UIR.each_ssa_use(ir) do site, used
            used == s || return
            site isa UIR.StmtOperand || return
            if UIR.activation_root(ir, UIR.stmt_region(ir, site.user)) != home
                value_ok[v] = false
            end
            return
        end
    end
    for v in cand
        binfo = get_binding(ctx, v)
        if value_ok[v] && !(v in closure_written)
            binfo.unboxed = true
            continue
        end
        # stays shared: try to TYPE the unavoidable container. Requirements:
        # a lowering-time-provable value type (declared type resolving to a
        # constant, or the join of all store RHS literal types) and
        # undef-safety (a typed container's empty state can be unobservable —
        # e.g. isbits RefValue — so no read may ever see undef).
        binfo.kind === :local || continue      # argument entry types unknown
        undef_safe[v] || continue
        isdefined(Base, :RefValue) || continue # bootstrap-stage guard
        T = _acp_container_eltype(ctx, lam, v, binfo)
        T === nothing && continue
        binfo.box_type = Base.RefValue{T}
    end
    return nothing
end

# ---------------------------------------------------------------------------
# Typed-container value types (lowering-time provable only)
# ---------------------------------------------------------------------------

"Resolve a (post-scope-analysis) type expression to a constant Type, or nothing."
function _acp_resolve_type(ctx, ex)
    k = kind(ex)
    if k == K"Value"
        t = ex.value
        return t isa Type ? t : nothing
    elseif k == K"core" || k == K"top"
        m = k == K"core" ? Core : Base
        n = Symbol(ex.name_val::String)
        (isconst(m, n) && isdefined(m, n)) || return nothing
        t = getglobal(m, n)
        return t isa Type ? t : nothing
    elseif k == K"globalref"
        m = ex.mod::Module
        n = Symbol(ex.name_val::String)
        (isconst(m, n) && isdefined(m, n)) || return nothing
        t = getglobal(m, n)
        return t isa Type ? t : nothing
    elseif k == K"BindingId"
        b = get_binding(ctx, ex.var_id)
        b.kind === :global || return nothing
        m = b.mod::Module
        n = Symbol(b.name)
        (isconst(m, n) && isdefined(m, n)) || return nothing
        t = getglobal(m, n)
        return t isa Type ? t : nothing
    end
    return nothing
end

"""
Value type for v's shared container: the declared type when it resolves to a
constant (declared-type stores are converted before the write, so the
container invariant holds), else the join of ALL store RHS types when every
one is a literal (then the container invariant holds because the stored
values ARE those literals). One concrete type or nothing.
"""
function _acp_container_eltype(ctx, lam, v::Int, binfo)
    if binfo.type !== nothing
        t = _acp_resolve_type(ctx, binding_type_ex(ctx, binfo))
        return (t isa Type && t !== Any && isconcretetype(t)) ? t : nothing
    end
    ts = Set{Type}()
    ok = _acp_store_types!(ctx, lam[3], v, ts)
    (ok && length(ts) == 1) || return nothing
    T = first(ts)
    return isconcretetype(T) ? T : nothing
end

# collect RHS literal types of every store to v under `ex` (INCLUDING nested
# lambda bodies — a miss would be a container-type unsoundness); false when
# any store's type is not syntactically known
function _acp_store_types!(ctx, ex, v::Int, ts::Set{Type})
    k = kind(ex)
    if k == K"=" && numchildren(ex) == 2
        lhs = ex[1]
        if kind(lhs) == K"BindingId" && Int(lhs.var_id) == v
            rhs = ex[2]
            rk = kind(rhs)
            if JuliaSyntax.is_literal(rk) || rk == K"Value"
                push!(ts, typeof(rhs.value))
            else
                return false
            end
        end
        return _acp_store_types!(ctx, ex[2], v, ts)
    elseif k == K"function_decl" || k == K"_opaque_closure"
        kind(ex[1]) == K"BindingId" && Int(ex[1].var_id) == v && return false
        for i in 2:numchildren(ex)
            _acp_store_types!(ctx, ex[i], v, ts) || return false
        end
        return true
    elseif is_leaf(ex) || k == K"inert" || k == K"inert_syntaxtree" || k == K"quote"
        return true
    else
        for c in children(ex)
            _acp_store_types!(ctx, c, v, ts) || return false
        end
        return true
    end
end
