# Transfer functions and interprocedural call inference for the UnifiedIR
# inference port. The frame/state walker lives in uinference.jl.

# ---------------------------------------------------------------------------
# Effects masks (§8.2 vocabulary; composition per §3.3/§5.1 rule 5)
# ---------------------------------------------------------------------------

const EFFECTS_ALL = UnifiedIR.FLAG_CONSISTENT | UnifiedIR.FLAG_EFFECT_FREE |
                    UnifiedIR.FLAG_NOTHROW | UnifiedIR.FLAG_TERMINATES
const EFFECTS_NONE = UInt32(0)
# a guaranteed-throw op keeps everything except NOTHROW (stock EFFECTS_THROWS)
const EFFECTS_THROWS = EFFECTS_ALL & ~UnifiedIR.FLAG_NOTHROW

"""
    UResult

Interprocedural result: rettype lattice element + effects mask (the four
`UnifiedIR.FLAG_*` bits of `EFFECTS_ALL`). This is the `st.cache` value.
"""
struct UResult
    rt::Any
    effects::UInt32
    UResult(@nospecialize(rt), effects::UInt32) = new(rt, effects)
end

"CC.Effects -> UnifiedIR flag mask."
function effects_mask(e::CC.Effects)
    m = EFFECTS_NONE
    CC.is_consistent(e)  && (m |= UnifiedIR.FLAG_CONSISTENT)
    CC.is_effect_free(e) && (m |= UnifiedIR.FLAG_EFFECT_FREE)
    CC.is_nothrow(e)     && (m |= UnifiedIR.FLAG_NOTHROW)
    CC.is_terminates(e)  && (m |= UnifiedIR.FLAG_TERMINATES)
    return m
end

function builtin_effects_mask(@nospecialize(f), argl::Vector{Any}, @nospecialize(rt))
    f isa Core.Builtin || return EFFECTS_NONE
    if f isa Core.IntrinsicFunction
        return try
            effects_mask(CC.intrinsic_effects(f, argl))
        catch
            EFFECTS_NONE
        end
    end
    return try
        effects_mask(CC.builtin_effects(CC.fallback_lattice, f, argl,
                                        CC.widenconst(rt)))
    catch
        EFFECTS_NONE
    end
end

"Effects of evaluating a statement's own operands (non-const global reads)."
function operand_effects_mask(fr::Frame, s::StmtId)
    ir = fr.ir
    m = EFFECTS_ALL
    for i in 1:UnifiedIR.nops(ir, s)
        o = UnifiedIR.getop(ir, s, i)
        if UnifiedIR.optag(o) == UnifiedIR.TAG_GLOBAL
            g = ir.body.globals[UnifiedIR.payload(o)]
            (isconst(g.mod, g.name) && isdefined(g.mod, g.name)) ||
                (m &= UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_TERMINATES)
        end
    end
    return m
end

"Record statement effects and fold them into the frame accumulator."
function note_effects!(fr::Frame, s::StmtId, mask::UInt32)
    mask &= operand_effects_mask(fr, s)
    fr.stmt_effects[s.id] = mask
    fr.effects &= mask
    return nothing
end

# ---------------------------------------------------------------------------
# Plain-statement transfer functions
# ---------------------------------------------------------------------------

# Identify the refinement subject of a value operand: a cell (through a
# fresh cell_get) or the SSA statement itself. Returns nothing for
# non-refinable operands (constants, globals).
function cond_subject(fr::Frame, o::UnifiedIR.Operand)
    UnifiedIR.optag(o) == UnifiedIR.TAG_STMT || return nothing
    sid = UnifiedIR.asstmt(o)
    if UnifiedIR.stmt_kind(fr.ir, sid) === K"cell_get"
        cellid = UnifiedIR.asstmt(UnifiedIR.getop(fr.ir, sid, 1)).id
        return (:cell, cellid)
    end
    return (:stmt, sid.id)
end

function transfer(fr::Frame, s::StmtId, k::UnifiedIR.Kind)
    rt, eff = _transfer(fr, s, k)
    note_effects!(fr, s, eff)
    return rt
end

"Meet a caller argument lattice with a callee-side conditional type."
function meet_cond(@nospecialize(argt), @nospecialize(ct))
    lat = CC.fallback_lattice
    try
        if ct isa Type
            return CC.tmeet(lat, argt, ct)
        elseif CC.:⊑(lat, ct, argt)
            return ct
        end
    catch
    end
    return ct
end

"""Translate a callee's `UInterCond` return into a caller-local `UCond` (the
from_interconditional port): positional args start at operand `firstargop`,
so callee parameter `slot` maps to operand `firstargop + slot - 1`. Widens to
Bool when the caller's operand in that position is not a refinable subject."""
function apply_intercond(fr::Frame, s::StmtId, firstargop::Int, r::UResult)
    rt = r.rt
    rt isa UInterCond || return r
    ir = fr.ir
    opidx = firstargop + rt.slot - 1
    (firstargop <= opidx && opidx <= UnifiedIR.nops(ir, s)) ||
        return UResult(Bool, r.effects)
    o = UnifiedIR.getop(ir, s, opidx)
    subj = cond_subject(fr, o)
    subj === nothing && return UResult(Bool, r.effects)
    argt = widenucond(opl(fr, o))
    return UResult(UCond(subj, meet_cond(argt, rt.thentype),
                         meet_cond(argt, rt.elsetype)), r.effects)
end

function _transfer(fr::Frame, s::StmtId, k::UnifiedIR.Kind)
    ir = fr.ir
    if k === K"call"
        cond = conditional_call(fr, s)
        cond === nothing || return (cond, EFFECTS_ALL)
        args = Any[widenucond(a) for a in opls(fr, s, 1)]
        r = infer_call(fr, args)
        r = apply_intercond(fr, s, 1, r)
        get(ENV, "UIR_DEBUG", "") == "1" && println("DBG call %", s.id, " args=", args, " -> ", r.rt)
        maybe_typeassert_refine!(fr, s, args, r.rt)
        return (r.rt, r.effects)
    elseif k === K"invoke"
        tl = opl(fr, UnifiedIR.getop(ir, s, 1))
        args = Any[widenucond(a) for a in opls(fr, s, 2)]
        r = infer_invoke_target(fr, tl, args)
        r = apply_intercond(fr, s, 2, r)
        return (r.rt, r.effects)
    elseif k === K"intrinsic"
        args = opls(fr, s, 1)
        f = CC.singleton_type(args[1])
        f === nothing && args[1] isa CC.Const && (f = (args[1]::CC.Const).val)
        f === nothing && return (Any, EFFECTS_NONE)
        argl = Any[widenucond(a) for a in args[2:end]]
        rt = try
            CC.builtin_tfunction(fr.st.cfg.interp, f, argl, nothing)
        catch
            Any
        end
        return (rt, builtin_effects_mask(f, argl, rt))
    elseif k === K"extract"
        vl = widenucond(opl(fr, UnifiedIR.getop(ir, s, 1)))
        idx = Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, s, 2))::Int64)
        argl = Any[vl, CC.Const(idx)]
        rt = CC.builtin_tfunction(fr.st.cfg.interp, Core.getfield, argl, nothing)
        return (rt, builtin_effects_mask(Core.getfield, argl, rt))
    elseif k === K"select"
        c = opl(fr, UnifiedIR.getop(ir, s, 1))
        a = opl(fr, UnifiedIR.getop(ir, s, 2))
        b = opl(fr, UnifiedIR.getop(ir, s, 3))
        c isa CC.Const && c.val === true && return (a, EFFECTS_ALL)
        c isa CC.Const && c.val === false && return (b, EFFECTS_ALL)
        return (CC.tmerge(CC.fallback_lattice, widenucond(a), widenucond(b)), EFFECTS_ALL)
    elseif k === K"refine" || k === K"value"
        return (opl(fr, UnifiedIR.getop(ir, s, 1)), EFFECTS_ALL)
    elseif k === K"globalref"
        return (opl(fr, UnifiedIR.getop(ir, s, 1)), EFFECTS_ALL)  # operand mask covers it
    elseif k === K"new"
        return transfer_new(fr, s)
    elseif k === K"splatnew"
        return transfer_splatnew(fr, s)
    elseif k === K"foreigncall"
        if UnifiedIR.nops(ir, s) >= 1
            m1 = opl(fr, UnifiedIR.getop(ir, s, 1))
            if m1 isa CC.Const && m1.val === FOREIGNGLOBAL_MARKER
                # Expr(:foreignglobal, name): the cglobal lowering (stock's
                # abstract_eval_foreignglobal — always Ptr{Cvoid})
                return (Ptr{Cvoid}, EFFECTS_NONE)
            end
        end
        UnifiedIR.nops(ir, s) >= 2 || return (Any, EFFECTS_NONE)
        rtl = opl(fr, UnifiedIR.getop(ir, s, 2))
        T = rtl isa CC.Const ? rtl.val : nothing
        mi = get(ir.meta, :mi, nothing)
        if T !== nothing && mi isa Core.MethodInstance
            # sparam-dependent ccall types instantiate in the mi environment
            # (the full sp_type_rewrap port): unsafe_wrap's Array{T,1} etc.
            rt = try
                CC.sp_type_rewrap(T, mi, true)
            catch
                nothing
            end
            rt === nothing || return (rt, EFFECTS_NONE)
        end
        return (foreigncall_rt(T), EFFECTS_NONE)
    elseif k === K"isdefined_global"
        # a binding can become defined later: not consistent
        return (Bool, UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_NOTHROW |
                      UnifiedIR.FLAG_TERMINATES)
    elseif k === K"cell_isdefined"
        return (Bool, EFFECTS_ALL)
    elseif k === K"boundscheck"
        # value depends on the inlining context: not consistent
        return (Bool, UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_NOTHROW |
                      UnifiedIR.FLAG_TERMINATES)
    elseif k === K"cell" || k === K"cell_shared"
        return (Any, EFFECTS_ALL)      # the cell token
    elseif k === K"cell_get"
        cellid = UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1)).id
        eff = cellid in fr.newed_cells ? EFFECTS_ALL & ~UnifiedIR.FLAG_NOTHROW :
              EFFECTS_ALL              # maybe-undef read can throw UndefVarError
        return (cell_lattice(fr, cellid), eff)
    elseif k === K"cell_set"
        cellop = UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1))
        cellid = cellop.id
        # a store kills any active Conditional/typeassert refinement of the cell
        for rm in fr.refinements
            delete!(rm, (:cell, cellid))
        end
        vl = widenucond(opl(fr, UnifiedIR.getop(ir, s, 2)))
        old = get(fr.celltypes, cellid, nothing)
        if old === nothing
            fr.celltypes[cellid] = vl
            fr.cells_changed = true
        else
            new = CC.tmerge(CC.fallback_lattice, old, vl)
            if !lat_eq(new, old)
                fr.celltypes[cellid] = new
                fr.cells_changed = true
            end
        end
        # flow-sensitive overlay (the VarTable port): later reads on this path
        # see the just-written value; joins fall back to the monotone celltypes.
        # The walker pushes it as a refinement scope; edge propagation through
        # `blockrefs` joins it across cfg edges, and the kill above removes it
        # on reassignment. Shared cells are excluded (closures may write).
        shared = UnifiedIR.stmt_kind(ir, cellop) === K"cell_shared"
        shared || (fr.pending_refine = (:cell, cellid) => vl)
        # writes to closure-shared cells are observable mutations
        eff = shared ? UnifiedIR.FLAG_NOTHROW | UnifiedIR.FLAG_TERMINATES : EFFECTS_ALL
        return (nothing, eff)
    elseif k === K"cell_new"
        return (nothing, EFFECTS_ALL)
    elseif k === K"throw_undef_if_not"
        condl = opl(fr, UnifiedIR.getop(ir, s, 1))
        condl isa CC.Const && condl.val === true && return (nothing, EFFECTS_ALL)
        # guaranteed throw poisons the tail (walker's dead-tail rule)
        condl isa CC.Const && condl.val === false && return (Union{}, EFFECTS_THROWS)
        return (nothing, EFFECTS_THROWS)
    elseif k === K"gc_preserve_end" || k === K"latestworld" || k === K"coverage_effect"
        # not independently removable, but no observable effect of their own
        return (nothing, UnifiedIR.FLAG_NOTHROW | UnifiedIR.FLAG_TERMINATES)
    elseif k === K"gc_preserve_begin"
        return (Any, UnifiedIR.FLAG_NOTHROW | UnifiedIR.FLAG_TERMINATES)
    elseif k === K"copyast"
        # fresh mutable copy each evaluation: not consistent
        return (Any, UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_NOTHROW |
                     UnifiedIR.FLAG_TERMINATES)
    elseif k === K"method_def" || k === K"cfunction" || k === K"new_opaque_closure"
        return (Any, EFFECTS_NONE)
    else
        return (Any, EFFECTS_NONE)     # unknown/external kind: opacity contract §8.2
    end
end

"ccall return-position semantics (the sp_type_rewrap port): Ref{T} means a
rooted T; Ref{Any} returns are invalid; free typevars degrade to Any."
function foreigncall_rt(@nospecialize(T))
    T isa Type || return Any
    T === Union{} && return Union{}
    if T isa DataType && T.name === Ref.body.name
        T = T.parameters[1]
        T === Any && return Union{}     # a return type of Ref{Any} is invalid
        T isa TypeVar && (T = T.ub)
    end
    T isa Type || return Any
    return CC.has_free_typevars(T) ? Any : T
end

"Back-propagate `typeassert(x, T)` as a refinement of x's subject (stock
SlotRefinement); the walker pushes `fr.pending_refine` for the region rest."
function maybe_typeassert_refine!(fr::Frame, s::StmtId, args::Vector{Any},
                                  @nospecialize(rt))
    length(args) == 3 || return nothing
    f = CC.singleton_type(args[1])
    f === nothing && args[1] isa CC.Const && (f = (args[1]::CC.Const).val)
    f === typeassert || return nothing
    rt === Union{} && return nothing
    subj = cond_subject(fr, UnifiedIR.getop(fr.ir, s, 2))
    subj === nothing && return nothing
    fr.pending_refine = subj => rt
    return nothing
end

"Produce UCond lattice elements for conditional-shaped calls (§10.3)."
function conditional_call(fr::Frame, s::StmtId)
    ir = fr.ir
    n = UnifiedIR.nops(ir, s)
    fo = UnifiedIR.getop(ir, s, 1)
    fl = opl(fr, fo)
    f = CC.singleton_type(fl)
    f === nothing && fl isa CC.Const && (f = fl.val)
    f === nothing && return nothing
    lat = CC.fallback_lattice
    if f === isa && n == 3
        vo = UnifiedIR.getop(ir, s, 2)
        vt = widenucond(opl(fr, vo))
        tl = opl(fr, UnifiedIR.getop(ir, s, 3))
        tl isa CC.Const && tl.val isa Type || return nothing
        T = tl.val
        rt = CC.builtin_tfunction(fr.st.cfg.interp, isa, Any[vt, tl], nothing)
        rt isa CC.Const && return rt                     # statically decided
        subj = cond_subject(fr, vo)
        subj === nothing && return rt
        thent = CC.tmeet(lat, vt, T)
        elset = CC.typesubtract(CC.widenconst(vt), T,
                                CC.InferenceParams().max_union_splitting)
        return UCond(subj, thent, elset)
    elseif f === (===) && n == 3
        ao = UnifiedIR.getop(ir, s, 2)
        bo = UnifiedIR.getop(ir, s, 3)
        al = widenucond(opl(fr, ao))
        bl = widenucond(opl(fr, bo))
        rt = CC.builtin_tfunction(fr.st.cfg.interp, ===, Any[al, bl], nothing)
        rt isa CC.Const && return rt
        # refine against a singleton side (x === nothing and friends).
        # NB: the sentinel must be distinct from the VALUE nothing — the
        # `x === nothing` pattern is the single most important client.
        for (co, cl, vo, vl) in ((ao, al, bo, bl), (bo, bl, ao, al))
            has_c = false
            local cval
            if cl isa CC.Const
                cval = cl.val
                has_c = true
            else
                stype = CC.singleton_type(cl)
                if stype !== nothing
                    cval = stype
                    has_c = true
                end
            end
            has_c || continue
            Base.issingletontype(typeof(cval)) || continue
            subj = cond_subject(fr, vo)
            subj === nothing && continue
            vt = widenucond(vl)
            thent = CC.tmeet(lat, vt, typeof(cval))
            thent === Union{} && (thent = typeof(cval))
            elset = CC.typesubtract(CC.widenconst(vt), typeof(cval),
                                    CC.InferenceParams().max_union_splitting)
            return UCond(subj, thent, elset)
        end
        return rt
    elseif f === (!) && n == 2
        cl = opl(fr, UnifiedIR.getop(ir, s, 2))
        cl isa UCond && return UCond(cl.subject, cl.elsetype, cl.thentype)
        return nothing
    elseif f === Core.ifelse && n == 4
        cl = opl(fr, UnifiedIR.getop(ir, s, 2))
        if cl isa CC.Const && cl.val isa Bool
            return opl(fr, UnifiedIR.getop(ir, s, cl.val ? 3 : 4))
        end
        return nothing
    end
    return nothing
end

# ---------------------------------------------------------------------------
# new / splatnew (the abstract_eval_new port)
# ---------------------------------------------------------------------------

function transfer_new(fr::Frame, s::StmtId)
    ir = fr.ir
    lat = CC.fallback_lattice
    tl = widenucond(opl(fr, UnifiedIR.getop(ir, s, 1)))
    local rt, isexact
    try
        rt, isexact = CC.instanceof_tfunc(tl, true)
    catch
        return (Any, EFFECTS_NONE)
    end
    rt === Union{} && return (Union{}, EFFECTS_THROWS)
    nargs = UnifiedIR.nops(ir, s) - 1
    ut = Base.unwrap_unionall(rt)
    (ut isa DataType && !isabstracttype(ut)) || return (rt isa Type ? rt : Any, EFFECTS_NONE)
    try
        ismut = ismutabletype(ut)
        fcount = CC.datatype_fieldcount(ut)
        (fcount === nothing || nargs > fcount) && return (rt, EFFECTS_NONE)
        # allocation with any undefined field is never consistent; mutable
        # allocation is not consistent (skip the NOTRETURNED refinement)
        consistent = fcount == nargs && !ismut
        nothrow = CC.isconcretedispatch(rt)
        ats = Vector{Any}(undef, nargs)
        anyrefine = false
        allconst = CC.isconcretedispatch(rt)
        for i in 1:nargs
            at = widenucond(opl(fr, UnifiedIR.getop(ir, s, i + 1)))
            ft = fieldtype(rt, i)
            nothrow && (nothrow = CC.:⊑(lat, at, ft))
            at = CC.tmeet(lat, at, ft)
            at === Union{} && return (Union{}, EFFECTS_THROWS)   # guaranteed TypeError
            if ismut && !isconst(rt, i)
                ats[i] = ft            # field may be mutated later
                allconst = false
                continue
            end
            allconst &= at isa CC.Const
            if !anyrefine
                anyrefine = CC.has_nontrivial_extended_info(lat, at) ||
                            CC.:⋤(lat, at, ft)
            end
            ats[i] = at
        end
        mask = UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_TERMINATES
        consistent && (mask |= UnifiedIR.FLAG_CONSISTENT)
        nothrow && (mask |= UnifiedIR.FLAG_NOTHROW)
        if allconst && fcount == nargs && consistent
            argvals = Vector{Any}(undef, nargs)
            for j in 1:nargs
                argvals[j] = (ats[j]::CC.Const).val
            end
            v = try
                CC.Const(ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32),
                               rt, argvals, UInt32(nargs)))
            catch
                nothing
            end
            v === nothing || return (v, mask)
        end
        if anyrefine || nargs > CC.datatype_min_ninitialized(rt)
            undefs = Union{Nothing,Bool}[false for _ in 1:nargs]
            if nargs < fcount
                for i in (nargs + 1):fcount
                    ft = fieldtype(rt, i)
                    push!(ats, ft)
                    push!(undefs, ft === Union{} ? true :
                          (isconcretetype(ft) && CC.datatype_pointerfree(ft) ?
                           false : nothing))
                end
            end
            return (CC.PartialStruct(lat, rt, undefs, ats), mask)
        end
        return (rt, mask)
    catch
        return (rt isa Type ? rt : Any, EFFECTS_NONE)
    end
end

function transfer_splatnew(fr::Frame, s::StmtId)
    ir = fr.ir
    lat = CC.fallback_lattice
    tl = widenucond(opl(fr, UnifiedIR.getop(ir, s, 1)))
    local rt, isexact
    try
        rt, isexact = CC.instanceof_tfunc(tl, true)
    catch
        return (Any, EFFECTS_NONE)
    end
    rt === Union{} && return (Union{}, EFFECTS_THROWS)
    res = rt isa Type ? rt : Any
    try
        nothrow = false
        if UnifiedIR.nops(ir, s) == 2 && CC.isconcretedispatch(rt) && !ismutabletype(rt)
            at = widenucond(opl(fr, UnifiedIR.getop(ir, s, 2)))
            n = fieldcount(rt)
            if at isa CC.Const && at.val isa Tuple && n == length(at.val::Tuple) &&
               all(i -> getfield(at.val::Tuple, i) isa fieldtype(rt, i), 1:n)
                nothrow = isexact
                res = CC.Const(ccall(:jl_new_structt, Any, (Any, Any), rt, at.val))
            elseif at isa CC.PartialStruct && CC.:⊑(lat, at, Tuple) && n > 0 &&
                   n == length(at.fields) && !CC.isvarargtype(at.fields[end]) &&
                   all(i -> CC.:⊑(lat, at.fields[i], fieldtype(rt, i)), 1:n)
                nothrow = isexact
                res = CC.PartialStruct(lat, rt, Union{Nothing,Bool}[false for _ in 1:n],
                                       Any[f for f in at.fields])
            end
        end
        mask = UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_TERMINATES
        u = Base.unwrap_unionall(rt)
        if u isa DataType && !ismutabletype(u)
            mask |= UnifiedIR.FLAG_CONSISTENT   # immutable allocation is consistent
        end
        nothrow && (mask |= UnifiedIR.FLAG_NOTHROW)
        return (res, mask)
    catch
        return (res, EFFECTS_NONE)
    end
end

# ---------------------------------------------------------------------------
# Interprocedural calls
# ---------------------------------------------------------------------------

const CONSTPROP_SRC_LIMIT = 250     # const_prop_entry_heuristic analog
# Frames per top-level query. Measured: the loading.jl giants
# (_include_from_serialized/compilecache/stale_cachefile) exhaust even 250k
# frames — a structural const-prop recompute cost, not a calibration issue —
# and deep IO chains (printstyled) become pathological beyond depth 128, so
# both knobs stay at the values the sweep timings were measured at; cutoffs
# resolve through native_fallback (or Any when the fallback is off).
const FRAME_BUDGET = 60_000

function infer_call(fr::Frame, args::Vector{Any})::UResult
    st = fr.st
    ftl = args[1]
    f = CC.singleton_type(ftl)
    if f === nothing && ftl isa CC.Const
        f = ftl.val
    end
    if f isa Core.Builtin
        if f === Core._apply_iterate
            return infer_apply(fr, args)
        elseif f === Core.invoke
            return infer_invoke(fr, args)
        elseif f === Core.throw || f === Core.throw_methoderror
            return UResult(Union{}, EFFECTS_THROWS)
        end
        # module-global reads: builtin_tfunction(sv=nothing) cannot consult
        # bindings; fold here (the abstract_eval_globalref port)
        if (f === getglobal || f === getfield) && 3 <= length(args) <= 5
            ml = args[2]; sl = args[3]
            if ml isa CC.Const && ml.val isa Module && sl isa CC.Const && sl.val isa Symbol
                M = ml.val; nm = sl.val
                if isconst(M, nm) && isdefined(M, nm)
                    return UResult(CC.Const(getglobal(M, nm)), EFFECTS_ALL)
                end
                bt = try
                    Core.get_binding_type(M, nm)
                catch
                    Any
                end
                return UResult(bt isa Type ? bt : Any,
                               UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_TERMINATES)
            end
        end
        argl = args[2:end]
        rt = try
            CC.builtin_tfunction(st.cfg.interp, f, argl, nothing)
        catch
            Any
        end
        return UResult(rt, builtin_effects_mask(f, argl, rt))
    end
    if f !== nothing && is_return_type_f(f)
        r = infer_return_type_call(fr, args)
        r === nothing || return r
        # stock's UNKNOWN: never descend into return_type's reflection body
        return UResult(Type, EFFECTS_NONE)
    end
    # union splitting (the abstract_call_gf_by_type port): small unions in
    # argument position dispatch per element and join — `<(::Union{Int32,
    # Int64}, 0)` must not fall into an abstract-signature match
    let r = maybe_union_split(fr, args)
        r === nothing || return r::UResult
    end
    # type callees (constructors) dispatch through Type{T}, not DataType
    ft = f === nothing ? CC.widenconst(ftl) : (f isa Type ? Type{f} : typeof(f))
    ft === Any && return UResult(Any, EFFECTS_NONE)
    ft === Union{} && return UResult(Union{}, EFFECTS_THROWS)
    argts = Vector{Any}(undef, length(args) - 1)
    for i in 2:length(args)
        a = args[i]
        if CC.isvarargtype(a)
            i == length(args) || return UResult(Any, EFFECTS_NONE)  # malformed
            argts[i - 1] = a
        else
            t = CC.widenconst(a)
            t === Union{} && return UResult(Union{}, EFFECTS_THROWS)  # unreachable call
            argts[i - 1] = t
        end
    end
    sig = try
        Tuple{ft, argts...}
    catch
        return UResult(Any, EFFECTS_NONE)
    end
    matches = try
        Base._methods_by_ftype(sig, st.cfg.max_methods, st.cfg.world)
    catch
        nothing
    end
    matches === nothing && return UResult(Any, EFFECTS_NONE)
    isempty(matches) && return UResult(Union{}, EFFECTS_THROWS)  # guaranteed MethodError
    rt = nothing
    fx = EFFECTS_ALL
    fully = true
    for match in matches
        r = infer_method(fr, match::Core.MethodMatch, args)
        rt = ⊔(st, rt, r.rt)
        fx &= r.effects
        fully &= (match::Core.MethodMatch).fully_covers
    end
    fully || (fx &= ~UnifiedIR.FLAG_NOTHROW)    # possible MethodError remains
    return UResult(rt === nothing ? Union{} : rt, fx)
end

"""Split top-level Union argument types (bounded by max_union_splitting
signature combinations) into separate `infer_call`s and join the results.
Returns nothing when no split applies. Split elements are strictly narrower
non-Union types, so the recursion terminates."""
function maybe_union_split(fr::Frame, args::Vector{Any})
    total = 1
    splitat = 0
    for i in 1:length(args)
        a = args[i]
        a isa Union || continue
        total *= length(CC.uniontypes(a))
        splitat == 0 && (splitat = i)
    end
    (splitat == 0 || total < 2) && return nothing
    total > CC.InferenceParams().max_union_splitting && return nothing
    st = fr.st
    rt = nothing
    fx = EFFECTS_ALL
    for elt in CC.uniontypes(args[splitat])
        sub = copy(args)
        sub[splitat] = elt
        r = infer_call(fr, sub)   # recurses to split any further union args
        fx &= r.effects
        r.rt === Union{} && continue   # per-element guaranteed throw
        rt = ⊔(st, rt, r.rt)
    end
    rt === nothing && return UResult(Union{}, EFFECTS_THROWS)
    return UResult(rt, fx)
end

# ---------------------------------------------------------------------------
# Core.Compiler.return_type (the return_type_tfunc port)
# ---------------------------------------------------------------------------

function is_return_type_f(@nospecialize(f))
    f === Core.Compiler.return_type && return true
    f === CC.return_type && return true
    isdefined(Base, :_return_type) && f === Base._return_type && return true
    return false
end

"The exactly-known type a Type-shaped native lattice pins, or nothing.
Covers `Type{T}` and this nightly's `TypeEgal{T}`/`TypeEq{T}` widenings."
function exact_type_param(@nospecialize(w))
    if CC.isType(w) || (isdefined(CC, :isTypeEgal) && CC.isTypeEgal(w)) ||
       (isdefined(CC, :isTypeEq) && CC.isTypeEq(w))
        p = w.parameters[1]
        (p isa Type && !CC.has_free_typevars(p)) && return p
    end
    return nothing
end

"""Fold `return_type(f, tt)` / `return_type(tt)` when the signature is known.
The runtime call *is* stock inference, so delegating the fold to
`Core.Compiler.return_type` reproduces the runtime answer exactly (stock's
own model of this call carries the same disclaimer)."""
function infer_return_type_call(fr::Frame, args::Vector{Any})
    # args[1] is return_type itself: `return_type(f, tt)` arrives as length-3
    # args, `return_type(tt)` as length-2
    (2 <= length(args) <= 3) || return nothing
    any(a -> CC.isvarargtype(a), args) && return nothing
    tt = args[end]
    local ttv
    if tt isa CC.Const
        ttv = tt.val
    else
        w = CC.widenconst(tt)
        ttv = exact_type_param(w)
        ttv === nothing && return nothing
    end
    (ttv isa DataType && ttv <: Tuple) || return nothing
    local sig
    if length(args) == 3
        aftl = args[2]
        aft = CC.singleton_type(aftl)
        aft === nothing && aftl isa CC.Const && (aft = (aftl::CC.Const).val)
        local ftt
        if aft !== nothing
            ftt = aft isa Type ? Type{aft} : typeof(aft)
        else
            w = CC.widenconst(aftl)
            p = exact_type_param(w)
            if p !== nothing
                ftt = Type{p}
            elseif isconcretetype(w) && !(w <: Core.Builtin)
                ftt = w
            else
                return nothing
            end
        end
        sig = try
            Tuple{ftt, ttv.parameters...}
        catch
            return nothing
        end
    else
        sig = ttv
    end
    rt = try
        Core.Compiler.return_type(sig)
    catch
        return nothing
    end
    return UResult(CC.Const(rt), EFFECTS_ALL)
end

# ---------------------------------------------------------------------------
# Core._apply_iterate (the abstract_apply port)
# ---------------------------------------------------------------------------

"""
    container_elements(x) -> (elems::Vector{Any}, exact::Bool) | nothing

The `precise_container_type` port: element lattices of an iterated argument.
`exact` means the runtime performs no user `iterate` calls (tuple-shaped
containers). The last element may be a `Vararg`. `nothing` = unknown shape.
"""
function container_elements(fr::Frame, @nospecialize(x))
    if x isa CC.PartialStruct
        widet = Base.unwrap_unionall(x.typ)
        if widet isa DataType &&
           (widet.name === Tuple.name || widet.name === CC._NAMEDTUPLE_NAME)
            return (Any[fl for fl in x.fields], true)
        end
    end
    if x isa CC.Const
        v = x.val
        if v isa Core.SimpleVector || v isa Tuple
            return (Any[CC.Const(v[i]) for i in 1:length(v)], true)
        elseif v isa NamedTuple
            return (Any[CC.Const(getfield(v, i)) for i in 1:nfields(v)], true)
        end
    end
    tti0 = CC.widenconst(x)
    tti = Base.unwrap_unionall(tti0)
    if tti isa DataType && tti.name === CC._NAMEDTUPLE_NAME
        # NamedTuple iterates as its Tuple parameter
        tp = tti.parameters[2]
        tp isa Type || return nothing
        tti0 = Base.rewrap_unionall(tp, tti0)
        tti = Base.unwrap_unionall(tti0)
    end
    if tti isa Union
        utis = CC.uniontypes(tti)
        elts = nothing
        for t in utis
            (t isa DataType && t <: Tuple && CC.isknownlength(t)) || return nothing
            ps = Any[Base.rewrap_unionall(p, tti0) for p in t.parameters]
            if elts === nothing
                elts = ps
            else
                length(ps) == length(elts) || return nothing
                for j in 1:length(ps)
                    elts[j] = CC.tmerge(CC.fallback_lattice, elts[j], ps[j])
                end
            end
        end
        return elts === nothing ? nothing : (elts, true)
    end
    if tti0 <: Tuple
        if tti0 isa DataType
            return (Any[p for p in tti0.parameters], true)
        elseif !(tti isa DataType)
            return (Any[Vararg{Any}], true)
        else
            len = length(tti.parameters)
            elts = Any[Base.rewrap_unionall(p, tti0) for p in tti.parameters]
            if len > 0 && CC.isvarargtype(tti.parameters[len])
                elts[len] = tti.parameters[len]   # keep the Vararg tail as-is
            end
            return (elts, true)
        end
    elseif tti0 === Core.SimpleVector
        return (Any[Vararg{Any}], false)
    elseif tti0 <: Array || tti0 <: GenericMemory
        et = try
            eltype(tti0)
        catch
            Any
        end
        return (Any[Vararg{et === Union{} ? Any : et}], false)
    end
    return nothing     # unknown iterable: degrade to Vararg{Any} + unknown effects
end

"""The abstract_iteration port: enumerate an iterated argument's element
lattices by running the `iterate` protocol abstractly. Phase 1 unrolls finite
iterators precisely (guaranteed-present elements only); phase 2 folds the
remainder into a `Vararg` tail at the widened state fixpoint. Returns
`Any[Union{}]` when iteration provably throws or cannot terminate."""
function iterate_elements(fr::Frame, @nospecialize(x))
    lat = CC.fallback_lattice
    itf = CC.Const(Base.iterate)
    r = infer_call(fr, Any[itf, x])
    sod = widenucond(r.rt)               # state-or-done, precise
    sodw = CC.widenconst(sod)
    sodw === Union{} && return Any[Union{}]   # not an iterator: throws
    elems = Any[]
    statetype = Union{}
    # phase 1: precise unroll while termination is impossible
    while true
        sodw === Nothing && return elems      # provably exhausted (exact)
        (Nothing <: sodw || length(elems) >= 32) && break
        (sodw isa DataType && sodw <: Tuple && !CC.isvatuple(sodw) &&
         length(sodw.parameters) == 2) || break
        nst, vt = try
            (CC.getfield_tfunc(lat, sod, CC.Const(2)),
             CC.getfield_tfunc(lat, sod, CC.Const(1)))
        catch
            break
        end
        # no new state information: the iterator cannot be finite (stock's
        # infinite-iteration rule — the apply never completes)
        CC.:⊑(lat, nst, statetype) && return Any[Union{}]
        push!(elems, vt)
        statetype = nst
        r = infer_call(fr, Any[itf, x, statetype])
        sod = widenucond(r.rt)
        sodw = CC.widenconst(sod)
    end
    # phase 2: widened tail to a state fixpoint
    valtype = Union{}
    statew = Union{}
    may_have_terminated = Nothing <: sodw
    guard = 0
    while valtype !== Any && (guard += 1) < 100
        nounion = try
            typeintersect(sodw, Tuple{Any,Any})
        catch
            Any
        end
        if nounion !== Union{} && !(nounion isa DataType)
            valtype = Any
            break
        end
        if nounion === Union{} || (nounion.parameters[1] <: valtype &&
                                   nounion.parameters[2] <: statew)
            # fixpoint (or the iterator failed / gave an invalid answer)
            if !CC.hasintersect(sodw, Nothing)
                # ...and cannot terminate during this loop
                may_have_terminated || return Any[Union{}]
                valtype = Union{}   # only completes if it ended before here
            end
            break
        end
        valtype = CC.tmerge(lat, valtype, nounion.parameters[1])
        statew = CC.tmerge(lat, statew, nounion.parameters[2])
        r = infer_call(fr, Any[itf, x, statew])
        sod = widenucond(r.rt)
        sodw = CC.widenconst(sod)
    end
    valtype === Union{} || push!(elems, Vararg{CC.widenconst(valtype)})
    return elems
end

"Core._apply_iterate(iterate, f, iters...): flatten precisely when possible."
function infer_apply(fr::Frame, args::Vector{Any})::UResult
    length(args) >= 3 || return UResult(Any, EFFECTS_NONE)
    fl = args[3]
    fl === Union{} && return UResult(Union{}, EFFECTS_THROWS)
    flat = Any[fl]
    exact = true
    precise = true
    for i in 4:length(args)
        a = args[i]
        if CC.isvarargtype(a)
            precise = false
            break
        end
        ce = container_elements(fr, a)
        if ce === nothing
            # not a tuple-shaped container: run the iterate protocol
            # abstractly (user iterate methods ⇒ inexact, unknown effects)
            elems = iterate_elements(fr, a)
            append!(flat, elems)
            exact = false
            continue
        end
        append!(flat, ce[1])
        exact &= ce[2]
    end
    if !precise
        flat = Any[fl, Vararg{Any}]
        exact = false
    else
        # fold a mid-list Vararg into a merged tail (stock's truncation rule)
        for k in 2:length(flat)
            if CC.isvarargtype(flat[k]) && k < length(flat)
                tail = CC.tuple_tail_elem(CC.fallback_lattice, CC.unwrapva(flat[k]),
                                          Any[flat[j] for j in (k + 1):length(flat)])
                resize!(flat, k)
                flat[k] = tail
                break
            end
        end
    end
    r = infer_call(fr, flat)
    # flattened positions do not map to caller operands: widen InterConditionals
    r.rt isa UInterCond && (r = UResult(Bool, r.effects))
    # non-tuple containers run user iterate methods with unknown effects
    return exact ? r : UResult(r.rt, EFFECTS_NONE)
end

# ---------------------------------------------------------------------------
# invoke (the abstract_invoke port)
# ---------------------------------------------------------------------------

"K\"invoke\": the first operand is a CONST CodeInstance/MethodInstance."
function infer_invoke_target(fr::Frame, @nospecialize(tl), args::Vector{Any})::UResult
    target = tl isa CC.Const ? tl.val : CC.singleton_type(tl)
    if target isa Core.CodeInstance
        return UResult(target.rettype,
                       effects_mask(CC.decode_effects(target.ipo_purity_bits)))
    elseif target isa Core.MethodInstance && target.def isa Method
        match = Core.MethodMatch(target.specTypes, target.sparam_vals,
                                 target.def::Method, true)
        return try
            infer_method(fr, match, args)
        catch
            UResult(Any, EFFECTS_NONE)
        end
    end
    isempty(args) && return UResult(Any, EFFECTS_NONE)
    return infer_call(fr, args)
end

widen_intercond(r::UResult) = r.rt isa UInterCond ? UResult(Bool, r.effects) : r

"`Core.invoke(f, types_or_method_or_ci, args...)` as a call (argument
positions shift under the builtin: InterConditionals widen)."
function infer_invoke(fr::Frame, args::Vector{Any})::UResult
    length(args) >= 3 || return UResult(Union{}, EFFECTS_THROWS)
    any(a -> CC.isvarargtype(a), args) && return UResult(Any, EFFECTS_NONE)
    ftl = args[2]
    ft = CC.widenconst(ftl)
    ft === Union{} && return UResult(Union{}, EFFECTS_THROWS)
    types = args[3]
    callargs = Any[ftl]
    append!(callargs, args[4:end])
    argts = Any[CC.widenconst(a) for a in args[4:end]]
    if types isa CC.Const
        v = types.val
        if v isa Core.CodeInstance
            return UResult(v.rettype,
                           effects_mask(CC.decode_effects(v.ipo_purity_bits)))
        elseif v isa Method
            argtype = Tuple{ft, argts...}
            return widen_intercond(invoke_match(fr, v, argtype, argtype, callargs))
        end
    end
    T, isexact = try
        CC.instanceof_tfunc(types, false)
    catch
        (Any, false)
    end
    isexact || return UResult(Any, EFFECTS_NONE)
    T === Union{} && return UResult(Union{}, EFFECTS_THROWS)
    unwrapped = Base.unwrap_unionall(T)
    (unwrapped isa DataType && unwrapped.name === Tuple.name) ||
        return UResult(Union{}, EFFECTS_THROWS)          # TypeError
    Base.isdispatchelem(ft) || return UResult(Any, EFFECTS_NONE)
    argtype0 = Tuple{argts...}
    nargtype = typeintersect(T, argtype0)
    nargtype === Union{} && return UResult(Union{}, EFFECTS_THROWS)
    nargtype isa DataType || return UResult(Any, EFFECTS_NONE)
    lookupsig = try
        Base.rewrap_unionall(Tuple{ft, unwrapped.parameters...}, T)
    catch
        return UResult(Any, EFFECTS_NONE)
    end
    matched, _ = try
        CC.findsup(lookupsig, CC.InternalMethodTable(fr.st.cfg.world))
    catch
        (nothing, nothing)
    end
    matched === nothing && return UResult(Any, EFFECTS_NONE)
    return widen_intercond(invoke_match(fr, matched.method,
                                        Tuple{ft, nargtype.parameters...},
                                        Tuple{ft, argts...}, callargs))
end

function invoke_match(fr::Frame, method::Method, @nospecialize(nargtype),
                      @nospecialize(argtype), callargs::Vector{Any})::UResult
    local nt
    r = try
        nt = typeintersect(nargtype, method.sig)
        nt === Union{} && return UResult(Union{}, EFFECTS_THROWS)
        tienv = ccall(:jl_type_intersection_with_env, Any, (Any, Any),
                      nt, method.sig)::Core.SimpleVector
        ti = tienv[1]
        env = tienv[2]::Core.SimpleVector
        match = Core.MethodMatch(ti, env, method, argtype <: method.sig)
        infer_method(fr, match, callargs)
    catch
        return UResult(Any, EFFECTS_NONE)
    end
    # the runtime checks args against `types`: not provably passing → may throw
    fx = r.effects
    passes = try
        argtype <: nt
    catch
        false
    end
    passes || (fx &= ~UnifiedIR.FLAG_NOTHROW)
    return UResult(r.rt, fx)
end

# ---------------------------------------------------------------------------
# Method frames: memoization, const-seeding, generated expansion
# ---------------------------------------------------------------------------

"Uncompressed or generator-expanded source for a method instance."
function method_src(m::Method, mi::Core.MethodInstance, world::UInt)
    if isdefined(m, :generator)
        return try
            CC.get_staged(mi, world)     # nothing when expansion fails
        catch
            nothing
        end
    end
    return try
        Base.uncompressed_ir(m)
    catch
        nothing
    end
end

function convert_src(srcci::Core.CodeInfo, m::Method, mi::Core.MethodInstance)
    try
        ir = codeinfo_to_ir(srcci; nargs = Int(m.nargs), name = m.name)
        ir.sptypes = Any[t for t in mi.sparam_vals]
        ir.meta[:sptypes_lat] = sptypes_lattice(mi)
        ir.meta[:mi] = mi     # sp_type_rewrap context for foreigncall rts
        return ir
    catch e
        e isa UnsupportedIR || rethrow()
        return nothing
    end
end

"""Static-parameter lattice elements for inference. `mi.sparam_vals` entries
are not always plain values (constrained TypeVars arrive as `svec(tv, flag)`
markers); reuse stock's decoding."""
function sptypes_lattice(mi::Core.MethodInstance)
    try
        return Any[vs.typ for vs in CC.sptypes_from_meth_instance(mi)]
    catch
        return Any[raw_sparam_lattice(sp) for sp in mi.sparam_vals]
    end
end

raw_sparam_lattice(@nospecialize(sp)) =
    (sp isa Core.SimpleVector || sp isa TypeVar) ? Any : CC.Const(sp)

"Drop InterConditionals whose slot has no positional caller operand (the
vararg-packed parameter of an isva method, or out-of-range slots)."
sanitize_intercond(m::Method, @nospecialize(rt)) =
    (rt isa UInterCond &&
     (m.isva ? rt.slot >= Int(m.nargs) : rt.slot > Int(m.nargs))) ? Bool : rt

"""Apply a method's declared `@assume_effects` overrides (the stock
`adjust_effects` port): `Base.@_total_meta`-style annotations must upgrade the
inferred mask — e.g. `==(::Type, ::Type)` is a total-declared foreigncall,
and concrete evaluation keys off the resulting EFFECTS_ALL."""
function apply_effects_override(m::Method, fx::UInt32)
    ovr = try
        CC.decode_effects_override(m.purity)
    catch
        return fx
    end
    ovr.consistent && (fx |= UnifiedIR.FLAG_CONSISTENT)
    ovr.effect_free && (fx |= UnifiedIR.FLAG_EFFECT_FREE)
    ovr.nothrow && (fx |= UnifiedIR.FLAG_NOTHROW)
    ovr.terminates_globally && (fx |= UnifiedIR.FLAG_TERMINATES)
    return fx
end

"Hashable memo key for a const-seeded frame, or nothing."
function const_key(mi::Core.MethodInstance, args::Vector{Any})
    key = Vector{Any}(undef, length(args) + 1)
    key[1] = mi
    for (i, a) in enumerate(args)
        key[i + 1] = a isa CC.Const ? (0x0, a.val) : (0x1, CC.widenconst(a))
    end
    t = (key...,)
    try
        hash(t)
    catch
        return nothing      # unhashable Const payload: skip const memoization
    end
    return t
end

"""Is a Const payload egal-stable, i.e. does egality pin its contents? Types,
symbols, strings (content-egal) and modules qualify despite the mutable flag;
identity-egal mutable objects (arrays, Refs) do not — a concrete evaluation
over them could bake in mutable state."""
egal_stable(@nospecialize(v)) =
    !ismutable(v) || v isa Union{Type, Symbol, String, Module, Method,
                                 Core.MethodInstance, Core.CodeInstance}

"""The concrete_eval_call port: when every argument is an egal-stable Const
and the callee's own (widened, context-free) inferred effects are total
(CONSISTENT|EFFECT_FREE|NOTHROW|TERMINATES), evaluate the call for real and
return Const of the result — both a precision and a speed lever (the abstract
const frame never runs). Returns nothing when ineligible."""
function concrete_eval(fr::Frame, match::Core.MethodMatch, args::Vector{Any},
                       @nospecialize(ck))
    st = fr.st
    f = CC.singleton_type(args[1])
    f === nothing && args[1] isa CC.Const && (f = (args[1]::CC.Const).val)
    f === nothing && return nothing
    argvals = Vector{Any}(undef, length(args) - 1)
    for i in 2:length(args)
        a = args[i]
        a isa CC.Const || return nothing
        v = (a::CC.Const).val
        egal_stable(v) || return nothing
        argvals[i - 1] = v
    end
    # the callee's effects come from its widened frame (memoized; computed
    # once per mi). EFFECTS_ALL implies a clean, converged, cutoff-free frame:
    # stale reads and resource cutoffs always pessimize the mask.
    wr = infer_method(fr, match, Any[])
    wr.effects == EFFECTS_ALL || return nothing
    v = try
        Core._call_in_world_total(st.cfg.world, f, argvals...)
    catch
        return nothing      # effects promised nothrow; be defensive anyway
    end
    r = UResult(CC.Const(v), EFFECTS_ALL)
    ck === nothing || (st.constcache[ck] = r)
    return r
end

"Would const-seeding add information over the widened signature?"
function const_args_profitable(args::Vector{Any})
    for i in 2:length(args)
        a = args[i]
        if a isa CC.Const
            Base.issingletontype(typeof(a.val)) || return true
        elseif a isa CC.PartialStruct
            return true
        end
    end
    return false
end

"""SCC membership/convergence bookkeeping for the outermost cycle root: fold
this pass's member results (the `cycle_scratch` memo) into the accumulated
`scc_prev` table with tmerge (bounded ascent), returning whether any member
appeared or moved. `scc_prev` also reseeds nested cycle roots on the next
pass, making the outer reruns a joint Gauss-Seidel iteration over the SCC."""
function scc_update!(st::UInferState, escalate::Bool)
    changed = false
    for (k, v) in st.cycle_scratch
        r = v isa UResult ? v : (v[1])::UResult   # unwrap epoch-tagged Bottoms
        old = get(st.scc_prev, k, nothing)
        if old === nothing
            st.scc_prev[k] = r
            changed = true
            continue
        end
        old = old::UResult
        merged = umerge(old.rt, r.rt)
        escalate && (merged = CC.widenconst(widenucond(merged)))
        fx = old.effects & r.effects
        if !ulat_eq(merged, old.rt) || fx != old.effects
            st.scc_prev[k] = UResult(merged, fx)
            changed = true
        end
    end
    return changed
end

function infer_method(fr::Frame, match::Core.MethodMatch, args::Vector{Any})::UResult
    st = fr.st
    m = match.method
    mi = CC.specialize_method(match)
    if haskey(st.active, mi)
        st.stats.cycles += 1
        push!(st.cycle_hit, mi)
        # an outstanding stale read: frames whose window saw it and lie below
        # the target are cycle-tainted until the target completes
        st.stale_depth = min(st.stale_depth, st.active[mi])
        st.stale_events += 1
        # cycle: current rt approximation; effects pessimized (recursion may
        # not terminate, and the stale mask would be unsound to trust)
        r = get(st.cache, mi, nothing)
        return r === nothing ? UResult(Union{}, EFFECTS_NONE) :
                               UResult((r::UResult).rt, EFFECTS_NONE)
    end
    # const-seeded frames (interprocedural constant propagation) use their own
    # memo cache keyed by the const-extended signature. Vararg methods qualify
    # too (stock const-props them; method_arglattice builds the precise vararg
    # tuple via tuple_tfunc) — e.g. `_any_tuple(f, false, tt...)` needs the
    # Const(false) seed for `TupleOrBottom`/`promote_op` guards to fold.
    argsfit = !any(a -> CC.isvarargtype(a), args) &&
              (m.isva ? length(args) >= Int(m.nargs) - 1 :
                        Int(m.nargs) == length(args))
    constseeded = argsfit && const_args_profitable(args)
    ck = nothing
    if constseeded
        ck = const_key(mi, args)
        if ck === nothing
            constseeded = false
        else
            r = get(st.constcache, ck, nothing)
            r === nothing || return r::UResult
            r = get(st.scratch, ck, nothing)
            r === nothing || return r::UResult
            r = get(st.cycle_scratch, ck, nothing)
            if r isa UResult
                st.cyscr_hits += 1
                return r
            elseif r isa Tuple && r[2] == st.resolutions
                st.cyscr_hits += 1
                return r[1]::UResult
            end
            # all-Const call of a total callee: evaluate for real instead of
            # running the abstract const frame (the concrete_eval_call port)
            r = concrete_eval(fr, match, args, ck)
            r === nothing || return r::UResult
        end
    end
    if !constseeded
        haskey(st.cache, mi) && return st.cache[mi]::UResult
        r = get(st.scratch, mi, nothing)
        r === nothing || return r::UResult
        r = get(st.cycle_scratch, mi, nothing)
        if r isa UResult
            st.cyscr_hits += 1
            return r
        elseif r isa Tuple && r[2] == st.resolutions
            st.cyscr_hits += 1
            return r[1]::UResult
        end
    end
    if length(st.active) >= st.cfg.max_depth ||
       st.stats.frames - st.budget_mark >= FRAME_BUDGET
        # resource cutoff: the result is CONTEXT-dependent — callers must not
        # memoize anything computed on top of it (see `tainted` below)
        st.limited += 1
        return native_result(fr, match)
    end
    srcci = method_src(m, mi, st.cfg.world)
    srcci === nothing && return native_result(fr, match)
    # a frame is tainted when its subtree (a) hit a resource cutoff, or
    # (b) depends on the stale approximation of a frame STILL active above us
    # (an outstanding stale read at a smaller depth). Such results are valid
    # transiently but must not enter the permanent caches: (a) goes to the
    # per-query scratch, (b) to the cycle scratch, which the cycle root clears
    # per fixpoint pass and flushes on completion
    mydepth = length(st.active) + 1
    lim0 = st.limited
    ev0 = st.stale_events
    h0 = st.cyscr_hits
    tainted_limit() = st.limited > lim0
    # tainted iff MY window saw a stale read whose target is still above us,
    # or consumed a stale-based scratch entry (independent clean subtrees
    # below an active cycle root stay permanently cacheable)
    tainted_cycle() =
        (st.stale_events > ev0 && st.stale_depth < mydepth) || st.cyscr_hits > h0
    function frame_done(flushable::Bool = false)
        # all outstanding stale reads targeted us or frames below us: resolved.
        # If we are the outermost stale target and the SCC's joint fixpoint
        # exited STABLE (our value and every member's per-pass result), the
        # members' last-pass results are jointly consistent with the final
        # values — flush them ALL into the permanent caches together (the
        # stock finish_cycle rule: the whole SCC commits when its outermost
        # frame converges). A genuinely non-converged pass discards instead.
        if st.stale_depth >= mydepth
            st.resolutions += 1
            if st.stale_depth == mydepth && flushable
                for (k, v) in st.cycle_scratch
                    v isa UResult || continue   # epoch-tagged Bottoms: no flush
                    if k isa Core.MethodInstance
                        st.cache[k] = v
                    else
                        st.constcache[k] = v
                    end
                end
            end
            empty!(st.cycle_scratch)
            empty!(st.scc_prev)
            st.stale_depth = typemax(Int)
        end
        return nothing
    end
    if constseeded && length(srcci.code) <= CONSTPROP_SRC_LIMIT
        # caller-precise seed (Const/PartialStruct lattice), own memo cache;
        # recursion protection via `active`
        argl = method_arglattice(m, mi, args)
        argl === nothing && return native_result(fr, match)
        st.active[mi] = mydepth
        hit_cycle = false
        local rc
        try
            src_ir = convert_src(srcci, m, mi)
            src_ir === nothing && return native_result(fr, match)
            delete!(st.cycle_hit, mi)
            rt_const = sanitize_intercond(m, infer_ir!(src_ir, copy(argl); state = st))
            rc = UResult(rt_const, apply_effects_override(m,
                get(src_ir.meta, :effects, EFFECTS_NONE)::UInt32))
            hit_cycle = mi in st.cycle_hit
        finally
            delete!(st.active, mi)
        end
        # a recursive const-seeded frame read its own stale approximation; the
        # const result is unsound — fall back to the widened fixpoint below
        # (bounded const-prop recursion loses const precision, keeps soundness)
        if !hit_cycle
            # cycle taint takes priority over limit taint: a stale-dependent
            # result must die with the pass, never enter the per-query scratch
            # (which outlives the cycle's resolution)
            if tainted_cycle()
                # a stale-collapsed Bottom expires as soon as any cycle root
                # resolves (the caches it depends on improve then); non-Bottom
                # entries live until the pass's scratch is cleared
                st.cycle_scratch[ck] = rc.rt === Union{} ? (rc, st.resolutions) : rc
            elseif tainted_limit()
                st.scratch[ck] = rc
            else
                st.constcache[ck] = rc
            end
            frame_done()
            return rc
        end
        # (frame_done(false): members computed against this aborted const frame
        # must not be flushed as converged)
        haskey(st.cache, mi) && (frame_done(false); return st.cache[mi]::UResult)
    end
    # the memoized path is keyed by mi: it MUST be computed at the
    # mi.specTypes-derived lattice, never at one caller's lattice — a shared
    # cache entry computed from the first caller's (wider or narrower) args
    # would be imprecise or unsound for every other caller of the same mi
    argl = method_arglattice(m, mi, Any[])
    argl === nothing && return native_result(fr, match)
    st.active[mi] = mydepth
    # nested cycle roots reseed from the SCC's last-pass approximation (the
    # joint Gauss-Seidel iteration ascends instead of restarting at ⊥ — a ⊥
    # restart both loses the self-edge contribution and can never converge)
    prevapprox = get(st.scc_prev, mi, nothing)
    st.cache[mi] = prevapprox === nothing ? UResult(Union{}, EFFECTS_ALL) :
                                            prevapprox::UResult
    ok = false
    converged = false
    deferred = false
    nc0 = st.nonconverged
    try
        # SCC joint fixpoint: the OUTERMOST stale-read target reruns until
        # neither its own value nor any member's per-pass result moves.
        # Each pass recomputes every member frame (unlike stock's suspended
        # frames), so the cap is deliberately tight, with early widenconst
        # escalation forcing convergence on big print/show SCCs.
        for it in 1:12
            delete!(st.cycle_hit, mi)
            # on reruns, cycle members memoized against our previous
            # approximation must be recomputed against the updated one
            it > 1 && empty!(st.cycle_scratch)
            nc0 = st.nonconverged      # snapshot at final-pass start
            src_ir = convert_src(srcci, m, mi)
            if src_ir === nothing
                st.cache[mi] = native_result(fr, match)
                converged = true
                break
            end
            rt = sanitize_intercond(m, infer_ir!(src_ir, copy(argl); state = st))
            fx = apply_effects_override(m,
                get(src_ir.meta, :effects, EFFECTS_NONE)::UInt32)
            old = st.cache[mi]::UResult
            widened = umerge(old.rt, rt)
            it >= 6 && (widened = CC.widenconst(widenucond(widened)))  # ascent escalation
            fx &= old.effects                                # monotone descent
            st.cache[mi] = UResult(widened, fx)
            if !(mi in st.cycle_hit)
                converged = true
                break
            end
            if st.stale_depth < mydepth
                # nested root inside a larger active SCC: exactly one pass by
                # design — the outermost root's reruns recompute us (avoids
                # compounding nested fixpoints). NOT a failed fixpoint: the
                # outer root's joint convergence check owns our stability.
                deferred = true
                break
            end
            # outermost root: joint convergence over the whole SCC
            changed = !ulat_eq(widened, old.rt) || fx != old.effects
            changed |= scc_update!(st, it >= 6)
            if !changed
                converged = true
                break
            end
        end
        if (st.cache[mi]::UResult).rt === Union{} && mi in st.cycle_hit
            # Bottom "fixpoint" reached only through our own optimistic seed
            # (recursive reads returned the Union{} seed, whose dead-tail kills
            # then suppressed every real path). A self-supporting Bottom cannot
            # be trusted: settle at the sound over-approximation.
            st.cache[mi] = UResult(Any, EFFECTS_NONE)
            converged = true
        end
        ok = true
    finally
        delete!(st.active, mi)
        # an escaping exception must not leave the optimistic seed behind
        ok || delete!(st.cache, mi)
    end
    # a deferred nested root is not a failed fixpoint (the outer root owns it)
    converged || deferred || (st.nonconverged += 1)
    converged && mi in st.cycle_hit && (st.resolutions += 1)
    r = st.cache[mi]::UResult
    # context-dependent (tainted) results move to the appropriate scratch;
    # cycle taint takes priority (see the const-seeded epilogue)
    if tainted_cycle()
        delete!(st.cache, mi)
        st.cycle_scratch[mi] = r.rt === Union{} ? (r, st.resolutions) : r
    elseif tainted_limit()
        delete!(st.cache, mi)
        st.scratch[mi] = r
    end
    # flush only a clean, jointly-converged final pass (no non-converged
    # inner fixpoints, no resource cutoffs)
    frame_done(converged && st.nonconverged == nc0 && !tainted_limit())
    return r
end

function method_arglattice(m::Method, mi::Core.MethodInstance, args::Vector{Any})
    nparams = Int(m.nargs)
    argl = Vector{Any}(undef, nparams)
    havecaller = !isempty(args) && !any(a -> CC.isvarargtype(a), args)
    if !m.isva && nparams == length(args) && havecaller
        for i in 1:nparams
            argl[i] = args[i]
        end
        return argl
    end
    if m.isva && havecaller && length(args) >= nparams - 1
        # precise vararg tuple from the caller's trailing argument lattices
        for i in 1:(nparams - 1)
            argl[i] = args[i]
        end
        rest = Any[widenucond(args[i]) for i in nparams:length(args)]
        argl[nparams] = try
            CC.tuple_tfunc(CC.fallback_lattice, rest)
        catch
            Tuple{Any[CC.widenconst(r) for r in rest]...}
        end
        return argl
    end
    spec = mi.specTypes
    sigts = Base.unwrap_unionall(spec)
    sigts isa DataType || return nothing
    ps = sigts.parameters
    if m.isva
        for i in 1:(nparams - 1)
            argl[i] = i <= length(ps) ? Base.rewrap_unionall(ps[i], spec) : Any
            argl[i] isa Type || (argl[i] = Any)
        end
        # vararg tuple lattice: precise when the trailing sig is concrete
        rest = Any[Base.rewrap_unionall(ps[i], spec) for i in nparams:length(ps)]
        # degrade unusable entries elementwise; a Vararg tail must stay Vararg
        rest = Any[(t isa Type || CC.isvarargtype(t)) ? t : Any for t in rest]
        argl[nparams] = try
            Tuple{rest...}
        catch
            Tuple
        end
        return argl
    end
    for i in 1:nparams
        argl[i] = i <= length(ps) ? Base.rewrap_unionall(ps[i], spec) : Any
        argl[i] isa Type || (argl[i] = Any)
    end
    return argl
end

native_result(fr::Frame, match::Core.MethodMatch) =
    UResult(native_rt(fr, match), EFFECTS_NONE)

function native_rt(fr::Frame, match::Core.MethodMatch)
    fr.st.cfg.native_fallback || return Any
    fr.st.stats.native_fallbacks += 1
    try
        return Core.Compiler.return_type(match.spec_types)
    catch
        return Any
    end
end
