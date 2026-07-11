# The inference port (§10.3): abstract interpretation running natively on
# UnifiedIR — a structured fixed-point walk over the region tree (no
# reconstruction of basic blocks outside `cfg` islands). Reuses the Compiler
# package's lattice elements and tfuncs (Const, tmerge, builtin_tfunction);
# the IR-shape-dependent walker is what this file replaces.
#
# §10.3 mappings implemented (v1 scope):
#   (a) diverging-arm refinement: arms whose every exit is return/unreachable
#       contribute nothing to the if's result join;
#   (b) irinterp edge-killing: Const conditions select a single arm during
#       inference (the surgery form lives in fold_constant_branches!);
#   (c) backedge refinement: loop carried-arg states are joined from init and
#       `continue` values with bounded widening.

const CC = Compiler

struct UInferConfig
    world::UInt
    max_methods::Int
    max_depth::Int
    max_loop_iter::Int
    native_fallback::Bool   # delegate callees outside the entry-converter
                            # feature matrix to stock inference (documented seam)
    interp::CC.NativeInterpreter
end
function UInferConfig(; world::UInt = Base.get_world_counter(),
                      max_methods::Int = 3, max_depth::Int = 128,
                      max_loop_iter::Int = 8, native_fallback::Bool = true)
    UInferConfig(world, max_methods, max_depth, max_loop_iter, native_fallback,
                 CC.NativeInterpreter(world))
end

mutable struct UInferStats
    frames::Int
    native_fallbacks::Int
    cycles::Int
end

mutable struct UInferState
    cfg::UInferConfig
    cache::Dict{Core.MethodInstance,Any}        # mi -> UResult (rettype + effects)
    active::Dict{Core.MethodInstance,Int}       # cycle detection
    cycle_hit::Set{Core.MethodInstance}         # frames whose stale value was read
    stats::UInferStats
    constcache::Dict{Any,Any}                   # (mi, const-arg key) -> UResult
    budget_mark::Int                            # stats.frames at top-level query entry
    limited::Int                                # depth/budget cutoffs (taint counter)
    scratch::Dict{Any,Any}                      # per-top-level-query memo for
                                                # cutoff-tainted results
    cycle_scratch::Dict{Any,Any}                # transient memo for results that
                                                # depend on a stale approximation
                                                # (per-fixpoint-pass; the SCC's
                                                # membership table for the pass)
    scc_prev::Dict{Any,Any}                     # SCC joint-fixpoint state: last
                                                # pass's tmerge-accumulated result
                                                # per member (mi or const key);
                                                # seeds nested cycle roots and
                                                # detects joint convergence
    stale_depth::Int                            # min active-stack depth of any
                                                # outstanding stale (cycle) read
    stale_events::Int                           # stale-read event counter
    cyscr_hits::Int                             # cycle-scratch consumption counter
    nonconverged::Int                           # non-converged fixpoint exits
    resolutions::Int                            # resolved-cycle epoch (Bottom
                                                # scratch entries expire on bump)
end
UInferState(cfg::UInferConfig = UInferConfig()) =
    UInferState(cfg, Dict{Core.MethodInstance,Any}(), Dict{Core.MethodInstance,Int}(),
                Set{Core.MethodInstance}(), UInferStats(0, 0, 0), Dict{Any,Any}(), 0, 0,
                Dict{Any,Any}(), Dict{Any,Any}(), Dict{Any,Any}(), typemax(Int), 0, 0, 0, 0)

⊔(st::UInferState, @nospecialize(a), @nospecialize(b)) =
    a === nothing ? b :
    b === nothing ? a : CC.tmerge(CC.fallback_lattice, widenucond(a), widenucond(b))

"""
    UCond

The port of `Core.Compiler.Conditional` (§10.3): a Bool-valued lattice
element that carries type refinements for a *subject* — a cell (slot analog)
or an SSA statement — on the true/false edges of a branch.
"""
struct UCond
    subject::Tuple{Symbol,Int32}     # (:cell, id) | (:stmt, id)
    thentype::Any
    elsetype::Any
end
"""
    UInterCond

The port of `Core.Compiler.InterConditional` (§10.3): a frame's Bool return
value that refines one of its *parameters* (by position in the root region's
args, 1 = the function itself). Context-free — safe to cache in `UResult`s —
and translated back to a caller-local `UCond` at each call site when the
caller passed a refinable subject in that position.
"""
struct UInterCond
    slot::Int
    thentype::Any
    elsetype::Any
end

widenucond(@nospecialize(t)) = (t isa UCond || t isa UInterCond) ? Bool : t
const RefMap = Dict{Tuple{Symbol,Int32},Any}

lat_eq(@nospecialize(a), @nospecialize(b)) =
    a === b || (CC.:⊑(CC.fallback_lattice, a, b) && CC.:⊑(CC.fallback_lattice, b, a))

"UInterCond-aware equality (⊑ is not defined on inter-conditionals)."
ulat_eq(@nospecialize(a), @nospecialize(b)) =
    (a isa UInterCond || b isa UInterCond) ?
        (a isa UInterCond && b isa UInterCond && a.slot == b.slot &&
         lat_eq(a.thentype, b.thentype) && lat_eq(a.elsetype, b.elsetype)) :
        lat_eq(a, b)

"UInterCond-aware tmerge for interprocedural result accumulation."
function umerge(@nospecialize(a), @nospecialize(b))
    a === Union{} && return b
    b === Union{} && return a
    if a isa UInterCond && b isa UInterCond && a.slot == b.slot
        return UInterCond(a.slot,
                          CC.tmerge(CC.fallback_lattice, a.thentype, b.thentype),
                          CC.tmerge(CC.fallback_lattice, a.elsetype, b.elsetype))
    end
    return CC.tmerge(CC.fallback_lattice, widenucond(a), widenucond(b))
end

# ---------------------------------------------------------------------------
# Frame inference over one IR body
# ---------------------------------------------------------------------------

mutable struct Frame
    ir::UnifiedIR.IR
    st::UInferState
    env::Vector{Any}                  # stmt id -> lattice element
    celltypes::Dict{Int32,Any}
    cells_changed::Bool
    rettype::Any                      # accumulated return-type join (nothing = none)
    continue_vals::Dict{Int32,Any}    # loop body region -> joined carried Vector{Any}
    break_vals::Dict{Int32,Any}       # loop body region -> joined result lattice
    reached::Set{Int32}               # blocks reached via cross-island gotos (§5.5)
    refinements::Vector{RefMap}       # active Conditional refinement scopes
    effects::UInt32                   # frame effects accumulator (§5.1 rule 5)
    stmt_effects::Vector{UInt32}      # per-stmt effect masks (sentinel = untouched)
    newed_cells::Set{Int32}           # cells with a cell_new (maybe-undef reads)
    pending_refine::Any               # (subject => lattice) from a typeassert, or nothing
end

"Frame constructor with empty analysis state (transfers.jl defines the masks)."
function Frame(ir::UnifiedIR.IR, st::UInferState, env::Vector{Any})
    fr = Frame(ir, st, env, Dict{Int32,Any}(), false, nothing, Dict{Int32,Any}(),
               Dict{Int32,Any}(), Set{Int32}(), RefMap[], ~UInt32(0),
               fill(~UInt32(0), length(env)), Set{Int32}(), nothing)
    for s in UnifiedIR.each_stmt(ir)
        if UnifiedIR.stmt_kind(ir, s) === K"cell_new"
            push!(fr.newed_cells, UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1)).id)
        end
    end
    return fr
end

"""
    infer_ir!(ir, argtypes; state=UInferState()) -> rettype lattice

Run inference over a dense, sealed UnifiedIR body. Writes lattice elements
into the `type` column and `ir.meta[:rettype]`; returns the return type.
`argtypes` are lattice elements for region 1's args (position 1 = the
function itself).
"""
function infer_ir!(ir::UnifiedIR.IR, argtypes::Vector{Any};
                   state::UInferState = UInferState())
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_DENSE, "infer_ir!")
    root = UnifiedIR.getregion(ir, UnifiedIR.root_region(ir))
    length(argtypes) == length(root.args) ||
        error("infer_ir!: $(length(argtypes)) argtypes for $(length(root.args)) parameters")
    if isempty(state.active)
        # top-level query entry: fresh budget, fresh taint-scratch memos
        state.budget_mark = state.stats.frames
        empty!(state.scratch)
        empty!(state.cycle_scratch)
        empty!(state.scc_prev)
        state.stale_depth = typemax(Int)
    end
    state.stats.frames += 1
    fr = Frame(ir, state, Vector{Any}(nothing, UnifiedIR.nstmts(ir)))
    for (i, a) in enumerate(root.args)
        fr.env[a.id] = argtypes[i]
    end
    effmask = UnifiedIR.FLAG_CONSISTENT | UnifiedIR.FLAG_EFFECT_FREE |
              UnifiedIR.FLAG_NOTHROW | UnifiedIR.FLAG_TERMINATES
    # cell fixpoint: celltypes grow monotonically under tmerge; termination
    # comes from widening, never from stopping with a stale (unsound) state
    iter = 0
    while true
        iter += 1
        fr.cells_changed = false
        fr.rettype = nothing
        fr.effects = effmask
        fill!(fr.stmt_effects, ~UInt32(0))
        infer_region!(fr, UnifiedIR.root_region(ir))
        fr.cells_changed || break
        if iter == 20
            # force-widen every cell to its widenconst to cap the ascent
            for (k, v) in fr.celltypes
                fr.celltypes[k] = CC.widenconst(widenucond(v))
            end
        elseif iter > 40
            for (k, v) in fr.celltypes
                fr.celltypes[k] = Any
            end
        end
    end
    # publish lattice elements into the type column and effect masks into the
    # flag column (only the 4 effect bits; other flag bits are preserved)
    for i in 1:UnifiedIR.nstmts(ir)
        s = StmtId(i)
        UnifiedIR.is_tombstone(ir, s) && continue
        m = fr.stmt_effects[i]
        if m != ~UInt32(0)
            old = UnifiedIR.stmt_flag(ir, s)
            new = (old & ~effmask) | (m & effmask)
            new == old || UnifiedIR.set_flag!(ir, s, new)
        end
        t = fr.env[i]
        t === nothing && continue
        UnifiedIR.set_type!(ir, s, widenucond(t))
    end
    rt = fr.rettype === nothing ? Union{} : fr.rettype
    if rt isa UCond
        # InterConditional export: a conditional return whose subject is a
        # root parameter is context-free by position; anything else widens
        idx = 0
        if rt.subject[1] === :stmt
            for (i, a) in enumerate(root.args)
                a.id == rt.subject[2] && (idx = i; break)
            end
        end
        rt = idx == 0 ? widenucond(rt) : UInterCond(idx, rt.thentype, rt.elsetype)
    end
    ir.meta[:rettype] = widenucond(rt)
    ir.meta[:effects] = fr.effects & effmask
    return rt
end

"Record a `return`: a single-operand UCond return stays conditional (the
InterConditional port; infer_ir! exports it by parameter position). Multiple
same-subject conditional returns join fieldwise; mixed returns widen."
function note_return!(fr::Frame, s::StmtId)
    if UnifiedIR.nops(fr.ir, s) == 1
        v = opl(fr, UnifiedIR.getop(fr.ir, s, 1))
        if v isa UCond
            old = fr.rettype
            if old === nothing
                fr.rettype = v
            elseif old isa UCond && old.subject == v.subject
                fr.rettype = UCond(v.subject,
                    CC.tmerge(CC.fallback_lattice, old.thentype, v.thentype),
                    CC.tmerge(CC.fallback_lattice, old.elsetype, v.elsetype))
            else
                fr.rettype = ⊔(fr.st, old, v)
            end
            return nothing
        end
    end
    fr.rettype = ⊔(fr.st, fr.rettype, joinvals(fr, opls(fr, s, 1)))
    return nothing
end

"Kill marker: a scope that masks any outer refinement of a subject (loop
backedges and throw edges invalidate path-sensitive facts)."
struct RefKill end
const REFINE_KILL = RefKill()

"Innermost active refinement for a subject, or nothing."
function refined(fr::Frame, key::Tuple{Symbol,Int32})
    for i in length(fr.refinements):-1:1
        v = get(fr.refinements[i], key, nothing)
        if v !== nothing
            v === REFINE_KILL && return nothing
            return v
        end
    end
    return nothing
end

"Cells stored anywhere within region `r`'s subtree (memoized per frame walk)."
function stored_cells_in(fr::Frame, r::RegionId)
    ir = fr.ir
    out = Set{Int32}()
    stack = RegionId[r]
    while !isempty(stack)
        cur = pop!(stack)
        for s in UnifiedIR.region_stmts(ir, cur)
            k = UnifiedIR.stmt_kind(ir, s)
            if k === K"cell_set" || k === K"cell_new"
                push!(out, UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1)).id)
            end
            if UnifiedIR.owns_regions(k)
                for rid in UnifiedIR.live_owned_regions(ir, s)
                    push!(stack, rid)
                end
            end
        end
    end
    return out
end

"Push a scope masking cell refinements invalidated by `r`'s stores (backedge/
throw-edge rule); returns whether a scope was pushed."
function push_store_kills!(fr::Frame, r::RegionId)
    cells = stored_cells_in(fr, r)
    isempty(cells) && return false
    m = RefMap()
    for c in cells
        m[(:cell, c)] = REFINE_KILL
    end
    push!(fr.refinements, m)
    return true
end

"Read a cell's lattice element (refinements shadow the global celltype)."
function cell_lattice(fr::Frame, cellid::Int32)
    r = refined(fr, (:cell, cellid))
    r === nothing || return r
    return get(fr.celltypes, cellid, Union{})
end

# lattice element of a value operand
function opl(fr::Frame, o::UnifiedIR.Operand)
    ir = fr.ir
    t = UnifiedIR.optag(o)
    if t == UnifiedIR.TAG_STMT
        sid = UnifiedIR.payload(o) % Int32
        r = refined(fr, (:stmt, sid))
        r === nothing || return r
        v = fr.env[sid]
        return v === nothing ? Any : v   # not-yet-visited: conservative
    elseif t == UnifiedIR.TAG_INLINE
        return CC.Const(UnifiedIR.imm_value(o))
    elseif t == UnifiedIR.TAG_CONST
        return CC.Const(ir.body.constants[UnifiedIR.payload(o)])
    elseif t == UnifiedIR.TAG_GLOBAL
        g = ir.body.globals[UnifiedIR.payload(o)]
        if isconst(g.mod, g.name) && isdefined(g.mod, g.name)
            return CC.Const(getglobal(g.mod, g.name))
        end
        return Any
    elseif t == UnifiedIR.TAG_SPARAM
        i = Int(UnifiedIR.payload(o))
        lat = get(ir.meta, :sptypes_lat, nothing)
        if lat isa Vector{Any} && i <= length(lat)
            return lat[i]     # stock-decoded lattice (sptypes_from_meth_instance)
        end
        if i <= length(ir.sptypes)
            # raw sparam values (transfers.jl decodes non-value markers)
            return raw_sparam_lattice(ir.sptypes[i])
        end
        return Any
    else
        return Any
    end
end

opls(fr::Frame, s::StmtId, from::Int) =
    Any[opl(fr, UnifiedIR.getop(fr.ir, s, i)) for i in from:UnifiedIR.nops(fr.ir, s)]

joinvals(fr::Frame, vals::Vector{Any}) =
    isempty(vals) ? CC.Const(nothing) :
    length(vals) == 1 ? widenucond(vals[1]) :
    CC.builtin_tfunction(fr.st.cfg.interp, Core.tuple, Any[widenucond(v) for v in vals], nothing)

# Region inference: returns the join of `result` values reaching the owner
# (nothing if no result terminator), accumulating return/continue/break joins on `fr`.
function infer_region!(fr::Frame, r::RegionId)
    ir = fr.ir
    results = nothing
    npush = 0
    for s in UnifiedIR.region_stmts(ir, r)
        k = UnifiedIR.stmt_kind(ir, s)
        if k === K"region_arg"
            continue
        elseif k === K"result"
            results = ⊔(fr.st, results, joinvals(fr, opls(fr, s, 1)))
        elseif k === K"return"
            note_return!(fr, s)
        elseif k === K"continue"
            tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, s, 1))
            condl = opl(fr, UnifiedIR.getop(ir, s, 2))
            vals = opls(fr, s, 3)
            prev = get(fr.continue_vals, tgt.id, nothing)
            joined = prev === nothing ? vals :
                Any[CC.tmerge(CC.fallback_lattice, prev[i], vals[i]) for i in 1:length(vals)]
            fr.continue_vals[tgt.id] = joined
            # cond not provably true ⇒ the loop can exit here with `vals`
            if !(condl isa CC.Const && condl.val === true)
                fr.break_vals[tgt.id] = ⊔(fr.st, get(fr.break_vals, tgt.id, nothing),
                                          joinvals(fr, vals))
            end
        elseif k === K"break"
            tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, s, 1))
            fr.break_vals[tgt.id] = ⊔(fr.st, get(fr.break_vals, tgt.id, nothing),
                                      joinvals(fr, opls(fr, s, 2)))
        elseif k === K"unreachable"
        elseif k === K"if"
            carry = infer_if!(fr, s)
            if carry !== nothing
                # one arm diverges: its complement's refinement holds for the
                # remainder of this region (§10.3(a) — the Pi/Conditional
                # machinery relocated)
                push!(fr.refinements, carry)
                npush += 1
            end
            fr.env[s.id] === Union{} && break   # no arm falls through: dead rest
        elseif k === K"loop"
            infer_loop!(fr, s)
            fr.env[s.id] === Union{} && break   # loop never exits: dead rest
        elseif k === K"try"
            infer_try!(fr, s)
            fr.env[s.id] === Union{} && break
        elseif k === K"cfg"
            infer_cfg!(fr, s)
            fr.env[s.id] === Union{} && break
        elseif k === K"closure"
            fr.env[s.id] = Any   # enters the core at P3
        else
            fr.env[s.id] = transfer(fr, s, k)
            if fr.pending_refine !== nothing
                # typeassert/store back-propagation: the refinement holds for
                # the remainder of this region
                pr = fr.pending_refine::Pair
                push!(fr.refinements, RefMap(pr.first => pr.second))
                npush += 1
                fr.pending_refine = nothing
            end
            # a Bottom-typed statement never completes (guaranteed throw):
            # the rest of this region is unreachable (stock's dead-tail rule)
            fr.env[s.id] === Union{} && break
        end
    end
    for _ in 1:npush
        pop!(fr.refinements)
    end
    return results
end

"Does control ever reach the join after this region (false = diverges)?"
function region_falls_through(ir::UnifiedIR.IR, r::RegionId)
    t = UnifiedIR.region_terminator(ir, r)
    t === nothing && return true
    return UnifiedIR.stmt_kind(ir, t) === K"result"
end

function infer_if!(fr::Frame, s::StmtId)::Union{Nothing,RefMap}
    ir = fr.ir
    condl = opl(fr, UnifiedIR.getop(ir, s, 1))
    rs = UnifiedIR.live_owned_regions(ir, s)
    local res
    carry = nothing
    # a Conditional with a Bottom arm decides the branch (stock's rule:
    # elsetype ⊥ ⇒ the condition is provably true on any live path)
    if condl isa UCond && (condl.thentype === Union{} || condl.elsetype === Union{})
        taken = condl.elsetype === Union{}
        reft = taken ? condl.thentype : condl.elsetype
        push!(fr.refinements, RefMap(condl.subject => reft))
        local rres
        if taken
            rres = infer_region!(fr, rs[1])
        elseif length(rs) >= 2
            rres = infer_region!(fr, rs[2])
        else
            rres = CC.Const(nothing)
        end
        pop!(fr.refinements)
        fr.env[s.id] = rres === nothing ? Union{} : rres
        # the surviving arm's refinement holds for the region rest
        return RefMap(condl.subject => reft)
    end
    if condl isa CC.Const && condl.val isa Bool
        # §10.3(b): a Const condition selects a single arm during inference
        if condl.val
            res = infer_region!(fr, rs[1])
        elseif length(rs) >= 2
            res = infer_region!(fr, rs[2])
        else
            res = CC.Const(nothing)
        end
    elseif condl isa UCond
        push!(fr.refinements, RefMap(condl.subject => condl.thentype))
        r1 = infer_region!(fr, rs[1])
        pop!(fr.refinements)
        local r2
        if length(rs) >= 2
            push!(fr.refinements, RefMap(condl.subject => condl.elsetype))
            r2 = infer_region!(fr, rs[2])
            pop!(fr.refinements)
        else
            r2 = CC.Const(nothing)
        end
        res = ⊔(fr.st, r1, r2)
        d1 = !region_falls_through(ir, rs[1])
        d2 = length(rs) >= 2 ? !region_falls_through(ir, rs[2]) : false
        if d1 && !d2
            carry = RefMap(condl.subject => condl.elsetype)
        elseif d2 && !d1
            carry = RefMap(condl.subject => condl.thentype)
        end
    else
        r1 = infer_region!(fr, rs[1])
        r2 = length(rs) >= 2 ? infer_region!(fr, rs[2]) : CC.Const(nothing)
        # §10.3(a): diverging arms (no result reached) contribute nothing
        res = ⊔(fr.st, r1, r2)
    end
    fr.env[s.id] = res === nothing ? Union{} : res
    return carry
end

function infer_loop!(fr::Frame, s::StmtId)
    ir = fr.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    bodyr = rs[1]
    breg = UnifiedIR.getregion(ir, bodyr)
    carried = Any[widenucond(v) for v in opls(fr, s, 1)]
    delete!(fr.break_vals, bodyr.id)
    # backedge rule: pre-loop refinements of cells the body stores are invalid
    # on iterations ≥ 2 — mask them for the whole body walk
    killed = push_store_kills!(fr, bodyr)
    iter = 0
    while true
        iter += 1
        for (i, a) in enumerate(breg.args)
            fr.env[a.id] = carried[i]
        end
        delete!(fr.continue_vals, bodyr.id)
        infer_region!(fr, bodyr)
        cont = get(fr.continue_vals, bodyr.id, nothing)
        cont === nothing && break   # body never continues: single trip
        newcarried = Any[CC.tmerge(CC.fallback_lattice, carried[i], widenucond(cont[i]))
                         for i in 1:length(carried)]
        if iter >= 4
            # §10.3(c) widening escalation: precision first, then widenconst,
            # then Any — the loop exits only at a (post-widening) fixpoint
            newcarried = Any[CC.widenconst(t) for t in newcarried]
        end
        iter > 24 && (newcarried = Any[Any for _ in newcarried])
        stable = all(i -> lat_eq(newcarried[i], carried[i]), 1:length(carried))
        carried = newcarried
        stable && break
    end
    killed && pop!(fr.refinements)
    result = get(fr.break_vals, bodyr.id, nothing)
    fr.env[s.id] = result === nothing ? Union{} : result   # never-exiting loop: ⊥
    # §5.1 rule 5: loops drop TERMINATES (bounded-trip proofs are future work)
    fr.effects &= ~UnifiedIR.FLAG_TERMINATES
    return nothing
end

function infer_try!(fr::Frame, s::StmtId)
    ir = fr.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    r1 = infer_region!(fr, rs[1])
    r2 = nothing
    if length(rs) >= 2
        h = UnifiedIR.getregion(ir, rs[2])
        for a in h.args
            fr.env[a.id] = Any   # %exc
        end
        # throw-edge rule: the handler may run after any prefix of the body,
        # so refinements of cells the body stores are invalid inside it
        killed = push_store_kills!(fr, rs[1])
        r2 = infer_region!(fr, rs[2])
        killed && pop!(fr.refinements)
    end
    res = ⊔(fr.st, r1, r2)
    fr.env[s.id] = res === nothing ? Union{} : res
    return nothing
end

function infer_cfg!(fr::Frame, s::StmtId)
    ir = fr.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    # classical per-block fixpoint, local to the island (§5.5): per-block
    # entry state = (block args, Conditional refinements) — the VarTable of
    # typeinf_local, with the unrefined base carried by the monotone
    # cell-type map
    blockargs = Dict{Int32,Vector{Any}}()
    blockrefs = Dict{Int32,RefMap}()
    blockargs[rs[1].id] = Any[widenucond(a) for a in opls(fr, s, 1)]
    blockrefs[rs[1].id] = RefMap()
    changed = true
    result = nothing
    guard = 0
    cursrc = Ref{Int32}(0)   # region id of the block being walked

    function merge_edge!(dest::RegionId, vals::Vector{Any}, ref::RefMap)
        # backward edge (region ids are in creation = statement order): the
        # island may cycle — §5.1 rule 5 drops TERMINATES. Applies to both
        # in-island backedges and backward cross-island gotos (catch→loop-head).
        dest.id <= cursrc[] && (fr.effects &= ~UnifiedIR.FLAG_TERMINATES)
        if UnifiedIR.getregion(ir, dest).owner != s
            # sealed cross-island exit: mark reached; values cross scopes
            # through cells, not block args
            dest.id in fr.reached || (push!(fr.reached, dest.id); changed = true)
            return
        end
        old = get(blockargs, dest.id, nothing)
        if old === nothing
            blockargs[dest.id] = Any[widenucond(v) for v in vals]
            blockrefs[dest.id] = copy(ref)
            changed = true
        else
            for i in 1:length(vals)
                m = CC.tmerge(CC.fallback_lattice, old[i], widenucond(vals[i]))
                lat_eq(m, old[i]) || (old[i] = m; changed = true)
            end
            # refinement join: keep common subjects at their tmerge; drop others
            oldref = blockrefs[dest.id]
            for (k, v) in collect(oldref)
                nv = get(ref, k, nothing)
                if nv === nothing
                    delete!(oldref, k)
                    changed = true
                else
                    m = CC.tmerge(CC.fallback_lattice, widenucond(v), widenucond(nv))
                    lat_eq(m, v) || (oldref[k] = m; changed = true)
                end
            end
        end
    end

    while changed && (guard += 1) < 200
        changed = false
        result = nothing
        nreached0 = length(fr.reached)
        for rid in rs
            blk = UnifiedIR.getregion(ir, rid)
            args = get(blockargs, rid.id, nothing)
            if args === nothing
                # blocks entered only through cross-island gotos carry no args
                rid.id in fr.reached || continue
                args = Any[]
            end
            for (i, a) in enumerate(blk.args)
                i <= length(args) && (fr.env[a.id] = args[i])
            end
            cursrc[] = rid.id
            entryref = get(blockrefs, rid.id, nothing)
            get(ENV, "UIR_DEBUG", "") == "1" && println("DBG walk block ^", rid.id, " of cfg %", s.id, " entryref=", entryref)
            push!(fr.refinements, entryref === nothing ? RefMap() : copy(entryref))
            npush = 1
            curref() = begin   # currently active refinements, flattened
                m = RefMap()
                for k in (length(fr.refinements) - npush + 1):length(fr.refinements)
                    merge!(m, fr.refinements[k])
                end
                m
            end
            for st in UnifiedIR.region_stmts(ir, rid)
                k = UnifiedIR.stmt_kind(ir, st)
                k === K"region_arg" && continue
                if k === K"result"
                    result = ⊔(fr.st, result, joinvals(fr, opls(fr, st, 1)))
                elseif k === K"return"
                    note_return!(fr, st)
                elseif k === K"if"
                    carry = infer_if!(fr, st)
                    if carry !== nothing
                        push!(fr.refinements, carry)
                        npush += 1
                    end
                    fr.env[st.id] === Union{} && break
                elseif k === K"loop"
                    infer_loop!(fr, st)
                    fr.env[st.id] === Union{} && break
                elseif k === K"try"
                    infer_try!(fr, st)
                    fr.env[st.id] === Union{} && break
                elseif k === K"cfg"
                    infer_cfg!(fr, st)
                    fr.env[st.id] === Union{} && break
                elseif k === K"closure"
                    fr.env[st.id] = Any
                elseif k === K"break"
                    tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, st, 1))
                    fr.break_vals[tgt.id] = ⊔(fr.st, get(fr.break_vals, tgt.id, nothing),
                                              joinvals(fr, opls(fr, st, 2)))
                elseif k === K"continue"
                    tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, st, 1))
                    condl = opl(fr, UnifiedIR.getop(ir, st, 2))
                    vals = Any[widenucond(v) for v in opls(fr, st, 3)]
                    prev = get(fr.continue_vals, tgt.id, nothing)
                    joined = prev === nothing ? vals :
                        Any[CC.tmerge(CC.fallback_lattice, prev[i], vals[i]) for i in 1:length(vals)]
                    fr.continue_vals[tgt.id] = joined
                    if !(condl isa CC.Const && condl.val === true)
                        fr.break_vals[tgt.id] = ⊔(fr.st, get(fr.break_vals, tgt.id, nothing),
                                                  joinvals(fr, vals))
                    end
                elseif k === K"goto"
                    (dest, args_ops) = UnifiedIR.edge_bundles(ir, st)[1]
                    merge_edge!(dest, Any[opl(fr, o) for o in args_ops], curref())
                elseif k === K"br_if"
                    condl = opl(fr, UnifiedIR.getop(ir, st, 1))
                    get(ENV, "UIR_DEBUG", "") == "1" && println("DBG br_if %", st.id, " condl=", condl)
                    bundles = UnifiedIR.edge_bundles(ir, st)
                    ref = curref()
                    if condl isa CC.Const && condl.val isa Bool
                        # §10.3(b) inside islands: Const conditions kill edges
                        (dest, args_ops) = bundles[condl.val ? 1 : 2]
                        merge_edge!(dest, Any[opl(fr, o) for o in args_ops], ref)
                    elseif condl isa UCond && (condl.thentype === Union{} ||
                                               condl.elsetype === Union{})
                        # Bottom-armed Conditional decides the edge (stock rule)
                        taken = condl.elsetype === Union{}
                        eref = copy(ref)
                        eref[condl.subject] = taken ? condl.thentype : condl.elsetype
                        (dest, args_ops) = bundles[taken ? 1 : 2]
                        merge_edge!(dest, Any[opl(fr, o) for o in args_ops], eref)
                    elseif condl isa UCond
                        thenref = copy(ref); thenref[condl.subject] = condl.thentype
                        elseref = copy(ref); elseref[condl.subject] = condl.elsetype
                        (d1, a1) = bundles[1]
                        merge_edge!(d1, Any[opl(fr, o) for o in a1], thenref)
                        (d2, a2) = bundles[2]
                        merge_edge!(d2, Any[opl(fr, o) for o in a2], elseref)
                    else
                        for (dest, args_ops) in bundles
                            merge_edge!(dest, Any[opl(fr, o) for o in args_ops], ref)
                        end
                    end
                elseif k === K"switch" || k === K"await"
                    ref = curref()
                    for (dest, args_ops) in UnifiedIR.edge_bundles(ir, st)
                        merge_edge!(dest, Any[opl(fr, o) for o in args_ops], ref)
                    end
                elseif k === K"unreachable"
                else
                    fr.env[st.id] = transfer(fr, st, k)
                    if fr.pending_refine !== nothing
                        pr = fr.pending_refine::Pair
                        push!(fr.refinements, RefMap(pr.first => pr.second))
                        npush += 1
                        fr.pending_refine = nothing
                    end
                    # dead-tail rule: a Bottom statement never completes
                    fr.env[st.id] === Union{} && break
                end
            end
            for _ in 1:npush
                pop!(fr.refinements)
            end
        end
        length(fr.reached) > nreached0 && (changed = true)
    end
    fr.env[s.id] = result === nothing ? Union{} : result
    return nothing
end

