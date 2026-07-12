# The tree→region emitter: replaces JuliaLowering's linear_ir.jl goto emission.
#
# Input: one `K"lambda"` from the closure-converted tree (the same thing
# `compile_lambda` receives in linear_ir.jl): children are
#   [1] block of argument BindingIds/Placeholders
#   [2] block of static-parameter BindingIds
#   [3] body
#   [4] optional declared return type
#
# Output: a sealed `UnifiedIR.IR` whose region 1 args are the lambda
# arguments.
#
# Variable classes (decided by a structural pre-pass, `scan!`):
#   :arg  — unassigned lambda argument → region-1 region_arg, used directly
#   :ssa  — local with exactly one assignment that structurally dominates
#           every read (def's region path is a prefix of each read's path and
#           precedes it in walk order) and no `@isdefined` observation
#           → plain SSA value (the RHS operand), no storage
#   :cell — everything else → frame-class `cell` (§6) created at entry;
#           reads that are not dominated by the first assignment get the
#           undef guard `cell_isdefined` + `throw_undef_if_not` (§6)
#
# The pre-pass region paths are the same as (or finer than) the regions the
# emitter creates, so a "dominates" verdict here implies UnifiedIR visibility
# (§5.1) at emission time. Finer-only refinement is sound: it can only demote
# a variable to a cell, never wrongly promote.

# ---------------------------------------------------------------------------
# Pre-pass: structural def/use scan
# ---------------------------------------------------------------------------

mutable struct VarScan
    nassign::Int
    first_path::Union{Nothing,Vector{Int}}
    first_seq::Int
    reads_dominated::Bool   # every read dominated by the first assignment
    has_isdefined::Bool
    read::Bool
end
VarScan() = VarScan(0, nothing, 0, true, false, false)

mutable struct ScanState
    vars::Dict{Int,VarScan}
    seq::Int
    tok::Int
end
ScanState() = ScanState(Dict{Int,VarScan}(), 0, 0)

getvar!(s::ScanState, id::Integer) = get!(VarScan, s.vars, Int(id))

function subpath(s::ScanState, path::Vector{Int})
    s.tok += 1
    p = copy(path)
    push!(p, s.tok)
    return p
end

function scan_assign!(s::ScanState, id::Integer, path::Vector{Int})
    v = getvar!(s, id)
    s.seq += 1
    v.nassign += 1
    if v.first_path === nothing
        v.first_path = copy(path)
        v.first_seq = s.seq
    end
    return nothing
end

function scan_read!(s::ScanState, id::Integer, path::Vector{Int})
    v = getvar!(s, id)
    s.seq += 1
    v.read = true
    v.reads_dominated &= _dominates(v, path)
    return nothing
end

function _dominates(v::VarScan, path::Vector{Int})
    fp = v.first_path
    fp === nothing && return false
    length(fp) <= length(path) || return false
    for i in eachindex(fp)
        fp[i] == path[i] || return false
    end
    return true   # first_seq < current seq holds by construction (seq bumped)
end

function scan!(s::ScanState, ex, path::Vector{Int})
    k = kind(ex)
    if k == K"BindingId"
        scan_read!(s, ex.var_id, path)
        return nothing
    elseif is_leaf(ex)
        return nothing
    elseif (k == K"=" || k == K"constdecl") && numchildren(ex) == 2
        scan!(s, ex[2], path)
        lhs = ex[1]
        if kind(lhs) == K"BindingId"
            scan_assign!(s, lhs.var_id, path)
        end
        return nothing
    elseif k == K"newvar"
        return nothing        # undef reinit: not a read; guards handle undef
    elseif k == K"isdefined"
        c = ex[1]
        kind(c) == K"BindingId" && (getvar!(s, c.var_id).has_isdefined = true)
        return nothing
    elseif k == K"throw_undef_if_not"
        numchildren(ex) >= 2 && scan!(s, ex[2], path)   # [var cond]: var is not a read
        return nothing
    elseif k == K"if" || k == K"elseif" || k == K"&&" || k == K"||"
        scan!(s, ex[1], path)                        # cond / first term: unconditional
        for i in 2:numchildren(ex)
            scan!(s, ex[i], subpath(s, path))        # arms / later terms: conditional
        end
        return nothing
    elseif k == K"_while" || k == K"_do_while"
        p = subpath(s, path)                         # loop body region (incl. cond)
        scan!(s, ex[1], p)
        scan!(s, ex[2], p)
        return nothing
    elseif k == K"symbolicblock"
        scan!(s, ex[2], subpath(s, path))            # break-block region
        return nothing
    elseif k == K"trycatchelse" || k == K"tryfinally"
        for c in children(ex)
            scan!(s, c, subpath(s, path))            # body / handler / else regions
        end
        return nothing
    elseif k == K"break"
        numchildren(ex) >= 2 && scan!(s, ex[2], path)  # ex[1] is the label
        return nothing
    elseif k == K"lambda" || k == K"inert" || k == K"inert_syntaxtree" ||
           k == K"quote" || k == K"meta" || k == K"loopinfo"
        return nothing
    elseif k == K"method" || k == K"opaque_closure_method"
        for c in children(ex)
            kind(c) == K"lambda" || scan!(s, c, path)
        end
        return nothing
    # pre-closure-conversion forms (capture-analysis mode; absent afterwards)
    elseif k == K"local" || k == K"global"
        return nothing                       # declarations are not reads
    elseif k == K"decl"
        numchildren(ex) >= 2 && scan!(s, ex[2], path)   # type expr only
        return nothing
    elseif k == K"function_decl"
        kind(ex[1]) == K"BindingId" && scan_assign!(s, ex[1].var_id, path)
        return nothing
    elseif k == K"function_type"
        return nothing                       # names the TYPE, not the instance
    elseif k == K"_opaque_closure"
        for i in 2:numchildren(ex)
            kind(ex[i]) == K"lambda" || scan!(s, ex[i], path)
        end
        return nothing
    else
        for c in children(ex)
            scan!(s, c, path)
        end
        return nothing
    end
end

struct VarPlan
    mode::Symbol      # :arg | :ssa | :cell | :foreign (analysis mode only)
    checked::Bool     # cell reads need the §6 undef guard
end

# ---------------------------------------------------------------------------
# Capture-analysis mode (§5.7 precise capture, julia#15276): the same emitter
# driven over the PRE-closure-conversion tree, producing the throwaway IR the
# shared mem2reg fixpoint judges. Differences from normal emission:
#   * candidate captured variables are FORCED to frame cells (no undef guard
#     at reads — the promotion machinery itself is the definite-assignment
#     judge);
#   * variables captured from further out (`foreign`) become opaque values;
#   * closure-creation sites (`function_decl` / `_opaque_closure`) emit a
#     marker call `CAPTURE_SITE, site-index, cell_get(v)...` — one `cell_get`
#     per captured candidate — whose operands the fixpoint either resolves to
#     reaching definitions (value capture is legal) or leaves as memory reads
#     (the variable must stay shared);
#   * nested lambda BODIES are skipped (they run at call time, not here);
#   * unknown forms become opaque calls after a subtree check that they hide
#     no candidate stores and no closure-creation sites.
# Driver: ../capture_analysis.jl (JuliaLowering.analyze_captures_precise!).
# ---------------------------------------------------------------------------

"Marker callee identifying closure-creation sites in capture-analysis IR."
struct CaptureSiteMarker end
const CAPTURE_SITE = CaptureSiteMarker()

"Marker callee for opaque values/effects (unknown forms, foreign variables)."
struct OpaqueMarker end
const OPAQUE = OpaqueMarker()

mutable struct AnalysisState
    candidates::Set{Int}                 # native captured ids under analysis
    foreign::Set{Int}                    # captured-from-outer ids (opaque)
    capture_sets::Dict{Int,Vector{Int}}  # closure binding id -> candidates it captures
    site_caps::Vector{Vector{Int}}       # site index -> captured candidate ids
    site_stmts::Vector{StmtId}           # site index -> marker stmt (pre-compaction)
    decl_regions::Dict{Int,RegionId}     # candidate -> region of its K"local" decl
    created::Set{Int}                    # closure bindings already instantiated
end
AnalysisState(candidates, foreign, capture_sets) =
    AnalysisState(Set{Int}(candidates), Set{Int}(foreign), capture_sets,
                  Vector{Int}[], StmtId[], Dict{Int,RegionId}(), Set{Int}())

# ---------------------------------------------------------------------------
# Emitter context
# ---------------------------------------------------------------------------

mutable struct RegionSt
    terminated::Bool  # a terminator has been emitted as the region's last stmt
end

struct BreakTarget
    region::RegionId
    mode::Symbol      # :break | :continue
    valued::Bool
end

mutable struct EmitCtx{JC}
    b::Builder
    jl::JC                              # JuliaLowering closure-conversion ctx
    bindings::Bindings
    plans::Dict{Int,VarPlan}
    ssamap::Dict{Int,Operand}           # :arg / :ssa binding id → value
    cellmap::Dict{Int,StmtId}           # :cell binding id → cell stmt
    sparams::Dict{Int,Int}              # static-parameter binding id → index
    handler_exc::Vector{StmtId}         # innermost-last handler %exc args
    break_targets::Dict{String,BreakTarget}
    states::Vector{RegionSt}            # parallel to the builder's open stack
    rettype::Union{Nothing,Operand}     # declared return type, if any
    mod::Module
    cursrc::Any                         # innermost expression being compiled —
                                        #   provenance fallback for statements
                                        #   emitted without an explicit `src`
    ana::Union{Nothing,AnalysisState}   # capture-analysis mode (see above)
end

cur(ctx::EmitCtx) = ctx.states[end]
alive(ctx::EmitCtx) = !cur(ctx).terminated

nothing_op(ctx::EmitCtx) = vop(ctx.b.ir, nothing)

function srcloc(ex)
    ex === nothing && return (Int32(0), Int32(0), Int32(0))
    try
        line, col = JuliaSyntax.source_location(ex)
        return (Int32(line), Int32(col), Int32(0))
    catch
        return (Int32(0), Int32(0), Int32(0))
    end
end

"""
Record graph-qualified provenance (§3.7 Level 2): the statement's `:source`
column entry is the originating syntax tree CURSOR (graph + id —
self-qualifying across graphs; the generic `provenance` walk hops through it
back into the syntax graph and on to the `SourceRef` terminal). The `debug`
line-info column keeps being derived from the same source at emission.
"""
function record_source!(ctx::EmitCtx, s::StmtId, src)
    v = src === nothing ? ctx.cursrc : src
    v === nothing && return nothing
    UnifiedIR.getattr(ctx.b.ir, :source)[Int(s.id)] = v
    return nothing
end

"Append a statement to the current region, tracking terminator state."
function stmt!(ctx::EmitCtx, k, args...; type = Any, src = nothing)
    st = cur(ctx)
    st.terminated && error("UnifiedBackend internal error: emitting into a terminated region")
    s = append_stmt!(ctx.b, k, args...; type, debug = srcloc(src === nothing ? ctx.cursrc : src))
    record_source!(ctx, s, src)
    UnifiedIR.is_terminator(k) && (st.terminated = true)
    return s
end

function open_arm!(ctx::EmitCtx, owner::StmtId; kind = REGION_ARM)
    r = open_region!(ctx.b, owner; kind)
    push!(ctx.states, RegionSt(false))
    return r
end

function close_arm!(ctx::EmitCtx)
    st = pop!(ctx.states)
    st.terminated || error("UnifiedBackend internal error: closing a region without a terminator")
    close_region!(ctx.b)
    return nothing
end

function with_target(f, ctx::EmitCtx, name::String, tgt::BreakTarget)
    old = get(ctx.break_targets, name, nothing)
    ctx.break_targets[name] = tgt
    try
        return f()
    finally
        old === nothing ? delete!(ctx.break_targets, name) :
                          (ctx.break_targets[name] = old)
    end
end

unsupported(ex, detail::String = "") =
    throw(UnsupportedForm(string(kind(ex)), detail))

# ---------------------------------------------------------------------------
# Entry points
# ---------------------------------------------------------------------------

function emit_method(jlctx, mex)::LoweredMethod
    name = method_name(jlctx, mex[1])
    ir, nargs, slotnames, _ = emit_lambda(jlctx, mex[3], name)
    return LoweredMethod(name, nargs, slotnames, ir)
end

function method_name(jlctx, fname)::Symbol
    k = kind(fname)
    if k == K"BindingId"
        return Symbol(jlctx.bindings.info[fname.var_id].name)
    elseif k == K"Value" && fname.value isa GlobalRef
        return (fname.value::GlobalRef).name
    elseif k == K"globalref"
        return Symbol(fname.name_val)
    else
        return Symbol("#anon")
    end
end

"""
    emit_lambda(jlctx, lam, name; analysis = nothing) -> (ir, nargs, slotnames, ctx)

Lower one (non-toplevel) `K"lambda"` to UnifiedIR. Mirrors what linear_ir.jl's
`compile_lambda` receives, but emits regions instead of gotos. With
`analysis::AnalysisState` the emitter runs in capture-analysis mode (see the
block comment above `CaptureSiteMarker`) over a pre-closure-conversion tree.
"""
function emit_lambda(jlctx, lam, name::Symbol; analysis = nothing)
    kind(lam) == K"lambda" || unsupported(lam, "expected a lambda")
    lam.is_toplevel_thunk && unsupported(lam, "toplevel thunk bodies are not lowered (methods are extracted instead)")
    args = collect(children(lam[1]))
    sparams = collect(children(lam[2]))
    body = lam[3]
    rett_ex = numchildren(lam) >= 4 ? lam[4] : nothing
    bindings = jlctx.bindings::Bindings

    # ---- pre-pass -----------------------------------------------------
    sc = ScanState()
    scan!(sc, body, Int[])
    rett_ex !== nothing && scan!(sc, rett_ex, Int[])

    argids = Set{Int}()
    for a in args
        kind(a) == K"BindingId" && push!(argids, Int(a.var_id))
    end
    plans = Dict{Int,VarPlan}()
    for (id, v) in sc.vars
        binfo = bindings.info[id]
        if binfo.kind === :argument
            plans[id] = v.nassign > 0 ? VarPlan(:cell, false) : VarPlan(:arg, false)
        elseif binfo.kind === :local
            if v.nassign == 1 && v.reads_dominated && !v.has_isdefined
                plans[id] = VarPlan(:ssa, false)
            else
                plans[id] = VarPlan(:cell, !v.reads_dominated)
            end
        end
        # :global / :static_parameter bindings are not frame storage
    end
    for id in argids
        haskey(plans, id) || (plans[id] = VarPlan(:arg, false))
    end
    if analysis !== nothing
        ana = analysis::AnalysisState
        # candidates are the machinery's problem: frame cells, no guards.
        # (Candidates untouched by the body itself — e.g. only ever captured —
        # get their plan created here.)
        for id in ana.candidates
            plans[id] = VarPlan(:cell, false)
        end
        # captured-from-outer variables are not this frame's storage
        for id in ana.foreign
            plans[id] = VarPlan(:foreign, false)
        end
    end

    # ---- builder setup ------------------------------------------------
    # the provenance universe (§3.7 Level 2): every emitted statement carries
    # a :source cursor into the lowering syntax graph
    b = Builder(name = name, cols = (source = UnifiedIR.ProvenanceCol(),))
    ctx = EmitCtx(b, jlctx, bindings, plans, Dict{Int,Operand}(),
                  Dict{Int,StmtId}(), Dict{Int,Int}(), StmtId[],
                  Dict{String,BreakTarget}(), [RegionSt(false)],
                  nothing, jlctx.mod::Module, lam, analysis)

    slotnames = Symbol[]
    argstmts = Dict{Int,StmtId}()
    for a in args
        s = append_stmt!(b, UnifiedIR.K"region_arg"; type = Any, debug = srcloc(a))
        record_source!(ctx, s, a)
        if kind(a) == K"BindingId"
            id = Int(a.var_id)
            push!(slotnames, Symbol(bindings.info[id].name))
            argstmts[id] = s
            plans[id].mode === :arg && (ctx.ssamap[id] = op_stmt(s))
        else
            push!(slotnames, :_)   # K"Placeholder": unused argument
        end
    end

    # frame cells for :cell-planned bindings, created at entry (region 1)
    for id in sort!(collect(keys(plans)))
        p = plans[id]
        p.mode === :cell || continue
        c = stmt!(ctx, UnifiedIR.K"cell", Any; type = Any)
        ctx.cellmap[id] = c
        if haskey(argstmts, id)   # assigned argument: initialize from the arg
            stmt!(ctx, UnifiedIR.K"cell_set", c, argstmts[id])
        end
    end

    for (i, sp) in enumerate(sparams)
        kind(sp) == K"BindingId" || continue
        ctx.sparams[Int(sp.var_id)] = i
    end

    if rett_ex !== nothing
        rt = emit_value(ctx, rett_ex)
        rt === nothing && unsupported(rett_ex, "return type expression diverges")
        ctx.rettype = rt
    end

    # ---- body (tail position: value then return) -----------------------
    v = emit_value(ctx, body)
    v === nothing || emit_return!(ctx, v, body)
    ctx.states[1].terminated ||
        error("UnifiedBackend internal error: function body did not terminate")

    ir = finish!(b)
    return ir, length(args), slotnames, ctx
end

function emit_return!(ctx::EmitCtx, v::Operand, srcex)
    if ctx.rettype !== nothing
        rt = ctx.rettype
        cv = stmt!(ctx, UnifiedIR.K"call", GlobalRef(Base, :convert), rt, v; src = srcex)
        ta = stmt!(ctx, UnifiedIR.K"call", GlobalRef(Core, :typeassert), cv, rt; src = srcex)
        v = op_stmt(ta)
    end
    stmt!(ctx, UnifiedIR.K"return", v; src = srcex)
    return nothing
end

# ---------------------------------------------------------------------------
# The compiler: mirrors linear_ir.jl `compile(ctx, ex, needs_value, in_tail_pos)`.
# Tail position is handled by the caller (emit_lambda / emit_return!); explicit
# K"return" nodes emit the return terminator directly.
#
# Returns an `Operand` (the expression's value; a `nothing` constant operand
# in pure-effect positions), or `nothing` if control diverged (a return /
# break / continue terminator was emitted while evaluating).
# ---------------------------------------------------------------------------

emit_value(ctx::EmitCtx, ex) = compile(ctx, ex, true)
emit_effect(ctx::EmitCtx, ex) = compile(ctx, ex, false)

function compile(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    old = ctx.cursrc
    ctx.cursrc = ex
    try
        return _compile(ctx, ex, needs_value)
    finally
        ctx.cursrc = old
    end
end

function _compile(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    alive(ctx) || return nothing
    k = kind(ex)
    if k == K"BindingId"
        return read_binding(ctx, ex, needs_value)
    elseif JuliaSyntax.is_literal(k)
        return vop(ctx.b.ir, ex.value)
    elseif k == K"nothing"
        return nothing_op(ctx)
    elseif k == K"Symbol"
        return vop(ctx.b.ir, Symbol(ex.name_val::String))
    elseif k == K"top"
        return vop(ctx.b.ir, GlobalRef(Base, Symbol(ex.name_val::String)))
    elseif k == K"core"
        return vop(ctx.b.ir, GlobalRef(Core, Symbol(ex.name_val::String)))
    elseif k == K"Value"
        return vop(ctx.b.ir, ex.value)
    elseif k == K"globalref"
        gr = GlobalRef(ex.mod::Module, Symbol(ex.name_val::String))
        return op_stmt(stmt!(ctx, UnifiedIR.K"globalref", gr; src = ex))
    elseif k == K"SourceLocation"
        return vop(ctx.b.ir, JuliaSyntax.source_location(LineNumberNode, ex))
    elseif k == K"inert"
        return vop(ctx.b.ir, JuliaLowering.est_to_expr(ex))
    elseif k == K"inert_syntaxtree"
        return vop(ctx.b.ir, ex[1])
    elseif k == K"Placeholder"
        needs_value &&
            throw(UnsupportedForm("Placeholder",
                "all-underscore identifiers are write-only and their values cannot be used"))
        return nothing_op(ctx)
    elseif k == K"TOMBSTONE"
        return nothing_op(ctx)
    elseif k == K"call" || k == K"new" || k == K"splatnew"
        return emit_call(ctx, ex, k)
    elseif k == K"=" || k == K"constdecl"
        return emit_assign(ctx, ex, needs_value)
    elseif k == K"block" || k == K"scope_block"
        return emit_block(ctx, ex, needs_value)
    elseif k == K"symbolicblock"
        return emit_symbolicblock(ctx, ex, needs_value)
    elseif k == K"break"
        return emit_break(ctx, ex)
    elseif k == K"return"
        v = emit_value(ctx, ex[1])
        v === nothing || emit_return!(ctx, v, ex)
        return nothing
    elseif k == K"removable"
        return needs_value ? compile(ctx, ex[1], true) : nothing_op(ctx)
    elseif k == K"if" || k == K"elseif"
        return emit_if(ctx, ex, needs_value)
    elseif k == K"&&" || k == K"||"
        return emit_shortcircuit(ctx, ex, k == K"&&", 1)
    elseif k == K"_while"
        return emit_while(ctx, ex[1], ex[2], needs_value, ex, false)
    elseif k == K"_do_while"
        return emit_do_while(ctx, ex, needs_value)
    elseif k == K"trycatchelse" || k == K"tryfinally"
        return emit_try(ctx, ex, needs_value)
    elseif k == K"isdefined"
        return emit_isdefined(ctx, ex)
    elseif k == K"throw_undef_if_not"
        # [K"throw_undef_if_not" var cond]
        c = emit_value(ctx, ex[2])
        c === nothing && return nothing
        nm = kind(ex[1]) == K"BindingId" ?
            Symbol(ctx.bindings.info[ex[1].var_id].name) : Symbol("var")
        stmt!(ctx, UnifiedIR.K"throw_undef_if_not", c, nm; src = ex)
        return nothing_op(ctx)
    elseif k == K"newvar"
        id = Int(ex[1].var_id)
        p = get(ctx.plans, id, nothing)
        if p !== nothing && p.mode === :cell
            stmt!(ctx, UnifiedIR.K"cell_new", ctx.cellmap[id]; src = ex)
        end
        return nothing_op(ctx)
    elseif k == K"boundscheck"
        return op_stmt(stmt!(ctx, UnifiedIR.K"boundscheck"; type = Bool, src = ex))
    elseif k == K"gc_preserve_begin"
        ops = Operand[]
        for c in children(ex)
            v = emit_value(ctx, c)
            v === nothing && return nothing
            push!(ops, v)
        end
        return op_stmt(stmt!(ctx, UnifiedIR.K"gc_preserve_begin", ops...; src = ex))
    elseif k == K"gc_preserve_end"
        v = emit_value(ctx, ex[1])
        v === nothing && return nothing
        stmt!(ctx, UnifiedIR.K"gc_preserve_end", v; src = ex)
        return nothing_op(ctx)
    elseif k == K"latestworld"
        stmt!(ctx, UnifiedIR.K"latestworld"; src = ex)
        return nothing_op(ctx)
    elseif k == K"latestworld_if_toplevel"
        return nothing_op(ctx)      # never a toplevel thunk here
    elseif k == K"meta" || k == K"inbounds" || k == K"inbounds_pop" ||
           k == K"inline" || k == K"noinline" || k == K"purity" || k == K"loopinfo"
        return nothing_op(ctx)      # compile hints: flag/column material, dropped in v1
    elseif k == K"unused_only"
        return compile(ctx, ex[1], needs_value)
    elseif k == K"static_parameter"
        return UnifiedIR.op_sparam(Int(ex.var_id))
    elseif ctx.ana !== nothing && (k == K"local" || k == K"global" || k == K"decl" ||
           k == K"function_decl" || k == K"function_type" || k == K"method_defs" ||
           k == K"method" || k == K"_opaque_closure" || k == K"lambda")
        return emit_analysis_form(ctx, ex, k, needs_value)
    elseif k == K"method"
        unsupported(ex, "method definition inside a method body (closures define methods at top level)")
    elseif k == K"lambda" || k == K"opaque_closure_method" || k == K"new_opaque_closure"
        unsupported(ex, "nested lambda / opaque closure")
    elseif k == K"foreigncall" || k == K"cfunction" || k == K"static_eval" ||
           k == K"foreignsymbol"
        ctx.ana !== nothing && return _ana_opaque(ctx, ex)
        unsupported(ex, "foreign calls are not supported in v1")
    elseif k == K"symboliclabel" || k == K"symbolicgoto" || k == K"oldsymbolicgoto"
        unsupported(ex, "@label/@goto requires the cfg island form (not emitted in v1)")
    elseif k == K"captured_local"
        unsupported(ex, "captured local in a global method")
    elseif k == K"copyast"
        v = emit_value(ctx, ex[1])
        v === nothing && return nothing
        return op_stmt(stmt!(ctx, UnifiedIR.K"copyast", v; src = ex))
    else
        ctx.ana !== nothing && return _ana_opaque(ctx, ex)
        unsupported(ex)
    end
end

# ---------------------------------------------------------------------------
# Capture-analysis forms (pre-closure-conversion tree; `ctx.ana` set)
# ---------------------------------------------------------------------------

function emit_analysis_form(ctx::EmitCtx, ex, k, needs_value::Bool)::Union{Operand,Nothing}
    ana = ctx.ana::AnalysisState
    if k == K"local"
        c = ex[1]
        if kind(c) == K"BindingId"
            id = Int(c.var_id)
            p = get(ctx.plans, id, nothing)
            if p !== nothing && p.mode === :cell
                # the declaration point: records per-iteration freshness for
                # the backedge rule, and re-undefines (stock re-boxes here)
                haskey(ana.decl_regions, id) ||
                    (ana.decl_regions[id] = current_region(ctx.b))
                stmt!(ctx, UnifiedIR.K"cell_new", ctx.cellmap[id]; src = ex)
            end
        end
        return nothing_op(ctx)
    elseif k == K"global"
        return nothing_op(ctx)
    elseif k == K"decl"
        # typed-local declaration [K"decl" var Texpr]: the type expression is
        # evaluated (it may read locals / throw); the decl itself is no store
        if numchildren(ex) >= 2
            v = emit_value(ctx, ex[2])
            v === nothing && return nothing
        end
        return nothing_op(ctx)
    elseif k == K"function_decl"
        f = ex[1]
        kind(f) == K"BindingId" || return _ana_opaque(ctx, ex)
        id = Int(f.var_id)
        if !haskey(ana.capture_sets, id)
            # global method declaration: a binding-table effect, no capture
            return nothing_op(ctx)
        end
        # the instance is created once per binding (later decls are no-ops)
        id in ana.created && return nothing_op(ctx)
        push!(ana.created, id)
        m = emit_capture_site(ctx, ex, id)
        assign_binding!(ctx, ex, id, op_stmt(m))
        return nothing_op(ctx)
    elseif k == K"function_type"
        # names the closure TYPE; reads no instance
        return op_stmt(stmt!(ctx, UnifiedIR.K"call", vop(ctx.b.ir, OPAQUE); src = ex))
    elseif k == K"method_defs"
        # ex[1] is the closure name binding (no runtime read); method bodies
        # run at call time — the signature svec evaluation happens here
        for i in 2:numchildren(ex)
            compile(ctx, ex[i], false) === nothing && return nothing
        end
        return nothing_op(ctx)
    elseif k == K"method"
        if numchildren(ex) == 3
            # [method name sig lambda]: sig svec evaluates now; body deferred
            v = emit_value(ctx, ex[2])
            v === nothing && return nothing
            if kind(ex[3]) != K"lambda"
                emit_effect(ctx, ex[3]) === nothing && return nothing
            end
            return op_stmt(stmt!(ctx, UnifiedIR.K"call", vop(ctx.b.ir, OPAQUE), v; src = ex))
        end
        return nothing_op(ctx)   # 1-arg form: global binding side effect
    elseif k == K"_opaque_closure"
        # [oc key argt rt_lb rt_ub allow_partial nargs is_va functionloc lambda]
        for i in 2:4
            v = emit_value(ctx, ex[i])
            v === nothing && return nothing
        end
        key = ex[1]
        if kind(key) == K"BindingId" && haskey(ana.capture_sets, Int(key.var_id))
            return op_stmt(emit_capture_site(ctx, ex, Int(key.var_id)))
        end
        return _ana_opaque(ctx, ex)
    else # k == K"lambda": bare lambda (global method capturing locals by value)
        return _ana_opaque(ctx, ex)
    end
end

"Emit the capture-site marker: `call CAPTURE_SITE, site-index, cell_get(v)...`."
function emit_capture_site(ctx::EmitCtx, ex, id::Int)
    ana = ctx.ana::AnalysisState
    caps = ana.capture_sets[id]
    ops = Operand[vop(ctx.b.ir, CAPTURE_SITE), vop(ctx.b.ir, length(ana.site_caps) + 1)]
    for v in caps
        g = stmt!(ctx, UnifiedIR.K"cell_get", ctx.cellmap[v]; src = ex)
        push!(ops, op_stmt(g))
    end
    m = stmt!(ctx, UnifiedIR.K"call", ops...; src = ex)
    push!(ana.site_caps, caps)
    push!(ana.site_stmts, m)
    return m
end

"Mirror `emit_assign`'s binding store for analysis-synthesized values."
function assign_binding!(ctx::EmitCtx, ex, id::Int, v::Operand)
    binfo = ctx.bindings.info[id]
    if binfo.kind === :global
        stmt!(ctx, UnifiedIR.K"call", GlobalRef(Core, :setglobal!),
              binfo.mod::Module, Symbol(binfo.name), v; src = ex)
        return nothing
    end
    p = get(ctx.plans, id, nothing)
    p === nothing && return nothing         # never read: no storage planned
    if p.mode === :ssa
        ctx.ssamap[id] = v
    elseif p.mode === :cell
        stmt!(ctx, UnifiedIR.K"cell_set", ctx.cellmap[id], v; src = ex)
    elseif p.mode === :foreign
        stmt!(ctx, UnifiedIR.K"call", vop(ctx.b.ir, OPAQUE), v; src = ex)
    end
    return nothing
end

"""
Opaque stand-in for a form the analysis does not model: verify the subtree
hides no store to a candidate and no closure-creation site (either would
invalidate verdicts — bail to the syntactic fallback), then emit one
unknown-effect, unknown-value call.
"""
function _ana_opaque(ctx::EmitCtx, ex)
    _ana_check_opaque(ctx.ana::AnalysisState, ex)
    return op_stmt(stmt!(ctx, UnifiedIR.K"call", vop(ctx.b.ir, OPAQUE); src = ex))
end

function _ana_check_opaque(ana::AnalysisState, ex)
    k = kind(ex)
    if k == K"inert" || k == K"inert_syntaxtree" || k == K"quote"
        return nothing
    elseif k == K"=" && numchildren(ex) == 2
        lhs = ex[1]
        if kind(lhs) == K"BindingId" && Int(lhs.var_id) in ana.candidates
            throw(UnsupportedForm("capture-analysis",
                "store to a captured variable in an unmodeled form"))
        end
        _ana_check_opaque(ana, ex[2])
        return nothing
    elseif k == K"function_decl" || k == K"_opaque_closure" || k == K"method"
        throw(UnsupportedForm("capture-analysis",
            "closure creation in an unmodeled form"))
    elseif is_leaf(ex)
        return nothing
    else
        for c in children(ex)
            _ana_check_opaque(ana, c)
        end
        return nothing
    end
end

# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------

function read_binding(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    id = Int(ex.var_id)
    binfo = ctx.bindings.info[id]
    if binfo.kind === :global
        gr = GlobalRef(binfo.mod::Module, Symbol(binfo.name))
        # a global read can throw UndefVarError: pin its evaluation point
        return op_stmt(stmt!(ctx, UnifiedIR.K"globalref", gr; src = ex))
    elseif binfo.kind === :static_parameter
        i = get(ctx.sparams, id, nothing)
        i === nothing && unsupported(ex, "static parameter not in this lambda")
        return UnifiedIR.op_sparam(i)
    end
    p = get(ctx.plans, id, nothing)
    p === nothing &&
        error("UnifiedBackend internal error: unplanned binding $(binfo.name)")
    if p.mode === :foreign
        # captured from an enclosing frame: an unknown (but defined) value
        return op_stmt(stmt!(ctx, UnifiedIR.K"call", vop(ctx.b.ir, OPAQUE); src = ex))
    elseif p.mode === :cell
        c = ctx.cellmap[id]
        if p.checked
            d = stmt!(ctx, UnifiedIR.K"cell_isdefined", c; type = Bool, src = ex)
            stmt!(ctx, UnifiedIR.K"throw_undef_if_not", d, Symbol(binfo.name); src = ex)
        end
        return op_stmt(stmt!(ctx, UnifiedIR.K"cell_get", c; src = ex))
    else
        v = get(ctx.ssamap, id, nothing)
        v === nothing &&
            error("UnifiedBackend internal error: read of $(binfo.name) before its SSA definition")
        return v
    end
end

function emit_assign(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    k = kind(ex)
    if k == K"constdecl" && numchildren(ex) == 1
        # undefined-constant declaration
        lhs = ex[1]
        mod, name = if kind(lhs) == K"BindingId"
            binfo = ctx.bindings.info[lhs.var_id]
            binfo.mod::Module, Symbol(binfo.name)
        elseif kind(lhs) == K"Value" && lhs.value isa GlobalRef
            gr = lhs.value::GlobalRef
            gr.mod, gr.name
        else
            unsupported(ex, "constdecl target")
        end
        stmt!(ctx, UnifiedIR.K"call", GlobalRef(Core, :declare_const), mod, name; src = ex)
        return nothing_op(ctx)
    end
    lhs, rhs = ex[1], ex[2]
    kind(lhs) == K"Placeholder" && return compile(ctx, rhs, needs_value)
    v = emit_value(ctx, rhs)
    v === nothing && return nothing
    if kind(lhs) == K"BindingId"
        id = Int(lhs.var_id)
        binfo = ctx.bindings.info[id]
        if binfo.kind === :global
            f = k == K"constdecl" ? GlobalRef(Core, :declare_const) :
                                    GlobalRef(Core, :setglobal!)
            stmt!(ctx, UnifiedIR.K"call", f, binfo.mod::Module, Symbol(binfo.name), v;
                  src = ex)
        else
            p = get(ctx.plans, id, nothing)
            p === nothing &&
                error("UnifiedBackend internal error: unplanned assignment to $(binfo.name)")
            if p.mode === :ssa
                ctx.ssamap[id] = v
            elseif p.mode === :cell
                stmt!(ctx, UnifiedIR.K"cell_set", ctx.cellmap[id], v; src = ex)
            elseif p.mode === :foreign
                stmt!(ctx, UnifiedIR.K"call", vop(ctx.b.ir, OPAQUE), v; src = ex)
            else
                error("UnifiedBackend internal error: assignment to unassigned argument plan")
            end
        end
    else
        unsupported(ex, "assignment target of kind $(kind(lhs))")
    end
    return needs_value ? v : nothing_op(ctx)
end

# ---------------------------------------------------------------------------
# Calls
# ---------------------------------------------------------------------------

function emit_call(ctx::EmitCtx, ex, k)::Union{Operand,Nothing}
    # `current_exception()` (inserted by try/catch desugaring) resolves to the
    # innermost handler's %exc region argument — K"the_exception" per §6.
    if k == K"call" && numchildren(ex) == 1 && kind(ex[1]) == K"Value" &&
       ex[1].value === JuliaLowering.current_exception && !isempty(ctx.handler_exc)
        return op_stmt(ctx.handler_exc[end])
    end
    ops = Operand[]
    for c in children(ex)
        v = emit_value(ctx, c)
        v === nothing && return nothing   # e.g. f(return x)
        push!(ops, v)
    end
    irk = k == K"call" ? UnifiedIR.K"call" :
          k == K"new"  ? UnifiedIR.K"new"  : UnifiedIR.K"splatnew"
    return op_stmt(stmt!(ctx, irk, ops...; src = ex))
end

# ---------------------------------------------------------------------------
# Sequencing and control flow
# ---------------------------------------------------------------------------

function emit_block(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    n = numchildren(ex)
    n == 0 && return nothing_op(ctx)
    res = nothing_op(ctx)
    for i in 1:n
        v = compile(ctx, ex[i], needs_value && i == n)
        v === nothing && return nothing
        i == n && (res = v)
    end
    return res
end

"""
`if` becomes the K"if" region op (§5.2). In value position each arm ends in
`result <val>`; unused ifs get the 0-result form with bare `result`s. Arms that
diverge (return/break/continue inside) keep their own exit terminator and
contribute nothing to the join.
"""
function emit_if(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    numchildren(ex) <= 3 || unsupported(ex, "if with more than 3 children")
    c = emit_condition(ctx, ex[1])
    c === nothing && return nothing
    s = stmt!(ctx, UnifiedIR.K"if", c; src = ex)
    open_arm!(ctx, s)
    v1 = compile(ctx, ex[2], needs_value)
    if alive(ctx)
        needs_value ? stmt!(ctx, UnifiedIR.K"result", v1) : stmt!(ctx, UnifiedIR.K"result")
    end
    close_arm!(ctx)
    has_else = numchildren(ex) > 2
    if has_else || needs_value
        open_arm!(ctx, s)
        v2 = has_else ? compile(ctx, ex[3], needs_value) : nothing_op(ctx)
        if alive(ctx)
            needs_value ? stmt!(ctx, UnifiedIR.K"result", v2) : stmt!(ctx, UnifiedIR.K"result")
        end
        close_arm!(ctx)
    end
    return needs_value ? op_stmt(s) : nothing_op(ctx)
end

"""
Conditions: post-desugar, `&&`/`||` survive in condition position (linear_ir
lowers them to short-circuit gotos; here they become nested value `if`s whose
arms produce Bools). Block-wrapped conditions evaluate their prefix first.
"""
function emit_condition(ctx::EmitCtx, ex)::Union{Operand,Nothing}
    k = kind(ex)
    if k == K"block"
        n = numchildren(ex)
        n == 0 && return nothing_op(ctx)
        for i in 1:n-1
            v = emit_effect(ctx, ex[i])
            v === nothing && return nothing
        end
        return emit_condition(ctx, ex[n])
    elseif k == K"&&" || k == K"||"
        return emit_shortcircuit(ctx, ex, k == K"&&", 1)
    else
        return emit_value(ctx, ex)
    end
end

function emit_shortcircuit(ctx::EmitCtx, ex, is_and::Bool, i::Int)::Union{Operand,Nothing}
    c = emit_condition(ctx, ex[i])
    c === nothing && return nothing
    i == numchildren(ex) && return c
    s = stmt!(ctx, UnifiedIR.K"if", c; src = ex)
    open_arm!(ctx, s)
    if is_and
        v = emit_shortcircuit(ctx, ex, is_and, i + 1)
        alive(ctx) && stmt!(ctx, UnifiedIR.K"result", v)
    else
        stmt!(ctx, UnifiedIR.K"result", true)
    end
    close_arm!(ctx)
    open_arm!(ctx, s)
    if is_and
        stmt!(ctx, UnifiedIR.K"result", false)
    else
        v = emit_shortcircuit(ctx, ex, is_and, i + 1)
        alive(ctx) && stmt!(ctx, UnifiedIR.K"result", v)
    end
    close_arm!(ctx)
    return op_stmt(s)
end

"""
`symbolicblock` — JuliaLowering's break-block:
  * the `while` pattern `symbolicblock(loop-exit, _while(cond,
    symbolicblock(loop-cont, body)))` collapses into ONE K"loop":
    `loop { c = cond; if c {result} else {break ^body}; body; continue ^body true }`
    with source `break`→`break ^body` and `continue`→`continue ^body true`.
  * any other labelled block becomes the §5.9 single-iteration loop
    (WebAssembly-block pattern): `loop { body; break ^self (val?) }` with
    inner `break label (val?)` exits targeting it.
"""
function emit_symbolicblock(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    label = ex[1].name_val::String
    body = ex[2]
    if label == "loop-exit" && kind(body) == K"_while" &&
       kind(body[2]) == K"symbolicblock" && body[2][1].name_val::String == "loop-cont"
        return emit_while(ctx, body[1], body[2][2], needs_value, ex, true)
    end
    s = stmt!(ctx, UnifiedIR.K"loop"; src = ex)
    r = open_arm!(ctx, s; kind = REGION_LOOP_BODY)
    with_target(ctx, label, BreakTarget(r, :break, needs_value)) do
        v = compile(ctx, body, needs_value)
        if alive(ctx)
            needs_value ? stmt!(ctx, UnifiedIR.K"break", op_region(r), v) :
                          stmt!(ctx, UnifiedIR.K"break", op_region(r))
        end
    end
    close_arm!(ctx)
    return needs_value ? op_stmt(s) : nothing_op(ctx)
end

"""
`while` (K"_while") → K"loop" (§5.3), no carried values in v1 — mutable loop
variables live in cells. Do-while shape with a body-initial condition test:

    %r = loop {
        %c = <cond>
        if %c { result } else { break ^body }
        <body>
        continue ^body true
    }

`continue ^body true` repeats unconditionally; the loop is left only through
the `break`. A while loop's value is `nothing`.
"""
function emit_while(ctx::EmitCtx, cond, body, needs_value::Bool, srcex,
                    register_targets::Bool)::Union{Operand,Nothing}
    s = stmt!(ctx, UnifiedIR.K"loop"; src = srcex)
    r = open_arm!(ctx, s; kind = REGION_LOOP_BODY)
    emitbody = function ()
        c = emit_condition(ctx, cond)
        if c !== nothing
            ifs = stmt!(ctx, UnifiedIR.K"if", c; src = srcex)
            open_arm!(ctx, ifs)
            stmt!(ctx, UnifiedIR.K"result")
            close_arm!(ctx)
            open_arm!(ctx, ifs)
            stmt!(ctx, UnifiedIR.K"break", op_region(r))
            close_arm!(ctx)
            emit_effect(ctx, body)
            alive(ctx) && stmt!(ctx, UnifiedIR.K"continue", op_region(r), true)
        end
    end
    if register_targets
        with_target(ctx, "loop-exit", BreakTarget(r, :break, false)) do
            with_target(ctx, "loop-cont", BreakTarget(r, :continue, false)) do
                emitbody()
            end
        end
    else
        emitbody()
    end
    close_arm!(ctx)
    return nothing_op(ctx)   # a while loop's value is `nothing`
end

"""
`_do_while(body, cond)` (from `for` loop desugaring) → K"loop" whose body runs
then re-tests: `loop { body; %c = cond; continue ^body %c }`. The `loop-cont`
symbolicblock inside the body (where source `continue` lands) is the generic
single-iteration loop, so `continue` correctly re-runs the iterator advance.
"""
function emit_do_while(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    s = stmt!(ctx, UnifiedIR.K"loop"; src = ex)
    r = open_arm!(ctx, s; kind = REGION_LOOP_BODY)
    emit_effect(ctx, ex[1])
    if alive(ctx)
        c = emit_condition(ctx, ex[2])
        c === nothing || stmt!(ctx, UnifiedIR.K"continue", op_region(r), c)
    end
    close_arm!(ctx)
    return nothing_op(ctx)
end

"`break label [val]` → K\"break\"/K\"continue\" targeting the right loop body region."
function emit_break(ctx::EmitCtx, ex)::Union{Operand,Nothing}
    name = ex[1].name_val::String
    t = get(ctx.break_targets, name, nothing)
    if t === nothing
        detail = name == "loop-exit" ? "`break` outside a loop" :
                 name == "loop-cont" ? "`continue` outside a loop" :
                 "break target `$name` is not in scope"
        throw(UnsupportedForm("break", detail))
    end
    if t.mode === :continue
        numchildren(ex) < 2 || throw(UnsupportedForm("break", "valued continue"))
        stmt!(ctx, UnifiedIR.K"continue", op_region(t.region), true; src = ex)
    elseif numchildren(ex) >= 2
        t.valued || throw(UnsupportedForm("break",
            "break with value to a label whose block value is unused"))
        v = emit_value(ctx, ex[2])
        v === nothing && return nothing
        stmt!(ctx, UnifiedIR.K"break", op_region(t.region), v; src = ex)
    elseif t.valued
        stmt!(ctx, UnifiedIR.K"break", op_region(t.region), nothing_op(ctx); src = ex)
    else
        stmt!(ctx, UnifiedIR.K"break", op_region(t.region); src = ex)
    end
    return nothing
end

# ---------------------------------------------------------------------------
# Exception handling (§6)
# ---------------------------------------------------------------------------

"""
`trycatchelse` → K"try" op: body region + handler region (REGION_HANDLER)
whose leading region_arg is the exception. `current_exception()` inside the
handler resolves to that arg; `rethrow` stays a call. Early exits
(return/break) out of the body are ordinary sealed exits whose semantics
include the structural leave/pop actions (§5.9/§6).

`tryfinally` is supported when no early exit crosses it: the finally block is
duplicated on the normal path and in a catch-all handler that rethrows.
JuliaLowering's tag-based finally lowering lived in linear_ir.jl (the pass
this file replaces), so the general early-exit case is UnsupportedForm in v1.
"""
function emit_try(ctx::EmitCtx, ex, needs_value::Bool)::Union{Operand,Nothing}
    if kind(ex) == K"trycatchelse"
        numchildren(ex) == 2 ||
            throw(UnsupportedForm("trycatchelse", "try/catch with an else block"))
        s = stmt!(ctx, UnifiedIR.K"try"; src = ex)
        open_arm!(ctx, s)
        v = compile(ctx, ex[1], needs_value)
        if alive(ctx)
            needs_value ? stmt!(ctx, UnifiedIR.K"result", v) : stmt!(ctx, UnifiedIR.K"result")
        end
        close_arm!(ctx)
        open_arm!(ctx, s; kind = REGION_HANDLER)
        exc = append_stmt!(ctx.b, UnifiedIR.K"region_arg"; type = Any)
        record_source!(ctx, exc, ex)
        push!(ctx.handler_exc, exc)
        try
            cv = compile(ctx, ex[2], needs_value)
            if alive(ctx)
                needs_value ? stmt!(ctx, UnifiedIR.K"result", cv) :
                              stmt!(ctx, UnifiedIR.K"result")
            end
        finally
            pop!(ctx.handler_exc)
        end
        close_arm!(ctx)
        return needs_value ? op_stmt(s) : nothing_op(ctx)
    else # tryfinally
        numchildren(ex) == 2 ||
            throw(UnsupportedForm("tryfinally", "tryfinally with a dynamic-scope operand (@with)"))
        has_escaping_exit(ex[1]) &&
            throw(UnsupportedForm("tryfinally",
                "return/break crossing a finally block (tag-based lowering not ported in v1)"))
        s = stmt!(ctx, UnifiedIR.K"try"; src = ex)
        open_arm!(ctx, s)
        v = compile(ctx, ex[1], needs_value)
        if alive(ctx)
            needs_value ? stmt!(ctx, UnifiedIR.K"result", v) : stmt!(ctx, UnifiedIR.K"result")
        end
        close_arm!(ctx)
        open_arm!(ctx, s; kind = REGION_HANDLER)
        record_source!(ctx, append_stmt!(ctx.b, UnifiedIR.K"region_arg"; type = Any), ex)  # %exc (unused)
        emit_effect(ctx, ex[2])                                    # finally, exceptional path
        if alive(ctx)
            stmt!(ctx, UnifiedIR.K"call", GlobalRef(Base, :rethrow); src = ex)
            stmt!(ctx, UnifiedIR.K"unreachable")
        end
        close_arm!(ctx)
        emit_effect(ctx, ex[2]) === nothing && return nothing     # finally, normal path
        return needs_value ? op_stmt(s) : nothing_op(ctx)
    end
end

"Does this subtree contain an exit (return / outward break / @goto) that would cross it?"
function has_escaping_exit(ex, locallabels::Set{String} = Set{String}())
    k = kind(ex)
    if k == K"return" || k == K"symbolicgoto" || k == K"oldsymbolicgoto"
        return true
    elseif k == K"break"
        return !(ex[1].name_val::String in locallabels)
    elseif k == K"symbolicblock"
        inner = union(locallabels, Set([ex[1].name_val::String]))
        return has_escaping_exit(ex[2], inner)
    elseif k == K"lambda" || k == K"inert" || k == K"inert_syntaxtree" || k == K"quote"
        return false
    elseif is_leaf(ex)
        return false
    else
        for c in children(ex)
            has_escaping_exit(c, locallabels) && return true
        end
        return false
    end
end

# ---------------------------------------------------------------------------
# isdefined
# ---------------------------------------------------------------------------

function emit_isdefined(ctx::EmitCtx, ex)::Union{Operand,Nothing}
    c = ex[1]
    if kind(c) == K"BindingId"
        id = Int(c.var_id)
        binfo = ctx.bindings.info[id]
        if binfo.kind === :global
            gr = GlobalRef(binfo.mod::Module, Symbol(binfo.name))
            return op_stmt(stmt!(ctx, UnifiedIR.K"isdefined_global", vop(ctx.b.ir, gr);
                                 type = Bool, src = ex))
        end
        p = get(ctx.plans, id, nothing)
        if p !== nothing && p.mode === :cell
            return op_stmt(stmt!(ctx, UnifiedIR.K"cell_isdefined", ctx.cellmap[id];
                                 type = Bool, src = ex))
        elseif p !== nothing && p.mode === :foreign
            # foreign definedness is unknown: an opaque Bool keeps both arms
            return op_stmt(stmt!(ctx, UnifiedIR.K"call", vop(ctx.b.ir, OPAQUE);
                                 type = Bool, src = ex))
        end
        return vop(ctx.b.ir, true)   # args and dominating SSA defs are always defined
    elseif kind(c) == K"globalref"
        gr = GlobalRef(c.mod::Module, Symbol(c.name_val::String))
        return op_stmt(stmt!(ctx, UnifiedIR.K"isdefined_global", vop(ctx.b.ir, gr);
                             type = Bool, src = ex))
    else
        unsupported(ex, "isdefined of $(kind(c))")
    end
end
