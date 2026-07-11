# Exit converter, UnifiedIR → lowered (slot-form, goto-form) CodeInfo
# (§10.5): flattens regions to gotos, lowers cells to slots, synthesizes
# result slots for region-op values. Output is *uninferred* lowered code the
# stock runtime pipeline consumes — the runtime-interaction boundary.
#
# v1 feature matrix: if / loop / cfg / cells / all plain kinds. `try` regions
# are synthesized to :enter/:leave form. Escaping tuple results are
# materialized via Core.tuple (§10.5).

const CI_FIELDS = fieldnames(Core.CodeInfo)

# fieldnames-driven CodeInfo constructor (JuliaLowering's trick)
function make_codeinfo(; kws...)
    d = Dict{Symbol,Any}(kws)
    args = Any[]
    for (f, t) in zip(fieldnames(Core.CodeInfo), fieldtypes(Core.CodeInfo))
        haskey(d, f) || error("make_codeinfo: missing field $f (CodeInfo layout changed?)")
        push!(args, convert(t, d[f]))
    end
    return ccall(:jl_new_structv, Any, (Any, Ptr{Any}, UInt32),
                 Core.CodeInfo, args, length(args))::Core.CodeInfo
end

function default_codeinfo_fields(nstmts::Int, nargs::Int, slotnames, slotflags)
    Dict{Symbol,Any}(
        :code => Any[],
        :debuginfo => Core.DebugInfo(:none),
        :ssavaluetypes => nstmts,
        :ssaflags => zeros(UInt32, nstmts),
        :slotnames => slotnames,
        :slotflags => slotflags,
        :slottypes => nothing,
        :rettype => Any,
        :parent => nothing,
        :edges => nothing,
        :min_world => Csize_t(1),
        :max_world => typemax(Csize_t),
        :method_for_inference_limit_heuristics => nothing,
        :nargs => nargs,
        :propagate_inbounds => false,
        :has_fcall => false,
        :has_image_globalref => false,
        :nospecializeinfer => false,
        :isva => false,
        :inlining => 0x00,
        :constprop => 0x00,
        :purity => 0x0000,
        :inlining_cost => 0xffff,
    )
end

mutable struct ExitCtx
    ir::UnifiedIR.IR
    code::Vector{Any}
    ssaof::Dict{Int32,Any}        # UIR stmt id -> SSAValue | literal | SlotNumber
    slotof::Dict{Int32,Int}       # cell / region-arg / result stmt id -> slot number
    slotnames::Vector{Symbol}
    nargs::Int
    pending_gotos::Vector{Tuple{Int,Symbol,Any}}  # (code idx, kind, target key)
    labels::Dict{Any,Int}         # label key -> code idx (1-based stmt index)
    trystack::Vector{Core.SSAValue}   # active :enter tokens, innermost last
    excstack::Vector{Core.SSAValue}   # active catch scopes (exception stack tokens)
    block_depth::Dict{Int32,Tuple{Int,Int}}  # block region -> (trydepth, excdepth)
end

emitstmt!(cx::ExitCtx, @nospecialize(st)) = (push!(cx.code, st); Core.SSAValue(length(cx.code)))

function newslot!(cx::ExitCtx, name::Symbol = :tmp)
    push!(cx.slotnames, name)
    return length(cx.slotnames)
end

marklabel!(cx::ExitCtx, key) = (cx.labels[key] = length(cx.code) + 1)

function emitgoto!(cx::ExitCtx, key)
    push!(cx.code, Core.GotoNode(0))
    push!(cx.pending_gotos, (length(cx.code), :goto, key))
end
function emitgotoifnot!(cx::ExitCtx, cond, key)
    push!(cx.code, Core.GotoIfNot(cond, 0))
    push!(cx.pending_gotos, (length(cx.code), :gotoifnot, key))
end

function fixgotos!(cx::ExitCtx)
    for (idx, k, key) in cx.pending_gotos
        tgt = get(cx.labels, key, 0)
        tgt == 0 && error("exit converter: unresolved label $key")
        if k === :goto
            cx.code[idx] = Core.GotoNode(tgt)
        elseif k === :enter
            cx.code[idx] = Core.EnterNode(tgt)
        else
            g = cx.code[idx]::Core.GotoIfNot
            cx.code[idx] = Core.GotoIfNot(g.cond, tgt)
        end
    end
    empty!(cx.pending_gotos)
end

# Emit :leave for every active try scope deeper than `depth` (exits run the
# structural leave actions of every try they cross, §5.9).
function emit_leaves!(cx::ExitCtx, depth::Int)
    for i in length(cx.trystack):-1:(depth + 1)
        emitstmt!(cx, Expr(:leave, cx.trystack[i]))
    end
end

# Emit :pop_exception for every catch scope deeper than `depth` (exits
# leaving a handler restore the exception stack).
function emit_pops!(cx::ExitCtx, depth::Int)
    for i in length(cx.excstack):-1:(depth + 1)
        emitstmt!(cx, Expr(:pop_exception, cx.excstack[i]))
    end
end

"Operand → CodeInfo value (SSAValue / SlotNumber / literal / GlobalRef)."
function exit_value(cx::ExitCtx, o::UnifiedIR.Operand)
    ir = cx.ir
    t = UnifiedIR.optag(o)
    if t == UnifiedIR.TAG_STMT
        id = UnifiedIR.asstmt(o).id
        haskey(cx.ssaof, id) && return cx.ssaof[id]
        haskey(cx.slotof, id) && return Core.SlotNumber(cx.slotof[id])
        error("exit converter: use of unemitted %$id")
    elseif t == UnifiedIR.TAG_INLINE
        return UnifiedIR.imm_value(o)
    elseif t == UnifiedIR.TAG_CONST
        v = ir.body.constants[UnifiedIR.payload(o)]
        return v isa Union{Symbol,Expr} ? QuoteNode(v) : v
    elseif t == UnifiedIR.TAG_GLOBAL
        return ir.body.globals[UnifiedIR.payload(o)]
    elseif t == UnifiedIR.TAG_SPARAM
        return Expr(:static_parameter, Int(UnifiedIR.payload(o)))
    else
        error("exit converter: bad operand tag")
    end
end

exit_values(cx, s, from) =
    Any[exit_value(cx, UnifiedIR.getop(cx.ir, s, i)) for i in from:UnifiedIR.nops(cx.ir, s)]

"""
    ir_to_codeinfo(ir; name=:f) -> Core.CodeInfo

Lower a dense, sealed UnifiedIR body to slot-form goto-form CodeInfo.
"""
ir_to_codeinfo(ir::UnifiedIR.IR; name::Symbol = :f) = ir_to_codeinfo_ctx(ir; name)[1]

"As `ir_to_codeinfo`, but also returns the emission context (stmt→SSA map
`ssaof`, cell→slot map `slotof`, structure labels `(:join/:head/:brk, id)`
→ pc) — the completeness harness's bridge from region joins to flattened
basic blocks (completeness.jl)."
function ir_to_codeinfo_ctx(ir::UnifiedIR.IR; name::Symbol = :f)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_DENSE, "ir_to_codeinfo")
    root = UnifiedIR.getregion(ir, UnifiedIR.root_region(ir))
    nargs = length(root.args)
    cx = ExitCtx(ir, Any[], Dict{Int32,Any}(), Dict{Int32,Int}(), Symbol[],
                 nargs, Tuple{Int,Symbol,Any}[], Dict{Any,Int}(), Core.SSAValue[],
                 Core.SSAValue[], Dict{Int32,Tuple{Int,Int}}())
    slotnames_meta = get(ir.meta, :slotnames, nothing)
    for (i, a) in enumerate(root.args)
        nm = slotnames_meta !== nothing && i <= length(slotnames_meta) ?
            Symbol(slotnames_meta[i]) : (i == 1 ? Symbol("#self#") : Symbol("arg", i - 1))
        push!(cx.slotnames, nm)
        cx.slotof[a.id] = i
    end
    emit_region!(cx, UnifiedIR.root_region(ir), nothing)
    fixgotos!(cx)
    # slotflags: mark everything used (0x08 = SLOT_USED)
    slotflags = fill(0x08, length(cx.slotnames))
    fields = default_codeinfo_fields(length(cx.code), nargs, cx.slotnames, slotflags)
    fields[:code] = cx.code
    fields[:ssaflags] = zeros(UInt32, length(cx.code))
    fields[:ssavaluetypes] = length(cx.code)
    fields[:has_fcall] = any(st -> Meta.isexpr(st, :foreigncall), cx.code)
    return make_codeinfo(; fields...), cx
end

# Emit one region's direct statements. `loopctx` = (bodyregion, headkey,
# breakkey, resultslots, carriedslots) of the innermost enclosing loop chain,
# as a Dict region=>ctx for multi-level exits.
function emit_region!(cx::ExitCtx, r::RegionId, loopctxs)
    ir = cx.ir
    loopctxs = loopctxs === nothing ? Dict{Int32,Any}() : loopctxs
    for s in UnifiedIR.region_stmts(ir, r)
        k = UnifiedIR.stmt_kind(ir, s)
        k === K"region_arg" && continue
        emit_stmt!(cx, s, k, loopctxs)
    end
end

function bind_result_slots!(cx::ExitCtx, s::StmtId, vals::Vector{Any}, slots::Vector{Int})
    for (i, sl) in enumerate(slots)
        emitstmt!(cx, Expr(:(=), Core.SlotNumber(sl), vals[i]))
    end
end

function emit_stmt!(cx::ExitCtx, s::StmtId, k::UnifiedIR.Kind, loopctxs)
    ir = cx.ir
    if k === K"if"
        cond = exit_value(cx, UnifiedIR.getop(ir, s, 1))
        rs = UnifiedIR.live_owned_regions(ir, s)
        # result slot(s): one slot; tuple results materialize a tuple value
        rslot = newslot!(cx, :ifres)
        cx.slotof[s.id] = rslot
        elsekey = (:else, s.id)
        joinkey = (:join, s.id)
        emitgotoifnot!(cx, cond, length(rs) >= 2 ? elsekey : joinkey)
        emit_region_with_result!(cx, rs[1], rslot, joinkey, loopctxs)
        if length(rs) >= 2
            marklabel!(cx, elsekey)
            emit_region_with_result!(cx, rs[2], rslot, joinkey, loopctxs)
        end
        marklabel!(cx, joinkey)
    elseif k === K"loop"
        rs = UnifiedIR.live_owned_regions(ir, s)
        bodyr = rs[1]
        breg = UnifiedIR.getregion(ir, bodyr)
        # carried args -> slots, initialized from the loop operands
        carried = Int[]
        for (i, a) in enumerate(breg.args)
            sl = newslot!(cx, Symbol(:carried, i))
            cx.slotof[a.id] = sl
            push!(carried, sl)
            emitstmt!(cx, Expr(:(=), Core.SlotNumber(sl),
                               exit_value(cx, UnifiedIR.getop(ir, s, i))))
        end
        rslot = newslot!(cx, :loopres)
        cx.slotof[s.id] = rslot
        headkey = (:head, s.id)
        breakkey = (:brk, s.id)
        marklabel!(cx, headkey)
        newctxs = copy(loopctxs)
        newctxs[bodyr.id] = (headkey, breakkey, rslot, carried, length(cx.trystack))
        emit_region!(cx, bodyr, newctxs)
        marklabel!(cx, breakkey)
    elseif k === K"continue"
        tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, s, 1))
        (headkey, breakkey, rslot, carried, trydepth) = loopctxs[tgt.id]
        emit_leaves!(cx, trydepth)
        cond = exit_value(cx, UnifiedIR.getop(ir, s, 2))
        vals = exit_values(cx, s, 3)
        # both paths need the new carried values bound
        for (i, sl) in enumerate(carried)
            emitstmt!(cx, Expr(:(=), Core.SlotNumber(sl), vals[i]))
        end
        contkey = (:cont, s.id)
        emitgotoifnot!(cx, cond, contkey)
        emitgoto!(cx, headkey)
        marklabel!(cx, contkey)
        bind_loop_result!(cx, rslot, Any[Core.SlotNumber(sl) for sl in carried])
        emitgoto!(cx, breakkey)
    elseif k === K"break"
        tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, s, 1))
        (headkey, breakkey, rslot, carried, trydepth) = loopctxs[tgt.id]
        emit_leaves!(cx, trydepth)
        vals = exit_values(cx, s, 2)
        bind_loop_result!(cx, rslot, vals)
        emitgoto!(cx, breakkey)
    elseif k === K"return"
        vals = exit_values(cx, s, 1)
        emit_pops!(cx, 0)
        emit_leaves!(cx, 0)
        emitstmt!(cx, Core.ReturnNode(isempty(vals) ? nothing : vals[1]))
    elseif k === K"unreachable"
        emitstmt!(cx, Core.ReturnNode())
    elseif k === K"result"
        error("exit converter: result terminator outside an owned region context")
    elseif k === K"try"
        emit_try!(cx, s, loopctxs)
    elseif k === K"cfg"
        emit_cfg!(cx, s, loopctxs)
    elseif k === K"cell" || k === K"cell_shared"
        sl = newslot!(cx, :cellslot)
        cx.slotof[s.id] = sl
        # fresh cells are undefined; a NewvarNode marks that for the runtime
        emitstmt!(cx, Core.NewvarNode(Core.SlotNumber(sl)))
    elseif k === K"cell_set"
        cellid = UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1)).id
        sl = cx.slotof[cellid]
        emitstmt!(cx, Expr(:(=), Core.SlotNumber(sl),
                           exit_value(cx, UnifiedIR.getop(ir, s, 2))))
    elseif k === K"cell_get"
        cellid = UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1)).id
        cx.ssaof[s.id] = emitstmt!(cx, Core.SlotNumber(cx.slotof[cellid]))
    elseif k === K"cell_new"
        cellid = UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1)).id
        emitstmt!(cx, Core.NewvarNode(Core.SlotNumber(cx.slotof[cellid])))
    elseif k === K"cell_isdefined"
        cellid = UnifiedIR.asstmt(UnifiedIR.getop(ir, s, 1)).id
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:isdefined, Core.SlotNumber(cx.slotof[cellid])))
    elseif k === K"throw_undef_if_not"
        nm = exit_value(cx, UnifiedIR.getop(ir, s, 2))
        nm isa QuoteNode && (nm = nm.value)
        emitstmt!(cx, Expr(:throw_undef_if_not, nm,
                           exit_value(cx, UnifiedIR.getop(ir, s, 1))))
    elseif k === K"extract"
        v = exit_value(cx, UnifiedIR.getop(ir, s, 1))
        idx = UnifiedIR.imm_value(UnifiedIR.getop(ir, s, 2))::Int64
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:call, GlobalRef(Core, :getfield), v, Int(idx) + 1))
    elseif k === K"call"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:call, exit_values(cx, s, 1)...))
    elseif k === K"invoke"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:invoke, exit_values(cx, s, 1)...))
    elseif k === K"new"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:new, exit_values(cx, s, 1)...))
    elseif k === K"splatnew"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:splatnew, exit_values(cx, s, 1)...))
    elseif k === K"foreigncall"
        vals = exit_values(cx, s, 1)
        v1 = isempty(vals) ? nothing : vals[1]
        v1 isa QuoteNode && (v1 = v1.value)
        if v1 === FOREIGNGLOBAL_MARKER
            # marker-encoded Expr(:foreignglobal, name) — see codeinfo_entry
            cx.ssaof[s.id] = emitstmt!(cx, Expr(:foreignglobal, vals[2:end]...))
        else
            cx.ssaof[s.id] = emitstmt!(cx, Expr(:foreigncall, vals...))
        end
    elseif k === K"cfunction"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:cfunction, exit_values(cx, s, 1)...))
    elseif k === K"globalref"
        cx.ssaof[s.id] = emitstmt!(cx, exit_value(cx, UnifiedIR.getop(ir, s, 1)))
    elseif k === K"isdefined_global"
        g = exit_value(cx, UnifiedIR.getop(ir, s, 1))
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:isdefined, g))
    elseif k === K"value"
        cx.ssaof[s.id] = exit_value(cx, UnifiedIR.getop(ir, s, 1))
    elseif k === K"refine"
        cx.ssaof[s.id] = exit_value(cx, UnifiedIR.getop(ir, s, 1))
    elseif k === K"select"
        c = exit_value(cx, UnifiedIR.getop(ir, s, 1))
        a = exit_value(cx, UnifiedIR.getop(ir, s, 2))
        b = exit_value(cx, UnifiedIR.getop(ir, s, 3))
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:call, GlobalRef(Core, :ifelse), c, a, b))
    elseif k === K"boundscheck"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:boundscheck))
    elseif k === K"gc_preserve_begin"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:gc_preserve_begin, exit_values(cx, s, 1)...))
    elseif k === K"gc_preserve_end"
        emitstmt!(cx, Expr(:gc_preserve_end, exit_value(cx, UnifiedIR.getop(ir, s, 1))))
    elseif k === K"latestworld"
        emitstmt!(cx, Expr(:latestworld))
    elseif k === K"coverage_effect"
        emitstmt!(cx, Expr(:code_coverage_effect))
    elseif k === K"copyast"
        cx.ssaof[s.id] = emitstmt!(cx, Expr(:copyast, exit_value(cx, UnifiedIR.getop(ir, s, 1))))
    else
        # external-dialect kinds hit the §8.2 legalization contract
        error("exit converter: kind $(UnifiedIR.kindname(k)) has no registered CodeInfo lowering (legalization contract, §8.2)")
    end
    return nothing
end

# loop results: single carried value -> the value; multiple -> Core.tuple
function bind_loop_result!(cx::ExitCtx, rslot::Int, vals::Vector{Any})
    v = length(vals) == 1 ? vals[1] :
        isempty(vals) ? nothing :
        emitstmt!(cx, Expr(:call, GlobalRef(Core, :tuple), vals...))
    emitstmt!(cx, Expr(:(=), Core.SlotNumber(rslot), v))
end

# Emit an owned region whose result terminators feed `rslot`, then jump to `joinkey`.
function emit_region_with_result!(cx::ExitCtx, r::RegionId, rslot::Int, joinkey, loopctxs)
    ir = cx.ir
    for s in UnifiedIR.region_stmts(ir, r)
        k = UnifiedIR.stmt_kind(ir, s)
        k === K"region_arg" && continue
        if k === K"result"
            vals = exit_values(cx, s, 1)
            v = length(vals) == 1 ? vals[1] :
                isempty(vals) ? nothing :
                emitstmt!(cx, Expr(:call, GlobalRef(Core, :tuple), vals...))
            emitstmt!(cx, Expr(:(=), Core.SlotNumber(rslot), v))
            emitgoto!(cx, joinkey)
        else
            emit_stmt!(cx, s, k, loopctxs)
        end
    end
end

# try/catch: :enter / :leave / :pop_exception synthesis (§10.5)
function emit_try!(cx::ExitCtx, s::StmtId, loopctxs)
    ir = cx.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    rslot = newslot!(cx, :tryres)
    cx.slotof[s.id] = rslot
    catchkey = (:catch, s.id)
    joinkey = (:jointry, s.id)
    enteridx = length(cx.code) + 1
    push!(cx.code, Core.EnterNode(0))
    push!(cx.pending_gotos, (enteridx, :enter, catchkey))
    push!(cx.trystack, Core.SSAValue(enteridx))
    # body: result terminators leave the handler scope then store + jump
    for st in UnifiedIR.region_stmts(ir, rs[1])
        k = UnifiedIR.stmt_kind(ir, st)
        k === K"region_arg" && continue
        if k === K"result"
            vals = exit_values(cx, st, 1)
            v = length(vals) == 1 ? vals[1] :
                isempty(vals) ? nothing :
                emitstmt!(cx, Expr(:call, GlobalRef(Core, :tuple), vals...))
            emitstmt!(cx, Expr(:(=), Core.SlotNumber(rslot), v))
            emitstmt!(cx, Expr(:leave, Core.SSAValue(enteridx)))
            emitgoto!(cx, joinkey)
        else
            emit_stmt!(cx, st, k, loopctxs)
        end
    end
    pop!(cx.trystack)
    if length(rs) >= 2
        marklabel!(cx, catchkey)
        push!(cx.excstack, Core.SSAValue(enteridx))
        h = UnifiedIR.getregion(ir, rs[2])
        if !isempty(h.args)
            exc = emitstmt!(cx, Expr(:the_exception))
            sl = newslot!(cx, :exc)
            cx.slotof[h.args[1].id] = sl
            emitstmt!(cx, Expr(:(=), Core.SlotNumber(sl), exc))
        end
        for st in UnifiedIR.region_stmts(ir, rs[2])
            k = UnifiedIR.stmt_kind(ir, st)
            k === K"region_arg" && continue
            if k === K"result"
                vals = exit_values(cx, st, 1)
                v = length(vals) == 1 ? vals[1] :
                    isempty(vals) ? nothing :
                    emitstmt!(cx, Expr(:call, GlobalRef(Core, :tuple), vals...))
                emitstmt!(cx, Expr(:(=), Core.SlotNumber(rslot), v))
                emitstmt!(cx, Expr(:pop_exception, Core.SSAValue(enteridx)))
                emitgoto!(cx, joinkey)
            else
                emit_stmt!(cx, st, k, loopctxs)
            end
        end
        pop!(cx.excstack)
    else
        marklabel!(cx, catchkey)
        # no handler: rethrow after running nothing (shouldn't occur from our frontends)
        emitstmt!(cx, Expr(:call, GlobalRef(Base, :rethrow)))
        emitstmt!(cx, Core.ReturnNode())
    end
    marklabel!(cx, joinkey)
end

# cfg islands: blocks in order; block args become slots assigned on each edge
function emit_cfg!(cx::ExitCtx, s::StmtId, loopctxs)
    ir = cx.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    rslot = newslot!(cx, :cfgres)
    cx.slotof[s.id] = rslot
    joinkey = (:joincfg, s.id)
    # block args -> slots; record scope depths (uniform across one island)
    for rid in rs
        cx.block_depth[rid.id] = (length(cx.trystack), length(cx.excstack))
        blk = UnifiedIR.getregion(ir, rid)
        for a in blk.args
            cx.slotof[a.id] = newslot!(cx, :blkarg)
        end
    end
    # entry: cfg operands feed the first block's args
    entry = UnifiedIR.getregion(ir, rs[1])
    for (i, a) in enumerate(entry.args)
        emitstmt!(cx, Expr(:(=), Core.SlotNumber(cx.slotof[a.id]),
                           exit_value(cx, UnifiedIR.getop(ir, s, i))))
    end
    for rid in rs
        marklabel!(cx, (:blk, rid.id))
        for st in UnifiedIR.region_stmts(ir, rid)
            k = UnifiedIR.stmt_kind(ir, st)
            k === K"region_arg" && continue
            if k === K"goto" || k === K"br_if" || k === K"switch"
                emit_cfg_terminator!(cx, st, k)
            elseif k === K"result"
                vals = exit_values(cx, st, 1)
                v = length(vals) == 1 ? vals[1] :
                    isempty(vals) ? nothing :
                    emitstmt!(cx, Expr(:call, GlobalRef(Core, :tuple), vals...))
                emitstmt!(cx, Expr(:(=), Core.SlotNumber(rslot), v))
                emitgoto!(cx, joinkey)
            elseif k === K"await"
                error("exit converter: await lowering requires runtime support (julia#58532; not in the v1 matrix)")
            else
                emit_stmt!(cx, st, k, loopctxs)
            end
        end
    end
    marklabel!(cx, joinkey)
end

function emit_edge_transfer!(cx::ExitCtx, dest::RegionId, args::Vector{UnifiedIR.Operand})
    blk = UnifiedIR.getregion(cx.ir, dest)
    for (i, a) in enumerate(blk.args)
        emitstmt!(cx, Expr(:(=), Core.SlotNumber(cx.slotof[a.id]), exit_value(cx, args[i])))
    end
    # a goto to an ancestor island's block leaves the try/catch scopes in
    # between: synthesize their pop_exception/:leave actions (§5.9)
    d = get(cx.block_depth, dest.id, nothing)
    if d !== nothing
        emit_pops!(cx, d[2])
        emit_leaves!(cx, d[1])
    end
    emitgoto!(cx, (:blk, dest.id))
end

function emit_cfg_terminator!(cx::ExitCtx, s::StmtId, k::UnifiedIR.Kind)
    ir = cx.ir
    bs = UnifiedIR.edge_bundles(ir, s)
    if k === K"goto"
        emit_edge_transfer!(cx, bs[1][1], bs[1][2])
    elseif k === K"br_if"
        cond = exit_value(cx, UnifiedIR.getop(ir, s, 1))
        elsekey = (:bre, s.id)
        emitgotoifnot!(cx, cond, elsekey)
        emit_edge_transfer!(cx, bs[1][1], bs[1][2])
        marklabel!(cx, elsekey)
        emit_edge_transfer!(cx, bs[2][1], bs[2][2])
    elseif k === K"switch"
        v = exit_value(cx, UnifiedIR.getop(ir, s, 1))
        ncases = Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, s, 2))::Int64)
        opidx = 3
        for c in 1:ncases
            caseval = exit_value(cx, UnifiedIR.getop(ir, s, opidx))
            iseq = emitstmt!(cx, Expr(:call, GlobalRef(Core, :(===)), v, caseval))
            nextkey = (:swnext, s.id, c)
            emitgotoifnot!(cx, iseq, nextkey)
            emit_edge_transfer!(cx, bs[c][1], bs[c][2])
            marklabel!(cx, nextkey)
            opidx += 1 + 2 + length(bs[c][2])
        end
        emit_edge_transfer!(cx, bs[end][1], bs[end][2])
    end
end
