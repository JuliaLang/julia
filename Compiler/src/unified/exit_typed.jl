# Typed exit converter (§10.5): dense, typed, optimized UnifiedIR → IRCode,
# synthesizing header/merge PhiNodes from region args and results. This is
# the boundary that lets the UnifiedIR optimizer's output feed the stock
# backend (validated by the stock IR verifier; executed via OpaqueClosure).
#
# v1 feature matrix (§6 staged strategy): no residual cells (promote first —
# the error lists survivors) and no `try` regions (exceptional values keep
# memory form by design, so try-bearing bodies stay on the CodeInfo exit
# path). `if`/`loop`/`cfg` islands and all plain kinds are covered; tuple
# result values de-tuple for extract-only uses and materialize otherwise.
#
# Layout discipline: blocks are objects, placed explicitly in final order;
# `GotoIfNot` fallthrough adjacency is guaranteed either by placing the
# then-block next or by a trampoline block.

mutable struct TBB
    order::Int               # final position; 0 = not yet placed
    phis::Vector{Any}        # PhiSpec, in emission order
    items::Vector{StmtId}
    tuplemat::Vector{Any}    # SynthTuple, appended before the terminator
    term::Any                # (:goto, TBB) | (:brifnot, cond, false::TBB, then::TBB)
                             # | (:return, val) | (:unreachable,) | nothing
end
TBB() = TBB(0, Any[], StmtId[], Any[], nothing)

mutable struct PhiSpec
    uirid::Int32
    typ::Any
    edges::Vector{Tuple{TBB,Any}}   # (pred block, value)
    ssaidx::Int
end
PhiSpec(uirid, typ) = PhiSpec(uirid, typ, Tuple{TBB,Any}[], 0)

mutable struct SynthTuple
    vals::Vector{Any}
    ssaidx::Int
end

mutable struct TCtx
    ir::UnifiedIR.IR
    placed::Vector{TBB}
    cur::TBB
    loopctx::Dict{Int32,Any}
    phi_of::Dict{Int32,PhiSpec}
end

"Create a block and place it at the end of the current layout."
function placebb!(cx::TCtx)
    bb = TBB()
    bb.order = length(cx.placed) + 1
    push!(cx.placed, bb)
    return bb
end
"Place a previously created (deferred) block now."
function place!(cx::TCtx, bb::TBB)
    @assert bb.order == 0
    bb.order = length(cx.placed) + 1
    push!(cx.placed, bb)
    return bb
end

setterm!(bb::TBB, t) = (bb.term === nothing && (bb.term = t); bb)

struct JoinCtx
    joinbb::TBB
    phis::Vector{PhiSpec}
    materialize::Bool
end

"""
    ir_to_ircode(ir) -> Compiler.IRCode

Convert dense, typed, cell-free UnifiedIR to stock IRCode with synthesized
phis. Throws `UnsupportedIR` outside the v1 matrix.
"""
function ir_to_ircode(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_DENSE, "ir_to_ircode")
    residual = String[]
    for s in UnifiedIR.each_stmt(ir)
        k = UnifiedIR.stmt_kind(ir, s)
        if k === K"cell" || k === K"cell_shared" || k === K"cell_get" ||
           k === K"cell_set" || k === K"cell_new" || k === K"cell_isdefined"
            push!(residual, string("%", s.id, " ", UnifiedIR.kindname(k)))
        elseif k === K"try"
            throw(UnsupportedIR("try regions keep the CodeInfo exit path (§6 staged strategy)"))
        elseif k === K"await" || k === K"closure"
            throw(UnsupportedIR("$(UnifiedIR.kindname(k)) in typed exit"))
        end
    end
    isempty(residual) ||
        throw(UnsupportedIR("residual cells in typed exit (promote first): " * join(residual, ", ")))

    root = UnifiedIR.getregion(ir, UnifiedIR.root_region(ir))
    argmap = Dict{Int32,Int}(a.id => i for (i, a) in enumerate(root.args))

    cx = TCtx(ir, TBB[], TBB(), Dict{Int32,Any}(), Dict{Int32,PhiSpec}())
    cx.cur = placebb!(cx)
    emit_tregion!(cx, UnifiedIR.root_region(ir), nothing)
    for bb in cx.placed
        bb.term === nothing && (bb.term = (:unreachable,))
    end
    return assemble_ircode(cx, ir, argmap, length(root.args))
end

# ---- use analysis ----------------------------------------------------------

function extract_only_uses(ir::UnifiedIR.IR, s::StmtId)
    extracts = StmtId[]
    others = 0
    UnifiedIR.each_ssa_use(ir) do site, used
        used == s || return
        if site isa UnifiedIR.StmtOperand &&
           UnifiedIR.stmt_kind(ir, site.user) === K"extract" &&
           UnifiedIR.asstmt(UnifiedIR.getop(ir, site.user, 1)) == s
            push!(extracts, site.user)
        else
            others += 1
        end
    end
    return (extracts, others)
end

function result_used(ir::UnifiedIR.IR, s::StmtId)
    used = false
    UnifiedIR.each_ssa_use(ir) do _, u
        u == s && (used = true)
    end
    return used
end

"Max value arity of the exits feeding owner `s` (results, breaks, continues)."
function owner_nvals(ir::UnifiedIR.IR, s::StmtId)
    rs = UnifiedIR.live_owned_regions(ir, s)
    rset = Set{Int32}(r.id for r in rs)
    n = 0
    for st in UnifiedIR.each_stmt(ir)
        k = UnifiedIR.stmt_kind(ir, st)
        if k === K"result"
            reg = UnifiedIR.stmt_region(ir, st)
            # a result terminator feeds its own region's owner
            UnifiedIR.getregion(ir, reg).owner == s && (n = max(n, UnifiedIR.nops(ir, st)))
        elseif k === K"break"
            tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, st, 1))
            tgt.id in rset && (n = max(n, UnifiedIR.nops(ir, st) - 1))
        elseif k === K"continue"
            tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, st, 1))
            tgt.id in rset && (n = max(n, UnifiedIR.nops(ir, st) - 2))
        end
    end
    return n
end

function make_joinctx!(cx::TCtx, owner::StmtId, joinbb::TBB)
    ir = cx.ir
    nvals = owner_nvals(ir, owner)
    used = result_used(ir, owner)
    if nvals <= 1
        p = PhiSpec(owner.id, UnifiedIR.stmt_type(ir, owner))
        if used
            push!(joinbb.phis, p)
            cx.phi_of[owner.id] = p
        end
        return JoinCtx(joinbb, [p], false)
    end
    extracts, others = extract_only_uses(ir, owner)
    if others == 0
        phis = PhiSpec[PhiSpec(Int32(0), Any) for _ in 1:nvals]
        append!(joinbb.phis, phis)
        for ex in extracts
            idx = Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, ex, 2))::Int64)
            0 <= idx < nvals || throw(UnsupportedIR("extract index out of range in typed exit"))
            cx.phi_of[ex.id] = phis[idx + 1]
        end
        return JoinCtx(joinbb, phis, false)
    end
    p = PhiSpec(owner.id, UnifiedIR.stmt_type(ir, owner))
    push!(joinbb.phis, p)
    cx.phi_of[owner.id] = p
    return JoinCtx(joinbb, [p], true)
end

function feed_join!(cx::TCtx, j::JoinCtx, vals::Vector{Any})
    frombb = cx.cur
    if j.materialize
        st = SynthTuple(vals, 0)
        push!(frombb.tuplemat, st)
        push!(j.phis[1].edges, (frombb, st))
    else
        for (i, p) in enumerate(j.phis)
            push!(p.edges, (frombb, i <= length(vals) ? vals[i] : nothing))
        end
    end
    setterm!(frombb, (:goto, j.joinbb))
    return nothing
end

# ---- region emission -------------------------------------------------------

function emit_tregion!(cx::TCtx, r::RegionId, jctx::Union{Nothing,JoinCtx})
    ir = cx.ir
    for s in UnifiedIR.region_stmts(ir, r)
        k = UnifiedIR.stmt_kind(ir, s)
        if k === K"region_arg"
        elseif k === K"extract" && haskey(cx.phi_of, s.id)
            # de-tupled: the positional phi IS this value; no stmt emitted
        elseif k === K"if"
            emit_tif!(cx, s)
        elseif k === K"loop"
            emit_tloop!(cx, s)
        elseif k === K"cfg"
            emit_tcfg!(cx, s)
        elseif k === K"result"
            jctx === nothing && throw(UnsupportedIR("result terminator at root in typed exit"))
            feed_join!(cx, jctx, Any[UnifiedIR.getop(ir, s, i) for i in 1:UnifiedIR.nops(ir, s)])
        elseif k === K"return"
            setterm!(cx.cur, (:return, UnifiedIR.nops(ir, s) >= 1 ? UnifiedIR.getop(ir, s, 1) : nothing))
        elseif k === K"unreachable"
            setterm!(cx.cur, (:unreachable,))
        elseif k === K"break"
            tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, s, 1))
            (_, exitctx) = cx.loopctx[tgt.id]
            feed_join!(cx, exitctx, Any[UnifiedIR.getop(ir, s, i) for i in 2:UnifiedIR.nops(ir, s)])
        elseif k === K"continue"
            emit_tcontinue!(cx, s)
        elseif k === K"goto" || k === K"br_if" || k === K"switch" || k === K"await"
            throw(UnsupportedIR("island terminator outside a cfg block"))
        else
            push!(cx.cur.items, s)
        end
    end
    return nothing
end

function emit_tif!(cx::TCtx, s::StmtId)
    ir = cx.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    cond = UnifiedIR.getop(ir, s, 1)
    frombb = cx.cur
    joinbb = TBB()                      # deferred: placed after the arms
    j = make_joinctx!(cx, s, joinbb)
    thenbb = placebb!(cx)               # fallthrough-adjacent to frombb
    if length(rs) >= 2
        elsebb = TBB()                  # deferred: placed after the then arm
        frombb.term = (:brifnot, cond, elsebb, thenbb)
        cx.cur = thenbb
        emit_tregion!(cx, rs[1], j)
        cx.cur.term === nothing && feed_join!(cx, j, Any[])
        place!(cx, elsebb)
        cx.cur = elsebb
        emit_tregion!(cx, rs[2], j)
        cx.cur.term === nothing && feed_join!(cx, j, Any[])
    else
        frombb.term = (:brifnot, cond, joinbb, thenbb)
        if result_used(ir, s)
            for p in j.phis
                push!(p.edges, (frombb, nothing))
            end
        end
        cx.cur = thenbb
        emit_tregion!(cx, rs[1], j)
        cx.cur.term === nothing && feed_join!(cx, j, Any[])
    end
    place!(cx, joinbb)
    cx.cur = joinbb
    return nothing
end

function emit_tloop!(cx::TCtx, s::StmtId)
    ir = cx.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    bodyr = rs[1]
    breg = UnifiedIR.getregion(ir, bodyr)
    frombb = cx.cur
    header = placebb!(cx)
    setterm!(frombb, (:goto, header))
    for (i, a) in enumerate(breg.args)
        p = PhiSpec(a.id, UnifiedIR.stmt_type(ir, a))
        push!(p.edges, (frombb, UnifiedIR.getop(ir, s, i)))
        push!(header.phis, p)
        cx.phi_of[a.id] = p
    end
    exitbb = TBB()                      # deferred: placed after the body
    j = make_joinctx!(cx, s, exitbb)
    cx.loopctx[bodyr.id] = (header, j)
    cx.cur = header
    emit_tregion!(cx, bodyr, nothing)
    place!(cx, exitbb)
    cx.cur = exitbb
    return nothing
end

function emit_tcontinue!(cx::TCtx, s::StmtId)
    ir = cx.ir
    tgt = UnifiedIR.asregion(UnifiedIR.getop(ir, s, 1))
    (header, exitctx) = cx.loopctx[tgt.id]
    breg = UnifiedIR.getregion(ir, tgt)
    cond = UnifiedIR.getop(ir, s, 2)
    vals = Any[UnifiedIR.getop(ir, s, i) for i in 3:UnifiedIR.nops(ir, s)]
    frombb = cx.cur
    ctrue = UnifiedIR.optag(cond) == UnifiedIR.TAG_INLINE && UnifiedIR.imm_value(cond) === true
    if ctrue
        for (i, a) in enumerate(breg.args)
            push!(cx.phi_of[a.id].edges, (frombb, vals[i]))
        end
        setterm!(frombb, (:goto, header))
    else
        backbb = placebb!(cx)           # fallthrough-adjacent (the true edge)
        exitfeed = TBB()
        frombb.term = (:brifnot, cond, exitfeed, backbb)
        for (i, a) in enumerate(breg.args)
            push!(cx.phi_of[a.id].edges, (backbb, vals[i]))
        end
        setterm!(backbb, (:goto, header))
        place!(cx, exitfeed)
        cx.cur = exitfeed
        feed_join!(cx, exitctx, vals)   # continue-false: results = carried vals (§5.3)
    end
    return nothing
end

function emit_tcfg!(cx::TCtx, s::StmtId)
    ir = cx.ir
    rs = UnifiedIR.live_owned_regions(ir, s)
    frombb = cx.cur
    joinbb = TBB()
    j = make_joinctx!(cx, s, joinbb)
    # blocks as deferred objects; placed in region order as we walk
    bbof = Dict{Int32,TBB}(rid.id => TBB() for rid in rs)
    for rid in rs
        blk = UnifiedIR.getregion(ir, rid)
        for a in blk.args
            p = PhiSpec(a.id, UnifiedIR.stmt_type(ir, a))
            push!(bbof[rid.id].phis, p)
            cx.phi_of[a.id] = p
        end
    end
    entryblk = UnifiedIR.getregion(ir, rs[1])
    for (i, a) in enumerate(entryblk.args)
        push!(cx.phi_of[a.id].edges, (frombb, UnifiedIR.getop(ir, s, i)))
    end
    setterm!(frombb, (:goto, bbof[rs[1].id]))
    for rid in rs
        bb = bbof[rid.id]
        place!(cx, bb)
        cx.cur = bb
        for st in UnifiedIR.region_stmts(ir, rid)
            k = UnifiedIR.stmt_kind(ir, st)
            if k === K"region_arg"
            elseif k === K"extract" && haskey(cx.phi_of, st.id)
                # de-tupled: the positional phi IS this value; no stmt emitted
            elseif k === K"goto"
                (dest, args) = UnifiedIR.edge_bundles(ir, st)[1]
                haskey(bbof, dest.id) || throw(UnsupportedIR("cross-island goto in typed exit"))
                dblk = UnifiedIR.getregion(ir, dest)
                for (i, a) in enumerate(dblk.args)
                    push!(cx.phi_of[a.id].edges, (cx.cur, args[i]))
                end
                setterm!(cx.cur, (:goto, bbof[dest.id]))
            elseif k === K"br_if"
                bs = UnifiedIR.edge_bundles(ir, st)
                (haskey(bbof, bs[1][1].id) && haskey(bbof, bs[2][1].id)) ||
                    throw(UnsupportedIR("cross-island br_if in typed exit"))
                cond = UnifiedIR.getop(ir, st, 1)
                # trampoline for the true edge keeps fallthrough adjacency
                srcbb = cx.cur
                tramp = placebb!(cx)
                for (edge, dbb) in ((bs[1], tramp), (bs[2], srcbb))
                    dest, args = edge
                    dblk = UnifiedIR.getregion(ir, dest)
                    predbb = dbb === tramp ? tramp : srcbb
                    for (i, a) in enumerate(dblk.args)
                        push!(cx.phi_of[a.id].edges, (predbb, args[i]))
                    end
                end
                setterm!(tramp, (:goto, bbof[bs[1][1].id]))
                srcbb.term = (:brifnot, cond, bbof[bs[2][1].id], tramp)
                cx.cur = tramp   # walk continues on the next region anyway
            elseif k === K"result"
                feed_join!(cx, j, Any[UnifiedIR.getop(ir, st, i) for i in 1:UnifiedIR.nops(ir, st)])
            elseif k === K"return"
                setterm!(cx.cur, (:return, UnifiedIR.nops(ir, st) >= 1 ? UnifiedIR.getop(ir, st, 1) : nothing))
            elseif k === K"unreachable"
                setterm!(cx.cur, (:unreachable,))
            elseif k === K"switch" || k === K"await"
                throw(UnsupportedIR("$(UnifiedIR.kindname(k)) in typed exit (v1)"))
            elseif k === K"if" || k === K"loop" || k === K"cfg" || k === K"try"
                throw(UnsupportedIR("nested region op inside island block in typed exit (v1)"))
            else
                push!(cx.cur.items, st)
            end
        end
    end
    place!(cx, joinbb)
    cx.cur = joinbb
    return nothing
end

# ---- assembly --------------------------------------------------------------

function assemble_ircode(cx::TCtx, ir::UnifiedIR.IR, argmap::Dict{Int32,Int}, nargs::Int)
    bbs = cx.placed
    ssaof = Dict{Int32,Int}()
    nst = 0
    bbstart = Dict{TBB,Int}()
    for bb in bbs
        bbstart[bb] = nst + 1
        for p in bb.phis
            nst += 1
            p.ssaidx = nst
            p.uirid != 0 && (ssaof[p.uirid] = nst)
        end
        for s in bb.items
            nst += 1
            ssaof[s.id] = nst
        end
        for st in bb.tuplemat
            nst += 1
            st.ssaidx = nst
        end
        nst += 1
    end

    function tval(@nospecialize(o))
        o === nothing && return nothing
        if o isa StmtId
            haskey(argmap, o.id) && return Core.Argument(argmap[o.id])
            p = get(cx.phi_of, o.id, nothing)
            p !== nothing && return Core.SSAValue(p.ssaidx)
            haskey(ssaof, o.id) && return Core.SSAValue(ssaof[o.id])
            error("typed exit: unmapped value %$(o.id)")
        elseif o isa SynthTuple
            return Core.SSAValue(o.ssaidx)
        elseif o isa UnifiedIR.Operand
            t = UnifiedIR.optag(o)
            t == UnifiedIR.TAG_STMT && return tval(UnifiedIR.asstmt(o))
            t == UnifiedIR.TAG_INLINE && return UnifiedIR.imm_value(o)
            if t == UnifiedIR.TAG_CONST
                v = ir.body.constants[UnifiedIR.payload(o)]
                return v isa Union{Symbol,Expr} ? QuoteNode(v) : v
            end
            t == UnifiedIR.TAG_GLOBAL && return ir.body.globals[UnifiedIR.payload(o)]
            t == UnifiedIR.TAG_SPARAM && return Expr(:static_parameter, Int(UnifiedIR.payload(o)))
            error("typed exit: bad operand tag")
        else
            return o
        end
    end

    function tflags(f::UInt32)
        out = UInt32(0)
        (f & UnifiedIR.FLAG_CONSISTENT != 0) && (out |= CC.IR_FLAG_CONSISTENT)
        (f & UnifiedIR.FLAG_EFFECT_FREE != 0) && (out |= CC.IR_FLAG_EFFECT_FREE)
        (f & UnifiedIR.FLAG_NOTHROW != 0) && (out |= CC.IR_FLAG_NOTHROW)
        (f & UnifiedIR.FLAG_TERMINATES != 0) && (out |= CC.IR_FLAG_TERMINATES)
        return out
    end

    function translate_stmt(s::StmtId)
        k = UnifiedIR.stmt_kind(ir, s)
        n = UnifiedIR.nops(ir, s)
        ops = Any[tval(UnifiedIR.getop(ir, s, i)) for i in 1:n]
        k === K"call" && return Expr(:call, ops...)
        k === K"invoke" && return Expr(:invoke, ops...)
        k === K"new" && return Expr(:new, ops...)
        k === K"splatnew" && return Expr(:splatnew, ops...)
        k === K"foreigncall" && return Expr(:foreigncall, ops...)
        k === K"cfunction" && return Expr(:cfunction, ops...)
        if k === K"extract"
            return Expr(:call, GlobalRef(Core, :getfield), ops[1],
                        Int(UnifiedIR.imm_value(UnifiedIR.getop(ir, s, 2))) + 1)
        end
        if k === K"refine"
            t = UnifiedIR.stmt_type(ir, s)
            return Core.PiNode(ops[1], t isa Type ? t : CC.widenconst(t))
        end
        (k === K"value" || k === K"globalref") && return ops[1]
        k === K"select" && return Expr(:call, GlobalRef(Core, :ifelse), ops...)
        k === K"isdefined_global" && return Expr(:isdefined, ops[1])
        k === K"boundscheck" && return Expr(:boundscheck)
        k === K"gc_preserve_begin" && return Expr(:gc_preserve_begin, ops...)
        k === K"gc_preserve_end" && return Expr(:gc_preserve_end, ops...)
        k === K"latestworld" && return Expr(:latestworld)
        k === K"coverage_effect" && return Expr(:code_coverage_effect)
        k === K"copyast" && return Expr(:copyast, ops...)
        if k === K"throw_undef_if_not"
            nm = ops[2] isa QuoteNode ? ops[2].value : ops[2]
            return Expr(:throw_undef_if_not, nm, ops[1])
        end
        throw(UnsupportedIR("kind $(UnifiedIR.kindname(k)) in typed exit"))
    end

    stmts = Vector{Any}(undef, nst)
    types = Vector{Any}(undef, nst)
    flags = fill(UInt32(0), nst)
    lines = fill(Int32(0), 3nst)
    infos = CC.CallInfo[CC.NoCallInfo() for _ in 1:nst]

    pos = 0
    blocks = CC.BasicBlock[]
    for (bi, bb) in enumerate(bbs)
        start = pos + 1
        for p in bb.phis
            pos += 1
            edges = Int32[Int32(pred.order) for (pred, _) in p.edges]
            vals = Any[tval(v) for (_, v) in p.edges]
            stmts[pos] = Core.PhiNode(edges, vals)
            t = p.typ
            types[pos] = t === nothing ? Any : t
        end
        for s in bb.items
            pos += 1
            stmts[pos] = translate_stmt(s)
            t = UnifiedIR.stmt_type(ir, s)
            types[pos] = t === nothing ? Any : t
            flags[pos] = tflags(UnifiedIR.stmt_flag(ir, s))
        end
        for st in bb.tuplemat
            pos += 1
            stmts[pos] = Expr(:call, GlobalRef(Core, :tuple), Any[tval(v) for v in st.vals]...)
            types[pos] = Any
        end
        pos += 1
        term = bb.term
        succs = Int[]
        if term[1] === :goto
            stmts[pos] = Core.GotoNode(term[2].order)   # IRCode: block numbers
            types[pos] = Any
            push!(succs, term[2].order)
        elseif term[1] === :brifnot
            _, cond, falsebb, thenbb = term
            thenbb.order == bi + 1 ||
                throw(UnsupportedIR("typed exit: brifnot fallthrough not adjacent (layout bug)"))
            stmts[pos] = Core.GotoIfNot(tval(cond), falsebb.order)
            types[pos] = Any
            push!(succs, thenbb.order)
            push!(succs, falsebb.order)
        elseif term[1] === :return
            stmts[pos] = Core.ReturnNode(tval(term[2]))
            types[pos] = Any
        else
            stmts[pos] = Core.ReturnNode()
            types[pos] = Union{}
        end
        push!(blocks, CC.BasicBlock(CC.StmtRange(start, pos), Int[], succs))
    end
    for (bi, blk) in enumerate(blocks)
        for su in blk.succs
            push!(blocks[su].preds, bi)
        end
    end
    index = Int[blocks[i].stmts.start for i in 2:length(blocks)]
    cfg = CC.CFG(blocks, index)

    is = CC.InstructionStream(stmts, types, infos, lines, flags)
    di = CC.DebugInfoStream(lines)
    argtypes = Any[t for t in ir.argtypes]
    length(argtypes) == nargs || (argtypes = Any[Any for _ in 1:nargs])
    splat = get(ir.meta, :sptypes_lat, nothing)
    sptypes = CC.VarState[]
    for (i, sp) in enumerate(ir.sptypes)
        lat = splat !== nothing && i <= length(splat) ? splat[i] : CC.Const(sp)
        push!(sptypes, CC.VarState(lat, false))
    end
    return CC.IRCode(is, cfg, di, argtypes, Expr[], sptypes)
end
