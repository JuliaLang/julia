# Structurization mode of the entry converter (§10.5, P1): recover structured
# regions from reducible control flow inside `cfg` islands.
#
#   * `merge_linear_blocks!`  — move-based jump threading: a block whose sole
#     global in-edge is an unconditional goto is *moved* (not cloned, so
#     region-owning statements are fine) into its predecessor.
#   * `collapse_branches!`    — a `br_if` whose successors are single-
#     predecessor blocks becomes a structured `K"if"`:
#       - diamond: both sides rejoin at one block J → arms yield the edge
#         arguments, the branch becomes `goto ^J (ifresults…)`;
#       - triangle: one side jumps straight to J → one/two-armed if;
#       - exit arms: sides that terminate in yield/return/unreachable/
#         break/continue move wholesale into the arms (a yielding arm makes
#         the branch `yield %if`; all-diverging arms leave `unreachable`).
#   * `collapse_loops!`       — a natural loop (single back-edge target H
#     dominating its sources, single outside exit target) becomes a
#     `K"loop"` region; the loop's blocks move into a nested island whose
#     back-edges are `continue` and whose exit edges are `break`. The nested
#     island then reduces further via the rules above (`br_if` straight to
#     header/exit becomes a conditional `continue` directly).
#   * `absorb_exit_gotos!`    — a sealed cross-island goto (§5.5) whose
#     target block has one in-edge and only nothrow statements before an
#     exit terminator absorbs the target into the source region (break/
#     return run the same structural actions from the deeper position, §5.9).
#   * `dissolve_diverging_islands!` — a single-block island whose terminator
#     diverges (return/unreachable/break/continue/cross-island goto) splices
#     into the parent region; the dynamically-unreachable tail after it dies.
#   * `selectify!`            — an `if` with two tiny, speculatable, yielding
#     arms becomes `K"select"` with the arm bodies hoisted.
#
# Irreducible or unrecognized shapes stay as (smaller) cfg islands —
# correctness first, opportunistic recovery. All passes run in one editable
# session; every rewrite is followed by a global order-key relabel.

is_diverge_kind(k::UnifiedIR.Kind) =
    k === K"return" || k === K"unreachable" || k === K"break" || k === K"continue"

# ---------------------------------------------------------------------------
# Editable-state move primitives
# ---------------------------------------------------------------------------

"Move statement `s` (with its owned region subtree) to the tail of `dest`."
function move_to_tail!(ir::UnifiedIR.IR, s::StmtId, dest::RegionId)
    UnifiedIR.unlink!(ir, s)
    reg = UnifiedIR.getregion(ir, dest)
    UnifiedIR.link_between!(ir, s, dest, reg.last.id, Int32(0))
    for rid in UnifiedIR.owned_regions(ir, s)
        UnifiedIR.getregion(ir, rid).parent = dest
    end
    return nothing
end

"Move statement `s` (with its owned region subtree) immediately before `at`."
function move_before!(ir::UnifiedIR.IR, s::StmtId, at::StmtId)
    e = ir.edit::UnifiedIR.EditState
    UnifiedIR.unlink!(ir, s)
    r = UnifiedIR.stmt_region(ir, at)
    UnifiedIR.link_between!(ir, s, r, e.prev[at.id], at.id)
    for rid in UnifiedIR.owned_regions(ir, s)
        UnifiedIR.getregion(ir, rid).parent = r
    end
    return nothing
end

# Bookkeeping after a structural rewrite: order keys are re-spread from the
# new region structure so later `comes_before`/`visible` queries stay sound.
function structural_epoch!(ir::UnifiedIR.IR)
    UnifiedIR.relabel_okeys!(ir)
    ir.cache.stmt_epoch += 1
    ir.cache.region_epoch += 1
    return nothing
end

island_entry(ir::UnifiedIR.IR, cfgop::StmtId) =
    (rs = UnifiedIR.live_owned_regions(ir, cfgop); isempty(rs) ? NULL_REGION : rs[1])

"Rewrite a single-predecessor block's args to the edge operands and drop them."
function bind_block_args!(ir::UnifiedIR.IR, blk::RegionId, eargs::Vector{UnifiedIR.Operand})
    reg = UnifiedIR.getregion(ir, blk)
    for (i, a) in enumerate(copy(reg.args))
        UnifiedIR.replace_uses_where!(_ -> true, ir, a => eargs[i])
        UnifiedIR.kill_stmt!(ir, a)
    end
    empty!(reg.args)
    return nothing
end

# ---------------------------------------------------------------------------
# Linear merging (move-based; handles region-owning statements)
# ---------------------------------------------------------------------------

function merge_linear_blocks!(ir::UnifiedIR.IR)
    merged = 0
    changed = true
    while changed
        changed = false
        tgt = block_in_edges(ir)
        for s in collect(UnifiedIR.each_stmt(ir))
            UnifiedIR.is_tombstone(ir, s) && continue
            UnifiedIR.stmt_kind(ir, s) === K"goto" || continue
            brid = UnifiedIR.stmt_region(ir, s)
            breg = UnifiedIR.getregion(ir, brid)
            breg.kind === UnifiedIR.REGION_BLOCK || continue
            UnifiedIR.region_terminator(ir, brid) == s || continue
            dest, eargs = UnifiedIR.edge_bundles(ir, s)[1]
            dest == brid && continue
            dreg = UnifiedIR.getregion(ir, dest)
            dreg.dead && continue
            dreg.owner == breg.owner || continue          # same island only
            get(tgt, dest.id, 0) == 1 || continue         # sole predecessor
            island_entry(ir, dreg.owner) == dest && continue
            length(dreg.args) == length(eargs) || continue
            bind_block_args!(ir, dest, eargs)
            UnifiedIR.kill_stmt!(ir, s)
            for m in UnifiedIR.region_stmts(ir, dest)
                move_to_tail!(ir, m, brid)
            end
            UnifiedIR.kill_region!(ir, dest)
            structural_epoch!(ir)
            merged += 1
            changed = true
            break
        end
    end
    return merged
end

# ---------------------------------------------------------------------------
# If recovery
# ---------------------------------------------------------------------------

# A block usable as an if arm: not the source, not the entry, same island,
# exactly one global in-edge (the branch edge under consideration).
function _arm_ok(ir::UnifiedIR.IR, tgt::Dict{Int32,Int}, owner::StmtId,
                 areg::RegionId, X::RegionId)
    X == areg && return false
    xr = UnifiedIR.getregion(ir, X)
    xr.dead && return false
    xr.owner == owner || return false
    get(tgt, X.id, 0) == 1 || return false
    island_entry(ir, owner) != X || return false
    return true
end

# Move `blk`'s members into a fresh arm of `ifop`, binding its block args to
# the edge operands. Returns the arm region.
function _build_arm!(ir::UnifiedIR.IR, ifop::StmtId, blk::RegionId,
                     eargs::Vector{UnifiedIR.Operand})
    bind_block_args!(ir, blk, eargs)
    arm = UnifiedIR.new_region!(ir, ifop, UnifiedIR.REGION_ARM)
    for m in UnifiedIR.region_stmts(ir, blk)
        move_to_tail!(ir, m, arm)
    end
    UnifiedIR.kill_region!(ir, blk)
    return arm
end

function collapse_branches!(ir::UnifiedIR.IR)
    n = 0
    changed = true
    while changed
        changed = false
        tgt = block_in_edges(ir)
        for t in collect(UnifiedIR.each_stmt(ir))
            UnifiedIR.is_tombstone(ir, t) && continue
            UnifiedIR.stmt_kind(ir, t) === K"br_if" || continue
            areg = UnifiedIR.stmt_region(ir, t)
            aregion = UnifiedIR.getregion(ir, areg)
            aregion.kind === UnifiedIR.REGION_BLOCK || continue
            UnifiedIR.region_terminator(ir, areg) == t || continue
            bs = UnifiedIR.edge_bundles(ir, t)
            (Tr, aT), (Fr, aF) = bs[1], bs[2]
            owner = aregion.owner
            cond = UnifiedIR.getop(ir, t, 1)
            if Tr == Fr
                # degenerate branch: both edges to the same block
                map(o -> o.bits, aT) == map(o -> o.bits, aF) || continue
                UnifiedIR.replace_stmt!(ir, t, K"goto", UnifiedIR.op_block(Tr),
                                        UnifiedIR.op_inline(length(aT)), aT...)
                n += 1; changed = true
                break
            end
            if _collapse_diamond!(ir, tgt, t, owner, areg, cond, Tr, aT, Fr, aF) ||
               _collapse_triangle!(ir, tgt, t, owner, areg, cond, Tr, aT, Fr, aF)
                n += 1; changed = true
                break
            end
        end
    end
    return n
end

# Emit A's replacement terminator for a join at `J` carrying the if's results.
function _join_goto!(ir::UnifiedIR.IR, t::StmtId, ifop::StmtId, J::RegionId, k::Int)
    if k == 0
        UnifiedIR.replace_stmt!(ir, t, K"goto", UnifiedIR.op_block(J), UnifiedIR.op_inline(0))
    elseif k == 1
        UnifiedIR.set_type!(ir, ifop, Any)
        UnifiedIR.replace_stmt!(ir, t, K"goto", UnifiedIR.op_block(J), UnifiedIR.op_inline(1),
                                UnifiedIR.op_stmt(ifop))
    else
        UnifiedIR.set_type!(ir, ifop, Any)
        exts = UnifiedIR.Operand[]
        for i in 1:k
            e = UnifiedIR.insert_before!(ir, t, K"extract", UnifiedIR.op_stmt(ifop),
                                         UnifiedIR.op_inline(i - 1); type = Any)
            push!(exts, UnifiedIR.op_stmt(e))
        end
        UnifiedIR.replace_stmt!(ir, t, K"goto", UnifiedIR.op_block(J), UnifiedIR.op_inline(k), exts...)
    end
    return nothing
end

function _collapse_diamond!(ir, tgt, t, owner, areg, cond, Tr, aT, Fr, aF)
    (_arm_ok(ir, tgt, owner, areg, Tr) && _arm_ok(ir, tgt, owner, areg, Fr)) || return false
    tT = UnifiedIR.region_terminator(ir, Tr)
    tF = UnifiedIR.region_terminator(ir, Fr)
    (tT === nothing || tF === nothing) && return false
    kT = UnifiedIR.stmt_kind(ir, tT)
    kF = UnifiedIR.stmt_kind(ir, tF)
    if kT === K"goto" && kF === K"goto"
        # diamond with a common join
        (JT, aJT) = UnifiedIR.edge_bundles(ir, tT)[1]
        (JF, aJF) = UnifiedIR.edge_bundles(ir, tF)[1]
        JT == JF || return false
        J = JT
        (J == areg || J == Tr || J == Fr) && return false
        jreg = UnifiedIR.getregion(ir, J)
        (jreg.dead || jreg.owner != owner) && return false
        k = length(aJT)
        k == length(aJF) || return false
        ifop = UnifiedIR.insert_before!(ir, t, K"if", cond; type = Any)
        armT = _build_arm!(ir, ifop, Tr, aT)
        armF = _build_arm!(ir, ifop, Fr, aF)
        # re-read the join args after arg binding rewrote operand pools
        aJT2 = UnifiedIR.edge_bundles(ir, tT)[1][2]
        aJF2 = UnifiedIR.edge_bundles(ir, tF)[1][2]
        UnifiedIR.kill_stmt!(ir, tT)
        UnifiedIR.kill_stmt!(ir, tF)
        UnifiedIR.push_stmt!(ir, armT, K"yield", aJT2...)
        UnifiedIR.push_stmt!(ir, armF, K"yield", aJF2...)
        _join_goto!(ir, t, ifop, J, k)
        structural_epoch!(ir)
        return true
    end
    exitlike(k) = k === K"yield" || is_diverge_kind(k)
    if exitlike(kT) && exitlike(kF)
        # arms keep their exit terminators: a yield in the arm yields the if
        ny = (kT === K"yield" ? 1 : 0) + (kF === K"yield" ? 1 : 0)
        yar = -1
        if kT === K"yield"
            yar = UnifiedIR.nops(ir, tT)
        end
        if kF === K"yield"
            yf = UnifiedIR.nops(ir, tF)
            yar >= 0 && yf != yar && return false
            yar = yf
        end
        (ny > 0 && yar > 1) && return false      # multi-value island yields: keep cfg
        ifop = UnifiedIR.insert_before!(ir, t, K"if", cond; type = Any)
        _build_arm!(ir, ifop, Tr, aT)
        _build_arm!(ir, ifop, Fr, aF)
        if ny == 0
            UnifiedIR.replace_stmt!(ir, t, K"unreachable")
        elseif yar == 1
            UnifiedIR.set_type!(ir, ifop, Any)
            UnifiedIR.replace_stmt!(ir, t, K"yield", UnifiedIR.op_stmt(ifop))
        else
            UnifiedIR.replace_stmt!(ir, t, K"yield")
        end
        structural_epoch!(ir)
        return true
    end
    return false
end

function _collapse_triangle!(ir, tgt, t, owner, areg, cond, Tr, aT, Fr, aF)
    # one side is the join itself; the other side is a single-pred block that
    # jumps straight to it
    local side::RegionId, aside::Vector{UnifiedIR.Operand}
    local J::RegionId, aJ::Vector{UnifiedIR.Operand}
    side_is_then = false
    if _arm_ok(ir, tgt, owner, areg, Tr) && Fr != Tr
        side, aside, J, aJ = Tr, aT, Fr, aF
        side_is_then = true
    elseif _arm_ok(ir, tgt, owner, areg, Fr) && Tr != Fr
        side, aside, J, aJ = Fr, aF, Tr, aT
        side_is_then = false
    else
        return false
    end
    J == areg && return false
    jreg = UnifiedIR.getregion(ir, J)
    (jreg.dead || jreg.owner != owner) && return false
    st = UnifiedIR.region_terminator(ir, side)
    st === nothing && return false
    UnifiedIR.stmt_kind(ir, st) === K"goto" || return false
    (JS, aJS) = UnifiedIR.edge_bundles(ir, st)[1]
    JS == J || return false
    k = length(aJS)
    k == length(aJ) || return false
    ifop = UnifiedIR.insert_before!(ir, t, K"if", cond; type = Any)
    if side_is_then
        arm = _build_arm!(ir, ifop, side, aside)
        aJS2 = UnifiedIR.edge_bundles(ir, st)[1][2]
        UnifiedIR.kill_stmt!(ir, st)
        UnifiedIR.push_stmt!(ir, arm, K"yield", aJS2...)
        if k > 0
            er = UnifiedIR.new_region!(ir, ifop, UnifiedIR.REGION_ARM)
            UnifiedIR.push_stmt!(ir, er, K"yield", aJ...)
        end
    else
        # side is the false successor: then-arm carries the direct-edge values
        tr = UnifiedIR.new_region!(ir, ifop, UnifiedIR.REGION_ARM)
        UnifiedIR.push_stmt!(ir, tr, K"yield", aJ...)
        arm = _build_arm!(ir, ifop, side, aside)
        aJS2 = UnifiedIR.edge_bundles(ir, st)[1][2]
        UnifiedIR.kill_stmt!(ir, st)
        UnifiedIR.push_stmt!(ir, arm, K"yield", aJS2...)
    end
    _join_goto!(ir, t, ifop, J, k)
    structural_epoch!(ir)
    return true
end

# ---------------------------------------------------------------------------
# Loop recovery
# ---------------------------------------------------------------------------

"All statements inside the region subtrees of `blocks` (nested included)."
function _subtree_stmts(ir::UnifiedIR.IR, blocks::Vector{RegionId})
    S = Set{Int32}()
    work = copy(blocks)
    while !isempty(work)
        r = pop!(work)
        for m in UnifiedIR.region_stmts(ir, r)
            push!(S, m.id)
            if UnifiedIR.owns_regions(UnifiedIR.stmt_kind(ir, m))
                append!(work, UnifiedIR.live_owned_regions(ir, m))
            end
        end
    end
    return S
end

function collapse_loops!(ir::UnifiedIR.IR)
    n = 0
    for cfgop in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, cfgop) && continue
        UnifiedIR.stmt_kind(ir, cfgop) === K"cfg" || continue
        # one collapse per island per call: dominators/in-edges go stale
        _collapse_one_loop!(ir, cfgop) && (n += 1)
    end
    return n
end

function _collapse_one_loop!(ir::UnifiedIR.IR, cfgop::StmtId)
    blocks = UnifiedIR.live_owned_regions(ir, cfgop)
    length(blocks) >= 1 || return false
    dom = UnifiedIR.island_dominators(ir, cfgop)
    own = Set{Int32}(b.id for b in blocks)
    # back edges: terminator edge to a dominator
    backs = Tuple{RegionId,RegionId}[]
    for b in blocks
        haskey(dom, b) || continue                     # unreachable source
        t = UnifiedIR.region_terminator(ir, b)
        t === nothing && continue
        is_edge_kind(UnifiedIR.stmt_kind(ir, t)) || continue
        for (dest, _) in UnifiedIR.edge_bundles(ir, t)
            dest.id in own || continue
            dest in dom[b] && push!(backs, (b, dest))
        end
    end
    isempty(backs) && return false
    headers = unique(h for (_, h) in backs)
    sort!(headers, by = h -> length(dom[h]), rev = true)   # innermost first
    for H in headers
        _collapse_natural_loop!(ir, cfgop, dom, own, H,
                                [b for (b, h) in backs if h == H]) && return true
    end
    return false
end

function _collapse_natural_loop!(ir::UnifiedIR.IR, cfgop::StmtId,
                                 dom::Dict{RegionId,Set{RegionId}},
                                 own::Set{Int32}, H::RegionId, srcs::Vector{RegionId})
    isempty(UnifiedIR.getregion(ir, H).args) || return false
    # predecessor map over the island's block graph
    blocks = UnifiedIR.live_owned_regions(ir, cfgop)
    preds = Dict{Int32,Vector{RegionId}}()
    for b in blocks
        t = UnifiedIR.region_terminator(ir, b)
        t === nothing && continue
        is_edge_kind(UnifiedIR.stmt_kind(ir, t)) || continue
        for (dest, _) in UnifiedIR.edge_bundles(ir, t)
            dest.id in own && push!(get!(() -> RegionId[], preds, dest.id), b)
        end
    end
    # natural loop membership
    L = Set{Int32}(H.id)
    work = RegionId[s for s in srcs if s != H]
    while !isempty(work)
        x = pop!(work)
        x.id in L && continue
        haskey(dom, x) || return false                 # unreachable member
        push!(L, x.id)
        for p in get(preds, x.id, RegionId[])
            p == H || p.id in L || push!(work, p)
        end
    end
    # reducibility: every member is dominated by H
    for xid in L
        H in get(dom, RegionId(xid), Set{RegionId}()) || return false
    end
    Lblocks = RegionId[b for b in blocks if b.id in L]
    # pure latches: blocks whose only content is `goto ^H ()`
    latch = Set{Int32}()
    for b in Lblocks
        b == H && continue
        ms = UnifiedIR.region_stmts(ir, b)
        length(ms) == 1 || continue
        isempty(UnifiedIR.getregion(ir, b).args) || continue
        UnifiedIR.stmt_kind(ir, ms[1]) === K"goto" || continue
        d, ea = UnifiedIR.edge_bundles(ir, ms[1])[1]
        (d == H && isempty(ea)) && push!(latch, b.id)
    end
    # terminator-level edge audit: all edges stay in this island; exits share
    # one target with no arguments
    E = NULL_REGION
    for b in Lblocks
        t = UnifiedIR.region_terminator(ir, b)
        t === nothing && return false
        tk = UnifiedIR.stmt_kind(ir, t)
        if is_edge_kind(tk)
            tk === K"await" && return false
            for (dest, eargs) in UnifiedIR.edge_bundles(ir, t)
                dest.id in own || return false          # cross-island edge: keep cfg
                if !(dest.id in L)
                    isempty(eargs) || return false
                    (UnifiedIR.isnull(E) || E == dest) || return false   # single exit target
                    E = dest
                end
            end
        elseif !(tk === K"return" || tk === K"unreachable" ||
                 tk === K"break" || tk === K"continue")
            return false
        end
    end
    E == H && return false
    # nested-statement audit: no BLOCK operand anywhere in the subtree may
    # target a block of THIS island (nested exits would cross the new loop
    # in unmodeled ways); other islands' targets are their own business only
    # if they are descendants (moved along) — ancestors are refused too (v1).
    S = _subtree_stmts(ir, Lblocks)
    terms = Set{Int32}()
    for b in Lblocks
        t = UnifiedIR.region_terminator(ir, b)
        t === nothing || push!(terms, t.id)
    end
    for sid in S
        sid in terms && continue
        s = StmtId(sid)
        for i in 1:UnifiedIR.nops(ir, s)
            UnifiedIR.optag(UnifiedIR.getop(ir, s, i)) == UnifiedIR.TAG_BLOCK && return false
        end
    end
    # escape audit: no def inside the loop subtree may be used outside it
    escaped = false
    UnifiedIR.each_ssa_use(ir) do site, used
        escaped && return
        used.id in S || return
        site isa UnifiedIR.StmtOperand || (escaped = true; return)
        site.user.id in S || (escaped = true)
    end
    escaped && return false

    # ---- transform ---------------------------------------------------------
    core = RegionId[b for b in Lblocks if !(b.id in latch)]
    # H first: it becomes the inner island's entry
    sort!(core, by = b -> b == H ? 0 : 1)
    Hmembers = UnifiedIR.region_stmts(ir, H)
    loopop = UnifiedIR.push_stmt!(ir, H, K"loop"; type = Any)
    bodyr = UnifiedIR.new_region!(ir, loopop, UnifiedIR.REGION_LOOP_BODY)
    inner = UnifiedIR.push_stmt!(ir, bodyr, K"cfg"; type = Any)
    bmap = Dict{Int32,RegionId}()
    for b in core
        bmap[b.id] = UnifiedIR.new_region!(ir, inner, UnifiedIR.REGION_BLOCK)
    end
    for b in core
        nb = bmap[b.id]
        for m in (b == H ? Hmembers : UnifiedIR.region_stmts(ir, b))
            m == loopop && continue
            move_to_tail!(ir, m, nb)
        end
        nreg = UnifiedIR.getregion(ir, nb)
        breg = UnifiedIR.getregion(ir, b)
        append!(nreg.args, breg.args)
        empty!(breg.args)
    end
    # trampolines, created on demand
    tcr = Ref(NULL_REGION); ter = Ref(NULL_REGION)
    function tc!()
        if UnifiedIR.isnull(tcr[])
            tcr[] = UnifiedIR.new_region!(ir, inner, UnifiedIR.REGION_BLOCK)
            UnifiedIR.push_stmt!(ir, tcr[], K"continue", UnifiedIR.op_region(bodyr),
                                 UnifiedIR.op_inline(true))
        end
        tcr[]
    end
    function te!()
        if UnifiedIR.isnull(ter[])
            ter[] = UnifiedIR.new_region!(ir, inner, UnifiedIR.REGION_BLOCK)
            UnifiedIR.push_stmt!(ir, ter[], K"break", UnifiedIR.op_region(bodyr))
        end
        ter[]
    end
    resolve(d::RegionId) = (d == H || d.id in latch) ? (:header) :
                           d.id in L ? bmap[d.id] : (:exit)
    for b in core
        nb = bmap[b.id]
        t = UnifiedIR.region_terminator(ir, nb)
        t === nothing && continue
        tk = UnifiedIR.stmt_kind(ir, t)
        is_edge_kind(tk) || continue
        if tk === K"goto"
            d, _ = UnifiedIR.edge_bundles(ir, t)[1]
            r = resolve(d)
            if r === :header
                UnifiedIR.replace_stmt!(ir, t, K"continue", UnifiedIR.op_region(bodyr),
                                        UnifiedIR.op_inline(true))
            elseif r === :exit
                UnifiedIR.replace_stmt!(ir, t, K"break", UnifiedIR.op_region(bodyr))
            else
                UnifiedIR.setop!(ir, t, 1, UnifiedIR.op_block(r::RegionId))
            end
        elseif tk === K"br_if"
            (d1, _), (d2, _) = UnifiedIR.edge_bundles(ir, t)
            r1, r2 = resolve(d1), resolve(d2)
            cnd = UnifiedIR.getop(ir, t, 1)
            if r1 === :header && r2 === :exit
                UnifiedIR.replace_stmt!(ir, t, K"continue", UnifiedIR.op_region(bodyr), cnd)
            elseif r1 === :exit && r2 === :header
                nc = UnifiedIR.insert_before!(ir, t, K"call",
                        UnifiedIR.vop(ir, GlobalRef(Base, :not_int)), cnd; type = Bool)
                UnifiedIR.replace_stmt!(ir, t, K"continue", UnifiedIR.op_region(bodyr),
                                        UnifiedIR.op_stmt(nc))
            else
                for i in 1:UnifiedIR.nops(ir, t)
                    o = UnifiedIR.getop(ir, t, i)
                    UnifiedIR.optag(o) == UnifiedIR.TAG_BLOCK || continue
                    r = resolve(UnifiedIR.asregion(o))
                    nd = r === :header ? tc!() : r === :exit ? te!() : (r::RegionId)
                    UnifiedIR.setop!(ir, t, i, UnifiedIR.op_block(nd))
                end
            end
        else # switch
            for i in 1:UnifiedIR.nops(ir, t)
                o = UnifiedIR.getop(ir, t, i)
                UnifiedIR.optag(o) == UnifiedIR.TAG_BLOCK || continue
                r = resolve(UnifiedIR.asregion(o))
                nd = r === :header ? tc!() : r === :exit ? te!() : (r::RegionId)
                UnifiedIR.setop!(ir, t, i, UnifiedIR.op_block(nd))
            end
        end
    end
    UnifiedIR.push_stmt!(ir, bodyr, K"unreachable")
    if UnifiedIR.isnull(E)
        UnifiedIR.push_stmt!(ir, H, K"unreachable")
    else
        UnifiedIR.push_stmt!(ir, H, K"goto", UnifiedIR.op_block(E), UnifiedIR.op_inline(0))
    end
    for b in core
        b == H || UnifiedIR.kill_region!(ir, b)
    end
    for bid in latch
        UnifiedIR.kill_region!(ir, RegionId(bid))
    end
    structural_epoch!(ir)
    return true
end

# ---------------------------------------------------------------------------
# Sealed cross-island goto absorption (§5.5/§5.9)
# ---------------------------------------------------------------------------

function absorb_exit_gotos!(ir::UnifiedIR.IR)
    n = 0
    changed = true
    while changed
        changed = false
        tgt = block_in_edges(ir)
        for s in collect(UnifiedIR.each_stmt(ir))
            UnifiedIR.is_tombstone(ir, s) && continue
            UnifiedIR.stmt_kind(ir, s) === K"goto" || continue
            sreg = UnifiedIR.stmt_region(ir, s)
            UnifiedIR.region_terminator(ir, sreg) == s || continue
            dest, eargs = UnifiedIR.edge_bundles(ir, s)[1]
            isempty(eargs) || continue
            dreg = UnifiedIR.getregion(ir, dest)
            (dreg.dead || dreg.kind !== UnifiedIR.REGION_BLOCK) && continue
            eb = UnifiedIR.enclosing_block(ir, s)
            # cross-island only (same-island is merge_linear_blocks!'s case)
            (!UnifiedIR.isnull(eb) && UnifiedIR.getregion(ir, eb).owner == dreg.owner) && continue
            island_entry(ir, dreg.owner) == dest && continue
            get(tgt, dest.id, 0) == 1 || continue
            isempty(dreg.args) || continue
            members = UnifiedIR.region_stmts(ir, dest)
            isempty(members) && continue
            term = members[end]
            UnifiedIR.is_terminator(UnifiedIR.stmt_kind(ir, term)) || continue
            tk = UnifiedIR.stmt_kind(ir, term)
            is_diverge_kind(tk) || continue
            ok = true
            for m in members
                m == term && continue
                mk = UnifiedIR.stmt_kind(ir, m)
                (UnifiedIR.owns_regions(mk) || mk === K"region_arg") && (ok = false; break)
                # moved statements execute under deeper handlers: nothrow only
                UnifiedIR.stmt_flag(ir, m) & UnifiedIR.FLAG_NOTHROW != 0 || (ok = false; break)
            end
            ok || continue
            if tk === K"break" || tk === K"continue"
                tr = UnifiedIR.asregion(UnifiedIR.getop(ir, term, 1))
                UnifiedIR.is_ancestor(ir, tr, sreg) || continue
            end
            UnifiedIR.kill_stmt!(ir, s)
            for m in members
                move_to_tail!(ir, m, sreg)
            end
            UnifiedIR.kill_region!(ir, dest)
            structural_epoch!(ir)
            n += 1
            changed = true
            break
        end
    end
    return n
end

# ---------------------------------------------------------------------------
# Diverging single-block island dissolution
# ---------------------------------------------------------------------------

function dissolve_diverging_islands!(ir::UnifiedIR.IR)
    n = 0
    for c in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, c) && continue
        UnifiedIR.stmt_kind(ir, c) === K"cfg" || continue
        UnifiedIR.nops(ir, c) == 0 || continue
        rs = UnifiedIR.live_owned_regions(ir, c)
        length(rs) == 1 || continue
        blk = rs[1]
        isempty(UnifiedIR.getregion(ir, blk).args) || continue
        term = UnifiedIR.region_terminator(ir, blk)
        term === nothing && continue
        tk = UnifiedIR.stmt_kind(ir, term)
        # yield-terminated islands are dissolve_islands!'s case; goto-terminated
        # ones must keep their island (the exit converter only lowers `goto`
        # inside island blocks — absorb_exit_gotos! resolves the absorbable
        # cases at block level before this can fire)
        is_diverge_kind(tk) || continue
        parent = UnifiedIR.stmt_region(ir, c)
        # the island never yields: any use of its (Union{}) result must sit in
        # the dynamically-unreachable tail we are about to remove
        ok = true
        UnifiedIR.each_ssa_use(ir) do site, used
            (ok && used == c) || return
            site isa UnifiedIR.StmtOperand || (ok = false; return)
            x = site.user
            steps = 0
            while UnifiedIR.stmt_region(ir, x) != parent
                ow = UnifiedIR.getregion(ir, UnifiedIR.stmt_region(ir, x)).owner
                (UnifiedIR.isnull(ow) || (steps += 1) > UnifiedIR.nregions(ir)) &&
                    (ok = false; return)
                x = ow
            end
            (x != c && UnifiedIR.comes_before(ir, c, x)) || (ok = false)
        end
        ok || continue
        e = ir.edit::UnifiedIR.EditState
        tail = StmtId[]
        i = e.next[c.id]
        while i != 0
            push!(tail, StmtId(i))
            i = e.next[i]
        end
        for m in UnifiedIR.region_stmts(ir, blk)
            move_before!(ir, m, c)
        end
        UnifiedIR.kill_stmt!(ir, c)
        for m in tail
            UnifiedIR.kill_stmt!(ir, m)
        end
        structural_epoch!(ir)
        n += 1
    end
    return n
end

# ---------------------------------------------------------------------------
# select conversion
# ---------------------------------------------------------------------------

"""
    selectify!(ir) -> Int

`if` ops with two tiny, fully speculatable arms (all members EFFECT_FREE |
NOTHROW | TERMINATES, no region owners) that each yield one value become
`K"select"` with the arm bodies hoisted before it. Editable state.
"""
function selectify!(ir::UnifiedIR.IR)
    n = 0
    spec = UnifiedIR.FLAG_EFFECT_FREE | UnifiedIR.FLAG_NOTHROW | UnifiedIR.FLAG_TERMINATES
    for s in collect(UnifiedIR.each_stmt(ir))
        UnifiedIR.is_tombstone(ir, s) && continue
        UnifiedIR.stmt_kind(ir, s) === K"if" || continue
        rs = UnifiedIR.live_owned_regions(ir, s)
        length(rs) == 2 || continue
        ok = true
        total = 0
        vals = UnifiedIR.Operand[]
        hoist = StmtId[]
        for r in rs
            ms = UnifiedIR.region_stmts(ir, r)
            isempty(ms) && (ok = false; break)
            term = ms[end]
            (UnifiedIR.stmt_kind(ir, term) === K"yield" && UnifiedIR.nops(ir, term) == 1) ||
                (ok = false; break)
            for m in ms[1:end-1]
                mk = UnifiedIR.stmt_kind(ir, m)
                (UnifiedIR.owns_regions(mk) || mk === K"region_arg") && (ok = false; break)
                # never speculate control-dependent type assertions (refine is
                # a Pi: its narrowing only holds under the branch condition),
                # nor extracts (their default-pure flags assume a well-typed
                # operand that the branch may be guarding)
                (mk === K"refine" || mk === K"extract") && (ok = false; break)
                UnifiedIR.stmt_flag(ir, m) & spec == spec || (ok = false; break)
                push!(hoist, m)
                total += 1
            end
            (ok && total <= 4) || (ok = false; break)
            push!(vals, UnifiedIR.getop(ir, term, 1))
        end
        ok || (empty!(hoist); continue)
        for m in hoist
            move_before!(ir, m, s)
        end
        sel = UnifiedIR.insert_before!(ir, s, K"select", UnifiedIR.getop(ir, s, 1),
                                       vals[1], vals[2]; type = UnifiedIR.stmt_type(ir, s))
        UnifiedIR.replace_uses_where!(_ -> true, ir, s => UnifiedIR.op_stmt(sel))
        UnifiedIR.kill_stmt!(ir, s)
        structural_epoch!(ir)
        n += 1
    end
    return n
end

# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

"""
    structurize!(ir) -> Int

Recover structured `if`/`loop` regions from reducible cfg islands (§10.5),
iterating the local rules to a fixpoint. Requires editable state; returns the
number of rewrites performed. Irreducible or unrecognized shapes are left as
(smaller) islands.
"""
function structurize!(ir::UnifiedIR.IR)
    UnifiedIR.check_state(ir, UnifiedIR.LAYOUT_EDITABLE, "structurize!")
    total = 0
    for _ in 1:64
        changed = 0
        changed += merge_linear_blocks!(ir)
        changed += collapse_branches!(ir)
        changed += collapse_loops!(ir)
        changed += absorb_exit_gotos!(ir)
        changed += dissolve_islands!(ir)
        changed += dissolve_diverging_islands!(ir)
        changed += drop_unreachable_blocks!(ir)
        total += changed
        changed == 0 && break
    end
    return total
end
