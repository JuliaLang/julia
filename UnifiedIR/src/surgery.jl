# Region surgery (§4.2): verified, transactional structure edits in editable
# state. Each operation keeps the region table, terminators, and result
# feeding consistent.

"Internal: tombstone any statement (no plain-stmt restriction), unlinking it."
function kill_stmt!(ir::IR, s::StmtId)
    body = ir.body
    body.kind[s.id] === KIND_DELETED && return
    if owns_regions(body.kind[s.id])
        for rid in owned_regions(ir, s)
            kill_region!(ir, rid)
        end
    end
    layout(ir) === LAYOUT_EDITABLE && unlink!(ir, s)
    body.kind[s.id] = KIND_DELETED
    body.ops[s.id] = OPS_EMPTY
    body.type[s.id] = nothing
    body.flag[s.id] = FLAG_PURE
    ir.cache.stmt_epoch += 1
    return nothing
end

"Internal: kill a whole region subtree (stmts tombstoned, regions marked dead)."
function kill_region!(ir::IR, r::RegionId)
    reg = getregion(ir, r)
    reg.dead && return
    for s in region_stmts(ir, r)
        kill_stmt!(ir, s)
    end
    reg.dead = true
    reg.first = NULL_STMT
    reg.last = NULL_STMT
    empty!(reg.args)
    ir.cache.region_epoch += 1
    return nothing
end

"Create a new (empty) region owned by `owner`."
function new_region!(ir::IR, owner::StmtId, kind::RegionKind;
                     activation::Activation = ACT_IMMEDIATE)
    check_state(ir, LAYOUT_EDITABLE, "new_region!")
    owns_regions(stmt_kind(ir, owner)) ||
        error("new_region!: kind $(kindname(stmt_kind(ir, owner))) does not own regions")
    reg = Region(kind, owner, stmt_region(ir, owner); activation)
    push!(ir.regions, reg)
    ir.cache.region_epoch += 1
    return RegionId(length(ir.regions))
end

"""
    replace_uses_where!(pred, ir, old => new)

Immediately rewrite `ssa_use` references of `old` to `new` at use sites where
`pred(user::StmtId)` holds. (Targeted form of `replace_uses!` used by surgery.)
"""
function replace_uses_where!(pred, ir::IR, p::Pair{StmtId,Operand})
    old, new = p
    body = ir.body
    for i in 1:Int(body.len)
        body.kind[i] === KIND_DELETED && continue
        s = StmtId(i)
        pred(s) || continue
        w = body.ops[i]
        if is_ops_inline(w)
            if inline_stmt(w).id == old.id
                if optag(new) == TAG_STMT
                    body.ops[i] = set_inline_stmt(w, asstmt(new))
                else
                    ops = operands(ir, s)
                    ops[1] = new
                    off = length(body.operands)
                    append!(body.operands, ops)
                    body.ops[i] = ops_pool(off, length(ops))
                end
            end
        else
            n = Int(ops_len(w)); off = ops_offset(w)
            for j in 1:n
                o = body.operands[off + j]
                optag(o) == TAG_STMT && asstmt(o).id == old.id &&
                    (body.operands[off + j] = new)
            end
        end
    end
    for reg in ir.regions
        if is_guard(reg) && reg.cond == old
            optag(new) == TAG_STMT || error("guard condition must remain a statement")
            reg.cond = asstmt(new)
        end
    end
    ir.cache.stmt_epoch += 1
    return ir
end

# ---------------------------------------------------------------------------
# wrap_in_if! (§4.2, worked example §4.5)
# ---------------------------------------------------------------------------

"""
    wrap_in_if!(ir, first, last, cond; else_arm=nothing) -> StmtId

Move the contiguous run `first..last` (in one region's list) into the then-arm
of a fresh `if cond`. Defs escaping the run are threaded through fresh `result` terminators
and their outside uses rewritten to the if's result (§5.1 rule 4). Escaping
defs require an `else_arm` builder `(ir, region) -> ()` that terminates the
arm (diverging, or producing matching result arity).
"""
function wrap_in_if!(ir::IR, first::StmtId, last::StmtId, cond::Value;
                     else_arm = nothing)
    check_state(ir, LAYOUT_EDITABLE, "wrap_in_if!")
    e = ir.edit::EditState
    r = stmt_region(ir, first)
    stmt_region(ir, last) == r || error("wrap_in_if!: run spans regions")
    # collect the run (direct members first..last)
    run = StmtId[]
    i = first
    while true
        push!(run, i)
        i == last && break
        nx = e.next[i.id]
        nx == 0 && error("wrap_in_if!: `last` does not follow `first` in the region list")
        i = StmtId(nx)
    end
    any(s -> is_terminator(stmt_kind(ir, s)), run) &&
        error("wrap_in_if!: run contains a terminator")
    # the if op takes the run's place
    ifop = insert_before!(ir, first, K"if", cond; type = Any)
    arm = new_region!(ir, ifop, REGION_ARM)
    # move the run into the arm
    for s in run
        unlink!(ir, s)
    end
    armreg = getregion(ir, arm)
    prevtail = Int32(0)
    for s in run
        link_between!(ir, s, arm, prevtail, Int32(0))
        prevtail = s.id
        # nested regions owned by s keep their owner; their parent chain is
        # unchanged (parent = the region s sits in) — update it:
        for rid in owned_regions(ir, s)
            getregion(ir, rid).parent = arm
        end
    end
    # escaping defs: run members with a result used outside the run subtree
    inside = Set{Int32}(s.id for s in run)
    function in_subtree(u::StmtId)
        u.id in inside && return true
        rr = stmt_region(ir, u)
        while !isnull(rr)
            rr == arm && return true
            reg = getregion(ir, rr)
            reg.owner.id in inside && return true
            rr = reg.parent
        end
        return false
    end
    escaping = StmtId[]
    counts = zeros(Int32, Int(ir.body.len))
    each_ssa_use(ir) do site, used
        site isa StmtOperand || return
        if used.id in inside && !in_subtree(site.user) && site.user != ifop
            counts[used.id] += 1
        end
    end
    for s in run
        counts[s.id] > 0 && push!(escaping, s)
    end
    # terminate the then-arm
    push_stmt!(ir, arm, K"result", escaping...)
    if isempty(escaping)
        set_type!(ir, ifop, Nothing)
        if else_arm !== nothing
            er = new_region!(ir, ifop, REGION_ARM)
            else_arm(ir, er)
            region_terminator(ir, er) === nothing && error("wrap_in_if!: else_arm did not terminate")
        end
    else
        else_arm === nothing &&
            error("wrap_in_if!: $(length(escaping)) defs escape the run; supply an else_arm (diverging or producing matching result arity) — §4.2 precondition")
        er = new_region!(ir, ifop, REGION_ARM)
        else_arm(ir, er)
        region_terminator(ir, er) === nothing && error("wrap_in_if!: else_arm did not terminate")
        # thread results: single value directly, tuple via extracts
        if length(escaping) == 1
            set_type!(ir, ifop, stmt_type(ir, escaping[1]))
            replace_uses_where!(u -> !in_subtree(u) && u != ifop, ir,
                                escaping[1] => op_stmt(ifop))
        else
            set_type!(ir, ifop, Tuple{Any[stmt_type(ir, s) for s in escaping]...})
            for (idx, s) in enumerate(escaping)
                ex = insert_after!(ir, ifop, K"extract", op_stmt(ifop), op_inline(idx);
                                   type = stmt_type(ir, s))
                replace_uses_where!(u -> !in_subtree(u) && u != ifop && u != ex, ir,
                                    s => op_stmt(ex))
            end
        end
    end
    ir.cache.region_epoch += 1
    return ifop
end

"""
    wrap_in_loop!(ir, first, last, cond) -> StmtId

Wrap the run into a do-while loop: `loop { run…; continue cond () }` with no
carried values. Escaping defs are unsupported (they would need carried args).
"""
function wrap_in_loop!(ir::IR, first::StmtId, last::StmtId, cond::Value)
    check_state(ir, LAYOUT_EDITABLE, "wrap_in_loop!")
    e = ir.edit::EditState
    r = stmt_region(ir, first)
    run = StmtId[]
    i = first
    while true
        push!(run, i)
        i == last && break
        i = StmtId(e.next[i.id])
        i.id == 0 && error("wrap_in_loop!: bad run")
    end
    loop = insert_before!(ir, first, K"loop"; type = Nothing)
    bodyr = new_region!(ir, loop, REGION_LOOP_BODY)
    for s in run
        unlink!(ir, s)
    end
    prevtail = Int32(0)
    for s in run
        link_between!(ir, s, bodyr, prevtail, Int32(0))
        prevtail = s.id
        for rid in owned_regions(ir, s)
            getregion(ir, rid).parent = bodyr
        end
    end
    inside = Set{Int32}(s.id for s in run)
    each_ssa_use(ir) do site, used
        site isa StmtOperand || return
        used.id in inside && !(site.user.id in inside) &&
            error("wrap_in_loop!: def %$(used.id) escapes the wrapped run")
    end
    push_stmt!(ir, bodyr, K"continue", op_region(bodyr), cond isa Operand ? cond : op_stmt(cond))
    ir.cache.region_epoch += 1
    return loop
end

# ---------------------------------------------------------------------------
# Region inlining (folded conditions; irinterp arm deletion)
# ---------------------------------------------------------------------------

"""
    inline_region!(ir, owner, keep::RegionId)

Dissolve region-owning op `owner`, splicing the contents of its region `keep`
into the parent at the owner's position and killing the other regions. Result
uses are rewritten to the kept region's result operands (single value directly;
tuple results through their `extract`s). The result terminator and the owner are removed.
"""
function inline_region!(ir::IR, owner::StmtId, keep::RegionId)
    check_state(ir, LAYOUT_EDITABLE, "inline_region!")
    keepreg = getregion(ir, keep)
    keepreg.owner == owner || error("inline_region!: ^r$(keep.id) is not owned by %$(owner.id)")
    isempty(keepreg.args) || error("inline_region!: cannot inline a region with region args")
    term = region_terminator(ir, keep)
    parent = stmt_region(ir, owner)
    # move stmts (except a result terminator) into parent before owner's successor
    members = region_stmts(ir, keep)
    resultvals = Operand[]
    if term !== nothing && stmt_kind(ir, term) === K"result"
        resultvals = operands(ir, term)
        kill_stmt!(ir, term)
        members = members[1:end-1]
    end
    e = ir.edit::EditState
    anchor = owner
    for s in members
        unlink!(ir, s)
        nxt = e.next[anchor.id]
        link_between!(ir, s, parent, anchor.id, nxt)
        assign_okey!(ir, s, deep_last(ir, anchor), nxt == 0 ? flat_next(ir, s) : StmtId(nxt))
        for rid in owned_regions(ir, s)
            getregion(ir, rid).parent = parent
        end
        anchor = s
    end
    keepreg.first = NULL_STMT
    keepreg.last = NULL_STMT
    keepreg.dead = true
    # rewrite result uses
    if length(resultvals) == 1
        replace_uses_where!(_ -> true, ir, owner => resultvals[1])
    elseif length(resultvals) > 1
        # tuple result: rewrite extract users; other users are an error (v1)
        for i in 1:Int(ir.body.len)
            ir.body.kind[i] === K"extract" || continue
            s = StmtId(i)
            v = getop(ir, s, 1)
            optag(v) == TAG_STMT && asstmt(v) == owner || continue
            idx = Int(imm_value(getop(ir, s, 2))::Int64)
            replace_stmt!(ir, s, K"refine", resultvals[idx]; type = stmt_type(ir, s))
        end
        counts = use_counts(ir)
        counts[owner.id] == 0 ||
            error("inline_region!: tuple result of %$(owner.id) escapes beyond extracts")
    end
    # kill remaining regions and the owner itself
    kill_stmt!(ir, owner)
    ir.cache.region_epoch += 1
    return ir
end

# ---------------------------------------------------------------------------
# splice_body!: the library-owned inlining primitive (§4.2)
# ---------------------------------------------------------------------------

"""
    splice_body!(ir, at, callee::IR; argmap, sparams=Any[]) -> Operand

Replace statement `at` with a copy of `callee`'s body. `argmap[i]` supplies
the operand for the callee's i-th function parameter (region 1 arg). The
callee must be dense/sealed, single-`return` (inliner normalizes first), and
of the same column universe (`convert_universe` otherwise — hard error).
Returns the operand that replaced `at`'s uses. Worlds are intersected;
constants/globals relocated; `on_splice!` hooks fire per column.
"""
function splice_body!(ir::IR, at::StmtId, callee::IR; argmap::Vector{Operand},
                      sparams::Vector{Any} = Any[])
    check_state(ir, LAYOUT_EDITABLE, "splice_body!")
    layout(callee) === LAYOUT_DENSE || error("splice_body!: callee must be dense/sealed")
    typeof(ir.body.cols) === typeof(callee.body.cols) ||
        error("splice_body!: column universes differ; convert_universe the callee first (§3.5)")
    croot = getregion(callee, root_region(callee))
    length(argmap) == length(croot.args) ||
        error("splice_body!: argmap length $(length(argmap)) != callee params $(length(croot.args))")
    # single-return check + collect
    nret = 0
    retstmt = NULL_STMT
    for i in 1:Int(callee.body.len)
        callee.body.kind[i] === K"return" || continue
        nret += 1
        retstmt = StmtId(i)
    end
    nret <= 1 || error("splice_body!: callee has $nret returns; normalize to one first (v1)")
    (nret == 1 && stmt_region(callee, retstmt) == root_region(callee)) ||
        nret == 0 || error("splice_body!: early return in callee (v1 requires root-level return)")

    stmtmap = Dict{Int32,Operand}()   # callee stmt -> caller operand
    for (i, a) in enumerate(croot.args)
        stmtmap[a.id] = argmap[i]
    end
    regionmap = Dict{Int32,RegionId}(1 => stmt_region(ir, at))

    function remap_op(o::Operand)::Operand
        t = optag(o)
        if t == TAG_STMT
            r = get(stmtmap, asstmt(o).id, nothing)
            r === nothing && error("splice_body!: forward or unmapped reference %$(payload(o)) in callee")
            return r
        elseif t == TAG_CONST
            return op_constidx(intern_const!(ir.body, callee.body.constants[payload(o)]))
        elseif t == TAG_GLOBAL
            return op_globalidx(intern_global!(ir.body, callee.body.globals[payload(o)]))
        elseif t == TAG_SPARAM
            idx = Int(payload(o))
            idx <= length(sparams) || error("splice_body!: unsubstituted static parameter $idx")
            return vop(ir, sparams[idx])
        elseif t == TAG_REGION || t == TAG_BLOCK
            rid = get(regionmap, Int32(payload(o)), nothing)
            rid === nothing && error("splice_body!: region reference before its mapping")
            return mkoperand(t, rid.id)
        else
            return o
        end
    end

    retop = Operand(OP_NONE.bits)
    e = ir.edit::EditState

    # copy region subtree rooted at callee region `cr`, linking stmts into
    # caller region `dest` after anchor (anchor==at means "before at")
    function copy_region_into!(cr::RegionId, dest::RegionId, anchor::StmtId)
        for s in region_stmts(callee, cr)
            k = callee.body.kind[s.id]
            if k === K"region_arg" && cr == root_region(callee)
                continue  # mapped to argmap
            end
            if k === K"return" && cr == root_region(callee)
                retop = length(operands(callee, s)) == 1 ?
                    remap_op(getop(callee, s, 1)) : Operand(OP_NONE.bits)
                continue
            end
            ops = Operand[remap_op(getop(callee, s, i)) for i in 1:nops(callee, s)]
            # pre-create owned regions so REGION operands of nested exits resolve:
            # (exits reference ancestor regions, which are created before descent)
            new = alloc_stmt!(ir, k, ops; type = callee.body.type[s.id],
                              flag = callee.body.flag[s.id],
                              debug = callee.body.debug[s.id])
            # place after anchor in dest
            if anchor == at
                link_between!(ir, new, dest, e.prev[at.id], at.id)
                assign_okey!(ir, new, flat_prev_of_new(ir, new), at)
            else
                nxt = e.next[anchor.id]
                link_between!(ir, new, dest, anchor.id, nxt)
                assign_okey!(ir, new, deep_last(ir, anchor),
                             nxt == 0 ? flat_next(ir, new) : StmtId(nxt))
            end
            stmtmap[s.id] = op_stmt(new)
            # copy extension column values
            copy_splice_cols!(ir.body.cols, callee.body.cols, s.id, new.id)
            if k === K"region_arg"
                push!(getregion(ir, dest).args, new)
            end
            # recurse into owned regions — pre-create ALL siblings first so
            # BLOCK/REGION operands between sibling regions (br_if edges of a
            # cfg island) resolve during the copy
            if owns_regions(k)
                crids = owned_regions(callee, s)
                for crid in crids
                    creg = getregion(callee, crid)
                    nr = Region(creg.kind, new, dest; activation = creg.activation)
                    push!(ir.regions, nr)
                    regionmap[crid.id] = RegionId(length(ir.regions))
                end
                for crid in crids
                    copy_region_into_fresh!(crid, regionmap[crid.id])
                end
            end
            anchor = new
        end
        return anchor
    end

    # copy a callee region's contents into a fresh (empty) caller region
    function copy_region_into_fresh!(cr::RegionId, dest::RegionId)
        destreg = getregion(ir, dest)
        for s in region_stmts(callee, cr)
            k = callee.body.kind[s.id]
            ops = Operand[remap_op(getop(callee, s, i)) for i in 1:nops(callee, s)]
            new = alloc_stmt!(ir, k, ops; type = callee.body.type[s.id],
                              flag = callee.body.flag[s.id],
                              debug = callee.body.debug[s.id])
            tail = destreg.last.id
            link_between!(ir, new, dest, tail, Int32(0))
            lo = tail != 0 ? deep_last(ir, StmtId(tail)) : region_entry_anchor(ir, dest)
            assign_okey!(ir, new, lo, flat_next(ir, new))
            stmtmap[s.id] = op_stmt(new)
            copy_splice_cols!(ir.body.cols, callee.body.cols, s.id, new.id)
            k === K"region_arg" && push!(destreg.args, new)
            if owns_regions(k)
                crids = owned_regions(callee, s)
                for crid in crids
                    creg = getregion(callee, crid)
                    nr = Region(creg.kind, new, dest; activation = creg.activation)
                    push!(ir.regions, nr)
                    regionmap[crid.id] = RegionId(length(ir.regions))
                end
                for crid in crids
                    copy_region_into_fresh!(crid, regionmap[crid.id])
                end
            end
        end
        return nothing
    end

    copy_region_into!(root_region(callee), stmt_region(ir, at), at)

    # intersect world validity
    lo = max(ir.valid_worlds[1], callee.valid_worlds[1])
    hi = min(ir.valid_worlds[2], callee.valid_worlds[2])
    ir.valid_worlds = (lo, hi)

    # replace uses of `at` with the return operand and remove it
    if optag(retop) != TAG_NONE
        replace_uses_where!(_ -> true, ir, at => retop)
    end
    kill_stmt!(ir, at)
    ir.cache.region_epoch += 1
    ir.cache.stmt_epoch += 1
    invalidate_derived!(ir.body.cols)
    return retop
end

"Row read for the splice copy hook: `nothing` = absent. Defaults to
`getindex` (sparse columns return the sentinel); strict Dict-shaped columns
(ProvenanceCol) override to miss through `get`."
splice_read(c, i::Int) = c[i]
splice_read(c::ProvenanceCol, i::Int) = get(c, i, nothing)

function copy_splice_cols!(dst, src, oldid::Int32, newid::Int32)
    dst isa NamedTuple && src isa NamedTuple || return
    for name in keys(src)
        name in keys(dst) || continue
        c = src[name]; d = dst[name]
        # `nothing` is the universal absent-row sentinel of the default copy
        # hook (sparse columns read it for missing rows; strict Dict-shaped
        # columns override `splice_read`); columns that need richer transfer
        # implement `on_splice!`.
        v = splice_read(c, Int(oldid))
        v === nothing && continue
        d[Int(newid)] = v
        on_splice!(d, oldid, newid)
    end
    return nothing
end
