# Dense-state mutation (§4.1): footprint-preserving replacement, restricted
# tombstone deletion, queued renames.

"""
    replace_stmt!(ir, id, kind, ops...; type=keep, flag=default) -> StmtId

Same identity, same footprint: result arity, terminator-ness, and
region-ownership must be preserved. Derived-class columns are conservatively
invalidated (§3.5).
"""
function replace_stmt!(ir::IR, id::StmtId, k::Kind, args...;
                       type = stmt_type(ir, id), flag::Union{Nothing,UInt32} = nothing)
    check_state(ir, (LAYOUT_DENSE, LAYOUT_EDITABLE), "replace_stmt!")
    old = stmt_kind(ir, id)
    oldinfo, newinfo = kindinfo(old), kindinfo(k)
    # footprint checks
    (oldinfo.result == 0) == (newinfo.result == 0) ||
        error("replace_stmt!: result-arity footprint change ($(oldinfo.qualified) -> $(newinfo.qualified))")
    oldinfo.is_terminator == newinfo.is_terminator ||
        error("replace_stmt!: terminator footprint change")
    oldinfo.owns_regions == newinfo.owns_regions ||
        error("replace_stmt!: region-ownership footprint change (use editable surgery)")
    ops = Operand[a isa Operand ? a : vop(ir, a) for a in args]
    check_arity(newinfo, length(ops))
    body = ir.body
    # reuse the pool range when the new operands fit (same-or-smaller arity)
    w = body.ops[id.id]
    if !is_ops_inline(w) && !has_inline_ops(k) && length(ops) <= ops_len(w)
        off = ops_offset(w)
        for (i, o) in enumerate(ops)
            body.operands[off + i] = o
        end
        body.ops[id.id] = ops_pool(off, length(ops))
    else
        body.ops[id.id] = encode_ops!(body, k, ops)
    end
    body.kind[id.id] = k
    body.type[id.id] = type
    body.flag[id.id] = flag === nothing ? newinfo.effects : flag
    ir.cache.stmt_epoch += 1
    invalidate_derived!(body.cols)
    return id
end

"""
    delete_stmt!(ir, id)

Tombstone a **plain** statement (§4.1): non-owner, non-terminator,
non-`region_arg`, not a guard-region condition. Uses must be gone by verify
time.
"""
function delete_stmt!(ir::IR, id::StmtId)
    check_state(ir, (LAYOUT_DENSE, LAYOUT_EDITABLE), "delete_stmt!")
    k = stmt_kind(ir, id)
    k === KIND_DELETED && return id
    owns_regions(k) && error("delete_stmt!: %$(id.id) owns regions (use editable surgery)")
    is_terminator(k) && error("delete_stmt!: %$(id.id) is a terminator")
    k === K"region_arg" && error("delete_stmt!: %$(id.id) is a region_arg")
    for reg in ir.regions
        is_guard(reg) && reg.cond == id &&
            error("delete_stmt!: %$(id.id) is a guard-region condition")
    end
    body = ir.body
    body.kind[id.id] = KIND_DELETED
    body.ops[id.id] = OPS_EMPTY
    body.type[id.id] = nothing
    body.flag[id.id] = FLAG_PURE
    if layout(ir) === LAYOUT_EDITABLE
        unlink!(ir, id)
    end
    ir.cache.stmt_epoch += 1
    return id
end

"""
    replace_uses!(ir, old => new)

Queue a rename of every `ssa_use` of `old` to `new` (a `StmtId` or value
`Operand`); flushed in one O(n) sweep by `flush_renames!` (or at pass end /
compact!). Renames compose; chains collapse; cycles error.
"""
function replace_uses!(ir::IR, p::Pair{StmtId,<:Union{StmtId,Operand}})
    new = p.second isa StmtId ? op_stmt(p.second) : p.second
    push!(ir.pending, p.first => new)
    return ir
end

function flush_renames!(ir::IR)
    isempty(ir.pending) && return ir
    # build resolved map with chain collapsing
    m = Dict{Int32,Operand}()
    for (old, new) in ir.pending
        m[old.id] = new
    end
    function resolve(o::Operand, seen::Vector{Int32})
        while optag(o) == TAG_STMT && haskey(m, asstmt(o).id)
            sid = asstmt(o).id
            sid in seen && error("replace_uses!: rename cycle through %$sid")
            push!(seen, sid)
            o = m[sid]
        end
        return o
    end
    for k in collect(keys(m))
        m[k] = resolve(m[k], Int32[k])
    end
    body = ir.body
    for i in 1:Int(body.len)
        body.kind[i] === KIND_DELETED && continue
        s = StmtId(i)
        w = body.ops[i]
        if is_ops_inline(w)
            r = get(m, inline_stmt(w).id, nothing)
            if r !== nothing
                if optag(r) == TAG_STMT
                    body.ops[i] = set_inline_stmt(w, asstmt(r))
                else
                    # replacement is not a statement: fall back to pool encoding
                    ops = operands(ir, s)
                    ops[1] = r
                    off = length(body.operands)
                    append!(body.operands, ops)
                    body.ops[i] = ops_pool(off, length(ops))
                end
            end
        else
            n = Int(ops_len(w))
            off = ops_offset(w)
            for j in 1:n
                o = body.operands[off + j]
                if optag(o) == TAG_STMT
                    r = get(m, asstmt(o).id, nothing)
                    r === nothing || (body.operands[off + j] = r)
                end
            end
        end
    end
    # guard conditions participate (§3.2: they are ssa_uses)
    for reg in ir.regions
        if is_guard(reg) && !isnull(reg.cond)
            r = get(m, reg.cond.id, nothing)
            if r !== nothing
                optag(r) == TAG_STMT || error("guard condition must remain a statement reference")
                reg.cond = asstmt(r)
            end
        end
    end
    empty!(ir.pending)
    ir.cache.stmt_epoch += 1
    return ir
end
