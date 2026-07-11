# Statement iteration and views.

"""
    each_stmt(ir) -> iterator of StmtId

Flat iteration over all live statements in layout order (dense/editable).
Deleting the cursor statement is safe; statements inserted after the cursor
are visited; before it, not (§4.2 contract).
"""
function each_stmt(ir::IR)
    if layout(ir) === LAYOUT_EDITABLE
        return EditIter(ir)
    else
        return (StmtId(i) for i in 1:Int(ir.body.len) if ir.body.kind[i] !== KIND_DELETED)
    end
end

struct EditIter
    ir::IR
end
function Base.iterate(it::EditIter)
    ir = it.ir
    s = flat_first(ir)
    s === nothing && return nothing
    return (s, s)
end
function Base.iterate(it::EditIter, cur::StmtId)
    s = flat_next(it.ir, cur)
    while s !== nothing && is_tombstone(it.ir, s)
        s = flat_next(it.ir, s)
    end
    s === nothing && return nothing
    return (s, s)
end
Base.IteratorSize(::Type{EditIter}) = Base.SizeUnknown()
Base.eltype(::Type{EditIter}) = StmtId

"""
    region_stmts(ir, r) -> Vector{StmtId}

Direct member statements of region `r` (excluding statements of nested
regions), in order.
"""
function region_stmts(ir::IR, r::RegionId)
    out = StmtId[]
    if layout(ir) === LAYOUT_EDITABLE
        e = ir.edit::EditState
        i = getregion(ir, r).first.id
        while i != 0
            ir.body.kind[i] === KIND_DELETED || push!(out, StmtId(i))
            i = e.next[i]
        end
    else
        reg = getregion(ir, r)
        i = reg.first.id
        while 0 < i <= reg.last.id
            if ir.body.region[i] == r
                ir.body.kind[i] === KIND_DELETED || push!(out, StmtId(i))
                if owns_regions(ir.body.kind[i]) && ir.body.kind[i] !== KIND_DELETED
                    # skip owned spans
                    rs = owned_regions(ir, StmtId(i))
                    isempty(rs) || (i = getregion(ir, rs[end]).last.id)
                end
            end
            i += 1
        end
    end
    return out
end

"The terminator of an ordered region (last live direct member), or nothing."
function region_terminator(ir::IR, r::RegionId)
    ss = region_stmts(ir, r)
    isempty(ss) && return nothing
    t = ss[end]
    return is_terminator(stmt_kind(ir, t)) ? t : nothing
end

# ---- editable-state flattened traversal helpers ----

"First statement in flattened order (editable state)."
function flat_first(ir::IR)
    reg = getregion(ir, root_region(ir))
    reg.first.id == 0 ? nothing : StmtId(reg.first.id)
end

"""
    flat_next(ir, s) -> StmtId | nothing

Successor of `s` in the flattened order (editable state): descends into owned
regions, then continues the region list, then pops to the parent's
continuation.
"""
function flat_next(ir::IR, s::StmtId)
    e = ir.edit::EditState
    # descend into first owned region
    if owns_regions(stmt_kind(ir, s)) && !is_tombstone(ir, s)
        rs = owned_regions(ir, s)
        for rid in rs
            f = getregion(ir, rid).first
            f.id != 0 && return StmtId(f.id)
        end
    end
    # continue within the region list, else pop upward
    i = s
    while true
        nxt = e.next[i.id]
        nxt != 0 && return StmtId(nxt)
        reg = getregion(ir, stmt_region(ir, i))
        owner = reg.owner
        isnull(owner) && return nothing   # end of root
        # next sibling owned region of the same owner?
        rs = owned_regions(ir, owner)
        idx = findfirst(==(stmt_region(ir, i)), rs)
        for j in (idx+1):length(rs)
            f = getregion(ir, rs[j]).first
            f.id != 0 && return StmtId(f.id)
        end
        i = owner
    end
end

"Predecessor of `s` in flattened order (editable state)."
function flat_prev(ir::IR, s::StmtId)
    e = ir.edit::EditState
    p = e.prev[s.id]
    if p == 0
        reg = getregion(ir, stmt_region(ir, s))
        owner = reg.owner
        isnull(owner) && return nothing
        rs = owned_regions(ir, owner)
        idx = findfirst(==(stmt_region(ir, s)), rs)
        for j in (idx-1):-1:1
            t = getregion(ir, rs[j]).last
            t.id != 0 && return deep_last(ir, StmtId(t.id))
        end
        return owner
    end
    return deep_last(ir, StmtId(p))
end

"Deepest last statement of the subtree rooted at s (s itself if no regions)."
function deep_last(ir::IR, s::StmtId)
    while owns_regions(stmt_kind(ir, s)) && !is_tombstone(ir, s)
        rs = owned_regions(ir, s)
        found = false
        for j in length(rs):-1:1
            t = getregion(ir, rs[j]).last
            if t.id != 0
                s = StmtId(t.id)
                found = true
                break
            end
        end
        found || break
    end
    return s
end
