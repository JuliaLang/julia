# compact!: one of the two renaming points (§2.2). Drops tombstones, orphaned
# pool/constant/global slots and dead regions; renumbers densely in region
# order; rewrites every reference reachable through the §3.2 protocol; bumps
# the generation; returns the RemapSet.

"""
    compact!(ir) -> (ir, RemapSet)

Transition editable → dense (also legal from dense, dropping tombstones).
"""
function compact!(ir::IR)
    check_state(ir, (LAYOUT_EDITABLE, LAYOUT_DENSE), "compact!")
    flush_renames!(ir)
    any(r -> is_guard(r) && !r.dead, ir.regions) &&
        error("compact! does not order guard regions; use schedule! (§4.3)")
    body = ir.body
    n = Int(body.len)

    old_of_new = Int32[]              # newid -> oldid
    stmt_map = zeros(Int32, n)        # oldid -> newid
    region_old_of_new = Int32[1]      # new region id -> old region id (root first)
    region_map = zeros(Int32, length(ir.regions))
    region_map[1] = 1
    newregions = Region[]
    rootreg = getregion(ir, root_region(ir))
    newroot = Region(rootreg.kind, rootreg.activation, NULL_STMT, NULL_REGION,
                     StmtId[], NULL_STMT, false, NULL_STMT, NULL_STMT, false)
    push!(newregions, newroot)

    # Pass 1: traversal order (owner-driven region tree walk)
    function emit_region!(oldr::RegionId, newr::Int32)
        nr = newregions[newr]
        firstid = length(old_of_new) + 1
        for s in region_stmts(ir, oldr)
            k = body.kind[s.id]
            k === KIND_DELETED && continue
            push!(old_of_new, s.id)
            newid = Int32(length(old_of_new))
            stmt_map[s.id] = newid
            if k === K"region_arg"
                push!(nr.args, StmtId(newid))
            end
            if owns_regions(k)
                for rid in owned_regions(ir, s)
                    reg = getregion(ir, rid)
                    reg.dead && continue
                    child = Region(reg.kind, reg.activation, StmtId(newid),
                                   RegionId(newr), StmtId[], NULL_STMT, reg.negated,
                                   NULL_STMT, NULL_STMT, false)
                    push!(newregions, child)
                    cid = Int32(length(newregions))
                    push!(region_old_of_new, rid.id)
                    region_map[rid.id] = cid
                    emit_region!(rid, cid)
                end
            end
        end
        nr.first = StmtId(firstid)
        nr.last = StmtId(length(old_of_new))
        if nr.last.id < nr.first.id   # empty region
            nr.first = NULL_STMT
            nr.last = NULL_STMT
        end
        return nothing
    end
    emit_region!(root_region(ir), Int32(1))

    nn = length(old_of_new)
    # Pass 2: rebuild columns and pools with remapping
    newkind = Vector{Kind}(undef, nn)
    newops = Vector{UInt64}(undef, nn)
    newtype = Vector{Any}(undef, nn)
    newflag = Vector{UInt32}(undef, nn)
    newdebug = Vector{NTuple{3,Int32}}(undef, nn)
    newregion_col = Vector{RegionId}(undef, nn)
    newpool = Operand[]
    newconsts = Any[]
    newconstmap = IdDict{Any,Int}()
    newglobals = GlobalRef[]
    newglobalmap = Dict{GlobalRef,Int}()
    const_map = zeros(Int32, length(body.constants))
    global_map = zeros(Int32, length(body.globals))

    function remap_word(o::Operand)::Operand
        t = optag(o)
        if t == TAG_STMT
            m = stmt_map[payload(o)]
            m == 0 && error("compact!: live reference to dropped statement %$(payload(o))")
            return op_stmt(StmtId(m))
        elseif t == TAG_REGION || t == TAG_BLOCK
            m = region_map[payload(o)]
            m == 0 && error("compact!: live reference to dropped region ^r$(payload(o))")
            return mkoperand(t, m)
        elseif t == TAG_CONST
            v = body.constants[payload(o)]
            ni = get!(newconstmap, v) do
                push!(newconsts, v); length(newconsts)
            end
            const_map[payload(o)] = ni
            return op_constidx(ni)
        elseif t == TAG_GLOBAL
            g = body.globals[payload(o)]
            ni = get!(newglobalmap, g) do
                push!(newglobals, g); length(newglobals)
            end
            global_map[payload(o)] = ni
            return op_globalidx(ni)
        else
            return o
        end
    end

    for newid in 1:nn
        old = old_of_new[newid]
        s = StmtId(old)
        k = body.kind[old]
        newkind[newid] = k
        newtype[newid] = body.type[old]
        newflag[newid] = body.flag[old]
        newdebug[newid] = body.debug[old]
        w = body.ops[old]
        if is_ops_inline(w)
            m = stmt_map[inline_stmt(w).id]
            m == 0 && error("compact!: inline reference to dropped statement")
            newops[newid] = set_inline_stmt(w, StmtId(m))
        else
            # shared substrate pool-rebuild (same helper compact_graph! uses)
            newops[newid] = append_remapped_range!(newpool, body.operands, w, remap_word)
        end
    end
    for newid in 1:nn
        newregion_col[newid] = RegionId(0)
    end
    # region column: fill from new region spans (direct members get their
    # region; nested handled since every stmt is emitted by exactly one region)
    for (ri, reg) in enumerate(newregions)
        # will be overwritten by deeper regions for nested spans; instead
        # record during emission — recompute directly:
        _ = ri; _ = reg
    end
    # recompute region column by re-walking: statement old region → new region
    for newid in 1:nn
        old = old_of_new[newid]
        newregion_col[newid] = RegionId(region_map[body.region[old].id])
    end

    rs = RemapSet(stmt_map, region_map, const_map, global_map)
    # Column hooks may throw; stage them before publishing anything so a
    # failing callback leaves the IR logically unchanged (§4.1).
    newcols = compact_cols_staged(body.cols, old_of_new, rs)

    body.len = Int32(nn)
    body.kind = newkind
    body.ops = newops
    body.type = newtype
    body.flag = newflag
    body.debug = newdebug
    body.region = newregion_col
    body.operands = newpool
    body.constants = newconsts
    body.constmap = newconstmap
    body.globals = newglobals
    body.globalmap = newglobalmap
    body.cols = newcols

    resize!(ir.regions, 0)
    append!(ir.regions, newregions)

    ir.edit = nothing
    ir.owner.state = LAYOUT_DENSE
    ir.owner.generation += 1
    empty!(ir.cache.entries)          # v1: drop analyses (update! protocol later)
    ir.cache.layout_epoch += 1
    ir.cache.stmt_epoch += 1
    ir.cache.region_epoch += 1
    return (ir, rs)
end
