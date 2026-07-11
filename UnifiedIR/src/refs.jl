# The universal reference protocol (§3.2): every semantic reference in the
# structure participates in visit_refs/remap_refs with a role; renaming points
# produce a RemapSet over all compactable namespaces.

@enum RefRole::UInt8 REF_SSA_USE REF_OWNER_LINK REF_ARG_DEF REF_CONTROL_TARGET REF_LAYOUT_ANCHOR

"A use site: statement operand or ownerless-guard condition (§3.2)."
abstract type UseSite end
struct StmtOperand <: UseSite
    user::StmtId
    opidx::Int32
end
struct GuardCondition <: UseSite
    region::RegionId
end

"""
    RemapSet

Renaming-point output (§2.2): old→new maps over all compactable namespaces.
Index oldid → newid; 0 = dropped.
"""
struct RemapSet
    stmt::Vector{Int32}
    region::Vector{Int32}
    konst::Vector{Int32}
    global_::Vector{Int32}
end

remap(rs::RemapSet, s::StmtId) = StmtId(s.id == 0 ? Int32(0) : rs.stmt[s.id])
remap(rs::RemapSet, r::RegionId) = RegionId(r.id == 0 ? Int32(0) : rs.region[r.id])

function remap(rs::RemapSet, o::Operand)::Operand
    t = optag(o)
    if t == TAG_STMT
        n = rs.stmt[payload(o)]
        n == 0 && error("remap: reference to dropped statement %$(payload(o))")
        return op_stmt(StmtId(n))
    elseif t == TAG_BLOCK || t == TAG_REGION
        n = rs.region[payload(o)]
        n == 0 && error("remap: reference to dropped region ^r$(payload(o))")
        return mkoperand(t, n)
    elseif t == TAG_CONST
        n = rs.konst[payload(o)]
        n == 0 && error("remap: reference to dropped constant")
        return op_constidx(n)
    elseif t == TAG_GLOBAL
        n = rs.global_[payload(o)]
        n == 0 && error("remap: reference to dropped global")
        return op_globalidx(n)
    else
        return o
    end
end

"""
    visit_refs(f, ir; roles=all)

Visit every reference in the structure: `f(role, site, operand_or_id)`.
For statement operands, `site::StmtOperand`; for guard conditions,
`site::GuardCondition`; for region-table links, `site::RegionId`.
"""
function visit_refs(f, ir::IR)
    body = ir.body
    for i in 1:Int(body.len)
        body.kind[i] === KIND_DELETED && continue
        s = StmtId(i)
        info = kindinfo(body.kind[i])
        n = nops(ir, s)
        for j in 1:n
            o = getop(ir, s, j)
            t = optag(o)
            if t == TAG_STMT || t == TAG_CONST || t == TAG_GLOBAL || t == TAG_SPARAM || t == TAG_INLINE
                role = REF_SSA_USE
                f(role, StmtOperand(s, Int32(j)), o)
            elseif t == TAG_REGION || t == TAG_BLOCK
                f(REF_CONTROL_TARGET, StmtOperand(s, Int32(j)), o)
            end
        end
        _ = info
    end
    for (ri, reg) in enumerate(ir.regions)
        rid = RegionId(ri)
        if is_guard(reg) && !isnull(reg.cond)
            f(REF_SSA_USE, GuardCondition(rid), op_stmt(reg.cond))
        end
        isnull(reg.owner) || f(REF_OWNER_LINK, rid, op_stmt(reg.owner))
        for a in reg.args
            f(REF_ARG_DEF, rid, op_stmt(a))
        end
        # spans are layout anchors; remapping handles them internally
    end
    return nothing
end

"Iterate the `ssa_use`-role uses of statements: f(use_site, used_stmt)."
function each_ssa_use(f, ir::IR)
    visit_refs(ir) do role, site, o
        role === REF_SSA_USE || return
        optag(o) == TAG_STMT || return
        f(site, asstmt(o))
    end
end

"Compute use counts over the ssa_use role only (§3.2)."
function use_counts(ir::IR)
    counts = zeros(Int32, Int(ir.body.len))
    each_ssa_use(ir) do _, used
        counts[used.id] += 1
    end
    return counts
end
