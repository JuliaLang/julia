# Analysis cache (§11.1): epoch-guarded memoization keyed by
# (analysis type, config). v1: epoch-checked recompute-on-miss; the
# `update!(a, ir, event)` absorption protocol is a later refinement.

struct AnalysisEntry
    value::Any
    stmt_epoch::UInt64
    region_epoch::UInt64
    type_epoch::UInt64
    flag_epoch::UInt64
    layout_epoch::UInt64
end

"""
    get_analysis!(compute, ir, key; deps=(:stmt,:region,:type,:flag,:layout))

Memoize `compute(ir)` under `key`, invalidated when any of the declared
epoch dependencies has moved.
"""
function get_analysis!(compute, ir::IR, key;
                       deps::Tuple{Vararg{Symbol}} = (:stmt, :region, :type, :flag, :layout))
    c = ir.cache
    e = get(c.entries, key, nothing)
    if e isa AnalysisEntry
        ok = true
        (:stmt in deps) && e.stmt_epoch != c.stmt_epoch && (ok = false)
        (:region in deps) && e.region_epoch != c.region_epoch && (ok = false)
        (:type in deps) && e.type_epoch != c.type_epoch && (ok = false)
        (:flag in deps) && e.flag_epoch != c.flag_epoch && (ok = false)
        (:layout in deps) && e.layout_epoch != c.layout_epoch && (ok = false)
        ok && return e.value
    end
    v = compute(ir)
    c.entries[key] = AnalysisEntry(v, c.stmt_epoch, c.region_epoch, c.type_epoch,
                                   c.flag_epoch, c.layout_epoch)
    return v
end

"Use counts over the ssa_use role (standard entry)."
cached_use_counts(ir::IR) =
    get_analysis!(use_counts, ir, :use_counts; deps = (:stmt, :layout))

"""
    ExitIndex

The §5.9 reverse index: for each region-owning statement, the exit
terminators feeding its results.
"""
function exit_index(ir::IR)
    get_analysis!(ir, :exit_index; deps = (:stmt, :region, :layout)) do ir
        idx = Dict{StmtId,Vector{StmtId}}()
        for s in each_stmt(ir)
            k = stmt_kind(ir, s)
            if k === K"break" || k === K"continue"
                tgt = asregion(getop(ir, s, 1))
                owner = getregion(ir, tgt).owner
                isnull(owner) || push!(get!(() -> StmtId[], idx, owner), s)
            elseif k === K"yield"
                reg = getregion(ir, stmt_region(ir, s))
                isnull(reg.owner) || push!(get!(() -> StmtId[], idx, reg.owner), s)
            end
        end
        idx
    end
end
