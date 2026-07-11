# Floating state (§4.3): no order; the equation/dataflow dialect's layout.
# `schedule!` is the floating→dense transition and the second renaming point.

struct CausalityError <: Exception
    cycle::Vector{StmtId}
end
Base.showerror(io::IO, e::CausalityError) =
    print(io, "CausalityError: instantaneous cycle through ", join(e.cycle, " → "))

"Is this statement reorderable (§4.3): REMOVABLE mask, or dialect-declared?"
function reorderable(ir::IR, s::StmtId)
    k = stmt_kind(ir, s)
    is_delay_kind(k) && return true         # delay cuts its own data edge
    k === K"region_arg" && return true
    f = stmt_flag(ir, s)
    return f & FLAG_REMOVABLE == FLAG_REMOVABLE
end

"""
    float!(ir) -> ir

Transition dense → floating. Legal iff every statement is reorderable and
control structure is guard-only (§4.3).
"""
function float!(ir::IR)
    check_state(ir, LAYOUT_DENSE, "float!")
    flush_renames!(ir)
    for s in each_stmt(ir)
        reorderable(ir, s) ||
            error("float!: %$(s.id) ($(kindname(stmt_kind(ir, s)))) is not reorderable (pure-but-throwing or potentially nonterminating operations may not float)")
        owns_regions(stmt_kind(ir, s)) &&
            error("float!: %$(s.id) owns regions; floating control structure is guard-only")
        is_terminator(stmt_kind(ir, s)) &&
            error("float!: %$(s.id) is a terminator")
    end
    ir.owner.state = LAYOUT_FLOATING
    ir.meta[:floating_node] = true
    ir.cache.layout_epoch += 1
    return ir
end

"""
    schedule!(ir; strategy=:asap) -> (ir, RemapSet)

Floating → dense: a topological sort over operand edges (delay data edges
cut) with deterministic tie-breaking, region-grouped emission, ending in a
compaction. Layout only — guard reification, merge/delay legalization are
separate passes (§4.3). Unbreakable cycles throw `CausalityError`.
"""
function schedule!(ir::IR; strategy::Symbol = :asap)
    check_state(ir, LAYOUT_FLOATING, "schedule!")
    body = ir.body
    n = Int(body.len)

    # dependency edges: def -> user (delay's data operand cut)
    deps = [Int32[] for _ in 1:n]      # deps[user] = defs it waits on
    for i in 1:n
        body.kind[i] === KIND_DELETED && continue
        s = StmtId(i)
        k = body.kind[i]
        cut = is_delay_kind(k) ? 1 : 0
        for j in 1:nops(ir, s)
            j == cut && continue
            o = getop(ir, s, j)
            optag(o) == TAG_STMT && push!(deps[i], Int32(payload(o)))
        end
        # guard condition of the stmt's region chain is a dependency
        r = body.region[i]
        while !isnull(r)
            reg = getregion(ir, r)
            is_guard(reg) && !isnull(reg.cond) && push!(deps[i], reg.cond.id)
            r = reg.parent
        end
    end

    # region-grouped scheduling: recursively schedule each region's units
    # (member stmts + child regions), topologically by external deps.
    children = Dict{Int32,Vector{Int32}}()
    for (ri, reg) in enumerate(ir.regions)
        reg.dead && continue
        isnull(reg.parent) && continue
        push!(get!(() -> Int32[], children, reg.parent.id), Int32(ri))
    end
    members = Dict{Int32,Vector{Int32}}()
    for i in 1:n
        body.kind[i] === KIND_DELETED && continue
        push!(get!(() -> Int32[], members, body.region[i].id), Int32(i))
    end

    # transitive stmt set per region (for unit-level dep aggregation)
    function region_stmtset(ri::Int32)
        out = Int32[]
        stack = [ri]
        while !isempty(stack)
            r = pop!(stack)
            append!(out, get(members, r, Int32[]))
            append!(stack, get(children, r, Int32[]))
        end
        return out
    end

    order = Int32[]                     # emitted old ids, in new order
    spans = Dict{Int32,Tuple{Int,Int}}() # old region id -> (first,last) new pos

    function schedule_region!(ri::Int32)
        firstpos = length(order) + 1
        # units: (:stmt, id) or (:region, rid)
        units = Vector{Tuple{Symbol,Int32}}()
        for i in get(members, ri, Int32[])
            push!(units, (:stmt, i))
        end
        for r in get(children, ri, Int32[])
            push!(units, (:region, r))
        end
        # unit deps: stmt ids outside the unit
        unit_of_stmt = Dict{Int32,Int}()
        stmtsets = Dict{Int,Vector{Int32}}()
        for (ui, (t, id)) in enumerate(units)
            ss = t === :stmt ? Int32[id] : region_stmtset(id)
            stmtsets[ui] = ss
            for s in ss
                unit_of_stmt[s] = ui
            end
        end
        pending = Dict{Int,Set{Int}}()  # unit -> units it waits on (within this region)
        rdeps = Dict{Int,Vector{Int}}()
        for (ui, ss) in stmtsets
            waits = Set{Int}()
            for s in ss, d in deps[s]
                du = get(unit_of_stmt, d, 0)
                (du != 0 && du != ui) && push!(waits, du)
            end
            pending[ui] = waits
        end
        for (ui, waits) in pending, w in waits
            push!(get!(() -> Int[], rdeps, w), ui)
        end
        # Kahn with deterministic tie-break (smallest original id first)
        ready = sort([ui for (ui, w) in pending if isempty(w)];
                     by = ui -> minimum(stmtsets[ui]; init = Int32(typemax(Int32))))
        emitted = 0
        while !isempty(ready)
            ui = popfirst!(ready)
            emitted += 1
            t, id = units[ui]
            if t === :stmt
                push!(order, id)
            else
                schedule_region!(id)
            end
            for w in get(rdeps, ui, Int[])
                delete!(pending[w], ui)
                if isempty(pending[w])
                    # insert keeping deterministic order
                    pos = searchsortedfirst(ready, w;
                        by = x -> minimum(stmtsets[x]; init = Int32(typemax(Int32))))
                    insert!(ready, pos, w)
                end
            end
        end
        if emitted < length(units)
            cyc = StmtId[]
            for (ui, w) in pending
                isempty(w) && continue
                for s in stmtsets[ui]
                    push!(cyc, StmtId(s))
                end
            end
            throw(CausalityError(sort!(cyc; by = s -> s.id)))
        end
        spans[ri] = (firstpos, length(order))
        return nothing
    end
    schedule_region!(Int32(1))

    # rebuild dense arrays in scheduled order (a compaction — renaming point)
    nn = length(order)
    stmt_map = zeros(Int32, n)
    for (newid, old) in enumerate(order)
        stmt_map[old] = Int32(newid)
    end
    region_map = zeros(Int32, length(ir.regions))
    live = [ri for (ri, reg) in enumerate(ir.regions) if !reg.dead]
    for (ni, ri) in enumerate(live)
        region_map[ri] = Int32(ni)
    end

    newconsts = Any[]; newconstmap = IdDict{Any,Int}()
    newglobals = GlobalRef[]; newglobalmap = Dict{GlobalRef,Int}()
    const_map = zeros(Int32, length(body.constants))
    global_map = zeros(Int32, length(body.globals))
    newpool = Operand[]

    function remap_word(o::Operand)::Operand
        t = optag(o)
        if t == TAG_STMT
            m = stmt_map[payload(o)]
            m == 0 && error("schedule!: reference to dropped statement")
            return op_stmt(StmtId(m))
        elseif t == TAG_REGION || t == TAG_BLOCK
            m = region_map[payload(o)]
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

    newkind = Vector{Kind}(undef, nn); newops = Vector{UInt64}(undef, nn)
    newtype = Vector{Any}(undef, nn); newflag = Vector{UInt32}(undef, nn)
    newdebug = Vector{NTuple{3,Int32}}(undef, nn)
    newregion_col = Vector{RegionId}(undef, nn)
    for newid in 1:nn
        old = Int(order[newid])
        newkind[newid] = body.kind[old]
        newtype[newid] = body.type[old]
        newflag[newid] = body.flag[old]
        newdebug[newid] = body.debug[old]
        newregion_col[newid] = RegionId(region_map[body.region[old].id])
        w = body.ops[old]
        if is_ops_inline(w)
            m = stmt_map[inline_stmt(w).id]
            newops[newid] = set_inline_stmt(w, StmtId(m))
        else
            len = Int(ops_len(w))
            if len == 0
                newops[newid] = OPS_EMPTY
            else
                off = length(newpool)
                for j in 1:len
                    push!(newpool, remap_word(body.operands[ops_offset(w) + j]))
                end
                newops[newid] = ops_pool(off, len)
            end
        end
    end

    newregions = Region[]
    for ri in live
        reg = ir.regions[ri]
        sp = get(spans, Int32(ri), (0, -1))
        nr = Region(reg.kind, reg.activation, reg.owner, # owner remapped below
                    isnull(reg.parent) ? NULL_REGION : RegionId(region_map[reg.parent.id]),
                    StmtId[StmtId(stmt_map[a.id]) for a in reg.args],
                    isnull(reg.cond) ? NULL_STMT : StmtId(stmt_map[reg.cond.id]),
                    reg.negated,
                    sp[2] >= sp[1] ? StmtId(sp[1]) : NULL_STMT,
                    sp[2] >= sp[1] ? StmtId(sp[2]) : NULL_STMT, false)
        isnull(reg.owner) || (nr.owner = StmtId(stmt_map[reg.owner.id]))
        push!(newregions, nr)
    end

    rs = RemapSet(stmt_map, region_map, const_map, global_map)
    old_of_new = Int32[order[i] for i in 1:nn]
    # Staged column hooks: a throwing callback aborts with the IR unchanged (§4.1).
    newcols = compact_cols_staged(body.cols, old_of_new, rs)
    body.len = Int32(nn)
    body.kind = newkind; body.ops = newops; body.type = newtype
    body.flag = newflag; body.debug = newdebug; body.region = newregion_col
    body.operands = newpool
    body.constants = newconsts; body.constmap = newconstmap
    body.globals = newglobals; body.globalmap = newglobalmap
    body.cols = newcols
    resize!(ir.regions, 0); append!(ir.regions, newregions)
    ir.owner.state = LAYOUT_DENSE
    ir.owner.generation += 1
    empty!(ir.cache.entries)
    ir.cache.layout_epoch += 1
    return (ir, rs)
end
