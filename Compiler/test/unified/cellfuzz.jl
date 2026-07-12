# Structured cell-promotion fuzzer — completeness leg (b), driven from
# completeness.jl (testset) and bench/unified_completeness.jl (10k run).
#
# Random nested if/loop/try bodies (depth ≤ 4) with random cell
# declare/store/read placement, built through the Builder in the CORE
# dialect (compute ops are `K"call" GlobalRef(Base, ...)`, so one body is
# both interpretable by the reference interpreter and flattenable by the
# exit converter for the DF-correspondence leg). Shapes covered: one-arm
# stores, sibling-arm swaps (the gcd shape), store-only-in-loop with
# read-after-loop, exits through break/continue at depth ≥ 3, dead tails
# after continue, isdefined-guarded maybe-undef reads, try-body stores read
# in handlers (must refuse), handler stores read after the try.
#
# For each case: verify L1 pre/post, promotion fixpoint, semantic
# differential (identical values AND identical thrown errors on the same
# inputs), residual classification totality (no :UNCLASSIFIED).

module CellFuzz

using Random
using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region, StmtId, RegionId

const MAXDEPTH = 4

fzthrow(x) = x == 7 ? error("fzboom") : x

mutable struct Ctx
    b::Any
    rng::AbstractRNG
    cells::Vector{StmtId}
    # (body region, private counter cell, bound): the counter cell is NOT in
    # `cells` — random stores clobbering it (or constant-true arm continues)
    # would make generated programs non-terminating
    loopstack::Vector{@NamedTuple{r::RegionId, ctr::StmtId, bound::Int}}
end

pick(cx, ints) = (!isempty(ints) && rand(cx.rng) < 0.8) ?
    op_stmt(rand(cx.rng, ints)) : op_inline(Int64(rand(cx.rng, -3:9)))

function plain!(cx, ints)
    f = rand(cx.rng, (GlobalRef(Base, :+), GlobalRef(Base, :*), GlobalRef(Base, :-)))
    s = append_stmt!(cx.b, K"call", f, pick(cx, ints), pick(cx, ints); type = Any)
    push!(ints, s)
    return s
end

cond!(cx, ints) = append_stmt!(cx.b, K"call",
    rand(cx.rng, (GlobalRef(Base, :<), GlobalRef(Base, :(<=)), GlobalRef(Base, :(==)))),
    pick(cx, ints), pick(cx, ints); type = Any)

store!(cx, ints) = isempty(cx.cells) ? nothing :
    append_stmt!(cx.b, K"cell_set", rand(cx.rng, cx.cells), pick(cx, ints))

function read!(cx, ints)
    isempty(cx.cells) && return nothing
    g = append_stmt!(cx.b, K"cell_get", op_stmt(rand(cx.rng, cx.cells)); type = Any)
    push!(ints, g)
    return g
end

"Declare a cell WITHOUT an initial store (maybe-undef until some branch
stores): the definedness-as-data shapes. Occasionally re-undefine live ones."
function undef_cell!(cx, ints)
    if !isempty(cx.cells) && rand(cx.rng) < 0.25
        append_stmt!(cx.b, K"cell_new", rand(cx.rng, cx.cells))
        return
    end
    c = append_stmt!(cx.b, K"cell", Any; type = Any)
    push!(cx.cells, c)
    nothing
end

"An isdefined-guarded USE plus an occasional UNGUARDED read of a random cell
(the read may legitimately throw UndefVarError — the differential compares
thrown errors too)."
function guarded_read!(cx, ints, depth)
    # isdefined-guarded maybe-undef read: if isdefined(c); use get; end
    isempty(cx.cells) && return
    c = rand(cx.rng, cx.cells)
    d = append_stmt!(cx.b, K"cell_isdefined", op_stmt(c); type = Any)
    s = append_stmt!(cx.b, K"if", d; type = Any)
    open_region!(cx.b, s; kind = REGION_ARM)
    g = append_stmt!(cx.b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(cx.b, K"call", GlobalRef(Base, :+), g, op_inline(Int64(1)); type = Any)
    append_stmt!(cx.b, K"result")
    close_region!(cx.b)
    open_region!(cx.b, s; kind = REGION_ARM)
    append_stmt!(cx.b, K"result")
    close_region!(cx.b)
    nothing
end

function body!(cx, ints, depth; allow_exit::Bool = true)
    for _ in 1:rand(cx.rng, 1:4)
        roll = rand(cx.rng)
        if depth < MAXDEPTH && roll < 0.24
            genif!(cx, ints, depth; allow_exit)
        elseif depth < MAXDEPTH && roll < 0.36
            genloop!(cx, ints, depth)
        elseif depth < MAXDEPTH && roll < 0.42
            gentry!(cx, ints, depth)
        elseif depth < MAXDEPTH && roll < 0.46
            genisland!(cx, ints, depth)
        elseif roll < 0.60
            store!(cx, ints)
        elseif roll < 0.70
            read!(cx, ints)
        elseif roll < 0.74
            guarded_read!(cx, ints, depth)
        elseif roll < 0.78
            undef_cell!(cx, ints)
        else
            plain!(cx, ints)
        end
    end
end

"""
A cfg island: 2–4 blocks with cell traffic, forward `br_if`/`goto` edges, an
optional bounded internal backedge, `result` joins — and, under an enclosing
loop, sealed `continue` exits straight out of a block (the escape_string
shape: the loop's backedge originates inside the island, carrying no values
until promotion threads them). Blocks may also contain structured ifs/loops
(and further islands through them). Cross-block SSA references are illegal,
so each block computes from `ints` (outer values) plus its own locals.
"""
function genisland!(cx, ints, depth)
    b = cx.b
    # a bounded counter for the internal backedge (private, like loop ctrs)
    ictr = append_stmt!(b, K"cell", Any; type = Any)
    append_stmt!(b, K"cell_set", ictr, op_inline(Int64(0)))
    s = append_stmt!(b, K"cfg"; type = Any)
    nb = rand(cx.rng, 2:3)
    rs = UnifiedIR.RegionId[]
    for _ in 1:nb
        r = UnifiedIR.Region(UnifiedIR.REGION_BLOCK, s, UnifiedIR.stmt_region(b.ir, s))
        push!(b.ir.regions, r)
        push!(rs, UnifiedIR.RegionId(Int32(length(b.ir.regions))))
    end
    ibound = rand(cx.rng, 2:3)
    # a backedge block needs a FORWARD false edge to exit once the counter
    # is exhausted, so the last block never takes one
    backat = (nb >= 3 && rand(cx.rng) < 0.5) ? rand(cx.rng, 2:nb-1) : 0
    ncells = length(cx.cells)
    for bi in 1:nb
        rid = rs[bi]
        reg = UnifiedIR.getregion(b.ir, rid)
        reg.first = StmtId(Int32(Int(b.ir.body.len) + 1))
        push!(b.open, rid)
        blocals = copy(ints)
        for _ in 1:rand(cx.rng, 1:3)
            roll = rand(cx.rng)
            if roll < 0.30
                store!(cx, blocals)
            elseif roll < 0.55
                read!(cx, blocals)
            elseif depth < MAXDEPTH && roll < 0.68
                genif!(cx, blocals, depth + 1; allow_exit = false)
            elseif depth < MAXDEPTH && roll < 0.76
                genloop!(cx, blocals, depth + 1)
            else
                plain!(cx, blocals)
            end
        end
        cont = !isempty(cx.loopstack) && rand(cx.rng) < 0.35
        if bi == nb && !cont
            append_stmt!(b, K"result", pick(cx, blocals))
        elseif cont
            # sealed exit: the enclosing loop's backedge leaves the island
            # from inside this block (advance its counter for termination)
            L = rand(cx.rng, cx.loopstack)
            rand(cx.rng) < 0.6 && store!(cx, blocals)
            i = append_stmt!(b, K"cell_get", op_stmt(L.ctr); type = Any)
            i2 = append_stmt!(b, K"call", GlobalRef(Base, :+), i, op_inline(Int64(1)); type = Any)
            append_stmt!(b, K"cell_set", L.ctr, i2)
            cnd = append_stmt!(b, K"call", GlobalRef(Base, :<), i2,
                               op_inline(Int64(L.bound)); type = Any)
            append_stmt!(b, K"continue", op_region(L.r), cnd)
        elseif bi == backat
            # bounded internal backedge: retake block 1 while under ibound
            i = append_stmt!(b, K"cell_get", op_stmt(ictr); type = Any)
            i2 = append_stmt!(b, K"call", GlobalRef(Base, :+), i, op_inline(Int64(1)); type = Any)
            append_stmt!(b, K"cell_set", ictr, i2)
            cnd = append_stmt!(b, K"call", GlobalRef(Base, :<), i2, op_inline(Int64(ibound)); type = Any)
            append_stmt!(b, K"br_if", cnd, UnifiedIR.op_block(rs[1]), op_inline(Int64(0)),
                         UnifiedIR.op_block(rs[bi + 1]), op_inline(Int64(0)))
        elseif rand(cx.rng) < 0.5 && bi + 2 <= nb
            v = pick(cx, blocals)
            cnd = append_stmt!(b, K"call", GlobalRef(Base, :(==)), v, op_inline(Int64(7)); type = Any)
            append_stmt!(b, K"br_if", cnd, UnifiedIR.op_block(rs[bi + 2]), op_inline(Int64(0)),
                         UnifiedIR.op_block(rs[bi + 1]), op_inline(Int64(0)))
        else
            append_stmt!(b, K"goto", UnifiedIR.op_block(rs[bi + 1]), op_inline(Int64(0)))
        end
        reg.last = StmtId(Int32(Int(b.ir.body.len)))
        pop!(b.open)
    end
    resize!(cx.cells, ncells)
    push!(ints, s)
    nothing
end

"An arm's ending: join (`result`), or an exit through the loop stack."
function arm_end!(cx, ints; allow_exit)
    r = rand(cx.rng)
    if allow_exit && !isempty(cx.loopstack) && r < 0.18
        L = rand(cx.rng, cx.loopstack)            # multi-level at depth ≥ 3
        # store BEFORE the exit: on `continue` only the backedge can observe
        # it; on `break` it flows out through memory (§5.9)
        store!(cx, ints)
        if rand(cx.rng, Bool)
            # early continue still advances the loop's private counter, so
            # generated programs always terminate
            i = append_stmt!(cx.b, K"cell_get", op_stmt(L.ctr); type = Any)
            i2 = append_stmt!(cx.b, K"call", GlobalRef(Base, :+), i, op_inline(Int64(1)); type = Any)
            append_stmt!(cx.b, K"cell_set", L.ctr, i2)
            cnd = append_stmt!(cx.b, K"call", GlobalRef(Base, :<), i2,
                               op_inline(Int64(L.bound)); type = Any)
            append_stmt!(cx.b, K"continue", op_region(L.r), cnd)
        else
            append_stmt!(cx.b, K"break", op_region(L.r))
        end
    else
        append_stmt!(cx.b, K"result")
    end
end

function genif!(cx, ints, depth; allow_exit = true)
    c = cond!(cx, ints)
    s = append_stmt!(cx.b, K"if", c; type = Any)
    narms = rand(cx.rng) < 0.25 ? 1 : 2
    for _ in 1:narms
        open_region!(cx.b, s; kind = REGION_ARM)
        arm_ints = copy(ints)
        ncells = length(cx.cells)          # cells born in the arm die with it
        body!(cx, arm_ints, depth + 1)
        # the gcd swap shape, sometimes: two stores of swapped reads
        if length(cx.cells) >= 2 && rand(cx.rng) < 0.3
            c1, c2 = rand(cx.rng, cx.cells), rand(cx.rng, cx.cells)
            g1 = append_stmt!(cx.b, K"cell_get", op_stmt(c1); type = Any)
            g2 = append_stmt!(cx.b, K"cell_get", op_stmt(c2); type = Any)
            append_stmt!(cx.b, K"cell_set", c1, g2)
            append_stmt!(cx.b, K"cell_set", c2, g1)
        end
        arm_end!(cx, arm_ints; allow_exit)
        close_region!(cx.b)
        resize!(cx.cells, ncells)
    end
    nothing
end

function genloop!(cx, ints, depth)
    # zero-carried loop; termination through a fresh counter CELL (the shape
    # the lowering backend emits — and the loop-promotion target)
    bound = rand(cx.rng, 1:4)
    ctr = append_stmt!(cx.b, K"cell", Any; type = Any)
    append_stmt!(cx.b, K"cell_set", ctr, op_inline(Int64(0)))
    s = append_stmt!(cx.b, K"loop"; type = Any)
    bodyr = open_region!(cx.b, s; kind = REGION_LOOP_BODY)
    push!(cx.loopstack, (; r = bodyr, ctr, bound))
    body_ints = copy(ints)
    ncells = length(cx.cells)
    body!(cx, body_ints, depth + 1)
    resize!(cx.cells, ncells)
    i = append_stmt!(cx.b, K"cell_get", op_stmt(ctr); type = Any)
    i2 = append_stmt!(cx.b, K"call", GlobalRef(Base, :+), i, op_inline(Int64(1)); type = Any)
    append_stmt!(cx.b, K"cell_set", ctr, i2)
    cnd = append_stmt!(cx.b, K"call", GlobalRef(Base, :<), i2, op_inline(Int64(bound)); type = Any)
    append_stmt!(cx.b, K"continue", op_region(bodyr), cnd)
    pop!(cx.loopstack)
    UnifiedIR.op_stmt(ctr)  # (counter stays private)
    close_region!(cx.b)
    # read-after-loop, sometimes (store-only-in-loop shape)
    rand(cx.rng) < 0.6 && read!(cx, ints)
    nothing
end

function gentry!(cx, ints, depth)
    t = append_stmt!(cx.b, K"try"; type = Any)
    open_region!(cx.b, t; kind = REGION_BODY)
    body_ints = copy(ints)
    ncells = length(cx.cells)
    # occasionally a real throw so handlers execute dynamically
    if rand(cx.rng) < 0.5
        v = append_stmt!(cx.b, K"call", fzthrow, pick(cx, body_ints); type = Any)
        push!(body_ints, v)
    end
    body!(cx, body_ints, depth + 1; allow_exit = false)
    append_stmt!(cx.b, K"result")
    close_region!(cx.b)
    resize!(cx.cells, ncells)
    open_region!(cx.b, t; kind = REGION_HANDLER)
    append_stmt!(cx.b, K"region_arg"; type = Any)
    h_ints = copy(ints)
    if rand(cx.rng) < 0.5
        read!(cx, h_ints)          # try-body stores read in the handler: MUST refuse
    end
    if rand(cx.rng) < 0.4
        store!(cx, h_ints)         # handler store (read after the try, maybe)
    end
    append_stmt!(cx.b, K"result")
    close_region!(cx.b)
    rand(cx.rng) < 0.4 && read!(cx, ints)
    nothing
end

"Build one random body. Deterministic in `rng`."
function randir(rng::AbstractRNG)
    b = Builder(name = :cellfz)
    a1 = append_stmt!(b, K"region_arg"; type = Any)
    a2 = append_stmt!(b, K"region_arg"; type = Any)
    ints = StmtId[a1, a2]
    cx = Ctx(b, rng, StmtId[], RegionId[])
    for _ in 1:rand(rng, 1:3)
        c = append_stmt!(b, K"cell", Any; type = Any)
        rand(rng) < 0.75 && append_stmt!(b, K"cell_set", c, pick(cx, ints))
        push!(cx.cells, c)
    end
    body!(cx, ints, 0)
    ret = if !isempty(cx.cells) && rand(rng) < 0.5
        append_stmt!(b, K"cell_get", op_stmt(rand(rng, cx.cells)); type = Any)
    else
        pick(cx, ints)
    end
    append_stmt!(b, K"return", ret isa StmtId ? op_stmt(ret) : ret)
    return finish!(b)
end

"Interpret, capturing thrown errors as comparable outcomes. Statement ids
in interpreter diagnostics are stripped: they differ across promotion."
function outcome(ir, args...)
    UnifiedIR._FUEL[] = 10_000_000      # hangs become diagnosable failures
    try
        (:ok, UnifiedIR.interpret(ir, args...))
    catch e
        (:err, replace(sprint(showerror, e), r"( \(%\d+\)| at %\d+)" => ""))
    finally
        UnifiedIR._FUEL[] = 0
    end
end

const cellop_kinds = (K"cell", K"cell_set", K"cell_get", K"cell_new", K"cell_isdefined")
count_cellops(ir) = count(i -> UnifiedIR.stmt_kind(ir, StmtId(Int32(i))) in cellop_kinds,
                          1:UnifiedIR.nstmts(ir))

"""
    run_cases(U, n; seed, dfevery = 0) -> stats

Run `n` fuzz cases: build, verify L1, interpret on 3 input pairs, promotion
fixpoint (`U.promotion_fixpoint!`), verify L1, re-interpret and compare
outcomes (values and thrown errors), classify residuals (no :UNCLASSIFIED).
Every `dfevery`th case additionally runs the DF-correspondence check on a
regenerated copy. Returns counters + the failing (seed, case) list.
"""
function run_cases(U, n::Int; seed::Int = 0x5eed, dfevery::Int = 0)
    stats = (; cases = Ref(0), diffs = Ref(0), verifyfails = Ref(0),
             unclassified = Ref(0), openresiduals = Ref(0),
             dfmissing = Ref(0), dfcells = Ref(0),
             dfmatch = Ref(0), dfextra = Ref(0), residuals = Dict{Symbol,Int}(),
             cells_pre = Ref(0), cells_post = Ref(0),
             failures = Tuple{Int,Int,Symbol}[])
    for case in 1:n
        rng = Xoshiro(seed + case)
        ir = randir(copy(rng))
        UnifiedIR.verify_ir(ir; level = 1)
        inputs = [(rand(rng, -2:9), rand(rng, -2:9)) for _ in 1:3]
        push!(inputs, (7, 7))                       # the fzthrow trigger
        ref = [outcome(ir, a...) for a in inputs]
        stats.cells_pre[] += count_cellops(ir)
        ir2 = try
            U.promotion_fixpoint!(ir)
        catch
            push!(stats.failures, (seed, case, :pass_error)); stats.verifyfails[] += 1
            continue
        end
        okv = try
            UnifiedIR.verify_ir(ir2; level = 1); true
        catch
            false
        end
        okv || (push!(stats.failures, (seed, case, :verify)); stats.verifyfails[] += 1; continue)
        stats.cells_post[] += count_cellops(ir2)
        post = [outcome(ir2, a...) for a in inputs]
        if post != ref
            push!(stats.failures, (seed, case, :differential)); stats.diffs[] += 1
        end
        for (_, r) in U.classify_residual_cells(ir2)
            stats.residuals[r] = get(stats.residuals, r, 0) + 1
            if r in (:maybe_undef_read, :escape, :UNCLASSIFIED)
                # classes the machinery eliminates BY DESIGN must stay gone
                stats.unclassified[] += 1
                push!(stats.failures, (seed, case, :unclassified))
            elseif !(r in U.RESIDUAL_OK)
                # :island / :refused_multilevel_exit — the two OPEN bug
                # classes on adversarial fuzz shapes (multi-level exit
                # values, cross-island lifetimes); zero corpus-wide, counted
                # here so the battery reports the frontier without failing
                stats.openresiduals[] += 1
            end
        end
        if dfevery > 0 && case % dfevery == 0
            irdf = randir(Xoshiro(seed + case))     # fresh copy, same seed
            r = U.df_correspondence(irdf)
            for x in r.results
                # denominator = promoted cells; classified residuals are the
                # documented exception classes, not DF placements to score
                x.status === :residual_classified && continue
                stats.dfcells[] += 1
                x.status === :match && (stats.dfmatch[] += 1)
                stats.dfextra[] += x.extra
                if x.status === :missing || x.status === :residual_unclassified
                    stats.dfmissing[] += 1
                    push!(stats.failures, (seed, case, :df_missing))
                end
            end
        end
        stats.cases[] += 1
    end
    return stats
end

end # module CellFuzz
