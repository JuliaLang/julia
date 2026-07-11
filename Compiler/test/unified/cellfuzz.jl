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
        elseif roll < 0.58
            store!(cx, ints)
        elseif roll < 0.70
            read!(cx, ints)
        elseif roll < 0.74
            guarded_read!(cx, ints, depth)
        else
            plain!(cx, ints)
        end
    end
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

"Interpret, capturing thrown errors as comparable outcomes."
function outcome(ir, args...)
    try
        (:ok, UnifiedIR.interpret(ir, args...))
    catch e
        (:err, sprint(showerror, e))
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
             unclassified = Ref(0), dfmissing = Ref(0), dfcells = Ref(0),
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
            if r === :UNCLASSIFIED
                stats.unclassified[] += 1
                push!(stats.failures, (seed, case, :unclassified))
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
