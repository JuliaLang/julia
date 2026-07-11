# Compaction fuzzing (§12 P1, §13.10, Appendix B): random-but-valid IR built
# through the Builder; semantics-preserving dense mutations (dead deletion,
# queued renames), editable inserts and region surgery; then compact!,
# verify_ir(level=1), and differential interpretation before/after compaction.
# Everything is seeded and deterministic.
#
# Coverage of the Appendix B fuzzing bullet:
#   * deleted ids + RemapSet coverage        — every iteration
#   * region surgery                         — random wrap_in_if! (if-true)
#   * registered extension columns           — the DenseCol{Int} marker loop
#   * callback failure + exception guarantee — the FzFailCol loop
#   * aliased stale views                    — SKIPPED: needs the debug-build
#     handle machinery (§2.3), which the P0 core does not implement yet.

using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region, StmtId, RemapSet
using Random
using Test

const FZ_MAXDEPTH = 3

# failure-injecting column for the callback-failure loop
const FZ_FAILREMAP = Ref(false)
struct FzFailCol
    inner::UnifiedIR.DenseCol{Int}
end
FzFailCol() = FzFailCol(UnifiedIR.DenseCol{Int}(0))
UnifiedIR.hasrefs(::Type{FzFailCol}) = true
UnifiedIR.semclass(::Type{FzFailCol}) = UnifiedIR.Annotation()
UnifiedIR.col_grow!(c::FzFailCol, n::Integer, oldlen::Integer) =
    UnifiedIR.col_grow!(c.inner, n, oldlen)
UnifiedIR.col_compact!(c::FzFailCol, old_of_new::Vector{Int32}) =
    (UnifiedIR.col_compact!(c.inner, old_of_new); c)
UnifiedIR.remap_refs!(c::FzFailCol, rs::RemapSet) =
    FZ_FAILREMAP[] ? error("fz injected failure") : c
UnifiedIR.col_clear!(c::FzFailCol) = UnifiedIR.col_clear!(c.inner)

# ---------------------------------------------------------------------------
# Random IR generation: pure test-dialect ops, nested if/loop 0-3 deep, with
# correct results/continues. Loops carry (acc, i) and run at most `bound` <= 4
# iterations, so interpretation always terminates.
# ---------------------------------------------------------------------------

fz_pick(rng, ints) = (!isempty(ints) && rand(rng) < 0.75) ?
    op_stmt(rand(rng, ints)) : op_inline(Int64(rand(rng, -4:7)))

function fz_plain!(b, rng, ints)
    r = rand(rng, 1:3)
    s = if r == 1
        append_stmt!(b, K"test.add", fz_pick(rng, ints), fz_pick(rng, ints); type = Int64)
    elseif r == 2
        append_stmt!(b, K"test.mul", fz_pick(rng, ints), fz_pick(rng, ints); type = Int64)
    else
        append_stmt!(b, K"test.iconst", op_inline(Int64(rand(rng, -9:9))); type = Int64)
    end
    push!(ints, s)
    return s
end

fz_icmp!(b, rng, ints) =
    append_stmt!(b, K"test.icmp", rand(rng, (:sgt, :slt, :sge, :sle, :eq, :ne)),
                 fz_pick(rng, ints), fz_pick(rng, ints); type = Bool)

function fz_body!(b, rng, ints, depth)
    for _ in 1:rand(rng, 1:4)
        roll = rand(rng)
        if depth < FZ_MAXDEPTH && roll < 0.22
            fz_if!(b, rng, ints, depth)
        elseif depth < FZ_MAXDEPTH && roll < 0.38
            fz_loop!(b, rng, ints, depth)
        else
            fz_plain!(b, rng, ints)
        end
    end
end

function fz_if!(b, rng, ints, depth)
    c = fz_icmp!(b, rng, ints)
    s = append_stmt!(b, K"if", c; type = Int64)
    for _ in 1:2
        open_region!(b, s; kind = REGION_ARM)
        arm_ints = copy(ints)
        fz_body!(b, rng, arm_ints, depth + 1)
        append_stmt!(b, K"result", fz_pick(rng, arm_ints))
        close_region!(b)
    end
    push!(ints, s)
    return s
end

function fz_loop!(b, rng, ints, depth)
    bound = rand(rng, 1:4)
    s = append_stmt!(b, K"loop", fz_pick(rng, ints), op_inline(Int64(0));
                     type = Tuple{Int64,Int64})
    body = open_region!(b, s; kind = REGION_LOOP_BODY)
    acc = append_stmt!(b, K"region_arg"; type = Int64)
    i = append_stmt!(b, K"region_arg"; type = Int64)
    body_ints = copy(ints); push!(body_ints, acc)
    fz_body!(b, rng, body_ints, depth + 1)
    acc2 = append_stmt!(b, K"test.add", fz_pick(rng, body_ints), op_stmt(acc); type = Int64)
    i2 = append_stmt!(b, K"test.add", op_stmt(i), op_inline(Int64(1)); type = Int64)
    cnd = append_stmt!(b, K"test.icmp", :slt, op_stmt(i2), op_inline(Int64(bound)); type = Bool)
    append_stmt!(b, K"continue", op_region(body), op_stmt(cnd), op_stmt(acc2), op_stmt(i2))
    close_region!(b)
    ex = append_stmt!(b, K"extract", op_stmt(s), op_inline(1); type = Int64)
    push!(ints, ex)
    return ex
end

function fz_randir(rng; cols = UnifiedIR.NOCOLS)
    b = Builder(name = :fz, cols = cols)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    ints = StmtId[a]
    fz_body!(b, rng, ints, 0)
    append_stmt!(b, K"return", fz_pick(rng, ints))
    finish!(b)
end

# ---------------------------------------------------------------------------
# Semantics-preserving mutations
# ---------------------------------------------------------------------------

function fz_mutate_dense!(ir, rng)
    # (a) queued renames: constant-fold some iconsts to their inline operand
    for s in collect(each_stmt(ir))
        stmt_kind(ir, s) === K"test.iconst" || continue
        rand(rng) < 0.5 || continue
        o = UnifiedIR.getop(ir, s, 1)
        UnifiedIR.optag(o) == UnifiedIR.TAG_INLINE || continue
        replace_uses!(ir, s => o)
    end
    flush_renames!(ir)
    # (b) footprint-preserving replace_stmt! (no-op rewrite of a random add)
    for s in collect(each_stmt(ir))
        (stmt_kind(ir, s) === K"test.add" && rand(rng) < 0.15) || continue
        replace_stmt!(ir, s, K"test.add", UnifiedIR.operands(ir, s)...; type = Int64)
    end
    # (c) delete a random subset of plain dead removable statements
    counts = use_counts(ir)
    for s in collect(each_stmt(ir))
        counts[s.id] == 0 || continue
        k = stmt_kind(ir, s)
        UnifiedIR.result_arity(k) == 0 && continue
        UnifiedIR.owns_regions(k) && continue
        k === K"region_arg" && continue
        UnifiedIR.stmt_flag(ir, s) & FLAG_REMOVABLE == FLAG_REMOVABLE || continue
        rand(rng) < 0.7 && delete_stmt!(ir, s)
    end
    return ir
end

function fz_mutate_editable!(ir, rng)
    arg = StmtId(1)
    cands = [s for s in collect(each_stmt(ir))
             if stmt_kind(ir, s) !== K"region_arg" && s != arg]
    inserted = StmtId[]
    for _ in 1:rand(rng, 1:4)
        isempty(cands) && break
        at = rand(rng, cands)
        k = rand(rng) < 0.5 ? K"test.add" : K"test.mul"
        s = insert_before!(ir, at, k, op_stmt(arg),
                           op_inline(Int64(rand(rng, -3:3))); type = Int64)
        push!(inserted, s)
    end
    # region surgery: wrap a used pure statement in `if (0 == 0)` — the else
    # arm never runs, so semantics are preserved while the region table,
    # result threading, and use rewriting all get exercised
    if rand(rng) < 0.4
        counts = use_counts(ir)
        wrapc = [s for s in collect(each_stmt(ir))
                 if (stmt_kind(ir, s) === K"test.add" || stmt_kind(ir, s) === K"test.mul" ||
                     stmt_kind(ir, s) === K"test.iconst") && counts[s.id] > 0]
        if !isempty(wrapc)
            x = rand(rng, wrapc)
            c = insert_before!(ir, x, K"test.icmp", :eq,
                               op_inline(Int64(0)), op_inline(Int64(0)); type = Bool)
            wrap_in_if!(ir, x, x, c; else_arm = (ir, er) -> begin
                push_stmt!(ir, er, K"result", op_inline(Int64(0)))
            end)
        end
    end
    return inserted
end

# ---------------------------------------------------------------------------
# Fuzz drivers
# ---------------------------------------------------------------------------

const FZ_ARGVALS = (Int64(-7), Int64(0), Int64(13))

function fz_iter(rng)
    ir = fz_randir(rng)
    verify_ir(ir; level = 1)
    expected = Any[interpret(ir, v) for v in FZ_ARGVALS]

    fz_mutate_dense!(ir, rng)
    verify_ir(ir; level = 1)
    got = Any[interpret(ir, v) for v in FZ_ARGVALS]
    got == expected || return (false, "dense mutation changed semantics")

    editable(ir)
    fz_mutate_editable!(ir, rng)
    verify_ir(ir; level = 1)              # editable-state lists/okeys/visibility

    n_old = nstmts(ir)
    live_old = [s.id for s in collect(each_stmt(ir))]
    ir, rs = compact!(ir)
    length(rs.stmt) == n_old || return (false, "RemapSet does not cover all old ids")
    for o in live_old
        rs.stmt[o] > 0 || return (false, "live stmt %$o dropped from RemapSet")
    end
    verify_ir(ir; level = 1)
    got = Any[interpret(ir, v) for v in FZ_ARGVALS]
    got == expected || return (false, "compaction changed semantics")
    return (true, "")
end

@testset "compaction fuzz (200 seeded iterations)" begin
    rng = Xoshiro(0x5eed)
    failures = String[]
    for i in 1:200
        ok, why = try
            fz_iter(rng)
        catch e
            (false, sprint(showerror, e))
        end
        ok || push!(failures, "iter $i: $why")
    end
    @test isempty(failures)
    isempty(failures) || foreach(println, failures)
end

@testset "extension-column fuzz: values follow their statements" begin
    rng = Xoshiro(0xc01)
    failures = String[]
    for i in 1:60
        try
            ir = fz_randir(rng; cols = (mycol = UnifiedIR.DenseCol{Int}(0),))
            for s in each_stmt(ir)
                ir.body.cols.mycol[s] = Int(s.id)      # identity marker per row
            end
            expected = Any[interpret(ir, v) for v in FZ_ARGVALS]
            fz_mutate_dense!(ir, rng)
            editable(ir)
            for s in fz_mutate_editable!(ir, rng)
                ir.body.cols.mycol[s] = Int(s.id)
            end
            # snapshot current live markers, then compact and check permutation
            marker = Dict{Int32,Int}(s.id => ir.body.cols.mycol[s] for s in each_stmt(ir))
            ir, rs = compact!(ir)
            verify_ir(ir; level = 1)
            for (old, m) in marker
                new = rs.stmt[old]
                new > 0 || push!(failures, "iter $i: live stmt %$old dropped")
                ir.body.cols.mycol[StmtId(new)] == m ||
                    push!(failures, "iter $i: column value did not follow %$old -> %$new")
            end
            got = Any[interpret(ir, v) for v in FZ_ARGVALS]
            got == expected || push!(failures, "iter $i: semantics changed")
        catch e
            push!(failures, "iter $i: " * sprint(showerror, e))
        end
    end
    @test isempty(failures)
    isempty(failures) || foreach(println, failures)
end

@testset "callback-failure fuzz: strong exception guarantee (§4.1)" begin
    rng = Xoshiro(0xfa11)
    failures = String[]
    for i in 1:20
        try
            ir = fz_randir(rng; cols = (fc = FzFailCol(),))
            expected = Any[interpret(ir, v) for v in FZ_ARGVALS]
            fz_mutate_dense!(ir, rng)
            editable(ir)
            fz_mutate_editable!(ir, rng)
            snap_kind = copy(ir.body.kind); snap_ops = copy(ir.body.ops)
            snap_operands = copy(ir.body.operands); snap_region = copy(ir.body.region)
            snap_len = ir.body.len; snap_gen = generation(ir)
            snap_nreg = length(ir.regions)
            FZ_FAILREMAP[] = true
            threw = false
            try
                compact!(ir)
            catch
                threw = true
            finally
                FZ_FAILREMAP[] = false
            end
            threw || push!(failures, "iter $i: injected failure did not abort compact!")
            (UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_EDITABLE &&
             generation(ir) == snap_gen && ir.body.len == snap_len &&
             ir.body.kind == snap_kind && ir.body.ops == snap_ops &&
             ir.body.operands == snap_operands && ir.body.region == snap_region &&
             length(ir.regions) == snap_nreg) ||
                push!(failures, "iter $i: IR not logically unchanged after aborted compact!")
            verify_ir(ir; level = 1)
            # the same compact! succeeds afterwards, semantics intact
            ir, _ = compact!(ir)
            verify_ir(ir; level = 1)
            got = Any[interpret(ir, v) for v in FZ_ARGVALS]
            got == expected || push!(failures, "iter $i: semantics changed after retry")
        catch e
            push!(failures, "iter $i: " * sprint(showerror, e))
        end
    end
    @test isempty(failures)
    isempty(failures) || foreach(println, failures)
end
