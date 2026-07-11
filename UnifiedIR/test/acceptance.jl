# Acceptance tests for Appendix B of unifiedir-design.md, against the P0/P1
# core. Each testset names the Appendix B item(s) it covers.
#
# Appendix B items NOT implementable against the current core, skipped here:
#
#  * Stale statement/region handles failing deterministically in debug builds:
#    the core has no debug-fattened handle (`StmtRef` + generation-checked
#    dereference) machinery yet; production behavior is contract + the
#    compaction fuzzing in test/fuzz.jl (§2.3, §13.10).
#  * `await` dominance/liveness split, `%C` normal-edge-only delivery, frame
#    snapshot per resumption, DCE of an unused continuation: these need cfg
#    liveness/DCE and a task runtime; only successor enumeration and textual
#    round-trip are testable today (covered below).
#  * `@goto` out of a `try`/`catch` round-tripping through the boundary
#    converters: the `IRCode`/`CodeInfo` converters are provider-side and not
#    part of this package (§8.3, §10).
#  * Exception tests matching "the existing boundary IR after promotion + exit
#    synthesis": needs the exit converter. The interpreter-level half (throw
#    before vs after a cell store, `cell_isdefined` in handlers, promotion
#    refusal) is covered below.
#  * Deferred closure EFFECT composition for DCE (unused closure with
#    effectful body is deletable): v1 `dce!` does not process region-owning
#    ops at all; the activation-boundary EXIT rules are covered below.
#  * The differential end-to-end benchmark (P0 harness): needs the boundary
#    converters and a function corpus.
#  * Provenance-graph tier relocation through inlining: only the debug tier
#    (codeloc triples) exists in the core today (checked in test/splice.jl);
#    the sparse provenance-graph column is P3 (§3.6).

using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region, op_block, op_sparam,
    StmtId, RegionId, RemapSet, is_guard, edge_bundles, getop, operands
using Test

# ---------------------------------------------------------------------------
# Helper column types (top-level; used by the column-protocol testsets)
# ---------------------------------------------------------------------------

# Annotation-class column: a wrapper over SparseCol so `semclass` can differ
# from the Derived default without pirating SparseCol{Symbol} itself.
struct AccNoteCol
    inner::SparseCol{Symbol}
end
AccNoteCol() = AccNoteCol(SparseCol{Symbol}())
UnifiedIR.semclass(::Type{AccNoteCol}) = UnifiedIR.Annotation()
UnifiedIR.col_grow!(::AccNoteCol, n::Integer, oldlen::Integer) = nothing
UnifiedIR.col_compact!(c::AccNoteCol, old_of_new::Vector{Int32}) =
    (UnifiedIR.col_compact!(c.inner, old_of_new); c)
UnifiedIR.col_clear!(c::AccNoteCol) = UnifiedIR.col_clear!(c.inner)
Base.getindex(c::AccNoteCol, s) = c.inner[s]
Base.setindex!(c::AccNoteCol, v, s) = (c.inner[s] = v)

# hasrefs column: values embed StmtIds and must be remapped at renaming points
# (the §3.5 contract the compaction fuzzers' id-perturbation exists to catch).
mutable struct AccRefCol
    data::Dict{Int32,StmtId}
end
AccRefCol() = AccRefCol(Dict{Int32,StmtId}())
UnifiedIR.hasrefs(::Type{AccRefCol}) = true
UnifiedIR.semclass(::Type{AccRefCol}) = UnifiedIR.Annotation()
UnifiedIR.col_grow!(::AccRefCol, n::Integer, oldlen::Integer) = nothing
function UnifiedIR.col_compact!(c::AccRefCol, old_of_new::Vector{Int32})
    new_of_old = Dict{Int32,Int32}(old_of_new[i] => Int32(i) for i in 1:length(old_of_new))
    nd = Dict{Int32,StmtId}()
    for (k, v) in c.data
        nk = get(new_of_old, k, Int32(0))
        nk != 0 && (nd[nk] = v)
    end
    c.data = nd
    return c
end
UnifiedIR.remap_refs!(c::AccRefCol, rs::RemapSet) =
    (for (k, v) in c.data; c.data[k] = UnifiedIR.remap(rs, v); end; c)
UnifiedIR.col_clear!(c::AccRefCol) = empty!(c.data)

# Failure-injecting column for the strong exception guarantee (§4.1): its
# remap hook throws while ACC_FAILREMAP[] is set.
const ACC_FAILREMAP = Ref(false)
struct AccFailCol
    inner::UnifiedIR.DenseCol{Int}
end
AccFailCol() = AccFailCol(UnifiedIR.DenseCol{Int}(0))
UnifiedIR.hasrefs(::Type{AccFailCol}) = true
UnifiedIR.semclass(::Type{AccFailCol}) = UnifiedIR.Annotation()
UnifiedIR.col_grow!(c::AccFailCol, n::Integer, oldlen::Integer) =
    UnifiedIR.col_grow!(c.inner, n, oldlen)
UnifiedIR.col_compact!(c::AccFailCol, old_of_new::Vector{Int32}) =
    (UnifiedIR.col_compact!(c.inner, old_of_new); c)
UnifiedIR.remap_refs!(c::AccFailCol, rs::RemapSet) =
    ACC_FAILREMAP[] ? error("injected column failure") : c
UnifiedIR.col_clear!(c::AccFailCol) = UnifiedIR.col_clear!(c.inner)

acc_throwit(x) = error("acc boom")

# ---------------------------------------------------------------------------
# Visibility clause 3: a body cannot use its owner's result (§5.1)
# ---------------------------------------------------------------------------

@testset "clause 3: if arm produces its own if's result" begin
    b = Builder(name = :badif)
    a = append_stmt!(b, K"region_arg"; type = Bool)
    z = append_stmt!(b, K"if", a; type = Int64)
    open_region!(b, z)
    append_stmt!(b, K"result", z)          # illegal self-use
    close_region!(b)
    append_stmt!(b, K"return", z)
    ir = finish!(b; verify = false)
    @test_throws VerifyError verify_ir(ir; level = 1)
    @test_throws "is not visible" verify_ir(ir; level = 1)
end

@testset "clause 3: loop body reads its own loop's result" begin
    b = Builder(name = :badloop)
    n = append_stmt!(b, K"region_arg"; type = Int64)
    r = append_stmt!(b, K"loop", 0; type = Int64)
    body = open_region!(b, r; kind = REGION_LOOP_BODY)
    j = append_stmt!(b, K"region_arg"; type = Int64)
    bad = append_stmt!(b, K"test.add", r, 1; type = Int64)  # bypasses carried args
    c = append_stmt!(b, K"test.icmp", :slt, j, n; type = Bool)
    append_stmt!(b, K"continue", op_region(body), op_stmt(c), op_stmt(bad))
    close_region!(b)
    append_stmt!(b, K"return", r)
    ir = finish!(b; verify = false)
    @test_throws VerifyError verify_ir(ir; level = 1)
    @test_throws "is not visible" verify_ir(ir; level = 1)
end

@testset "clause 3: closure body reads its own closure value" begin
    # accidental recursion is impossible without an explicit binder (§5.1/§5.7)
    b = Builder(name = :badclo)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    f = append_stmt!(b, K"closure"; type = Any)
    open_region!(b, f; kind = REGION_BODY, activation = ACT_DEFERRED)
    append_stmt!(b, K"return", f)         # illegal self-reference
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b; verify = false)
    @test_throws VerifyError verify_ir(ir; level = 1)
end

# ---------------------------------------------------------------------------
# Guard conditions in def-use, scheduling, liveness, RemapSets (§3.2, §4.3)
# ---------------------------------------------------------------------------

@testset "guard conditions: def-use, scheduling, RemapSet" begin
    # The guarded eq is written BEFORE its condition so only the guard
    # dependency (not id order) can schedule the cond first.
    src = """
    node @g(%1::Int64)  layout=floating {
      region ^g2 = guard(^base, cond %3)
      eq %2 @^g2 = test.add %1, const 1 :: Int64
      eq %3 = test.icmp const :sgt, %1, const 0 :: Bool
    }
    """
    ir = parse_ir(src)
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_FLOATING
    @test verify_ir(ir; level = 1)
    # textual round trip of the guard-bearing floating node
    ir_rt = parse_ir(print_ir(ir))
    @test struct_eq(ir, ir_rt)

    # def-use: the cond (%3) has exactly one use — the GuardCondition site
    counts = use_counts(ir)
    @test counts[3] == 1
    @test counts[1] == 2         # arg used by add and icmp
    guards = [RegionId(ri) for (ri, reg) in enumerate(ir.regions) if is_guard(reg)]
    @test length(guards) == 1
    @test getregion(ir, guards[1]).cond == StmtId(3)

    # scheduling: guard cond is a dependency of every guarded statement
    old_add, old_cond = 2, 3
    ir, rs = schedule!(ir)
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_DENSE
    @test rs.stmt[old_cond] != 0 && rs.stmt[old_add] != 0       # RemapSet covers both
    @test rs.stmt[old_cond] < rs.stmt[old_add]                  # cond scheduled first
    @test verify_ir(ir; level = 1)

    # the guard region's cond field was remapped through the RemapSet
    guards = [RegionId(ri) for (ri, reg) in enumerate(ir.regions) if is_guard(reg)]
    @test length(guards) == 1
    gcond = getregion(ir, guards[1]).cond
    @test gcond == StmtId(rs.stmt[old_cond])
    @test stmt_kind(ir, gcond) == K"test.icmp"

    # liveness: dce! may drop the unused guarded equation, but the guard
    # condition's GuardCondition use keeps the condition itself alive even
    # though it has zero statement-operand uses
    dce!(ir)
    @test !UnifiedIR.is_tombstone(ir, gcond)
    @test stmt_kind(ir, gcond) == K"test.icmp"

    # dense-state restrictions on the scheduled (dense + guard-region) IR:
    @test_throws "guard-region condition" delete_stmt!(ir, gcond)
    # compact! refuses to order live guard regions — that is schedule!'s job (§4.3)
    @test_throws "does not order guard regions" compact!(ir)
end

# ---------------------------------------------------------------------------
# Dense delete_stmt! refusals (§4.1)
# ---------------------------------------------------------------------------

@testset "dense delete_stmt! refuses non-plain statements" begin
    b = Builder(name = :del)
    a = append_stmt!(b, K"region_arg"; type = Bool)
    z = build_if!(b, a; type = Int64, f_else = b -> append_stmt!(b, K"result", 2)) do b
        append_stmt!(b, K"result", 1)
    end
    x = append_stmt!(b, K"test.add", z, 1; type = Int64)
    t = append_stmt!(b, K"return", x)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test_throws "is a terminator" delete_stmt!(ir, t)
    @test_throws "is a region_arg" delete_stmt!(ir, a)
    @test_throws "owns regions" delete_stmt!(ir, z)
    # guard-condition refusal is covered in the guard testset above
    # a genuinely plain dead statement still deletes fine after its use is gone
    replace_uses!(ir, x => z); flush_renames!(ir)
    @test delete_stmt!(ir, x) == x
    @test verify_ir(ir; level = 1)
end

# ---------------------------------------------------------------------------
# br_if / switch: 2+ successor edge bundles with block args, no implicit
# fallthrough; round trip and interpretation (§5.5, Appendix B)
# ---------------------------------------------------------------------------

@testset "br_if round trip with block arguments" begin
    # f(a) = a > 0 ? a+1 : a*2
    b = Builder(name = :brf)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    cfg = append_stmt!(b, K"cfg", a; type = Int64)
    # region ids are deterministic: root=1, then blocks in creation order
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb2 (entry)
    x = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"test.icmp", :sgt, x, 0; type = Bool)
    br = append_stmt!(b, K"br_if", op_stmt(c),
                      op_block(RegionId(3)), op_inline(1), op_stmt(x),
                      op_block(RegionId(4)), op_inline(1), op_stmt(x))
    close_region!(b)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb3 (then)
    y = append_stmt!(b, K"region_arg"; type = Int64)
    y1 = append_stmt!(b, K"test.add", y, 1; type = Int64)
    append_stmt!(b, K"result", y1)
    close_region!(b)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb4 (else)
    z = append_stmt!(b, K"region_arg"; type = Int64)
    z1 = append_stmt!(b, K"test.mul", z, 2; type = Int64)
    append_stmt!(b, K"result", z1)
    close_region!(b)
    append_stmt!(b, K"return", cfg)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    # both successors enumerate, each with its argument bundle
    bs = edge_bundles(ir, br)
    @test length(bs) == 2
    @test bs[1][1] == RegionId(3) && bs[2][1] == RegionId(4)
    @test length(bs[1][2]) == 1 && length(bs[2][2]) == 1
    @test interpret(ir, 5) == 6
    @test interpret(ir, -5) == -10
    txt = print_ir(ir)
    ir2 = parse_ir(txt)
    @test struct_eq(ir, ir2)
    @test interpret(ir2, 5) == 6
    @test interpret(ir2, -5) == -10
end

@testset "switch round trip with case bundles" begin
    b = Builder(name = :sw)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    cfg = append_stmt!(b, K"cfg", a; type = Int64)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb2 (entry)
    x = append_stmt!(b, K"region_arg"; type = Int64)
    sw = append_stmt!(b, K"switch", op_stmt(x), op_inline(2),
                      op_inline(0), op_block(RegionId(3)), op_inline(1), op_stmt(x),
                      op_inline(1), op_block(RegionId(4)), op_inline(0),
                      op_block(RegionId(5)), op_inline(1), op_stmt(x))
    close_region!(b)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb3: case 0
    y = append_stmt!(b, K"region_arg"; type = Int64)
    y1 = append_stmt!(b, K"test.add", y, 100; type = Int64)
    append_stmt!(b, K"result", y1)
    close_region!(b)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb4: case 1 (no args)
    append_stmt!(b, K"result", 111)
    close_region!(b)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb5: default
    z = append_stmt!(b, K"region_arg"; type = Int64)
    z1 = append_stmt!(b, K"test.mul", z, -1; type = Int64)
    append_stmt!(b, K"result", z1)
    close_region!(b)
    append_stmt!(b, K"return", cfg)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    bs = edge_bundles(ir, sw)
    @test length(bs) == 3                                  # 2 cases + default
    @test [d.id for (d, _) in bs] == [3, 4, 5]
    @test interpret(ir, 0) == 100
    @test interpret(ir, 1) == 111
    @test interpret(ir, 7) == -7
    ir2 = parse_ir(print_ir(ir))
    @test struct_eq(ir, ir2)
    @test interpret(ir2, 0) == 100
    @test interpret(ir2, 1) == 111
    @test interpret(ir2, 7) == -7
end

@testset "await: successor enumeration + round trip (execution: skipped)" begin
    # v1 carries await in cfg form (§5.6): a block terminator with normal and
    # resume edge bundles. Activation is a property of the resume EDGE, not a
    # lexical region bit, so the blocks are ordinary REGION_BLOCKs.
    b = Builder(name = :aw)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    cfg = append_stmt!(b, K"cfg"; type = Int64)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb2 (entry)
    aw = append_stmt!(b, K"await", op_inline(0),
                      op_block(RegionId(3)), op_inline(0),
                      op_block(RegionId(4)), op_inline(0))
    close_region!(b)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb3: normal
    append_stmt!(b, K"result", 1)
    close_region!(b)
    open_region!(b, cfg; kind = REGION_BLOCK)             # ^bb4: resume
    append_stmt!(b, K"result", 2)
    close_region!(b)
    append_stmt!(b, K"return", cfg)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    bs = edge_bundles(ir, aw)
    @test length(bs) == 2                                  # normal then resume
    @test bs[1][1] == RegionId(3) && bs[2][1] == RegionId(4)
    ir2 = parse_ir(print_ir(ir))
    @test struct_eq(ir, ir2)
end

# ---------------------------------------------------------------------------
# Floating conversion legality + delay temporal identity (§4.3)
# ---------------------------------------------------------------------------

@testset "float! rejects non-reorderable statements" begin
    # test.opaque carries no NOTHROW/TERMINATES flags: pure-but-throwing or
    # potentially nonterminating operations may not float.
    b = Builder(name = :nofloat)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    append_stmt!(b, K"test.opaque", a; type = Int64)
    b.ir.meta[:floating_node] = true
    ir = finish!(b)
    @test_throws "is not reorderable" float!(ir)
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_DENSE   # unchanged

    # positive control: REMOVABLE-flagged statements float fine
    b = Builder(name = :yesfloat)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    x = append_stmt!(b, K"test.add", a, 1; type = Int64)
    append_stmt!(b, K"test.mul", x, x; type = Int64)
    b.ir.meta[:floating_node] = true
    ir = finish!(b)
    float!(ir)
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_FLOATING
    @test verify_ir(ir; level = 1)
    ir, _ = schedule!(ir)
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_DENSE
    @test verify_ir(ir; level = 1)

    # a terminator likewise blocks float! (checked even when its flags are
    # reorderable; with default flags the reorderability error fires first)
    b = Builder(name = :termfloat)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    r = append_stmt!(b, K"return", a)
    ir = finish!(b)
    set_flag!(ir, r, FLAG_PURE)
    @test_throws "is a terminator" float!(ir)
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_DENSE
end

@testset "independent delays keep distinct identity through schedule!" begin
    src = """
    node @dd(%1::Int64)  layout=floating {
      eq %2 = test.delay %1, const 0 :: Int64
      eq %3 = test.delay %1, const 0 :: Int64
      eq %4 = test.add %2, %3 :: Int64
    }
    """
    ir = parse_ir(src)
    @test verify_ir(ir; level = 1)    # acyclic modulo the delayed data edge
    ir, rs = schedule!(ir)
    # two syntactically identical delays are distinct cells: never CSE'd
    @test nstmts(ir) == 4
    delays = [i for i in 1:nstmts(ir)
              if UnifiedIR.kindname(stmt_kind(ir, StmtId(i))) === Symbol("test.delay")]
    @test length(delays) == 2
    @test rs.stmt[2] != 0 && rs.stmt[3] != 0 && rs.stmt[2] != rs.stmt[3]
end

# ---------------------------------------------------------------------------
# Text parsing rejects unsupported identity-bearing constants (§9)
# ---------------------------------------------------------------------------

@testset "opaque constants: printed as markers, rejected by the parser" begin
    # mutable constant through the K"value" escape hatch
    b = Builder(name = :opq)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    v = append_stmt!(b, K"value", [1, 2, 3]; type = Any)
    append_stmt!(b, K"return", v)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    txt = print_ir(ir)
    @test occursin("#<opaque", txt)
    @test_throws UnifiedIR.ParseError parse_ir(txt)
    @test_throws "outside the portable subset" parse_ir(txt)

    # identity-bearing constant as an ordinary call operand
    b = Builder(name = :opq2)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    o = append_stmt!(b, K"test.opaque", Dict{Int,Int}(1 => 2), a; type = Any)
    append_stmt!(b, K"return", o)
    ir = finish!(b)
    txt = print_ir(ir)
    @test occursin("#<opaque", txt)
    @test_throws UnifiedIR.ParseError parse_ir(txt)
end

# ---------------------------------------------------------------------------
# Derived-column invalidation on replacement (§3.5, Appendix B)
# ---------------------------------------------------------------------------

@testset "replacing a call invalidates Derived columns, keeps Annotation" begin
    b = Builder(name = :inval, cols = (ci = SparseCol{Symbol}(), note = AccNoteCol()))
    a = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"call", acc_throwit, a; type = Any)
    append_stmt!(b, K"return", c)
    ir = finish!(b)
    callst = c
    ir.body.cols.ci[callst] = :some_callinfo      # SparseCol defaults to Derived()
    ir.body.cols.note[callst] = :provenance       # Annotation class
    @test UnifiedIR.semclass(typeof(ir.body.cols.ci)) isa UnifiedIR.Derived
    replace_stmt!(ir, callst, K"call", identity, a; type = Any)
    @test ir.body.cols.ci[callst] === nothing     # conservatively invalidated
    @test ir.body.cols.note[callst] === :provenance
end

@testset "replace_stmt! footprint checks (§4.1)" begin
    b = Builder(name = :fp)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    x = append_stmt!(b, K"test.add", a, 1; type = Int64)
    t = append_stmt!(b, K"return", x)
    ir = finish!(b)
    @test_throws "footprint change" replace_stmt!(ir, x, K"return", a)       # 1->0 results
    @test_throws "footprint change" replace_stmt!(ir, t, K"test.add", a, a)  # terminator->plain
    @test_throws "footprint change" replace_stmt!(ir, x, K"if", a)           # non-owner->owner
    # legal same-footprint replacement works and preserves identity
    replace_stmt!(ir, x, K"test.mul", a, a; type = Int64)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 6) == 36
end

@testset "replace_uses! composes; cycles error (§4.1)" begin
    b = Builder(name = :ru)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    x = append_stmt!(b, K"test.add", a, 1; type = Int64)
    y = append_stmt!(b, K"test.add", a, 2; type = Int64)
    z = append_stmt!(b, K"test.add", x, y; type = Int64)
    append_stmt!(b, K"return", z)
    ir = finish!(b)
    # chains collapse: x -> y, y -> a means both rewrite to a
    replace_uses!(ir, x => y)
    replace_uses!(ir, y => a)
    flush_renames!(ir)
    @test operands(ir, z) == [op_stmt(a), op_stmt(a)]
    @test interpret(ir, 21) == 42
    # cycles are an error
    replace_uses!(ir, x => y)
    replace_uses!(ir, y => x)
    @test_throws "rename cycle" flush_renames!(ir)
    empty!(ir.pending)
end

# ---------------------------------------------------------------------------
# hasrefs columns are remapped through compact! (§3.5 reference protocol)
# ---------------------------------------------------------------------------

@testset "hasrefs column values follow the RemapSet" begin
    b = Builder(name = :refc, cols = (rc = AccRefCol(),))
    a = append_stmt!(b, K"region_arg"; type = Int64)
    dead = append_stmt!(b, K"test.add", a, 1; type = Int64)
    x = append_stmt!(b, K"test.mul", a, a; type = Int64)
    y = append_stmt!(b, K"test.add", x, 2; type = Int64)
    append_stmt!(b, K"return", y)
    ir = finish!(b)
    ir.body.cols.rc.data[y.id] = x                # column value embeds a StmtId
    delete_stmt!(ir, dead)                        # forces renumbering
    ir, rs = compact!(ir)
    @test rs.stmt[dead.id] == 0
    ny, nx = rs.stmt[y.id], rs.stmt[x.id]
    @test ny != 0 && nx != 0
    @test ir.body.cols.rc.data[ny] == StmtId(nx)  # row permuted AND value remapped
    @test verify_ir(ir; level = 1)
end

# ---------------------------------------------------------------------------
# Strong exception guarantee at renaming points (§4.1, §13.10, Appendix B)
# ---------------------------------------------------------------------------

@testset "compact!: injected column failure leaves the IR logically unchanged" begin
    b = Builder(name = :failc, cols = (fc = AccFailCol(),))
    a = append_stmt!(b, K"region_arg"; type = Int64)
    dead = append_stmt!(b, K"test.add", a, 1; type = Int64)
    x = append_stmt!(b, K"test.mul", a, a; type = Int64)
    append_stmt!(b, K"return", x)
    ir = finish!(b)
    ir.body.cols.fc.inner[x] = 33
    editable(ir)
    delete_stmt!(ir, dead)
    ins = insert_before!(ir, x, K"test.add", op_stmt(a), op_inline(Int64(0)); type = Int64)
    snap_kind = copy(ir.body.kind); snap_ops = copy(ir.body.ops)
    snap_operands = copy(ir.body.operands); snap_region = copy(ir.body.region)
    snap_len = ir.body.len; snap_gen = generation(ir)
    snap_nreg = length(ir.regions)
    snap_fc = copy(ir.body.cols.fc.inner.data)
    ACC_FAILREMAP[] = true
    try
        @test_throws "injected column failure" compact!(ir)
    finally
        ACC_FAILREMAP[] = false
    end
    # not merely "doesn't crash": the IR is logically unchanged
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_EDITABLE
    @test generation(ir) == snap_gen
    @test ir.body.len == snap_len
    @test ir.body.kind == snap_kind
    @test ir.body.ops == snap_ops
    @test ir.body.operands == snap_operands
    @test ir.body.region == snap_region
    @test length(ir.regions) == snap_nreg
    @test ir.body.cols.fc.inner.data == snap_fc
    @test verify_ir(ir; level = 1)
    # and the same compact! succeeds once the callback behaves
    ir, rs = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test generation(ir) == snap_gen + 1
    @test ir.body.cols.fc.inner[StmtId(rs.stmt[x.id])] == 33   # value followed its stmt
    @test interpret(ir, 6) == 36
end

@testset "schedule!: injected column failure leaves the IR floating/unchanged" begin
    src = """
    node @sf(%1::Int64)  layout=floating {
      eq %3 = test.add %2, const 1 :: Int64
      eq %2 = test.delay %3, const 0 :: Int64
    }
    """
    ir = parse_ir(src; cols = (fc = AccFailCol(),))
    snap_kind = copy(ir.body.kind); snap_ops = copy(ir.body.ops)
    snap_gen = generation(ir)
    ACC_FAILREMAP[] = true
    try
        @test_throws "injected column failure" schedule!(ir)
    finally
        ACC_FAILREMAP[] = false
    end
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_FLOATING
    @test generation(ir) == snap_gen
    @test ir.body.kind == snap_kind && ir.body.ops == snap_ops
    ir, _ = schedule!(ir)
    @test UnifiedIR.layout(ir) === UnifiedIR.LAYOUT_DENSE
    @test verify_ir(ir; level = 0)
end

# ---------------------------------------------------------------------------
# Exceptions and cells: throws before vs after a store; promotion refusal (§6)
# ---------------------------------------------------------------------------

function acc_mktry(; store_first::Bool)
    b = Builder(name = :exc)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    cell = append_stmt!(b, K"cell", Int64; type = Any)
    t = append_stmt!(b, K"try"; type = Any)
    open_region!(b, t; kind = REGION_BODY)
    if store_first
        append_stmt!(b, K"cell_set", cell, a)
        append_stmt!(b, K"call", acc_throwit, a; type = Any)
    else
        append_stmt!(b, K"call", acc_throwit, a; type = Any)
        append_stmt!(b, K"cell_set", cell, a)
    end
    append_stmt!(b, K"result", 0)
    close_region!(b)
    open_region!(b, t; kind = REGION_HANDLER)
    exc = append_stmt!(b, K"region_arg"; type = Any)
    d = append_stmt!(b, K"cell_isdefined", op_stmt(cell); type = Bool)
    append_stmt!(b, K"result", d)
    close_region!(b)
    append_stmt!(b, K"return", t)
    finish!(b)
end

@testset "cells across the throw edge: before vs after the store" begin
    ir_before = acc_mktry(store_first = false)
    ir_after = acc_mktry(store_first = true)
    @test verify_ir(ir_before; level = 1)
    @test verify_ir(ir_after; level = 1)
    # the handler observes the last store on the throwing prefix — possibly none
    @test interpret(ir_before, 42) === false
    @test interpret(ir_after, 42) === true
    # §6 v1 promotion policy: never across a throw edge — the handler-crossing
    # cell stays in memory form, and refusing does not perturb semantics
    @test promote_cells!(ir_before) == 0
    @test promote_cells!(ir_after) == 0
    @test interpret(ir_before, 42) === false
    @test interpret(ir_after, 42) === true

    # positive control: a same-region cell (no throw edge) still promotes
    b = Builder(name = :prom)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    cell = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_set", cell, a)
    d = append_stmt!(b, K"cell_isdefined", op_stmt(cell); type = Bool)
    g = append_stmt!(b, K"cell_get", op_stmt(cell); type = Int64)
    x = append_stmt!(b, K"test.add", g, 1; type = Int64)
    s = append_stmt!(b, K"select", op_stmt(d), op_stmt(x), op_inline(Int64(0)); type = Int64)
    append_stmt!(b, K"return", s)
    ir = finish!(b)
    @test interpret(ir, 41) == 42
    @test promote_cells!(ir) == 1
    ir, _ = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 41) == 42
    @test all(i -> stmt_kind(ir, StmtId(i)) != K"cell", 1:nstmts(ir))
end

# ---------------------------------------------------------------------------
# Activation boundaries: deferred closure bodies (§3.3, §5.1)
# ---------------------------------------------------------------------------

@testset "exits may not cross a deferred activation boundary" begin
    b = Builder(name = :clobreak)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    r = build_loop!(b, 0; type = Int64, argtypes = Any[Int64]) do b, args
        j, = args
        lb = UnifiedIR.current_region(b)
        f = append_stmt!(b, K"closure"; type = Any)
        open_region!(b, f; kind = REGION_BODY, activation = ACT_DEFERRED)
        append_stmt!(b, K"break", op_region(lb), op_stmt(j))   # illegal cross-activation exit
        close_region!(b)
        cnd = append_stmt!(b, K"test.icmp", :slt, j, a; type = Bool)
        append_stmt!(b, K"continue", op_region(lb), op_stmt(cnd), op_stmt(j))
    end
    append_stmt!(b, K"return", r)
    ir = finish!(b; verify = false)
    @test_throws "crosses an activation boundary" verify_ir(ir; level = 1)
end

@testset "return + captures inside a deferred body are legal" begin
    b = Builder(name = :cloret)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    f = append_stmt!(b, K"closure"; type = Any)
    open_region!(b, f; kind = REGION_BODY, activation = ACT_DEFERRED)
    x = append_stmt!(b, K"region_arg"; type = Int64)
    y = append_stmt!(b, K"test.add", x, a; type = Int64)   # capture of %a
    append_stmt!(b, K"return", y)                          # returns from the closure
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
end

# ---------------------------------------------------------------------------
# Surgery preconditions (§4.2)
# ---------------------------------------------------------------------------

@testset "wrap_in_if! requires an else for escaping defs" begin
    b = Builder(name = :wrapfail)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"test.icmp", :sgt, a, 0; type = Bool)
    x = append_stmt!(b, K"test.mul", a, 2; type = Int64)
    append_stmt!(b, K"return", x)                          # x escapes the wrapped run
    ir = finish!(b)
    editable(ir)
    @test_throws "supply an else_arm" wrap_in_if!(ir, x, x, c)
end
