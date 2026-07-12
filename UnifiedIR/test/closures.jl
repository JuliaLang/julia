# Closure regions (§5.7, §3.3): the v1 `closure` op semantics — verifier L1
# deltas (result-feeding class, cell-class boundary rule, op discipline),
# `closure_environment` as the single source of truth for the derived
# environment, the interpreter's `UClosure` (creation-time snapshot; cells by
# reference), textual round-trips, and mode-aware DCE (deferred effects do
# not count at the creation site).

using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region, StmtId, getop, CLOSURE_FLAG_ISVA
using Test

# open a deferred closure body region (the §5.7 canonical shape)
open_closure_body!(b, f) = open_region!(b, f; kind = REGION_BODY, activation = ACT_DEFERRED)

@testset "value capture + call + round trip" begin
    b = Builder(name = :mk)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    base = append_stmt!(b, K"test.add", a, 10; type = Int64)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    x = append_stmt!(b, K"region_arg"; type = Int64)
    s = append_stmt!(b, K"test.add", x, base; type = Int64)   # capture of %base
    append_stmt!(b, K"return", s)
    close_region!(b)
    r = append_stmt!(b, K"call", f, 5; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 100) == 115

    env = closure_environment(ir, f)
    @test env.values == [base] && isempty(env.cells)

    txt = print_ir(ir)
    @test occursin("closure (%$(x.id)::Int64) {", txt)
    ir2 = parse_ir(txt)
    @test struct_eq(ir, ir2)
    @test getregion(ir2, UnifiedIR.owned_regions(ir2, f)[1]).activation === ACT_DEFERRED
    @test interpret(ir2, 100) == 115
end

@testset "isva: trailing args pack; round trip" begin
    b = Builder(name = :va)
    f = append_stmt!(b, K"closure", op_inline(CLOSURE_FLAG_ISVA); type = Any)
    open_closure_body!(b, f)
    x = append_stmt!(b, K"region_arg"; type = Int64)
    rest = append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"return", rest)
    close_region!(b)
    r = append_stmt!(b, K"call", f, 1, 2, 3; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir) == (2, 3)
    txt = print_ir(ir)
    @test occursin("...", txt)
    ir2 = parse_ir(txt)
    @test struct_eq(ir, ir2)
    @test interpret(ir2) == (2, 3)
end

@testset "shared mutation through cell_shared, both directions" begin
    b = Builder(name = :share)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 1)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
    g2 = append_stmt!(b, K"test.add", g, 100; type = Int64)
    append_stmt!(b, K"cell_set", op_stmt(c), g2)      # store INSIDE the closure
    append_stmt!(b, K"return", g2)
    close_region!(b)
    r1 = append_stmt!(b, K"call", f; type = Int64)    # reads 1  -> 101, stores 101
    append_stmt!(b, K"cell_set", c, 5)                # store AFTER creation
    r2 = append_stmt!(b, K"call", f; type = Int64)    # reads 5  -> 105 (sees the later store)
    gf = append_stmt!(b, K"cell_get", c; type = Int64) # frame reads the closure's store: 105
    t1 = append_stmt!(b, K"test.mul", r1, 10000; type = Int64)
    t2 = append_stmt!(b, K"test.add", t1, gf; type = Int64)
    append_stmt!(b, K"return", t2)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir) == 101 * 10000 + 105
    env = closure_environment(ir, f)
    @test env.cells == [c] && isempty(env.values)
    _ = r2
end

@testset "multi-shot loop: each creation snapshots its iteration's slot" begin
    # loop carries (i, fprev); each iteration creates a closure capturing the
    # carried arg %i; the loop's fall-out result delivers the LAST closure,
    # which must still see i == 3 (its creation-time snapshot), not the
    # post-loop i == 4.
    b = Builder(name = :snap)
    r = build_loop!(b, 1, 0; type = Any, argtypes = Any[Int64, Any]) do b, args
        i, fprev = args
        f = append_stmt!(b, K"closure"; type = Any)
        open_closure_body!(b, f)
        v = append_stmt!(b, K"test.add", i, 100; type = Int64)
        append_stmt!(b, K"return", v)
        close_region!(b)
        i2 = append_stmt!(b, K"test.add", i, 1; type = Int64)
        cnd = append_stmt!(b, K"test.icmp", :sle, i2, 3; type = Bool)
        append_stmt!(b, K"continue", op_region(UnifiedIR.current_region(b)),
                     cnd, i2, f)
        _ = fprev
    end
    fl = append_stmt!(b, K"extract", op_stmt(r), op_inline(2); type = Any)
    res = append_stmt!(b, K"call", fl; type = Int64)
    append_stmt!(b, K"return", res)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir) == 103           # created when i == 3
end

@testset "maybe-undef capture: UndefVarError at USE time, not creation" begin
    b = Builder(name = :undf)
    doread = append_stmt!(b, K"region_arg"; type = Bool)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)   # never stored
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
    append_stmt!(b, K"return", g)
    close_region!(b)
    z = build_if!(b, doread; type = Any) do b
        r = append_stmt!(b, K"call", f; type = Int64)
        append_stmt!(b, K"result", r)
    end
    UnifiedIR.open_region!(b, z)
    append_stmt!(b, K"result", 0)
    UnifiedIR.close_region!(b)
    append_stmt!(b, K"return", z)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, false) == 0                    # creation alone never throws
    @test_throws UndefVarError interpret(ir, true)     # the READ throws
end

@testset "nested closures: transitive environment snapshot" begin
    b = Builder(name = :nest)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    outer = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, outer)
    x = append_stmt!(b, K"region_arg"; type = Int64)
    inner = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, inner)
    y = append_stmt!(b, K"region_arg"; type = Int64)
    s1 = append_stmt!(b, K"test.add", x, y; type = Int64)   # captures outer param
    s2 = append_stmt!(b, K"test.add", s1, a; type = Int64)  # transitive home capture
    append_stmt!(b, K"return", s2)
    close_region!(b)
    append_stmt!(b, K"return", inner)
    close_region!(b)
    g = append_stmt!(b, K"call", outer, 20; type = Any)     # -> inner closure
    r = append_stmt!(b, K"call", g, 300; type = Int64)      # 20 + 300 + a
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    # the OUTER environment includes the home value the inner body needs
    env = closure_environment(ir, outer)
    @test env.values == [a]
    @test interpret(ir, 1000) == 1320
end

@testset "throw at call time binds to the CALL site's handler (§3.3)" begin
    b = Builder(name = :thr)
    # creation inside try1: must NOT trigger try1's handler
    t1 = append_stmt!(b, K"try"; type = Any)
    UnifiedIR.open_region!(b, t1; kind = REGION_BODY)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"call", GlobalRef(Base, :error), "boom")
    append_stmt!(b, K"unreachable")
    close_region!(b)
    append_stmt!(b, K"result", f)
    close_region!(b)
    UnifiedIR.open_region!(b, t1; kind = REGION_HANDLER)
    append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"result", :creation_caught)
    close_region!(b)
    # call inside try2: the throw unwinds to HERE
    t2 = append_stmt!(b, K"try"; type = Any)
    UnifiedIR.open_region!(b, t2; kind = REGION_BODY)
    r = append_stmt!(b, K"call", t1; type = Any)
    append_stmt!(b, K"result", r)
    close_region!(b)
    UnifiedIR.open_region!(b, t2; kind = REGION_HANDLER)
    append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"result", :call_caught)
    close_region!(b)
    append_stmt!(b, K"return", t2)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir) === :call_caught
end

@testset "mode-aware DCE: unused effectful closure is removable (§3.3)" begin
    b = Builder(name = :dcecl)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"test.print", 42)      # effectful body — deferred, uncounted
    append_stmt!(b, K"return")
    close_region!(b)
    append_stmt!(b, K"return", 7)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test dce!(ir) >= 1
    @test verify_ir(ir; level = 1)
    io = IOBuffer()
    @test interpret(ir; io = io) == 7
    @test isempty(String(take!(io)))
    ir, _ = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test nstmts(ir) == 1                    # only the return survives

    # control: a USED closure is not removed, and a creation whose flags are
    # not REMOVABLE is kept even when unused
    b = Builder(name = :dcekeep)
    f = append_stmt!(b, K"closure"; type = Any, flag = UInt32(0))
    open_closure_body!(b, f)
    append_stmt!(b, K"return")
    close_region!(b)
    append_stmt!(b, K"return", 1)
    ir = finish!(b)
    @test dce!(ir) == 0
    @test stmt_kind(ir, f) === K"closure"
end

@testset "verifier negatives: the L1 closure deltas" begin
    # (1) result-feeding class: `result` directly in a closure body
    b = Builder(name = :neg1)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"result", 1)
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b; verify = false)
    @test_throws "`result` in a closure body" verify_ir(ir; level = 1)

    # (1b) ...but a result feeding an `if` INSIDE the body is fine
    b = Builder(name = :neg1b)
    p = append_stmt!(b, K"region_arg"; type = Bool)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    z = build_if!(b, p; type = Int64) do b
        append_stmt!(b, K"result", 1)
    end
    UnifiedIR.open_region!(b, z)
    append_stmt!(b, K"result", 2)
    UnifiedIR.close_region!(b)
    append_stmt!(b, K"return", z)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, true) == 1 && interpret(ir, false) == 2

    # (2) cell-class boundary: frame cell reached from a deferred body
    b = Builder(name = :neg2)
    c = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 1)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
    append_stmt!(b, K"return", g)
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b; verify = false)
    @test_throws "activation boundary" verify_ir(ir; level = 1)

    # (2b) frame cell used ENTIRELY inside one deferred body is legal
    b = Builder(name = :neg2b)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    c = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_set", op_stmt(c), 1)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
    append_stmt!(b, K"return", g)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir) == 1

    # (3) op discipline: operand arity and tag
    b = Builder(name = :neg3)
    f = append_stmt!(b, K"closure", op_inline(1), op_inline(0); type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"return")
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b; verify = false)
    @test_throws "at most one" verify_ir(ir; level = 1)

    b = Builder(name = :neg3b)
    f = append_stmt!(b, K"closure", :notflags; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"return")
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b; verify = false)
    @test_throws "INLINE integer flags" verify_ir(ir; level = 1)

    # (4) region discipline: activation and count
    b = Builder(name = :neg4)
    f = append_stmt!(b, K"closure"; type = Any)
    UnifiedIR.open_region!(b, f; kind = REGION_BODY)   # ACT_IMMEDIATE: illegal
    append_stmt!(b, K"return")
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b; verify = false)
    @test_throws "not ACT_DEFERRED" verify_ir(ir; level = 1)

    b = Builder(name = :neg4b)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"return")
    close_region!(b)
    open_closure_body!(b, f)
    append_stmt!(b, K"return")
    close_region!(b)
    append_stmt!(b, K"return", f)
    ir = finish!(b; verify = false)
    @test_throws "owns 2 live regions" verify_ir(ir; level = 1)

    # (5) only closures own deferred regions
    b = Builder(name = :neg5)
    p = append_stmt!(b, K"region_arg"; type = Bool)
    z = append_stmt!(b, K"if", p; type = Any)
    UnifiedIR.open_region!(b, z; kind = REGION_ARM, activation = ACT_DEFERRED)
    append_stmt!(b, K"result")
    close_region!(b)
    append_stmt!(b, K"return")
    ir = finish!(b; verify = false)
    @test_throws "not owned by a closure" verify_ir(ir; level = 1)

    # (6) cell ops must reference cells
    b = Builder(name = :neg6)
    v = append_stmt!(b, K"test.iconst", op_inline(1); type = Int64)
    g = append_stmt!(b, K"cell_get", op_stmt(v); type = Any)
    append_stmt!(b, K"return", g)
    ir = finish!(b; verify = false)
    @test_throws "is not a cell" verify_ir(ir; level = 1)
end

@testset "closure_environment: ordering, dedup, cells split" begin
    b = Builder(name = :envx)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 7)
    v1 = append_stmt!(b, K"test.add", a, 1; type = Int64)
    v2 = append_stmt!(b, K"test.add", a, 2; type = Int64)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
    s1 = append_stmt!(b, K"test.add", v2, v1; type = Int64)   # v2 referenced before v1
    s2 = append_stmt!(b, K"test.add", s1, v1; type = Int64)   # v1 again (dedup)
    s3 = append_stmt!(b, K"test.add", s2, g; type = Int64)
    append_stmt!(b, K"return", s3)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    env = closure_environment(ir, f)
    @test env.values == [v1, v2]       # ordered by id, deduplicated
    @test env.cells == [c]
    @test interpret(ir, 10) == (12 + 11) + 11 + 7
end
