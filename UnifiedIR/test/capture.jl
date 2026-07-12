# promote_capture_cells! (§5.7): value-capture promotion of cell_shared
# cells read inside deferred regions. Unit tests mirror the JuliaLowering
# capture-analysis sentinels (julia#15276 zoo) at the IR level — a
# wrong-direction decision here is a silent miscompile, so each case asserts
# the DECISION (does the shared cell survive?) and the OBSERVABLE semantics
# through the reference interpreter. Plus the seeded closure-shape fuzz
# battery with interpreter differentials pre/post promotion.

using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region, StmtId, getop
using Test
using Random

count_kind(ir, k) = count(s -> stmt_kind(ir, s) === k, collect(each_stmt(ir)))
shared_cells(ir) = count_kind(ir, K"cell_shared")
frame_cells(ir) = count_kind(ir, K"cell")

# run the fixpoint in the lowering configuration (maybe-undef stays memory)
function fix!(ir)
    n = promote_fixpoint!(ir; include_undef = false)
    verify_ir(ir; level = 1)
    return n
end

open_deferred!(b, f) = open_region!(b, f; kind = REGION_BODY, activation = ACT_DEFERRED)

# closure capturing `cells...`: body returns the sum of their gets (plus an
# optional param)
function mk_reader!(b, cells; param::Bool = false)
    f = append_stmt!(b, K"closure"; type = Any)
    open_deferred!(b, f)
    acc = nothing
    if param
        acc = append_stmt!(b, K"region_arg"; type = Int64)
    end
    for c in cells
        g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
        acc = acc === nothing ? g : append_stmt!(b, K"test.add", acc, g; type = Int64)
    end
    append_stmt!(b, K"return", acc)
    close_region!(b)
    return f
end

@testset "value capture: single dominating store resolves; cell disappears" begin
    b = Builder(name = :vc)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 41)
    f = mk_reader!(b, [c])
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test interpret(ir) == 41
    @test fix!(ir) > 0
    @test shared_cells(ir) == 0 && frame_cells(ir) == 0
    @test interpret(ir) == 41
    # the closure's derived environment shrank to a pure value (or nothing)
    for s in each_stmt(ir)
        stmt_kind(ir, s) === K"closure" || continue
        env = closure_environment(ir, s)
        @test isempty(env.cells)
    end
end

@testset "arm join resolves (zoo1 shape)" begin
    b = Builder(name = :join)
    p = append_stmt!(b, K"region_arg"; type = Bool)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    z = build_if!(b, p; type = Any) do b
        append_stmt!(b, K"cell_set", op_stmt(c), 1)
        append_stmt!(b, K"result")
    end
    UnifiedIR.open_region!(b, z)
    append_stmt!(b, K"cell_set", op_stmt(c), 2)
    append_stmt!(b, K"result")
    UnifiedIR.close_region!(b)
    f = mk_reader!(b, [c])
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test interpret(ir, true) == 1 && interpret(ir, false) == 2
    fix!(ir)
    @test shared_cells(ir) == 0            # the join IS the point (criterion c)
    @test interpret(ir, true) == 1 && interpret(ir, false) == 2
end

@testset "try join resolves (zoo2 shape)" begin
    b = Builder(name = :tjoin)
    p = append_stmt!(b, K"region_arg"; type = Bool)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    t = append_stmt!(b, K"try"; type = Any)
    UnifiedIR.open_region!(b, t; kind = REGION_BODY)
    z = build_if!(b, p; type = Any) do b
        append_stmt!(b, K"call", GlobalRef(Base, :error), "boom")
        append_stmt!(b, K"result")
    end
    UnifiedIR.open_region!(b, z)
    append_stmt!(b, K"result")
    UnifiedIR.close_region!(b)
    append_stmt!(b, K"cell_set", op_stmt(c), 10)
    append_stmt!(b, K"result")
    close_region!(b)
    UnifiedIR.open_region!(b, t; kind = REGION_HANDLER)
    append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"cell_set", op_stmt(c), -1)
    append_stmt!(b, K"result")
    close_region!(b)
    f = mk_reader!(b, [c])
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test interpret(ir, false) == 10 && interpret(ir, true) == -1
    fix!(ir)
    @test shared_cells(ir) == 0            # exception join via promote_try_cells!
    @test interpret(ir, false) == 10 && interpret(ir, true) == -1
end

@testset "store after creation stays shared (zoo3 sentinel)" begin
    b = Builder(name = :after)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 1)
    f = mk_reader!(b, [c])
    append_stmt!(b, K"cell_set", c, 2)     # the closure must observe this
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test interpret(ir) == 2
    fix!(ir)
    @test shared_cells(ir) == 1            # criterion (b) veto
    @test interpret(ir) == 2
end

@testset "store inside the closure stays shared (zoo5/counter sentinel)" begin
    b = Builder(name = :cnt)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_deferred!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
    g2 = append_stmt!(b, K"test.add", g, 1; type = Int64)
    append_stmt!(b, K"cell_set", op_stmt(c), g2)
    append_stmt!(b, K"return", g2)
    close_region!(b)
    append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"call", f; type = Int64)
    fin = append_stmt!(b, K"cell_get", c; type = Int64)
    append_stmt!(b, K"return", fin)
    ir = finish!(b)
    @test interpret(ir) == 3
    fix!(ir)
    @test shared_cells(ir) == 1            # criterion (a) veto
    @test interpret(ir) == 3
end

@testset "multi-shot backedge hazard stays shared (zoo4 sentinel)" begin
    # closure created in the loop; the cell is stored LATER in the same body:
    # iteration i's closure observes iteration i+1's store — must stay shared.
    # The escaping closure is carried out through the loop result.
    b = Builder(name = :backedge)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    r = build_loop!(b, 1, 0; type = Any, argtypes = Any[Int64, Any]) do b, args
        i, fprev = args
        f = mk_reader!(b, [c])
        append_stmt!(b, K"cell_set", op_stmt(c), i)   # store after the site, same loop
        i2 = append_stmt!(b, K"test.add", i, 1; type = Int64)
        cnd = append_stmt!(b, K"test.icmp", :sle, i2, 3; type = Bool)
        append_stmt!(b, K"continue", op_region(UnifiedIR.current_region(b)), cnd, i2, f)
        _ = fprev
    end
    fl = append_stmt!(b, K"extract", op_stmt(r), op_inline(2); type = Any)
    res = append_stmt!(b, K"call", fl; type = Int64)
    append_stmt!(b, K"return", res)
    ir = finish!(b)
    @test interpret(ir) == 3               # shared: sees the FINAL store, not its iteration's
    fix!(ir)
    @test shared_cells(ir) == 1            # backedge veto
    @test interpret(ir) == 3
end

@testset "fresh cell per iteration cancels the backedge hazard" begin
    # the cell is DECLARED inside the loop (fresh shared box per iteration):
    # a same-iteration store before the site is final for that box.
    b = Builder(name = :fresh)
    r = build_loop!(b, 1, 0; type = Any, argtypes = Any[Int64, Any]) do b, args
        i, fprev = args
        c = append_stmt!(b, K"cell_shared", Int64; type = Any)
        append_stmt!(b, K"cell_set", op_stmt(c), i)
        f = mk_reader!(b, [c])
        i2 = append_stmt!(b, K"test.add", i, 1; type = Int64)
        cnd = append_stmt!(b, K"test.icmp", :sle, i2, 3; type = Bool)
        append_stmt!(b, K"continue", op_region(UnifiedIR.current_region(b)), cnd, i2, f)
        _ = fprev
    end
    fl = append_stmt!(b, K"extract", op_stmt(r), op_inline(2); type = Any)
    res = append_stmt!(b, K"call", fl; type = Int64)
    append_stmt!(b, K"return", res)
    ir = finish!(b)
    @test interpret(ir) == 3               # last iteration's snapshot
    fix!(ir)
    @test shared_cells(ir) == 0            # cancellation: value capture legal
    @test interpret(ir) == 3
end

@testset "maybe-undef keeps the shared cell and use-time UndefVarError" begin
    b = Builder(name = :undef)
    p = append_stmt!(b, K"region_arg"; type = Bool)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    z = build_if!(b, p; type = Any) do b
        append_stmt!(b, K"cell_set", op_stmt(c), 1)
        append_stmt!(b, K"result")
    end
    UnifiedIR.open_region!(b, z)
    append_stmt!(b, K"result")             # else arm: no store (maybe-undef)
    UnifiedIR.close_region!(b)
    f = mk_reader!(b, [c])
    r = append_stmt!(b, K"call", f; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test interpret(ir, true) == 1
    @test_throws UndefVarError interpret(ir, false)
    fix!(ir)
    @test shared_cells(ir) == 1            # criterion (c): unresolved probe
    @test interpret(ir, true) == 1
    @test_throws UndefVarError interpret(ir, false)   # still a USE-time error
end

@testset "one poisoned site keeps the whole cell shared" begin
    b = Builder(name = :twosite)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 1)
    f1 = mk_reader!(b, [c])
    append_stmt!(b, K"cell_set", c, 2)     # after f1, before f2
    f2 = mk_reader!(b, [c])
    r1 = append_stmt!(b, K"call", f1; type = Int64)   # must see 2 (shared)
    r2 = append_stmt!(b, K"call", f2; type = Int64)
    t1 = append_stmt!(b, K"test.mul", r1, 100; type = Int64)
    t2 = append_stmt!(b, K"test.add", t1, r2; type = Int64)
    append_stmt!(b, K"return", t2)
    ir = finish!(b)
    @test interpret(ir) == 202
    fix!(ir)
    @test shared_cells(ir) == 1
    @test interpret(ir) == 202
end

@testset "nested closure: transitive read resolves through the outer site" begin
    b = Builder(name = :nested)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 7)
    outer = append_stmt!(b, K"closure"; type = Any)
    open_deferred!(b, outer)
    inner = append_stmt!(b, K"closure"; type = Any)
    open_deferred!(b, inner)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)   # two boundaries deep
    append_stmt!(b, K"return", g)
    close_region!(b)
    ri = append_stmt!(b, K"call", inner; type = Int64)
    append_stmt!(b, K"return", ri)
    close_region!(b)
    r = append_stmt!(b, K"call", outer; type = Int64)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test interpret(ir) == 7
    fix!(ir)
    @test shared_cells(ir) == 0
    @test interpret(ir) == 7
end

@testset "activation-local promotion: frame cells inside a deferred body" begin
    # a frame cell used ENTIRELY inside one deferred region promotes through
    # the ordinary passes (the §5.1 audit case: activation guards must not
    # block activation-LOCAL work)
    b = Builder(name = :actlocal)
    f = append_stmt!(b, K"closure"; type = Any)
    open_deferred!(b, f)
    n = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_set", op_stmt(c), 0)
    r = build_loop!(b, 1; type = Any, argtypes = Any[Int64]) do b, args
        i, = args
        g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
        s2 = append_stmt!(b, K"test.add", g, i; type = Int64)
        append_stmt!(b, K"cell_set", op_stmt(c), s2)
        i2 = append_stmt!(b, K"test.add", i, 1; type = Int64)
        cnd = append_stmt!(b, K"test.icmp", :sle, i2, n; type = Bool)
        append_stmt!(b, K"continue", op_region(UnifiedIR.current_region(b)), cnd, i2)
    end
    _ = r
    fin = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
    append_stmt!(b, K"return", fin)
    close_region!(b)
    res = append_stmt!(b, K"call", f, 4; type = Int64)
    append_stmt!(b, K"return", res)
    ir = finish!(b)
    @test interpret(ir) == 10              # 1+2+3+4
    fix!(ir)
    @test frame_cells(ir) == 0             # promoted inside the deferred body
    @test interpret(ir) == 10
end

@testset "promote_capture_cells!: no-op on closure-free IR" begin
    b = Builder(name = :plain)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, a)
    g = append_stmt!(b, K"cell_get", c; type = Int64)
    append_stmt!(b, K"return", g)
    ir = finish!(b)
    txt_before = print_ir(ir)
    editable(ir)
    @test UnifiedIR.promote_capture_cells!(ir) == 0
    ir, _ = compact!(ir)
    @test print_ir(ir) == txt_before       # untouched
    # boundary parameter: only :deferred exists (the await seam is documented)
    editable(ir)
    @test_throws ErrorException UnifiedIR.promote_capture_cells!(ir; boundary = :resume)
end

# ---------------------------------------------------------------------------
# Seeded closure-shape fuzz: random home bodies over shared cells with
# closure creations/calls/stores/joins/loops; differential interpretation
# before and after the promotion fixpoint, in both fixpoint configurations,
# through a textual round trip. Histogram reported honestly.
# ---------------------------------------------------------------------------

function cfz_outcome(ir, v)
    io = IOBuffer()
    try
        (:ok, interpret(ir, v; io = io), String(take!(io)))
    catch e
        (:err, e isa UndefVarError ? :undef : nameof(typeof(e)), String(take!(io)))
    end
end

function cfz_gen(rng, stats)
    b = Builder(name = :cfz)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    acc = append_stmt!(b, K"cell", Int64; type = Any)     # result accumulator
    append_stmt!(b, K"cell_set", acc, 0)
    cells = StmtId[]
    for _ in 1:rand(rng, 1:2)
        c = append_stmt!(b, K"cell_shared", Int64; type = Any)
        push!(cells, c)
        if rand(rng) < 0.8
            append_stmt!(b, K"cell_set", c, rand(rng, -5:5))
        else
            stats[:maybe_undef] += 1
        end
    end
    closures = StmtId[]

    function ev_closure!(vis_cells)
        caps = [c for c in vis_cells if rand(rng) < 0.6]
        isempty(caps) && push!(caps, rand(rng, vis_cells))
        f = append_stmt!(b, K"closure"; type = Any)
        open_region!(b, f; kind = REGION_BODY, activation = ACT_DEFERRED)
        x = append_stmt!(b, K"region_arg"; type = Int64)
        v = x
        for c in caps
            g = append_stmt!(b, K"cell_get", op_stmt(c); type = Int64)
            v = append_stmt!(b, K"test.add", v, g; type = Int64)
        end
        if rand(rng) < 0.15
            append_stmt!(b, K"cell_set", op_stmt(rand(rng, caps)), v)
            stats[:store_in_closure] += 1
        end
        if rand(rng) < 0.15                                  # nested closure
            inner = append_stmt!(b, K"closure"; type = Any)
            open_region!(b, inner; kind = REGION_BODY, activation = ACT_DEFERRED)
            gi = append_stmt!(b, K"cell_get", op_stmt(rand(rng, caps)); type = Int64)
            append_stmt!(b, K"return", gi)
            close_region!(b)
            rv = append_stmt!(b, K"call", inner; type = Int64)
            v = append_stmt!(b, K"test.add", v, rv; type = Int64)
            stats[:nested] += 1
        end
        append_stmt!(b, K"return", v)
        close_region!(b)
        stats[:closures] += 1
        return f
    end
    function ev_call!(vis_closures)
        isempty(vis_closures) && return
        f = rand(rng, vis_closures)
        r = append_stmt!(b, K"call", f, rand(rng, 0:9); type = Int64)
        g = append_stmt!(b, K"cell_get", acc; type = Int64)
        s = append_stmt!(b, K"test.add", g, r; type = Int64)
        append_stmt!(b, K"cell_set", acc, s)
        stats[:calls] += 1
        return
    end
    function ev_store!(vis_cells)
        append_stmt!(b, K"cell_set", op_stmt(rand(rng, vis_cells)),
                     rand(rng, -9:9))
        stats[:stores] += 1
    end
    function ev_ifjoin!(vis_cells)
        c = rand(rng, vis_cells)
        cnd = append_stmt!(b, K"test.icmp", :sgt, a, rand(rng, -3:3); type = Bool)
        z = build_if!(b, cnd; type = Any) do b
            append_stmt!(b, K"cell_set", op_stmt(c), rand(rng, 10:19))
            append_stmt!(b, K"result")
        end
        UnifiedIR.open_region!(b, z)
        append_stmt!(b, K"cell_set", op_stmt(c), rand(rng, 20:29))
        append_stmt!(b, K"result")
        UnifiedIR.close_region!(b)
        stats[:ifjoins] += 1
    end
    function ev_loop!(vis_cells, vis_closures)
        bound = rand(rng, 1:3)
        s = append_stmt!(b, K"loop", op_inline(Int64(1)); type = Any)
        body = open_region!(b, s; kind = REGION_LOOP_BODY)
        i = append_stmt!(b, K"region_arg"; type = Int64)
        inner_cells = copy(vis_cells)
        inner_closures = copy(vis_closures)
        if rand(rng) < 0.5                                    # fresh decl shape
            c = append_stmt!(b, K"cell_shared", Int64; type = Any)
            append_stmt!(b, K"cell_set", op_stmt(c), i)
            push!(inner_cells, c)
            stats[:fresh_decl] += 1
        end
        for _ in 1:rand(rng, 1:3)
            roll = rand(rng)
            if roll < 0.35
                ev_store!(inner_cells)
            elseif roll < 0.7
                push!(inner_closures, ev_closure!(inner_cells))
            else
                ev_call!(inner_closures)
            end
        end
        i2 = append_stmt!(b, K"test.add", i, 1; type = Int64)
        cnd = append_stmt!(b, K"test.icmp", :sle, i2, bound; type = Bool)
        append_stmt!(b, K"continue", op_region(body), cnd, i2)
        close_region!(b)
        stats[:loops] += 1
    end

    for _ in 1:rand(rng, 3:6)
        roll = rand(rng)
        if roll < 0.25
            push!(closures, ev_closure!(cells))
        elseif roll < 0.45
            ev_call!(closures)
        elseif roll < 0.65
            ev_store!(cells)
        elseif roll < 0.8
            ev_ifjoin!(cells)
        else
            ev_loop!(cells, closures)
        end
    end
    fin = append_stmt!(b, K"cell_get", acc; type = Int64)
    out = append_stmt!(b, K"test.add", fin, a; type = Int64)
    append_stmt!(b, K"return", out)
    return finish!(b)
end

@testset "closure-shape capture fuzz (seeded differential)" begin
    rng = Xoshiro(0xc10)
    stats = Dict{Symbol,Int}(k => 0 for k in
        (:closures, :calls, :stores, :ifjoins, :loops, :fresh_decl,
         :store_in_closure, :maybe_undef, :nested))
    argvals = (Int64(-3), Int64(0), Int64(5))
    iters = 300
    failures = String[]
    promoted_total = 0
    survived_total = 0
    for i in 1:iters
        try
            ir = cfz_gen(rng, stats)
            verify_ir(ir; level = 1)
            expected = [cfz_outcome(ir, v) for v in argvals]
            # copy through the textual round trip (also fuzzes closure text)
            ir2 = parse_ir(print_ir(ir))
            struct_eq(ir, ir2) || push!(failures, "iter $i: round trip not structural")
            # config A: lowering fixpoint (no definedness-as-data)
            promoted_total += promote_fixpoint!(ir; include_undef = false)
            verify_ir(ir; level = 1)
            got = [cfz_outcome(ir, v) for v in argvals]
            got == expected ||
                push!(failures, "iter $i: include_undef=false differential mismatch")
            survived_total += shared_cells(ir)
            # config B: optimizer fixpoint (with the undef split)
            promote_fixpoint!(ir2; include_undef = true)
            verify_ir(ir2; level = 1)
            got2 = [cfz_outcome(ir2, v) for v in argvals]
            got2 == expected ||
                push!(failures, "iter $i: include_undef=true differential mismatch")
        catch e
            push!(failures, "iter $i: " * sprint(showerror, e))
        end
        length(failures) > 10 && break
    end
    @info "capture fuzz shape histogram ($iters iters)" stats... promoted_total survived_total
    @test stats[:closures] > 200            # the shapes DO generate closures
    @test stats[:calls] > 100
    @test isempty(failures)
    isempty(failures) || foreach(println, first(failures, 10))
end
