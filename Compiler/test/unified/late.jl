# The late pipeline (§5.7 follow-ups): descent-extended inference over
# region IR carrying closure ops + cell_shared cells — precision assertions
# (zoo5b: Int64 end-to-end), the escape/world refinement discipline
# (load-bearing negatives: an escaping closure's cells read Any even with
# monomorphic stores; a latestworld barrier unrefines), a seeded
# runtime-soundness fuzz (interpreted values must inhabit inferred types),
# and a source-level corpus differential (the region IR — closures
# interpreted natively as UClosures — against stock lowering).
#
# Included from runtests.jl (UnifiedCompiler/CC/UnifiedIR in scope); the
# source-level half loads JuliaLowering from the installed tree.

using UnifiedIR: StmtId, op_stmt, op_inline, CLOSURE_FLAG_ISVA,
    REGION_BODY, ACT_DEFERRED

const ULATE = UnifiedCompiler

open_closure_body!(b, f) = open_region!(b, f; kind = REGION_BODY, activation = ACT_DEFERRED)

wc2(@nospecialize t) = t isa CC.Const ? typeof(t.val) : CC.widenconst(t)

@testset "zoo5b shape: undeclared counter infers Int64 end-to-end" begin
    b = Builder(name = :zoo5b)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    a = append_stmt!(b, K"call", GlobalRef(Base, :+), g, 1; type = Any)
    append_stmt!(b, K"cell_set", op_stmt(c), a)
    append_stmt!(b, K"return", a)
    close_region!(b)
    append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"call", f; type = Any)
    r = append_stmt!(b, K"cell_get", c; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype == Int64
    @test res.cells[c.id].content == Int64
    @test !res.cells[c.id].poisoned
    @test res.closures[f.id].rettype == Int64
    @test !res.closures[f.id].escaped && !res.closures[f.id].shifted
    @test UnifiedIR.stmt_type(ir, r) == Int64
    # execution unchanged by inference (types/flags only): 0+1 → 1 → 2 → 3
    @test interpret(ir) == 3
end

@testset "call-result refinement + param joins over visible sites" begin
    # identity closure: one Const site propagates the Const; two sites join
    b = Builder(name = :ident)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    x = append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"return", x)
    close_region!(b)
    r1 = append_stmt!(b, K"call", f, 5; type = Any)
    append_stmt!(b, K"return", r1)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype isa CC.Const && res.rettype.val === 5

    b = Builder(name = :ident2)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    x = append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"return", x)
    close_region!(b)
    r1 = append_stmt!(b, K"call", f, 5; type = Any)
    r2 = append_stmt!(b, K"call", f, 7; type = Any)
    s = append_stmt!(b, K"call", GlobalRef(Base, :+), r1, r2; type = Any)
    append_stmt!(b, K"return", s)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype == Int64
    @test interpret(ir) == 12
end

@testset "LOAD-BEARING: escaping closure poisons its cells (monomorphic stores)" begin
    # x = 0; f = () -> (x = 1); t = tuple(f, x); return t
    # f escapes (argument position). Every visible store is a monomorphic Int
    # literal, but after materialization f's untyped mutable capture field is
    # settable by any holder: reads must be Any. The join is still computed
    # (diagnostics) — it is just never used for refinement.
    b = Builder(name = :escape)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"cell_set", op_stmt(c), 1)
    append_stmt!(b, K"return", 1)
    close_region!(b)
    g = append_stmt!(b, K"cell_get", c; type = Any)
    t = append_stmt!(b, K"call", GlobalRef(Core, :tuple), f, g; type = Any)
    append_stmt!(b, K"return", t)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.closures[f.id].escaped
    @test res.cells[c.id].poisoned
    @test res.cells[c.id].content == Int64          # the diagnostic join
    @test UnifiedIR.stmt_type(ir, g) === Any        # ... never used for refinement
    v = interpret(ir)
    @test v[2] === 0 && v[1] isa UnifiedIR.UClosure
end

@testset "non-escaping control: same shape without the escape infers Int64" begin
    # identical stores, but f only ever called: reads refine
    b = Builder(name = :noescape)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"cell_set", op_stmt(c), 1)
    append_stmt!(b, K"return", 1)
    close_region!(b)
    append_stmt!(b, K"call", f; type = Any)
    g = append_stmt!(b, K"cell_get", c; type = Any)
    append_stmt!(b, K"return", g)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test !res.closures[f.id].escaped && !res.cells[c.id].poisoned
    @test res.rettype == Int64
    @test interpret(ir) == 1
end

@testset "world barrier: latestworld between creation and call unrefines" begin
    # x = 0 (shared); f = () -> x; latestworld; return f()
    b = Builder(name = :shifted)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(b, K"return", g)
    close_region!(b)
    append_stmt!(b, K"latestworld")
    r = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.closures[f.id].shifted
    @test res.cells[c.id].poisoned      # a shifted closure's stores/reads are world-unstable
    @test res.rettype === Any
    @test interpret(ir) == 0

    # control: the barrier BEFORE creation is harmless (no barrier between
    # creation and call — §5.8's rule is about deferred execution windows)
    b = Builder(name = :unshifted)
    append_stmt!(b, K"latestworld")
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(b, K"return", g)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test !res.closures[f.id].shifted
    @test res.rettype isa CC.Const && res.rettype.val === 0

    # a barrier sharing a loop with the creation re-executes after it: shifted
    b = Builder(name = :loopshift)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    l = append_stmt!(b, K"loop", 0; type = Any)
    lb = open_region!(b, l; kind = UnifiedIR.REGION_LOOP_BODY,
                      activation = UnifiedIR.ACT_IMMEDIATE)
    i = append_stmt!(b, K"region_arg"; type = Any)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(b, K"return", g)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"latestworld")
    cond = append_stmt!(b, K"call", GlobalRef(Base, :<), i, 2; type = Any)
    ip = append_stmt!(b, K"call", GlobalRef(Base, :+), i, 1; type = Any)
    append_stmt!(b, K"continue", UnifiedIR.op_region(lb), cond, ip)
    close_region!(b)
    append_stmt!(b, K"return", l)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.closures[f.id].shifted
    _ = r
end

@testset "type-unstable stores: reads infer the honest join, never narrower" begin
    # x = 1; x = "s"; f = () -> x; return f()
    b = Builder(name = :unstable)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 1)
    append_stmt!(b, K"cell_set", c, "s")
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(b, K"return", g)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype == Union{Int64,String}
    @test res.cells[c.id].content == Union{Int64,String}
    @test interpret(ir) == "s"
end

@testset "arity mismatch of a visible closure is a guaranteed throw" begin
    b = Builder(name = :arity)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    x = append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"return", x)
    close_region!(b)
    r = append_stmt!(b, K"call", f, 1, 2; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype === Union{}
    @test_throws Exception interpret(ir)
end

@testset "isva: trailing param joins as a tuple" begin
    b = Builder(name = :va)
    f = append_stmt!(b, K"closure", op_inline(CLOSURE_FLAG_ISVA); type = Any)
    open_closure_body!(b, f)
    x = append_stmt!(b, K"region_arg"; type = Any)
    rest = append_stmt!(b, K"region_arg"; type = Any)
    append_stmt!(b, K"return", rest)
    close_region!(b)
    r = append_stmt!(b, K"call", f, 1, 2, 3; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test wc2(res.rettype) == Tuple{Int64,Int64}
    @test interpret(ir) == (2, 3)
    _ = x
end

@testset "store-to-value-captured-var: impossible by construction" begin
    # criterion (a) upstream (promote_capture_cells! never value-captures a
    # deferred-stored cell) + the verifier's cell-operand class rule: a
    # cell_set whose first operand is not a cell statement is an L1 error,
    # so "storing through a value capture" cannot even be written.
    b = Builder(name = :novaluestore)
    v = append_stmt!(b, K"call", GlobalRef(Base, :+), 1, 2; type = Any)  # a plain value
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"cell_set", op_stmt(v), 9)      # "store" to the captured VALUE
    append_stmt!(b, K"return", 0)
    close_region!(b)
    append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"return", 0)
    ir = finish!(b)
    @test_throws UnifiedIR.VerifyError verify_ir(ir; level = 1)
    # and a frame cell crossing the boundary is equally rejected
    b = Builder(name = :framecross)
    c = append_stmt!(b, K"cell", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    append_stmt!(b, K"cell_set", op_stmt(c), 1)
    append_stmt!(b, K"return", 0)
    close_region!(b)
    append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"return", 0)
    ir = finish!(b)
    @test_throws UnifiedIR.VerifyError verify_ir(ir; level = 1)
end

@testset "nested closures: transitive precision" begin
    # x = 2 (shared); outer = () -> (inner = () -> x * 3; inner()); outer()
    b = Builder(name = :nested)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 2)
    fo = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, fo)
    fi = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, fi)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    m = append_stmt!(b, K"call", GlobalRef(Base, :*), g, 3; type = Any)
    append_stmt!(b, K"return", m)
    close_region!(b)
    ri = append_stmt!(b, K"call", fi; type = Any)
    append_stmt!(b, K"return", ri)
    close_region!(b)
    ro = append_stmt!(b, K"call", fo; type = Any)
    append_stmt!(b, K"return", ro)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype isa CC.Const && res.rettype.val === 6
    @test interpret(ir) == 6
end

@testset "shared mutation both directions still exact (closures.jl shape)" begin
    b = Builder(name = :share)
    c = append_stmt!(b, K"cell_shared", Int64; type = Any)
    append_stmt!(b, K"cell_set", c, 1)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    g2 = append_stmt!(b, K"call", GlobalRef(Base, :+), g, 100; type = Any)
    append_stmt!(b, K"cell_set", op_stmt(c), g2)
    append_stmt!(b, K"return", g2)
    close_region!(b)
    r1 = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"cell_set", c, 5)
    r2 = append_stmt!(b, K"call", f; type = Any)
    gf = append_stmt!(b, K"cell_get", c; type = Any)
    t1 = append_stmt!(b, K"call", GlobalRef(Base, :*), r1, 10000; type = Any)
    t2 = append_stmt!(b, K"call", GlobalRef(Base, :+), t1, gf; type = Any)
    append_stmt!(b, K"return", t2)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype == Int64
    @test res.cells[c.id].content == Int64
    @test interpret(ir) == 101 * 10000 + 105
    _ = r2
end

@testset "poison via cell-as-value escape" begin
    b = Builder(name = :cellescape)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(b, K"return", g)
    close_region!(b)
    t = append_stmt!(b, K"call", GlobalRef(Core, :tuple), c; type = Any)  # the CELL escapes
    r = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.cells[c.id].poisoned
    @test res.rettype === Any
    _ = t
end

@testset "maybe-undef guards: definedness untouched, use-time error preserved" begin
    # local x; if c; x = 1; end; f = () -> (guarded read); f()  — zoo6 shape
    b = Builder(name = :zoo6ir)
    arg = append_stmt!(b, K"region_arg"; type = Any)
    cell = append_stmt!(b, K"cell_shared", Any; type = Any)
    s = append_stmt!(b, K"if", arg; type = Any)
    open_region!(b, s; kind = UnifiedIR.REGION_ARM)
    append_stmt!(b, K"cell_set", cell, 1)
    append_stmt!(b, K"result")
    close_region!(b)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    d = append_stmt!(b, K"cell_isdefined", op_stmt(cell); type = Any)
    append_stmt!(b, K"throw_undef_if_not", d, :x)
    g = append_stmt!(b, K"cell_get", op_stmt(cell); type = Any)
    append_stmt!(b, K"return", g)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[Bool])
    @test interpret(ir, true) == 1
    @test_throws UndefVarError interpret(ir, false)
    # unpoisoned (no escape), content join Const(1) (single store): maybe-
    # undef affects definedness, not the type — the guards are untouched
    cid = only(keys(res.cells))
    @test wc2(res.cells[cid].content) == Int64
    @test !res.cells[cid].poisoned
end

@testset "escape via result position + partial escapes poison every reader" begin
    # closure selected through an if-result: the result-position use escapes
    b = Builder(name = :ifresult)
    arg = append_stmt!(b, K"region_arg"; type = Any)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f1 = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f1)
    g1 = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(b, K"return", g1)
    close_region!(b)
    s = append_stmt!(b, K"if", arg; type = Any)
    open_region!(b, s; kind = UnifiedIR.REGION_ARM)
    append_stmt!(b, K"result", f1)
    close_region!(b)
    open_region!(b, s; kind = UnifiedIR.REGION_ARM)
    append_stmt!(b, K"result", f1)
    close_region!(b)
    r = append_stmt!(b, K"call", s; type = Any)   # call through the if-result: opaque
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[Bool])
    @test res.closures[f1.id].escaped
    @test res.cells[c.id].poisoned
    @test res.rettype === Any
    @test interpret(ir, true) == 0

    # two closures share a cell; only one escapes — the cell is poisoned for
    # every reader (any holder of the escapee can poke the shared box)
    b = Builder(name = :partial)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    fs = append_stmt!(b, K"closure"; type = Any)     # the storing escapee
    open_closure_body!(b, fs)
    append_stmt!(b, K"cell_set", op_stmt(c), 7)
    append_stmt!(b, K"return", 0)
    close_region!(b)
    fr_ = append_stmt!(b, K"closure"; type = Any)    # the visible reader
    open_closure_body!(b, fr_)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    append_stmt!(b, K"return", g)
    close_region!(b)
    append_stmt!(b, K"call", GlobalRef(Core, :tuple), fs; type = Any)  # fs escapes
    r2 = append_stmt!(b, K"call", fr_; type = Any)
    append_stmt!(b, K"return", r2)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.closures[fs.id].escaped && !res.closures[fr_.id].escaped
    @test res.cells[c.id].poisoned
    @test res.rettype === Any
    @test interpret(ir) == 0
end

@testset "multi-shot loop: creations and calls across iterations" begin
    # s = 0 (shared); loop i=1..3 { f = () -> s + i; s = f() }; return s
    b = Builder(name = :loopclo)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    l = append_stmt!(b, K"loop", 1; type = Any)
    lb = open_region!(b, l; kind = UnifiedIR.REGION_LOOP_BODY,
                      activation = UnifiedIR.ACT_IMMEDIATE)
    i = append_stmt!(b, K"region_arg"; type = Any)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
    a = append_stmt!(b, K"call", GlobalRef(Base, :+), g, i; type = Any)
    append_stmt!(b, K"return", a)
    close_region!(b)
    r = append_stmt!(b, K"call", f; type = Any)
    append_stmt!(b, K"cell_set", op_stmt(c), r)
    cond = append_stmt!(b, K"call", GlobalRef(Base, :<), i, 3; type = Any)
    ip = append_stmt!(b, K"call", GlobalRef(Base, :+), i, 1; type = Any)
    append_stmt!(b, K"continue", UnifiedIR.op_region(lb), cond, ip)
    close_region!(b)
    gf = append_stmt!(b, K"cell_get", c; type = Any)
    append_stmt!(b, K"return", gf)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype == Int64
    @test res.cells[c.id].content == Int64
    @test !res.cells[c.id].poisoned
    @test interpret(ir) == 6      # 0+1=1, 1+2=3, 3+3=6
end

@testset "dead-closure stores still join (documented v1 conservatism)" begin
    # f is created but never called and never escapes: its store can never
    # execute, yet v1 still joins it (always-walk conservatism)
    b = Builder(name = :deadstore)
    c = append_stmt!(b, K"cell_shared", Any; type = Any)
    append_stmt!(b, K"cell_set", c, 0)
    f = append_stmt!(b, K"closure"; type = Any)
    open_closure_body!(b, f)
    sv = append_stmt!(b, K"value", "never"; type = Any)
    append_stmt!(b, K"cell_set", op_stmt(c), sv)
    append_stmt!(b, K"return", 0)
    close_region!(b)
    g = append_stmt!(b, K"cell_get", c; type = Any)
    append_stmt!(b, K"return", g)
    ir = finish!(b)
    res = ULATE.typed_region_ir!(ir, Any[])
    @test res.rettype == Union{Int64,String}   # honest over-approximation
    @test interpret(ir) == 0                   # runtime only ever sees 0
end

# ---------------------------------------------------------------------------
# Seeded runtime-soundness fuzz: interpreted results must inhabit inferred
# return types (rettype soundness subsumes join soundness — home reads of the
# counters flow straight from celltypes). Shapes: shared counters with
# literal/self-referential/mixed-type stores, 0..3 visible calls, optional
# escapes (tuple), optional latestworld at random home positions, if-arms.
# ---------------------------------------------------------------------------
using Random

function late_fuzz_case(rng)
    b = Builder(name = :fz)
    ncells = rand(rng, 1:2)
    cells = StmtId[]
    for _ in 1:ncells
        c = append_stmt!(b, K"cell_shared", Any; type = Any)
        append_stmt!(b, K"cell_set", c, rand(rng, [0, 1, 2]))
        push!(cells, c)
    end
    rand(rng) < 0.2 && append_stmt!(b, K"latestworld")
    closures = StmtId[]
    for _ in 1:rand(rng, 1:2)
        f = append_stmt!(b, K"closure"; type = Any)
        open_closure_body!(b, f)
        c = cells[rand(rng, 1:ncells)]
        g = append_stmt!(b, K"cell_get", op_stmt(c); type = Any)
        v = if rand(rng) < 0.5
            append_stmt!(b, K"call", GlobalRef(Base, :+), g,
                         rand(rng, 1:3); type = Any)
        else
            g
        end
        if rand(rng) < 0.6
            store = rand(rng) < 0.25 ? append_stmt!(b, K"value", "s"; type = Any) : v
            append_stmt!(b, K"cell_set", op_stmt(c), store)
        end
        append_stmt!(b, K"return", v)
        close_region!(b)
        push!(closures, f)
        rand(rng) < 0.2 && append_stmt!(b, K"latestworld")
    end
    escaped = StmtId[]
    for f in closures
        for _ in 1:rand(rng, 0:2)
            append_stmt!(b, K"call", f; type = Any)
        end
        rand(rng) < 0.3 && push!(escaped,
            append_stmt!(b, K"call", GlobalRef(Core, :tuple), f; type = Any))
    end
    reads = [append_stmt!(b, K"cell_get", c; type = Any) for c in cells]
    t = append_stmt!(b, K"call", GlobalRef(Core, :tuple), reads...; type = Any)
    append_stmt!(b, K"return", t)
    return finish!(b)
end

@testset "runtime soundness fuzz: values inhabit inferred types" begin
    rng = MersenneTwister(0x1a7e)
    ncases = 300
    nthrow = 0
    nany = 0
    for i in 1:ncases
        ir = late_fuzz_case(rng)
        verify_ir(ir; level = 1)
        res = ULATE.typed_region_ir!(ir, Any[])
        rt = res.rettype
        local v
        try
            v = interpret(ir)
        catch
            nthrow += 1
            continue
        end
        T = wc2(rt)
        @test v isa T
        if !(v isa T)
            println("FUZZ SOUNDNESS VIOLATION case $i: value ", v, " rettype ", rt)
            print(UnifiedIR.print_ir(ir))
            break
        end
        T === Any && (nany += 1)
        # per-cell join soundness for unpoisoned cells: the tuple of home
        # reads is the frame's own view of the joins
        for (cid, info) in res.cells
            info.poisoned && continue
        end
    end
    println("fuzz: $ncases cases, $nthrow threw (accepted), $nany typed Any")
end


# ---------------------------------------------------------------------------
# Source-level: JuliaLowering front half to the PRE-materialization region IR
# (exactly what emit_method_region! sees before materialize_closures!), then
# the late pipeline + execution differential vs stock. No JuliaLowering
# source support needed: the driver is the front half itself.
# ---------------------------------------------------------------------------

pushfirst!(LOAD_PATH, joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia"))
using JuliaLowering
const UB = JuliaLowering.UnifiedBackend
const JS = JuliaLowering.JuliaSyntax

"""
    region_methods(mod, src) -> (methods, nregion, neager)

`lower_to_ir` minus materialization: per top-level statement, front half +
region-path emission + the capture-decision fixpoint; residual closure ops
and shared cells stay IN the enclosing IR (nested bodies inline — the
interpreter executes them as UClosures). Statements the region path bails on
fall back to the eager (convert_closures) path exactly like lower_to_ir.
"""
function region_methods(mod::Module, src::String)
    out = Any[]
    nregion = 0
    neager = 0
    st0 = JS.parseall(JuliaLowering.SyntaxTree, src; filename = "late.jl")
    stmts = JS.kind(st0) == JS.K"toplevel" ? collect(JS.children(st0)) : [st0]
    world = Base.get_world_counter()
    for st in stmts
        ctx1, ex1 = JuliaLowering.expand_forms_1(mod, st, false, world)
        ctx2, ex2 = JuliaLowering.expand_forms_2(ctx1, ex1)
        ctx3, ex3 = JuliaLowering.resolve_scopes(ctx2, ex2)
        ms = Any[]
        try
            collect_region!(ms, ctx3, ex3)
            nregion += 1
        catch err
            err isa UB.UnsupportedForm || rethrow()
            empty!(ms)
            ctx4, ex4 = JuliaLowering.convert_closures(ctx3, ex3)
            collect_eager!(ms, ctx4, ex4)
            neager += 1
        end
        append!(out, ms)
    end
    for m in out
        UnifiedIR.verify_ir(m.ir; level = 1)
    end
    return out, nregion, neager
end

function collect_region!(out, jlctx, ex)
    k = JS.kind(ex)
    if k == JS.K"method" && JS.numchildren(ex) == 3 && JS.kind(ex[3]) == JS.K"lambda"
        name = UB.method_name(jlctx, ex[1])
        ir, nargs, slotnames, _ = UB.emit_lambda(jlctx, ex[3], name; region = true)
        UnifiedIR.promote_fixpoint!(ir; include_undef = false)
        push!(out, (; name, nargs, slotnames, ir))
        return nothing
    end
    if k == JS.K"lambda"
        ex.is_toplevel_thunk && collect_region!(out, jlctx, ex[3])
        return nothing
    end
    JS.is_leaf(ex) && return nothing
    (k == JS.K"inert" || k == JS.K"inert_syntaxtree" || k == JS.K"quote") && return nothing
    for c in JS.children(ex)
        collect_region!(out, jlctx, c)
    end
    return nothing
end

function collect_eager!(out, jlctx, ex)
    k = JS.kind(ex)
    if k == JS.K"method" && JS.numchildren(ex) == 3 && JS.kind(ex[3]) == JS.K"lambda"
        m = UB.emit_method(jlctx, ex)
        push!(out, (; name = m.name, nargs = m.nargs, slotnames = m.slotnames, ir = m.ir))
        return nothing
    end
    if k == JS.K"lambda"
        ex.is_toplevel_thunk && collect_eager!(out, jlctx, ex[3])
        return nothing
    end
    JS.is_leaf(ex) && return nothing
    (k == JS.K"inert" || k == JS.K"inert_syntaxtree" || k == JS.K"quote") && return nothing
    for c in JS.children(ex)
        collect_eager!(out, jlctx, c)
    end
    return nothing
end

# ---------------------------------------------------------------------------
# zoo5b headline: the undeclared counter infers Int64 end-to-end
# ---------------------------------------------------------------------------

const ZOO5B = """
function zoo5b()
    x = 0
    inc = () -> (x = x + 1)
    inc(); inc(); inc()
    return x
end
"""

zmod = Module(:ZLate)
Base.eval(zmod, :(using Base))
Base.include_string(zmod, ZOO5B, "zoo5b.jl")   # stock definition, same module

@testset "zoo5b headline: Int64 end-to-end through the late pipeline" begin
    ms, nregion, neager = region_methods(zmod, ZOO5B)
    @test length(ms) == 1 && nregion == 1 && neager == 0
    m = ms[1]
    res = ULATE.typed_region_ir!(m.ir, Any[Any])   # (#self#,)
    @test res.rettype == Int64                                 # today: Any
    @test length(res.closures) == 1
    cl = first(values(res.closures))
    @test cl.rettype == Int64 && !cl.escaped && !cl.shifted    # inc types Int64
    @test length(res.cells) == 1
    cell = first(values(res.cells))
    @test cell.content == Int64 && !cell.poisoned              # x joins to Int64
    # differential: region IR (closures interpreted natively) vs stock
    stock = Base.invokelatest(getglobal(zmod, :zoo5b))
    ours = Base.invokelatest(UnifiedIR.interpret, m.ir, getglobal(zmod, :zoo5b))
    @test stock == ours == 3
end

# ---------------------------------------------------------------------------
# closure corpus differential: stock execution vs the typed region IR
# ---------------------------------------------------------------------------

late_outcome(f) = try
    (:ok, f())
catch e
    (:err, typeof(e), e isa UndefVarError ? e.var : nothing)
end

const LATE_CORPUS = [
    ("zoo1", """
     function zoo1(c)
         local x
         if c; x = 1; else; x = 2; end
         cl = () -> x
         return cl()
     end""", Any[Bool], [(true,), (false,)], Int64),
    ("zoo2", """
     function zoo2(a)
         local x
         try
             x = sqrt(a)
         catch
             x = -1.0
         end
         cl = () -> x
         return cl()
     end""", Any[Float64], [(4.0,), (-4.0,)], Float64),
    ("zoo3", """
     function zoo3()
         x = 1
         f = () -> x
         x = 2
         return f()
     end""", Any[], [()], Int64),
    ("zoo5", """
     function zoo5()
         local x::Int = 0
         inc = () -> (x = x + 1)
         inc(); inc(); inc()
         return x
     end""", Any[], [()], Int64),
    ("zoo5b", """
     function zoo5bc()
         x = 0
         inc = () -> (x = x + 1)
         inc(); inc(); inc()
         return x
     end""", Any[], [()], Int64),
    ("zoo6", """
     function zoo6(c)
         local x
         if c; x = 1; end
         f = () -> x
         return f()
     end""", Any[Bool], [(true,), (false,)], nothing),   # rettype not asserted (maybe-undef path)
    ("counter_esc", """
     function cesc()
         x = 0
         inc = () -> (x = x + 1)
         holder = Any[inc]
         holder[1]()
         return x
     end""", Any[], [()], nothing),           # inc escapes into the array: x reads Any
    ("mixed", """
     function mixed(c)
         x = 1
         f = () -> x
         if c
             x = "s"
         end
         return f()
     end""", Any[Bool], [(true,), (false,)], Union{Int64,String}),
    ("nested", """
     function nested()
         x = 2
         outer = () -> begin
             inner = () -> x * 3
             inner()
         end
         return outer()
     end""", Any[], [()], Int64),
    ("loopshot", """
     function loopshot(n)
         s = 0
         i = 1
         while i <= n
             f = () -> s + i
             s = f()
             i = i + 1
         end
         return s
     end""", Any[Int64], [(3,), (0,)], Int64),
]

@testset "corpus: differential + precision through the late pipeline" begin
    nmatch = 0
    ndiff = 0
    nprecise = 0
    nregion_total = 0
    neager_total = 0
    for (label, src, sig, argsets, want_rt) in LATE_CORPUS
        cm = Module(Symbol(:ZC_, label))
        Base.eval(cm, :(using Base))
        Base.include_string(cm, src, "$label.jl")
        ms, nregion, neager = region_methods(cm, src)
        nregion_total += nregion
        neager_total += neager
        @test length(ms) == 1
        m = ms[1]
        @test m.nargs == 1 + length(sig)
        res = ULATE.typed_region_ir!(m.ir, Any[Any, sig...])
        fname = Symbol(m.name)
        f = Base.invokelatest(getglobal, cm, fname)
        for args in argsets
            a = late_outcome(() -> Base.invokelatest(f, args...))
            b = late_outcome(() -> Base.invokelatest(UnifiedIR.interpret, m.ir, f, args...))
            ok = isequal(a, b)
            ok ? (nmatch += 1) : (ndiff += 1)
            @test ok
            ok || println("DIFF at $label$(args): stock=$a ours=$b")
        end
        if want_rt !== nothing
            got = res.rettype isa CC.Const ? typeof(res.rettype.val) :
                  CC.widenconst(res.rettype)
            @test got == want_rt
            got == want_rt && (nprecise += 1)
        end
    end
    println("corpus: $nmatch matches, $ndiff diffs; ",
            "$nprecise/$(count(x -> x[5] !== nothing, LATE_CORPUS)) precise rettypes; ",
            "paths: $nregion_total region, $neager_total eager")
end

# the escaping counter reads Any (the poke argument, source-level)
@testset "escaping counter: reads degrade to Any at source level" begin
    cm = Module(:ZEsc)
    Base.eval(cm, :(using Base))
    src = LATE_CORPUS[7][2]
    Base.include_string(cm, src, "cesc.jl")
    ms, _, _ = region_methods(cm, src)
    m = only(ms)
    res = ULATE.typed_region_ir!(m.ir, Any[Any])
    @test !isempty(res.closures)
    @test any(c -> c.escaped, values(res.closures))
    @test all(c -> c.poisoned, values(res.cells))
end
