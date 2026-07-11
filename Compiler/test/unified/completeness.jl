# Join-point completeness regression tests (design doc §6 "Join-point
# completeness"; harness in src/unified/completeness.jl, fuzzer in
# cellfuzz.jl, full-scale runs in bench/unified_completeness.jl).

include("cellfuzz.jl")
using .CellFuzz: count_cellops, outcome

const UC = UnifiedCompiler

@testset "promote_arm_cells!: the gcd shape (swap in sibling arm)" begin
    # a, b conditionally swapped in one arm; both read after the join
    function swapir(; onearmed::Bool)
        b = Builder(name = :swap)
        x = append_stmt!(b, K"region_arg"; type = Any)
        y = append_stmt!(b, K"region_arg"; type = Any)
        ca = append_stmt!(b, K"cell", Any; type = Any)
        cb = append_stmt!(b, K"cell", Any; type = Any)
        append_stmt!(b, K"cell_set", ca, x)
        append_stmt!(b, K"cell_set", cb, y)
        c = append_stmt!(b, K"call", GlobalRef(Base, :<), x, y; type = Any)
        s = append_stmt!(b, K"if", c; type = Any)
        open_region!(b, s; kind = REGION_ARM)
        g1 = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(ca); type = Any)
        g2 = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(cb); type = Any)
        append_stmt!(b, K"cell_set", ca, g2)
        append_stmt!(b, K"cell_set", cb, g1)
        append_stmt!(b, K"result")
        close_region!(b)
        if !onearmed
            open_region!(b, s; kind = REGION_ARM)
            append_stmt!(b, K"result")
            close_region!(b)
        end
        ra = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(ca); type = Any)
        rb = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(cb); type = Any)
        v = append_stmt!(b, K"call", GlobalRef(Base, :-), ra, rb; type = Any)
        append_stmt!(b, K"return", v)
        finish!(b)
    end
    for onearmed in (false, true)
        ir = swapir(; onearmed)
        ref = [outcome(ir, a...) for a in ((2, 5), (5, 2), (3, 3))]
        ir = UC.promotion_fixpoint!(ir)
        @test verify_ir(ir; level = 1)
        @test count_cellops(ir) == 0
        @test [outcome(ir, a...) for a in ((2, 5), (5, 2), (3, 3))] == ref
        @test isempty(UC.classify_residual_cells(ir))
    end
end

@testset "promote_arm_cells!: nested depth 3 + all-diverging-but-one" begin
    # three nested one-armed ifs each mutating the cell; innermost sibling
    # arm returns (diverges past every join)
    b = Builder(name = :nest3)
    x = append_stmt!(b, K"region_arg"; type = Any)
    c = append_stmt!(b, K"cell", Any; type = Any)
    append_stmt!(b, K"cell_set", c, x)
    conds = StmtId[]
    for lvl in 1:3
        cd = append_stmt!(b, K"call", GlobalRef(Base, :<), UnifiedIR.op_stmt(x),
                          UnifiedIR.op_inline(Int64(10 * lvl)); type = Any)
        s = append_stmt!(b, K"if", cd; type = Any)
        open_region!(b, s; kind = REGION_ARM)
        g = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(c); type = Any)
        v = append_stmt!(b, K"call", GlobalRef(Base, :+), g, UnifiedIR.op_inline(Int64(lvl)); type = Any)
        append_stmt!(b, K"cell_set", c, v)
        push!(conds, s)
    end
    # innermost: a diverging sibling arm (returns)
    inner = append_stmt!(b, K"call", GlobalRef(Base, :(==)), UnifiedIR.op_stmt(x),
                         UnifiedIR.op_inline(Int64(1)); type = Any)
    si = append_stmt!(b, K"if", inner; type = Any)
    open_region!(b, si; kind = REGION_ARM)
    append_stmt!(b, K"return", UnifiedIR.op_inline(Int64(-99)))
    close_region!(b)
    open_region!(b, si; kind = REGION_ARM)
    g = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(c); type = Any)
    append_stmt!(b, K"cell_set", c, g)   # degenerate store (joins with one live arm)
    append_stmt!(b, K"result")
    close_region!(b)
    for _ in 1:3
        append_stmt!(b, K"result")
        close_region!(b)
    end
    r = append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(c); type = Any)
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    ref = [outcome(ir, a) for a in (0, 1, 5, 15, 25, 99)]
    ir = UC.promotion_fixpoint!(ir)
    @test verify_ir(ir; level = 1)
    @test count_cellops(ir) == 0
    @test [outcome(ir, a) for a in (0, 1, 5, 15, 25, 99)] == ref
end

@testset "arm promotion refusals: maybe-undef and throw-edge" begin
    # maybe-undef: no dominating store, one-arm-only store, isdefined after
    b = Builder(name = :mundef)
    x = append_stmt!(b, K"region_arg"; type = Any)
    c = append_stmt!(b, K"cell", Any; type = Any)
    cd = append_stmt!(b, K"call", GlobalRef(Base, :<), UnifiedIR.op_stmt(x),
                      UnifiedIR.op_inline(Int64(0)); type = Any)
    s = append_stmt!(b, K"if", cd; type = Any)
    open_region!(b, s; kind = REGION_ARM)
    append_stmt!(b, K"cell_set", c, x)
    append_stmt!(b, K"result")
    close_region!(b)
    d = append_stmt!(b, K"cell_isdefined", UnifiedIR.op_stmt(c); type = Any)
    append_stmt!(b, K"return", d)
    ir = finish!(b)
    ref = [outcome(ir, a) for a in (-1, 1)]
    ir = UC.promotion_fixpoint!(ir)
    @test verify_ir(ir; level = 1)
    res = UC.classify_residual_cells(ir)
    @test length(res) == 1 && res[1][2] === :maybe_undef_read
    @test [outcome(ir, a) for a in (-1, 1)] == ref

    # throw-edge: store in try body observed by the handler MUST refuse
    b2 = Builder(name = :tedge)
    x2 = append_stmt!(b2, K"region_arg"; type = Any)
    c2 = append_stmt!(b2, K"cell", Any; type = Any)
    append_stmt!(b2, K"cell_set", c2, x2)
    t = append_stmt!(b2, K"try"; type = Any)
    open_region!(b2, t; kind = REGION_BODY)
    v = append_stmt!(b2, K"call", CellFuzz.fzthrow, UnifiedIR.op_stmt(x2); type = Any)
    append_stmt!(b2, K"cell_set", c2, v)
    append_stmt!(b2, K"result", UnifiedIR.op_inline(Int64(0)))
    close_region!(b2)
    open_region!(b2, t; kind = REGION_HANDLER)
    append_stmt!(b2, K"region_arg"; type = Any)
    hg = append_stmt!(b2, K"cell_get", UnifiedIR.op_stmt(c2); type = Any)
    append_stmt!(b2, K"result", hg)
    close_region!(b2)
    append_stmt!(b2, K"return", t)
    ir2 = finish!(b2)
    ref2 = [outcome(ir2, a) for a in (1, 7)]
    ir2 = UC.promotion_fixpoint!(ir2)
    @test verify_ir(ir2; level = 1)
    res2 = UC.classify_residual_cells(ir2)
    @test length(res2) == 1 && res2[1][2] === :throw_edge_handler
    @test [outcome(ir2, a) for a in (1, 7)] == ref2
end

@testset "the real thing: gcd and _gcd fully promoted" begin
    for (f, ats) in ((Base.gcd, Any[Int, Int]), (Base._gcd, Any[Int, Int]))
        ir = UC.typed_ir(f, ats)
        @test count_cellops(ir) == 0
        res = UC.classify_residual_cells(ir)
        @test isempty(res)
    end
    # DF correspondence on gcd's pre-promotion body: no :missing
    ir0 = UC.structure_prep!(UC.lowered_ir(Base.gcd, Any[Int, Int]))
    r = UC.df_correspondence(ir0)
    @test r.ok
    @test all(x -> x.status === :match, r.results)
end

@testset "fuzz battery (800 cases, seeded)" begin
    s = CellFuzz.run_cases(UC, 800; seed = 20260711, dfevery = 40)
    @test s.diffs[] == 0
    @test s.verifyfails[] == 0
    @test s.unclassified[] == 0
    @test s.dfmissing[] == 0
    @test isempty(s.failures)
    println("fuzz battery: ", s.cases[], " cases; residuals ", s.residuals,
            "; cells ", s.cells_pre[], " -> ", s.cells_post[],
            "; df cells=", s.dfcells[], " match=", s.dfmatch[], " extras=", s.dfextra[])
end

# -- island mem2reg (§6 fourth join-point class: island phis) ----------------

# Multi-entry loop: structurize! must leave a cfg island (irreducible), and
# the island pass promotes the cells across its internal backedge.
function _cmpl_mentry(n, b)
    s = 0
    i = 0
    if b
        @goto mid
    end
    @label top
    i += 1
    @label mid
    s += i
    if i < n
        @goto top
    end
    return s
end

# Irreducible sub-graph INSIDE a while loop: the outer cells are carried
# across iterations THROUGH the island (its exits are sealed continues), so
# the island pass must refuse them — deleting the stores would leave the next
# iteration's incoming read stale (the backedge-staleness rule).
function _cmpl_stale(n, b)
    total = 0
    k = 0
    while k < n
        k += 1
        if b
            @goto mid
        end
        @label top
        total += 1
        @label mid
        total += k
        if total < 0
            @goto top
        end
    end
    return total
end

@testset "promote_island_cells!: phi placement, backedge, staleness refusal" begin
    # diamond with a join-block phi, in the textual island form
    src = """
    func @isl(%1::Any, %2::Int64) -> Int64 {
      %3 = cell const type Int64 :: Any
      cell_set %3, %2
      %5 = cfg () :: Int64 {
      ^bb2():
        %6 = cell_get %3 :: Int64
        %7 = call global Base.slt_int, %6, const 10 :: Bool
        br_if %7 (^bb3) (^bb4)
      ^bb3():
        %9 = cell_get %3 :: Int64
        %10 = call global Base.mul_int, %9, %9 :: Int64
        cell_set %3, %10
        goto (^bb5)
      ^bb4():
        %13 = cell_get %3 :: Int64
        %14 = call global Base.add_int, %13, const 1 :: Int64
        cell_set %3, %14
        goto (^bb5)
      ^bb5():
        %17 = cell_get %3 :: Int64
        result %17
      }
      return %5
    }
    """
    pre = [UnifiedIR.interpret(UnifiedIR.parse_ir(src), nothing, x) for x in (3, 42)]
    ir = UnifiedIR.parse_ir(src)
    UnifiedIR.editable(ir)
    UC.PROMOTION_TRACE[] = Tuple{Symbol,Int,Int}[]
    n = UC.promote_island_cells!(ir)
    tr = copy(UC.PROMOTION_TRACE[]::Vector{Tuple{Symbol,Int,Int}})
    UC.PROMOTION_TRACE[] = nothing
    ir, _ = UnifiedIR.compact!(ir)
    @test n == 1
    @test UnifiedIR.verify_ir(ir; level = 1)
    # exactly one island phi, at the join block (^bb5 = region 5 pre-compact)
    @test tr == [(:island_phi, 5, 3)]
    @test [UnifiedIR.interpret(ir, nothing, x) for x in (3, 42)] == pre == [9, 43]
    # and the full fixpoint leaves no cell ops at all
    irf = UC.promotion_fixpoint!(UnifiedIR.parse_ir(src))
    @test count_cellops(irf) == 0

    W = Base.get_world_counter()
    # multi-entry loop: a real island survives structuring; promotes fully
    irc = UC.structure_prep!(UC.lowered_ir(_cmpl_mentry, Any[Int, Bool]; world = W))
    @test any(s -> UnifiedIR.stmt_kind(irc, s) === UnifiedIR.@K_str("cfg"),
              UnifiedIR.each_stmt(irc))
    irc = UC.promotion_fixpoint!(irc)
    @test UnifiedIR.verify_ir(irc; level = 1)
    @test count_cellops(irc) == 0
    @test UnifiedIR.interpret(irc, _cmpl_mentry, 5, false) == _cmpl_mentry(5, false) == 15
    @test UnifiedIR.interpret(irc, _cmpl_mentry, 5, true) == _cmpl_mentry(5, true) == 15
    # DF correspondence agrees with stock placement on the island body
    rc = UC.df_correspondence(UC.structure_prep!(UC.lowered_ir(_cmpl_mentry, Any[Int, Bool]; world = W)))
    @test rc.ok
    @test all(x -> x.status === :match, rc.results)

    # island inside a while loop: loop-carried cells refuse (stay classified)
    irb = UC.structure_prep!(UC.lowered_ir(_cmpl_stale, Any[Int, Bool]; world = W))
    @test any(s -> UnifiedIR.stmt_kind(irb, s) === UnifiedIR.@K_str("cfg"),
              UnifiedIR.each_stmt(irb))
    irb = UC.promotion_fixpoint!(irb)
    @test UnifiedIR.verify_ir(irb; level = 1)
    res = UC.classify_residual_cells(irb)
    @test !isempty(res)
    @test all(p -> p.second === :island, res)
    @test UnifiedIR.interpret(irb, _cmpl_stale, 4, false) == _cmpl_stale(4, false) == 14
    @test UnifiedIR.interpret(irb, _cmpl_stale, 4, true) == _cmpl_stale(4, true) == 10
end

@testset "island exit threading: loop-carried cells through sealed continues" begin
    # the escape_string shape: a loop whose body is one island; the state
    # cell's backedge is the island's sealed continue, carrying no values
    # until promotion threads them (loop arg + continue value + island reads
    # from the arg)
    src = """
    func @thr(%1::Any, %2::Int64) -> Int64 {
      %3 = cell const type Int64 :: Any
      cell_set %3, %2
      %5 = loop () :: Any {
        %6 = cfg () :: Union{} {
        ^bb2():
          %7 = cell_get %3 :: Int64
          %8 = call global Base.slt_int, %7, const 100 :: Bool
          br_if %8 (^bb3) (^bb4)
        ^bb3():
          %10 = cell_get %3 :: Int64
          %11 = call global Base.mul_int, %10, const 2 :: Int64
          cell_set %3, %11
          %13 = call global Base.slt_int, %11, const 50 :: Bool
          continue %5 if %13
        ^bb4():
          %15 = cell_get %3 :: Int64
          %16 = call global Base.add_int, %15, const 1 :: Int64
          cell_set %3, %16
          continue %5 if const false
        }
        unreachable
      }
      %19 = cell_get %3 :: Int64
      return %19
    }
    """
    pre = [UnifiedIR.interpret(UnifiedIR.parse_ir(src), nothing, x) for x in (3, 200)]
    @test pre == [96, 201]      # 3→6→12→24→48→96 exits; 200: +1 exits
    ir = UC.promotion_fixpoint!(UnifiedIR.parse_ir(src))
    @test UnifiedIR.verify_ir(ir; level = 1)
    @test count_cellops(ir) == 0
    @test [UnifiedIR.interpret(ir, nothing, x) for x in (3, 200)] == pre
    r = UC.df_correspondence(UnifiedIR.parse_ir(src))
    @test r.ok
    @test all(x -> x.status === :match, r.results)

    # single-internal-backedge island: the entry block's implicit in-edge
    # (the cfg entry) joins with ONE internal backedge — the entry phi must
    # still be placed (regression: the counter read below collapsed to the
    # pre-island constant and the island spun forever)
    src2 = """
    func @spin(%1::Any, %2::Int64) -> Int64 {
      %3 = cell const type Int64 :: Any
      cell_set %3, const 0
      %5 = cfg () :: Int64 {
      ^bb2():
        %6 = call global Base.add_int, %2, const 0 :: Int64
        goto (^bb3)
      ^bb3():
        %8 = cell_get %3 :: Int64
        %9 = call global Base.add_int, %8, const 1 :: Int64
        cell_set %3, %9
        %11 = call global Base.slt_int, %9, const 3 :: Bool
        br_if %11 (^bb2) (^bb4)
      ^bb4():
        %13 = cell_get %3 :: Int64
        result %13
      }
      return %5
    }
    """
    @test UnifiedIR.interpret(UnifiedIR.parse_ir(src2), nothing, 7) == 3
    ir2 = UC.promotion_fixpoint!(UnifiedIR.parse_ir(src2))
    @test UnifiedIR.verify_ir(ir2; level = 1)
    @test count_cellops(ir2) == 0
    @test UnifiedIR.interpret(ir2, nothing, 7) == 3

    # doubly-carried cell (the countlines shape): the value crosses TWO
    # backedge levels. The inner loop cannot take the outer store as its
    # carried init — it goes stale across the OUTER backedge (the init
    # backedge hazard; a naive init miscompiled exactly this shape, caught
    # by the fuzzer differential) — so the cell stays classified memory,
    # and execution stays CORRECT
    function _cmpl_2level(n)
        total = 0
        i = 0
        while i < n
            i += 1
            j = 0
            while j < i
                j += 1
                total = total + j
            end
        end
        return total
    end
    W = Base.get_world_counter()
    ir3 = UC.promotion_fixpoint!(UC.structure_prep!(UC.lowered_ir(_cmpl_2level, Any[Int]; world = W)))
    @test UnifiedIR.verify_ir(ir3; level = 1)
    res3 = UC.classify_residual_cells(ir3)
    @test all(p -> p.second === :refused_multilevel_exit, res3)
    @test UnifiedIR.interpret(ir3, _cmpl_2level, 4) == _cmpl_2level(4) == 20
end

@testset "promotion miscompile regressions (seeded)" begin
    # each of these seeds once produced a miscompile (wrong value, undef
    # read, or non-termination) caught by the fuzzer differential:
    #   - single-internal-backedge island entry phi missed
    #   - carried-arg init stale across an enclosing backedge (doubly
    #     carried counter reset; multi-level continue variant)
    #   - store sinking across a swallowed-exception boundary (post-try
    #     reads observed mid-try memory)
    #   - joining arms leaking mid-arm through nested sealed exits
    inputs = [(1, 2), (7, 7), (-2, 9)]
    for (seed, case) in [(31337, 5), (31337, 116), (20260711, 66),
                         (24242, 838), (24242, 5203), (24242, 6844), (24242, 8458)]
        ir = CellFuzz.randir(CellFuzz.Random.Xoshiro(seed + case))
        pre = [outcome(ir, a...) for a in inputs]
        ir2 = UC.promotion_fixpoint!(CellFuzz.randir(CellFuzz.Random.Xoshiro(seed + case)))
        @test UnifiedIR.verify_ir(ir2; level = 1)
        post = [outcome(ir2, a...) for a in inputs]
        @test post == pre
    end
end
