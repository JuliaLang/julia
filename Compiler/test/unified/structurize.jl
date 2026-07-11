# Structurization (§10.5 P1) + loop-carried cell promotion: interpret-based
# differentials (semantic equality that type checks cannot see) and the
# soundness regressions found while building the passes.

s_mysum(n) = begin s = 0; i = 1; while i <= n; s += i; i += 1; end; s end
s_mypow(x, n) = begin r = 1; for _ in 1:n; r *= x; end; r end
s_clampt(x) = x < 0 ? 0 : (x > 1 ? 1 : x)
s_nested(n) = begin t = 0; i = 1; while i <= n; j = 1; while j <= i; t += j; j += 1; end; i += 1; end; t end
s_condstore(c, a) = begin x = a; if c; x = 2a; end; x + 1 end
s_trydiv(a, b) = try; div(a, b); catch; 0; end

# entry-convert, then run ONLY the structurization-session passes
function structurized_ir(f, sig)
    ir = UnifiedCompiler.lowered_ir(f, sig)
    UnifiedIR.editable(ir)
    UnifiedCompiler.structurize!(ir)
    UnifiedCompiler.promote_loop_cells!(ir)
    UnifiedCompiler.selectify!(ir)
    ir, _ = UnifiedIR.compact!(ir)
    UnifiedIR.verify_ir(ir; level = 1)
    return ir
end

@testset "structurize: interpret differential (entry → structurize only)" begin
    for (f, sig, inputs) in [
        (s_mysum,  Tuple{Int}, [(10,), (0,), (100,)]),
        (s_mypow,  Tuple{Int,Int}, [(2, 10), (3, 0)]),
        (s_clampt, Tuple{Int}, [(-3,), (0,), (1,), (5,)]),
        (s_nested, Tuple{Int}, [(0,), (1,), (5,)]),
        (s_condstore, Tuple{Bool,Int}, [(true, 5), (false, 5)]),
        (s_trydiv, Tuple{Int,Int}, [(7, 2), (7, 0)]),
    ]
        ir0 = UnifiedCompiler.lowered_ir(f, sig)
        ir1 = structurized_ir(f, sig)
        for inp in inputs
            want = f(inp...)
            @test UnifiedIR.interpret(ir0, f, inp...) == want
            @test UnifiedIR.interpret(ir1, f, inp...) == want
        end
    end
    # the loop shapes must actually structurize: no islands left, loops back
    for f in (s_mysum, s_nested)
        ir = structurized_ir(f, Tuple{Int})
        @test !any(s -> UnifiedIR.stmt_kind(ir, s) === UnifiedIR.@K_str("cfg"),
                   UnifiedIR.each_stmt(ir))
        @test any(s -> UnifiedIR.stmt_kind(ir, s) === UnifiedIR.@K_str("loop"),
                  UnifiedIR.each_stmt(ir))
    end
    # single loop: the carried cells promote away entirely (t in the nested
    # case legitimately stays memory-form: it would need multi-level carrying
    # through the outer loop — outside the v1 promotion rules)
    ir = structurized_ir(s_mysum, Tuple{Int})
    @test !any(s -> UnifiedIR.stmt_kind(ir, s) === UnifiedIR.@K_str("cell"),
               UnifiedIR.each_stmt(ir))
end

@testset "promote_loop_cells!: backedge shape promotes correctly (§6)" begin
    # The i-cell pattern promote_cells! must refuse (backedge reach): a store
    # before the loop and a re-store in the body, the body read preceding the
    # body store. Loop-carried promotion handles it by routing the read
    # through the carried arg — never the pre-loop store.
    b = UnifiedIR.Builder(name = :backedge)
    n = UnifiedIR.append_stmt!(b, K"region_arg"; type = Int64)
    cell = UnifiedIR.append_stmt!(b, K"cell", Int64; type = Any)
    UnifiedIR.append_stmt!(b, K"cell_new", UnifiedIR.op_stmt(cell))
    UnifiedIR.append_stmt!(b, K"cell_set", UnifiedIR.op_stmt(cell), 1)
    loop = UnifiedIR.append_stmt!(b, K"loop"; type = Any)
    body = UnifiedIR.open_region!(b, loop; kind = UnifiedIR.REGION_LOOP_BODY)
    g = UnifiedIR.append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(cell); type = Int64)
    g2 = UnifiedIR.append_stmt!(b, K"call", GlobalRef(Base, :+), g, 1; type = Int64)
    UnifiedIR.append_stmt!(b, K"cell_set", UnifiedIR.op_stmt(cell), g2)
    cnd = UnifiedIR.append_stmt!(b, K"call", GlobalRef(Base, :<=), g2, n; type = Bool)
    UnifiedIR.append_stmt!(b, K"continue", UnifiedIR.op_region(body), UnifiedIR.op_stmt(cnd))
    UnifiedIR.close_region!(b)
    fin = UnifiedIR.append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(cell); type = Int64)
    UnifiedIR.append_stmt!(b, K"return", fin)
    ir = UnifiedIR.finish!(b)
    @test UnifiedIR.interpret(ir, 5) == 6
    @test UnifiedIR.promote_cells!(ir) == 0            # §6 policy refuses
    UnifiedIR.editable(ir)
    @test UnifiedCompiler.promote_loop_cells!(ir) == 1 # carried promotion succeeds
    ir, _ = UnifiedIR.compact!(ir)
    @test UnifiedIR.verify_ir(ir; level = 1)
    @test UnifiedIR.interpret(ir, 5) == 6              # semantics unchanged
    @test UnifiedIR.interpret(ir, 0) == 2              # ≥1 trip (do-while) shape
    @test !any(s -> UnifiedIR.stmt_kind(ir, s) === UnifiedIR.@K_str("cell"),
               UnifiedIR.each_stmt(ir))
end

@testset "adce_region_ops!: conditional store to an outer cell survives" begin
    # Regression: cell stores carry pure-ish flags; an `if { cell_set }` whose
    # cell outlives the op must not be removed by region-op ADCE.
    b = UnifiedIR.Builder(name = :condstore)
    c = UnifiedIR.append_stmt!(b, K"region_arg"; type = Bool)
    cell = UnifiedIR.append_stmt!(b, K"cell", Int64; type = Any)
    st1 = UnifiedIR.append_stmt!(b, K"cell_set", UnifiedIR.op_stmt(cell), 1)
    ifop = UnifiedIR.append_stmt!(b, K"if", UnifiedIR.op_stmt(c); type = Nothing)
    arm = UnifiedIR.open_region!(b, ifop; kind = UnifiedIR.REGION_ARM)
    st2 = UnifiedIR.append_stmt!(b, K"cell_set", UnifiedIR.op_stmt(cell), 2)
    UnifiedIR.append_stmt!(b, K"result")
    UnifiedIR.close_region!(b)
    g = UnifiedIR.append_stmt!(b, K"cell_get", UnifiedIR.op_stmt(cell); type = Int64)
    UnifiedIR.append_stmt!(b, K"return", g)
    ir = UnifiedIR.finish!(b)
    # the flags inference assigns to cell ops (the trigger of the regression)
    for s in (st1, st2)
        UnifiedIR.set_flag!(ir, s, UnifiedIR.FLAG_PURE)
    end
    @test UnifiedIR.interpret(ir, true) == 2
    @test UnifiedIR.interpret(ir, false) == 1
    UnifiedIR.editable(ir)
    @test UnifiedCompiler.adce_region_ops!(ir) == 0
    ir, _ = UnifiedIR.compact!(ir)
    @test UnifiedIR.verify_ir(ir; level = 1)
    @test UnifiedIR.interpret(ir, true) == 2
    @test UnifiedIR.interpret(ir, false) == 1
end

@testset "structurize + optimize: full-pipeline interpret differential" begin
    st = UnifiedCompiler.UInferState()
    for (f, ats, inputs) in [
        (s_mysum,  (Int,), [(10,), (0,)]),
        (s_mypow,  (Int, Int), [(2, 10), (3, 0)]),
        (s_condstore, (Bool, Int), [(true, 5), (false, 5)]),
    ]
        ir = UnifiedCompiler.lowered_ir(f, Tuple{ats...})
        ir = UnifiedCompiler.optimize_ir!(ir, Any[Compiler.Const(f), ats...]; state = st)
        for inp in inputs
            @test UnifiedIR.interpret(ir, f, inp...) == f(inp...)
        end
    end
end
