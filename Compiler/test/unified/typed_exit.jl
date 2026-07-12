# Typed exit converter (§10.5): UnifiedIR → IRCode with phi synthesis.
# Acceptance: the STOCK IR verifier passes, and Core.OpaqueClosure execution
# matches the UnifiedIR interpreter on the same inputs.

using UnifiedIR: op_stmt, op_inline, op_region

function oc_vs_interp(ir, inputs...)
    irc = UnifiedCompiler.ir_to_ircode(ir)
    Compiler.verify_ir(irc)
    oc = Core.OpaqueClosure(irc)
    for inp in inputs
        want = UnifiedIR.interpret(ir, nothing, inp...)
        got = oc(inp...)
        isequal(got, want) || return false
    end
    return true
end

@testset "typed exit: if" begin
    b = Builder(name = :tif)
    append_stmt!(b, K"region_arg"; type = Any)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"call", GlobalRef(Base, :slt_int), 0, a; type = Bool)
    z = build_if!(b, c; type = Int64) do b
        append_stmt!(b, K"result", 1)
    end
    UnifiedIR.open_region!(b, z)
    y = append_stmt!(b, K"call", GlobalRef(Base, :mul_int), a, a; type = Int64)
    append_stmt!(b, K"result", y)
    UnifiedIR.close_region!(b)
    append_stmt!(b, K"return", z)
    ir = finish!(b)
    @test oc_vs_interp(ir, (5,), (-3,), (0,))
end

@testset "typed exit: loop with carried args + de-tupled extracts" begin
    b = Builder(name = :tloop)
    append_stmt!(b, K"region_arg"; type = Any)
    n = append_stmt!(b, K"region_arg"; type = Int64)
    r = build_loop!(b, 0, 1; type = Tuple{Int64,Int64}, argtypes = Any[Int64, Int64]) do b, args
        s, j = args
        s2 = append_stmt!(b, K"call", GlobalRef(Base, :add_int), s, j; type = Int64)
        j2 = append_stmt!(b, K"call", GlobalRef(Base, :add_int), j, 1; type = Int64)
        cnd = append_stmt!(b, K"call", GlobalRef(Base, :sle_int), j2, n; type = Bool)
        body = UnifiedIR.current_region(b)
        append_stmt!(b, K"continue", op_region(body), op_stmt(cnd), op_stmt(s2), op_stmt(j2))
    end
    tot = append_stmt!(b, K"extract", op_stmt(r), op_inline(1); type = Int64)
    lst = append_stmt!(b, K"extract", op_stmt(r), op_inline(2); type = Int64)
    fin = append_stmt!(b, K"call", GlobalRef(Base, :add_int), tot, lst; type = Int64)
    append_stmt!(b, K"return", fin)
    ir = finish!(b)
    @test oc_vs_interp(ir, (10,), (1,), (3,))
end

@testset "typed exit: break with value out of loop + nested if" begin
    # find first j with j*j > n, else n itself
    b = Builder(name = :tbreak)
    append_stmt!(b, K"region_arg"; type = Any)
    n = append_stmt!(b, K"region_arg"; type = Int64)
    r = build_loop!(b, 1; type = Int64, argtypes = Any[Int64]) do b, args
        j, = args
        sq = append_stmt!(b, K"call", GlobalRef(Base, :mul_int), j, j; type = Int64)
        c = append_stmt!(b, K"call", GlobalRef(Base, :slt_int), n, sq; type = Bool)
        body = UnifiedIR.current_region(b)
        fi = append_stmt!(b, K"if", c; type = Nothing)
        UnifiedIR.open_region!(b, fi)
        append_stmt!(b, K"break", op_region(body), op_stmt(j))
        UnifiedIR.close_region!(b)
        j2 = append_stmt!(b, K"call", GlobalRef(Base, :add_int), j, 1; type = Int64)
        cnd = append_stmt!(b, K"call", GlobalRef(Base, :sle_int), j2, n; type = Bool)
        append_stmt!(b, K"continue", op_region(body), op_stmt(cnd), op_stmt(j2))
    end
    append_stmt!(b, K"return", r)
    ir = finish!(b)
    @test oc_vs_interp(ir, (10,), (2,), (100,))
end

@testset "typed exit: escaping tuple result materializes" begin
    # loop result used as a first-class tuple (not extract-only)
    b = Builder(name = :ttup)
    append_stmt!(b, K"region_arg"; type = Any)
    n = append_stmt!(b, K"region_arg"; type = Int64)
    r = build_loop!(b, 0, 1; type = Tuple{Int64,Int64}, argtypes = Any[Int64, Int64]) do b, args
        s, j = args
        s2 = append_stmt!(b, K"call", GlobalRef(Base, :add_int), s, j; type = Int64)
        j2 = append_stmt!(b, K"call", GlobalRef(Base, :add_int), j, 1; type = Int64)
        cnd = append_stmt!(b, K"call", GlobalRef(Base, :sle_int), j2, n; type = Bool)
        body = UnifiedIR.current_region(b)
        append_stmt!(b, K"continue", op_region(body), op_stmt(cnd), op_stmt(s2), op_stmt(j2))
    end
    append_stmt!(b, K"return", r)      # whole tuple escapes
    ir = finish!(b)
    @test oc_vs_interp(ir, (10,), (1,))
end

@testset "typed exit: cfg island with block args" begin
    src = """
    func @absmax(%1::Any, %2::Int64, %3::Int64) -> Int64 {
      %4 = cfg (%2) {
      ^bb2(%5::Int64):
        %6 = call global Base.slt_int, %5, %3 :: Bool
        br_if %6 (^bb3: %3) (^bb4: %5)
      ^bb3(%8::Int64):
        result %8
      ^bb4(%10::Int64):
        result %10
      } :: Int64
      return %4
    }
    """
    ir = parse_ir(src)
    @test UnifiedIR.verify_ir(ir; level = 1)
    @test oc_vs_interp(ir, (3, 7), (9, 2))
end

@testset "typed exit: feature matrix errors" begin
    # residual cell → clean error
    b = Builder(name = :tcell)
    append_stmt!(b, K"region_arg"; type = Any)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    cell = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_set", cell, a)
    g = append_stmt!(b, K"cell_get", op_stmt(cell); type = Int64)
    append_stmt!(b, K"return", g)
    ir = finish!(b)
    @test_throws UnifiedCompiler.UnsupportedIR UnifiedCompiler.ir_to_ircode(ir)
end
