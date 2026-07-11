using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region, TEST_KINDS
using Test

@testset "builder + if + interpret" begin
    # func @f(%a::Int64) -> Int64 { %c = icmp sgt %a, 0; %z = if %c { yield 1 } else { yield a*a }; return %z }
    b = Builder(name = :f)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"test.icmp", :sgt, a, 0; type = Bool)
    z = build_if!(b, c; type = Int64) do b
        append_stmt!(b, K"yield", 1)
    end
    # build_if! with else via explicit API
    UnifiedIR.open_region!(b, z)
    y = append_stmt!(b, K"test.mul", a, a; type = Int64)
    append_stmt!(b, K"yield", y)
    UnifiedIR.close_region!(b)
    append_stmt!(b, K"return", z)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test nstmts(ir) == 7
    @test interpret(ir, 5) == 1
    @test interpret(ir, -3) == 9

    # print / parse round trip
    txt = print_ir(ir)
    ir2 = parse_ir(txt)
    @test struct_eq(ir, ir2)
    @test interpret(ir2, -3) == 9
end

@testset "loop (§5.3) + interpret" begin
    # sum 1..n:  loop (s=0, j=1) { continue (j<=n) (s+j, j+1) } -> extract
    b = Builder(name = :sumn)
    n = append_stmt!(b, K"region_arg"; type = Int64)
    r = build_loop!(b, 0, 1; type = Tuple{Int64,Int64}, argtypes = Any[Int64, Int64]) do b, args
        s, j = args
        s2 = append_stmt!(b, K"test.add", s, j; type = Int64)
        j2 = append_stmt!(b, K"test.add", j, 1; type = Int64)
        cnd = append_stmt!(b, K"test.icmp", :sle, j2, n; type = Bool)
        body = UnifiedIR.current_region(b)
        append_stmt!(b, K"continue", op_region(body), op_stmt(cnd), op_stmt(s2), op_stmt(j2))
    end
    sum_ = append_stmt!(b, K"extract", op_stmt(r), op_inline(0); type = Int64)
    append_stmt!(b, K"return", sum_)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 10) == 55
    @test interpret(ir, 1) == 1
    txt = print_ir(ir)
    ir2 = parse_ir(txt)
    @test struct_eq(ir, ir2)
    @test interpret(ir2, 10) == 55
end

@testset "cells + try + promotion" begin
    b = Builder(name = :cells)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    cell = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_set", cell, a)
    g = append_stmt!(b, K"cell_get", op_stmt(cell); type = Int64)
    d = append_stmt!(b, K"test.add", g, 1; type = Int64)
    append_stmt!(b, K"return", d)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 41) == 42
    @test promote_cells!(ir) == 1
    @test interpret(ir, 41) == 42
    ir, remap = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test nstmts(ir) == 3       # region_arg, add, return
    @test interpret(ir, 41) == 42
end

@testset "editable insertion + compact (§4.5-flavored)" begin
    b = Builder(name = :ins)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    x = append_stmt!(b, K"test.mul", a, a; type = Int64)
    append_stmt!(b, K"return", x)
    ir = finish!(b)
    editable(ir)
    y = insert_before!(ir, x, K"test.add", a, 100; type = Int64)
    # replace mul with add(y, a)
    replace_stmt!(ir, x, K"test.add", y, a; type = Int64)
    ir, remap = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 5) == 110
end

@testset "wrap_in_if! surgery" begin
    b = Builder(name = :wrap)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    c = append_stmt!(b, K"test.icmp", :sgt, a, 0; type = Bool)
    x = append_stmt!(b, K"test.mul", a, 2; type = Int64)
    r = append_stmt!(b, K"return", x)
    ir = finish!(b)
    editable(ir)
    ifop = wrap_in_if!(ir, x, x, c; else_arm = (ir, er) -> begin
        push_stmt!(ir, er, K"yield", 0)
    end)
    ir, _ = compact!(ir)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 5) == 10
    @test interpret(ir, -5) == 0
end

@testset "dce" begin
    b = Builder(name = :dced)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    dead = append_stmt!(b, K"test.mul", a, a; type = Int64)
    dead2 = append_stmt!(b, K"test.add", dead, 1; type = Int64)
    live = append_stmt!(b, K"test.add", a, 1; type = Int64)
    append_stmt!(b, K"return", live)
    ir = finish!(b)
    @test dce!(ir) == 2
    ir, _ = compact!(ir)
    @test nstmts(ir) == 3
    @test interpret(ir, 4) == 5
end

@testset "floating + schedule (§4.3, test.delay)" begin
    src = """
    node @counter(%1::Bool)  layout=floating {
      eq %3 = test.add %2, const 1 :: Int64
      eq %2 = test.delay %3, const 0 :: Int64
    }
    """
    ir = parse_ir(src)
    @test UnifiedIR.layout(ir) == UnifiedIR.LAYOUT_FLOATING
    @test verify_ir(ir; level = 1)   # acyclic modulo delay
    ir, remap = schedule!(ir)
    @test UnifiedIR.layout(ir) == UnifiedIR.LAYOUT_DENSE
    # delay must be scheduled before its (cut) data input can be used
    @test verify_ir(ir; level = 0)
end

@testset "visibility clause 3 (own-region use rejected)" begin
    b = Builder(name = :bad)
    a = append_stmt!(b, K"region_arg"; type = Bool)
    z = append_stmt!(b, K"if", a; type = Int64)
    UnifiedIR.open_region!(b, z)
    append_stmt!(b, K"yield", z)      # illegal: arm yields the if's own result
    UnifiedIR.close_region!(b)
    append_stmt!(b, K"return", z)
    ir = finish!(b; verify = false)
    @test_throws VerifyError verify_ir(ir; level = 1)
end

@testset "dense delete restrictions" begin
    b = Builder(name = :del)
    a = append_stmt!(b, K"region_arg"; type = Int64)
    x = append_stmt!(b, K"test.mul", a, a; type = Int64)
    t = append_stmt!(b, K"return", x)
    ir = finish!(b)
    @test_throws ErrorException delete_stmt!(ir, t)   # terminator
    @test_throws ErrorException delete_stmt!(ir, a)   # region_arg
end

println("smoke ok")
