# Regression tests for specific soundness classes found during integration.

using UnifiedIR
using UnifiedIR: op_stmt, op_inline, op_region
using Test

@testset "promote_cells!: backedge reach (§6)" begin
    # i-cell pattern: cell stored before a loop and re-stored INSIDE it, with
    # the loop's get occurring before the in-loop store. The in-loop store
    # reaches the get via the backedge, so the cell must NOT be promoted to
    # the dominating pre-loop store. (Found via a miscompiled while-loop:
    # the loop condition froze at the init value — an infinite loop.)
    b = Builder(name = :backedge)
    n = append_stmt!(b, K"region_arg"; type = Int64)
    cell = append_stmt!(b, K"cell", Int64; type = Any)
    append_stmt!(b, K"cell_new", op_stmt(cell))                 # newvar decl
    append_stmt!(b, K"cell_set", op_stmt(cell), 1)              # i = 1
    loop = append_stmt!(b, K"loop"; type = Any)
    body = UnifiedIR.open_region!(b, loop; kind = UnifiedIR.REGION_LOOP_BODY)
    g = append_stmt!(b, K"cell_get", op_stmt(cell); type = Int64)   # read i
    g2 = append_stmt!(b, K"test.add", g, 1; type = Int64)
    append_stmt!(b, K"cell_set", op_stmt(cell), g2)             # i = i + 1
    cnd = append_stmt!(b, K"test.icmp", :sle, g2, n; type = Bool)
    append_stmt!(b, K"continue", op_region(body), op_stmt(cnd))
    UnifiedIR.close_region!(b)
    fin = append_stmt!(b, K"cell_get", op_stmt(cell); type = Int64)
    append_stmt!(b, K"return", fin)
    ir = finish!(b)
    @test verify_ir(ir; level = 1)
    @test interpret(ir, 5) == 6
    @test promote_cells!(ir) == 0          # must refuse: backedge reach
    @test interpret(ir, 5) == 6            # semantics unchanged

    # positive control: declaration-new + single dominating store, straight
    # line — promotable (the cell_new relaxation this bug shipped with)
    b2 = Builder(name = :declnew)
    a2 = append_stmt!(b2, K"region_arg"; type = Int64)
    c2 = append_stmt!(b2, K"cell", Int64; type = Any)
    append_stmt!(b2, K"cell_new", op_stmt(c2))
    append_stmt!(b2, K"cell_set", op_stmt(c2), a2)
    gg = append_stmt!(b2, K"cell_get", op_stmt(c2); type = Int64)
    d2 = append_stmt!(b2, K"test.add", gg, 1; type = Int64)
    append_stmt!(b2, K"return", d2)
    ir2 = finish!(b2)
    @test promote_cells!(ir2) == 1
    ir2, _ = compact!(ir2)
    @test verify_ir(ir2; level = 1)
    @test interpret(ir2, 41) == 42
end
