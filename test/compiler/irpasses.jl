# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# Tests for domsort

## Test that domsort doesn't mangle single-argument phis (#29262)
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # block 1
        Expr(:call, :opaque),
        Expr(:gotoifnot, Core.SSAValue(1), 10),
        # block 2
        Core.PhiNode(Any[8], Any[Core.SSAValue(7)]), # <- This phi must not get replaced by %7
        Core.PhiNode(Any[2, 8], Any[true, false]),
        Expr(:gotoifnot, Core.SSAValue(1), 7),
        # block 3
        Expr(:call, :+, Core.SSAValue(3), 1),
        # block 4
        Core.PhiNode(Any[5, 6], Any[0, Core.SSAValue(6)]),
        Expr(:call, >, Core.SSAValue(7), 10),
        Expr(:gotoifnot, Core.SSAValue(8), 3),
        # block 5
        Core.PhiNode(Any[2, 8], Any[0, Core.SSAValue(7)]),
        Expr(:return, Core.SSAValue(10)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    domtree = Core.Compiler.construct_domtree(ir.cfg)
    ir = Core.Compiler.domsort_ssa!(ir, domtree)
    Core.Compiler.verify_ir(ir)
    @test isa(ir.stmts[3], Core.PhiNode) && length(ir.stmts[3].edges) == 1
end
