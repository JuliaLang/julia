# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests that interpreter matches codegen
using Test
using Core: GotoIfNot, ReturnNode

# test that interpreter correctly handles PhiNodes (#29262)
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # block 1
        QuoteNode(:a),
        QuoteNode(:b),
        GlobalRef(@__MODULE__, :test29262),
        GotoIfNot(Core.SSAValue(3), 6),
        # block 2
        Core.PhiNode(Any[4], Any[Core.SSAValue(1)]),
        Core.PhiNode(Any[4, 5], Any[Core.SSAValue(2), Core.SSAValue(5)]),
        ReturnNode(Core.SSAValue(6)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = Any[ Any for _ = 1:nstmts ]
    src.codelocs = fill(Int32(1), nstmts)
    src.inferred = true
    Core.Compiler.verify_ir(Core.Compiler.inflate_ir(src))
    global test29262 = true
    @test :a === @eval $m
    global test29262 = false
    @test :b === @eval $m
end

let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # block 1
        QuoteNode(:a),
        QuoteNode(:b),
        QuoteNode(:c),
        GlobalRef(@__MODULE__, :test29262),
        # block 2
        Core.PhiNode(Any[4, 16], Any[false, true]), # false, true
        Core.PhiNode(Any[4, 16], Any[Core.SSAValue(1), Core.SSAValue(2)]), # :a, :b
        Core.PhiNode(Any[4, 16], Any[Core.SSAValue(3), Core.SSAValue(6)]), # :c, :a
        Core.PhiNode(Any[16], Any[Core.SSAValue(7)]), # NULL, :c
        # block 3
        Core.PhiNode(Any[], Any[]), # NULL, NULL
        Core.PhiNode(Any[17, 8], Any[true, Core.SSAValue(4)]), # test29262, test29262, [true]
        Core.PhiNode(Any[17], Vector{Any}(undef, 1)), # NULL, NULL
        Core.PhiNode(Any[8], Vector{Any}(undef, 1)), # NULL, NULL
        Core.PhiNode(Any[], Any[]), # NULL, NULL
        Core.PhiNode(Any[17, 8], Any[Core.SSAValue(2), Core.SSAValue(8)]), # NULL, :c, [:b]
        Core.PhiNode(Any[], Any[]), # NULL, NULL
        GotoIfNot(Core.SSAValue(5), 5),
        # block 4
        GotoIfNot(Core.SSAValue(10), 9),
        # block 5
        Expr(:call, GlobalRef(Core, :tuple), Core.SSAValue(6), Core.SSAValue(7), Core.SSAValue(8), Core.SSAValue(14)),
        ReturnNode(Core.SSAValue(18)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = Any[ Any for _ = 1:nstmts ]
    src.codelocs = fill(Int32(1), nstmts)
    src.inferred = true
    Core.Compiler.verify_ir(Core.Compiler.inflate_ir(src))
    global test29262 = true
    @test (:b, :a, :c, :c) === @eval $m
    global test29262 = false
    @test (:b, :a, :c, :b) === @eval $m
end

let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # block 1
        QuoteNode(:a),
        QuoteNode(:b),
        GlobalRef(@__MODULE__, :test29262),
        # block 2
        Expr(:enter, 11),
        # block 3
        Core.UpsilonNode(),
        Core.UpsilonNode(),
        Core.UpsilonNode(Core.SSAValue(2)),
        GotoIfNot(Core.SSAValue(3), 10),
        # block 4
        Core.UpsilonNode(Core.SSAValue(1)),
        # block 5
        Expr(:throw_undef_if_not, :expected, false),
        # block 6
        Core.PhiCNode(Any[Core.SSAValue(5), Core.SSAValue(7), Core.SSAValue(9)]), # NULL, :a, :b
        Core.PhiCNode(Any[Core.SSAValue(6)]), # NULL
        Expr(:leave, 1),
        # block 7
        ReturnNode(Core.SSAValue(11)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = Any[ Any for _ = 1:nstmts ]
    src.codelocs = fill(Int32(1), nstmts)
    src.inferred = true
    Core.Compiler.verify_ir(Core.Compiler.inflate_ir(src))
    global test29262 = true
    @test :a === @eval $m
    global test29262 = false
    @test :b === @eval $m
end
