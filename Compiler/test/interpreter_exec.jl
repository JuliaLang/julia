# This file is a part of Julia. License is MIT: https://julialang.org/license

# tests that interpreter matches codegen
include("setup_Compiler.jl")

using Test
using Core.IR

# test that interpreter correctly handles PhiNodes (#29262)
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::CodeInfo
    src.code = Any[
        # block 1
        QuoteNode(:a),
        QuoteNode(:b),
        GlobalRef(@__MODULE__, :test29262),
        GotoIfNot(SSAValue(3), 6),
        # block 2
        PhiNode(Int32[4], Any[SSAValue(1)]),
        PhiNode(Int32[4, 5], Any[SSAValue(2), SSAValue(5)]),
        ReturnNode(SSAValue(6)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.ssaflags = fill(zero(UInt32), nstmts)
    src.debuginfo = Core.DebugInfo(:none)
    Compiler.verify_ir(Compiler.inflate_ir(src))
    global test29262 = true
    @test :a === @eval $m
    global test29262 = false
    @test :b === @eval $m
end

let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::CodeInfo
    src.code = Any[
        # block 1
        QuoteNode(:a),
        QuoteNode(:b),
        QuoteNode(:c),
        GlobalRef(@__MODULE__, :test29262),
        # block 2
        PhiNode(Int32[4, 16], Any[false, true]), # false, true
        PhiNode(Int32[4, 16], Any[SSAValue(1), SSAValue(2)]), # :a, :b
        PhiNode(Int32[4, 16], Any[SSAValue(3), SSAValue(6)]), # :c, :a
        PhiNode(Int32[16], Any[SSAValue(7)]), # NULL, :c
        # block 3
        PhiNode(Int32[], Any[]), # NULL, NULL
        PhiNode(Int32[17, 8], Any[true, SSAValue(4)]), # test29262, test29262, [true]
        PhiNode(Int32[17], Vector{Any}(undef, 1)), # NULL, NULL
        PhiNode(Int32[8], Vector{Any}(undef, 1)), # NULL, NULL
        PhiNode(Int32[], Any[]), # NULL, NULL
        PhiNode(Int32[17, 8], Any[SSAValue(2), SSAValue(8)]), # NULL, :c, [:b]
        PhiNode(Int32[], Any[]), # NULL, NULL
        GotoIfNot(SSAValue(5), 5),
        # block 4
        GotoIfNot(SSAValue(10), 9),
        # block 5
        Expr(:call, GlobalRef(Core, :tuple), SSAValue(6), SSAValue(7), SSAValue(8), SSAValue(14)),
        ReturnNode(SSAValue(18)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.ssaflags = fill(zero(UInt32), nstmts)
    src.debuginfo = Core.DebugInfo(:none)
    m.args[1] = copy(src)
    Compiler.verify_ir(Compiler.inflate_ir(src))
    global test29262 = true
    @test (:b, :a, :c, :c) === @eval $m
    m.args[1] = copy(src)
    global test29262 = false
    @test (:b, :a, :c, :b) === @eval $m
end

let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::CodeInfo
    src.code = Any[
        # block 1
        QuoteNode(:a),
        QuoteNode(:b),
        GlobalRef(@__MODULE__, :test29262),
        # block 2
        EnterNode(12),
        # block 3
        UpsilonNode(),
        UpsilonNode(),
        UpsilonNode(SSAValue(2)),
        GotoIfNot(SSAValue(3), 10),
        # block 4
        UpsilonNode(SSAValue(1)),
        # block 5
        Expr(:throw_undef_if_not, :expected, false),
        ReturnNode(), # unreachable
        # block 6
        PhiCNode(Any[SSAValue(5), SSAValue(7), SSAValue(9)]), # NULL, :a, :b
        PhiCNode(Any[SSAValue(6)]), # NULL
        Expr(:pop_exception, SSAValue(4)),
        # block 7
        ReturnNode(SSAValue(12)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.ssaflags = fill(zero(UInt32), nstmts)
    src.debuginfo = Core.DebugInfo(:none)
    Compiler.verify_ir(Compiler.inflate_ir(src))
    global test29262 = true
    @test :a === @eval $m
    global test29262 = false
    @test :b === @eval $m
    @test isempty(current_exceptions())
end

# The interpreter must enforce the same memory-order rules as codegen on the reformulated
# global accesses (`Core.getglobal_partition`/`Core.setglobal_partition` builtin calls
# produced by `reformulate_globals_pass!`): an invalid or `:not_atomic` order throws
# `ConcurrencyViolationError`, and a valid explicit order keeps the builtin's semantics
# (return value, fences).
iscall_gr(x, name) = Meta.isexpr(x, :call) && x.args[1] == GlobalRef(Core, name)
global exec_gp_g::Int = 1
exec_gp_badread() = getglobal(@__MODULE__, :exec_gp_g, :not_atomic)
exec_gp_badwrite() = setglobal!(@__MODULE__, :exec_gp_g, 5, :not_atomic)
exec_gp_goodswap() = swapglobal!(@__MODULE__, :exec_gp_g, 7, :sequentially_consistent)
let ci = code_typed(exec_gp_badread, ())[1][1]
    @assert any(x -> iscall_gr(x, :getglobal_partition), ci.code)
    @test_throws ConcurrencyViolationError exec_gp_badread() # compiled
    @test_throws ConcurrencyViolationError Core.eval(@__MODULE__, Expr(:thunk, ci)) # interpreted
end
let ci = code_typed(exec_gp_badwrite, ())[1][1]
    @assert any(x -> iscall_gr(x, :setglobal_partition), ci.code)
    @test_throws ConcurrencyViolationError exec_gp_badwrite() # compiled
    @test_throws ConcurrencyViolationError Core.eval(@__MODULE__, Expr(:thunk, ci)) # interpreted
    @test exec_gp_g === 1 # the failed store must not have written
end
let ci = code_typed(exec_gp_goodswap, ())[1][1]
    @assert any(x -> iscall_gr(x, :setglobal_partition), ci.code)
    @test Core.eval(@__MODULE__, Expr(:thunk, ci)) === 1 # interpreted swap returns the old value
    @test exec_gp_g === 7
end
# A resolved `isdefinedglobal` with a memory order reformulates to the
# `Core.isdefinedglobal_partition` builtin and stays correct compiled and interpreted.
global exec_gp_def::Int = 3
exec_gp_isdef() = isdefinedglobal(@__MODULE__, :exec_gp_def, true, :acquire)
let ci = code_typed(exec_gp_isdef, ())[1][1]
    @assert any(x -> iscall_gr(x, :isdefinedglobal_partition), ci.code)
    @test exec_gp_isdef() === true # compiled
    @test Core.eval(@__MODULE__, Expr(:thunk, ci)) === true # interpreted
end
