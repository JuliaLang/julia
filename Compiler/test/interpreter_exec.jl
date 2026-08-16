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

# Long tuple splats become `Expr(:invoke, ci, Core._apply_iterate, ...)`; the
# interpreter must flatten the containers and invoke `ci` (#62742).
@noinline interp_invoke_apply_target(xs...) = (xs[1], xs[end], length(xs))
interp_invoke_apply_prefix(t::NTuple{33,Float32}) =
    interp_invoke_apply_target(0.0f0, t...)
interp_invoke_apply_multiple(a::NTuple{33,Float32}, b::NTuple{33,Float32}) =
    interp_invoke_apply_target(a..., b...)
let t = ntuple(Float32, 33)
    for (f, tt, args, expected) in (
            (interp_invoke_apply_prefix, (NTuple{33,Float32},), (t,), (0.0f0, 33.0f0, 34)),
            (interp_invoke_apply_multiple, (NTuple{33,Float32}, NTuple{33,Float32}), (t, t), (1.0f0, 33.0f0, 66)))
        ir = first(only(Base.code_ircode(f, tt)))
        @test any(1:length(ir.stmts)) do i
            stmt = ir[SSAValue(i)][:stmt]
            Meta.isexpr(stmt, :invoke) &&
                Compiler.singleton_type(Compiler.argextype(stmt.args[2], ir)) === Core._apply_iterate
        end
        ir.argtypes[1] = Tuple{}
        oc = Core.OpaqueClosure(ir; do_compile=false)
        @test oc(args...) === expected
    end
end
