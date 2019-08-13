# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Meta
using Core: PhiNode, SSAValue, GotoNode, PiNode, QuoteNode

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

# test that we don't stack-overflow in SNCA with large functions.
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    code = Any[]
    N = 2^15
    for i in 1:2:N
        push!(code, Expr(:call, :opaque))
        push!(code, Expr(:gotoifnot, Core.SSAValue(i), N+2)) # skip one block
    end
    # all goto here
    push!(code, Expr(:call, :opaque))
    push!(code, Expr(:return))
    src.code = code

    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    domtree = Core.Compiler.construct_domtree(ir.cfg)
    ir = Core.Compiler.domsort_ssa!(ir, domtree)
    Core.Compiler.verify_ir(ir)
end

# Tests for SROA

mutable struct Foo30594; x::Float64; end
Base.copy(x::Foo30594) = Foo30594(x.x)
function add!(p::Foo30594, off::Foo30594)
    p.x += off.x
    return p
end
Base.:(+)(a::Foo30594, b::Foo30594) = add!(copy(a), b)

let results = Float64[]
    @noinline use30594(x) = push!(results, x.x); nothing
    function foo30594(cnt::Int, dx::Int)
        step = Foo30594(dx)
        curr = step + Foo30594(1)
        for i in 1:cnt
            use30594(curr)
            curr = curr + step
        end
        nothing
    end

    foo30594(4, -1)
    @test results == [0.0, -1.0, -2.0, -3.0]
end

# Issue #29983
# This one is a bit hard to trigger, but the key is to create a case
# where SROA needs to introduce an intermediate type-unstable phi node
struct Foo29983{T}
    x::Tuple{T}
end
struct Bar29983{S}
    x::S
end
Base.:+(a::T, b::Bar29983{S}) where {T, S} = Bar29983(a + b.x)
Base.:+(a::Bar29983{S}, b::T) where {T, S} = b + a
Base.:+(a::Bar29983{S}, b::Bar29983{T}) where {T, S} = Bar29983(a.x + b.x)
Base.:+(a::Foo29983, b::Foo29983) = Foo29983((a.x[1] + b.x[1],))

function f(x::Vector{T}) where {T}
    x1 = Foo29983((x[1],))
    la1 = Foo29983((x[1],))
    f1 = Foo29983((0,))
    for _ in 1:2
        f1 += la1
    end
    return f1
end

@test f([Bar29983(1.0)]).x[1].x == 2.0

# Issue #31139 - Checking for correct number of arguments in getfield elim
let nt = (a=1, b=2)
    blah31139(x) = getfield(x)
    # Shouldn't throw
    @test isa(code_typed(blah31139, Tuple{typeof(nt)}), Array)
    # Should throw
    @test_throws ArgumentError blah31139(nt)
end

# Expr(:new) annoted as PartialStruct
struct FooPartial
    x
    y
    global f_partial
    f_partial(x) = new(x, 2).x
end
let ci = code_typed(f_partial, Tuple{Float64})[1].first
    @test length(ci.code) == 1 && isexpr(ci.code[1], :return)
end

# A SSAValue after the compaction line
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # block 1
        nothing,
        # block 2
        PhiNode(Any[1, 7], Any[Core.SlotNumber(2), SSAValue(9)]),
        Expr(:call, isa, SSAValue(2), UnionAll),
        Expr(:gotoifnot, Core.SSAValue(3), 11),
        # block 3
        nothing,
        nothing,
        PiNode(SSAValue(2), UnionAll),
        Expr(:call, getfield, SSAValue(7), QuoteNode(:body)),
        SSAValue(8), # <-- This SSAValue is the problem.
                     # SROA needs to propagate the old taint when it follows
                     # the phinode here
        GotoNode(2),
        # block 5
        Expr(:return, Core.SSAValue(2)),
    ]
    src.ssavaluetypes = Any[
        Nothing,
        Any,
        Bool,
        Any,
        Nothing,
        Nothing,
        UnionAll,
        Any,
        Any,
        Any,
        Any
    ]
    nstmts = length(src.code)
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src, Any[], Any[Any, Any])
    @test Core.Compiler.verify_ir(ir) === nothing
    domtree = Core.Compiler.construct_domtree(ir.cfg)
    ir = @test_nowarn Core.Compiler.getfield_elim_pass!(ir, domtree)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# Issue #31546 - missing widenconst in SROA
function f_31546(x)
    (a, b) = x == "r"  ? (false, false) :
             x == "r+" ? (true, false) :
             x == "w"  ? (true, true) : error()
    return a, b
end
@test f_31546("w") == (true, true)

# Tests for cfg simplification
let src = code_typed(gcd, Tuple{Int, Int})[1].first
    # Test that cfg_simplify doesn't mangle IR on code with loops
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
end

let m = Meta.@lower 1 + 1
    # Test that CFG simplify combines redundant basic blocks
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        Core.Compiler.GotoNode(2),
        Core.Compiler.GotoNode(3),
        Core.Compiler.GotoNode(4),
        Core.Compiler.GotoNode(5),
        Core.Compiler.GotoNode(6),
        Core.Compiler.GotoNode(7),
        Expr(:return, 2)
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.compact!(ir)
    @test length(ir.cfg.blocks) == 1 && length(ir.stmts) == 1
end

let m = Meta.@lower 1 + 1
    # Test that CFG simplify doesn't mess up when chaining past return blocks
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        Core.Compiler.GotoIfNot(Core.Compiler.Argument(2), 3),
        Core.Compiler.GotoNode(4),
        Expr(:return, 1),
        Core.Compiler.GotoNode(5),
        Core.Compiler.GotoIfNot(Core.Compiler.Argument(2), 7),
        # This fall through block of the previous GotoIfNot
        # must be moved up along with it, when we merge it
        # into the goto 4 block.
        Expr(:return, 2),
        Expr(:return, 3)
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) == 5
    ret_2 = ir.stmts[ir.cfg.blocks[3].stmts[end]]
    @test isa(ret_2, Core.Compiler.ReturnNode) && ret_2.val == 2
end
