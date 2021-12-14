# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Meta
using Core: PhiNode, SSAValue, GotoNode, PiNode, QuoteNode, ReturnNode, GotoIfNot

# Tests for domsort

## Test that domsort doesn't mangle single-argument phis (#29262)
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # block 1
        Expr(:call, :opaque),
        GotoIfNot(Core.SSAValue(1), 10),
        # block 2
        Core.PhiNode(Int32[8], Any[Core.SSAValue(7)]), # <- This phi must not get replaced by %7
        Core.PhiNode(Int32[2, 8], Any[true, false]),
        GotoIfNot(Core.SSAValue(1), 7),
        # block 3
        Expr(:call, :+, Core.SSAValue(3), 1),
        # block 4
        Core.PhiNode(Int32[5, 6], Any[0, Core.SSAValue(6)]),
        Expr(:call, >, Core.SSAValue(7), 10),
        GotoIfNot(Core.SSAValue(8), 3),
        # block 5
        Core.PhiNode(Int32[2, 8], Any[0, Core.SSAValue(7)]),
        ReturnNode(Core.SSAValue(10)),
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    domtree = Core.Compiler.construct_domtree(ir.cfg.blocks)
    ir = Core.Compiler.domsort_ssa!(ir, domtree)
    Core.Compiler.verify_ir(ir)
    phi = ir.stmts.inst[3]
    @test isa(phi, Core.PhiNode) && length(phi.edges) == 1
end

# test that we don't stack-overflow in SNCA with large functions.
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    code = Any[]
    N = 2^15
    for i in 1:2:N
        push!(code, Expr(:call, :opaque))
        push!(code, GotoIfNot(Core.SSAValue(i), N+2)) # skip one block
    end
    # all goto here
    push!(code, Expr(:call, :opaque))
    push!(code, ReturnNode(nothing))
    src.code = code

    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    domtree = Core.Compiler.construct_domtree(ir.cfg.blocks)
    ir = Core.Compiler.domsort_ssa!(ir, domtree)
    Core.Compiler.verify_ir(ir)
end

# Tests for SROA

import Core.Compiler: argextype, singleton_type
const EMPTY_SPTYPES = Any[]

code_typed1(args...; kwargs...) = first(only(code_typed(args...; kwargs...)))::Core.CodeInfo
get_code(args...; kwargs...) = code_typed1(args...; kwargs...).code

# check if `x` is a statement with a given `head`
isnew(@nospecialize x) = Meta.isexpr(x, :new)

# check if `x` is a dynamic call of a given function
iscall(y) = @nospecialize(x) -> iscall(y, x)
function iscall((src, f)::Tuple{Core.CodeInfo,Function}, @nospecialize(x))
    return iscall(x) do @nospecialize x
        singleton_type(argextype(x, src, EMPTY_SPTYPES)) === f
    end
end
iscall(pred::Function, @nospecialize(x)) = Meta.isexpr(x, :call) && pred(x.args[1])

struct ImmutableXYZ; x; y; z; end
mutable struct MutableXYZ; x; y; z; end

# should optimize away very basic cases
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = ImmutableXYZ(x, y, z)
        xyz.x, xyz.y, xyz.z
    end
    @test !any(isnew, src.code)
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.x, xyz.y, xyz.z
    end
    @test !any(isnew, src.code)
end

# should handle simple mutabilities
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.y = 42
        xyz.x, xyz.y, xyz.z
    end
    @test !any(isnew, src.code)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), 42, #=x=# Core.Argument(4)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.x, xyz.z = xyz.z, xyz.x
        xyz.x, xyz.y, xyz.z
    end
    @test !any(isnew, src.code)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=z=# Core.Argument(4), #=y=# Core.Argument(3), #=x=# Core.Argument(2)]
    end
end
# circumvent uninitialized fields as far as there is a solid `setfield!` definition
let src = code_typed1() do
        r = Ref{Any}()
        r[] = 42
        return r[]
    end
    @test !any(isnew, src.code)
end
let src = code_typed1((Bool,)) do cond
        r = Ref{Any}()
        if cond
            r[] = 42
            return r[]
        else
            r[] = 32
            return r[]
        end
    end
    @test !any(isnew, src.code)
end
let src = code_typed1((Bool,)) do cond
        r = Ref{Any}()
        if cond
            r[] = 42
        else
            r[] = 32
        end
        return r[]
    end
    @test !any(isnew, src.code)
end
let src = code_typed1((Bool,Bool,Any,Any,Any)) do c1, c2, x, y, z
        r = Ref{Any}()
        if c1
            if c2
                r[] = x
            else
                r[] = y
            end
        else
            r[] = z
        end
        return r[]
    end
    @test !any(isnew, src.code)
end
let src = code_typed1((Bool,)) do cond
        r = Ref{Any}()
        if cond
            r[] = 42
        end
        return r[]
    end
    # N.B. `r` should be allocated since `cond` might be `false` and then it will be thrown
    @test any(isnew, src.code)
end
let src = code_typed1((Bool,Bool,Any,Any)) do c1, c2, x, y
        r = Ref{Any}()
        if c1
            if c2
                r[] = x
            end
        else
            r[] = y
        end
        return r[]
    end
    # N.B. `r` should be allocated since `c2` might be `false` and then it will be thrown
    @test any(isnew, src.code)
end

# should include a simple alias analysis
struct ImmutableOuter{T}; x::T; y::T; z::T; end
mutable struct MutableOuter{T}; x::T; y::T; z::T; end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = ImmutableXYZ(x, y, z)
        outer = ImmutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test !any(src.code) do @nospecialize x
        Meta.isexpr(x, :new)
    end
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=y=# Core.Argument(4)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = ImmutableXYZ(x, y, z)
        # #42831 forms ::PartialStruct(ImmutableOuter{Any}, Any[ImmutableXYZ, ImmutableXYZ, ImmutableXYZ])
        # so the succeeding `getproperty`s are type stable and inlined
        outer = ImmutableOuter{Any}(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test !any(isnew, src.code)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=y=# Core.Argument(4)]
    end
end

# FIXME our analysis isn't yet so powerful at this moment: may be unable to handle nested objects well
# OK: mutable(immutable(...)) case
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        t   = (xyz,)
        v = t[1].x
        v, v, v
    end
    @test !any(isnew, src.code)
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        outer = ImmutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test !any(isnew, src.code)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=y=# Core.Argument(4)]
    end
end
let # this is a simple end to end test case, which demonstrates allocation elimination
    # by handling `mutable[RefValue{String}](immutable[Tuple](...))` case correctly
    # NOTE this test case isn't so robust and might be subject to future changes of the broadcasting implementation,
    # in that case you don't really need to stick to keeping this test case around
    simple_sroa(s) = broadcast(identity, Ref(s))
    s = Base.inferencebarrier("julia")::String
    simple_sroa(s)
    # NOTE don't hard-code `"julia"` in `@allocated` clause and make sure to execute the
    # compiled code for `simple_sroa`, otherwise everything can be folded even without SROA
    @test @allocated(simple_sroa(s)) == 0
end
# FIXME: immutable(mutable(...)) case
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = ImmutableXYZ(x, y, z)
        outer = MutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test_broken !any(isnew, src.code)
end
# FIXME: mutable(mutable(...)) case
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        outer = MutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test_broken !any(isnew, src.code)
end

# should work nicely with inlining to optimize away a complicated case
# adapted from http://wiki.luajit.org/Allocation-Sinking-Optimization#implementation%5B
struct Point
    x::Float64
    y::Float64
end
#=@inline=# add(a::Point, b::Point) = Point(a.x + b.x, a.y + b.y)
function compute()
    a = Point(1.5, 2.5)
    b = Point(2.25, 4.75)
    for i in 0:(100000000-1)
        a = add(add(a, b), b)
    end
    a.x, a.y
end
let src = code_typed1(compute)
    @test !any(isnew, src.code)
end

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

# Expr(:new) annotated as PartialStruct
struct FooPartial
    x
    y
    global f_partial
    f_partial(x) = new(x, 2).x
end
let ci = code_typed(f_partial, Tuple{Float64})[1].first
    @test length(ci.code) == 1 && isa(ci.code[1], ReturnNode)
end

# A SSAValue after the compaction line
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # block 1
        nothing,
        # block 2
        PhiNode(Int32[1, 7], Any[Core.Argument(2), SSAValue(9)]),
        Expr(:call, isa, SSAValue(2), UnionAll),
        GotoIfNot(Core.SSAValue(3), 11),
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
        ReturnNode(Core.SSAValue(2)),
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
    ir = @test_nowarn Core.Compiler.sroa_pass!(ir)
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
        ReturnNode(2)
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
    @test length(ir.cfg.blocks) == 1 && Core.Compiler.length(ir.stmts) == 1
end

let m = Meta.@lower 1 + 1
    # Test that CFG simplify doesn't mess up when chaining past return blocks
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        Core.Compiler.GotoIfNot(Core.Compiler.Argument(2), 3),
        Core.Compiler.GotoNode(4),
        ReturnNode(1),
        Core.Compiler.GotoNode(5),
        Core.Compiler.GotoIfNot(Core.Compiler.Argument(2), 7),
        # This fall through block of the previous GotoIfNot
        # must be moved up along with it, when we merge it
        # into the goto 4 block.
        ReturnNode(2),
        ReturnNode(3)
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
    ret_2 = ir.stmts.inst[ir.cfg.blocks[3].stmts[end]]
    @test isa(ret_2, Core.Compiler.ReturnNode) && ret_2.val == 2
end

let m = Meta.@lower 1 + 1
    # Test that CFG simplify doesn't try to merge every block in a loop into
    # its predecessor
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::Core.CodeInfo
    src.code = Any[
        # Block 1
        Core.Compiler.GotoNode(2),
        # Block 2
        Core.Compiler.GotoNode(3),
        # Block 3
        Core.Compiler.GotoNode(1)
    ]
    nstmts = length(src.code)
    src.ssavaluetypes = nstmts
    src.codelocs = fill(Int32(1), nstmts)
    src.ssaflags = fill(Int32(0), nstmts)
    ir = Core.Compiler.inflate_ir(src)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) == 1
end

# Issue #29213
function f_29213()
    while true
        try
            break
        finally
        end
    end

    while 1==1
        try
            ed = (_not_defined,)
        finally
            break
        end
    end

    ed = string(ed)
end

@test_throws UndefVarError f_29213()

function test_29253(K)
    if true
        try
            error()
        catch e
        end
    end
    size(K,1)
end
let K = rand(2,2)
    @test test_29253(K) == 2
end

function no_op_refint(r)
    r[]
    return
end
let code = code_typed(no_op_refint,Tuple{Base.RefValue{Int}})[1].first.code
    @test length(code) == 1
    @test isa(code[1], Core.ReturnNode)
    @test code[1].val === nothing
end

# check getfield elim handling of GlobalRef
const _some_coeffs = (1,[2],3,4)
splat_from_globalref(x) = (x, _some_coeffs...,)
@test splat_from_globalref(0) == (0, 1, [2], 3, 4)

function pi_on_argument(x)
    if isa(x, Core.Argument)
        return x.n
    end
    return -2
end
let code = code_typed(pi_on_argument, Tuple{Any})[1].first.code,
    nisa = 0, found_pi = false
    for stmt in code
        if Meta.isexpr(stmt, :call)
            callee = stmt.args[1]
            if (callee === isa || callee === :isa || (isa(callee, GlobalRef) &&
                                                      callee.name === :isa))
                nisa += 1
            end
        elseif stmt === Core.PiNode(Core.Argument(2), Core.Argument)
            found_pi = true
        end
    end
    @test nisa == 1
    @test found_pi
end

# issue #38936
# check that getfield elim can handle unions of tuple types
mutable struct S38936{T} content::T end
struct PrintAll{T} <: Function
    parts::T
end
function (f::PrintAll)(io::IO)
    for x in f.parts
        print(io, x)
    end
end
let f = PrintAll((S38936("<span>"), "data", S38936("</span")))
    @test !any(code_typed(f, (IOBuffer,))[1][1].code) do stmt
        stmt isa Expr && stmt.head === :call && stmt.args[1] === GlobalRef(Core, :tuple)
    end
end

exc39508 = ErrorException("expected")
@noinline function test39508()
    local err
    try
        err = exc39508::Exception
        throw(err)
        false
    catch ex
        @test ex === err
    end
    return err
end
@test test39508() === exc39508

let # `sroa_pass!` should work with constant globals
    # immutable pass
    src = @eval Module() begin
        const REF_FLD = :x
        struct ImmutableRef{T}
            x::T
        end

        code_typed((Int,)) do x
            r = ImmutableRef{Int}(x) # should be eliminated
            x = getfield(r, REF_FLD) # should be eliminated
            return sin(x)
        end |> only |> first
    end
    @test !any(src.code) do @nospecialize(stmt)
        Meta.isexpr(stmt, :call) || return false
        ft = Core.Compiler.argextype(stmt.args[1], src, EMPTY_SPTYPES)
        return Core.Compiler.widenconst(ft) == typeof(getfield)
    end
    @test !any(src.code) do @nospecialize(stmt)
        return Meta.isexpr(stmt, :new)
    end

    # mutable pass
    src = @eval Module() begin
        const REF_FLD = :x
        code_typed() do
            r = Ref{Int}(42) # should be eliminated
            x = getfield(r, REF_FLD) # should be eliminated
            return sin(x)
        end |> only |> first
    end
    @test !any(src.code) do @nospecialize(stmt)
        Meta.isexpr(stmt, :call) || return false
        ft = Core.Compiler.argextype(stmt.args[1], src, EMPTY_SPTYPES)
        return Core.Compiler.widenconst(ft) == typeof(getfield)
    end
    @test !any(src.code) do @nospecialize(stmt)
        return Meta.isexpr(stmt, :new)
    end
end

let
    # `typeassert` elimination after SROA
    # NOTE we can remove this optimization once inference is able to reason about memory-effects
    src = @eval Module() begin
        mutable struct Foo; x; end

        code_typed((Int,)) do a
            x1 = Foo(a)
            x2 = Foo(x1)
            return typeassert(x2.x, Foo).x
        end |> only |> first
    end
    # eliminate `typeassert(x2.x, Foo)`
    @test all(src.code) do @nospecialize stmt
        Meta.isexpr(stmt, :call) || return true
        ft = Core.Compiler.argextype(stmt.args[1], src, EMPTY_SPTYPES)
        return Core.Compiler.widenconst(ft) !== typeof(typeassert)
    end
end

let
    # Test for https://github.com/JuliaLang/julia/issues/43402
    # Ensure that structs required not used outside of the ccall,
    # still get listed in the ccall_preserves

    src = @eval Module() begin
        @inline function effectful()
            s1 = Ref{Csize_t}()
            s2 = Ref{Csize_t}()
            ccall(:some_ccall, Cvoid,
                  (Ref{Csize_t},Ref{Csize_t}),
                  s1, s2)
            return s1[], s2[]
        end

        code_typed() do
            s1, s2 = effectful()
            return s1
        end |> only |> first
    end

    refs = map(Core.SSAValue, findall(x->x isa Expr && x.head == :new, src.code))
    some_ccall = findfirst(x -> x isa Expr && x.head == :foreigncall && x.args[1] == :(:some_ccall), src.code)
    @assert some_ccall !== nothing
    stmt = src.code[some_ccall]
    nccallargs = length(stmt.args[3]::Core.SimpleVector)
    preserves = stmt.args[6+nccallargs:end]
    @test length(refs) == 2
    @test length(preserves) == 2
    @test all(alloc -> alloc in preserves, refs)
end
