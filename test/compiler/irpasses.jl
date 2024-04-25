# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Meta
using Core.IR

include("irutils.jl")

# domsort
# =======

## Test that domsort doesn't mangle single-argument phis (#29262)
let code = Any[
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
    ir = make_ircode(code)
    domtree = Core.Compiler.construct_domtree(ir)
    ir = Core.Compiler.domsort_ssa!(ir, domtree)
    Core.Compiler.verify_ir(ir)
    phi = ir.stmts.stmt[3]
    @test isa(phi, Core.PhiNode) && length(phi.edges) == 1
end

# test that we don't stack-overflow in SNCA with large functions.
let code = Any[]
    N = 2^15
    for i in 1:2:N
        push!(code, Expr(:call, :opaque))
        push!(code, GotoIfNot(Core.SSAValue(i), N+2)) # skip one block
    end
    # all goto here
    push!(code, Expr(:call, :opaque))
    push!(code, ReturnNode(nothing))
    ir = make_ircode(code)
    domtree = Core.Compiler.construct_domtree(ir)
    ir = Core.Compiler.domsort_ssa!(ir, domtree)
    Core.Compiler.verify_ir(ir)
end

# SROA
# ====

using Core.Compiler: widenconst

is_load_forwarded(src::CodeInfo) = !any(iscall((src, getfield)), src.code)
is_scalar_replaced(src::CodeInfo) =
    is_load_forwarded(src) && !any(iscall((src, setfield!)), src.code) && !any(isnew, src.code)

function is_load_forwarded(@nospecialize(T), src::CodeInfo)
    for i in 1:length(src.code)
        x = src.code[i]
        if iscall((src, getfield), x)
            widenconst(argextype(x.args[1], src)) <: T && return false
        end
    end
    return true
end
function is_scalar_replaced(@nospecialize(T), src::CodeInfo)
    is_load_forwarded(T, src) || return false
    for i in 1:length(src.code)
        x = src.code[i]
        if iscall((src, setfield!), x)
            widenconst(argextype(x.args[1], src)) <: T && return false
        elseif isnew(x)
            widenconst(argextype(SSAValue(i), src)) <: T && return false
        end
    end
    return true
end

struct ImmutableXYZ; x; y; z; end
mutable struct MutableXYZ; x; y; z; end
struct ImmutableOuter{T}; x::T; y::T; z::T; end
mutable struct MutableOuter{T}; x::T; y::T; z::T; end
struct ImmutableRef{T}; x::T; end
Base.getindex(r::ImmutableRef) = r.x
mutable struct SafeRef{T}; x::T; end
Base.getindex(s::SafeRef) = getfield(s, 1)
Base.setindex!(s::SafeRef, x) = setfield!(s, 1, x)

# simple immutability
# -------------------

let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = ImmutableXYZ(x, y, z)
        xyz.x, xyz.y, xyz.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=z=# Core.Argument(4)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = (x, y, z)
        xyz[1], xyz[2], xyz[3]
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=z=# Core.Argument(4)]
    end
end

# simple mutability
# -----------------

let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.x, xyz.y, xyz.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=z=# Core.Argument(4)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.y = 42
        xyz.x, xyz.y, xyz.z
    end
    @test is_scalar_replaced(src)
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
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=z=# Core.Argument(4), #=y=# Core.Argument(3), #=x=# Core.Argument(2)]
    end
end

# uninitialized fields
# --------------------

# safe cases
let src = code_typed1() do
        r = Ref{Any}()
        r[] = 42
        return r[]
    end
    @test is_scalar_replaced(src)
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
    @test is_scalar_replaced(src)
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
    @test is_scalar_replaced(src)
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
    @test is_scalar_replaced(src)
end

# unsafe cases
let src = code_typed1() do
        r = Ref{Any}()
        return r[]
    end
    @test count(isnew, src.code) == 1
    @test count(iscall((src, getfield)), src.code) == 1
end
let src = code_typed1((Bool,)) do cond
        r = Ref{Any}()
        if cond
            r[] = 42
        end
        return r[]
    end
    # N.B. `r` should be allocated since `cond` might be `false` and then it will be thrown
    @test count(isnew, src.code) == 1
    @test count(iscall((src, setfield!)), src.code) == 1
    @test count(iscall((src, getfield)), src.code) == 1
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
    @test count(isnew, src.code) == 1
    @test count(iscall((src, setfield!)), src.code) == 2
    @test count(iscall((src, getfield)), src.code) == 1
end

# aliased load forwarding
# -----------------------
# TODO fix broken examples with EscapeAnalysis

# OK: immutable(immutable(...)) case
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

# OK (mostly): immutable(mutable(...)) case
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        t   = (xyz,)
        v = t[1].x
        v, v, v
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        outer = ImmutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test is_scalar_replaced(src)
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
    let src = code_typed1(simple_sroa, (String,))
        @test is_scalar_replaced(src)
    end
    s = Base.inferencebarrier("julia")::String
    simple_sroa(s)
    # NOTE don't hard-code `"julia"` in `@allocated` clause and make sure to execute the
    # compiled code for `simple_sroa`, otherwise everything can be folded even without SROA
    @test @allocated(simple_sroa(s)) == 0
end
let # FIXME: some nested example
    src = code_typed1((Int,)) do x
        Ref(Ref(x))[][]
    end
    @test_broken is_scalar_replaced(src)

    src = code_typed1((Int,)) do x
        Ref(Ref(Ref(Ref(Ref(Ref(Ref(Ref(Ref(Ref((x)))))))))))[][][][][][][][][][]
    end
    @test_broken is_scalar_replaced(src)
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

let # should work with constant globals
    # immutable case
    # --------------
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
    @test count(iscall((src, getfield)), src.code) == 0
    @test count(isnew, src.code) == 0

    # mutable case
    # ------------
    src = @eval Module() begin
        const REF_FLD = :x
        code_typed() do
            r = Ref{Int}(42) # should be eliminated
            x = getfield(r, REF_FLD) # should be eliminated
            return sin(x)
        end |> only |> first
    end
    @test count(iscall((src, getfield)), src.code) == 0
    @test count(isnew, src.code) == 0
end

# don't SROA statement that may throw
# https://github.com/JuliaLang/julia/issues/48067
function issue48067(a::Int, b)
   r = Ref(a)
   try
       setfield!(r, :x, b)
       nothing
   catch err
       getfield(r, :x)
   end
end
let src = code_typed1(issue48067, (Int,String))
    @test any(iscall((src, setfield!)), src.code)
end
@test issue48067(42, "julia") == 42

# should work nicely with inlining to optimize away a complicated case
# adapted from http://wiki.luajit.org/Allocation-Sinking-Optimization#implementation%5B
struct Point
    x::Float64
    y::Float64
end
#=@inline=# add(a::Point, b::Point) = Point(a.x + b.x, a.y + b.y)
function compute_points()
    a = Point(1.5, 2.5)
    b = Point(2.25, 4.75)
    for i in 0:(100000000-1)
        a = add(add(a, b), b)
    end
    a.x, a.y
end
let src = code_typed1(compute_points)
    @test !any(isnew, src.code)
end

# preserve elimination
# --------------------

function ispreserved(@nospecialize(x))
    return function (@nospecialize(stmt),)
        if Meta.isexpr(stmt, :foreigncall)
            nccallargs = length(stmt.args[3]::Core.SimpleVector)
            for pidx = (6+nccallargs):length(stmt.args)
                if stmt.args[pidx] === x
                    return true
                end
            end
        end
        return false
    end
end

let src = code_typed1((String,)) do s
        ccall(:some_ccall, Cint, (Ptr{String},), Ref(s))
    end
    @test count(isnew, src.code) == 0
    @test any(ispreserved(#=s=#Core.Argument(2)), src.code)
end

# if the mutable struct is directly used, we shouldn't eliminate it
let src = code_typed1() do
        a = MutableXYZ(-512275808,882558299,-2133022131)
        b = Int32(42)
        ccall(:some_ccall, Cvoid, (MutableXYZ, Int32), a, b)
        return a.x
    end
    @test count(isnew, src.code) == 1
end

# should eliminate allocation whose address isn't taked even if it has uninitialized field(s)
mutable struct BadRef
    x::String
    y::String
    BadRef(x) = new(x)
end
Base.cconvert(::Type{Ptr{BadRef}}, a::String) = BadRef(a)
Base.unsafe_convert(::Type{Ptr{BadRef}}, ar::BadRef) = Ptr{BadRef}(pointer_from_objref(ar.x))
let src = code_typed1((String,)) do s
        ccall(:jl_breakpoint, Cvoid, (Ptr{BadRef},), s)
    end
    @test count(isnew, src.code) == 0
    @test any(ispreserved(#=s=#Core.Argument(2)), src.code)
end

# isdefined elimination
# ---------------------

let src = code_typed1((Any,)) do a
        r = Ref{Any}()
        r[] = a
        if isassigned(r)
            return r[]
        end
        return nothing
    end
    @test is_scalar_replaced(src)
end

let src = code_typed1((Bool, Any,)) do cnd, a
        r = Ref{Any}()
        if cnd
            r[] = a # this `setfield!` shouldn't be eliminated
        end
        return isassigned(r)
    end
    @test count(isnew, src.code) == 1
    @test count(iscall((src, setfield!)), src.code) == 1
end

callit(f, args...) = f(args...)
function isdefined_elim()
    local arr::Vector{Any}
    callit() do
        arr = Any[]
    end
    return arr
end
let src = code_typed1(isdefined_elim)
    @test count(isisdefined, src.code) == 0
end
@test isdefined_elim() == Any[]

function abmult(r::Int, x0)
    if r < 0
        r = -r
    end
    f = x -> x * r
    return @inline f(x0)
end
let src = code_typed1(abmult, (Int,Int))
    @test is_scalar_replaced(src)
end
@test abmult(-3, 3) == 9

function abmult2(r0::Int, x0)
    r::Int = r0
    if r < 0
        r = -r
    end
    f = x -> x * r
    return f(x0)
end
let src = code_typed1(abmult2, (Int,Int))
    @test is_scalar_replaced(src)
end
@test abmult2(-3, 3) == 9

# comparison lifting
# ==================

let # lifting `===` through PhiNode
    src = code_typed1((Bool,Int,)) do c, x
        y = c ? x : nothing
        y === nothing # => ϕ(false, true)
    end
    @test count(iscall((src, ===)), src.code) == 0

    # should optimize away the iteration protocol
    src = code_typed1((Int,)) do n
        s = 0
        for i in 1:n
            s += i
        end
        s
    end
    @test !any(src.code) do @nospecialize x
        iscall((src, ===), x) && argextype(x.args[2], src) isa Union
    end
end

let # lifting `===` through Core.ifelse
    src = code_typed1((Bool,Int,)) do c, x
        y = Core.ifelse(c, x, nothing)
        y === nothing # => Core.ifelse(c, false, true)
    end
    @test count(iscall((src, ===)), src.code) == 0
end

let # lifting `isa` through PhiNode
    src = code_typed1((Bool,Int,)) do c, x
        y = c ? x : nothing
        isa(y, Int) # => ϕ(true, false)
    end
    @test count(iscall((src, isa)), src.code) == 0

    src = code_typed1((Int,)) do n
        s = 0
        itr = 1:n
        st = iterate(itr)
        while !isa(st, Nothing)
            i, st = itr
            s += i
            st = iterate(itr, st)
        end
        s
    end
    @test !any(src.code) do @nospecialize x
        iscall((src, isa), x) && argextype(x.args[2], src) isa Union
    end
end

let # lifting `isa` through Core.ifelse
    src = code_typed1((Bool,Int,)) do c, x
        y = Core.ifelse(c, x, nothing)
        isa(y, Int) # => Core.ifelse(c, true, false)
    end
    @test count(iscall((src, isa)), src.code) == 0
end


let # lifting `isdefined` through PhiNode
    src = code_typed1((Bool,Some{Int},)) do c, x
        y = c ? x : nothing
        isdefined(y, 1) # => ϕ(true, false)
    end
    @test count(iscall((src, isdefined)), src.code) == 0

    src = code_typed1((Int,)) do n
        s = 0
        itr = 1:n
        st = iterate(itr)
        while isdefined(st, 2)
            i, st = itr
            s += i
            st = iterate(itr, st)
        end
        s
    end
    @test !any(src.code) do @nospecialize x
        iscall((src, isdefined), x) && argextype(x.args[2], src) isa Union
    end
end

let # lifting `isdefined` through Core.ifelse
    src = code_typed1((Bool,Some{Int},)) do c, x
        y = Core.ifelse(c, x, nothing)
        isdefined(y, 1) # => Core.ifelse(c, true, false)
    end
    @test count(iscall((src, isdefined)), src.code) == 0
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
struct FooPartialNew
    x
    y
    global f_partial
    f_partial(x) = new(x, 2).x
end
@test fully_eliminated(f_partial, Tuple{Float64})

# A SSAValue after the compaction line
let code = Any[
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
    ssavaluetypes = Any[
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
    slottypes = Any[Any, Any, Any]
    ir = make_ircode(code; ssavaluetypes, slottypes)
    ir = @test_nowarn Core.Compiler.sroa_pass!(ir)
    @test Core.Compiler.verify_ir(ir) === nothing
end

# A lifted Core.ifelse with an eliminated branch (#50276)
let code = Any[
        # block 1
        #=  %1: =# Core.Argument(2),
        # block 2
        #=  %2: =# Expr(:call, Core.ifelse, SSAValue(1), true, missing),
        #=  %3: =# GotoIfNot(SSAValue(2), 11),
        # block 3
        #=  %4: =# PiNode(SSAValue(2), Bool), # <-- This PiNode is the trigger of the bug, since it
                                              #     means that only one branch of the Core.ifelse
                                              #     is lifted.
        #=  %5: =# GotoIfNot(false, 8),
        # block 2
        #=  %6: =# nothing,
        #=  %7: =# GotoNode(8),
        # block 4
        #=  %8: =# PhiNode(Int32[5, 7], Any[SSAValue(4), SSAValue(6)]),
        #               ^-- N.B. This PhiNode also needs to have a Union{ ... } type in order
        #                   for lifting to be performed (it is skipped for e.g. `Bool`)
        #
        #=  %9: =# Expr(:call, isa, SSAValue(8), Missing),
        #= %10: =# ReturnNode(SSAValue(9)),
        # block 5
        #= %11: =# ReturnNode(false),
    ]
    ssavaluetypes = Any[
        Any,
        Union{Missing, Bool},
        Any,
        Bool,
        Any,
        Missing,
        Any,
        Union{Nothing, Bool},
        Bool,
        Any,
        Any
    ]
    slottypes = Any[Any, Any, Any]
    ir = make_ircode(code; ssavaluetypes, slottypes)
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

let # Test that CFG simplify combines redundant basic blocks
    code = Any[
        Core.Compiler.GotoNode(2),
        Core.Compiler.GotoNode(3),
        Core.Compiler.GotoNode(4),
        Core.Compiler.GotoNode(5),
        Core.Compiler.GotoNode(6),
        Core.Compiler.GotoNode(7),
        ReturnNode(2)
    ]
    ir = make_ircode(code)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.compact!(ir)
    @test length(ir.cfg.blocks) == 1 && Core.Compiler.length(ir.stmts) == 1
end

# Test cfg_simplify in complicated sequences of dropped and merged bbs
using Core.Compiler: Argument, IRCode, GotoNode, GotoIfNot, ReturnNode, NoCallInfo, BasicBlock, StmtRange, SSAValue
bb_term(ir, bb) = Core.Compiler.getindex(ir, SSAValue(Core.Compiler.last(ir.cfg.blocks[bb].stmts)))[:stmt]

function each_stmt_a_bb(stmts, preds, succs)
    ir = IRCode()
    empty!(ir.stmts.stmt)
    append!(ir.stmts.stmt, stmts)
    empty!(ir.stmts.type); append!(ir.stmts.type, [Nothing for _ = 1:length(stmts)])
    empty!(ir.stmts.flag); append!(ir.stmts.flag, [0x0 for _ = 1:length(stmts)])
    empty!(ir.stmts.line); append!(ir.stmts.line, [Int32(0) for _ = 1:3length(stmts)])
    empty!(ir.stmts.info); append!(ir.stmts.info, [NoCallInfo() for _ = 1:length(stmts)])
    empty!(ir.cfg.blocks); append!(ir.cfg.blocks, [BasicBlock(StmtRange(i, i), preds[i], succs[i]) for i = 1:length(stmts)])
    empty!(ir.cfg.index);  append!(ir.cfg.index,  [i for i = 2:length(stmts)])
    Core.Compiler.verify_ir(ir)
    return ir
end

for gotoifnot in (false, true)
    stmts = [
        # BB 1
        GotoIfNot(Argument(1), 8),
        # BB 2
        GotoIfNot(Argument(2), 4),
        # BB 3
        GotoNode(9),
        # BB 4
        GotoIfNot(Argument(3), 10),
        # BB 5
        GotoIfNot(Argument(4), 11),
        # BB 6
        GotoIfNot(Argument(5), 12),
        # BB 7
        GotoNode(13),
        # BB 8
        ReturnNode(1),
        # BB 9
        nothing,
        # BB 10
        nothing,
        # BB 11
        gotoifnot ? GotoIfNot(Argument(6), 13) : GotoNode(13),
        # BB 12
        ReturnNode(2),
        # BB 13
        ReturnNode(3),
    ]
    preds = Vector{Int}[Int[], [1], [2], [2], [4], [5], [6], [1], [3], [4, 9], [5, 10], gotoifnot ? [6,11] : [6], [7, 11]]
    succs = Vector{Int}[[2, 8], [3, 4], [9], [5, 10], [6, 11], [7, 12], [13], Int[], [10], [11], gotoifnot ? [12, 13] : [13], Int[], Int[]]
    ir = each_stmt_a_bb(stmts, preds, succs)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)

    if gotoifnot
        let term4 = bb_term(ir, 4), term5 = bb_term(ir, 5)
            @test isa(term4, GotoIfNot) && bb_term(ir, term4.dest).val == 3
            @test isa(term5, ReturnNode) && term5.val == 2
        end
    else
        @test length(ir.cfg.blocks) == 10
        let term = bb_term(ir, 3)
            @test isa(term, GotoNode) && bb_term(ir, term.label).val == 3
        end
    end
end

let stmts = [
        # BB 1
        GotoIfNot(Argument(1), 4),
        # BB 2
        GotoIfNot(Argument(2), 5),
        # BB 3
        GotoNode(5),
        # BB 4
        ReturnNode(1),
        # BB 5
        ReturnNode(2)
    ]
    preds = Vector{Int}[Int[], [1], [2], [1], [2, 3]]
    succs = Vector{Int}[[2, 4], [3, 5], [5], Int[], Int[]]
    ir = each_stmt_a_bb(stmts, preds, succs)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)

    @test length(ir.cfg.blocks) == 4
    terms = map(i->bb_term(ir, i), 1:length(ir.cfg.blocks))
    @test Set(term.val for term in terms if isa(term, ReturnNode)) == Set([1,2])
end

let # Test that CFG simplify doesn't mess up when chaining past return blocks
    code = Any[
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
    ir = make_ircode(code)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) == 5
    ret_2 = ir.stmts.stmt[ir.cfg.blocks[3].stmts[end]]
    @test isa(ret_2, Core.Compiler.ReturnNode) && ret_2.val == 2
end

let # Test that CFG simplify doesn't try to merge every block in a loop into
    # its predecessor
    code = Any[
        # Block 1
        Core.Compiler.GotoNode(2),
        # Block 2
        Core.Compiler.GotoNode(3),
        # Block 3
        Core.Compiler.GotoNode(1)
    ]
    ir = make_ircode(code)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) == 1
end

# `cfg_simplify!` shouldn't error in a presence of `try/catch` block
let ir = Base.code_ircode(; optimize_until="slot2ssa") do
        v = try
        catch
        end
        v
    end |> only |> first
    Core.Compiler.verify_ir(ir)
    nb = length(ir.cfg.blocks)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    na = length(ir.cfg.blocks)
    @test na < nb
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
@test fully_eliminated(no_op_refint,Tuple{Base.RefValue{Int}}; retval=nothing)

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
    @test count(iscall((src, typeassert)), src.code) == 0
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

    refs = map(Core.SSAValue, findall(@nospecialize(x)->Meta.isexpr(x, :new), src.code))
    some_ccall = findfirst(@nospecialize(x) -> Meta.isexpr(x, :foreigncall) && x.args[1] == :(:some_ccall), src.code)
    @assert some_ccall !== nothing
    stmt = src.code[some_ccall]
    nccallargs = length(stmt.args[3]::Core.SimpleVector)
    preserves = stmt.args[6+nccallargs:end]
    @test length(refs) == 2
    @test length(preserves) == 2
    @test all(alloc -> alloc in preserves, refs)
end

# test `flags_for_effects` and DCE
# ================================

let # effect-freeness computation for array allocation

    # should eliminate dead allocations
    good_dims = [1, 2, 3, 4, 10]
    Ns = [1, 2, 3, 4, 10]
    for dim = good_dims, N = Ns
        Int64(dim)^N > typemax(Int) && continue
        dims = ntuple(i->dim, N)
        @test @eval fully_eliminated() do
            Array{Int,$N}(undef, $(dims...))
            nothing
        end
    end

    # shouldn't eliminate erroneous dead allocations
    bad_dims = [-1, typemax(Int)]
    for dim in bad_dims, N in [1, 2, 3, 4, 10], T in Any[Int, Union{Missing,Nothing}, Nothing, Any]
        dims = ntuple(i->dim, N)
        @test @eval !fully_eliminated() do
            Array{$T,$N}(undef, $(dims...))
            nothing
        end
        @test_throws "invalid " @eval let
            Array{$T,$N}(undef, $(dims...))
            nothing
        end
    end

    # some high-level examples
    @test fully_eliminated() do
        Int[]
        nothing
    end
    @test fully_eliminated() do
        Matrix{Tuple{String,String}}(undef, 4, 4)
        nothing
    end
    @test fully_eliminated() do
        IdDict{Any,Any}()
        nothing
    end
end

# allow branch folding to look at type information
let ci = code_typed1(optimize=false) do
        cond = 1 + 1 == 2
        if !cond
            gcd(24, 36)
        else
            gcd(64, 128)
        end
    end
    ir = Core.Compiler.inflate_ir(ci)
    @test any(@nospecialize(stmt)->isa(stmt, Core.GotoIfNot), ir.stmts.stmt)
    ir = Core.Compiler.compact!(ir, true)
    @test !any(@nospecialize(stmt)->isa(stmt, Core.GotoIfNot), ir.stmts.stmt)
end

# Test that adce_pass! can drop phi node uses that can be concluded unused
# from PiNode analysis.
let src = @eval Module() begin
        @noinline mkfloat() = rand(Float64)
        @noinline use(a::Float64) = ccall(:jl_, Cvoid, (Any,), a)
        dispatch(a::Float64) = use(a)
        dispatch(a::Tuple) = nothing
        function foo(b)
            a = mkfloat()
            a = b ? (a, 2.0) : a
            dispatch(a)
        end
        code_typed(foo, Tuple{Bool})[1][1]
    end
    @test count(iscall((src, Core.tuple)), src.code) == 0
end

# Test that cfg_simplify can converging control flow through empty blocks
function foo_cfg_empty(b)
    if b
        @goto x
    end
    @label x
    return b
end
let ci = code_typed(foo_cfg_empty, Tuple{Bool}, optimize=true)[1][1]
    ir = Core.Compiler.inflate_ir(ci)
    @test length(ir.stmts) == 3
    @test length(ir.cfg.blocks) == 3
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) <= 2
    @test isa(ir.stmts[length(ir.stmts)][:stmt], ReturnNode)
end

@test Core.Compiler.is_effect_free(Base.infer_effects(getfield, (Complex{Int}, Symbol)))
@test Core.Compiler.is_effect_free(Base.infer_effects(getglobal, (Module, Symbol)))

# Test that UseRefIterator gets SROA'd inside of new_to_regular (#44557)
# expression and new_to_regular offset are arbitrary here, we just want to see the UseRefIterator erased
let e = Expr(:call, Core.GlobalRef(Base, :arrayset), false, Core.SSAValue(4), Core.SSAValue(9), Core.SSAValue(8))
    new_to_reg(expr) = Core.Compiler.new_to_regular(expr, 1)
    @allocated new_to_reg(e) # warmup call
    @test (@allocated new_to_reg(e)) == 0
end

# Test that SROA doesn't try to forward a previous iteration's SSA value
let sroa_no_forward() = begin
    res = (0, 0)
    for i in 1:5
        a = first(res)
        a == 5 && error()
        if i == 1
            res = (i, 2.0)
        end
    end
    return res
    end
    @test sroa_no_forward() == (1, 2.0)
end

@noinline function foo_defined_last_iter(n::Int)
    local x
    for i = 1:n
        if i == 5
            x = 1
        end
    end
    if n > 2
        return x + n
    end
    return 0
end
const_call_defined_last_iter() = foo_defined_last_iter(3)
@test foo_defined_last_iter(2) == 0
@test_throws UndefVarError foo_defined_last_iter(3)
@test_throws UndefVarError const_call_defined_last_iter()
@test foo_defined_last_iter(6) == 7

let src = code_typed1(foo_defined_last_iter, Tuple{Int})
    for i = 1:length(src.code)
        e = src.code[i]
        if isexpr(e, :throw_undef_if_not)
            @assert !isa(e.args[2], Bool)
        end
    end
end

# Issue #47180, incorrect phi counts in CmdRedirect
function a47180(b; stdout )
    c = setenv(b, b.env)
    if true
        c = pipeline(c, stdout)
    end
    c
end
@test isa(a47180(``; stdout), Base.AbstractCmd)

# Test that _compute_sparams can be eliminated for NamedTuple
named_tuple_elim(name::Symbol, result) = NamedTuple{(name,)}(result)
let src = code_typed1(named_tuple_elim, Tuple{Symbol, Tuple})
    @test count(iscall((src, Core._compute_sparams)), src.code) == 0 &&
          count(iscall((src, Core._svec_ref)), src.code) == 0 &&
          count(iscall(x->!isa(argextype(x, src).val, Core.Builtin)), src.code) == 0
end

# Test that sroa works if the struct type is a PartialStruct
mutable struct OneConstField
    const a::Int
    b::Int
end

@eval function one_const_field_partial()
    # Use explicit :new here to avoid inlining messing with the type
    strct = $(Expr(:new, OneConstField, 1, 2))
    strct.b = 4
    strct.b = 5
    return strct.b
end
@test fully_eliminated(one_const_field_partial; retval=5)

# Test that SROA updates the type of intermediate phi nodes (#50285)
struct Immut50285
    x::Any
end

function immut50285(b, x, y)
    if b
       z = Immut50285(x)
    else
       z = Immut50285(y)
    end
    z.x::Union{Float64, Int}
end

let src = code_typed1(immut50285, Tuple{Bool, Int, Float64})
    @test count(isnew, src.code) == 0
    @test count(iscall((src, typeassert)), src.code) == 0
end

function mut50285(b, x, y)
    z = Ref{Any}()
    if b
       z[] = x
    else
       z[] = y
    end
    z[]::Union{Float64, Int}
end

let src = code_typed1(mut50285, Tuple{Bool, Int, Float64})
    @test count(isnew, src.code) == 0
    @test count(iscall((src, typeassert)), src.code) == 0
end

# Test that we can eliminate new{typeof(x)}(x)
struct TParamTypeofTest1{T}
    x::T
    @eval TParamTypeofTest1(x) = $(Expr(:new, :(TParamTypeofTest1{typeof(x)}), :x))
end
tparam_typeof_test_elim1(x) = TParamTypeofTest1(x).x
@test fully_eliminated(tparam_typeof_test_elim1, Tuple{Any})

struct TParamTypeofTest2{S,T}
    x::S
    y::T
    @eval TParamTypeofTest2(x, y) = $(Expr(:new, :(TParamTypeofTest2{typeof(x),typeof(y)}), :x, :y))
end
tparam_typeof_test_elim2(x, y) = TParamTypeofTest2(x, y).x
@test fully_eliminated(tparam_typeof_test_elim2, Tuple{Any,Any})

# Test that sroa doesn't get confused by free type parameters in struct types
struct Wrap1{T}
    x::T
    @eval @inline (T::Type{Wrap1{X}} where X)(x) = $(Expr(:new, :T, :x))
end
Wrap1(x) = Wrap1{typeof(x)}(x)

function wrap1_wrap1_ifelse(b, x, w1)
    w2 = Wrap1(Wrap1(x))
    w3 = Wrap1(typeof(w1)(w1.x))
    Core.ifelse(b, w3, w2).x.x
end
function wrap1_wrap1_wrapper(b, x, y)
    w1 = Base.inferencebarrier(Wrap1(y))::Wrap1{<:Union{Int, Float64}}
    wrap1_wrap1_ifelse(b, x, w1)
end
@test wrap1_wrap1_wrapper(true, 1, 1.0) === 1.0
@test wrap1_wrap1_wrapper(false, 1, 1.0) === 1

# Test unswitching-union optimization within SRO Apass
function sroaunswitchuniontuple(c, x1, x2)
    t = c ? (x1,) : (x2,)
    return getfield(t, 1)
end
struct SROAUnswitchUnion1{T}
    x::T
end
struct SROAUnswitchUnion2{S,T}
    x::T
    @inline SROAUnswitchUnion2{S}(x::T) where {S,T} = new{S,T}(x)
end
function sroaunswitchunionstruct1(c, x1, x2)
    x = c ? SROAUnswitchUnion1(x1) : SROAUnswitchUnion1(x2)
    return getfield(x, :x)
end
function sroaunswitchunionstruct2(c, x1, x2)
    x = c ? SROAUnswitchUnion2{:a}(x1) : SROAUnswitchUnion2{:a}(x2)
    return getfield(x, :x)
end
let src = code_typed1(sroaunswitchuniontuple, Tuple{Bool, Int, Float64})
    @test count(isnew, src.code) == 0
    @test count(iscall((src, getfield)), src.code) == 0
end
let src = code_typed1(sroaunswitchunionstruct1, Tuple{Bool, Int, Float64})
    @test count(isnew, src.code) == 0
    @test count(iscall((src, getfield)), src.code) == 0
end
@test sroaunswitchunionstruct2(true, 1, 1.0) === 1
@test sroaunswitchunionstruct2(false, 1, 1.0) === 1.0

# Test SROA of union into getfield
struct SingleFieldStruct1
    x::Int
end
struct SingleFieldStruct2
    x::Int
end
function foo(b, x)
    if b
        f = SingleFieldStruct1(x)
    else
        f = SingleFieldStruct2(x)
    end
    getfield(f, :x) + 1
end
@test foo(true, 1) == 2

# ifelse folding
@test Core.Compiler.is_removable_if_unused(Base.infer_effects(exp, (Float64,)))
@test !Core.Compiler.is_inlineable(code_typed1(exp, (Float64,)))
@test fully_eliminated(; retval=Core.Argument(2)) do x::Float64
    return Core.ifelse(true, x, exp(x))
end
@test fully_eliminated(; retval=Core.Argument(2)) do x::Float64
    return ifelse(true, x, exp(x)) # the optimization should be applied to post-inlining IR too
end
@test fully_eliminated(; retval=Core.Argument(2)) do x::Float64
    return ifelse(isa(x, Float64), x, exp(x))
end
func_coreifelse(c, x) = Core.ifelse(c, x, x)
func_ifelse(c, x) = ifelse(c, x, x)
@test fully_eliminated(func_coreifelse, (Bool,Float64); retval=Core.Argument(3))
@test !fully_eliminated(func_coreifelse, (Any,Float64))
@test fully_eliminated(func_ifelse, (Bool,Float64); retval=Core.Argument(3))
@test !fully_eliminated(func_ifelse, (Any,Float64))

# PhiC fixup of compact! with cfg modification
@inline function big_dead_throw_catch()
    x = 1
    try
        x = 2
        if Ref{Bool}(false)[]
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            Base.donotdelete(x)
            x = 3
        end
    catch
        return x
    end
end

function call_big_dead_throw_catch()
    if Ref{Bool}(false)[]
        return big_dead_throw_catch()
    end
    return 4
end

# Issue #51159 - Unreachable reached in try-catch block
function f_with_early_try_catch_exit()
    result = false
    for i in 3
        x = try
        catch
            # This introduces an early Expr(:leave) that we must respect when building
            # φᶜ-nodes in slot2ssa. In particular, we have to ignore the `result = x`
            # assignment that occurs outside of this try-catch block
            continue
        end
        result = x
    end
    result
end

let ir = first(only(Base.code_ircode(f_with_early_try_catch_exit, (); optimize_until="compact")))
    for i = 1:length(ir.stmts)
        expr = ir.stmts[i][:stmt]
        if isa(expr, PhiCNode)
            # The φᶜ should only observe the value of `result` at the try-catch :enter
            # (from the `result = false` assignment), since `result = x` assignment is
            # dominated by an Expr(:leave).
            @test length(expr.values) == 1
        end
    end
end

@test isnothing(f_with_early_try_catch_exit())

# Issue #51144 - UndefRefError during compaction
let code = Any[
        # block 1  → 2, 3
        #=  %1: =# Expr(:(=), Core.SlotNumber(4), Core.Argument(2)),
        #=  %2: =# Expr(:call, :(===), Core.SlotNumber(4), nothing),
        #=  %3: =# GotoIfNot(Core.SSAValue(1), 5),
        # block 2
        #=  %4: =# ReturnNode(nothing),
        # block 3  → 4, 5
        #=  %5: =# Expr(:(=), Core.SlotNumber(4), false),
        #=  %6: =# GotoIfNot(Core.Argument(2), 8),
        # block 4  → 5
        #=  %7: =# Expr(:(=), Core.SlotNumber(4), true),
        # block 5
        #=  %8: =# ReturnNode(nothing), # Must not insert a π-node here
    ]
    slottypes = Any[Any, Union{Bool, Nothing}, Bool, Union{Bool, Nothing}]
    src = make_codeinfo(code; slottypes)

    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ());
    mi.specTypes = Tuple{}
    mi.def = Module()

    # Simulate the important results from inference
    interp = Core.Compiler.NativeInterpreter()
    sv = Core.Compiler.OptimizationState(mi, src, interp)
    slot_id = 4
    for block_id = 3:5
        # (_4 !== nothing) conditional narrows the type, triggering PiNodes
        sv.bb_vartables[block_id][slot_id] = VarState(Bool, #= maybe_undef =# false)
    end

    ir = Core.Compiler.convert_to_ircode(src, sv)
    ir = Core.Compiler.slot2reg(ir, src, sv)
    ir = Core.Compiler.compact!(ir)

    Core.Compiler.verify_ir(ir)
end

function f_with_merge_to_entry_block()
    while true
        i = @noinline rand(Int)
        if @noinline isodd(i)
            return i
        end
    end
end

let (ir, _) = only(Base.code_ircode(f_with_merge_to_entry_block))
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
end

# Test that CFG simplify doesn't leave an un-renamed SSA Value
let # Test that CFG simplify doesn't try to merge every block in a loop into
    # its predecessor
    code = Any[
        # Block 1
        GotoIfNot(Argument(1), 3),
        # Block 2
        GotoNode(5),
        # Block 3
        Expr(:call, Base.inferencebarrier, 1),
        GotoNode(6),
        # Block 4
        Expr(:call, Base.inferencebarrier, 2), # fallthrough
        # Block 5
        PhiNode(Int32[4, 5], Any[SSAValue(3), SSAValue(5)]),
        ReturnNode(1)
    ]
    ir = make_ircode(code)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) == 4
end

# JET.test_opt(Core.Compiler.cfg_simplify!, (Core.Compiler.IRCode,))

# Test support for Core.OptimizedGenerics.KeyValue protocol
function persistent_dict_elim()
    a = Base.PersistentDict(:a => 1)
    return a[:a]
end

# Ideally we would be able to fully eliminate this,
# but currently this would require an extra round of constprop
@test_broken fully_eliminated(persistent_dict_elim)
@test code_typed(persistent_dict_elim)[1][1].code[end] == Core.ReturnNode(1)

function persistent_dict_elim_multiple()
    a = Base.PersistentDict(:a => 1)
    b = Base.PersistentDict(a, :b => 2)
    return b[:a]
end
@test_broken fully_eliminated(persistent_dict_elim_multiple)
let code = code_typed(persistent_dict_elim_multiple)[1][1].code
    @test count(x->isexpr(x, :invoke), code) == 0
    @test code[end] == Core.ReturnNode(1)
end

function persistent_dict_elim_multiple_phi(c::Bool)
    if c
        a = Base.PersistentDict(:a => 1)
    else
        a = Base.PersistentDict(:a => 1)
    end
    b = Base.PersistentDict(a, :b => 2)
    return b[:a]
end
@test_broken fully_eliminated(persistent_dict_elim_multiple_phi)
@test code_typed(persistent_dict_elim_multiple_phi)[1][1].code[end] == Core.ReturnNode(1)

function persistent_dict_elim_multiple_phi2(c::Bool)
    z = Base.inferencebarrier(1)::Int
    if c
        a = Base.PersistentDict(:a => z)
    else
        a = Base.PersistentDict(:a => z)
    end
    b = Base.PersistentDict(a, :b => 2)
    return b[:a]
end
@test persistent_dict_elim_multiple_phi2(true) == 1

# Test CFG simplify with try/catch blocks
let code = Any[
        # Block 1
        GotoIfNot(Argument(1), 5),
        # Block 2
        EnterNode(4),
        # Block 3
        Expr(:leave, SSAValue(2)),
        # Block 4
        GotoNode(5),
        # Block 5
        ReturnNode(1)
    ]
    ir = make_ircode(code)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) <= 5
end

# Test CFG simplify with single predecessor phi node
let code = Any[
        # Block 1
        GotoNode(3),
        # Block 2
        nothing,
        # Block 3
        Expr(:call, Base.inferencebarrier, 1),
        GotoNode(5),
        # Block 4
        PhiNode(Int32[4], Any[SSAValue(3)]),
        ReturnNode(SSAValue(5))
    ]
    ir = make_ircode(code)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) <= 2
    ir = Core.Compiler.compact!(ir)
    @test length(ir.stmts) <= 3
    @test (ir[SSAValue(length(ir.stmts))][:stmt]::ReturnNode).val !== nothing
end

let code = Any[
    Expr(:call, Base.inferencebarrier, Argument(1)), # ::Bool
    Expr(:call, Core.tuple, 1), # ::Tuple{Int}
    Expr(:call, Core.tuple, 1.0), # ::Tuple{Float64}
    Expr(:call, Core.ifelse, SSAValue(1), SSAValue(2), SSAValue(3)), # ::Tuple{Int} (e.g. from inlining)
    Expr(:call, Core.getfield, SSAValue(4), 1), # ::Int
    ReturnNode(SSAValue(5))
]
    try
        argtypes = Any[Bool]
        ssavaluetypes = Any[Bool, Tuple{Int}, Tuple{Float64}, Tuple{Int}, Int, Any]
        ir = make_ircode(code; slottypes=argtypes, ssavaluetypes)
        Core.Compiler.verify_ir(ir)
        Core.Compiler.__set_check_ssa_counts(true)
        ir = Core.Compiler.sroa_pass!(ir)
        Core.Compiler.verify_ir(ir)
    finally
        Core.Compiler.__set_check_ssa_counts(false)
    end
end

# Test SROA all_same on NewNode
let code = Any[
    # Block 1
    Expr(:call, tuple, Argument(1)),
    GotoIfNot(Argument(4), 5),
    # Block 2
    Expr(:call, tuple, Argument(2)),
    GotoIfNot(Argument(4), 9),
    # Block 3
    PhiNode(Int32[2, 4], Any[SSAValue(1), SSAValue(3)]),
    Expr(:call, getfield, SSAValue(5), 1),
    Expr(:call, tuple, SSAValue(6), Argument(2)), # ::Tuple{Int, Int}
    Expr(:call, tuple, SSAValue(7), Argument(3)), # ::Tuple{Tuple{Int, Int}, Int}
    # Block 4
    PhiNode(Int32[4, 8], Any[nothing, SSAValue(8)]),
    Expr(:call, Core.Intrinsics.not_int, Argument(4)),
    GotoIfNot(SSAValue(10), 13),
    # Block 5
    ReturnNode(1),
    # Block 6
    PiNode(SSAValue(9), Tuple{Tuple{Int, Int}, Int}),
    Expr(:call, getfield, SSAValue(13), 1),
    Expr(:call, getfield, SSAValue(14), 1),
    ReturnNode(SSAValue(15))
]

    argtypes = Any[Int, Int, Int, Bool]
    ssavaluetypes = Any[Tuple{Int}, Any, Tuple{Int}, Any, Tuple{Int}, Int, Tuple{Int, Int}, Tuple{Tuple{Int, Int}, Int},
                        Union{Nothing, Tuple{Tuple{Int, Int}, Int}}, Bool, Any, Any,
                        Tuple{Tuple{Int, Int}, Int},
                        Tuple{Int, Int}, Int, Any]
    ir = make_ircode(code; slottypes=argtypes, ssavaluetypes)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.sroa_pass!(ir)
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.compact!(ir)
    Core.Compiler.verify_ir(ir)
end

# Test correctness of current_scope folding
@eval function scope_folding()
    $(Expr(:tryfinally,
        Expr(:block,
            Expr(:tryfinally, :(), :(), 2),
            :(return Core.current_scope())),
    :(), 1))
end

@eval function scope_folding_opt()
    $(Expr(:tryfinally,
        Expr(:block,
            Expr(:tryfinally, :(), :(), :(Base.inferencebarrier(2))),
            :(return Core.current_scope())),
    :(), :(Base.inferencebarrier(1))))
end

@test scope_folding() == 1
@test scope_folding_opt() == 1
@test_broken fully_eliminated(scope_folding)
@test_broken fully_eliminated(scope_folding_opt)

# Function that happened to have lots of sroa that
# happened to trigger a bad case in the renamer. We
# just want to check this doesn't crash in inference.
function f52610()
    slots_dict = IdDict()
    for () in Base.inferencebarrier(1)
       for x in 1
           if Base.inferencebarrier(true)
               slots_dict[x] = 0
           end
       end
    end
    return nothing
end
@test code_typed(f52610)[1][2] === Nothing

# Issue #52703
@eval function f52703()
    try
        $(Expr(:tryfinally,
            Expr(:block,
                Expr(:tryfinally, :(), :(), 2),
                :(return Base.inferencebarrier(Core.current_scope)()::Int)),
        :(), 1))
    catch
        return 1
    end
    return 0
end
@test code_typed(f52703)[1][2] === Int

# Issue #52858 - compaction gets confused by pending node
let code = Any[
    # Block 1
    GotoIfNot(true, 6),
    # Block 2
    Expr(:call, println, 1),
    Expr(:call, Base.inferencebarrier, true),
    GotoIfNot(SSAValue(3), 6),
    # Block 3
    nothing,
    # Block 4
    PhiNode(Int32[1, 4, 5], Any[1, 2, 3]),
    ReturnNode(SSAValue(6))
]
    ir = make_ircode(code)
    Core.Compiler.insert_node!(ir, SSAValue(5),
        Core.Compiler.NewInstruction(
            Expr(:call, println, 2), Nothing, Int32(1)),
            #= attach_after = =# true)
    ir = Core.Compiler.compact!(ir, true)
    @test Core.Compiler.verify_ir(ir) === nothing
    @test count(x->isa(x, GotoIfNot), ir.stmts.stmt) == 1
end

# Issue #52857 - Affinity of sroa definedness check
let code = Any[
    Expr(:new, ImmutableRef{Any}),
    GotoIfNot(Argument(1), 4),
    Expr(:call, GlobalRef(Base, :getfield), SSAValue(1), 1), # Will throw
    ReturnNode(1)
]
    ir = make_ircode(code; ssavaluetypes = Any[ImmutableRef{Any}, Any, Any, Any], slottypes=Any[Bool], verify=true)
    ir = Core.Compiler.sroa_pass!(ir)
    @test Core.Compiler.verify_ir(ir) === nothing
    @test !any(iscall((ir, getfield)), ir.stmts.stmt)
    @test length(ir.cfg.blocks[end].stmts) == 1
end

# https://github.com/JuliaLang/julia/issues/47065
# `Core.Compiler.sort!` should be able to handle a big list
let n = 1000
    ex = :(return 1)
    for _ in 1:n
        ex = :(rand() < .1 && $(ex))
    end
    @eval global function f_1000_blocks()
        $ex
        return 0
    end
end
@test f_1000_blocks() == 0

# https://github.com/JuliaLang/julia/issues/53521
# Incorrect scope counting in :leave
using Base.ScopedValues
function f53521()
    VALUE = ScopedValue(1)
    @with VALUE => 2 begin
        for i = 1
            @with VALUE => 3 begin
                try
                    foo()
                catch
                    nothing
                end
            end
        end
    end
end
@test code_typed(f53521)[1][2] === Nothing

# Test that adce_pass! sets Refined on PhiNode values
let code = Any[
    # Basic Block 1
    GotoIfNot(false, 3)
    # Basic Block 2
    nothing
    # Basic Block 3
    PhiNode(Int32[1, 2], Any[1.0, 1])
    ReturnNode(Core.SSAValue(3))
]
    ir = make_ircode(code; ssavaluetypes=Any[Any, Nothing, Union{Int64, Float64}, Any])
    (ir, made_changes) = Core.Compiler.adce_pass!(ir)
    @test made_changes
    @test (ir[Core.SSAValue(length(ir.stmts))][:flag] & Core.Compiler.IR_FLAG_REFINED) != 0
end

# JuliaLang/julia#52991: statements that may not :terminate should not be deleted
@noinline Base.@assume_effects :effect_free :nothrow function issue52991(n)
    local s = 0
    try
        while true
            yield()
            if n - rand(1:10) > 0
                s += 1
            else
                break
            end
        end
    catch
    end
    return s
end
@test !Core.Compiler.is_removable_if_unused(Base.infer_effects(issue52991, (Int,)))
let src = code_typed1((Int,)) do x
        issue52991(x)
        nothing
    end
    @test count(isinvoke(:issue52991), src.code) == 1
end
let t = @async begin
        issue52991(11) # this call never terminates
        nothing
    end
    sleep(1)
    if istaskdone(t)
        ok = false
    else
        ok = true
        schedule(t, InterruptException(); error=true)
    end
    @test ok
end

# JuliaLang/julia47664
@test !fully_eliminated() do
    any(isone, Iterators.repeated(0))
end
@test !fully_eliminated() do
    all(iszero, Iterators.repeated(0))
end

## Test that cfg_simplify respects implicit `unreachable` terminators
let code = Any[
        # block 1
        GotoIfNot(Core.Argument(2), 4),
        # block 2
        Expr(:call, Base.throw, "error"), # an implicit `unreachable` terminator
        # block 3
        Expr(:call, :opaque),
        # block 4
        ReturnNode(nothing),
    ]
    ir = make_ircode(code; ssavaluetypes=Any[Union{}, Union{}, Any, Union{}])

    # Unfortunately `compute_basic_blocks` does not notice the `throw()` so it gives us
    # a slightly imprecise CFG. Instead manually construct the CFG we need for this test:
    empty!(ir.cfg.blocks)
    push!(ir.cfg.blocks, BasicBlock(StmtRange(1,1), [], [2,4]))
    push!(ir.cfg.blocks, BasicBlock(StmtRange(2,2), [1], []))
    push!(ir.cfg.blocks, BasicBlock(StmtRange(3,3), [], []))
    push!(ir.cfg.blocks, BasicBlock(StmtRange(4,4), [1], []))
    empty!(ir.cfg.index)
    append!(ir.cfg.index, Int[2,3,4])
    ir.stmts.stmt[1] = GotoIfNot(Core.Argument(2), 4)

    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) == 3 # should have removed block 3
end
