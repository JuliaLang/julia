# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.Meta
import Core:
    CodeInfo, Argument, SSAValue, GotoNode, GotoIfNot, PiNode, PhiNode,
    QuoteNode, ReturnNode

include(normpath(@__DIR__, "irutils.jl"))

# domsort
# =======

## Test that domsort doesn't mangle single-argument phis (#29262)
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::CodeInfo
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
    src = m.args[1]::CodeInfo
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

# SROA
# ====

import Core.Compiler: widenconst, is_array_alloc

is_load_forwarded(src::CodeInfo) =
    !any(iscall((src, getfield)), src.code) && !any(iscall((src, Core.arrayref)), src.code)
function is_scalar_replaced(src::CodeInfo)
    is_load_forwarded(src) || return false
    any(iscall((src, setfield!)), src.code) && return false
    any(isnew, src.code) && return false
    any(iscall((src, Core.arrayset)), src.code) && return false
    any(is_array_alloc, src.code) && return false
    return true
end

function is_load_forwarded(@nospecialize(T), src::CodeInfo)
    for i in 1:length(src.code)
        x = src.code[i]
        if iscall((src, getfield), x)
            widenconst(argextype(x.args[1], src)) <: T && return false
        elseif iscall((src, Core.arrayref), x)
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
        elseif iscall((src, Core.arrayset), x)
            widenconst(argextype(x.args[1], src)) <: T && return false
        elseif is_array_alloc(x)
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

# load forwarding
# ---------------
# even if allocation can't be eliminated

# safe cases
for T in (ImmutableRef{Any}, Ref{Any})
    let src = @eval code_typed1((Bool,Any,)) do c, a
            r = $T(a)
            if c
                return r[]
            else
                return r
            end
        end
        @test is_load_forwarded(src)
        @test count(isnew, src.code) == 1
    end
    let src = @eval code_typed1((Bool,String,)) do c, a
            r = $T(a)
            if c
                return r[]::String # adce_pass! will further eliminate this type assert call also
            else
                return r
            end
        end
        @test is_load_forwarded(src)
        @test count(isnew, src.code) == 1
        @test !any(iscall((src, typeassert)), src.code)
    end
    let src = @eval code_typed1((Bool,Any,)) do c, a
            r = $T(a)
            if c
                return r[]
            else
                throw(r)
            end
        end
        @test is_load_forwarded(src)
        @test count(isnew, src.code) == 1
    end
end
let src = code_typed1((Bool,Any,Any)) do c, a, b
        r = Ref{Any}(a)
        if c
            return r[]
        end
        r[] = b
        return r
    end
    @test is_load_forwarded(src)
    @test count(isnew, src.code) == 1
    @test count(iscall((src, setfield!)), src.code) == 1
    @test count(src.code) do @nospecialize x
        isreturn(x) && x.val === Argument(3) # a
    end == 1
end

# unsafe case
let src = code_typed1((Bool,Any,Any)) do c, a, b
        r = Ref{Any}(a)
        r[] = b
        @noinline some_escape!(r)
        return r[]
    end
    @test !is_load_forwarded(src)
    @test count(isnew, src.code) == 1
    @test count(iscall((src, setfield!)), src.code) == 1
end
let src = code_typed1((Bool,String,Regex)) do c, a, b
        r1 = Ref{Any}(a)
        r2 = Ref{Any}(b)
        return ifelse(c, r1, r2)[]
    end
    r = only(findall(isreturn, src.code))
    v = (src.code[r]::Core.ReturnNode).val
    @test v !== Argument(3) # a
    @test v !== Argument(4) # b
    @test_broken is_load_forwarded(src) # ideally
end

# aliased load forwarding
# -----------------------

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

# OK: mutable(immutable(...)) case
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
let # some insanely nested example
    src = code_typed1((Int,)) do x
        (Ref(Ref(Ref(Ref(Ref(Ref(Ref(Ref(Ref(Ref((x))))))))))))[][][][][][][][][][]
    end
    @test is_scalar_replaced(src)
end

# OK: immutable(mutable(...)) case
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = ImmutableXYZ(x, y, z)
        outer = MutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=z=# Core.Argument(4)]
    end
end

# OK: mutable(mutable(...)) case
# new chain
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        outer = MutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=z=# Core.Argument(4)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.x, xyz.y, xyz.z = z, y, x
        outer = MutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=z=# Core.Argument(4), #=y=# Core.Argument(3), #=x=# Core.Argument(2)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.x, xyz.y, xyz.z = xyz.z, xyz.y, xyz.x
        outer = MutableOuter(xyz, xyz, xyz)
        outer.x.x, outer.y.y, outer.z.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=z=# Core.Argument(4), #=y=# Core.Argument(3), #=x=# Core.Argument(2)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        inner = MutableOuter(xyz, xyz, xyz)
        outer = MutableOuter(inner, inner, inner)
        outer.x.x.x, outer.y.y.y, outer.z.z.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=z=# Core.Argument(4)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        xyz.x, xyz.y, xyz.z = z, y, x
        inner = MutableOuter(xyz, xyz, xyz)
        outer = MutableOuter(inner, inner, inner)
        outer.x.x.x, outer.y.y.y, outer.z.z.z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=z=# Core.Argument(4), #=y=# Core.Argument(3), #=x=# Core.Argument(2)]
    end
end
# setfield! chain
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        outer = Ref{MutableXYZ}()
        outer[] = xyz
        return outer[].x, outer[].y, outer[].z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), #=z=# Core.Argument(4)]
    end
end
let src = code_typed1((Any,Any,Any)) do x, y, z
        xyz = MutableXYZ(x, y, z)
        outer = Ref{MutableXYZ}()
        outer[] = xyz
        xyz.z = 42
        return outer[].x, outer[].y, outer[].z
    end
    @test is_scalar_replaced(src)
    @test any(src.code) do @nospecialize x
        iscall((src, tuple), x) &&
        x.args[2:end] == Any[#=x=# Core.Argument(2), #=y=# Core.Argument(3), 42]
    end
end

# ϕ-allocation elimination
# ------------------------

# safe cases
let src = code_typed1((Bool,Any,Any)) do cond, x, y
        if cond
            ϕ = Ref{Any}(x)
        else
            ϕ = Ref{Any}(y)
        end
        ϕ[]
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.PhiNode) &&
        #=x=# Core.Argument(3) in x.values &&
        #=y=# Core.Argument(4) in x.values
    end == 1
end
let src = code_typed1((Bool,Bool,Any,Any,Any)) do cond1, cond2, x, y, z
        if cond1
            ϕ = Ref{Any}(x)
        elseif cond2
            ϕ = Ref{Any}(y)
        else
            ϕ = Ref{Any}(z)
        end
        ϕ[]
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.PhiNode) &&
        #=x=# Core.Argument(4) in x.values &&
        #=y=# Core.Argument(5) in x.values &&
        #=z=# Core.Argument(6) in x.values
    end == 1
end
let src = code_typed1((Bool,Any,Any,Any)) do cond, x, y, z
        if cond
            ϕ = Ref{Any}(x)
        else
            ϕ = Ref{Any}(y)
        end
        ϕ[] = z
        ϕ[]
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.ReturnNode) &&
        #=z=# Core.Argument(5) === x.val
    end == 1
end
let src = code_typed1((Bool,Any,Any,)) do cond, x, y
        if cond
            ϕ = Ref{Any}(x)
            out1 = ϕ[]
        else
            ϕ = Ref{Any}(y)
            out1 = ϕ[]
        end
        out2 = ϕ[]
        out1, out2
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.PhiNode) &&
        #=x=# Core.Argument(3) in x.values &&
        #=y=# Core.Argument(4) in x.values
    end == 2
end
let src = code_typed1((Bool,Any,Any,Any)) do cond, x, y, z
        if cond
            ϕ = Ref{Any}(x)
        else
            ϕ = Ref{Any}(y)
            ϕ[] = z
        end
        ϕ[]
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.PhiNode) &&
        #=x=# Core.Argument(3) in x.values &&
        #=z=# Core.Argument(5) in x.values
    end == 1
end
let src = code_typed1((Bool,Any,Any,Any)) do cond, x, y, z
        if cond
            ϕ = Ref{Any}(x)
            out1 = ϕ[]
        else
            ϕ = Ref{Any}(y)
            out1 = ϕ[]
            ϕ[] = z
        end
        out2 = ϕ[]
        out1, out2
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.PhiNode) &&
        #=x=# Core.Argument(3) in x.values &&
        #=y=# Core.Argument(4) in x.values
    end == 1
    @test count(src.code) do @nospecialize x
        isa(x, Core.PhiNode) &&
        #=x=# Core.Argument(3) in x.values &&
        #=z=# Core.Argument(5) in x.values
    end == 1
end
let src = code_typed1((Bool,Any,Any)) do cond, x, y
        # these allocation form multiple ϕ-nodes
        if cond
            ϕ2 = ϕ1 = Ref{Any}(x)
        else
            ϕ2 = ϕ1 = Ref{Any}(y)
        end
        ϕ1[], ϕ2[]
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.PhiNode) &&
        #=x=# Core.Argument(3) in x.values &&
        #=y=# Core.Argument(4) in x.values
    end == 1
end
let src = code_typed1((Bool,String,)) do cond, x
        # these allocation form multiple ϕ-nodes
        if cond
            ϕ2 = ϕ1 = Ref{Any}("foo")
        else
            ϕ2 = ϕ1 = Ref{Any}("bar")
        end
        ϕ2[] = x
        y = ϕ1[] # => x
        return y
    end
    @test is_scalar_replaced(src)
    @test count(src.code) do @nospecialize x
        isa(x, Core.ReturnNode) &&
        #=x=# x.val === Core.Argument(3)
    end == 1
end

# unsafe cases
let src = code_typed1((Bool,Any,Any)) do cond, x, y
        if cond
            ϕ = Ref{Any}(x)
        else
            ϕ = Ref{Any}(y)
        end
        some_escape!(ϕ)
        ϕ[]
    end
    @test count(isnew, src.code) == 2
    @test count(iscall((src, getfield)), src.code) == 1
end
let src = code_typed1((Bool,Any,Any)) do cond, x, y
        if cond
            ϕ = Ref{Any}(x)
            some_escape!(ϕ)
        else
            ϕ = Ref{Any}(y)
        end
        ϕ[]
    end
    @test count(isnew, src.code) == 2
    @test count(iscall((src, getfield)), src.code) == 1
end
let src = code_typed1((Bool,Any,)) do cond, x
        if cond
            ϕ = Ref{Any}(x)
        else
            ϕ = Ref{Any}()
        end
        ϕ[]
    end
    @test count(isnew, src.code) == 2
    @test count(iscall((src, getfield)), src.code) == 1
end
let src = code_typed1((Bool,Any)) do c, a
        local r
        if c
            r = Ref{Any}(a)
        end
        (r::Base.RefValue{Any})[]
    end
    @test count(isnew, src.code) == 1
    @test count(iscall((src, getfield)), src.code) == 1
end

function mutable_ϕ_elim(x, xs)
    r = Ref(x)
    for x in xs
        r = Ref(x)
    end
    return r[]
end
let src = code_typed1(mutable_ϕ_elim, (String, Vector{String}))
    @test is_scalar_replaced(Ref{String}, src)

    xs = String[string(gensym()) for _ in 1:100]
    mutable_ϕ_elim("init", xs)
    @test @allocated(mutable_ϕ_elim("init", xs)) == 0
end

# demonstrate the power of our field / alias analysis with realistic end to end examples
# adapted from http://wiki.luajit.org/Allocation-Sinking-Optimization#implementation%5B
abstract type AbstractPoint{T} end
struct Point{T} <: AbstractPoint{T}
    x::T
    y::T
end
mutable struct MPoint{T} <: AbstractPoint{T}
    x::T
    y::T
end
add(a::P, b::P) where P<:AbstractPoint = P(a.x + b.x, a.y + b.y)
function compute_point(T, n, ax, ay, bx, by)
    a = T(ax, ay)
    b = T(bx, by)
    for i in 0:(n-1)
        a = add(add(a, b), b)
    end
    a.x, a.y
end
function compute_point(n, a, b)
    for i in 0:(n-1)
        a = add(add(a, b), b)
    end
    a.x, a.y
end
function compute_point!(n, a, b)
    for i in 0:(n-1)
        a′ = add(add(a, b), b)
        a.x = a′.x
        a.y = a′.y
    end
end

let # immutable case
    src = code_typed1((Int,)) do n
        compute_point(Point, n, 1+.5, 2+.5, 2+.25, 4+.75)
    end
    @test is_scalar_replaced(Point, src)
    src = code_typed1((Int,)) do n
        compute_point(Point, n, 1+.5im, 2+.5im, 2+.25im, 4+.75im)
    end
    @test is_scalar_replaced(Point, src)
    @test is_load_forwarded(ComplexF64, src)
    @test !is_scalar_replaced(ComplexF64, src)

    # mutable case
    src = code_typed1((Int,)) do n
        compute_point(MPoint, n, 1+.5, 2+.5, 2+.25, 4+.75)
    end
    @test is_scalar_replaced(MPoint, src)
    src = code_typed1((Int,)) do n
        compute_point(MPoint, n, 1+.5im, 2+.5im, 2+.25im, 4+.75im)
    end
    @test is_scalar_replaced(MPoint, src)
    @test is_load_forwarded(ComplexF64, src)
    @test !is_scalar_replaced(ComplexF64, src)
end
compute_point(MPoint, 10, 1+.5, 2+.5, 2+.25, 4+.75)
compute_point(MPoint, 10, 1+.5im, 2+.5im, 2+.25im, 4+.75im)
@test @allocated(compute_point(MPoint, 10000, 1+.5, 2+.5, 2+.25, 4+.75)) == 0
@test @allocated(compute_point(MPoint, 10000, 1+.5im, 2+.5im, 2+.25im, 4+.75im)) == 0

let # immutable case
    src = code_typed1((Int,)) do n
        compute_point(n, Point(1+.5, 2+.5), Point(2+.25, 4+.75))
    end
    @test is_scalar_replaced(Point, src)
    src = code_typed1((Int,)) do n
        compute_point(n, Point(1+.5im, 2+.5im), Point(2+.25im, 4+.75im))
    end
    @test is_scalar_replaced(Point, src)
    @test is_load_forwarded(ComplexF64, src)
    @test !is_scalar_replaced(ComplexF64, src)

    # mutable case
    src = code_typed1((Int,)) do n
        compute_point(n, MPoint(1+.5, 2+.5), MPoint(2+.25, 4+.75))
    end
    @test is_scalar_replaced(MPoint, src)
    src = code_typed1((Int,)) do n
        compute_point(n, MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))
    end
    @test is_scalar_replaced(MPoint, src)
    @test is_load_forwarded(ComplexF64, src)
    @test !is_scalar_replaced(ComplexF64, src)
end
compute_point(10, MPoint(1+.5, 2+.5), MPoint(2+.25, 4+.75))
compute_point(10, MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))
@test @allocated(compute_point(10000, MPoint(1+.5, 2+.5), MPoint(2+.25, 4+.75))) == 0
@test @allocated(compute_point(10000, MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im))) == 0

let # mutable case
    src = code_typed1(compute_point!, (Int,MPoint{Float64},MPoint{Float64}))
    @test is_scalar_replaced(MPoint, src)
    src = code_typed1(compute_point!, (Int,MPoint{ComplexF64},MPoint{ComplexF64}))
    @test is_scalar_replaced(MPoint, src)
    @test is_load_forwarded(ComplexF64, src)
    @test !is_scalar_replaced(ComplexF64, src)
end
let
    af, bf = MPoint(1+.5, 2+.5), MPoint(2+.25, 4+.75)
    ac, bc = MPoint(1+.5im, 2+.5im), MPoint(2+.25im, 4+.75im)
    compute_point!(10, af, bf)
    compute_point!(10, ac, bc)
    @test @allocated(compute_point!(10000, af, bf)) == 0
    @test @allocated(compute_point!(10000, ac, bc)) == 0
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

callit(f, args...) = f(args...)
function isdefined_elim()
    local arr::Vector{Any}
    callit() do
        arr = Any[]
    end
    return arr
end
let src = code_typed1(isdefined_elim)
    @test count(isnew, src.code) == 0 # eliminates closure constructs
end
@test isdefined_elim() == Any[]

# preserve elimination
# --------------------

let src = code_typed1((String,)) do s
        ccall(:some_ccall, Cint, (Ptr{String},), Ref(s))
    end
    @test count(isnew, src.code) == 0
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

# constant globals
# ----------------

let # immutable case
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

# array SROA
# ----------

let src = code_typed1((Any,)) do s
        a = Vector{Any}(undef, 1)
        a[1] = s
        return a[1]
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Any,)) do s
        a = Any[nothing]
        a[1] = s
        return a[1]
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((String,String)) do s, t
        a = Vector{Any}(undef, 2)
        a[1] = Ref(s)
        a[2] = Ref(t)
        return a[1]
    end
    @test count(isnew, src.code) == 1
end
let src = code_typed1((String,)) do s
        a = Vector{Base.RefValue{String}}(undef, 1)
        a[1] = Ref(s)
        return a[1][]
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((String,String)) do s, t
        a = Vector{Base.RefValue{String}}(undef, 2)
        a[1] = Ref(s)
        a[2] = Ref(t)
        return a[1][]
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Any,)) do s
        a = Vector{Any}[Any[nothing]]
        a[1][1] = s
        return a[1][1]
    end
    @test_broken is_scalar_replaced(src)
end
let src = code_typed1((Bool,Any,Any)) do c, s, t
        a = Any[nothing]
        if c
            a[1] = s
        else
            a[1] = t
        end
        return a[1]
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Bool,Any,Any,Any,Any,)) do c, s1, s2, t1, t2
        if c
            a = Vector{Any}(undef, 2)
            a[1] = s1
            a[2] = s2
        else
            a = Vector{Any}(undef, 2)
            a[1] = t1
            a[2] = t2
        end
        return a[1]
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Bool,Any,Any)) do c, s, t
        # XXX this implicitly forms tuple to getfield chains
        # and SROA on it produces complicated control flow
        if c
            a = Any[s]
        else
            a = Any[t]
        end
        return a[1]
    end
    @test_broken is_scalar_replaced(src)
end

# arraylen / arraysize elimination
let src = code_typed1((Any,)) do s
        a = Vector{Any}(undef, 1)
        a[1] = s
        return a[1], length(a)
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Any,)) do s
        a = Matrix{Any}(undef, 2, 2)
        a[1, 1] = s
        return a[1, 1], length(a)
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Any,)) do s
        a = Vector{Any}(undef, 1)
        a[1] = s
        return a[1], size(a, 1)
    end
    @test is_scalar_replaced(src)
end
let src = code_typed1((Any,)) do s
        a = Matrix{Any}(undef, 2, 2)
        a[1, 1] = s
        return a[1, 1], size(a)
    end
    @test is_scalar_replaced(src)
end

# comparison lifting
# ==================

let # lifting `===`
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

let # lifting `isa`
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

let # lifting `isdefined`
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
@test fully_eliminated(f_partial, Tuple{Float64})

# A SSAValue after the compaction line
let m = Meta.@lower 1 + 1
    @assert Meta.isexpr(m, :thunk)
    src = m.args[1]::CodeInfo
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
    ir, = @test_nowarn Core.Compiler.linear_pass!(ir)
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
    src = m.args[1]::CodeInfo
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
    src = m.args[1]::CodeInfo
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
    src = m.args[1]::CodeInfo
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

# test `stmt_effect_free` and DCE
# ===============================

let # effect-freeness computation for array allocation

    # should eliminate dead allocations
    good_dims = (0, 2)
    for dim in good_dims, N in 0:10
        dims = ntuple(i->dim, N)
        @eval @test fully_eliminated(()) do
            Array{Int,$N}(undef, $(dims...))
            nothing
        end
    end

    # shouldn't eliminate errorneous dead allocations
    bad_dims = [-1,           # should keep "invalid Array dimensions"
                typemax(Int)] # should keep "invalid Array size"
    for dim in bad_dims, N in 1:10
        dims = ntuple(i->dim, N)
        @eval @test !fully_eliminated(()) do
            Array{Int,$N}(undef, $(dims...))
            nothing
        end
    end

    # some high-level examples
    @test fully_eliminated(()) do
        Int[]
        nothing
    end
    @test fully_eliminated(()) do
        Matrix{Tuple{String,String}}(undef, 4, 4)
        nothing
    end
    @test fully_eliminated(()) do
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
    @test count(@nospecialize(stmt)->isa(stmt, Core.GotoIfNot), ir.stmts.inst) == 1
    ir = Core.Compiler.compact!(ir, true)
    @test count(@nospecialize(stmt)->isa(stmt, Core.GotoIfNot), ir.stmts.inst) == 0
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
    return 1
end
let ci = code_typed(foo_cfg_empty, Tuple{Bool}, optimize=true)[1][1]
    ir = Core.Compiler.inflate_ir(ci)
    @test length(ir.stmts) == 3
    @test length(ir.cfg.blocks) == 3
    Core.Compiler.verify_ir(ir)
    ir = Core.Compiler.cfg_simplify!(ir)
    Core.Compiler.verify_ir(ir)
    @test length(ir.cfg.blocks) <= 2
    @test isa(ir.stmts[length(ir.stmts)][:inst], ReturnNode)
end
