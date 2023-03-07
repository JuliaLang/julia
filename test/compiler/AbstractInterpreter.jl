# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
const CC = Core.Compiler
import Core: MethodInstance, CodeInstance
import .CC: WorldRange, WorldView

include("irutils.jl")

"""
    @newinterp NewInterpreter

Defines new `NewInterpreter <: AbstractInterpreter` whose cache is separated
from the native code cache, satisfying the minimum interface requirements.
"""
macro newinterp(name)
    cachename = Symbol(string(name, "Cache"))
    name = esc(name)
    quote
        struct $cachename
            dict::IdDict{MethodInstance,CodeInstance}
        end
        struct $name <: CC.AbstractInterpreter
            interp::CC.NativeInterpreter
            cache::$cachename
            meta # additional information
            $name(world = Base.get_world_counter();
                interp = CC.NativeInterpreter(world),
                cache = $cachename(IdDict{MethodInstance,CodeInstance}()),
                meta = nothing,
                ) = new(interp, cache, meta)
        end
        CC.InferenceParams(interp::$name) = CC.InferenceParams(interp.interp)
        CC.OptimizationParams(interp::$name) = CC.OptimizationParams(interp.interp)
        CC.get_world_counter(interp::$name) = CC.get_world_counter(interp.interp)
        CC.get_inference_cache(interp::$name) = CC.get_inference_cache(interp.interp)
        CC.code_cache(interp::$name) = WorldView(interp.cache, WorldRange(CC.get_world_counter(interp)))
        CC.get(wvc::WorldView{<:$cachename}, mi::MethodInstance, default) = get(wvc.cache.dict, mi, default)
        CC.getindex(wvc::WorldView{<:$cachename}, mi::MethodInstance) = getindex(wvc.cache.dict, mi)
        CC.haskey(wvc::WorldView{<:$cachename}, mi::MethodInstance) = haskey(wvc.cache.dict, mi)
        CC.setindex!(wvc::WorldView{<:$cachename}, ci::CodeInstance, mi::MethodInstance) = setindex!(wvc.cache.dict, ci, mi)
    end
end

# OverlayMethodTable
# ==================

import Base.Experimental: @MethodTable, @overlay

@newinterp MTOverlayInterp
@MethodTable(OverlayedMT)
CC.method_table(interp::MTOverlayInterp) = CC.OverlayMethodTable(CC.get_world_counter(interp), OverlayedMT)

function CC.add_remark!(interp::MTOverlayInterp, ::CC.InferenceState, remark)
    if interp.meta !== nothing
        # Core.println(remark)
        push!(interp.meta, remark)
    end
    return nothing
end

strangesin(x) = sin(x)
@overlay OverlayedMT strangesin(x::Float64) = iszero(x) ? nothing : cos(x)

# inference should use the overlayed method table
@test Base.return_types((Float64,); interp=MTOverlayInterp()) do x
    strangesin(x)
end |> only === Union{Float64,Nothing}
@test Base.return_types((Any,); interp=MTOverlayInterp()) do x
    @invoke strangesin(x::Float64)
end |> only === Union{Float64,Nothing}

# effect analysis should figure out that the overlayed method is used
@test Base.infer_effects((Float64,); interp=MTOverlayInterp()) do x
    strangesin(x)
end |> !Core.Compiler.is_nonoverlayed
@test Base.infer_effects((Any,); interp=MTOverlayInterp()) do x
    @invoke strangesin(x::Float64)
end |> !Core.Compiler.is_nonoverlayed

# account for overlay possibility in unanalyzed matching method
callstrange(::Nothing) = Core.compilerbarrier(:type, nothing) # trigger inference bail out
callstrange(::Float64) = strangesin(x)
callstrange_entry(x) = callstrange(x) # needs to be defined here because of world age
let interp = MTOverlayInterp(; meta=Set{Any}())
    matches = Core.Compiler.findall(Tuple{typeof(callstrange),Any}, Core.Compiler.method_table(interp)).matches
    @test Core.Compiler.length(matches) == 2
    if Core.Compiler.getindex(matches, 1).method == which(callstrange, (Nothing,))
        @test Base.infer_effects(callstrange_entry, (Any,); interp) |> !Core.Compiler.is_nonoverlayed
        @test "Call inference reached maximally imprecise information. Bailing on." in interp.meta
    else
        @warn "`nonoverlayed` test for inference bailing out is skipped since the method match sort order is changed."
    end
end

# but it should never apply for the native compilation
@test Base.infer_effects((Float64,)) do x
    strangesin(x)
end |> Core.Compiler.is_nonoverlayed
@test Base.infer_effects((Any,)) do x
    @invoke strangesin(x::Float64)
end |> Core.Compiler.is_nonoverlayed

# fallback to the internal method table
@test Base.return_types((Int,); interp=MTOverlayInterp()) do x
    cos(x)
end |> only === Float64
@test Base.return_types((Any,); interp=MTOverlayInterp()) do x
    @invoke cos(x::Float64)
end |> only === Float64

# not fully covered overlay method match
overlay_match(::Any) = nothing
@overlay OverlayedMT overlay_match(::Int) = missing
@test Base.return_types((Any,); interp=MTOverlayInterp()) do x
    overlay_match(x)
end |> only === Union{Nothing,Missing}

# partial concrete evaluation
@test Base.return_types(; interp=MTOverlayInterp()) do
    isbitstype(Int) ? nothing : missing
end |> only === Nothing
Base.@assume_effects :terminates_globally function issue41694(x)
    res = 1
    1 < x < 20 || throw("bad")
    while x > 1
        res *= x
        x -= 1
    end
    return res
end
@test Base.return_types(; interp=MTOverlayInterp()) do
    issue41694(3) == 6 ? nothing : missing
end |> only === Nothing

# disable partial concrete evaluation when tainted by any overlayed call
Base.@assume_effects :total totalcall(f, args...) = f(args...)
@test Base.return_types(; interp=MTOverlayInterp()) do
    if totalcall(strangesin, 1.0) == cos(1.0)
        return nothing
    else
        return missing
    end
end |> only === Nothing

# AbstractLattice
# ===============

using Core: SlotNumber, Argument
using Core.Compiler: slot_id, tmerge_fast_path
import .CC:
    AbstractLattice, BaseInferenceLattice, IPOResultLattice, InferenceLattice, OptimizerLattice,
    widenlattice, is_valid_lattice_norec, typeinf_lattice, ipo_lattice, optimizer_lattice,
    widenconst, tmeet, tmerge, ⊑, abstract_eval_special_value, widenreturn

@newinterp TaintInterpreter
struct TaintLattice{PL<:AbstractLattice} <: CC.AbstractLattice
    parent::PL
end
CC.widenlattice(𝕃::TaintLattice) = 𝕃.parent
CC.is_valid_lattice_norec(::TaintLattice, @nospecialize(elm)) = isa(elm, Taint)

struct InterTaintLattice{PL<:AbstractLattice} <: CC.AbstractLattice
    parent::PL
end
CC.widenlattice(𝕃::InterTaintLattice) = 𝕃.parent
CC.is_valid_lattice_norec(::InterTaintLattice, @nospecialize(elm)) = isa(elm, InterTaint)

const AnyTaintLattice{L} = Union{TaintLattice{L},InterTaintLattice{L}}

CC.typeinf_lattice(::TaintInterpreter) = InferenceLattice(TaintLattice(BaseInferenceLattice.instance))
CC.ipo_lattice(::TaintInterpreter) = InferenceLattice(InterTaintLattice(IPOResultLattice.instance))
CC.optimizer_lattice(::TaintInterpreter) = InterTaintLattice(OptimizerLattice())

struct Taint
    typ
    slots::BitSet
    function Taint(@nospecialize(typ), slots::BitSet)
        if typ isa Taint
            slots = typ.slots ∪ slots
            typ = typ.typ
        end
        return new(typ, slots)
    end
end
Taint(@nospecialize(typ), id::Int) = Taint(typ, push!(BitSet(), id))
function Base.:(==)(a::Taint, b::Taint)
    return a.typ == b.typ && a.slots == b.slots
end

struct InterTaint
    typ
    slots::BitSet
    function InterTaint(@nospecialize(typ), slots::BitSet)
        if typ isa InterTaint
            slots = typ.slots ∪ slots
            typ = typ.typ
        end
        return new(typ, slots)
    end
end
InterTaint(@nospecialize(typ), id::Int) = InterTaint(typ, push!(BitSet(), id))
function Base.:(==)(a::InterTaint, b::InterTaint)
    return a.typ == b.typ && a.slots == b.slots
end

const AnyTaint = Union{Taint, InterTaint}

function CC.tmeet(𝕃::AnyTaintLattice, @nospecialize(v), @nospecialize(t::Type))
    T = isa(𝕃, TaintLattice) ? Taint : InterTaint
    if isa(v, T)
        v = v.typ
    end
    return tmeet(widenlattice(𝕃), v, t)
end
function CC.tmerge(𝕃::AnyTaintLattice, @nospecialize(typea), @nospecialize(typeb))
    r = tmerge_fast_path(𝕃, typea, typeb)
    r !== nothing && return r
    # type-lattice for Taint
    T = isa(𝕃, TaintLattice) ? Taint : InterTaint
    if isa(typea, T)
        if isa(typeb, T)
            return T(
                tmerge(widenlattice(𝕃), typea.typ, typeb.typ),
                typea.slots ∪ typeb.slots)
        else
            typea = typea.typ
        end
    elseif isa(typeb, T)
        typeb = typeb.typ
    end
    return tmerge(widenlattice(𝕃), typea, typeb)
end
function CC.:⊑(𝕃::AnyTaintLattice, @nospecialize(typea), @nospecialize(typeb))
    T = isa(𝕃, TaintLattice) ? Taint : InterTaint
    if isa(typea, T)
        if isa(typeb, T)
            typea.slots ⊆ typeb.slots || return false
            return ⊑(widenlattice(𝕃), typea.typ, typeb.typ)
        end
        typea = typea.typ
    elseif isa(typeb, T)
        return false
    end
    return ⊑(widenlattice(𝕃), typea, typeb)
end
CC.widenconst(taint::AnyTaint) = widenconst(taint.typ)

function CC.abstract_eval_special_value(interp::TaintInterpreter,
    @nospecialize(e), vtypes::CC.VarTable, sv::CC.InferenceState)
    ret = @invoke CC.abstract_eval_special_value(interp::CC.AbstractInterpreter,
        e::Any, vtypes::CC.VarTable, sv::CC.InferenceState)
    if isa(e, SlotNumber) || isa(e, Argument)
        return Taint(ret, slot_id(e))
    end
    return ret
end

function CC.widenreturn(𝕃::InferenceLattice{<:InterTaintLattice}, @nospecialize(rt), @nospecialize(bestguess), nargs::Int, slottypes::Vector{Any}, changes::CC.VarTable)
    if isa(rt, Taint)
        return InterTaint(rt.typ, BitSet((id for id in rt.slots if id ≤ nargs)))
    end
    return CC.widenreturn(widenlattice(𝕃), rt, bestguess, nargs, slottypes, changes)
end

@test CC.tmerge(typeinf_lattice(TaintInterpreter()), Taint(Int, 1), Taint(Int, 2)) == Taint(Int, BitSet(1:2))

# code_typed(ifelse, (Bool, Int, Int); interp=TaintInterpreter())

# External lattice without `Conditional`

import .CC:
    AbstractLattice, ConstsLattice, PartialsLattice, InferenceLattice, OptimizerLattice,
    typeinf_lattice, ipo_lattice, optimizer_lattice

@newinterp NonconditionalInterpreter
CC.typeinf_lattice(::NonconditionalInterpreter) = InferenceLattice(PartialsLattice(ConstsLattice()))
CC.ipo_lattice(::NonconditionalInterpreter) = InferenceLattice(PartialsLattice(ConstsLattice()))
CC.optimizer_lattice(::NonconditionalInterpreter) = OptimizerLattice(PartialsLattice(ConstsLattice()))

@test Base.return_types((Any,); interp=NonconditionalInterpreter()) do x
    c = isa(x, Int) || isa(x, Float64)
    if c
        return x
    else
        return nothing
    end
end |> only === Any

# CallInfo × inlining
# ===================

import .CC: CallInfo

struct NoinlineInterpreterCache
    dict::IdDict{MethodInstance,CodeInstance}
end

"""
    NoinlineInterpreter(noinline_modules::Set{Module}) <: AbstractInterpreter

An `AbstractInterpreter` that has additional inlineability rules based on caller module context.
"""
struct NoinlineInterpreter <: CC.AbstractInterpreter
    noinline_modules::Set{Module}
    interp::CC.NativeInterpreter
    cache::NoinlineInterpreterCache
    NoinlineInterpreter(noinline_modules::Set{Module}, world = Base.get_world_counter();
        interp = CC.NativeInterpreter(world),
        cache = NoinlineInterpreterCache(IdDict{MethodInstance,CodeInstance}())
        ) = new(noinline_modules, interp, cache)
end
CC.InferenceParams(interp::NoinlineInterpreter) = CC.InferenceParams(interp.interp)
CC.OptimizationParams(interp::NoinlineInterpreter) = CC.OptimizationParams(interp.interp)
CC.get_world_counter(interp::NoinlineInterpreter) = CC.get_world_counter(interp.interp)
CC.get_inference_cache(interp::NoinlineInterpreter) = CC.get_inference_cache(interp.interp)
CC.code_cache(interp::NoinlineInterpreter) = WorldView(interp.cache, WorldRange(CC.get_world_counter(interp)))
CC.get(wvc::WorldView{<:NoinlineInterpreterCache}, mi::MethodInstance, default) = get(wvc.cache.dict, mi, default)
CC.getindex(wvc::WorldView{<:NoinlineInterpreterCache}, mi::MethodInstance) = getindex(wvc.cache.dict, mi)
CC.haskey(wvc::WorldView{<:NoinlineInterpreterCache}, mi::MethodInstance) = haskey(wvc.cache.dict, mi)
CC.setindex!(wvc::WorldView{<:NoinlineInterpreterCache}, ci::CodeInstance, mi::MethodInstance) = setindex!(wvc.cache.dict, ci, mi)

struct NoinlineCallInfo <: CallInfo
    info::CallInfo # wrapped call
end
CC.nsplit_impl(info::NoinlineCallInfo) = CC.nsplit(info.info)
CC.getsplit_impl(info::NoinlineCallInfo, idx::Int) = CC.getsplit(info.info, idx)
CC.getresult_impl(info::NoinlineCallInfo, idx::Int) = CC.getresult(info.info, idx)

function CC.abstract_call(interp::NoinlineInterpreter,
    arginfo::CC.ArgInfo, si::CC.StmtInfo, sv::CC.InferenceState, max_methods::Union{Int,Nothing})
    ret = @invoke CC.abstract_call(interp::CC.AbstractInterpreter,
        arginfo::CC.ArgInfo, si::CC.StmtInfo, sv::CC.InferenceState, max_methods::Union{Int,Nothing})
    if sv.mod in interp.noinline_modules
        return CC.CallMeta(ret.rt, ret.effects, NoinlineCallInfo(ret.info))
    end
    return ret
end
function CC.inlining_policy(interp::NoinlineInterpreter,
    @nospecialize(src), @nospecialize(info::CallInfo), stmt_flag::UInt8, mi::MethodInstance,
    argtypes::Vector{Any})
    if isa(info, NoinlineCallInfo)
        return nothing
    end
    return @invoke CC.inlining_policy(interp::CC.AbstractInterpreter,
        src::Any, info::CallInfo, stmt_flag::UInt8, mi::MethodInstance,
        argtypes::Vector{Any})
end

@inline function inlined_usually(x, y, z)
    return x * y + z
end

# check if the inlining algorithm works as expected
let src = code_typed1((Float64,Float64,Float64)) do x, y, z
        inlined_usually(x, y, z)
    end
    @test count(isinvoke(:inlined_usually), src.code) == 0
    @test count(iscall((src, inlined_usually)), src.code) == 0
end
let NoinlineModule = Module()
    interp = NoinlineInterpreter(Set((NoinlineModule,)))

    # this anonymous function's context is Main -- it should be inlined as usual
    let src = code_typed1((Float64,Float64,Float64); interp) do x, y, z
            inlined_usually(x, y, z)
        end
        @test count(isinvoke(:inlined_usually), src.code) == 0
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end

    # it should work for cached results
    method = only(methods(inlined_usually, (Float64,Float64,Float64,)))
    mi = CC.specialize_method(method, Tuple{typeof(inlined_usually),Float64,Float64,Float64}, Core.svec())
    @test haskey(interp.cache.dict, mi)
    let src = code_typed1((Float64,Float64,Float64); interp) do x, y, z
            inlined_usually(x, y, z)
        end
        @test count(isinvoke(:inlined_usually), src.code) == 0
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end

    # now the context module is `NoinlineModule` -- it should not be inlined
    let src = @eval NoinlineModule $code_typed1((Float64,Float64,Float64); interp=$interp) do x, y, z
            $inlined_usually(x, y, z)
        end
        @test count(isinvoke(:inlined_usually), src.code) == 1
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end

    # the context module is totally irrelevant -- it should be inlined as usual
    OtherModule = Module()
    let src = @eval OtherModule $code_typed1((Float64,Float64,Float64); interp=$interp) do x, y, z
            $inlined_usually(x, y, z)
        end
        @test count(isinvoke(:inlined_usually), src.code) == 0
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end
end
