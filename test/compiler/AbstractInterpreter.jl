# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
const CC = Core.Compiler
import Core: MethodInstance, CodeInstance
import .CC: WorldRange, WorldView

# define new `AbstractInterpreter` that satisfies the minimum interface requirements
# while managing its cache independently
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
            $name(world = Base.get_world_counter();
                interp = CC.NativeInterpreter(world),
                cache = $cachename(IdDict{MethodInstance,CodeInstance}())
                ) = new(interp, cache)
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

# partial pure/concrete evaluation
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

# disable partial pure/concrete evaluation when tainted by any overlayed call
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
    widen, is_valid_lattice, typeinf_lattice, ipo_lattice, optimizer_lattice,
    widenconst, tmeet, tmerge, ⊑, abstract_eval_special_value, widenreturn,
    widenlattice

@newinterp TaintInterpreter
struct TaintLattice{PL<:AbstractLattice} <: CC.AbstractLattice
    parent::PL
end
CC.widenlattice(𝕃::TaintLattice) = 𝕃.parent
CC.is_valid_lattice(𝕃::TaintLattice, @nospecialize(elm)) =
    is_valid_lattice(widenlattice(𝕃), elem) || isa(elm, Taint)

struct InterTaintLattice{PL<:AbstractLattice} <: CC.AbstractLattice
    parent::PL
end
CC.widenlattice(𝕃::InterTaintLattice) = 𝕃.parent
CC.is_valid_lattice(𝕃::InterTaintLattice, @nospecialize(elm)) =
    is_valid_lattice(widenlattice(𝕃), elem) || isa(elm, InterTaint)

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
                tmerge(widenlattice(𝕃), typea.typ, typeb),
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
