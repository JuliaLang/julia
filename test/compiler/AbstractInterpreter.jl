# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
const CC = Core.Compiler

include("irutils.jl")
include("newinterp.jl")

# interpreter that performs abstract interpretation only
# (semi-concrete interpretation should be disabled automatically)
@newinterp AbsIntOnlyInterp1
CC.may_optimize(::AbsIntOnlyInterp1) = false
@test Base.infer_return_type(Base.init_stdio, (Ptr{Cvoid},); interp=AbsIntOnlyInterp1()) >: IO

# it should work even if the interpreter discards inferred source entirely
@newinterp AbsIntOnlyInterp2
CC.may_optimize(::AbsIntOnlyInterp2) = false
CC.transform_result_for_cache(::AbsIntOnlyInterp2, ::Core.MethodInstance, ::CC.WorldRange, ::CC.InferenceResult) = nothing
@test Base.infer_return_type(Base.init_stdio, (Ptr{Cvoid},); interp=AbsIntOnlyInterp2()) >: IO

# OverlayMethodTable
# ==================

using Base.Experimental: @MethodTable, @overlay

# @overlay method with return type annotation
@MethodTable RT_METHOD_DEF
@overlay RT_METHOD_DEF Base.sin(x::Float64)::Float64 = cos(x)
@overlay RT_METHOD_DEF function Base.sin(x::T)::T where T<:AbstractFloat
    cos(x)
end

@newinterp MTOverlayInterp
@MethodTable OverlayedMT
CC.method_table(interp::MTOverlayInterp) = CC.OverlayMethodTable(CC.get_inference_world(interp), OverlayedMT)

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
callstrange(::Float64) = strangesin(x)
callstrange(::Nothing) = Core.compilerbarrier(:type, nothing) # trigger inference bail out
callstrange_entry(x) = callstrange(x) # needs to be defined here because of world age
let interp = MTOverlayInterp(Set{Any}())
    matches = Core.Compiler.findall(Tuple{typeof(callstrange),Any}, Core.Compiler.method_table(interp))
    @test matches !== nothing
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
Base.@assume_effects :terminates_locally function issue41694(x)
    res = 1
    0 ≤ x < 20 || error("bad fact")
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

# GPUCompiler needs accurate inference through kwfunc with the overlay of `Core.throw_inexacterror`
# https://github.com/JuliaLang/julia/issues/48097
@newinterp Issue48097Interp
@MethodTable Issue48097MT
CC.method_table(interp::Issue48097Interp) = CC.OverlayMethodTable(CC.get_inference_world(interp), Issue48097MT)
CC.InferenceParams(::Issue48097Interp) = CC.InferenceParams(; unoptimize_throw_blocks=false)
function CC.concrete_eval_eligible(interp::Issue48097Interp,
    @nospecialize(f), result::CC.MethodCallResult, arginfo::CC.ArgInfo, sv::CC.AbsIntState)
    ret = @invoke CC.concrete_eval_eligible(interp::CC.AbstractInterpreter,
        f::Any, result::CC.MethodCallResult, arginfo::CC.ArgInfo, sv::CC.AbsIntState)
    if ret === :semi_concrete_eval
        # disable semi-concrete interpretation
        return :none
    end
    return ret
end
@overlay Issue48097MT @noinline Core.throw_inexacterror(f::Symbol, ::Type{T}, val) where {T} = return
issue48097(; kwargs...) = return 42
@test_broken fully_eliminated(; interp=Issue48097Interp(), retval=42) do
    issue48097(; a=1f0, b=1.0)
end

# Should not concrete-eval overlayed methods in semi-concrete interpretation
@newinterp OverlaySinInterp
@MethodTable OverlaySinMT
CC.method_table(interp::OverlaySinInterp) = CC.OverlayMethodTable(CC.get_inference_world(interp), OverlaySinMT)
overlay_sin1(x) = error("Not supposed to be called.")
@overlay OverlaySinMT overlay_sin1(x) = cos(x)
@overlay OverlaySinMT Base.sin(x::Union{Float32,Float64}) = overlay_sin1(x)
let oc = Base.code_ircode(; interp=OverlaySinInterp()) do
        sin(0.)
    end |> only |> first |> Core.OpaqueClosure
    @test oc() == cos(0.)
end
@overlay OverlaySinMT Base.sin(x::Union{Float32,Float64}) = @noinline overlay_sin1(x)
let oc = Base.code_ircode(; interp=OverlaySinInterp()) do
        sin(0.)
    end |> only |> first |> Core.OpaqueClosure
    @test oc() == cos(0.)
end
_overlay_sin2(x) = error("Not supposed to be called.")
@overlay OverlaySinMT _overlay_sin2(x) = cos(x)
overlay_sin2(x) = _overlay_sin2(x)
@overlay OverlaySinMT Base.sin(x::Union{Float32,Float64}) = @noinline overlay_sin2(x)
let oc = Base.code_ircode(; interp=OverlaySinInterp()) do
        sin(0.)
    end |> only |> first |> Core.OpaqueClosure
    @test oc() == cos(0.)
end

# AbstractLattice
# ===============

using Core: SlotNumber, Argument
using Core.Compiler: slot_id, tmerge_fast_path
import .CC:
    AbstractLattice, BaseInferenceLattice, IPOResultLattice, InferenceLattice,
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
CC.optimizer_lattice(::TaintInterpreter) = InterTaintLattice(SimpleInferenceLattice.instance)

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
    AbstractLattice, ConstsLattice, PartialsLattice, InferenceLattice,
    typeinf_lattice, ipo_lattice, optimizer_lattice

@newinterp NonconditionalInterpreter
CC.typeinf_lattice(::NonconditionalInterpreter) = InferenceLattice(PartialsLattice(ConstsLattice()))
CC.ipo_lattice(::NonconditionalInterpreter) = InferenceLattice(PartialsLattice(ConstsLattice()))
CC.optimizer_lattice(::NonconditionalInterpreter) = PartialsLattice(ConstsLattice())

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

@newinterp NoinlineInterpreter
noinline_modules(interp::NoinlineInterpreter) = interp.meta::Set{Module}

import .CC: CallInfo

struct NoinlineCallInfo <: CallInfo
    info::CallInfo # wrapped call
end
CC.nsplit_impl(info::NoinlineCallInfo) = CC.nsplit(info.info)
CC.getsplit_impl(info::NoinlineCallInfo, idx::Int) = CC.getsplit(info.info, idx)
CC.getresult_impl(info::NoinlineCallInfo, idx::Int) = CC.getresult(info.info, idx)

function CC.abstract_call(interp::NoinlineInterpreter,
    arginfo::CC.ArgInfo, si::CC.StmtInfo, sv::CC.InferenceState, max_methods::Int)
    ret = @invoke CC.abstract_call(interp::CC.AbstractInterpreter,
        arginfo::CC.ArgInfo, si::CC.StmtInfo, sv::CC.InferenceState, max_methods::Int)
    if sv.mod in noinline_modules(interp)
        return CC.CallMeta(ret.rt, ret.exct, ret.effects, NoinlineCallInfo(ret.info))
    end
    return ret
end
function CC.src_inlining_policy(interp::NoinlineInterpreter,
    @nospecialize(src), @nospecialize(info::CallInfo), stmt_flag::UInt32)
    if isa(info, NoinlineCallInfo)
        return false
    end
    return @invoke CC.src_inlining_policy(interp::CC.AbstractInterpreter,
        src::Any, info::CallInfo, stmt_flag::UInt32)
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
    @test CC.haskey(CC.code_cache(interp), mi)
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

# Make sure that Core.Compiler has enough NamedTuple infrastructure
# to properly give error messages for basic kwargs...
Core.eval(Core.Compiler, quote f(;a=1) = a end)
@test_throws MethodError Core.Compiler.f(;b=2)

# Custom lookup function
# ======================

# In the following test with `ConstInvokeInterp`, we use a custom lookup function that
# uses const-prop'ed source if available, and check if LLVM emits code using it.

using Core: MethodInstance, CodeInstance
using Base: CodegenParams
using InteractiveUtils

@newinterp ConstInvokeInterp
function CC.concrete_eval_eligible(interp::ConstInvokeInterp,
    @nospecialize(f), result::CC.MethodCallResult, arginfo::CC.ArgInfo, sv::CC.AbsIntState)
    ret = @invoke CC.concrete_eval_eligible(interp::CC.AbstractInterpreter,
        f::Any, result::CC.MethodCallResult, arginfo::CC.ArgInfo, sv::CC.AbsIntState)
    if ret === :semi_concrete_eval
        return :none # disable semi-concrete interpretation
    end
    return ret
end
Base.@constprop :aggressive @noinline function custom_lookup_target(c::Bool, x::Int)
    if c
        y = sin(x)
        z = nothing
    else
        y = cos(x)
        z = missing
    end
    return y, z
end
custom_lookup_context(x::Int) = custom_lookup_target(true, x)

const CONST_INVOKE_INTERP_WORLD = Base.get_world_counter()
const CONST_INVOKE_INTERP = ConstInvokeInterp(; world=CONST_INVOKE_INTERP_WORLD)
function custom_lookup(mi::MethodInstance, min_world::UInt, max_world::UInt)
    for inf_result in CONST_INVOKE_INTERP.inf_cache
        if inf_result.linfo === mi
            if CC.any(inf_result.overridden_by_const)
                return CodeInstance(CONST_INVOKE_INTERP, inf_result)
            end
        end
    end
    # XXX: This seems buggy, custom_lookup should probably construct the absint on demand.
    return CC.getindex(CC.code_cache(CONST_INVOKE_INTERP), mi)
end

let # generate cache
    code_typed(custom_lookup_context; world=CONST_INVOKE_INTERP_WORLD, interp=CONST_INVOKE_INTERP)

    # check if the lookup function works as expected
    target_mi = CC.specialize_method(only(methods(custom_lookup_target)), Tuple{typeof(custom_lookup_target),Bool,Int}, Core.svec())
    target_ci = custom_lookup(target_mi, CONST_INVOKE_INTERP_WORLD, CONST_INVOKE_INTERP_WORLD)
    @test target_ci.rettype == Tuple{Float64,Nothing} # constprop'ed source

    raw = false
    lookup = @cfunction(custom_lookup, Any, (Any,Csize_t,Csize_t))
    params = CodegenParams(;
        debug_info_kind=Cint(0),
        debug_info_level=Cint(2),
        safepoint_on_entry=raw,
        gcstack_arg=raw,
        lookup)
    io = IOBuffer()
    code_llvm(io, custom_lookup_target, (Bool,Int,); params)
    s = String(take!(io))
    @test  occursin("j_sin_", s)
    @test !occursin("j_cos_", s)
end

# custom inferred data
# ====================

@newinterp CustomDataInterp
struct CustomDataInterpToken end
CC.cache_owner(::CustomDataInterp) = CustomDataInterpToken()
struct CustomData
    inferred
    CustomData(@nospecialize inferred) = new(inferred)
end
function CC.transform_result_for_cache(interp::CustomDataInterp,
    mi::Core.MethodInstance, valid_worlds::CC.WorldRange, result::CC.InferenceResult)
    inferred_result = @invoke CC.transform_result_for_cache(interp::CC.AbstractInterpreter,
        mi::Core.MethodInstance, valid_worlds::CC.WorldRange, result::CC.InferenceResult)
    return CustomData(inferred_result)
end
function CC.src_inlining_policy(interp::CustomDataInterp, @nospecialize(src),
                            @nospecialize(info::CC.CallInfo), stmt_flag::UInt32)
    if src isa CustomData
        src = src.inferred
    end
    return @invoke CC.src_inlining_policy(interp::CC.AbstractInterpreter, src::Any,
                                          info::CC.CallInfo, stmt_flag::UInt32)
end
CC.retrieve_ir_for_inlining(cached_result::CodeInstance, src::CustomData) =
    CC.retrieve_ir_for_inlining(cached_result, src.inferred)
CC.retrieve_ir_for_inlining(mi::MethodInstance, src::CustomData, preserve_local_sources::Bool) =
    CC.retrieve_ir_for_inlining(mi, src.inferred, preserve_local_sources)
let src = code_typed((Int,); interp=CustomDataInterp()) do x
        return sin(x) + cos(x)
    end |> only |> first
    @test count(isinvoke(:sin), src.code) == 1
    @test count(isinvoke(:cos), src.code) == 1
    @test count(isinvoke(:+), src.code) == 0
end
