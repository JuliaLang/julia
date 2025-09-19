# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

include("setup_Compiler.jl")
include("irutils.jl")
include("newinterp.jl")

# interpreter that performs abstract interpretation only
# (semi-concrete interpretation should be disabled automatically)
@newinterp AbsIntOnlyInterp1
Compiler.may_optimize(::AbsIntOnlyInterp1) = false
@test Base.infer_return_type(Base.init_stdio, (Ptr{Cvoid},); interp=AbsIntOnlyInterp1()) >: IO

# it should work even if the interpreter discards inferred source entirely
@newinterp AbsIntOnlyInterp2
Compiler.may_optimize(::AbsIntOnlyInterp2) = false
Compiler.transform_result_for_cache(::AbsIntOnlyInterp2, ::Compiler.InferenceResult, edges::Core.SimpleVector) = nothing
@test Base.infer_return_type(Base.init_stdio, (Ptr{Cvoid},); interp=AbsIntOnlyInterp2()) >: IO

# OverlayMethodTable
# ==================

using Base.Experimental: @MethodTable, @overlay, @consistent_overlay

# @overlay method with return type annotation
@MethodTable RT_METHOD_DEF
@overlay RT_METHOD_DEF Base.sin(x::Float64)::Float64 = cos(x)
@overlay RT_METHOD_DEF function Base.sin(x::T)::T where T<:AbstractFloat
    cos(x)
end

@newinterp MTOverlayInterp
@MethodTable OVERLAY_MT
Compiler.method_table(interp::MTOverlayInterp) = Compiler.OverlayMethodTable(Compiler.get_inference_world(interp), OVERLAY_MT)

function Compiler.add_remark!(interp::MTOverlayInterp, ::Compiler.InferenceState, remark)
    if interp.meta !== nothing
        # Core.println(remark)
        push!(interp.meta, remark)
    end
    return nothing
end

struct StrangeSinError end
strangesin(x) = sin(x)
@overlay OVERLAY_MT strangesin(x::Float64) =
    iszero(x) ? throw(StrangeSinError()) : x < 0 ? nothing : cos(x)

# inference should use the overlayed method table
@test Base.return_types((Float64,); interp=MTOverlayInterp()) do x
    strangesin(x)
end |> only === Union{Float64,Nothing}
@test Base.return_types((Any,); interp=MTOverlayInterp()) do x
    @invoke strangesin(x::Float64)
end |> only === Union{Float64,Nothing}
@test only(Base.return_types(strangesin, (Float64,); interp=MTOverlayInterp())) === Union{Float64,Nothing}
@test Base.infer_exception_type(strangesin, (Float64,); interp=MTOverlayInterp()) === Union{StrangeSinError,DomainError}
@test only(Base.infer_exception_types(strangesin, (Float64,); interp=MTOverlayInterp())) === Union{StrangeSinError,DomainError}
@test last(only(code_typed(strangesin, (Float64,); interp=MTOverlayInterp()))) === Union{Float64,Nothing}
@test last(only(Base.code_ircode(strangesin, (Float64,); interp=MTOverlayInterp()))) === Union{Float64,Nothing}

# effect analysis should figure out that the overlayed method is used
@test Base.infer_effects((Float64,); interp=MTOverlayInterp()) do x
    strangesin(x)
end |> !Compiler.is_nonoverlayed
@test Base.infer_effects((Any,); interp=MTOverlayInterp()) do x
    @invoke strangesin(x::Float64)
end |> !Compiler.is_nonoverlayed

# account for overlay possibility in unanalyzed matching method
callstrange(::Float64) = strangesin(x)
callstrange(::Number) = Core.compilerbarrier(:type, nothing) # trigger inference bail out
callstrange(::Any) = 1.0
callstrange_entry(x) = callstrange(x) # needs to be defined here because of world age
let interp = MTOverlayInterp(Set{Any}())
    matches = Compiler.findall(Tuple{typeof(callstrange),Any}, Compiler.method_table(interp))
    @test matches !== nothing
    @test Compiler.length(matches) == 3
    @test Base.infer_effects(callstrange_entry, (Any,); interp) |> !Compiler.is_nonoverlayed
    @test "Call inference reached maximally imprecise information: bailing on doing more abstract inference." in interp.meta
end

# but it should never apply for the native compilation
@test Base.infer_effects((Float64,)) do x
    strangesin(x)
end |> Compiler.is_nonoverlayed
@test Base.infer_effects((Any,)) do x
    @invoke strangesin(x::Float64)
end |> Compiler.is_nonoverlayed

# fallback to the internal method table
@test Base.return_types((Int,); interp=MTOverlayInterp()) do x
    cos(x)
end |> only === Float64
@test Base.return_types((Any,); interp=MTOverlayInterp()) do x
    @invoke cos(x::Float64)
end |> only === Float64

# not fully covered overlay method match
overlay_match(::Any) = nothing
@overlay OVERLAY_MT overlay_match(::Int) = missing
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

# override `:native_executable` to allow concrete-eval for overlay-ed methods
function myfactorial(x::Int, raise)
    res = 1
    0 ≤ x < 20 || raise("x is too big")
    Base.@assume_effects :terminates_locally while x > 1
        res *= x
        x -= 1
    end
    return res
end
raise_on_gpu1(x) = error(x)
@overlay OVERLAY_MT @noinline raise_on_gpu1(x) = #=do something with GPU=# error(x)
raise_on_gpu2(x) = error(x)
@consistent_overlay OVERLAY_MT @noinline raise_on_gpu2(x) = #=do something with GPU=# error(x)
raise_on_gpu3(x) = error(x)
@consistent_overlay OVERLAY_MT @noinline Base.@assume_effects :foldable raise_on_gpu3(x) = #=do something with GPU=# error_on_gpu(x)
cpu_factorial(x::Int) = myfactorial(x, error)
gpu_factorial1(x::Int) = myfactorial(x, raise_on_gpu1)
gpu_factorial2(x::Int) = myfactorial(x, raise_on_gpu2)
gpu_factorial3(x::Int) = myfactorial(x, raise_on_gpu3)

@test Base.infer_effects(cpu_factorial, (Int,); interp=MTOverlayInterp()) |> Compiler.is_nonoverlayed
@test Base.infer_effects(gpu_factorial1, (Int,); interp=MTOverlayInterp()) |> !Compiler.is_nonoverlayed
@test Base.infer_effects(gpu_factorial2, (Int,); interp=MTOverlayInterp()) |> Compiler.is_consistent_overlay
let effects = Base.infer_effects(gpu_factorial3, (Int,); interp=MTOverlayInterp())
    # check if `@consistent_overlay` together works with `@assume_effects`
    # N.B. the overlaid `raise_on_gpu3` is not :foldable otherwise since `error_on_gpu` is (intetionally) undefined.
    @test Compiler.is_consistent_overlay(effects)
    @test Compiler.is_foldable(effects)
end
@test Base.infer_return_type(; interp=MTOverlayInterp()) do
    Val(gpu_factorial2(3))
end == Val{6}
@test Base.infer_return_type(; interp=MTOverlayInterp()) do
    Val(gpu_factorial3(3))
end == Val{6}

# GPUCompiler needs accurate inference through kwfunc with the overlay of `Core.throw_inexacterror`
# https://github.com/JuliaLang/julia/issues/48097
@newinterp Issue48097Interp
@MethodTable ISSUE_48097_MT
Compiler.method_table(interp::Issue48097Interp) = Compiler.OverlayMethodTable(Compiler.get_inference_world(interp), ISSUE_48097_MT)
function Compiler.concrete_eval_eligible(interp::Issue48097Interp,
    @nospecialize(f), result::Compiler.MethodCallResult, arginfo::Compiler.ArgInfo, sv::Compiler.AbsIntState)
    ret = @invoke Compiler.concrete_eval_eligible(interp::Compiler.AbstractInterpreter,
        f::Any, result::Compiler.MethodCallResult, arginfo::Compiler.ArgInfo, sv::Compiler.AbsIntState)
    if ret === :semi_concrete_eval
        # disable semi-concrete interpretation
        return :none
    end
    return ret
end
@overlay ISSUE_48097_MT @noinline Core.throw_inexacterror(f::Symbol, ::Type{T}, val) where {T} = return
issue48097(; kwargs...) = return 42
@test fully_eliminated(; interp=Issue48097Interp(), retval=42) do
    issue48097(; a=1f0, b=1.0)
end

# https://github.com/JuliaLang/julia/issues/52938
@newinterp Issue52938Interp
@MethodTable ISSUE_52938_MT
Compiler.method_table(interp::Issue52938Interp) = Compiler.OverlayMethodTable(Compiler.get_inference_world(interp), ISSUE_52938_MT)
inner52938(x, types::Type, args...; kwargs...) = x
outer52938(x) = @inline inner52938(x, Tuple{}; foo=Ref(42), bar=1)
@test fully_eliminated(outer52938, (Any,); interp=Issue52938Interp(), retval=Argument(2))

# https://github.com/JuliaGPU/CUDA.jl/issues/2241
@newinterp Cuda2241Interp
@MethodTable CUDA_2241_MT
Compiler.method_table(interp::Cuda2241Interp) = Compiler.OverlayMethodTable(Compiler.get_inference_world(interp), CUDA_2241_MT)
inner2241(f, types::Type, args...; kwargs...) = nothing
function outer2241(f)
    @inline inner2241(f, Tuple{}; foo=Ref(42), bar=1)
    return nothing
end
# NOTE CUDA.jl overlays `throw_boundserror` in a way that causes effects, but these effects
#      are ignored for this call graph at the `@assume_effects` annotation on `typejoin`.
#      Here it's important to use `@consistent_overlay` to avoid tainting the `:nonoverlayed` bit.
const cuda_kernel_state = Ref{Any}()
@consistent_overlay CUDA_2241_MT @inline Base.throw_boundserror(A, I) =
    (cuda_kernel_state[] = (A, I); error())
@test fully_eliminated(outer2241, (Nothing,); interp=Cuda2241Interp(), retval=nothing)

# Should not concrete-eval overlayed methods in semi-concrete interpretation
@newinterp OverlaySinInterp
@MethodTable OVERLAY_SIN_MT
Compiler.method_table(interp::OverlaySinInterp) = Compiler.OverlayMethodTable(Compiler.get_inference_world(interp), OVERLAY_SIN_MT)
overlay_sin1(x) = error("Not supposed to be called.")
@overlay OVERLAY_SIN_MT overlay_sin1(x) = cos(x)
@overlay OVERLAY_SIN_MT Base.sin(x::Union{Float32,Float64}) = overlay_sin1(x)
let ir = Base.code_ircode(; interp=OverlaySinInterp()) do
        sin(0.)
    end |> only |> first
    ir.argtypes[1] = Tuple{}
    oc = Core.OpaqueClosure(ir)
    @test oc() == cos(0.)
end
@overlay OVERLAY_SIN_MT Base.sin(x::Union{Float32,Float64}) = @noinline overlay_sin1(x)
let ir = Base.code_ircode(; interp=OverlaySinInterp()) do
        sin(0.)
    end |> only |> first
    ir.argtypes[1] = Tuple{}
    oc = Core.OpaqueClosure(ir)
    @test oc() == cos(0.)
end
_overlay_sin2(x) = error("Not supposed to be called.")
@overlay OVERLAY_SIN_MT _overlay_sin2(x) = cos(x)
overlay_sin2(x) = _overlay_sin2(x)
@overlay OVERLAY_SIN_MT Base.sin(x::Union{Float32,Float64}) = @noinline overlay_sin2(x)
let ir = Base.code_ircode(; interp=OverlaySinInterp()) do
        sin(0.)
    end |> only |> first
    ir.argtypes[1] = Tuple{}
    oc = Core.OpaqueClosure(ir)
    @test oc() == cos(0.)
end

# AbstractLattice
# ===============

using Core: SlotNumber, Argument
using .Compiler: slot_id, tmerge_fast_path
import .Compiler:
    AbstractLattice, BaseInferenceLattice, IPOResultLattice, InferenceLattice,
    widenlattice, is_valid_lattice_norec, typeinf_lattice, ipo_lattice, optimizer_lattice,
    widenconst, tmeet, tmerge, ⊑, abstract_eval_special_value, widenreturn

@newinterp TaintInterpreter
struct TaintLattice{PL<:AbstractLattice} <: Compiler.AbstractLattice
    parent::PL
end
Compiler.widenlattice(𝕃::TaintLattice) = 𝕃.parent
Compiler.is_valid_lattice_norec(::TaintLattice, @nospecialize(elm)) = isa(elm, Taint)

struct InterTaintLattice{PL<:AbstractLattice} <: Compiler.AbstractLattice
    parent::PL
end
Compiler.widenlattice(𝕃::InterTaintLattice) = 𝕃.parent
Compiler.is_valid_lattice_norec(::InterTaintLattice, @nospecialize(elm)) = isa(elm, InterTaint)

const AnyTaintLattice{L} = Union{TaintLattice{L},InterTaintLattice{L}}

Compiler.typeinf_lattice(::TaintInterpreter) = InferenceLattice(TaintLattice(BaseInferenceLattice.instance))
Compiler.ipo_lattice(::TaintInterpreter) = InferenceLattice(InterTaintLattice(IPOResultLattice.instance))
Compiler.optimizer_lattice(::TaintInterpreter) = InterTaintLattice(SimpleInferenceLattice.instance)

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

function Compiler.tmeet(𝕃::AnyTaintLattice, @nospecialize(v), @nospecialize(t::Type))
    T = isa(𝕃, TaintLattice) ? Taint : InterTaint
    if isa(v, T)
        v = v.typ
    end
    return tmeet(widenlattice(𝕃), v, t)
end
function Compiler.tmerge(𝕃::AnyTaintLattice, @nospecialize(typea), @nospecialize(typeb))
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
function Compiler.:⊑(𝕃::AnyTaintLattice, @nospecialize(typea), @nospecialize(typeb))
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
Compiler.widenconst(taint::AnyTaint) = widenconst(taint.typ)

function Compiler.abstract_eval_special_value(interp::TaintInterpreter,
    @nospecialize(e), sstate::Compiler.StatementState, sv::Compiler.InferenceState)
    ret = @invoke Compiler.abstract_eval_special_value(interp::Compiler.AbstractInterpreter,
        e::Any, sstate::Compiler.StatementState, sv::Compiler.InferenceState)
    if isa(e, SlotNumber) || isa(e, Argument)
        return Taint(ret, slot_id(e))
    end
    return ret
end

function Compiler.widenreturn(𝕃::InferenceLattice{<:InterTaintLattice}, @nospecialize(rt), @nospecialize(bestguess), nargs::Int, slottypes::Vector{Any}, changes::Compiler.VarTable)
    if isa(rt, Taint)
        return InterTaint(rt.typ, BitSet((id for id in rt.slots if id ≤ nargs)))
    end
    return Compiler.widenreturn(widenlattice(𝕃), rt, bestguess, nargs, slottypes, changes)
end

@test Compiler.tmerge(typeinf_lattice(TaintInterpreter()), Taint(Int, 1), Taint(Int, 2)) == Taint(Int, BitSet(1:2))

# code_typed(ifelse, (Bool, Int, Int); interp=TaintInterpreter())

# External lattice without `Conditional`

import .Compiler:
    AbstractLattice, ConstsLattice, PartialsLattice, InferenceLattice,
    typeinf_lattice, ipo_lattice, optimizer_lattice

@newinterp NonconditionalInterpreter
Compiler.typeinf_lattice(::NonconditionalInterpreter) = InferenceLattice(PartialsLattice(ConstsLattice()))
Compiler.ipo_lattice(::NonconditionalInterpreter) = InferenceLattice(PartialsLattice(ConstsLattice()))
Compiler.optimizer_lattice(::NonconditionalInterpreter) = PartialsLattice(ConstsLattice())

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

import .Compiler: CallInfo

struct NoinlineCallInfo <: CallInfo
    info::CallInfo # wrapped call
end
Compiler.add_edges_impl(edges::Vector{Any}, info::NoinlineCallInfo) = Compiler.add_edges!(edges, info.info)
Compiler.nsplit_impl(info::NoinlineCallInfo) = Compiler.nsplit(info.info)
Compiler.getsplit_impl(info::NoinlineCallInfo, idx::Int) = Compiler.getsplit(info.info, idx)
Compiler.getresult_impl(info::NoinlineCallInfo, idx::Int) = Compiler.getresult(info.info, idx)

function Compiler.abstract_call(interp::NoinlineInterpreter,
    arginfo::Compiler.ArgInfo, si::Compiler.StmtInfo, sv::Compiler.InferenceState, max_methods::Int)
    ret = @invoke Compiler.abstract_call(interp::Compiler.AbstractInterpreter,
        arginfo::Compiler.ArgInfo, si::Compiler.StmtInfo, sv::Compiler.InferenceState, max_methods::Int)
    return Compiler.Future{Compiler.CallMeta}(ret, interp, sv) do ret, interp, sv
        if sv.mod in noinline_modules(interp)
            (;rt, exct, effects, info) = ret
            return Compiler.CallMeta(rt, exct, effects, NoinlineCallInfo(info))
        end
        return ret
    end
end
function Compiler.src_inlining_policy(interp::NoinlineInterpreter,
    @nospecialize(src), @nospecialize(info::CallInfo), stmt_flag::UInt32)
    if isa(info, NoinlineCallInfo)
        return false
    end
    return @invoke Compiler.src_inlining_policy(interp::Compiler.AbstractInterpreter,
        src::Any, info::CallInfo, stmt_flag::UInt32)
end

@inline function inlined_usually(x, y, z)
    return x * y + z
end
foo_split(x::Float64) = 1
foo_split(x::Int) = 2

# check if the inlining algorithm works as expected
let src = code_typed1((Float64,Float64,Float64)) do x, y, z
        inlined_usually(x, y, z)
    end
    @test count(isinvoke(:inlined_usually), src.code) == 0
    @test count(iscall((src, inlined_usually)), src.code) == 0
end
let NoinlineModule = Module()
    OtherModule = Module()
    main_func(x, y, z) = inlined_usually(x, y, z)
    @eval NoinlineModule noinline_func(x, y, z) = $inlined_usually(x, y, z)
    @eval OtherModule other_func(x, y, z) = $inlined_usually(x, y, z)
    @eval NoinlineModule bar_split_error() = $foo_split(Core.compilerbarrier(:type, nothing))

    interp = NoinlineInterpreter(Set((NoinlineModule,)))

    # this anonymous function's context is Main -- it should be inlined as usual
    let src = code_typed1(main_func, (Float64,Float64,Float64); interp)
        @test count(isinvoke(:inlined_usually), src.code) == 0
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end

    # it should work for cached results
    method = only(methods(inlined_usually, (Float64,Float64,Float64,)))
    mi = Compiler.specialize_method(method, Tuple{typeof(inlined_usually),Float64,Float64,Float64}, Core.svec())
    @test Compiler.haskey(Compiler.code_cache(interp), mi)
    let src = code_typed1(main_func, (Float64,Float64,Float64); interp)
        @test count(isinvoke(:inlined_usually), src.code) == 0
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end

    # now the context module is `NoinlineModule` -- it should not be inlined
    let src = code_typed1(NoinlineModule.noinline_func, (Float64,Float64,Float64); interp)
        @test count(isinvoke(:inlined_usually), src.code) == 1
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end

    # the context module is totally irrelevant -- it should be inlined as usual
    let src = code_typed1(OtherModule.other_func, (Float64,Float64,Float64); interp)
        @test count(isinvoke(:inlined_usually), src.code) == 0
        @test count(iscall((src, inlined_usually)), src.code) == 0
    end

    let src = code_typed1(NoinlineModule.bar_split_error)
        @test count(iscall((src, foo_split)), src.code) == 0
        @test count(iscall((src, Core.throw_methoderror)), src.code) > 0
    end
end

# custom inferred data
# ====================

@newinterp CustomDataInterp
struct CustomDataInterpToken end
Compiler.cache_owner(::CustomDataInterp) = CustomDataInterpToken()
struct CustomData
    inferred
    CustomData(@nospecialize inferred) = new(inferred)
end
function Compiler.transform_result_for_cache(interp::CustomDataInterp, result::Compiler.InferenceResult, edges::Core.SimpleVector)
    inferred_result = @invoke Compiler.transform_result_for_cache(
        interp::Compiler.AbstractInterpreter, result::Compiler.InferenceResult, edges::Core.SimpleVector)
    return CustomData(inferred_result)
end
function Compiler.src_inlining_policy(interp::CustomDataInterp, @nospecialize(src),
                            @nospecialize(info::Compiler.CallInfo), stmt_flag::UInt32)
    if src isa CustomData
        src = src.inferred
    end
    return @invoke Compiler.src_inlining_policy(interp::Compiler.AbstractInterpreter, src::Any,
                                          info::Compiler.CallInfo, stmt_flag::UInt32)
end
Compiler.retrieve_ir_for_inlining(cached_result::CodeInstance, src::CustomData) =
    Compiler.retrieve_ir_for_inlining(cached_result, src.inferred)
Compiler.retrieve_ir_for_inlining(mi::MethodInstance, src::CustomData, preserve_local_sources::Bool) =
    Compiler.retrieve_ir_for_inlining(mi, src.inferred, preserve_local_sources)
let src = code_typed((Int,); interp=CustomDataInterp()) do x
        return sin(x) + cos(x)
    end |> only |> first
    @test count(isinvoke(:sin), src.code) == 1
    @test count(isinvoke(:cos), src.code) == 1
    @test count(isinvoke(:+), src.code) == 0
end

# ephemeral cache mode
@newinterp DebugInterp #=ephemeral_cache=#true
func_ext_cache1(a) = func_ext_cache2(a) * cos(a)
func_ext_cache2(a) = sin(a)
let interp = DebugInterp()
    @test Base.infer_return_type(func_ext_cache1, (Float64,); interp) === Float64
    @test isdefined(interp, :global_cache)
    found = false
    for (mi, codeinst) in interp.global_cache.dict
        if mi.def.name === :func_ext_cache2
            found = true
            break
        end
    end
    @test found
end

@newinterp InvokeInterp
struct InvokeOwner end
codegen = IdDict{CodeInstance, CodeInfo}()
Compiler.cache_owner(::InvokeInterp) = InvokeOwner()
Compiler.codegen_cache(::InvokeInterp) = codegen
let interp = InvokeInterp()
    source_mode = Compiler.SOURCE_MODE_ABI
    f = (+)
    args = (1, 1)
    mi = @ccall jl_method_lookup(Any[f, args...]::Ptr{Any}, (1+length(args))::Csize_t, Base.tls_world_age()::Csize_t)::Ref{Core.MethodInstance}
    ci = Compiler.typeinf_ext_toplevel(interp, mi, source_mode)
    @test invoke(f, ci, args...) == 2

    f = error
    args = "test"
    mi = @ccall jl_method_lookup(Any[f, args...]::Ptr{Any}, (1+length(args))::Csize_t, Base.tls_world_age()::Csize_t)::Ref{Core.MethodInstance}
    ci = Compiler.typeinf_ext_toplevel(interp, mi, source_mode)
    result = nothing
    try
        invoke(f, ci, args...)
    catch e
        result = sprint(Base.show_backtrace, catch_backtrace())
    end
    @test isa(result, String)
    @test contains(result, "[1] error(::Char, ::Char, ::Char, ::Char)")
end
