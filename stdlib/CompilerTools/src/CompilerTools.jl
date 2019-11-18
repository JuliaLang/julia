__precompile__(false)
module CompilerTools

using SymEngine
using Core.Compiler
using Core.Compiler: specialize_method, InferenceState, InferenceResult,
    MethodInstance, SimpleVector, resolve_call_cycle!,
    widenconst_bestguess, abstract_call_method, Bottom, unwrap_unionall,
    tmerge, abstract_call_method_with_const_args, Const, PartialStruct,
    IntrinsicFunction, anymap, is_pure_intrinsic_infer, _all, T_IFUNC

struct Dummy <: Compiler.AbstractInterpreter
    cache::Dict{MethodInstance, Any}
end
Dummy() = Dummy(Dict{MethodInstance, Any}())

f(A) = A*A

function test_it()
    ninferred[] = 0
    method = first(methods(f))
    interp = Dummy()
    mi = specialize_method(method, Tuple{typeof(f), Matrix{Float64}}, Core.svec())::MethodInstance
    result = InferenceResult(interp, mi)
    @show result.argtypes
    frame = InferenceState(result, #=cached=#true, Core.Compiler.CustomParams(typemax(UInt64)))
    @show frame.slottypes
    Core.Compiler.typeinf_local(interp, frame)
    display(frame.src)
    @show ninferred[]
end

Core.Compiler.update_valid_age!(interp::Dummy, min_valid::UInt64, max_valid::UInt64, sv::InferenceState) = nothing

function Core.Compiler.typeinf_edge(interp::Dummy, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    @show atypes
    mi = specialize_method(method, atypes, sparams)::MethodInstance
    frame = false
    if haskey(interp.cache, mi)
        return interp.cache[mi]
    end
    if !caller.cached && caller.parent === nothing
        # this caller exists to return to the user
        # (if we asked resolve_call_cyle, it might instead detect that there is a cycle that it can't merge)
        frame = false
    else
        frame = resolve_call_cycle!(interp, mi, caller)
    end
    if frame == false
        # completely new
        mi.inInference = true
        result = InferenceResult(interp, mi)
        frame = InferenceState(result, #=cached=#true, caller.params) # always use the cache for edge targets
        if frame === nothing
            # can't get the source for this, so we know nothing
            mi.inInference = false
            return Any, nothing
        end
        if caller.cached || caller.limited # don't involve uncached functions in cycle resolution
            frame.parent = caller
        end
        Core.Compiler.typeinf(interp, frame)
        return widenconst_bestguess(frame.bestguess), frame.inferred ? mi : nothing
    elseif frame === true
        # unresolvable cycle
        return Any, nothing
    end
    frame.bestguess, nothing
end

const ninferred = Ref{Int}(0)
function Core.Compiler.cache_result(interp::Dummy, result::InferenceResult, min_valid::UInt, max_valid::UInt)
    interp.cache[result.linfo] = (result.result, result.src)
    ninferred[] += 1
    nothing
end

#=
struct Symbolic
    sym
end


struct ArraySize
    size::Tuple
end

function Core.Compiler.widenconst(interp::Dummy, @nospecialize(t))
    if isa(t, ArraySize)
        return Array{Float64, length(t.size)}
    elseif isa(t, Symbolic)
        return Int64
    end
    Core.Compiler.widenconst(t)
end

function Core.Compiler.abstract_call_applicable(interp::Dummy,
    applicable::Vector{Any},
    @nospecialize(f), argtypes::Vector{Any}, sv::InferenceState)

    @show sv.linfo
    @show f
    if f === Base._uniontypes
        return Any, Any[]
    end

    napplicable = length(applicable)
    rettype = Bottom
    edges = Any[]
    nonbot = 0  # the index of the only non-Bottom inference result if > 0
    seen = 0    # number of signatures actually inferred
    istoplevel = sv.linfo.def isa Module
    multiple_matches = napplicable > 1
    for i in 1:napplicable
        match = applicable[i]::SimpleVector
        method = match[3]::Method
        sig = match[1]
        if istoplevel && !isdispatchtuple(sig)
            # only infer concrete call sites in top-level expressions
            rettype = Any
            break
        end
        sigtuple = unwrap_unionall(sig)::DataType
        this_rt = Bottom

        # HACK
        @show sig
        if isa(sig, DataType) && sig.parameters[1] <: Type{<:Array} && length(argtypes) >= 3
            if isa(argtypes[3], Const)
                if isa(argtypes[3].val, Tuple)
                    this_rt = ArraySize(map(Const, argtypes[3].val))
                else
                    this_rt = ArraySize((Const(argtypes[3].val),))
                end
            elseif isa(argtypes[3], PartialStruct)
                this_rt = ArraySize(tuple(argtypes[3].fields...))
            else
                @show argtypes[3]
                this_rt = abstract_call_method_with_const_args(interp, rettype, f, argtypes, match, sv)
            end
        elseif any(x->isa(x, ArraySize) || isa(x, Const) || isa(x, PartialStruct), argtypes)
            @show argtypes
            this_rt = abstract_call_method_with_const_args(interp, rettype, f, argtypes, match, sv)
        else
            this_rt, _, _ = abstract_call_method(interp, method, sig, match[2]::SimpleVector, multiple_matches, sv)
        end
        if this_rt !== Bottom
            if nonbot === 0
                nonbot = i
            else
                nonbot = -1
            end
        end
        seen += 1
        rettype = tmerge(rettype, this_rt)
        rettype === Any && break
    end
    return rettype, edges
end

function Core.Compiler.has_nontrivial_const_info(dummy::Dummy, @nospecialize(a))
    (isa(a, ArraySize) || isa(a, Symbolic)) && return true
    return Core.Compiler.has_nontrivial_const_info(a)
end

function Core.Compiler.const_prop_profitable(dummy::Dummy, @nospecialize(a))
    (isa(a, ArraySize) || isa(a, Symbolic)) && return true
    return Core.Compiler.const_prop_profitable(a)
end

function Core.Compiler.arraysize_tfunc(dummy::Dummy, @nospecialize(a), @nospecialize(d))
    @show (a, d)
    if isa(a, ArraySize)
        if isa(d, Const)
            return a.size[d.val]
        end
    end
    return Int
end

function Core.Compiler.intrinsic_tfunction(interp::Dummy, f::IntrinsicFunction, argtypes::Array{Any, 1})
    if is_pure_intrinsic_infer(f) && _all(@nospecialize(a) -> isa(a, Union{Const, Symbolic}), argtypes)
        argvals = anymap(a -> isa(a, Const) ? a.val : isa(a, Symbolic) ? a.sym : a, argtypes)
        if any(a->isa(a, Symbolic), argtypes)
            if f === Core.Intrinsics.mul_int
                f = *
            elseif f === Core.Intrinsics.sub_int
                f = -
            end
            if !(in(f, (Core.Intrinsics.sle_int, Core.Intrinsics.slt_int)))
                return Symbolic(f(argvals...))
            end
        else
            return Const(f(argvals...))
        end
    end

    iidx = Int(reinterpret(Int32, f)) + 1
    if iidx < 0 || iidx > length(T_IFUNC)
        # invalid intrinsic
        return Any
    end
    @show (f, argtypes)
    tf = T_IFUNC[iidx]
    tf = tf::Tuple{Int, Int, Any}
    if !(tf[1] <= length(argtypes) <= tf[2])
        # wrong # of args
        return Bottom
    end
    return tf[3](interp, argtypes...)
end
=#


end
