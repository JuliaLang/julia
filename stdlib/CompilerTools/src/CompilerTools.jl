__precompile__(false)
module CompilerTools

using Core.Compiler
using Core.Compiler: specialize_method, InferenceState, InferenceResult,
    MethodInstance, SimpleVector, resolve_call_cycle!,
    widenconst_bestguess

struct Dummy <: Compiler.AbstractInterpreter
    cache::Dict{MethodInstance, Any}
end
Dummy() = Dummy(Dict{MethodInstance, Any}())

struct ArraySize
    size::Tuple
end

f(A) = A*A

function test_it()
    ninferred[] = 0
    method = first(methods(f))
    mi = specialize_method(method, Tuple{Matrix{Float64}}, Core.svec())::MethodInstance
    frame = InferenceState(InferenceResult(mi), #=cached=#true, Core.Compiler.CustomParams(typemax(UInt64)))

    Core.Compiler.typeinf_local(Dummy(), frame)
    display(frame.src)
    @show ninferred[]
end

Core.Compiler.update_valid_age!(interp::Dummy, min_valid::UInt64, max_valid::UInt64, sv::InferenceState) = nothing

function Core.Compiler.typeinf_edge(interp::Dummy, method::Method, @nospecialize(atypes), sparams::SimpleVector, caller::InferenceState)
    @show caller.linfo
    @show method
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
        result = InferenceResult(mi)
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
    @show (result.linfo, result.result)
    interp.cache[result.linfo] = (result.result, result.src)
    ninferred[] += 1
    nothing
end

end
