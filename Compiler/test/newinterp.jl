# This file is a part of Julia. License is MIT: https://julialang.org/license

# TODO set up a version who defines new interpreter with persistent cache?

include("setup_Compiler.jl")

"""
    @newinterp NewInterpreter [ephemeral_cache::Bool=false]

Defines new `NewInterpreter <: AbstractInterpreter` whose cache is separated
from the native code cache, satisfying the minimum interface requirements.

When the `ephemeral_cache=true` option is specified, `NewInterpreter` will hold
`CodeInstance` in an ephemeral non-integrated cache, rather than in the integrated
`Compiler.InternalCodeCache`.
Keep in mind that ephemeral cache lacks support for invalidation and doesn't persist across
sessions. However it is an usual Julia object of the type `global_cache::IdDict{MethodInstance,CodeInstance}`,
making it easier for debugging and inspecting the compiler behavior.
"""
macro newinterp(InterpName, ephemeral_cache::Bool=false)
    InterpCacheName = esc(Symbol(string(InterpName, "Cache")))
    InterpName = esc(InterpName)
    C = Core
    quote
        $(ephemeral_cache && quote
        struct $InterpCacheName
            dict::IdDict{$C.MethodInstance,$C.CodeInstance}
        end
        $InterpCacheName() = $InterpCacheName(IdDict{$C.MethodInstance,$C.CodeInstance}())
        end)
        struct $InterpName <: $Compiler.AbstractInterpreter
            meta # additional information
            world::UInt
            inf_params::$Compiler.InferenceParams
            opt_params::$Compiler.OptimizationParams
            inf_cache::Vector{$Compiler.InferenceResult}
            $(ephemeral_cache && :(global_cache::$InterpCacheName))
            function $InterpName(meta = nothing;
                                 world::UInt = Base.get_world_counter(),
                                 inf_params::$Compiler.InferenceParams = $Compiler.InferenceParams(),
                                 opt_params::$Compiler.OptimizationParams = $Compiler.OptimizationParams(),
                                 inf_cache::Vector{$Compiler.InferenceResult} = $Compiler.InferenceResult[],
                                 $(ephemeral_cache ?
                                    Expr(:kw, :(global_cache::$InterpCacheName), :($InterpCacheName())) :
                                    Expr(:kw, :_, :nothing)))
                return $(ephemeral_cache ?
                    :(new(meta, world, inf_params, opt_params, inf_cache, global_cache)) :
                    :(new(meta, world, inf_params, opt_params, inf_cache)))
            end
        end
        $Compiler.InferenceParams(interp::$InterpName) = interp.inf_params
        $Compiler.OptimizationParams(interp::$InterpName) = interp.opt_params
        $Compiler.get_inference_world(interp::$InterpName) = interp.world
        $Compiler.get_inference_cache(interp::$InterpName) = interp.inf_cache
        $Compiler.cache_owner(::$InterpName) = $InterpName
        $(ephemeral_cache && quote
        $Compiler.code_cache(interp::$InterpName) = $Compiler.OverlayCodeCache(interp.global_cache, interp.inf_cache)
        $Compiler.get(cache::$InterpCacheName, mi::$C.MethodInstance, default) = get(cache.dict, mi, default)
        $Compiler.getindex(cache::$InterpCacheName, mi::$C.MethodInstance) = getindex(cache.dict, mi)
        $Compiler.haskey(cache::$InterpCacheName, mi::$C.MethodInstance) = haskey(cache.dict, mi)
        $Compiler.setindex!(cache::$InterpCacheName, ci::$C.CodeInstance, mi::$C.MethodInstance) = setindex!(cache.dict, ci, mi)
        end)
    end
end
