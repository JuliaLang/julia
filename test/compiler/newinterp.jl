# This file is a part of Julia. License is MIT: https://julialang.org/license

# TODO set up a version who defines new interpreter with persistent cache?

"""
    @newinterp NewInterpreter

Defines new `NewInterpreter <: AbstractInterpreter` whose cache is separated
from the native code cache, satisfying the minimum interface requirements.
"""
macro newinterp(InterpName)
    cache_token = QuoteNode(gensym(string(InterpName, "Cache")))
    InterpName = esc(InterpName)
    C = Core
    CC = Core.Compiler
    quote
        struct $InterpName <: $CC.AbstractInterpreter
            meta # additional information
            world::UInt
            inf_params::$CC.InferenceParams
            opt_params::$CC.OptimizationParams
            inf_cache::Vector{$CC.InferenceResult}
            function $InterpName(meta = nothing;
                                 world::UInt = Base.get_world_counter(),
                                 inf_params::$CC.InferenceParams = $CC.InferenceParams(),
                                 opt_params::$CC.OptimizationParams = $CC.OptimizationParams(),
                                 inf_cache::Vector{$CC.InferenceResult} = $CC.InferenceResult[])
                return new(meta, world, inf_params, opt_params, inf_cache)
            end
        end
        $CC.InferenceParams(interp::$InterpName) = interp.inf_params
        $CC.OptimizationParams(interp::$InterpName) = interp.opt_params
        $CC.get_inference_world(interp::$InterpName) = interp.world
        $CC.get_inference_cache(interp::$InterpName) = interp.inf_cache
        $CC.cache_owner(::$InterpName) = $cache_token
    end
end
