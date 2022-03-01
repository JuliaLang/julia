# This file is a part of Julia. License is MIT: https://julialang.org/license

const CC = Core.Compiler
import Core: MethodInstance, CodeInstance
import .CC: WorldRange, WorldView

# define new `AbstractInterpreter` that satisfies the minimum interface requirements
# while managing its cache independently
macro newinterp(name)
    cachename = gensym(string(name, "Cache"))
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

# `OverlayMethodTable`
# --------------------
import Base.Experimental: @MethodTable, @overlay

@newinterp MTOverlayInterp
@MethodTable(OverlayedMT)
CC.method_table(interp::MTOverlayInterp) = CC.OverlayMethodTable(CC.get_world_counter(interp), OverlayedMT)

@overlay OverlayedMT sin(x::Float64) = 1
@test Base.return_types((Int,), MTOverlayInterp()) do x
    sin(x)
end == Any[Int]
