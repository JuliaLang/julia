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

strangesin(x) = sin(x)
@overlay OverlayedMT strangesin(x::Float64) = iszero(x) ? nothing : cos(x)

# inference should use the overlayed method table
@test Base.return_types((Float64,); interp=MTOverlayInterp()) do x
    strangesin(x)
end |> only === Union{Float64,Nothing}
@test Base.return_types((Any,); interp=MTOverlayInterp()) do x
    Base.@invoke strangesin(x::Float64)
end |> only === Union{Float64,Nothing}

# effect analysis should figure out that the overlayed method is used
@test Base.infer_effects((Float64,); interp=MTOverlayInterp()) do x
    strangesin(x)
end |> !Core.Compiler.is_nonoverlayed
@test Base.infer_effects((Any,); interp=MTOverlayInterp()) do x
    Base.@invoke strangesin(x::Float64)
end |> !Core.Compiler.is_nonoverlayed

# but it should never apply for the native compilation
@test Base.infer_effects((Float64,)) do x
    strangesin(x)
end |> Core.Compiler.is_nonoverlayed
@test Base.infer_effects((Any,)) do x
    Base.@invoke strangesin(x::Float64)
end |> Core.Compiler.is_nonoverlayed

# fallback to the internal method table
@test Base.return_types((Int,); interp=MTOverlayInterp()) do x
    cos(x)
end |> only === Float64
@test Base.return_types((Any,); interp=MTOverlayInterp()) do x
    Base.@invoke cos(x::Float64)
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
