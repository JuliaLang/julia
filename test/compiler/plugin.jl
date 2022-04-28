# %%
const CC = Core.Compiler
using Test, .CC.Plugin
import Core: MethodInstance, CodeInstance
import .CC: WorldRange, WorldView

# %%
# SinCosRewriter
# --------------

struct SinCosRewriterCache
    dict::IdDict{MethodInstance,CodeInstance}
end
struct SinCosRewriter <: CC.AbstractInterpreter
    interp::CC.NativeInterpreter
    cache::SinCosRewriterCache
end
let global_cache = SinCosRewriterCache(IdDict{MethodInstance,CodeInstance}())
    global function SinCosRewriter(
        world = Base.get_world_counter();
        interp = CC.NativeInterpreter(world),
        cache = global_cache)
        return SinCosRewriter(interp, cache)
    end
end
CC.InferenceParams(interp::SinCosRewriter) = CC.InferenceParams(interp.interp)
CC.OptimizationParams(interp::SinCosRewriter) = CC.OptimizationParams(interp.interp)
CC.get_world_counter(interp::SinCosRewriter) = CC.get_world_counter(interp.interp)
CC.get_inference_cache(interp::SinCosRewriter) = CC.get_inference_cache(interp.interp)
CC.code_cache(interp::SinCosRewriter) = WorldView(interp.cache, WorldRange(CC.get_world_counter(interp)))
CC.get(wvc::WorldView{<:SinCosRewriterCache}, mi::MethodInstance, default) = get(wvc.cache.dict, mi, default)
CC.getindex(wvc::WorldView{<:SinCosRewriterCache}, mi::MethodInstance) = getindex(wvc.cache.dict, mi)
CC.haskey(wvc::WorldView{<:SinCosRewriterCache}, mi::MethodInstance) = haskey(wvc.cache.dict, mi)
function CC.setindex!(wvc::WorldView{<:SinCosRewriterCache}, ci::CodeInstance, mi::MethodInstance)
    # ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, ci)
    setindex!(wvc.cache.dict, ci, mi)
end

function CC.abstract_call_gf_by_type(
    interp::SinCosRewriter, @nospecialize(f),
    arginfo::CC.ArgInfo, @nospecialize(atype),
    sv::CC.InferenceState, max_methods::Int)
    if f === sin
        f = cos
        atype′ = CC.unwrap_unionall(atype)::DataType
        atype′ = Tuple{typeof(cos), atype′.parameters[2:end]...}
        atype = CC.rewrap_unionall(atype′, atype)
    end
    return Base.@invoke CC.abstract_call_gf_by_type(
        interp::CC.AbstractInterpreter, f,
        arginfo::CC.ArgInfo, atype,
        sv::CC.InferenceState, max_methods::Int)
end

global gv::Any = 42
macro test_native_execution!()
    return :(let
        Base.Experimental.@force_compile
        global gv
        v1 = 42
        v2 = gv::Int
        v3 = gv
        @test isone(sin(v1)^2 + cos(v1)^2)
        @test isone(@noinline sin(v2)^2 + cos(v2)^2)
        @test isone(sin(v3)^2 + cos(v3)^2)
    end)
end

# simple
@test execute_with_plugin(; interp=SinCosRewriter()) do
    sin(42)
end == cos(42)
@test execute_with_plugin(42; interp=SinCosRewriter()) do a
    sin(a)
end == cos(42)
@test execute_with_plugin(sin, 42; interp=SinCosRewriter()) do f, a
    f(a)
end == cos(42)
@test_native_execution!

# global
nested(a) = sin(a) # => cos(a)
@test execute_with_plugin(42; interp=SinCosRewriter()) do a
    nested(a)
end == cos(42)
@test_native_execution!

# dynamic
global gv::Any = 42
@test execute_with_plugin() do
    sin(gv::Any) # dynamic dispatch
end == cos(42)
@test execute_with_plugin(sin) do f
    f(gv)
end == cos(42)
global gf::Any = sin
@test execute_with_plugin() do
    (gf::Any)(gv::Any)
end == cos(42)
@test_native_execution!

# static dispatch
@noinline noninlined_sin() = sin(gv::Int)
@test_broken execute_with_plugin(; interp=SinCosRewriter()) do
    noninlined_sin()
end == cos(42)
@test_native_execution!

# invoke
@test_broken execute_with_plugin(42.0; interp=SinCosRewriter()) do a
    Base.@invoke sin(a::Float64)
end == cos(42.0)
@test_native_execution!

# end to end
function kernel(fs)
    r = 0
    for i = 1:length(fs)
        r += sum(fs[i](i))
    end
    return r
end
let
    fs = (sin, sin, cos)
    gs = (cos, cos, cos)
    @test execute_with_plugin(kernel, fs) == kernel(gs)

    fs = Any[sin, sin, cos]
    gs = Any[cos, cos, cos]
    @test execute_with_plugin(kernel, fs) == kernel(gs)
end
@test_native_execution!
