module CompilerDevTools

using Compiler
using Core.IR

struct SplitCacheOwner; end
struct SplitCacheInterp <: Compiler.AbstractInterpreter
    world::UInt
    inf_params::Compiler.InferenceParams
    opt_params::Compiler.OptimizationParams
    inf_cache::Vector{Compiler.InferenceResult}
    codegen_cache::IdDict{CodeInstance,CodeInfo}
    function SplitCacheInterp(;
        world::UInt = Base.get_world_counter(),
        inf_params::Compiler.InferenceParams = Compiler.InferenceParams(),
        opt_params::Compiler.OptimizationParams = Compiler.OptimizationParams(),
        inf_cache::Vector{Compiler.InferenceResult} = Compiler.InferenceResult[])
        new(world, inf_params, opt_params, inf_cache, IdDict{CodeInstance,CodeInfo}())
    end
end

Compiler.InferenceParams(interp::SplitCacheInterp) = interp.inf_params
Compiler.OptimizationParams(interp::SplitCacheInterp) = interp.opt_params
Compiler.get_inference_world(interp::SplitCacheInterp) = interp.world
Compiler.get_inference_cache(interp::SplitCacheInterp) = interp.inf_cache
Compiler.cache_owner(::SplitCacheInterp) = SplitCacheOwner()
Compiler.codegen_cache(interp::SplitCacheInterp) = interp.codegen_cache

import Core.OptimizedGenerics.CompilerPlugins: typeinf, typeinf_edge
@eval @noinline typeinf(::SplitCacheOwner, mi::MethodInstance, source_mode::UInt8) =
    Base.invoke_in_world(which(typeinf, Tuple{SplitCacheOwner, MethodInstance, UInt8}).primary_world, Compiler.typeinf_ext_toplevel, SplitCacheInterp(; world=Base.tls_world_age()), mi, source_mode)

@eval @noinline function typeinf_edge(::SplitCacheOwner, mi::MethodInstance, parent_frame::Compiler.InferenceState, world::UInt, source_mode::UInt8)
    # TODO: This isn't quite right, we're just sketching things for now
    interp = SplitCacheInterp(; world)
    Compiler.typeinf_edge(interp, mi.def, mi.specTypes, Core.svec(), parent_frame, false, false)
end

function with_new_compiler(f, args...)
    mi = @ccall jl_method_lookup(Any[f, args...]::Ptr{Any}, (1+length(args))::Csize_t, Base.tls_world_age()::Csize_t)::Ref{Core.MethodInstance}
    world = Base.tls_world_age()
    new_compiler_ci = Core.OptimizedGenerics.CompilerPlugins.typeinf(
        SplitCacheOwner(), mi, Compiler.SOURCE_MODE_ABI
    )
    invoke(f, new_compiler_ci, args...)
end

export with_new_compiler

end
