module CompilerDevTools

using Compiler
using Compiler: argextype, widenconst
using Core.IR
using Base: isexpr

mutable struct SplitCacheOwner end

struct SplitCacheInterp <: Compiler.AbstractInterpreter
    world::UInt
    owner::SplitCacheOwner
    inf_params::Compiler.InferenceParams
    opt_params::Compiler.OptimizationParams
    inf_cache::Vector{Compiler.InferenceResult}
    codegen_cache::IdDict{CodeInstance,CodeInfo}
    function SplitCacheInterp(;
        world::UInt = Base.get_world_counter(),
        owner::SplitCacheOwner = SplitCacheOwner(),
        inf_params::Compiler.InferenceParams = Compiler.InferenceParams(),
        opt_params::Compiler.OptimizationParams = Compiler.OptimizationParams(),
        inf_cache::Vector{Compiler.InferenceResult} = Compiler.InferenceResult[])
        new(world, owner, inf_params, opt_params, inf_cache, IdDict{CodeInstance,CodeInfo}())
    end
end

Compiler.InferenceParams(interp::SplitCacheInterp) = interp.inf_params
Compiler.OptimizationParams(interp::SplitCacheInterp) = interp.opt_params
Compiler.get_inference_world(interp::SplitCacheInterp) = interp.world
Compiler.get_inference_cache(interp::SplitCacheInterp) = interp.inf_cache
Compiler.cache_owner(interp::SplitCacheInterp) = interp.owner
Compiler.codegen_cache(interp::SplitCacheInterp) = interp.codegen_cache

import Core.OptimizedGenerics.CompilerPlugins: typeinf, typeinf_edge
@eval @noinline typeinf(owner::SplitCacheOwner, mi::MethodInstance, source_mode::UInt8) =
    Base.invoke_in_world(which(typeinf, Tuple{SplitCacheOwner, MethodInstance, UInt8}).primary_world, Compiler.typeinf_ext_toplevel, SplitCacheInterp(; world=Base.tls_world_age(), owner), mi, source_mode)

@eval @noinline function typeinf_edge(owner::SplitCacheOwner, mi::MethodInstance, parent_frame::Compiler.InferenceState, world::UInt, source_mode::UInt8)
    # TODO: This isn't quite right, we're just sketching things for now
    interp = SplitCacheInterp(; world, owner)
    Compiler.typeinf_edge(interp, mi.def, mi.specTypes, Core.svec(), parent_frame, false, false)
end

function lookup_method_instance(f, args...)
    @ccall jl_method_lookup(Any[f, args...]::Ptr{Any}, (1+length(args))::Csize_t, Base.tls_world_age()::Csize_t)::Ref{Core.MethodInstance}
end

function Compiler.optimize(interp::SplitCacheInterp, opt::Compiler.OptimizationState, caller::Compiler.InferenceResult)
    @invoke Compiler.optimize(interp::Compiler.AbstractInterpreter, opt::Compiler.OptimizationState, caller::Compiler.InferenceResult)
    ir = opt.result.ir::Compiler.IRCode
    override = GlobalRef(@__MODULE__(), :with_new_compiler)
    for inst in ir.stmts
        stmt = inst[:stmt]
        isexpr(stmt, :call) || continue
        f = stmt.args[1]
        f === override && continue
        if isa(f, GlobalRef)
            T = widenconst(argextype(f, ir))
            T <: Core.Builtin && continue
        end
        insert!(stmt.args, 1, override)
        insert!(stmt.args, 3, interp.owner)
    end
end

with_new_compiler(f, args...; owner::SplitCacheOwner = SplitCacheOwner()) = with_new_compiler(f, owner, args...)

function with_new_compiler(f, owner::SplitCacheOwner, args...)
    mi = lookup_method_instance(f, args...)
    new_compiler_ci = Core.OptimizedGenerics.CompilerPlugins.typeinf(
        owner, mi, Compiler.SOURCE_MODE_ABI
    )
    invoke(f, new_compiler_ci, args...)
end

export with_new_compiler

end
