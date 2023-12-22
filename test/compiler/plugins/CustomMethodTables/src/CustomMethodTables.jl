# This file is a part of Julia. License is MIT: https://julialang.org/license

module CustomMethodTables

const CC = Core.Compiler

function overlay(mt::CC.MethodTable, f, args...)
    return Base.invoke_within(CMTCompiler(mt), f, args...)
end

const COMPILER_WORLD = Ref{UInt}(0)
function __init__()
    COMPILER_WORLD[] = Base.get_world_counter()
end

struct CMTCompiler <: CC.AbstractCompiler
    mt::CC.MethodTable
end
CC.compiler_world(::CMTCompiler) = COMPILER_WORLD[]
CC.abstract_interpreter(compiler::CMTCompiler, world::UInt) =
    CMTInterp(compiler; world)

method_table(C::CMTCompiler) = C.mt

struct CMTInterp <: CC.AbstractInterpreter
    compiler::CMTCompiler
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::Vector{CC.InferenceResult}
    function CMTInterp(compiler::CMTCompiler;
                world::UInt = Base.get_world_counter(),
                inf_params::CC.InferenceParams = CC.InferenceParams(),
                opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                inf_cache::Vector{CC.InferenceResult} = CC.InferenceResult[])
        return new(compiler, world, inf_params, opt_params, inf_cache)
    end
end

CC.InferenceParams(interp::CMTInterp) = interp.inf_params
CC.OptimizationParams(interp::CMTInterp) = interp.opt_params
CC.get_inference_world(interp::CMTInterp) = interp.world
CC.get_inference_cache(interp::CMTInterp) = interp.inf_cache
CC.cache_owner(interp::CMTInterp) = interp.compiler
CC.method_table(interp::CMTInterp) = Core.Compiler.OverlayMethodTable(CC.get_inference_world(interp), method_table(interp.compiler))

# precompilation
precompile(CC.abstract_interpreter, (CMTCompiler, UInt))
precompile(CC.typeinf_ext_toplevel, (CMTInterp, CC.MethodInstance))

COMPILER_WORLD[] = Base.get_world_counter()
# Insert code execution statements here

# end precompile
COMPILER_WORLD[] = 0


end # module CustomMethodTables
