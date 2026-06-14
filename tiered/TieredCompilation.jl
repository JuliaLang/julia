# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    TieredCompilation

Prototype Julia-side API for the tiered-compilation experiment. It provides:

  * [`tier_stats`](@ref) — inspect the hotness counter and compiled entry-point kind
    of a method's native `CodeInstance`.
  * [`promote_tier!`](@ref) — manually compile a higher-tier ("optimized")
    `CodeInstance` under a separate compiler `owner` and atomically install it onto
    the live native `CodeInstance` (next-call replacement).

The per-`CodeInstance` call counter that `tier_stats` reads is only emitted into
generated code when Julia is started with `JULIA_TIER=1`. Set `JULIA_TIER_LOG=1`
to log tier transitions to stderr.

This is an experimental prototype; see `tiered/demo.jl` for a runnable example.
"""
module TieredCompilation

const CC = Base.Compiler

export tier_stats, promote_tier!

# ---------------------------------------------------------------------------
# Tier-2 ("optimized") compiler stage.
#
# A "stage" in this prototype is just an AbstractInterpreter with its own cache
# `owner` token, so the CodeInstance it produces is distinct from the native
# (owner === nothing) one and can coexist in the cache. We provide a codegen cache
# so the result is actually added to the JIT (see Compiler.add_codeinsts_to_jit!),
# which is what makes the produced CodeInstance executable.
# ---------------------------------------------------------------------------
struct OptInterp <: CC.AbstractInterpreter
    world::UInt
    inf_params::CC.InferenceParams
    opt_params::CC.OptimizationParams
    inf_cache::CC.InferenceCache
    codegen::IdDict{Core.CodeInstance,Core.CodeInfo}
end
function OptInterp(; world::UInt = Base.get_world_counter(),
                     inf_params::CC.InferenceParams = CC.InferenceParams(),
                     opt_params::CC.OptimizationParams = CC.OptimizationParams(),
                     inf_cache::CC.InferenceCache = CC.InferenceCache())
    return OptInterp(world, inf_params, opt_params, inf_cache,
                     IdDict{Core.CodeInstance,Core.CodeInfo}())
end

CC.InferenceParams(interp::OptInterp) = interp.inf_params
CC.OptimizationParams(interp::OptInterp) = interp.opt_params
CC.get_inference_world(interp::OptInterp) = interp.world
CC.get_inference_cache(interp::OptInterp) = interp.inf_cache
CC.cache_owner(::OptInterp) = OptInterp           # distinct, stable owner token
CC.codegen_cache(interp::OptInterp) = interp.codegen

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Resolve the MethodInstance for a `f(::types...)` call.
function _method_instance(@nospecialize(f), @nospecialize(types))
    mi = Base.method_instance(f, types)
    mi === nothing && error("no matching method instance for $(Base.signature_type(f, types))")
    return mi::Core.MethodInstance
end

# The native (owner === nothing) CodeInstance for `mi` in the current world. This
# is the same object whose generated code carries the prologue call counter.
function _native_ci(mi::Core.MethodInstance)
    world = Base.get_world_counter()
    return CC.typeinf_ext_toplevel(mi, world, CC.SOURCE_MODE_ABI, CC.TRIM_NO)::Core.CodeInstance
end

# Decode the invoke ABI (see jl_invoke_api) into a readable label.
function _invoke_kind(ci::Core.CodeInstance)
    api = ccall(:jl_invoke_api, Int32, (Any,), ci)
    return api == -1 ? :specsig :
           api ==  0 ? :uncompiled :
           api ==  1 ? :jlcall :
           api ==  2 ? :constreturn :
           api ==  3 ? :sparam :
           api ==  4 ? :interpret : :unknown
end

# Keep promoted CodeInstances (and their JIT-compiled code) alive: after the swap,
# the native CI's entry points reference the optimized CI's compiled code.
const _RETAINED = IdDict{Core.CodeInstance,Core.CodeInstance}()

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

"""
    tier_stats(f, types) -> NamedTuple

Return profiling information about the native `CodeInstance` compiled for the call
`f(::types...)`: its compiler `owner`, the current hotness `callcount` (recorded
entries; nonzero only under `JULIA_TIER=1`), and the kind of compiled entry point
(`invoke`).
"""
function tier_stats(@nospecialize(f), @nospecialize(types))
    ci = _native_ci(_method_instance(f, types))
    return (; owner = ci.owner,
              callcount = ccall(:jl_tier_callcount, UInt64, (Any,), ci),
              invoke = _invoke_kind(ci))
end

"""
    promote_tier!(f, types) -> CodeInstance

Compile a higher-tier ("optimized") `CodeInstance` for `f(::types...)` under a
separate compiler `owner`, then atomically install its entry points onto the live
native `CodeInstance` so subsequent calls run the new code (next-call replacement).
Returns the newly produced optimized `CodeInstance`.
"""
function promote_tier!(@nospecialize(f), @nospecialize(types))
    mi = _method_instance(f, types)
    native_ci = _native_ci(mi)
    interp = OptInterp(; world = Base.get_world_counter())
    opt_ci = CC.typeinf_ext_toplevel(interp, mi, CC.SOURCE_MODE_ABI)::Core.CodeInstance
    ccall(:jl_tier_swap_target, Cvoid, (Any, Any), native_ci, opt_ci)
    _RETAINED[native_ci] = opt_ci
    return opt_ci
end

end # module TieredCompilation
