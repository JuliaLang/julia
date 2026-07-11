# Activation through the ordinary compiler-replacement mechanism: a cache
# owner + `Core.OptimizedGenerics.CompilerPlugins.typeinf` overload (the
# Compiler-as-package plugin route; cf. Compiler/extras/CompilerDevTools).
# Nothing bootstrap-side changes: load the package, run
# `with_unified_compiler(f, args...)`.
#
# The UnifiedInterp pipeline additionally routes every inferred method body
# through the UnifiedIR boundary converters as a *verified shadow* (entry
# convert → verify L1 → UnifiedIR passes → coverage stats): the differential
# evidence that the IR round-trips the real pipeline's code, while the
# native-IR path continues to feed codegen. (Direct UnifiedIR codegen input
# is the §12 P4 step.)

mutable struct UnifiedCacheOwner end

struct UnifiedInterp <: Compiler.AbstractInterpreter
    world::UInt
    owner::Union{Nothing,UnifiedCacheOwner}   # nothing = native cache (global mode)
    inf_params::Compiler.InferenceParams
    opt_params::Compiler.OptimizationParams
    inf_cache::Compiler.InferenceCache
    codegen_cache::IdDict{Core.CodeInstance,Core.CodeInfo}
    function UnifiedInterp(;
            world::UInt = Base.get_world_counter(),
            owner::Union{Nothing,UnifiedCacheOwner} = GLOBAL_OWNER,
            inf_params::Compiler.InferenceParams = Compiler.InferenceParams(),
            opt_params::Compiler.OptimizationParams = Compiler.OptimizationParams())
        new(world, owner, inf_params, opt_params, Compiler.InferenceCache(),
            IdDict{Core.CodeInstance,Core.CodeInfo}())
    end
end

const GLOBAL_OWNER = UnifiedCacheOwner()

Compiler.InferenceParams(interp::UnifiedInterp) = interp.inf_params
Compiler.OptimizationParams(interp::UnifiedInterp) = interp.opt_params
Compiler.get_inference_world(interp::UnifiedInterp) = interp.world
Compiler.get_inference_cache(interp::UnifiedInterp) = interp.inf_cache
Compiler.cache_owner(interp::UnifiedInterp) = interp.owner
Compiler.codegen_cache(interp::UnifiedInterp) = interp.codegen_cache

# shadow-coverage statistics: how much of the live pipeline's code the
# UnifiedIR converters+passes handled
mutable struct ShadowStats
    seen::Int
    converted::Int
    verified::Int
    outside_matrix::Int
    errors::Int
    last_error::Any
end
const SHADOW = ShadowStats(0, 0, 0, 0, 0, nothing)

const SHADOW_ACTIVE = Ref(false)   # reentrancy guard: the shadow's own code
                                   # compiles through the global hook

function shadow_roundtrip!(src::Core.CodeInfo, nargs::Int, name::Symbol)
    SHADOW_ENABLED[] || return nothing
    SHADOW_ACTIVE[] && return nothing
    SHADOW_ACTIVE[] = true
    try
        return _shadow_roundtrip!(src, nargs, name)
    finally
        SHADOW_ACTIVE[] = false
    end
end

function _shadow_roundtrip!(src::Core.CodeInfo, nargs::Int, name::Symbol)
    SHADOW.seen += 1
    ir = try
        codeinfo_to_ir(src; nargs, name)
    catch e
        if e isa UnsupportedIR
            SHADOW.outside_matrix += 1
        else
            SHADOW.errors += 1
            SHADOW.last_error = e
        end
        return nothing
    end
    SHADOW.converted += 1
    try
        UnifiedIR.verify_ir(ir; level = 1)
        UnifiedIR.promote_cells!(ir)
        UnifiedIR.dce!(ir)
        ir, _ = UnifiedIR.compact!(ir)
        UnifiedIR.verify_ir(ir; level = 1)
        SHADOW.verified += 1
    catch e
        SHADOW.errors += 1
        SHADOW.last_error = e
    end
    return nothing
end

import Core.OptimizedGenerics.CompilerPlugins: typeinf, typeinf_edge

# Pin to the module-load sentinel world (defined at the very end of this
# file): late enough that every method of this module — including the
# transform_result_for_cache shadow hook — is visible, while still excluding
# later user worlds (the DevTools call-rewriting device). Using this method's
# own primary_world only works when a pkgimage flattens the module onto one
# world; under an interpreted include the later-defined hooks would be
# invisible.
@noinline typeinf(owner::UnifiedCacheOwner, mi::Core.MethodInstance, source_mode::UInt8) =
    Base.invoke_in_world(_activation_world(),
                         Compiler.typeinf_ext_toplevel,
                         UnifiedInterp(; world = Base.tls_world_age()), mi, source_mode)

@noinline function typeinf_edge(owner::UnifiedCacheOwner, mi::Core.MethodInstance,
                                parent_frame::Compiler.InferenceState, world::UInt,
                                source_mode::UInt8)
    interp = UnifiedInterp(; world)
    Compiler.typeinf_edge(interp, mi.def, mi.specTypes, Core.svec(), parent_frame, false, false)
end

function lookup_method_instance(f, args...)
    @ccall jl_method_lookup(Any[f, args...]::Ptr{Any}, (1 + length(args))::Csize_t,
                            Base.tls_world_age()::Csize_t)::Ref{Core.MethodInstance}
end

# Keep the whole reachable call graph inside the unified compiler's world
# (the DevTools call-rewriting device) and run the UnifiedIR shadow on every
# cached result.
const GLOBAL_MODE = Ref(false)

function Compiler.transform_result_for_cache(interp::UnifiedInterp,
                                             result::Compiler.InferenceResult,
                                             edges::Core.SimpleVector)
    mi = result.linfo
    m = mi.def
    if m isa Method
        src = try
            Base.uncompressed_ir(m)
        catch
            nothing
        end
        src isa Core.CodeInfo && shadow_roundtrip!(src, Int(m.nargs), m.name)
    end
    # the call-graph rewriting keeps plugin-scoped compilation in this world;
    # under global activation everything already routes here — skip it
    GLOBAL_MODE[] && return @invoke Compiler.transform_result_for_cache(
        interp::Compiler.AbstractInterpreter, result::Compiler.InferenceResult,
        edges::Core.SimpleVector)
    opt = result.src
    if opt isa Compiler.OptimizationState
        ir = opt.optresult.ir::Compiler.IRCode
        override = with_unified_compiler
        for inst in ir.stmts
            stmt = inst[:stmt]
            Base.isexpr(stmt, :call) || continue
            f = stmt.args[1]
            f === override && continue
            T = Compiler.widenconst(Compiler.argextype(f, ir))
            T <: Core.Builtin && continue
            insert!(stmt.args, 1, override)
            insert!(stmt.args, 3, interp.owner)
        end
    end
    @invoke Compiler.transform_result_for_cache(interp::Compiler.AbstractInterpreter,
                                                result::Compiler.InferenceResult,
                                                edges::Core.SimpleVector)
end

"""
    with_unified_compiler(f, args...) -> result

Run `f(args...)` compiled by the unified compiler stack, activated through
the ordinary `CompilerPlugins.typeinf` plugin mechanism (no bootstrap
change). The whole reachable call graph is compiled in this world; every
inferred body is round-tripped through the UnifiedIR converters (see
[`shadow_stats`](@ref)).
"""
with_unified_compiler(f, args...; owner::UnifiedCacheOwner = GLOBAL_OWNER) =
    with_unified_compiler(f, owner, args...)

function with_unified_compiler(f, owner::UnifiedCacheOwner, args...)
    isa(f, Core.Builtin) && return f(args...)
    mi = lookup_method_instance(f, args...)
    ci = Core.OptimizedGenerics.CompilerPlugins.typeinf(owner, mi, Compiler.SOURCE_MODE_ABI)
    return invoke(f, ci, args...)
end

"Shadow-coverage counters of the plugin pipeline (UnifiedIR round-trip)."
shadow_stats() = (; seen = SHADOW.seen, converted = SHADOW.converted,
                  verified = SHADOW.verified, outside_matrix = SHADOW.outside_matrix,
                  errors = SHADOW.errors, last_error = SHADOW.last_error)

# global-activation typeinf entry: the signature the runtime invokes through
# jl_typeinf_func (mi, world, source_mode, trim_mode)
function unified_typeinf_ext_toplevel(mi::Core.MethodInstance, world::UInt,
                                      source_mode::UInt8, trim_mode::UInt8)
    # native cache owner: precompiled sysimage results stay hot; only fresh
    # inference routes through the unified stack (what hot-loading the
    # Compiler stdlib does)
    interp = UnifiedInterp(; world, owner = nothing)
    return Compiler.typeinf_ext_toplevel(interp, mi, source_mode)
end

"""
    activate!(; shadow=true)

Route ALL runtime type inference through the unified compiler stack — the
global form of the ordinary compiler-replacement mechanism
(`jl_set_typeinf_func`, exactly what hot-loading the Compiler stdlib does).
Every body inferred from now on round-trips the UnifiedIR converters when
`shadow` is enabled. Restore with [`deactivate!`](@ref).
"""
function activate!(; shadow::Bool = true)
    # warm up: compile the whole hook + shadow path under the STOCK compiler
    # first, so nothing the hook executes needs compiling through the hook
    SHADOW_ENABLED[] = shadow
    let mi = lookup_method_instance(+, 1, 2)
        unified_typeinf_ext_toplevel(mi, Base.get_world_counter(), Compiler.SOURCE_MODE_ABI, 0x00)
        wf = (x) -> begin s = 0; for i in 1:x; s += try; i > 2 ? i : error(); catch; 0; end; end; s end
        ci = Base.code_lowered(wf, Tuple{Int})[1]
        shadow_roundtrip!(ci, 2, :warmup)
    end
    GLOBAL_MODE[] = true
    ccall(:jl_set_typeinf_func, Cvoid, (Any,), unified_typeinf_ext_toplevel)
    return nothing
end

"Restore the stock compiler as the runtime's inference entry."
function deactivate!()
    GLOBAL_MODE[] = false
    ccall(:jl_set_typeinf_func, Cvoid, (Any,), Core.Compiler.typeinf_ext_toplevel)
    return nothing
end

const SHADOW_ENABLED = Ref(true)

# world sentinel: the last method this module defines (activate.jl is the
# last include) — see the invoke_in_world note above
_activation_world_sentinel() = nothing
_activation_world() = which(_activation_world_sentinel, Tuple{}).primary_world
