# Inference-as-a-service (§8.5): the Queries API, provider-side. A consumer
# on core + Queries carries zero `@static if VERSION` branches.

"""
    InferenceConfig(; method_table=nothing, cache_token=nothing,
                    inline_all=false, semi_concrete=false,
                    world=Base.get_world_counter())

Declarative configuration for the queries (§8.5). `method_table` overlays and
`semi_concrete` are accepted for interface completeness; v1 resolves against
the global table.
"""
struct InferenceConfig
    method_table::Any
    cache_token::Any
    inline_all::Bool
    semi_concrete::Bool
    world::UInt
end
InferenceConfig(; method_table = nothing, cache_token = nothing,
                inline_all::Bool = false, semi_concrete::Bool = false,
                world::UInt = Base.get_world_counter()) =
    InferenceConfig(method_table, cache_token, inline_all, semi_concrete, world)

const QUERY_STATES = Dict{Any,UInferState}()

function query_state(config::InferenceConfig)
    key = (config.cache_token, config.world)
    get!(QUERY_STATES, key) do
        UInferState(UInferConfig(; world = config.world))
    end
end

"""
    infer_return(f, argtypes; config=InferenceConfig()) -> lattice element

Const-aware return-type query: `argtypes` may contain `Const(v)` lattice
elements (§8.5).
"""
function infer_return(@nospecialize(f), argtypes::Vector{Any};
                      config::InferenceConfig = InferenceConfig())
    st = query_state(config)
    args = Any[CC.Const(f)]
    append!(args, argtypes)
    fr = Frame(UnifiedIR.Builder().ir, st, Any[])
    # top-level query: no caller subject to translate an InterConditional to
    return widenucond(infer_call(fr, args).rt)
end

"""
    typed_ir(f, argtypes; config=InferenceConfig(), optimize_until=nothing)
        -> UnifiedIR.IR

Const-seeded typed IR for the matching method: entry-convert, infer, and
(unless `optimize_until == "inference"`) run the optimizer pipeline.
"""
function typed_ir(@nospecialize(f), argtypes::Vector{Any};
                  config::InferenceConfig = InferenceConfig(),
                  optimize_until::Union{Nothing,String} = nothing)
    st = query_state(config)
    widened = Any[a isa CC.Const ? typeof(a.val) : a for a in argtypes]
    ir = lowered_ir(f, Tuple{widened...}; world = config.world)
    args = Any[CC.Const(f)]
    append!(args, argtypes)
    if optimize_until == "inference"
        infer_ir!(ir, args; state = st)
        return ir
    end
    return optimize_ir!(ir, args; state = st, inline = config.inline_all ||
                        optimize_until === nothing)
end

"""
    effects_of(f, argtypes; config=InferenceConfig()) -> NamedTuple

Coarse effects summary over the typed IR (v1: the REMOVABLE-mask bits
aggregated across statements; §8.2's vocabulary).
"""
function effects_of(@nospecialize(f), argtypes::Vector{Any};
                    config::InferenceConfig = InferenceConfig())
    ir = typed_ir(f, argtypes; config, optimize_until = "inference")
    refine_effects!(ir)
    consistent = effect_free = nothrow = terminates = true
    for s in UnifiedIR.each_stmt(ir)
        k = UnifiedIR.stmt_kind(ir, s)
        (k === K"region_arg" || UnifiedIR.is_terminator(k)) && continue
        UnifiedIR.owns_regions(k) && continue
        fl = UnifiedIR.stmt_flag(ir, s)
        consistent &= (fl & UnifiedIR.FLAG_CONSISTENT) != 0
        effect_free &= (fl & UnifiedIR.FLAG_EFFECT_FREE) != 0
        nothrow &= (fl & UnifiedIR.FLAG_NOTHROW) != 0
        terminates &= (fl & UnifiedIR.FLAG_TERMINATES) != 0
    end
    # a loop anywhere spoils termination (v1 coarseness)
    for s in UnifiedIR.each_stmt(ir)
        UnifiedIR.stmt_kind(ir, s) === K"loop" && (terminates = false)
        UnifiedIR.stmt_kind(ir, s) === K"cfg" && (terminates = false)
    end
    return (; consistent, effect_free, nothrow, terminates)
end
