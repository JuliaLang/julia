# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    AbstractInterpreter

An abstract base class that allows multiple dispatch to determine the method of
executing Julia code.  The native Julia LLVM pipeline is enabled by using the
`NativeInterpreter` concrete instantiation of this abstract class, others can be
swapped in as long as they follow the `AbstractInterpreter` API.

If `interp` is an `AbstractInterpreter`, it is expected to provide at least the following methods:
- `InferenceParams(interp)` - return an `InferenceParams` instance
- `OptimizationParams(interp)` - return an `OptimizationParams` instance
- `get_world_counter(interp)` - return the world age for this interpreter
- `get_inference_cache(interp)` - return the runtime inference cache
- `code_cache(interp)` - return the global inference cache
"""
abstract type AbstractInterpreter end

struct ArgInfo
    fargs::Union{Nothing,Vector{Any}}
    argtypes::Vector{Any}
end

struct TriState; state::UInt8; end
const ALWAYS_FALSE     = TriState(0x00)
const ALWAYS_TRUE      = TriState(0x01)
const TRISTATE_UNKNOWN = TriState(0x02)

function tristate_merge(old::TriState, new::TriState)
    (old === ALWAYS_FALSE || new === ALWAYS_FALSE) && return ALWAYS_FALSE
    old === TRISTATE_UNKNOWN && return old
    return new
end

"""
    effects::Effects

Represents computational effects of a method call.

The effects are composed of the following set of different properties:
- `effects.consistent::TriState`: this method is guaranteed to return or terminate consistently
- `effect_free::TriState`: this method is free from externally semantically visible side effects
- `nothrow::TriState`: this method is guaranteed to not throw an exception
- `terminates::TriState`: this method is guaranteed to terminate
- `nonoverlayed::Bool`: indicates that any methods that may be called within this method
  are not defined in an [overlayed method table](@ref OverlayMethodTable)
- `notaskstate::TriState`: this method does not access any state bound to the current
  task and may thus be moved to a different task without changing observable
  behavior. Note that this currently implies that `noyield` as well, since
  yielding modifies the state of the current task, though this may be split
  in the future.
See [`Base.@assume_effects`](@ref) for more detailed explanation on the definitions of these properties.

Along the abstract interpretation, `Effects` at each statement are analyzed locally and
they are merged into the single global `Effects` that represents the entire effects of
the analyzed method (see `tristate_merge!`).
Each effect property is represented as tri-state and managed separately.
The tri-state consists of `ALWAYS_TRUE`, `TRISTATE_UNKNOWN` and `ALWAYS_FALSE`, where they
have the following meanings:
- `ALWAYS_TRUE`: this method is guaranteed to not have this effect.
- `ALWAYS_FALSE`: this method may have this effect, and there is no need to do any further
  analysis w.r.t. this effect property as this conclusion will not be refined anyway.
- `TRISTATE_UNKNOWN`: this effect property may still be refined to `ALWAYS_TRUE` or
  `ALWAYS_FALSE`, e.g. using return type information.

An effect property is initialized with `ALWAYS_TRUE` and then transitioned towards
`ALWAYS_FALSE`. When we find a statement that has some effect, either of `TRISTATE_UNKNOWN`
or `ALWAYS_FALSE` is propagated. Note that however, within the current flow-insensitive
analysis design, it is usually difficult to derive a global conclusion accurately from local
analysis on each statement, and therefore, the effect analysis usually propagates the
`ALWAYS_FALSE` state conservatively.
"""
struct Effects
    consistent::TriState
    effect_free::TriState
    nothrow::TriState
    terminates::TriState
    nonoverlayed::Bool
    notaskstate::TriState
    # This effect is currently only tracked in inference and modified
    # :consistent before caching. We may want to track it in the future.
    inbounds_taints_consistency::Bool
end
function Effects(
    consistent::TriState,
    effect_free::TriState,
    nothrow::TriState,
    terminates::TriState,
    nonoverlayed::Bool,
    notaskstate::TriState)
    return Effects(
        consistent,
        effect_free,
        nothrow,
        terminates,
        nonoverlayed,
        notaskstate,
        false)
end

const EFFECTS_TOTAL    = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  ALWAYS_TRUE,  ALWAYS_TRUE,  true,  ALWAYS_TRUE)
const EFFECTS_THROWS   = Effects(ALWAYS_TRUE,  ALWAYS_TRUE,  ALWAYS_FALSE, ALWAYS_TRUE,  true,  ALWAYS_TRUE)
const EFFECTS_UNKNOWN  = Effects(ALWAYS_FALSE, ALWAYS_FALSE, ALWAYS_FALSE, ALWAYS_FALSE, true,  ALWAYS_FALSE)  # mostly unknown, but it's not overlayed at least (e.g. it's not a call)
const EFFECTS_UNKNOWN′ = Effects(ALWAYS_FALSE, ALWAYS_FALSE, ALWAYS_FALSE, ALWAYS_FALSE, false, ALWAYS_FALSE) # unknown, really

function Effects(e::Effects = EFFECTS_UNKNOWN′;
    consistent::TriState = e.consistent,
    effect_free::TriState = e.effect_free,
    nothrow::TriState = e.nothrow,
    terminates::TriState = e.terminates,
    nonoverlayed::Bool = e.nonoverlayed,
    notaskstate::TriState = e.notaskstate,
    inbounds_taints_consistency::Bool = e.inbounds_taints_consistency)
    return Effects(
        consistent,
        effect_free,
        nothrow,
        terminates,
        nonoverlayed,
        notaskstate,
        inbounds_taints_consistency)
end

is_consistent(effects::Effects)   = effects.consistent === ALWAYS_TRUE
is_effect_free(effects::Effects)  = effects.effect_free === ALWAYS_TRUE
is_nothrow(effects::Effects)      = effects.nothrow === ALWAYS_TRUE
is_terminates(effects::Effects)   = effects.terminates === ALWAYS_TRUE
is_notaskstate(effects::Effects)  = effects.notaskstate === ALWAYS_TRUE
is_nonoverlayed(effects::Effects) = effects.nonoverlayed

# implies :notaskstate, but not explicitly checked here
is_foldable(effects::Effects) =
    is_consistent(effects) &&
    is_effect_free(effects) &&
    is_terminates(effects)

is_total(effects::Effects) =
    is_foldable(effects) &&
    is_nothrow(effects)

is_removable_if_unused(effects::Effects) =
    is_effect_free(effects) &&
    is_terminates(effects) &&
    is_nothrow(effects)

function encode_effects(e::Effects)
    return (e.consistent.state << 0) |
           (e.effect_free.state << 2) |
           (e.nothrow.state << 4) |
           (e.terminates.state << 6) |
           (UInt32(e.nonoverlayed) << 8) |
           (UInt32(e.notaskstate.state) << 9)
end
function decode_effects(e::UInt32)
    return Effects(
        TriState((e >> 0) & 0x03),
        TriState((e >> 2) & 0x03),
        TriState((e >> 4) & 0x03),
        TriState((e >> 6) & 0x03),
        _Bool(   (e >> 8) & 0x01),
        TriState((e >> 9) & 0x03),
        false)
end

function tristate_merge(old::Effects, new::Effects)
    return Effects(
        tristate_merge(
            old.consistent, new.consistent),
        tristate_merge(
            old.effect_free, new.effect_free),
        tristate_merge(
            old.nothrow, new.nothrow),
        tristate_merge(
            old.terminates, new.terminates),
        old.nonoverlayed & new.nonoverlayed,
        tristate_merge(
            old.notaskstate, new.notaskstate),
        old.inbounds_taints_consistency | new.inbounds_taints_consistency)
end

struct EffectsOverride
    consistent::Bool
    effect_free::Bool
    nothrow::Bool
    terminates_globally::Bool
    terminates_locally::Bool
    notaskstate::Bool
end

function encode_effects_override(eo::EffectsOverride)
    e = 0x00
    eo.consistent && (e |= 0x01)
    eo.effect_free && (e |= 0x02)
    eo.nothrow && (e |= 0x04)
    eo.terminates_globally && (e |= 0x08)
    eo.terminates_locally && (e |= 0x10)
    eo.notaskstate && (e |= 0x20)
    return e
end

function decode_effects_override(e::UInt8)
    return EffectsOverride(
        (e & 0x01) != 0x00,
        (e & 0x02) != 0x00,
        (e & 0x04) != 0x00,
        (e & 0x08) != 0x00,
        (e & 0x10) != 0x00,
        (e & 0x20) != 0x00)
end

"""
    InferenceResult

A type that represents the result of running type inference on a chunk of code.
"""
mutable struct InferenceResult
    linfo::MethodInstance
    argtypes::Vector{Any}
    overridden_by_const::BitVector
    result                   # ::Type, or InferenceState if WIP
    src                      # ::Union{CodeInfo, OptimizationState} if inferred copy is available, nothing otherwise
    valid_worlds::WorldRange # if inference and optimization is finished
    ipo_effects::Effects     # if inference is finished
    effects::Effects         # if optimization is finished
    argescapes               # ::ArgEscapeCache if optimized, nothing otherwise
    function InferenceResult(linfo::MethodInstance,
                             arginfo#=::Union{Nothing,Tuple{ArgInfo,InferenceState}}=# = nothing)
        argtypes, overridden_by_const = matching_cache_argtypes(linfo, arginfo)
        return new(linfo, argtypes, overridden_by_const, Any, nothing,
            WorldRange(), Effects(), Effects(), nothing)
    end
end

"""
    OptimizationParams

Parameters that control optimizer operation.
"""
struct OptimizationParams
    inlining::Bool              # whether inlining is enabled
    inline_cost_threshold::Int  # number of CPU cycles beyond which it's not worth inlining
    inline_nonleaf_penalty::Int # penalty for dynamic dispatch
    inline_tupleret_bonus::Int  # extra inlining willingness for non-concrete tuple return types (in hopes of splitting it up)
    inline_error_path_cost::Int # cost of (un-optimized) calls in blocks that throw

    trust_inference::Bool

    # Duplicating for now because optimizer inlining requires it.
    # Keno assures me this will be removed in the near future
    MAX_METHODS::Int
    MAX_TUPLE_SPLAT::Int
    MAX_UNION_SPLITTING::Int

    function OptimizationParams(;
            inlining::Bool = inlining_enabled(),
            inline_cost_threshold::Int = 100,
            inline_nonleaf_penalty::Int = 1000,
            inline_tupleret_bonus::Int = 250,
            inline_error_path_cost::Int = 20,
            max_methods::Int = 3,
            tuple_splat::Int = 32,
            union_splitting::Int = 4,
            trust_inference::Bool = false
        )
        return new(
            inlining,
            inline_cost_threshold,
            inline_nonleaf_penalty,
            inline_tupleret_bonus,
            inline_error_path_cost,
            trust_inference,
            max_methods,
            tuple_splat,
            union_splitting
        )
    end
end

"""
    InferenceParams

Parameters that control type inference operation.
"""
struct InferenceParams
    ipo_constant_propagation::Bool
    aggressive_constant_propagation::Bool
    unoptimize_throw_blocks::Bool

    # don't consider more than N methods. this trades off between
    # compiler performance and generated code performance.
    # typically, considering many methods means spending lots of time
    # obtaining poor type information.
    # It is important for N to be >= the number of methods in the error()
    # function, so we can still know that error() is always Bottom.
    MAX_METHODS::Int
    # the maximum number of union-tuples to swap / expand
    # before computing the set of matching methods
    MAX_UNION_SPLITTING::Int
    # the maximum number of union-tuples to swap / expand
    # when inferring a call to _apply_iterate
    MAX_APPLY_UNION_ENUM::Int

    # parameters limiting large (tuple) types
    TUPLE_COMPLEXITY_LIMIT_DEPTH::Int

    # when attempting to inline _apply_iterate, abort the optimization if the
    # tuple contains more than this many elements
    MAX_TUPLE_SPLAT::Int

    function InferenceParams(;
            ipo_constant_propagation::Bool = true,
            aggressive_constant_propagation::Bool = false,
            unoptimize_throw_blocks::Bool = true,
            max_methods::Int = 3,
            union_splitting::Int = 4,
            apply_union_enum::Int = 8,
            tupletype_depth::Int = 3,
            tuple_splat::Int = 32,
        )
        return new(
            ipo_constant_propagation,
            aggressive_constant_propagation,
            unoptimize_throw_blocks,
            max_methods,
            union_splitting,
            apply_union_enum,
            tupletype_depth,
            tuple_splat,
        )
    end
end

"""
    NativeInterpreter

This represents Julia's native type inference algorithm and codegen backend.
It contains many parameters used by the compilation pipeline.
"""
struct NativeInterpreter <: AbstractInterpreter
    # Cache of inference results for this particular interpreter
    cache::Vector{InferenceResult}
    # The world age we're working inside of
    world::UInt

    # Parameters for inference and optimization
    inf_params::InferenceParams
    opt_params::OptimizationParams

    function NativeInterpreter(world::UInt = get_world_counter();
                               inf_params = InferenceParams(),
                               opt_params = OptimizationParams(),
                               )
        # Sometimes the caller is lazy and passes typemax(UInt).
        # we cap it to the current world age
        if world == typemax(UInt)
            world = get_world_counter()
        end

        # If they didn't pass typemax(UInt) but passed something more subtly
        # incorrect, fail out loudly.
        @assert world <= get_world_counter()

        return new(
            # Initially empty cache
            Vector{InferenceResult}(),

            # world age counter
            world,

            # parameters for inference and optimization
            inf_params,
            opt_params,
        )
    end
end

# Quickly and easily satisfy the AbstractInterpreter API contract
InferenceParams(ni::NativeInterpreter) = ni.inf_params
OptimizationParams(ni::NativeInterpreter) = ni.opt_params
get_world_counter(ni::NativeInterpreter) = ni.world
get_inference_cache(ni::NativeInterpreter) = ni.cache
code_cache(ni::NativeInterpreter) = WorldView(GLOBAL_CI_CACHE, get_world_counter(ni))

"""
    lock_mi_inference(ni::NativeInterpreter, mi::MethodInstance)

Hint that `mi` is in inference to help accelerate bootstrapping.
This helps us limit the amount of wasted work we might do when inference is working on initially inferring itself
by letting us detect when inference is already in progress and not running a second copy on it.
This creates a data-race, but the entry point into this code from C (`jl_type_infer`) already includes detection and restriction on recursion,
so it is hopefully mostly a benign problem (since it should really only happen during the first phase of bootstrapping that we encounter this flag).
"""
lock_mi_inference(::NativeInterpreter, mi::MethodInstance) = (mi.inInference = true; nothing)
lock_mi_inference(::AbstractInterpreter, ::MethodInstance) = return

"""
See `lock_mi_inference`.
"""
unlock_mi_inference(::NativeInterpreter, mi::MethodInstance) = (mi.inInference = false; nothing)
unlock_mi_inference(::AbstractInterpreter, ::MethodInstance) = return

"""
Emit an analysis remark during inference for the current line (`sv.pc`).
These annotations are ignored by the native interpreter, but can be used by external tooling
to annotate inference results.
"""
add_remark!(::AbstractInterpreter, sv#=::InferenceState=#, s) = return

may_optimize(::AbstractInterpreter) = true
may_compress(::AbstractInterpreter) = true
may_discard_trees(::AbstractInterpreter) = true
verbose_stmt_info(::AbstractInterpreter) = false

"""
    method_table(interp::AbstractInterpreter) -> MethodTableView

Returns a method table this `interp` uses for method lookup.
External `AbstractInterpreter` can optionally return `OverlayMethodTable` here
to incorporate customized dispatches for the overridden methods.
"""
method_table(interp::AbstractInterpreter) = InternalMethodTable(get_world_counter(interp))

"""
By default `AbstractInterpreter` implements the following inference bail out logic:
- `bail_out_toplevel_call(::AbstractInterpreter, sig, ::InferenceState)`: bail out from inter-procedural inference when inferring top-level and non-concrete call site `callsig`
- `bail_out_call(::AbstractInterpreter, rt, ::InferenceState)`: bail out from inter-procedural inference when return type `rt` grows up to `Any`
- `bail_out_apply(::AbstractInterpreter, rt, ::InferenceState)`: bail out from `_apply_iterate` inference when return type `rt` grows up to `Any`

It also bails out from local statement/frame inference when any lattice element gets down to `Bottom`,
but `AbstractInterpreter` doesn't provide a specific interface for configuring it.
"""
bail_out_toplevel_call(::AbstractInterpreter, @nospecialize(callsig), sv#=::InferenceState=#) =
    return sv.restrict_abstract_call_sites && !isdispatchtuple(callsig)
bail_out_call(::AbstractInterpreter, @nospecialize(rt), sv#=::InferenceState=#) =
    return rt === Any
bail_out_apply(::AbstractInterpreter, @nospecialize(rt), sv#=::InferenceState=#) =
    return rt === Any

"""
    infer_compilation_signature(::AbstractInterpreter)::Bool

For some call sites (for example calls to varargs methods), the signature to be compiled
and executed at run time can differ from the argument types known at the call site.
This flag controls whether we should always infer the compilation signature in addition
to the call site signature.
"""
infer_compilation_signature(::AbstractInterpreter) = false
infer_compilation_signature(::NativeInterpreter) = true
