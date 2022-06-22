# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    AbstractInterpreter

An abstract base class that allows multiple dispatch to determine the method of
executing Julia code. The native Julia-LLVM pipeline is enabled by using the
`NativeInterpreter` concrete instantiation of this abstract class, others can be
swapped in as long as they follow the `AbstractInterpreter` API.

If `interp::NewInterpreter` is an `AbstractInterpreter`, it is expected to provide at least
the following methods to satisfy the `AbstractInterpreter` API requirement:
- `InferenceParams(interp::NewInterpreter)` - return an `InferenceParams` instance
- `OptimizationParams(interp::NewInterpreter)` - return an `OptimizationParams` instance
- `get_world_counter(interp::NewInterpreter)` - return the world age for this interpreter
- `get_inference_cache(interp::NewInterpreter)` - return the local inference cache
- `code_cache(interp::NewInterpreter)` - return the global inference cache
"""
abstract type AbstractInterpreter end

struct ArgInfo
    fargs::Union{Nothing,Vector{Any}}
    argtypes::Vector{Any}
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
    # NOTE the main constructor is defined within inferencestate.jl
    global function _InferenceResult(
        linfo::MethodInstance,
        arginfo#=::Union{Nothing,Tuple{ArgInfo,InferenceState}}=#)
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

This represents Julia's native type inference algorithm and the Julia-LLVM codegen backend.
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
    already_inferred_quick_test(::AbstractInterpreter, ::MethodInstance)

For the `NativeInterpreter`, we don't need to do an actual cache query to know if something
was already inferred. If we reach this point, but the inference flag has been turned off,
then it's in the cache. This is purely for a performance optimization.
"""
already_inferred_quick_test(interp::NativeInterpreter, mi::MethodInstance) = !mi.inInference
already_inferred_quick_test(interp::AbstractInterpreter, mi::MethodInstance) = false

"""
    lock_mi_inference(::AbstractInterpreter, mi::MethodInstance)

Hint that `mi` is in inference to help accelerate bootstrapping.
This is particularly used by `NativeInterpreter` and helps us limit the amount of wasted
work we might do when inference is working on initially inferring itself by letting us
detect when inference is already in progress and not running a second copy on it.
This creates a data-race, but the entry point into this code from C (`jl_type_infer`)
already includes detection and restriction on recursion, so it is hopefully mostly a
benign problem, since it should really only happen during the first phase of bootstrapping
that we encounter this flag.
"""
lock_mi_inference(::NativeInterpreter, mi::MethodInstance) = (mi.inInference = true; nothing)
lock_mi_inference(::AbstractInterpreter, ::MethodInstance) = return

"""
See `lock_mi_inference`.
"""
unlock_mi_inference(::NativeInterpreter, mi::MethodInstance) = (mi.inInference = false; nothing)
unlock_mi_inference(::AbstractInterpreter, ::MethodInstance) = return

"""
    add_remark!(::AbstractInterpreter, sv::InferenceState, remark)

Emit an analysis remark during inference for the current line (i.e. `sv.currpc`).
These annotations are ignored by default, but can be used by external tooling to annotate
inference results.
"""
function add_remark! end

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
- `bail_out_toplevel_call(::AbstractInterpreter, sig, ::InferenceState)`: bail out from
   inter-procedural inference when inferring top-level and non-concrete call site `callsig`
- `bail_out_call(::AbstractInterpreter, rt, ::InferenceState)`: bail out from
  inter-procedural  inference when return type `rt` grows up to `Any`
- `bail_out_apply(::AbstractInterpreter, rt, ::InferenceState)`: bail out from
  `_apply_iterate` inference when return type `rt` grows up to `Any`

It also bails out from local statement/frame inference when any lattice element gets down to `Bottom`,
but `AbstractInterpreter` doesn't provide a specific interface for configuring it.
"""
function bail_out_toplevel_call end, function bail_out_call end, function bail_out_apply end

"""
    infer_compilation_signature(::AbstractInterpreter)::Bool

For some call sites (for example calls to varargs methods), the signature to be compiled
and executed at run time can differ from the argument types known at the call site.
This flag controls whether we should always infer the compilation signature in addition
to the call site signature.
"""
infer_compilation_signature(::AbstractInterpreter) = false
infer_compilation_signature(::NativeInterpreter) = true
