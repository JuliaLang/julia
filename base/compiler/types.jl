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
abstract type AbstractLattice end

struct ArgInfo
    fargs::Union{Nothing,Vector{Any}}
    argtypes::Vector{Any}
end

struct StmtInfo
    """
    If `used` is false, we know that the return value is statically unused and
    need thus not be computed.
    """
    used::Bool
end

struct MethodInfo
    propagate_inbounds::Bool
    method_for_inference_limit_heuristics::Union{Nothing,Method}
end
MethodInfo(src::CodeInfo) = MethodInfo(
    src.propagate_inbounds,
    src.method_for_inference_limit_heuristics::Union{Nothing,Method})

"""
    v::VarState

A special wrapper that represents a local variable of a method being analyzed.
This does not participate in the native type system nor the inference lattice, and it thus
should be always unwrapped to `v.typ` when performing any type or lattice operations on it.
`v.undef` represents undefined-ness of this static parameter. If `true`, it means that the
variable _may_ be undefined at runtime, otherwise it is guaranteed to be defined.
If `v.typ === Bottom` it means that the variable is strictly undefined.
"""
struct VarState
    typ
    undef::Bool
    VarState(@nospecialize(typ), undef::Bool) = new(typ, undef)
end

abstract type ForwardableArgtypes end

"""
    InferenceResult(linfo::MethodInstance, [argtypes::ForwardableArgtypes, 𝕃::AbstractLattice])

A type that represents the result of running type inference on a chunk of code.

See also [`matching_cache_argtypes`](@ref).
"""
mutable struct InferenceResult
    const linfo::MethodInstance
    const argtypes::Vector{Any}
    const overridden_by_const::BitVector
    result                   # extended lattice element if inferred, nothing otherwise
    src                      # ::Union{CodeInfo, IRCode, OptimizationState} if inferred copy is available, nothing otherwise
    valid_worlds::WorldRange # if inference and optimization is finished
    ipo_effects::Effects     # if inference is finished
    effects::Effects         # if optimization is finished
    argescapes               # ::ArgEscapeCache if optimized, nothing otherwise
    must_be_codeinf::Bool    # if this must come out as CodeInfo or leaving it as IRCode is ok
    function InferenceResult(linfo::MethodInstance, cache_argtypes::Vector{Any}, overridden_by_const::BitVector)
        # def = linfo.def
        # nargs = def isa Method ? Int(def.nargs) : 0
        # @assert length(cache_argtypes) == nargs
        return new(linfo, cache_argtypes, overridden_by_const, nothing, nothing,
            WorldRange(), Effects(), Effects(), nothing, true)
    end
end
InferenceResult(linfo::MethodInstance, 𝕃::AbstractLattice=fallback_lattice) =
    InferenceResult(linfo, matching_cache_argtypes(𝕃, linfo)...)
InferenceResult(linfo::MethodInstance, argtypes::ForwardableArgtypes, 𝕃::AbstractLattice=fallback_lattice) =
    InferenceResult(linfo, matching_cache_argtypes(𝕃, linfo, argtypes)...)

"""
    inf_params::InferenceParams

Parameters that control abstract interpretation-based type inference operation.

---
- `inf_params.max_methods::Int = 3`\\
  Type inference gives up analysis on a call when there are more than `max_methods` matching
  methods. This trades off between compiler latency and generated code performance.
  Typically, considering many methods means spending _lots_ of time obtaining poor type
  information, so this option should be kept low. [`Base.Experimental.@max_methods`](@ref)
  can have a more fine-grained control on this configuration with per-module or per-method
  annotation basis.
---
- `inf_params.max_union_splitting::Int = 4`\\
  Specifies the maximum number of union-tuples to swap or expand before computing the set of
  matching methods or conditional types.
---
- `inf_params.max_apply_union_enum::Int = 8`\\
  Specifies the maximum number of union-tuples to swap or expand when inferring a call to
  `Core._apply_iterate`.
---
- `inf_params.max_tuple_splat::Int = 32`\\
  When attempting to infer a call to `Core._apply_iterate`, abort the analysis if the tuple
  contains more than this many elements.
---
- `inf_params.tuple_complexity_limit_depth::Int = 3`\\
  Specifies the maximum depth of large tuple type that can appear as specialized method
  signature when inferring a recursive call graph.
---
- `inf_params.ipo_constant_propagation::Bool = true`\\
  If `false`, disables analysis with extended lattice information, i.e. disables any of
  the concrete evaluation, semi-concrete interpretation and constant propagation entirely.
  [`Base.@constprop :none`](@ref Base.@constprop) can have a more fine-grained control on
  this configuration with per-method annotation basis.
---
- `inf_params.aggressive_constant_propagation::Bool = false`\\
  If `true`, forces constant propagation on any methods when any extended lattice
  information available. [`Base.@constprop :aggressive`](@ref Base.@constprop) can have a
  more fine-grained control on this configuration with per-method annotation basis.
---
- `inf_params.unoptimize_throw_blocks::Bool = true`\\
  If `true`, skips inferring calls that are in a block that is known to `throw`.
  It may improve the compiler latency without sacrificing the runtime performance
  in common situations.
---
- `inf_params.assume_bindings_static::Bool = false`\\
  If `true`, assumes that no new bindings will be added, i.e. a non-existing binding at
  inference time can be assumed to always not exist at runtime (and thus e.g. any access to
  it will `throw`). Defaults to `false` since this assumption does not hold in Julia's
  semantics for native code execution.
---
"""
struct InferenceParams
    max_methods::Int
    max_union_splitting::Int
    max_apply_union_enum::Int
    max_tuple_splat::Int
    tuple_complexity_limit_depth::Int
    ipo_constant_propagation::Bool
    aggressive_constant_propagation::Bool
    unoptimize_throw_blocks::Bool
    assume_bindings_static::Bool
    ignore_recursion_hardlimit::Bool

    function InferenceParams(
        max_methods::Int,
        max_union_splitting::Int,
        max_apply_union_enum::Int,
        max_tuple_splat::Int,
        tuple_complexity_limit_depth::Int,
        ipo_constant_propagation::Bool,
        aggressive_constant_propagation::Bool,
        unoptimize_throw_blocks::Bool,
        assume_bindings_static::Bool,
        ignore_recursion_hardlimit::Bool)
        return new(
            max_methods,
            max_union_splitting,
            max_apply_union_enum,
            max_tuple_splat,
            tuple_complexity_limit_depth,
            ipo_constant_propagation,
            aggressive_constant_propagation,
            unoptimize_throw_blocks,
            assume_bindings_static,
            ignore_recursion_hardlimit)
    end
end
function InferenceParams(
    params::InferenceParams = InferenceParams( # default constructor
        #=max_methods::Int=# 3,
        #=max_union_splitting::Int=# 4,
        #=max_apply_union_enum::Int=# 8,
        #=max_tuple_splat::Int=# 32,
        #=tuple_complexity_limit_depth::Int=# 3,
        #=ipo_constant_propagation::Bool=# true,
        #=aggressive_constant_propagation::Bool=# false,
        #=unoptimize_throw_blocks::Bool=# true,
        #=assume_bindings_static::Bool=# false,
        #=ignore_recursion_hardlimit::Bool=# false);
    max_methods::Int = params.max_methods,
    max_union_splitting::Int = params.max_union_splitting,
    max_apply_union_enum::Int = params.max_apply_union_enum,
    max_tuple_splat::Int = params.max_tuple_splat,
    tuple_complexity_limit_depth::Int = params.tuple_complexity_limit_depth,
    ipo_constant_propagation::Bool = params.ipo_constant_propagation,
    aggressive_constant_propagation::Bool = params.aggressive_constant_propagation,
    unoptimize_throw_blocks::Bool = params.unoptimize_throw_blocks,
    assume_bindings_static::Bool = params.assume_bindings_static,
    ignore_recursion_hardlimit::Bool = params.ignore_recursion_hardlimit)
    return InferenceParams(
        max_methods,
        max_union_splitting,
        max_apply_union_enum,
        max_tuple_splat,
        tuple_complexity_limit_depth,
        ipo_constant_propagation,
        aggressive_constant_propagation,
        unoptimize_throw_blocks,
        assume_bindings_static,
        ignore_recursion_hardlimit)
end

"""
    opt_params::OptimizationParams

Parameters that control optimizer operation.

---
- `opt_params.inlining::Bool = inlining_enabled()`\\
  Controls whether or not inlining is enabled.
---
- `opt_params.inline_cost_threshold::Int = 100`\\
  Specifies the number of CPU cycles beyond which it's not worth inlining.
---
- `opt_params.inline_nonleaf_penalty::Int = 1000`\\
  Specifies the penalty cost for a dynamic dispatch.
---
- `opt_params.inline_tupleret_bonus::Int = 250`\\
  Specifies the extra inlining willingness for a method specialization with non-concrete
  tuple return types (in hopes of splitting it up). `opt_params.inline_tupleret_bonus` will
  be added to `opt_params.inline_cost_threshold` when making inlining decision.
---
- `opt_params.inline_error_path_cost::Int = 20`\\
  Specifies the penalty cost for an un-optimized dynamic call in a block that is known to
  `throw`. See also [`(inf_params::InferenceParams).unoptimize_throw_blocks`](@ref InferenceParams).
---
- `opt_params.max_tuple_splat::Int = 32`\\
  When attempting to inline `Core._apply_iterate`, abort the optimization if the tuple
  contains more than this many elements.
---
- `opt_params.compilesig_invokes::Bool = true`\\
  If `true`, gives the inliner license to change which `MethodInstance` to invoke when
  generating `:invoke` expression based on the [`@nospecialize`](@ref) annotation,
  in order to avoid over-specialization.
---
- `opt_params.trust_inference::Bool = false`\\
  If `false`, the inliner will unconditionally generate a fallback block when union-splitting
  a callsite, in case of existing subtyping bugs. This option may be removed in the future.
---
- `opt_params.assume_fatal_throw::Bool = false`\\
  If `true`, gives the optimizer license to assume that any `throw` is fatal and thus the
  state after a `throw` is not externally observable. In particular, this gives the
  optimizer license to move side effects (that are proven not observed within a particular
  code path) across a throwing call. Defaults to `false`.
---
"""
struct OptimizationParams
    inlining::Bool
    inline_cost_threshold::Int
    inline_nonleaf_penalty::Int
    inline_tupleret_bonus::Int
    inline_error_path_cost::Int
    max_tuple_splat::Int
    compilesig_invokes::Bool
    trust_inference::Bool
    assume_fatal_throw::Bool

    function OptimizationParams(
        inlining::Bool,
        inline_cost_threshold::Int,
        inline_nonleaf_penalty::Int,
        inline_tupleret_bonus::Int,
        inline_error_path_cost::Int,
        max_tuple_splat::Int,
        compilesig_invokes::Bool,
        trust_inference::Bool,
        assume_fatal_throw::Bool)
        return new(
            inlining,
            inline_cost_threshold,
            inline_nonleaf_penalty,
            inline_tupleret_bonus,
            inline_error_path_cost,
            max_tuple_splat,
            compilesig_invokes,
            trust_inference,
            assume_fatal_throw)
    end
end
function OptimizationParams(
    params::OptimizationParams = OptimizationParams(
        #=inlining::Bool=# inlining_enabled(),
        #=inline_cost_threshold::Int=# 100,
        #=inline_nonleaf_penalty::Int=# 1000,
        #=inline_tupleret_bonus::Int=# 250,
        #=inline_error_path_cost::Int=# 20,
        #=max_tuple_splat::Int=# 32,
        #=compilesig_invokes::Bool=# true,
        #=trust_inference::Bool=# false,
        #=assume_fatal_throw::Bool=# false);
    inlining::Bool = params.inlining,
    inline_cost_threshold::Int = params.inline_cost_threshold,
    inline_nonleaf_penalty::Int = params.inline_nonleaf_penalty,
    inline_tupleret_bonus::Int = params.inline_tupleret_bonus,
    inline_error_path_cost::Int = params.inline_error_path_cost,
    max_tuple_splat::Int = params.max_tuple_splat,
    compilesig_invokes::Bool = params.compilesig_invokes,
    trust_inference::Bool = params.trust_inference,
    assume_fatal_throw::Bool = params.assume_fatal_throw)
    return OptimizationParams(
        inlining,
        inline_cost_threshold,
        inline_nonleaf_penalty,
        inline_tupleret_bonus,
        inline_error_path_cost,
        max_tuple_splat,
        compilesig_invokes,
        trust_inference,
        assume_fatal_throw)
end

"""
    NativeInterpreter <: AbstractInterpreter

This represents Julia's native type inference algorithm and the Julia-LLVM codegen backend.
"""
struct NativeInterpreter <: AbstractInterpreter
    # The world age we're working inside of
    world::UInt

    # method table to lookup for during inference on this world age
    method_table::CachedMethodTable{InternalMethodTable}

    # Cache of inference results for this particular interpreter
    inf_cache::Vector{InferenceResult}

    # Parameters for inference and optimization
    inf_params::InferenceParams
    opt_params::OptimizationParams

    # a boolean flag to indicate if this interpreter is performing semi concrete interpretation
    irinterp::Bool
end

function NativeInterpreter(world::UInt = get_world_counter();
                           inf_params::InferenceParams = InferenceParams(),
                           opt_params::OptimizationParams = OptimizationParams())
    # Sometimes the caller is lazy and passes typemax(UInt).
    # we cap it to the current world age for correctness
    if world == typemax(UInt)
        world = get_world_counter()
    end

    # If they didn't pass typemax(UInt) but passed something more subtly
    # incorrect, fail out loudly.
    @assert world <= get_world_counter()

    method_table = CachedMethodTable(InternalMethodTable(world))

    inf_cache = Vector{InferenceResult}() # Initially empty cache

    return NativeInterpreter(world, method_table, inf_cache, inf_params, opt_params, #=irinterp=#false)
end

function NativeInterpreter(interp::NativeInterpreter;
                           world::UInt = interp.world,
                           method_table::CachedMethodTable{InternalMethodTable} = interp.method_table,
                           inf_cache::Vector{InferenceResult} = interp.inf_cache,
                           inf_params::InferenceParams = interp.inf_params,
                           opt_params::OptimizationParams = interp.opt_params,
                           irinterp::Bool = interp.irinterp)
    return NativeInterpreter(world, method_table, inf_cache, inf_params, opt_params, irinterp)
end

# Quickly and easily satisfy the AbstractInterpreter API contract
InferenceParams(interp::NativeInterpreter) = interp.inf_params
OptimizationParams(interp::NativeInterpreter) = interp.opt_params
get_world_counter(interp::NativeInterpreter) = interp.world
get_inference_cache(interp::NativeInterpreter) = interp.inf_cache
code_cache(interp::NativeInterpreter) = WorldView(GLOBAL_CI_CACHE, get_world_counter(interp))

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
method_table(interp::NativeInterpreter) = interp.method_table

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

typeinf_lattice(::AbstractInterpreter) = InferenceLattice(BaseInferenceLattice.instance)
ipo_lattice(::AbstractInterpreter) = InferenceLattice(IPOResultLattice.instance)
optimizer_lattice(::AbstractInterpreter) = OptimizerLattice(SimpleInferenceLattice.instance)

typeinf_lattice(interp::NativeInterpreter) = interp.irinterp ? optimizer_lattice(interp) : InferenceLattice(BaseInferenceLattice.instance)
ipo_lattice(interp::NativeInterpreter) = interp.irinterp ? optimizer_lattice(interp) : InferenceLattice(IPOResultLattice.instance)
optimizer_lattice(interp::NativeInterpreter) = OptimizerLattice(SimpleInferenceLattice.instance)

"""
    switch_to_irinterp(interp::AbstractInterpreter) -> irinterp::AbstractInterpreter

This interface allows `ir_abstract_constant_propagation` to convert `interp` to a new
`irinterp::AbstractInterpreter` to perform semi-concrete interpretation.
`NativeInterpreter` uses this interface to switch its lattice to `optimizer_lattice` during
semi-concrete interpretation on `IRCode`.
"""
switch_to_irinterp(interp::AbstractInterpreter) = interp
switch_to_irinterp(interp::NativeInterpreter) = NativeInterpreter(interp; irinterp=true)

"""
    switch_from_irinterp(irinterp::AbstractInterpreter) -> interp::AbstractInterpreter

The inverse operation of `switch_to_irinterp`, allowing `typeinf` to convert `irinterp` back
to a new `interp::AbstractInterpreter` to perform ordinary abstract interpretation.
"""
switch_from_irinterp(irinterp::AbstractInterpreter) = irinterp
switch_from_irinterp(irinterp::NativeInterpreter) = NativeInterpreter(irinterp; irinterp=false)

abstract type CallInfo end

@nospecialize

nsplit(info::CallInfo) = nsplit_impl(info)::Union{Nothing,Int}
getsplit(info::CallInfo, idx::Int) = getsplit_impl(info, idx)::MethodLookupResult
getresult(info::CallInfo, idx::Int) = getresult_impl(info, idx)

nsplit_impl(::CallInfo) = nothing
getsplit_impl(::CallInfo, ::Int) = error("unexpected call into `getsplit`")
getresult_impl(::CallInfo, ::Int) = nothing

@specialize
