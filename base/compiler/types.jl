# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    AbstractInterpreter

An abstract base class that allows multiple dispatch to determine the method of
executing Julia code.  The native Julia LLVM pipeline is enabled by using the
`NativeInterpreter` concrete instantiation of this abstract class, others can be
swapped in as long as they follow the AbstractInterpreter API.

All AbstractInterpreters are expected to provide at least the following methods:

- InferenceParams(interp) - return an `InferenceParams` instance
- OptimizationParams(interp) - return an `OptimizationParams` instance
- get_world_counter(interp) - return the world age for this interpreter
- get_inference_cache(interp) - return the runtime inference cache
"""
abstract type AbstractInterpreter; end


"""
    InferenceResult

A type that represents the result of running type inference on a chunk of code.
"""
mutable struct InferenceResult
    linfo::MethodInstance
    argtypes::Vector{Any}
    overridden_by_const::BitVector
    result # ::Type, or InferenceState if WIP
    src #::Union{CodeInfo, OptimizationState, Nothing} # if inferred copy is available
    function InferenceResult(linfo::MethodInstance, given_argtypes = nothing)
        argtypes, overridden_by_const = matching_cache_argtypes(linfo, given_argtypes)
        return new(linfo, argtypes, overridden_by_const, Any, nothing)
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
    inline_tupleret_bonus::Int  # extra willingness for non-isbits tuple return types

    # Duplicating for now because optimizer inlining requires it.
    # Keno assures me this will be removed in the near future
    MAX_METHODS::Int
    MAX_TUPLE_SPLAT::Int
    MAX_UNION_SPLITTING::Int

    function OptimizationParams(;
            inlining::Bool = inlining_enabled(),
            inline_cost_threshold::Int = 100,
            inline_nonleaf_penalty::Int = 1000,
            inline_tupleret_bonus::Int = 400,
            max_methods::Int = 4,
            tuple_splat::Int = 32,
            union_splitting::Int = 4,
        )
        return new(
            inlining,
            inline_cost_threshold,
            inline_nonleaf_penalty,
            inline_tupleret_bonus,
            max_methods,
            tuple_splat,
            union_splitting,
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
    # when inferring a call to _apply
    MAX_APPLY_UNION_ENUM::Int

    # parameters limiting large (tuple) types
    TUPLE_COMPLEXITY_LIMIT_DEPTH::Int

    # when attempting to inlining _apply, abort the optimization if the tuple
    # contains more than this many elements
    MAX_TUPLE_SPLAT::Int

    function InferenceParams(;
            ipo_constant_propagation::Bool = true,
            aggressive_constant_propagation::Bool = false,
            max_methods::Int = 4,
            union_splitting::Int = 4,
            apply_union_enum::Int = 8,
            tupletype_depth::Int = 3,
            tuple_splat::Int = 32,
        )
        return new(
            ipo_constant_propagation,
            aggressive_constant_propagation,
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
