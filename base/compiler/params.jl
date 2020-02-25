# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Params
    cache::Vector{InferenceResult}
    world::UInt
    global_cache::Bool

    # optimization
    inlining::Bool
    ipo_constant_propagation::Bool
    aggressive_constant_propagation::Bool
    inline_cost_threshold::Int  # number of CPU cycles beyond which it's not worth inlining
    inline_nonleaf_penalty::Int # penalty for dynamic dispatch
    inline_tupleret_bonus::Int  # extra willingness for non-isbits tuple return types

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

    # reasonable defaults
    global function CustomParams(world::UInt,
                    ;
                    inlining::Bool = inlining_enabled(),
                    inline_cost_threshold::Int = DEFAULT_PARAMS.inline_cost_threshold,
                    inline_nonleaf_penalty::Int = DEFAULT_PARAMS.inline_nonleaf_penalty,
                    inline_tupleret_bonus::Int = DEFAULT_PARAMS.inline_tupleret_bonus,
                    ipo_constant_propagation::Bool = true,
                    aggressive_constant_propagation::Bool = false,
                    max_methods::Int = DEFAULT_PARAMS.MAX_METHODS,
                    tupletype_depth::Int = DEFAULT_PARAMS.TUPLE_COMPLEXITY_LIMIT_DEPTH,
                    tuple_splat::Int = DEFAULT_PARAMS.MAX_TUPLE_SPLAT,
                    union_splitting::Int = DEFAULT_PARAMS.MAX_UNION_SPLITTING,
                    apply_union_enum::Int = DEFAULT_PARAMS.MAX_APPLY_UNION_ENUM)
        return new(Vector{InferenceResult}(),
                   world, false,
                   inlining, ipo_constant_propagation, aggressive_constant_propagation,
                   inline_cost_threshold, inline_nonleaf_penalty, inline_tupleret_bonus,
                   max_methods, union_splitting, apply_union_enum, tupletype_depth,
                   tuple_splat)
    end
    function Params(world::UInt)
        world == typemax(UInt) && (world = get_world_counter()) # workaround for bad callers
        @assert world <= get_world_counter()
        inlining = inlining_enabled()
        return new(Vector{InferenceResult}(),
                   world, true,
                   #=inlining, ipo_constant_propagation, aggressive_constant_propagation, inline_cost_threshold, inline_nonleaf_penalty,=#
                   inlining, true, false, 100, 1000,
                   #=inline_tupleret_bonus, max_methods, union_splitting, apply_union_enum=#
                   400, 1, 4, 8,
                   #=tupletype_depth, tuple_splat=#
                   3, 32)
    end
end
const DEFAULT_PARAMS = Params(UInt(0))
