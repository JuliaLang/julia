# This file is a part of Julia. License is MIT: https://julialang.org/license

struct Params
    cache::Vector{InferenceResult}
    world::UInt

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

    # parameters limiting large types
    MAX_TUPLETYPE_LEN::Int

    # when attempting to inlining _apply, abort the optimization if the tuple
    # contains more than this many elements
    MAX_TUPLE_SPLAT::Int

    # reasonable defaults
    function Params(world::UInt;
                    inlining::Bool = inlining_enabled(),
                    inline_cost_threshold::Int = 100,
                    inline_nonleaf_penalty::Int = 1000,
                    inline_tupleret_bonus::Int = 400,
                    max_methods::Int = 4,
                    tupletype_len::Int = 15,
                    tuple_splat::Int = 16,
                    union_splitting::Int = 4,
                    apply_union_enum::Int = 8)
        return new(Vector{InferenceResult}(),
                   world, inlining, true, false, inline_cost_threshold, inline_nonleaf_penalty,
                   inline_tupleret_bonus, max_methods, union_splitting, apply_union_enum,
                   tupletype_len, tuple_splat)
    end
end
