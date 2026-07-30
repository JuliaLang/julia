# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    TypedCallable{A,R}(f)

Wrap `f` as a concretely typed callable. Calls accept arguments matching the tuple
type `A`, invoke `f` in the latest world, and return a value of type `R`.

Unlike an opaque closure created by [`@opaque`](@ref Base.Experimental.@opaque), a
`TypedCallable` does not capture its creation world.

!!! warning
    This interface is experimental and subject to change or removal without notice.
"""
function (::Type{Core.TypedCallable{A,R}})(@nospecialize(f)) where {A,R}
    A <: Tuple || throw(ArgumentError("TypedCallable argument type must be a Tuple type"))
    # Use a builtin so inference and trimming can recognize the construction.
    return Core._typed_callable(f, A, R)::Core.TypedCallable{A,R}
end
