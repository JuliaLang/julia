# This file is a part of Julia. License is MIT: https://julialang.org/license

module OptimizedGenerics

# This file defines interfaces that are recognized and optimized by the compiler
# They are intended to be used by data structure implementations that wish to
# opt into some level of compiler optimizations. These interfaces are
# EXPERIMENTAL and currently intended for use by Base only. They are subject
# to change or removal without notice. It is undefined behavior to add methods
# to these generics that do not conform to the specified interface.
#
# The intended way to use these generics is that data structures will provide
# appropriate implementations for a generic. In the absence of compiler
# optimizations, these behave like regular methods. However, the compiler is
# semantically allowed to perform certain structural optimizations on
# appropriate combinations of these intrinsics without proving correctness.

# Compiler-recognized generics for immutable key-value stores (dicts, etc.)
"""
    module KeyValue

Implements a key-value like interface where the compiler has liberty to perform
the following transformations. The core optimization semantically allowed for
the compiler is:

    get(set(x, key, val), key) -> (val,)

where the compiler will recursively look through `x`. Keys are compared by
egality.

Implementations must observe the following constraints:

1. It is undefined behavior for `get` not to return the exact (by egality) val
   stored for a given `key`.
"""
module KeyValue
    """
        set(collection, [key [, val]])
        set(T, collection, key, val)

    Set the `key` in `collection` to `val`. If `val` is omitted, deletes the
    value from the collection. If `key` is omitted as well, deletes all elements
    of the collection.
    """
    function set end

    """
        get(collection, key)

    Retrieve the value corresponding to `key` in `collection` as a single
    element tuple or `nothing` if no value corresponding to the key was found.
    `key`s are compared by egal.
    """
    function get end
end

end
