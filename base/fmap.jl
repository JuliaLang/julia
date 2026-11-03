# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    fmap(f, x, xs...; ET=nothing)

For a single `x`, apply the callable `f` as a univariate function to each element in `x`,
while preserving the structure of `x`, and return a new object with the result. This
procedure is referred to as "mapping". The new object must be of the same type as `x`,
except that the type of the elements could change depending on the types of the values
returned from `f`. The meaning of "element" depends on the type of `x`, but is usually clear
from the context. Custom types can optionally add docstrings for `fmap` that clarify the
values and structure that are mapped over.

For multiple `x` inputs, apply the callable `f` as a multivariate function to elements in
the same location of the `x` inputs and return a new object with the result. The `x` inputs
must have the same type (though the element types may differ) and the same structure, or
else an error will be thrown. The returned object must be of the same type as the `x`
inputs, except that the type of the elements could change depending on the types of the
values returned from `f`. If the types of the `x` inputs do not match, a `MethodError` is
thrown. If the types match but the structures do not, then an `ArgumentError` is thrown.

If all `x` inputs are empty and `ET` is `nothing`, then the output will be an empty
object of the same type and element type as the input(s). If all `x` inputs are empty and
`ET` is not `nothing`, then the output will be an empty object of the same type as the
inputs, except the element type will be `ET`. If the `x` inputs are not empty, then the
element type is determined by the types of the values returned from `f`. The element type
will be widened if necessary to accomodate the type of each individual element in the
returned object. Note that element type widening only affects the element type of the
container, whereas the types of individual elements are determined solely by the output
of `f`.

The order in which `f` is applied to the element locations of the `x` inputs is not
specified.

It is recommended to use `fmap` or `Iterators.map` rather than `map`. However, `map` is
retained for backwards compatibility.

The "f" in `fmap` stands for functor, which comes from Haskell and category theory. Loosely
speaking, a functor is an object that can be mapped over.

See also [`Iterators.map`](@ref).

# Examples
```julia-repl
julia> fmap(sqrt, [4, 9])
2-element Vector{Float64}:
 2.0
 3.0

julia> fmap(x -> 2x, (1, 2.0))
(2, 4.0)

julia> fmap(*, (1, 2.0, "a"), (3, 4.0, "b"))
(3, 8.0, "ab")

julia> fmap(+, [1 2; 3 4], [5 6; 7 8])
2×2 Matrix{Int64}:
  6   8
 10  12

julia> fmap([1, 2], (3, 4))
ERROR: MethodError: no method matching fmap(::Vector{Int64}, ::Tuple{Int64, Int64})
The function `fmap` exists, but no method is defined for this combination of argument types.

julia> fmap(+, [1, 2], [3, 4, 5])
ERROR: ArgumentError: The input containers do not have the same structure.
```
"""
function fmap end
