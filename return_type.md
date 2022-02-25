    return_type(f, T::Type{<:Tuple}) -> R::Type

Return a type `R` such that `f(args...) isa R` where `args isa T`.

The type `R` obtained from `return_type` is merely an upper bound.  There may
exist a stricter type `S` such that `f(args...) isa S` for every `args isa T`
with `S <: R` and `S != R`. Furthermore, the exact type `R` obtained from
`return_type` depends on various factors including but not limited to the exact
Julia version used, packages loaded, and command line options. As such, when
used in publicly registered packages, **it is the package authors'
responsibility to ensure that the API guarantees provided by the package do not
depend on the exact type `R` obtained from `return_type`.**

# Extended help

## Examples

The following function is an invalid use-case of `return_type`.

```julia
"""
    invalid_usecase1(f, xs::AbstractArray) -> ys::Array

Return an array `ys` such that `vec(ys)` is `isequal`-equivalent to

    [f(xs[1]), f(xs[2]), ..., f(xs[end])]
"""
function invalid_usecase1(f, xs)
    R = return_type(f, Tuple{eltype(xs)})
    ys = similar(xs, R)
    for i in eachindex(xs, ys)
        ys[i] = f(xs[i])
    end
    return ys
end
```

This is because the value obtained through `eltype(invalid_usecase1(f, xs))`
depends on exactly what `return_type` returns.  It may be fixed by re-computing
the element type before returning the result.

```julia
function valid_usecase1(f, xs)
    R = return_type(f, Tuple{eltype(xs)})
    ys = similar(xs, R)
    S = Union{}
    for i in eachindex(xs, ys)
        ys[i] = f(xs[i])
        S = promote_type(S, typeof(ys[i]))
    end
    if S != R
        zs = similar(xs, S)
        copyto!(zs, ys)
        return zs
    end
    return ys
end
```

Note that using [`isconcretetype`](@ref) is not enough to safely use
`return_type`.  Following function is another invalid use-case of
`return_type`.

```julia
function invalid_usecase2(f, xs)
    R = return_type(f, Tuple{eltype(xs)})
    if isconcretetype(R)
        ys = similar(xs, R)
    else
        ys = similar(xs, Any)
    end
    for i in eachindex(xs, ys)
        ys[i] = f(xs[i])
    end
    return ys
end
```

This is because whether or not the caller gets `Any` element type depends
on if `return_type` can infer a concrete return type of the given
function.  A fix similar to `valid_usecase1` can be used.

*Technically*, another possible fix for `invalid_usecase1` and
`invalid_usecase2` is to loosen the API guarantee:

>     another_valid_usecase1(f, xs::AbstractArray) -> ys::Array
>
> Return an array `ys` such that every element in `xs` with the same index
> is mapped with `f`.
>
> The element type of `ys` is _undefined_. It must not be used with generic
> functions whose behavior depend on the element type of `ys`.

However, it is strongly discouraged to define such unconventional API guarantee.
