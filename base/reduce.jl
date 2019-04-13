# This file is a part of Julia. License is MIT: https://julialang.org/license


###### Generic (map)reduce functions ######

## foldl && mapfoldl

function mapfoldl_impl(f, op, nt::NamedTuple{(:init,)}, itr, i...)
    init = nt.init
    # Unroll the while loop once; if init is known, the call to op may
    # be evaluated at compile time
    y = iterate(itr, i...)
    y === nothing && return init
    v = op(init, f(y[1]))
    while true
        y = iterate(itr, y[2])
        y === nothing && break
        v = op(v, f(y[1]))
    end
    return v
end

function mapfoldl_impl(f, op, nt::NamedTuple{()}, itr)
    y = iterate(itr)
    if y === nothing
        return Base.mapreduce_empty_iter(f, op, itr, IteratorEltype(itr))
    end
    x, i = y
    init = mapreduce_first(f, op, x)
    return mapfoldl_impl(f, op, (init=init,), itr, i)
end


"""
    mapfoldl(f, op, itr; [init])

Like [`mapreduce`](@ref), but with guaranteed left associativity, as in [`foldl`](@ref).
If provided, the keyword argument `init` will be used exactly once. In general, it will be
necessary to provide `init` to work with empty collections.
"""
mapfoldl(f, op, itr; kw...) = mapfoldl_impl(f, op, kw.data, itr)

"""
    foldl(op, itr; [init])

Like [`reduce`](@ref), but with guaranteed left associativity. If provided, the keyword
argument `init` will be used exactly once. In general, it will be necessary to provide
`init` to work with empty collections.

# Examples
```jldoctest
julia> foldl(=>, 1:4)
((1 => 2) => 3) => 4

julia> foldl(=>, 1:4; init=0)
(((0 => 1) => 2) => 3) => 4
```
"""
foldl(op, itr; kw...) = mapfoldl(identity, op, itr; kw...)

## foldr & mapfoldr

mapfoldr_impl(f, op, nt::NamedTuple{(:init,)}, itr) =
    mapfoldl_impl(f, (x,y) -> op(y,x), nt, Iterators.reverse(itr))

# we can't just call mapfoldl_impl with (x,y) -> op(y,x), because
# we need to use the type of op for mapreduce_empty_iter and mapreduce_first.
function mapfoldr_impl(f, op, nt::NamedTuple{()}, itr)
    ritr = Iterators.reverse(itr)
    y = iterate(ritr)
    if y === nothing
        return Base.mapreduce_empty_iter(f, op, itr, IteratorEltype(itr))
    end
    x, i = y
    init = mapreduce_first(f, op, x)
    return mapfoldl_impl(f, (x,y) -> op(y,x), (init=init,), ritr, i)
end

"""
    mapfoldr(f, op, itr; [init])

Like [`mapreduce`](@ref), but with guaranteed right associativity, as in [`foldr`](@ref). If
provided, the keyword argument `init` will be used exactly once. In general, it will be
necessary to provide `init` to work with empty collections.
"""
mapfoldr(f, op, itr; kw...) = mapfoldr_impl(f, op, kw.data, itr)


"""
    foldr(op, itr; [init])

Like [`reduce`](@ref), but with guaranteed right associativity. If provided, the keyword
argument `init` will be used exactly once. In general, it will be necessary to provide
`init` to work with empty collections.

# Examples
```jldoctest
julia> foldr(=>, 1:4)
1 => (2 => (3 => 4))

julia> foldr(=>, 1:4; init=0)
1 => (2 => (3 => (4 => 0)))
```
"""
foldr(op, itr; kw...) = mapfoldr(identity, op, itr; kw...)

## reduce & mapreduce

# `mapreduce_impl()` is called by `mapreduce()` (via `_mapreduce()`, when `A`
# supports linear indexing) and does actual calculations (for `A[ifirst:ilast]` subset).
# For efficiency, no parameter validity checks are done, it's the caller's responsibility.
# `ifirst:ilast` range is assumed to be a valid non-empty subset of `A` indices.

# This is a generic implementation of `mapreduce_impl()`,
# certain `op` (e.g. `min` and `max`) may have their own specialized versions.
@noinline function mapreduce_impl(f, op, A::AbstractArray, ifirst::Integer, ilast::Integer, blksize::Int)
    if ifirst == ilast
        @inbounds a1 = A[ifirst]
        return mapreduce_first(f, op, a1)
    elseif ifirst + blksize > ilast
        # sequential portion
        @inbounds a1 = A[ifirst]
        @inbounds a2 = A[ifirst+1]
        v = op(f(a1), f(a2))
        @simd for i = ifirst + 2 : ilast
            @inbounds ai = A[i]
            v = op(v, f(ai))
        end
        return v
    else
        # pairwise portion
        imid = (ifirst + ilast) >> 1
        v1 = mapreduce_impl(f, op, A, ifirst, imid, blksize)
        v2 = mapreduce_impl(f, op, A, imid+1, ilast, blksize)
        return op(v1, v2)
    end
end

mapreduce_impl(f, op, A::AbstractArray, ifirst::Integer, ilast::Integer) =
    mapreduce_impl(f, op, A, ifirst, ilast, pairwise_blocksize(f, op))

"""
    mapreduce(f, op, itrs...; [init])

Apply function `f` to each element(s) in `itrs`, and then reduce the result using the binary
function `op`. If provided, `init` must be a neutral element for `op` that will be returned
for empty collections. It is unspecified whether `init` is used for non-empty collections.
In general, it will be necessary to provide `init` to work with empty collections.

[`mapreduce`](@ref) is functionally equivalent to calling
`reduce(op, map(f, itr); init=init)`, but will in general execute faster since no
intermediate collection needs to be created. See documentation for [`reduce`](@ref) and
[`map`](@ref).

!!! compat "Julia 1.2"
    `mapreduce` with multiple iterators requires Julia 1.2 or later.

# Examples
```jldoctest
julia> mapreduce(x->x^2, +, [1:3;]) # == 1 + 4 + 9
14
```

The associativity of the reduction is implementation-dependent. Additionally, some
implementations may reuse the return value of `f` for elements that appear multiple times in
`itr`. Use [`mapfoldl`](@ref) or [`mapfoldr`](@ref) instead for
guaranteed left or right associativity and invocation of `f` for every value.
"""
mapreduce(f, op, itr; kw...) = mapfoldl(f, op, itr; kw...)
mapreduce(f, op, itrs...; kw...) = reduce(op, Generator(f, itrs...); kw...)

# Note: sum_seq usually uses four or more accumulators after partial
# unrolling, so each accumulator gets at most 256 numbers
pairwise_blocksize(f, op) = 1024

# This combination appears to show a benefit from a larger block size
pairwise_blocksize(::typeof(abs2), ::typeof(+)) = 4096


# handling empty arrays
_empty_reduce_error() = throw(ArgumentError("reducing over an empty collection is not allowed"))

"""
    Base.reduce_empty(op, T)

The value to be returned when calling [`reduce`](@ref), [`foldl`](@ref) or [`foldr`](@ref)
with reduction `op` over an empty array with element type of `T`.

If not defined, this will throw an `ArgumentError`.
"""
reduce_empty(op, T) = _empty_reduce_error()
reduce_empty(::typeof(+), T) = zero(T)
reduce_empty(::typeof(+), ::Type{Bool}) = zero(Int)
reduce_empty(::typeof(*), T) = one(T)
reduce_empty(::typeof(*), ::Type{<:AbstractChar}) = ""
reduce_empty(::typeof(&), ::Type{Bool}) = true
reduce_empty(::typeof(|), ::Type{Bool}) = false



"""
    Base.mapreduce_empty(f, op, T)

The value to be returned when calling [`mapreduce`](@ref), [`mapfoldl`](@ref`) or
[`mapfoldr`](@ref) with map `f` and reduction `op` over an empty array with element type
of `T`.

If not defined, this will throw an `ArgumentError`.
"""
mapreduce_empty(f, op, T) = _empty_reduce_error()
mapreduce_empty(::typeof(identity), op, T) = reduce_empty(op, T)
mapreduce_empty(::typeof(abs), op, T)      = abs(reduce_empty(op, T))
mapreduce_empty(::typeof(abs2), op, T)     = abs2(reduce_empty(op, T))

mapreduce_empty(f::typeof(abs),  ::typeof(max), T) = abs(zero(T))
mapreduce_empty(f::typeof(abs2), ::typeof(max), T) = abs2(zero(T))

mapreduce_empty_iter(f, op, itr, ::HasEltype) = mapreduce_empty(f, op, eltype(itr))
mapreduce_empty_iter(f, op::typeof(&), itr, ::EltypeUnknown) = true
mapreduce_empty_iter(f, op::typeof(|), itr, ::EltypeUnknown) = false
mapreduce_empty_iter(f, op, itr, ::EltypeUnknown) = _empty_reduce_error()

# handling of single-element iterators
"""
    Base.reduce_first(op, x)

The value to be returned when calling [`reduce`](@ref), [`foldl`](@ref`) or
[`foldr`](@ref) with reduction `op` over an iterator which contains a single element
`x`. This value may also used to initialise the recursion, so that `reduce(op, [x, y])`
may call `op(reduce_first(op, x), y)`.

The default is `x` for most types. The main purpose is to ensure type stability, so
additional methods should only be defined for cases where `op` gives a result with
different types than its inputs.
"""
reduce_first(op, x) = x
reduce_first(::typeof(+), x::Bool) = Int(x)
reduce_first(::typeof(*), x::AbstractChar) = string(x)


"""
    Base.mapreduce_first(f, op, x)

The value to be returned when calling [`mapreduce`](@ref), [`mapfoldl`](@ref`) or
[`mapfoldr`](@ref) with map `f` and reduction `op` over an iterator which contains a
single element `x`. This value may also used to initialise the recursion, so that
`mapreduce(f, op, [x, y])` may call `op(reduce_first(op, f, x), f(y))`.

The default is `reduce_first(op, f(x))`.
"""
mapreduce_first(f, op, x) = reduce_first(op, f(x))

_mapreduce(f, op, A::AbstractArray) = _mapreduce(f, op, IndexStyle(A), A)

function _mapreduce(f, op, ::IndexLinear, A::AbstractArray{T}) where T
    inds = LinearIndices(A)
    n = length(inds)
    if n == 0
        return mapreduce_empty(f, op, T)
    elseif n == 1
        @inbounds a1 = A[first(inds)]
        return mapreduce_first(f, op, a1)
    elseif n < 16 # process short array here, avoid mapreduce_impl() compilation
        @inbounds i = first(inds)
        @inbounds a1 = A[i]
        @inbounds a2 = A[i+=1]
        s = op(f(a1), f(a2))
        while i < last(inds)
            @inbounds Ai = A[i+=1]
            s = op(s, f(Ai))
        end
        return s
    else
        return mapreduce_impl(f, op, A, first(inds), last(inds))
    end
end

mapreduce(f, op, a::Number) = mapreduce_first(f, op, a)

_mapreduce(f, op, ::IndexCartesian, A::AbstractArray) = mapfoldl(f, op, A)

"""
    reduce(op, itr; [init])

Reduce the given collection `itr` with the given binary operator `op`. If provided, the
initial value `init` must be a neutral element for `op` that will be returned for empty
collections. It is unspecified whether `init` is used for non-empty collections.

For empty collections, providing `init` will be necessary, except for some special cases
(e.g. when `op` is one of `+`, `*`, `max`, `min`, `&`, `|`) when Julia can determine the
neutral element of `op`.

Reductions for certain commonly-used operators may have special implementations, and
should be used instead: `maximum(itr)`, `minimum(itr)`, `sum(itr)`, `prod(itr)`,
 `any(itr)`, `all(itr)`.

The associativity of the reduction is implementation dependent. This means that you can't
use non-associative operations like `-` because it is undefined whether `reduce(-,[1,2,3])`
should be evaluated as `(1-2)-3` or `1-(2-3)`. Use [`foldl`](@ref) or
[`foldr`](@ref) instead for guaranteed left or right associativity.

Some operations accumulate error. Parallelism will be easier if the reduction can be
executed in groups. Future versions of Julia might change the algorithm. Note that the
elements are not reordered if you use an ordered collection.

# Examples
```jldoctest
julia> reduce(*, [2; 3; 4])
24

julia> reduce(*, [2; 3; 4]; init=-1)
-24
```
"""
reduce(op, itr; kw...) = mapreduce(identity, op, itr; kw...)

reduce(op, a::Number) = a  # Do we want this?
