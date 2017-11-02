# This file is a part of Julia. License is MIT: https://julialang.org/license

## reductions ##

###### Generic (map)reduce functions ######

if Int === Int32
    const SmallSigned = Union{Int8,Int16}
    const SmallUnsigned = Union{UInt8,UInt16}
else
    const SmallSigned = Union{Int8,Int16,Int32}
    const SmallUnsigned = Union{UInt8,UInt16,UInt32}
end

# Certain reductions like sum and prod may wish to promote the items being reduced over to
# an appropriate size. Note we need x + zero(x) because some types like Bool have their sum
# lie in a larger type.
promote_sys_size(T::Type) = T
promote_sys_size(::Type{<:SmallSigned}) = Int
promote_sys_size(::Type{<:SmallUnsigned}) = UInt

promote_sys_size_add(x) = convert(promote_sys_size(typeof(x + zero(x))), x)
promote_sys_size_mul(x) = convert(promote_sys_size(typeof(x * one(x))), x)
const _PromoteSysSizeFunction = Union{typeof(promote_sys_size_add),
                                      typeof(promote_sys_size_mul)}

## foldl && mapfoldl

@noinline function mapfoldl_impl(f, op, v0, itr, i)
    # Unroll the while loop once; if v0 is known, the call to op may
    # be evaluated at compile time
    if done(itr, i)
        return v0
    else
        (x, i) = next(itr, i)
        v = op(v0, f(x))
        while !done(itr, i)
            @inbounds (x, i) = next(itr, i)
            v = op(v, f(x))
        end
        return v
    end
end

"""
    mapfoldl(f, op, v0, itr)

Like [`mapreduce`](@ref), but with guaranteed left associativity, as in [`foldl`](@ref).
`v0` will be used exactly once.
"""
mapfoldl(f, op, v0, itr) = mapfoldl_impl(f, op, v0, itr, start(itr))

"""
    mapfoldl(f, op, itr)

Like `mapfoldl(f, op, v0, itr)`, but using the first element of `itr` to generate `v0`.
Specifically, `mapfoldl(f, op, itr)` produces the same result as
`mapfoldl(f, op, f(first(itr)), drop(itr, 1))`.
In general, this cannot be used with empty collections (see [`reduce(op, itr)`](@ref)).
"""
function mapfoldl(f, op, itr)
    i = start(itr)
    if done(itr, i)
        return Base.mapreduce_empty_iter(f, op, itr, iteratoreltype(itr))
    end
    (x, i) = next(itr, i)
    v0 = f(x)
    mapfoldl_impl(f, op, v0, itr, i)
end

"""
    foldl(op, v0, itr)

Like [`reduce`](@ref), but with guaranteed left associativity. `v0` will be used
exactly once.

```jldoctest
julia> foldl(-, 1, 2:5)
-13
```
"""
foldl(op, v0, itr) = mapfoldl(identity, op, v0, itr)

"""
    foldl(op, itr)

Like `foldl(op, v0, itr)`, but using the first element of `itr` as `v0`. In general, this
cannot be used with empty collections (see [`reduce(op, itr)`](@ref)).

```jldoctest
julia> foldl(-, 2:5)
-10
```
"""
foldl(op, itr) = mapfoldl(identity, op, itr)

## foldr & mapfoldr

function mapfoldr_impl(f, op, v0, itr, i::Integer)
    # Unroll the while loop once; if v0 is known, the call to op may
    # be evaluated at compile time
    if isempty(itr) || i == 0
        return v0
    else
        x = itr[i]
        v  = op(f(x), v0)
        while i > 1
            x = itr[i -= 1]
            v = op(f(x), v)
        end
        return v
    end
end

"""
    mapfoldr(f, op, v0, itr)

Like [`mapreduce`](@ref), but with guaranteed right associativity, as in [`foldr`](@ref).
`v0` will be used exactly once.
"""
mapfoldr(f, op, v0, itr) = mapfoldr_impl(f, op, v0, itr, endof(itr))

"""
    mapfoldr(f, op, itr)

Like `mapfoldr(f, op, v0, itr)`, but using the first element of `itr` to generate `v0`.
Specifically, `mapfoldr(f, op, itr)` produces the same result as
`mapfoldr(f, op, f(last(itr)), take(itr, length(itr)-1))`.
In general, this cannot be used with empty collections (see [`reduce(op, itr)`](@ref)).
"""
function mapfoldr(f, op, itr)
    i = endof(itr)
    if isempty(itr)
        return Base.mapreduce_empty_iter(f, op, itr, iteratoreltype(itr))
    end
    return mapfoldr_impl(f, op, f(itr[i]), itr, i-1)
end

"""
    foldr(op, v0, itr)

Like [`reduce`](@ref), but with guaranteed right associativity. `v0` will be used
exactly once.

```jldoctest
julia> foldr(-, 1, 2:5)
-1
```
"""
foldr(op, v0, itr) = mapfoldr(identity, op, v0, itr)

"""
    foldr(op, itr)

Like `foldr(op, v0, itr)`, but using the last element of `itr` as `v0`. In general, this
cannot be used with empty collections (see [`reduce(op, itr)`](@ref)).

```jldoctest
julia> foldr(-, 2:5)
-2
```
"""
foldr(op, itr) = mapfoldr(identity, op, itr)

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
        return f(a1)
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
    mapreduce(f, op, itr)

Like `mapreduce(f, op, v0, itr)`. In general, this cannot be used with empty collections
(see `reduce(op, itr)`).
"""
mapreduce(f, op, itr) = mapfoldl(f, op, itr)

"""
    mapreduce(f, op, v0, itr)

Apply function `f` to each element in `itr`, and then reduce the result using the binary
function `op`. `v0` must be a neutral element for `op` that will be returned for empty
collections. It is unspecified whether `v0` is used for non-empty collections.

[`mapreduce`](@ref) is functionally equivalent to calling `reduce(op, v0,
map(f, itr))`, but will in general execute faster since no intermediate collection needs to
be created. See documentation for [`reduce`](@ref) and [`map`](@ref).

```jldoctest
julia> mapreduce(x->x^2, +, [1:3;]) # == 1 + 4 + 9
14
```

The associativity of the reduction is implementation-dependent. Additionally, some
implementations may reuse the return value of `f` for elements that appear multiple times in
`itr`. Use [`mapfoldl`](@ref) or [`mapfoldr`](@ref) instead for
guaranteed left or right associativity and invocation of `f` for every value.
"""
mapreduce(f, op, v0, itr) = mapfoldl(f, op, v0, itr)

# Note: sum_seq usually uses four or more accumulators after partial
# unrolling, so each accumulator gets at most 256 numbers
pairwise_blocksize(f, op) = 1024

# This combination appears to show a benefit from a larger block size
pairwise_blocksize(::typeof(abs2), ::typeof(+)) = 4096


# handling empty arrays
_empty_reduce_error() = throw(ArgumentError("reducing over an empty collection is not allowed"))
reduce_empty(op, T) = _empty_reduce_error()
reduce_empty(::typeof(+), T) = zero(T)
reduce_empty(::typeof(*), T) = one(T)
reduce_empty(::typeof(&), ::Type{Bool}) = true
reduce_empty(::typeof(|), ::Type{Bool}) = false

mapreduce_empty(f, op, T) = _empty_reduce_error()
mapreduce_empty(::typeof(identity), op, T) = reduce_empty(op, T)
mapreduce_empty(f::_PromoteSysSizeFunction, op, T) =
    f(mapreduce_empty(identity, op, T))
mapreduce_empty(::typeof(abs), ::typeof(+), T) = abs(zero(T))
mapreduce_empty(::typeof(abs2), ::typeof(+), T) = abs2(zero(T))
mapreduce_empty(::typeof(abs), ::Union{typeof(scalarmax), typeof(max)}, T) =
    abs(zero(T))
mapreduce_empty(::typeof(abs2), ::Union{typeof(scalarmax), typeof(max)}, T) =
    abs2(zero(T))

# Allow mapreduce_empty to “see through” promote_sys_size
let ComposedFunction = typename(typeof(identity ∘ identity)).wrapper
    global mapreduce_empty(f::ComposedFunction{<:_PromoteSysSizeFunction}, op, T) =
        f.f(mapreduce_empty(f.g, op, T))
end

mapreduce_empty_iter(f, op, itr, ::HasEltype) = mapreduce_empty(f, op, eltype(itr))
mapreduce_empty_iter(f, op::typeof(&), itr, ::EltypeUnknown) = true
mapreduce_empty_iter(f, op::typeof(|), itr, ::EltypeUnknown) = false
mapreduce_empty_iter(f, op, itr, ::EltypeUnknown) = _empty_reduce_error()

_mapreduce(f, op, A::AbstractArray) = _mapreduce(f, op, IndexStyle(A), A)

function _mapreduce(f, op, ::IndexLinear, A::AbstractArray{T}) where T
    inds = linearindices(A)
    n = length(inds)
    if n == 0
        return mapreduce_empty(f, op, T)
    elseif n == 1
        @inbounds a1 = A[inds[1]]
        return f(a1)
    elseif n < 16 # process short array here, avoid mapreduce_impl() compilation
        @inbounds i = inds[1]
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

_mapreduce(f, op, ::IndexCartesian, A::AbstractArray) = mapfoldl(f, op, A)

mapreduce(f, op, A::AbstractArray) = _mapreduce(f, op, IndexStyle(A), A)
mapreduce(f, op, a::Number) = f(a)

"""
    reduce(op, v0, itr)

Reduce the given collection `itr` with the given binary operator `op`. `v0` must be a
neutral element for `op` that will be returned for empty collections. It is unspecified
whether `v0` is used for non-empty collections.

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
julia> reduce(*, 1, [2; 3; 4])
24
```
"""
reduce(op, v0, itr) = mapreduce(identity, op, v0, itr)

"""
    reduce(op, itr)

Like `reduce(op, v0, itr)`. This cannot be used with empty collections, except for some
special cases (e.g. when `op` is one of `+`, `*`, `max`, `min`, `&`, `|`) when Julia can
determine the neutral element of `op`.

```jldoctest
julia> reduce(*, [2; 3; 4])
24
```
"""
reduce(op, itr) = mapreduce(identity, op, itr)
reduce(op, a::Number) = a

###### Specific reduction functions ######

## sum

"""
    sum(f, itr)

Sum the results of calling function `f` on each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

```jldoctest
julia> sum(abs2, [2; 3; 4])
29
```

Note the important difference between `sum(A)` and `reduce(+, A)` for arrays
with small integer eltype:

```jldoctest
julia> sum(Int8[100, 28])
128

julia> reduce(+, Int8[100, 28])
-128
```

In the former case, the integers are widened to system word size and therefore
the result is 128. In the latter case, no such widening happens and integer
overflow results in -128.
"""
sum(f::Callable, a) = mapreduce(promote_sys_size_add ∘ f, +, a)

"""
    sum(itr)

Returns the sum of all elements in a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

```jldoctest
julia> sum(1:20)
210
```
"""
sum(a) = mapreduce(promote_sys_size_add, +, a)
sum(a::AbstractArray{Bool}) = count(a)


# Kahan (compensated) summation: O(1) error growth, at the expense
# of a considerable increase in computational expense.

"""
    sum_kbn(A)

Returns the sum of all elements of `A`, using the Kahan-Babuska-Neumaier compensated
summation algorithm for additional accuracy.
"""
function sum_kbn(A)
    T = @default_eltype(A)
    c = promote_sys_size_add(zero(T)::T)
    i = start(A)
    if done(A, i)
        return c
    end
    Ai, i = next(A, i)
    s = Ai - c
    while !(done(A, i))
        Ai, i = next(A, i)
        t = s + Ai
        if abs(s) >= abs(Ai)
            c -= ((s-t) + Ai)
        else
            c -= ((Ai-t) + s)
        end
        s = t
    end
    s - c
end

## prod
"""
    prod(f, itr)

Returns the product of `f` applied to each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

```jldoctest
julia> prod(abs2, [2; 3; 4])
576
```
"""
prod(f::Callable, a) = mapreduce(promote_sys_size_mul ∘ f, *, a)

"""
    prod(itr)

Returns the product of all elements of a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

```jldoctest
julia> prod(1:20)
2432902008176640000
```
"""
prod(a) = mapreduce(promote_sys_size_mul, *, a)

## maximum & minimum

function mapreduce_impl(f, op::Union{typeof(scalarmax),
                                     typeof(scalarmin),
                                     typeof(max),
                                     typeof(min)},
                        A::AbstractArray, first::Int, last::Int)
    # locate the first non NaN number
    @inbounds a1 = A[first]
    v = f(a1)
    i = first + 1
    while (v == v) && (i <= last)
        @inbounds ai = A[i]
        v = op(v, f(ai))
        i += 1
    end
    v
end

maximum(f::Callable, a) = mapreduce(f, scalarmax, a)
minimum(f::Callable, a) = mapreduce(f, scalarmin, a)

"""
    maximum(itr)

Returns the largest element in a collection.

```jldoctest
julia> maximum(-20.5:10)
9.5

julia> maximum([1,2,3])
3
```
"""
maximum(a) = mapreduce(identity, scalarmax, a)

"""
    minimum(itr)

Returns the smallest element in a collection.

```jldoctest
julia> minimum(-20.5:10)
-20.5

julia> minimum([1,2,3])
1
```
"""
minimum(a) = mapreduce(identity, scalarmin, a)

## extrema

extrema(r::AbstractRange) = (minimum(r), maximum(r))
extrema(x::Real) = (x, x)

"""
    extrema(itr) -> Tuple

Compute both the minimum and maximum element in a single pass, and return them as a 2-tuple.

```jldoctest
julia> extrema(2:10)
(2, 10)

julia> extrema([9,pi,4.5])
(3.141592653589793, 9.0)
```
"""
function extrema(itr)
    s = start(itr)
    done(itr, s) && throw(ArgumentError("collection must be non-empty"))
    (v, s) = next(itr, s)
    vmin = vmax = v
    while !done(itr, s)
        (x, s) = next(itr, s)
        vmax = max(x, vmax)
        vmin = min(x, vmin)
    end
    return (vmin, vmax)
end

## all & any

"""
    any(itr) -> Bool

Test whether any elements of a boolean collection are `true`, returning `true` as
soon as the first `true` value in `itr` is encountered (short-circuiting).

```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
  true
 false
 false
  true

julia> any(a)
true

julia> any((println(i); v) for (i, v) in enumerate(a))
1
true
```
"""
any(itr) = any(identity, itr)

"""
    all(itr) -> Bool

Test whether all elements of a boolean collection are `true`, returning `false` as
soon as the first `false` value in `itr` is encountered (short-circuiting).

```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
  true
 false
 false
  true

julia> all(a)
false

julia> all((println(i); v) for (i, v) in enumerate(a))
1
2
false
```
"""
all(itr) = all(identity, itr)

"""
    any(p, itr) -> Bool

Determine whether predicate `p` returns `true` for any elements of `itr`, returning
`true` as soon as the first item in `itr` for which `p` returns `true` is encountered
(short-circuiting).

```jldoctest
julia> any(i->(4<=i<=6), [3,5,7])
true

julia> any(i -> (println(i); i > 3), 1:10)
1
2
3
4
true
```
"""
function any(f, itr)
    for x in itr
        f(x) && return true
    end
    return false
end

"""
    all(p, itr) -> Bool

Determine whether predicate `p` returns `true` for all elements of `itr`, returning
`false` as soon as the first item in `itr` for which `p` returns `false` is encountered
(short-circuiting).

```jldoctest
julia> all(i->(4<=i<=6), [4,5,6])
true

julia> all(i -> (println(i); i < 3), 1:10)
1
2
3
false
```
"""
function all(f, itr)
    for x in itr
        f(x) || return false
    end
    return true
end

## in & contains

"""
    in(item, collection) -> Bool
    ∈(item,collection) -> Bool
    ∋(collection,item) -> Bool
    ∉(item,collection) -> Bool
    ∌(collection,item) -> Bool

Determine whether an item is in the given collection, in the sense that it is `==` to one of
the values generated by iterating over the collection. Some collections need a slightly
different definition; for example [`Set`](@ref)s check whether the item
[`isequal`](@ref) to one of the elements. [`Dict`](@ref)s look for
`(key,value)` pairs, and the key is compared using [`isequal`](@ref). To test for
the presence of a key in a dictionary, use [`haskey`](@ref) or `k in keys(dict)`.

```jldoctest
julia> a = 1:3:20
1:3:19

julia> 4 in a
true

julia> 5 in a
false
```
"""
in(x, itr) = any(y -> y == x, itr)

const ∈ = in
∉(x, itr)=!∈(x, itr)
∋(itr, x)= ∈(x, itr)
∌(itr, x)=!∋(itr, x)


## count

"""
    count(p, itr) -> Integer
    count(itr) -> Integer

Count the number of elements in `itr` for which predicate `p` returns `true`.
If `p` is omitted, counts the number of `true` elements in `itr` (which
should be a collection of boolean values).

```jldoctest
julia> count(i->(4<=i<=6), [2,3,4,5,6])
3

julia> count([true, false, true, true])
3
```
"""
function count(pred, itr)
    n = 0
    for x in itr
        n += pred(x)::Bool
    end
    return n
end
function count(pred, a::AbstractArray)
    n = 0
    for i in eachindex(a)
        @inbounds n += pred(a[i])::Bool
    end
    return n
end
count(itr) = count(identity, itr)
