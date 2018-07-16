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

"""
    Base.add_sum(x,y)

The reduction operator used in `sum`. The main difference from [`+`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
add_sum(x,y) = x + y
add_sum(x::SmallSigned,y::SmallSigned) = Int(x) + Int(y)
add_sum(x::SmallUnsigned,y::SmallUnsigned) = UInt(x) + UInt(y)

"""
    Base.mul_prod(x,y)

The reduction operator used in `prod`. The main difference from [`*`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
mul_prod(x,y) = x * y
mul_prod(x::SmallSigned,y::SmallSigned) = Int(x) * Int(y)
mul_prod(x::SmallUnsigned,y::SmallUnsigned) = UInt(x) * UInt(y)

## foldl && mapfoldl

@noinline function mapfoldl_impl(f, op, nt::NamedTuple{(:init,)}, itr, i...)
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
    (x, i) = y
    init = mapreduce_first(f, op, x)
    mapfoldl_impl(f, op, (init=init,), itr, i)
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
((1=>2)=>3) => 4

julia> foldl(=>, 1:4; init=0)
(((0=>1)=>2)=>3) => 4
```
"""
foldl(op, itr; kw...) = mapfoldl(identity, op, itr; kw...)

## foldr & mapfoldr

function mapfoldr_impl(f, op, nt::NamedTuple{(:init,)}, itr, i::Integer)
    init = nt.init
    # Unroll the while loop once; if init is known, the call to op may
    # be evaluated at compile time
    if isempty(itr) || i == 0
        return init
    else
        x = itr[i]
        v  = op(f(x), init)
        while i > 1
            x = itr[i -= 1]
            v = op(f(x), v)
        end
        return v
    end
end

function mapfoldr_impl(f, op, ::NamedTuple{()}, itr, i::Integer)
    if isempty(itr)
        return Base.mapreduce_empty_iter(f, op, itr, IteratorEltype(itr))
    end
    return mapfoldr_impl(f, op, (init=mapreduce_first(f, op, itr[i]),), itr, i-1)
end

"""
    mapfoldr(f, op, itr; [init])

Like [`mapreduce`](@ref), but with guaranteed right associativity, as in [`foldr`](@ref). If
provided, the keyword argument `init` will be used exactly once. In general, it will be
necessary to provide `init` to work with empty collections.
"""
mapfoldr(f, op, itr; kw...) = mapfoldr_impl(f, op, kw.data, itr, lastindex(itr))


"""
    foldr(op, itr; [init])

Like [`reduce`](@ref), but with guaranteed right associativity. If provided, the keyword
argument `init` will be used exactly once. In general, it will be necessary to provide
`init` to work with empty collections.

# Examples
```jldoctest
julia> foldr(=>, 1:4)
1 => (2=>(3=>4))

julia> foldr(=>, 1:4; init=0)
1 => (2=>(3=>(4=>0)))
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
    mapreduce(f, op, itr; [init])

Apply function `f` to each element in `itr`, and then reduce the result using the binary
function `op`. If provided, `init` must be a neutral element for `op` that will be returne
for empty collections. It is unspecified whether `init` is used for non-empty collections.
In general, it will be necessary to provide `init` to work with empty collections.

[`mapreduce`](@ref) is functionally equivalent to calling
`reduce(op, map(f, itr); init=init)`, but will in general execute faster since no
intermediate collection needs to be created. See documentation for [`reduce`](@ref) and
[`map`](@ref).

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

reduce_empty(::typeof(add_sum), T) = reduce_empty(+, T)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:SmallSigned}  = zero(Int)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:SmallUnsigned} = zero(UInt)
reduce_empty(::typeof(mul_prod), T) = reduce_empty(*, T)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:SmallSigned}  = one(Int)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:SmallUnsigned} = one(UInt)

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

reduce_first(::typeof(add_sum), x) = reduce_first(+, x)
reduce_first(::typeof(add_sum), x::SmallSigned)   = Int(x)
reduce_first(::typeof(add_sum), x::SmallUnsigned) = UInt(x)
reduce_first(::typeof(mul_prod), x) = reduce_first(*, x)
reduce_first(::typeof(mul_prod), x::SmallSigned)   = Int(x)
reduce_first(::typeof(mul_prod), x::SmallUnsigned) = UInt(x)

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

###### Specific reduction functions ######

## sum

"""
    sum(f, itr)

Sum the results of calling function `f` on each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
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
sum(f, a) = mapreduce(f, add_sum, a)

"""
    sum(itr)

Returns the sum of all elements in a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
```jldoctest
julia> sum(1:20)
210
```
"""
sum(a) = sum(identity, a)
sum(a::AbstractArray{Bool}) = count(a)

## prod
"""
    prod(f, itr)

Returns the product of `f` applied to each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
```jldoctest
julia> prod(abs2, [2; 3; 4])
576
```
"""
prod(f, a) = mapreduce(f, mul_prod, a)

"""
    prod(itr)

Returns the product of all elements of a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
```jldoctest
julia> prod(1:20)
2432902008176640000
```
"""
prod(a) = mapreduce(identity, mul_prod, a)

## maximum & minimum

function mapreduce_impl(f, op::Union{typeof(max), typeof(min)},
                        A::AbstractArray, first::Int, last::Int)
    # locate the first non NaN number
    @inbounds a1 = A[first]
    v = mapreduce_first(f, op, a1)
    i = first + 1
    while (v == v) && (i <= last)
        @inbounds ai = A[i]
        v = op(v, f(ai))
        i += 1
    end
    v
end

maximum(f::Callable, a) = mapreduce(f, max, a)
minimum(f::Callable, a) = mapreduce(f, min, a)

"""
    maximum(itr)

Returns the largest element in a collection.

# Examples
```jldoctest
julia> maximum(-20.5:10)
9.5

julia> maximum([1,2,3])
3
```
"""
maximum(a) = mapreduce(identity, max, a)

"""
    minimum(itr)

Returns the smallest element in a collection.

# Examples
```jldoctest
julia> minimum(-20.5:10)
-20.5

julia> minimum([1,2,3])
1
```
"""
minimum(a) = mapreduce(identity, min, a)

## all & any

"""
    any(itr) -> Bool

Test whether any elements of a boolean collection are `true`, returning `true` as
soon as the first `true` value in `itr` is encountered (short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
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

julia> any([missing, true])
true

julia> any([false, missing])
missing
```
"""
any(itr) = any(identity, itr)

"""
    all(itr) -> Bool

Test whether all elements of a boolean collection are `true`, returning `false` as
soon as the first `false` value in `itr` is encountered (short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
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

julia> all([missing, false])
false

julia> all([true, missing])
missing
```
"""
all(itr) = all(identity, itr)

"""
    any(p, itr) -> Bool

Determine whether predicate `p` returns `true` for any elements of `itr`, returning
`true` as soon as the first item in `itr` for which `p` returns `true` is encountered
(short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> any(i->(4<=i<=6), [3,5,7])
true

julia> any(i -> (println(i); i > 3), 1:10)
1
2
3
4
true

julia> any(i -> i > 0, [1, missing])
true

julia> any(i -> i > 0, [-1, missing])
missing

julia> any(i -> i > 0, [-1, 0])
false
```
"""
any(f, itr) = _any(f, itr, :)

function _any(f, itr, ::Colon)
    anymissing = false
    for x in itr
        v = f(x)
        if ismissing(v)
            anymissing = true
        elseif v
            return true
        end
    end
    return anymissing ? missing : false
end

"""
    all(p, itr) -> Bool

Determine whether predicate `p` returns `true` for all elements of `itr`, returning
`false` as soon as the first item in `itr` for which `p` returns `false` is encountered
(short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> all(i->(4<=i<=6), [4,5,6])
true

julia> all(i -> (println(i); i < 3), 1:10)
1
2
3
false

julia> all(i -> i > 0, [1, missing])
missing

julia> all(i -> i > 0, [-1, missing])
false

julia> all(i -> i > 0, [1, 2])
true
```
"""
all(f, itr) = _all(f, itr, :)

function _all(f, itr, ::Colon)
    anymissing = false
    for x in itr
        v = f(x)
        if ismissing(v)
            anymissing = true
        # this syntax allows throwing a TypeError for non-Bool, for consistency with any
        elseif v
            continue
        else
            return false
        end
    end
    return anymissing ? missing : true
end

## count

"""
    count(p, itr) -> Integer
    count(itr) -> Integer

Count the number of elements in `itr` for which predicate `p` returns `true`.
If `p` is omitted, counts the number of `true` elements in `itr` (which
should be a collection of boolean values).

# Examples
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
