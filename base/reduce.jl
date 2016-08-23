# This file is a part of Julia. License is MIT: http://julialang.org/license

## reductions ##

###### Generic (map)reduce functions ######

if Int === Int32
typealias SmallSigned Union{Int8,Int16}
typealias SmallUnsigned Union{UInt8,UInt16}
else
typealias SmallSigned Union{Int8,Int16,Int32}
typealias SmallUnsigned Union{UInt8,UInt16,UInt32}
end

typealias CommonReduceResult Union{UInt64,UInt128,Int64,Int128,Float32,Float64}
typealias WidenReduceResult Union{SmallSigned, SmallUnsigned, Float16}

# r_promote: promote x to the type of reduce(op, [x])
r_promote(op, x::WidenReduceResult) = widen(x)
r_promote(op, x) = x
r_promote(::typeof(+), x::WidenReduceResult) = widen(x)
r_promote(::typeof(*), x::WidenReduceResult) = widen(x)
r_promote(::typeof(+), x::Number) = oftype(x + zero(x), x)
r_promote(::typeof(*), x::Number) = oftype(x * one(x), x)
r_promote(::typeof(+), x) = x
r_promote(::typeof(*), x) = x
r_promote(::typeof(scalarmax), x::WidenReduceResult) = x
r_promote(::typeof(scalarmin), x::WidenReduceResult) = x
r_promote(::typeof(scalarmax), x) = x
r_promote(::typeof(scalarmin), x) = x
r_promote(::typeof(max), x::WidenReduceResult) = r_promote(scalarmax, x)
r_promote(::typeof(min), x::WidenReduceResult) = r_promote(scalarmin, x)
r_promote(::typeof(max), x) = r_promote(scalarmax, x)
r_promote(::typeof(min), x) = r_promote(scalarmin, x)


## foldl && mapfoldl

function mapfoldl_impl(f, op, v0, itr, i)
    # Unroll the while loop once; if v0 is known, the call to op may
    # be evaluated at compile time
    if done(itr, i)
        return r_promote(op, v0)
    else
        (x, i) = next(itr, i)
        v = op(r_promote(op, v0), f(x))
        while !done(itr, i)
            (x, i) = next(itr, i)
            v = op(v, f(x))
        end
        return v
    end
end

"""
    mapfoldl(f, op, v0, itr)

Like [`mapreduce`](:func:`mapreduce`), but with guaranteed left associativity. `v0` will be
used exactly once.
"""
mapfoldl(f, op, v0, itr) = mapfoldl_impl(f, op, v0, itr, start(itr))

"""
    mapfoldl(f, op, itr)

Like `mapfoldl(f, op, v0, itr)`, but using the first element of `itr` as `v0`. In general,
this cannot be used with empty collections (see `reduce(op, itr)`).
"""
function mapfoldl(f, op, itr)
    i = start(itr)
    if done(itr, i)
        return Base.mr_empty(f, op, eltype(itr))
    end
    (x, i) = next(itr, i)
    v0 = f(x)
    mapfoldl_impl(f, op, v0, itr, i)
end

"""
    foldl(op, v0, itr)

Like [`reduce`](:func:`reduce`), but with guaranteed left associativity. `v0` will be used
exactly once.
"""
foldl(op, v0, itr) = mapfoldl(identity, op, v0, itr)

"""
    foldl(op, itr)

Like `foldl(op, v0, itr)`, but using the first element of `itr` as `v0`. In general, this
cannot be used with empty collections (see `reduce(op, itr)`).
"""
foldl(op, itr) = mapfoldl(identity, op, itr)

## foldr & mapfoldr

function mapfoldr_impl(f, op, v0, itr, i::Integer)
    # Unroll the while loop once; if v0 is known, the call to op may
    # be evaluated at compile time
    if i == 0
        return r_promote(op, v0)
    else
        x = itr[i]
        v  = op(f(x), r_promote(op, v0))
        while i > 1
            x = itr[i -= 1]
            v = op(f(x), v)
        end
        return v
    end
end

"""
    mapfoldr(f, op, v0, itr)

Like [`mapreduce`](:func:`mapreduce`), but with guaranteed right associativity. `v0` will be
used exactly once.
"""
mapfoldr(f, op, v0, itr) = mapfoldr_impl(f, op, v0, itr, endof(itr))

"""
    mapfoldr(f, op, itr)

Like `mapfoldr(f, op, v0, itr)`, but using the first element of `itr` as `v0`. In general,
this cannot be used with empty collections (see `reduce(op, itr)`).
"""
mapfoldr(f, op, itr) = (i = endof(itr); mapfoldr_impl(f, op, f(itr[i]), itr, i-1))

"""
    foldr(op, v0, itr)

Like [`reduce`](:func:`reduce`), but with guaranteed right associativity. `v0` will be used
exactly once.
"""
foldr(op, v0, itr) = mapfoldr(identity, op, v0, itr)

"""
    foldr(op, itr)

Like `foldr(op, v0, itr)`, but using the last element of `itr` as `v0`. In general, this
cannot be used with empty collections (see `reduce(op, itr)`).
"""
foldr(op, itr) = mapfoldr(identity, op, itr)

## reduce & mapreduce

function mapreduce_impl(f, op, A::AbstractArray, ifirst::Integer, ilast::Integer, blksize::Int=pairwise_blocksize(f, op))
    if ifirst + blksize > ilast
        # sequential portion
        fx1 = r_promote(op, f(A[ifirst]))
        fx2 = r_promote(op, f(A[ifirst + 1]))
        v = op(fx1, fx2)
        @simd for i = ifirst + 2 : ilast
            @inbounds Ai = A[i]
            v = op(v, f(Ai))
        end
        return v
    else
        # pairwise portion
        imid = (ifirst + ilast) >>> 1
        v1 = mapreduce_impl(f, op, A, ifirst, imid, blksize)
        v2 = mapreduce_impl(f, op, A, imid+1, ilast, blksize)
        return op(v1, v2)
    end
end

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

[`mapreduce`](:func:`mapreduce`) is functionally equivalent to calling `reduce(op, v0,
map(f, itr))`, but will in general execute faster since no intermediate collection needs to
be created. See documentation for [`reduce`](:func:`reduce`) and [`map`](:func:`map`).

```jldoctest
julia> mapreduce(x->x^2, +, [1:3;]) # == 1 + 4 + 9
14
```

The associativity of the reduction is implementation-dependent. Additionally, some
implementations may reuse the return value of `f` for elements that appear multiple times in
`itr`. Use [`mapfoldl`](:func:`mapfoldl`) or [`mapfoldr`](:func:`mapfoldr`) instead for
guaranteed left or right associativity and invocation of `f` for every value.
"""
mapreduce(f, op, v0, itr) = mapfoldl(f, op, v0, itr)

# Note: sum_seq usually uses four or more accumulators after partial
# unrolling, so each accumulator gets at most 256 numbers
pairwise_blocksize(f, op) = 1024

# This combination appears to show a benefit from a larger block size
pairwise_blocksize(::typeof(abs2), ::typeof(+)) = 4096


# handling empty arrays
mr_empty(f, op, T) = throw(ArgumentError("reducing over an empty collection is not allowed"))
# use zero(T)::T to improve type information when zero(T) is not defined
mr_empty(::typeof(identity), op::typeof(+), T) = r_promote(op, zero(T)::T)
mr_empty(::typeof(abs), op::typeof(+), T) = r_promote(op, abs(zero(T)::T))
mr_empty(::typeof(abs2), op::typeof(+), T) = r_promote(op, abs2(zero(T)::T))
mr_empty(::typeof(identity), op::typeof(*), T) = r_promote(op, one(T)::T)
mr_empty(::typeof(abs), op::typeof(scalarmax), T) = abs(zero(T)::T)
mr_empty(::typeof(abs2), op::typeof(scalarmax), T) = abs2(zero(T)::T)
mr_empty(::typeof(abs), op::typeof(max), T) = mr_empty(abs, scalarmax, T)
mr_empty(::typeof(abs2), op::typeof(max), T) = mr_empty(abs2, scalarmax, T)
mr_empty(f, op::typeof(&), T) = true
mr_empty(f, op::typeof(|), T) = false

_mapreduce(f, op, A::AbstractArray) = _mapreduce(f, op, linearindexing(A), A)

function _mapreduce{T}(f, op, ::LinearFast, A::AbstractArray{T})
    inds = linearindices(A)
    n = length(inds)
    @inbounds begin
        if n == 0
            return mr_empty(f, op, T)
        elseif n == 1
            return r_promote(op, f(A[inds[1]]))
        elseif n < 16
            fx1 = r_promote(op, f(A[inds[1]]))
            fx2 = r_promote(op, f(A[inds[2]]))
            s = op(fx1, fx2)
            i = inds[2]
            while i < last(inds)
                Ai = A[i+=1]
                s = op(s, f(Ai))
            end
            return s
        else
            return mapreduce_impl(f, op, A, first(inds), last(inds))
        end
    end
end

_mapreduce{T}(f, op, ::LinearSlow, A::AbstractArray{T}) = mapfoldl(f, op, A)

mapreduce(f, op, A::AbstractArray) = _mapreduce(f, op, linearindexing(A), A)
mapreduce(f, op, a::Number) = f(a)

"""
    reduce(op, v0, itr)

Reduce the given collection `ìtr` with the given binary operator `op`. `v0` must be a
neutral element for `op` that will be returned for empty collections. It is unspecified
whether `v0` is used for non-empty collections.

Reductions for certain commonly-used operators have special implementations which should be
used instead: `maximum(itr)`, `minimum(itr)`, `sum(itr)`, `prod(itr)`, `any(itr)`,
`all(itr)`.

The associativity of the reduction is implementation dependent. This means that you can't
use non-associative operations like `-` because it is undefined whether `reduce(-,[1,2,3])`
should be evaluated as `(1-2)-3` or `1-(2-3)`. Use [`foldl`](:func:`foldl`) or
[`foldr`](:func:`foldr`) instead for guaranteed left or right associativity.

Some operations accumulate error, and parallelism will also be easier if the reduction can
be executed in groups. Future versions of Julia might change the algorithm. Note that the
elements are not reordered if you use an ordered collection.
"""
reduce(op, v0, itr) = mapreduce(identity, op, v0, itr)

"""
    reduce(op, itr)

Like `reduce(op, v0, itr)`. This cannot be used with empty collections, except for some
special cases (e.g. when `op` is one of `+`, `*`, `max`, `min`, `&`, `|`) when Julia can
determine the neutral element of `op`.
"""
reduce(op, itr) = mapreduce(identity, op, itr)
reduce(op, a::Number) = a

### short-circuiting specializations of mapreduce

## conditions and results of short-circuiting

immutable Predicate{F}
    f::F
end
(pred::Predicate)(x) = pred.f(x)::Bool

const ShortCircuiting = Union{typeof(&), typeof(|)}

## short-circuiting (sc) mapreduce definitions

function mapreduce_sc_impl(f, op::typeof(&), itr)
    for x in itr
        f(x) || return false
    end
    return true
end

function mapreduce_sc_impl(f, op::typeof(|), itr)
    for x in itr
        f(x) && return true
    end
    return false
end

# mapreduce_sc tests if short-circuiting is safe;
# if so, mapreduce_sc_impl is called. If it's not
# safe, call mapreduce_no_sc, which redirects to
# non-short-circuiting definitions.

mapreduce_no_sc(f, op, itr::Any)           =  mapfoldl(f, op, itr)
mapreduce_no_sc(f, op, itr::AbstractArray) = _mapreduce(f, op, itr)

mapreduce_sc(f::Function,  op, itr) = mapreduce_no_sc(f, op, itr)
mapreduce_sc(f::Predicate, op, itr) = mapreduce_sc_impl(f, op, itr)

mapreduce_sc(f::typeof(identity), op, itr) =
    eltype(itr) <: Bool ?
        mapreduce_sc_impl(f, op, itr) :
        mapreduce_no_sc(f, op, itr)

mapreduce(f, op::ShortCircuiting, n::Number) = n
mapreduce(f, op::ShortCircuiting, itr::AbstractArray) = mapreduce_sc(f,op,itr)
mapreduce(f, op::ShortCircuiting, itr::Any)           = mapreduce_sc(f,op,itr)


###### Specific reduction functions ######

## sum

"""
    sum(f, itr)

Sum the results of calling function `f` on each element of `itr`.
"""
sum(f::Callable, a) = mapreduce(f, +, a)

"""
    sum(itr)

Returns the sum of all elements in a collection.
"""
sum(a) = mapreduce(identity, +, a)
sum(a::AbstractArray{Bool}) = countnz(a)

"""
    sumabs(itr)

Sum absolute values of all elements in a collection. This is equivalent to `sum(abs(itr))` but faster.
"""
sumabs(a) = mapreduce(abs, +, a)

"""
    sumabs2(itr)

Sum squared absolute values of all elements in a collection.
This is equivalent to `sum(abs2(itr))` but faster.
"""
sumabs2(a) = mapreduce(abs2, +, a)

# Kahan (compensated) summation: O(1) error growth, at the expense
# of a considerable increase in computational expense.

"""
    sum_kbn(A)

Returns the sum of all array elements, using the Kahan-Babuska-Neumaier compensated
summation algorithm for additional accuracy.
"""
function sum_kbn{T<:AbstractFloat}(A::AbstractArray{T})
    c = r_promote(+, zero(T)::T)
    if isempty(A)
        return c
    end
    inds = linearindices(A)
    s = A[first(inds)] + c
    for i in first(inds)+1:last(inds)
        @inbounds Ai = A[i]
        t = s + Ai
        if abs(s) >= abs(Ai)
            c += ((s-t) + Ai)
        else
            c += ((Ai-t) + s)
        end
        s = t
    end
    s + c
end


## prod

prod(f::Callable, a) = mapreduce(f, *, a)

"""
    prod(itr)

Returns the product of all elements of a collection.
"""
prod(a) = mapreduce(identity, *, a)

## maximum & minimum

function mapreduce_impl(f, op::Union{typeof(scalarmax),
                                     typeof(scalarmin),
                                     typeof(max),
                                     typeof(min)},
                        A::AbstractArray, first::Int, last::Int)
    # locate the first non NaN number
    v = f(A[first])
    i = first + 1
    while v != v && i <= last
        @inbounds Ai = A[i]
        v = f(Ai)
        i += 1
    end
    while i <= last
        @inbounds Ai = A[i]
        x = f(Ai)
        v = op(v, x)
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

"""
    maxabs(itr)

Compute the maximum absolute value of a collection of values.

```jldoctest
julia> maxabs([-1, 3, 4*im])
4.0
```
"""
maxabs(a) = mapreduce(abs, scalarmax, a)

"""
    minabs(itr)

Compute the minimum absolute value of a collection of values.

```jldoctest
julia> minabs([-1, 3, 4*im])
1.0
```
"""
minabs(a) = mapreduce(abs, scalarmin, a)

## extrema

extrema(r::Range) = (minimum(r), maximum(r))
extrema(x::Real) = (x, x)

"""
    extrema(itr) -> Tuple

Compute both the minimum and maximum element in a single pass, and return them as a 2-tuple.

```jldoctest
julia> extrema(2:10)
(2,10)

julia> extrema([9,pi,4.5])
(3.141592653589793,9.0)
```
"""
function extrema(itr)
    s = start(itr)
    done(itr, s) && throw(ArgumentError("collection must be non-empty"))
    (v, s) = next(itr, s)
    while v != v && !done(itr, s)
        (x, s) = next(itr, s)
        v = x
    end
    vmin = v
    vmax = v
    while !done(itr, s)
        (x, s) = next(itr, s)
        if x > vmax
            vmax = x
        elseif x < vmin
            vmin = x
        end
    end
    return (vmin, vmax)
end

"""
    extrema(A,dims) -> Array{Tuple}

Compute the minimum and maximum elements of an array over the given dimensions.
"""
function extrema(A::AbstractArray, dims)
    sz = [size(A)...]
    sz[[dims...]] = 1
    B = Array{Tuple{eltype(A),eltype(A)}}(sz...)
    extrema!(B, A)
end

@generated function extrema!{T,N}(B, A::AbstractArray{T,N})
    quote
        sA = size(A)
        sB = size(B)
        @nloops $N i B begin
            AI = @nref $N A i
            (@nref $N B i) = (AI, AI)
        end
        Bmax = sB
        Istart = ones(Int,ndims(A))
        Istart[([sB...].==1) & ([sA...].!=1)] = 2
        @inbounds @nloops $N i d->(Istart[d]:size(A,d)) begin
            AI = @nref $N A i
            @nexprs $N d->(j_d = min(Bmax[d], i_{d}))
            BJ = @nref $N B j
            if AI < BJ[1]
                (@nref $N B j) = (AI, BJ[2])
            elseif AI > BJ[2]
                (@nref $N B j) = (BJ[1], AI)
            end
        end
        B
    end
end

## all & any

"""
    any(itr) -> Bool

Test whether any elements of a boolean collection are `true`.

```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
  true
 false
 false
  true

julia> any(a)
true
```
"""
any(itr) = any(identity, itr)

"""
    all(itr) -> Bool

Test whether all elements of a boolean collection are `true`.

```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
  true
 false
 false
  true

julia> all(a)
false
```
"""
all(itr) = all(identity, itr)

nonboolean_error(f, op) = throw(ArgumentError("""
    Using non-boolean collections with $f(itr) is not allowed, use
    reduce($op, itr) instead. If you are using $f(map(f, itr)) or
    $f([f(x) for x in itr]), use $f(f, itr) instead.
"""))
or_bool_only(a, b) = nonboolean_error(:any, :|)
or_bool_only(a::Bool, b::Bool) = a|b
and_bool_only(a, b) = nonboolean_error(:all, :&)
and_bool_only(a::Bool, b::Bool) = a&b

"""
    any(p, itr) -> Bool

Determine whether predicate `p` returns `true` for any elements of `itr`.

```jldoctest
julia> any(i->(4<=i<=6), [3,5,7])
true
```
"""
any(f::Any, itr) = any(Predicate(f), itr)
any(f::Predicate, itr) = mapreduce_sc_impl(f, |, itr)
any(f::typeof(identity), itr) =
    eltype(itr) <: Bool ?
        mapreduce_sc_impl(f, |, itr) :
        reduce(or_bool_only, false, itr)

"""
    all(p, itr) -> Bool

Determine whether predicate `p` returns `true` for all elements of `itr`.

```jldoctest
julia> all(i->(4<=i<=6), [4,5,6])
true
```
"""
all(f::Any, itr) = all(Predicate(f), itr)
all(f::Predicate, itr) = mapreduce_sc_impl(f, &, itr)
all(f::typeof(identity), itr) =
    eltype(itr) <: Bool ?
        mapreduce_sc_impl(f, &, itr) :
        reduce(and_bool_only, true, itr)

## in & contains

"""
    in(item, collection) -> Bool
    ∈(item,collection) -> Bool
    ∋(collection,item) -> Bool
    ∉(item,collection) -> Bool
    ∌(collection,item) -> Bool

Determine whether an item is in the given collection, in the sense that it is `==` to one of
the values generated by iterating over the collection. Some collections need a slightly
different definition; for example [`Set`](:obj:`Set`)s check whether the item
[`isequal`](:func:`isequal`) to one of the elements. [`Dict`](:obj:`Dict`)s look for
`(key,value)` pairs, and the key is compared using [`isequal`](:func:`isequal`). To test for
the presence of a key in a dictionary, use [`haskey`](:func:`haskey`) or `k in keys(dict)`.

```jldoctest
julia> a = 1:3:20
1:3:19

julia> 4 in a
true

julia> 5 in a
false
```
"""
in(x, itr) = any(Predicate(y -> y == x), itr)

const ∈ = in
∉(x, itr)=!∈(x, itr)
∋(itr, x)= ∈(x, itr)
∌(itr, x)=!∋(itr, x)

function contains(eq::Function, itr, x)
    for y in itr
        eq(y, x) && return true
    end
    return false
end


## countnz & count

"""
    count(p, itr) -> Integer

Count the number of elements in `itr` for which predicate `p` returns `true`.

```jldoctest
julia> count(i->(4<=i<=6), [2,3,4,5,6])
3
```
"""
function count(pred, itr)
    n = 0
    for x in itr
        n += pred(x)
    end
    return n
end

"""
    countnz(A)

Counts the number of nonzero values in array `A` (dense or sparse). Note that this is not a constant-time operation.
For sparse matrices, one should usually use [`nnz`](:func:`nnz`), which returns the number of stored values.
"""
countnz(a) = count(x -> x != 0, a)
