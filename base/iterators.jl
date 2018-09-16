# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Methods for working with Iterators.
"""
module Iterators

# small dance to make this work from Base or Intrinsics
import ..@__MODULE__, ..parentmodule
const Base = parentmodule(@__MODULE__)
using .Base:
    @inline, Pair, AbstractDict, IndexLinear, IndexCartesian, IndexStyle, AbstractVector, Vector,
    tail, tuple_type_head, tuple_type_tail, tuple_type_cons, SizeUnknown, HasLength, HasShape,
    IsInfinite, EltypeUnknown, HasEltype, OneTo, @propagate_inbounds, Generator, AbstractRange,
    LinearIndices, (:), |, +, -, !==, !, <=, <, missing

import .Base:
    first, last,
    isempty, length, size, axes, ndims,
    eltype, IteratorSize, IteratorEltype,
    haskey, keys, values, pairs,
    getindex, setindex!, get, iterate,
    popfirst!, isdone, peek

export enumerate, zip, rest, countfrom, take, drop, cycle, repeated, product, flatten, partition

tail_if_any(::Tuple{}) = ()
tail_if_any(x::Tuple) = tail(x)

_min_length(a, b, ::IsInfinite, ::IsInfinite) = min(length(a),length(b)) # inherit behaviour, error
_min_length(a, b, A, ::IsInfinite) = length(a)
_min_length(a, b, ::IsInfinite, B) = length(b)
_min_length(a, b, A, B) = min(length(a),length(b))

_diff_length(a, b, A, ::IsInfinite) = 0
_diff_length(a, b, ::IsInfinite, ::IsInfinite) = 0
_diff_length(a, b, ::IsInfinite, B) = length(a) # inherit behaviour, error
_diff_length(a, b, A, B) = max(length(a)-length(b), 0)

and_iteratorsize(isz::T, ::T) where {T} = isz
and_iteratorsize(::HasLength, ::HasShape) = HasLength()
and_iteratorsize(::HasShape, ::HasLength) = HasLength()
and_iteratorsize(a, b) = SizeUnknown()

and_iteratoreltype(iel::T, ::T) where {T} = iel
and_iteratoreltype(a, b) = EltypeUnknown()

## Reverse-order iteration for arrays and other collections.  Collections
## should implement iterate etcetera if possible/practical.
"""
    Iterators.reverse(itr)

Given an iterator `itr`, then `reverse(itr)` is an iterator over the
same collection but in the reverse order.

This iterator is "lazy" in that it does not make a copy of the collection in
order to reverse it; see [`Base.reverse`](@ref) for an eager implementation.

Not all iterator types `T` support reverse-order iteration.  If `T`
doesn't, then iterating over `Iterators.reverse(itr::T)` will throw a [`MethodError`](@ref)
because of the missing [`iterate`](@ref) methods for `Iterators.Reverse{T}`.
(To implement these methods, the original iterator
`itr::T` can be obtained from `r = Iterators.reverse(itr)` by `r.itr`.)

# Examples
```jldoctest
julia> foreach(println, Iterators.reverse(1:5))
5
4
3
2
1
```
"""
reverse(itr) = Reverse(itr)

struct Reverse{T}
    itr::T
end
eltype(::Type{Reverse{T}}) where {T} = eltype(T)
length(r::Reverse) = length(r.itr)
size(r::Reverse) = size(r.itr)
IteratorSize(::Type{Reverse{T}}) where {T} = IteratorSize(T)
IteratorEltype(::Type{Reverse{T}}) where {T} = IteratorEltype(T)
last(r::Reverse) = first(r.itr) # the first shall be last
first(r::Reverse) = last(r.itr) # and the last shall be first

# reverse-order array iterators: assumes more-specialized Reverse for eachindex
@propagate_inbounds function iterate(A::Reverse{<:AbstractArray}, state=(reverse(eachindex(A.itr)),))
    y = iterate(state...)
    y === nothing && return y
    idx, itrs = y
    (A.itr[idx], (state[1], itrs))
end

reverse(R::AbstractRange) = Base.reverse(R) # copying ranges is cheap
reverse(G::Generator) = Generator(G.f, reverse(G.iter))
reverse(r::Reverse) = r.itr
reverse(x::Union{Number,AbstractChar}) = x
reverse(p::Pair) = Base.reverse(p) # copying pairs is cheap

iterate(r::Reverse{<:Tuple}, i::Int = length(r.itr)) = i < 1 ? nothing : (r.itr[i], i-1)

# enumerate

struct Enumerate{I}
    itr::I
end

"""
    enumerate(iter)

An iterator that yields `(i, x)` where `i` is a counter starting at 1,
and `x` is the `i`th value from the given iterator. It's useful when
you need not only the values `x` over which you are iterating, but
also the number of iterations so far. Note that `i` may not be valid
for indexing `iter`; it's also possible that `x != iter[i]`, if `iter`
has indices that do not start at 1. See the `enumerate(IndexLinear(),
iter)` method if you want to ensure that `i` is an index.

# Examples
```jldoctest
julia> a = ["a", "b", "c"];

julia> for (index, value) in enumerate(a)
           println("\$index \$value")
       end
1 a
2 b
3 c
```
"""
enumerate(iter) = Enumerate(iter)

length(e::Enumerate) = length(e.itr)
size(e::Enumerate) = size(e.itr)
@propagate_inbounds function iterate(e::Enumerate, state=(1,))
    i, rest = state[1], tail(state)
    n = iterate(e.itr, rest...)
    n === nothing && return n
    (i, n[1]), (i+1, n[2])
end

eltype(::Type{Enumerate{I}}) where {I} = Tuple{Int, eltype(I)}

IteratorSize(::Type{Enumerate{I}}) where {I} = IteratorSize(I)
IteratorEltype(::Type{Enumerate{I}}) where {I} = IteratorEltype(I)

@inline function iterate(r::Reverse{<:Enumerate})
    ri = reverse(r.itr.itr)
    iterate(r, (length(ri), ri))
end
@inline function iterate(r::Reverse{<:Enumerate}, state)
    i, ri, rest = state[1], state[2], tail(tail(state))
    n = iterate(ri, rest...)
    n === nothing && return n
    (i, n[1]), (i-1, ri, n[2])
end

"""
    Iterators.Pairs(values, keys) <: AbstractDict{eltype(keys), eltype(values)}

Transforms an indexable container into an Dictionary-view of the same data.
Modifying the key-space of the underlying data may invalidate this object.
"""
struct Pairs{K, V, I, A} <: AbstractDict{K, V}
    data::A
    itr::I
    Pairs(data::A, itr::I) where {A, I} = new{eltype(I), eltype(A), I, A}(data, itr)
end

"""
    pairs(IndexLinear(), A)
    pairs(IndexCartesian(), A)
    pairs(IndexStyle(A), A)

An iterator that accesses each element of the array `A`, returning
`i => x`, where `i` is the index for the element and `x = A[i]`.
Identical to `pairs(A)`, except that the style of index can be selected.
Also similar to `enumerate(A)`, except `i` will be a valid index
for `A`, while `enumerate` always counts from 1 regardless of the indices
of `A`.

Specifying [`IndexLinear()`](@ref) ensures that `i` will be an integer;
specifying [`IndexCartesian()`](@ref) ensures that `i` will be a
[`CartesianIndex`](@ref); specifying `IndexStyle(A)` chooses whichever has
been defined as the native indexing style for array `A`.

Mutation of the bounds of the underlying array will invalidate this iterator.

# Examples
```jldoctest
julia> A = ["a" "d"; "b" "e"; "c" "f"];

julia> for (index, value) in pairs(IndexStyle(A), A)
           println("\$index \$value")
       end
1 a
2 b
3 c
4 d
5 e
6 f

julia> S = view(A, 1:2, :);

julia> for (index, value) in pairs(IndexStyle(S), S)
           println("\$index \$value")
       end
CartesianIndex(1, 1) a
CartesianIndex(2, 1) b
CartesianIndex(1, 2) d
CartesianIndex(2, 2) e
```

See also: [`IndexStyle`](@ref), [`axes`](@ref).
"""
pairs(::IndexLinear,    A::AbstractArray) = Pairs(A, LinearIndices(A))
pairs(::IndexCartesian, A::AbstractArray) = Pairs(A, CartesianIndices(axes(A)))

# preserve indexing capabilities for known indexable types
# faster than zip(keys(a), values(a)) for arrays
pairs(A::AbstractArray)  = pairs(IndexCartesian(), A)
pairs(A::AbstractVector) = pairs(IndexLinear(), A)
pairs(tuple::Tuple) = Pairs(tuple, keys(tuple))
pairs(nt::NamedTuple) = Pairs(nt, keys(nt))
pairs(v::Core.SimpleVector) = Pairs(v, LinearIndices(v))
# pairs(v::Pairs) = v # listed for reference, but already defined from being an AbstractDict

length(v::Pairs) = length(v.itr)
axes(v::Pairs) = axes(v.itr)
size(v::Pairs) = size(v.itr)
@propagate_inbounds function iterate(v::Pairs{K, V}, state...) where {K, V}
    x = iterate(v.itr, state...)
    x === nothing && return x
    indx, n = x
    item = v.data[indx]
    return (Pair{K, V}(indx, item), n)
end
@inline isdone(v::Pairs, state...) = isdone(v.itr, state...)

IteratorSize(::Type{<:Pairs{<:Any, <:Any, I}}) where {I} = IteratorSize(I)
IteratorSize(::Type{<:Pairs{<:Any, <:Any, <:Base.AbstractUnitRange, <:Tuple}}) = HasLength()

reverse(v::Pairs) = Pairs(v.data, reverse(v.itr))

haskey(v::Pairs, key) = (key in v.itr)
keys(v::Pairs) = v.itr
values(v::Pairs) = v.data
getindex(v::Pairs, key) = v.data[key]
setindex!(v::Pairs, value, key) = (v.data[key] = value; v)
get(v::Pairs, key, default) = get(v.data, key, default)
get(f::Base.Callable, v::Pairs, key) = get(f, v.data, key)

# zip

abstract type AbstractZipIterator end

zip_iteratorsize(a, b) = and_iteratorsize(a,b) # as `and_iteratorsize` but inherit `Union{HasLength,IsInfinite}` of the shorter iterator
zip_iteratorsize(::HasLength, ::IsInfinite) = HasLength()
zip_iteratorsize(::HasShape, ::IsInfinite) = HasLength()
zip_iteratorsize(a::IsInfinite, b) = zip_iteratorsize(b,a)
zip_iteratorsize(a::IsInfinite, b::IsInfinite) = IsInfinite()


struct Zip1{I} <: AbstractZipIterator
    a::I
end
zip(a) = Zip1(a)
length(z::Zip1) = length(z.a)
size(z::Zip1) = size(z.a)
axes(z::Zip1) = axes(z.a)
eltype(::Type{Zip1{I}}) where {I} = Tuple{eltype(I)}
@propagate_inbounds function iterate(z::Zip1, state...)
    n = iterate(z.a, state...)
    n === nothing && return n
    return ((n[1],), n[2])
end
@inline isdone(z::Zip1, state...) = isdone(z.a, state...)

IteratorSize(::Type{Zip1{I}}) where {I} = IteratorSize(I)
IteratorEltype(::Type{Zip1{I}}) where {I} = IteratorEltype(I)

struct Zip2{I1, I2} <: AbstractZipIterator
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)
length(z::Zip2) = _min_length(z.a, z.b, IteratorSize(z.a), IteratorSize(z.b))
size(z::Zip2) = promote_shape(size(z.a), size(z.b))
axes(z::Zip2) = promote_shape(axes(z.a), axes(z.b))
eltype(::Type{Zip2{I1,I2}}) where {I1,I2} = Tuple{eltype(I1), eltype(I2)}
@inline isdone(z::Zip2) = isdone(z.a) | isdone(z.b)
@inline isdone(z::Zip2, (sa, sb)::Tuple{Any, Any}) = isdone(z.a, sa) | isdone(z.b, sb)
function zip_iterate(a, b, sta, stb) # the states are either Tuple{} or Tuple{Any}
    da, db = isdone(a), isdone(b)
    da === true && return nothing
    db === true && return nothing
    if da === missing
       ya = iterate(a, sta...)
       ya === nothing && return nothing
    end
    if db === missing
       yb = iterate(b, stb...)
       yb === nothing && return nothing
    end
    if da === false
         ya = iterate(a, sta...)
         ya === nothing && return nothing
    end
    if db === false
         yb = iterate(b, stb...)
         yb === nothing && return nothing
    end
    return (ya, yb)
end
let interleave(a, b) = ((a[1], b[1]), (a[2], b[2]))
    global iterate
    @propagate_inbounds function iterate(z::Zip2)
        ys = zip_iterate(z.a, z.b, (), ())
        ys === nothing && return nothing
        return interleave(ys...)
    end
    @propagate_inbounds function iterate(z::Zip2, st::Tuple{Any, Any})
        ys = zip_iterate(z.a, z.b, (st[1],), (st[2],))
        ys === nothing && return nothing
        return interleave(ys...)
    end
end

IteratorSize(::Type{Zip2{I1,I2}}) where {I1,I2} = zip_iteratorsize(IteratorSize(I1),IteratorSize(I2))
IteratorEltype(::Type{Zip2{I1,I2}}) where {I1,I2} = and_iteratoreltype(IteratorEltype(I1),IteratorEltype(I2))

struct Zip{I, Z<:AbstractZipIterator} <: AbstractZipIterator
    a::I
    z::Z
end

"""
    zip(iters...)

For a set of iterable objects, return an iterable of tuples, where the `i`th tuple contains
the `i`th component of each input iterable.

# Examples
```jldoctest
julia> a = 1:5
1:5

julia> b = ["e","d","b","c","a"]
5-element Array{String,1}:
 "e"
 "d"
 "b"
 "c"
 "a"

julia> c = zip(a,b)
Base.Iterators.Zip2{UnitRange{Int64},Array{String,1}}(1:5, ["e", "d", "b", "c", "a"])

julia> length(c)
5

julia> first(c)
(1, "e")
```
"""
zip(a, b, c...) = Zip(a, zip(b, c...))
length(z::Zip) = _min_length(z.a, z.z, IteratorSize(z.a), IteratorSize(z.z))
size(z::Zip) = promote_shape(size(z.a), size(z.z))
axes(z::Zip) = promote_shape(axes(z.a), axes(z.z))
eltype(::Type{Zip{I,Z}}) where {I,Z} = tuple_type_cons(eltype(I), eltype(Z))
@inline isdone(z::Zip) = isdone(z.a) | isdone(z.z)
@inline isdone(z::Zip, (sa, sz)) = isdone(z.a, sa) | isdone(z.a, sz)
let interleave(a, b) = ((a[1], b[1]...), (a[2], b[2]))
    global iterate
    @propagate_inbounds function iterate(z::Zip)
        ys = zip_iterate(z.a, z.z, (), ())
        ys === nothing && return nothing
        return interleave(ys...)
    end
    @propagate_inbounds function iterate(z::Zip, st::Tuple{Any, Any})
        ys = zip_iterate(z.a, z.z, (st[1],), (st[2],))
        ys === nothing && return nothing
        return interleave(ys...)
    end
end

IteratorSize(::Type{Zip{I1,I2}}) where {I1,I2} = zip_iteratorsize(IteratorSize(I1),IteratorSize(I2))
IteratorEltype(::Type{Zip{I1,I2}}) where {I1,I2} = and_iteratoreltype(IteratorEltype(I1),IteratorEltype(I2))

reverse(z::Zip1) = Zip1(reverse(z.a))
reverse(z::Zip2) = Zip2(reverse(z.a), reverse(z.b))
reverse(z::Zip) = Zip(reverse(z.a), reverse(z.z))

# filter

struct Filter{F,I}
    flt::F
    itr::I
end

"""
    Iterators.filter(flt, itr)

Given a predicate function `flt` and an iterable object `itr`, return an
iterable object which upon iteration yields the elements `x` of `itr` that
satisfy `flt(x)`. The order of the original iterator is preserved.

This function is *lazy*; that is, it is guaranteed to return in ``Θ(1)`` time
and use ``Θ(1)`` additional space, and `flt` will not be called by an
invocation of `filter`. Calls to `flt` will be made when iterating over the
returned iterable object. These calls are not cached and repeated calls will be
made when reiterating.

See [`Base.filter`](@ref) for an eager implementation of filtering for arrays.

# Examples
```jldoctest
julia> f = Iterators.filter(isodd, [1, 2, 3, 4, 5])
Base.Iterators.Filter{typeof(isodd),Array{Int64,1}}(isodd, [1, 2, 3, 4, 5])

julia> foreach(println, f)
1
3
5
```
"""
filter(flt, itr) = Filter(flt, itr)

function iterate(f::Filter, state...)
    y = iterate(f.itr, state...)
    while y !== nothing
        if f.flt(y[1])
            return y
        end
        y = iterate(f.itr, y[2])
    end
    nothing
end

eltype(::Type{Filter{F,I}}) where {F,I} = eltype(I)
IteratorEltype(::Type{Filter{F,I}}) where {F,I} = IteratorEltype(I)
IteratorSize(::Type{<:Filter}) = SizeUnknown()

reverse(f::Filter) = Filter(f.flt, reverse(f.itr))

# Rest -- iterate starting at the given state

struct Rest{I,S}
    itr::I
    st::S
end

"""
    rest(iter, state)

An iterator that yields the same elements as `iter`, but starting at the given `state`.

# Examples
```jldoctest
julia> collect(Iterators.rest([1,2,3,4], 2))
3-element Array{Int64,1}:
 2
 3
 4
```
"""
rest(itr,state) = Rest(itr,state)
rest(itr) = itr

"""
    peel(iter)

Returns the first element and an iterator over the remaining elements.

# Examples
```jldoctest
julia> (a, rest) = Iterators.peel("abc");

julia> a
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> collect(rest)
2-element Array{Char,1}:
 'b'
 'c'
```
"""
function peel(itr)
    y = iterate(itr)
    y === nothing && throw(BoundsError())
    val, s = y
    val, rest(itr, s)
end

@propagate_inbounds iterate(i::Rest, st=i.st) = iterate(i.itr, st)
isdone(i::Rest, st...) = isdone(i.itr, st...)

eltype(::Type{<:Rest{I}}) where {I} = eltype(I)
IteratorEltype(::Type{<:Rest{I}}) where {I} = IteratorEltype(I)
rest_iteratorsize(a) = SizeUnknown()
rest_iteratorsize(::IsInfinite) = IsInfinite()
IteratorSize(::Type{<:Rest{I}}) where {I} = rest_iteratorsize(IteratorSize(I))

# Count -- infinite counting

struct Count{S<:Number}
    start::S
    step::S
end

"""
    countfrom(start=1, step=1)

An iterator that counts forever, starting at `start` and incrementing by `step`.

# Examples
```jldoctest
julia> for v in Iterators.countfrom(5, 2)
           v > 10 && break
           println(v)
       end
5
7
9
```
"""
countfrom(start::Number, step::Number) = Count(promote(start, step)...)
countfrom(start::Number)               = Count(start, oneunit(start))
countfrom()                            = Count(1, 1)

eltype(::Type{Count{S}}) where {S} = S

iterate(it::Count, state=it.start) = (state, state + it.step)

IteratorSize(::Type{<:Count}) = IsInfinite()

# Take -- iterate through the first n elements

struct Take{I}
    xs::I
    n::Int
    function Take(xs::I, n::Integer) where {I}
        n < 0 && throw(ArgumentError("Take length must be nonnegative"))
        return new{I}(xs, n)
    end
end

"""
    take(iter, n)

An iterator that generates at most the first `n` elements of `iter`.

# Examples
```jldoctest
julia> a = 1:2:11
1:2:11

julia> collect(a)
6-element Array{Int64,1}:
  1
  3
  5
  7
  9
 11

julia> collect(Iterators.take(a,3))
3-element Array{Int64,1}:
 1
 3
 5
```
"""
take(xs, n::Integer) = Take(xs, Int(n))
take(xs::Take, n::Integer) = Take(xs.xs, min(Int(n), xs.n))

eltype(::Type{Take{I}}) where {I} = eltype(I)
IteratorEltype(::Type{Take{I}}) where {I} = IteratorEltype(I)
take_iteratorsize(a) = HasLength()
take_iteratorsize(::SizeUnknown) = SizeUnknown()
IteratorSize(::Type{Take{I}}) where {I} = take_iteratorsize(IteratorSize(I))
length(t::Take) = _min_length(t.xs, 1:t.n, IteratorSize(t.xs), HasLength())
isdone(t::Take) = isdone(t.xs)
isdone(t::Take, state) = (state[1] <= 0) | isdone(t.xs, tail(state))

@propagate_inbounds function iterate(it::Take, state=(it.n,))
    n, rest = state[1], tail(state)
    n <= 0 && return nothing
    y = iterate(it.xs, rest...)
    y === nothing && return nothing
    return y[1], (n - 1, y[2])
end

# Drop -- iterator through all but the first n elements

struct Drop{I}
    xs::I
    n::Int
    function Drop(xs::I, n::Integer) where {I}
        n < 0 && throw(ArgumentError("Drop length must be nonnegative"))
        return new{I}(xs, n)
    end
end

"""
    drop(iter, n)

An iterator that generates all but the first `n` elements of `iter`.

# Examples
```jldoctest
julia> a = 1:2:11
1:2:11

julia> collect(a)
6-element Array{Int64,1}:
  1
  3
  5
  7
  9
 11

julia> collect(Iterators.drop(a,4))
2-element Array{Int64,1}:
  9
 11
```
"""
drop(xs, n::Integer) = Drop(xs, Int(n))
drop(xs::Take, n::Integer) = Take(drop(xs.xs, Int(n)), max(0, xs.n - Int(n)))
drop(xs::Drop, n::Integer) = Drop(xs.xs, Int(n) + xs.n)

eltype(::Type{Drop{I}}) where {I} = eltype(I)
IteratorEltype(::Type{Drop{I}}) where {I} = IteratorEltype(I)
drop_iteratorsize(::SizeUnknown) = SizeUnknown()
drop_iteratorsize(::Union{HasShape, HasLength}) = HasLength()
drop_iteratorsize(::IsInfinite) = IsInfinite()
IteratorSize(::Type{Drop{I}}) where {I} = drop_iteratorsize(IteratorSize(I))
length(d::Drop) = _diff_length(d.xs, 1:d.n, IteratorSize(d.xs), HasLength())

function iterate(it::Drop)
    y = iterate(it.xs)
    for i in 1:it.n
        y === nothing && return y
        y = iterate(it.xs, y[2])
    end
    y
end
iterate(it::Drop, state) = iterate(it.xs, state)
isdone(it::Drop, state) = isdone(it.xs, state)

# Cycle an iterator forever

struct Cycle{I}
    xs::I
end

"""
    cycle(iter)

An iterator that cycles through `iter` forever.
If `iter` is empty, so is `cycle(iter)`.

# Examples
```jldoctest
julia> for (i, v) in enumerate(Iterators.cycle("hello"))
           print(v)
           i > 10 && break
       end
hellohelloh
```
"""
cycle(xs) = Cycle(xs)

eltype(::Type{Cycle{I}}) where {I} = eltype(I)
IteratorEltype(::Type{Cycle{I}}) where {I} = IteratorEltype(I)
IteratorSize(::Type{Cycle{I}}) where {I} = IsInfinite()

iterate(it::Cycle) = iterate(it.xs)
isdone(it::Cycle) = isdone(it.xs)
isdone(it::Cycle, state) = false
function iterate(it::Cycle, state)
    y = iterate(it.xs, state)
    y === nothing && return iterate(it)
    y
end

reverse(it::Cycle) = Cycle(reverse(it.xs))

# Repeated - repeat an object infinitely many times

struct Repeated{O}
    x::O
end
repeated(x) = Repeated(x)

"""
    repeated(x[, n::Int])

An iterator that generates the value `x` forever. If `n` is specified, generates `x` that
many times (equivalent to `take(repeated(x), n)`).

# Examples
```jldoctest
julia> a = Iterators.repeated([1 2], 4);

julia> collect(a)
4-element Array{Array{Int64,2},1}:
 [1 2]
 [1 2]
 [1 2]
 [1 2]
```
"""
repeated(x, n::Integer) = take(repeated(x), Int(n))

eltype(::Type{Repeated{O}}) where {O} = O

iterate(it::Repeated, state...) = (it.x, nothing)

IteratorSize(::Type{<:Repeated}) = IsInfinite()
IteratorEltype(::Type{<:Repeated}) = HasEltype()

reverse(it::Union{Repeated,Take{<:Repeated}}) = it

# Product -- cartesian product of iterators
struct ProductIterator{T<:Tuple}
    iterators::T
end

"""
    product(iters...)

Return an iterator over the product of several iterators. Each generated element is
a tuple whose `i`th element comes from the `i`th argument iterator. The first iterator
changes the fastest.

# Examples
```jldoctest
julia> collect(Iterators.product(1:2, 3:5))
2×3 Array{Tuple{Int64,Int64},2}:
 (1, 3)  (1, 4)  (1, 5)
 (2, 3)  (2, 4)  (2, 5)
```
"""
product(iters...) = ProductIterator(iters)

IteratorSize(::Type{ProductIterator{Tuple{}}}) = HasShape{0}()
IteratorSize(::Type{ProductIterator{T}}) where {T<:Tuple} =
    prod_iteratorsize( IteratorSize(tuple_type_head(T)), IteratorSize(ProductIterator{tuple_type_tail(T)}) )

prod_iteratorsize(::HasLength, ::HasLength) = HasShape{2}()
prod_iteratorsize(::HasLength, ::HasShape{N}) where {N} = HasShape{N+1}()
prod_iteratorsize(::HasShape{N}, ::HasLength) where {N} = HasShape{N+1}()
prod_iteratorsize(::HasShape{M}, ::HasShape{N}) where {M,N} = HasShape{M+N}()

# products can have an infinite iterator
prod_iteratorsize(::IsInfinite, ::IsInfinite) = IsInfinite()
prod_iteratorsize(a, ::IsInfinite) = IsInfinite()
prod_iteratorsize(::IsInfinite, b) = IsInfinite()
prod_iteratorsize(a, b) = SizeUnknown()

size(P::ProductIterator) = _prod_size(P.iterators)
_prod_size(::Tuple{}) = ()
_prod_size(t::Tuple) = (_prod_size1(t[1], IteratorSize(t[1]))..., _prod_size(tail(t))...)
_prod_size1(a, ::HasShape)  = size(a)
_prod_size1(a, ::HasLength) = (length(a),)
_prod_size1(a, A) =
    throw(ArgumentError("Cannot compute size for object of type $(typeof(a))"))

axes(P::ProductIterator) = _prod_indices(P.iterators)
_prod_indices(::Tuple{}) = ()
_prod_indices(t::Tuple) = (_prod_axes1(t[1], IteratorSize(t[1]))..., _prod_indices(tail(t))...)
_prod_axes1(a, ::HasShape)  = axes(a)
_prod_axes1(a, ::HasLength) = (OneTo(length(a)),)
_prod_axes1(a, A) =
    throw(ArgumentError("Cannot compute indices for object of type $(typeof(a))"))

ndims(p::ProductIterator) = length(axes(p))
length(P::ProductIterator) = prod(size(P))

IteratorEltype(::Type{ProductIterator{Tuple{}}}) = HasEltype()
IteratorEltype(::Type{ProductIterator{Tuple{I}}}) where {I} = IteratorEltype(I)
function IteratorEltype(::Type{ProductIterator{T}}) where {T<:Tuple}
    I = tuple_type_head(T)
    P = ProductIterator{tuple_type_tail(T)}
    IteratorEltype(I) == EltypeUnknown() ? EltypeUnknown() : IteratorEltype(P)
end

eltype(::Type{<:ProductIterator{I}}) where {I} = _prod_eltype(I)
_prod_eltype(::Type{Tuple{}}) = Tuple{}
_prod_eltype(::Type{I}) where {I<:Tuple} =
    Base.tuple_type_cons(eltype(tuple_type_head(I)),_prod_eltype(tuple_type_tail(I)))

iterate(::ProductIterator{Tuple{}}) = (), true
iterate(::ProductIterator{Tuple{}}, state) = nothing

@inline isdone(P::ProductIterator) = any(isdone, P.iterators)
@inline function _pisdone(iters, states)
    iter1 = first(iters)
    done1 = isdone(iter1, first(states)[2]) # check step
    done1 === true || return done1 # false or missing
    done1 = isdone(iter1) # check restart
    done1 === true || return done1 # false or missing
    return _pisdone(tail(iters), tail(states)) # check tail
end
@inline isdone(P::ProductIterator, states) = _pisdone(P.iterators, states)

@inline _piterate() = ()
@inline function _piterate(iter1, rest...)
    next = iterate(iter1)
    next === nothing && return nothing
    restnext = _piterate(rest...)
    restnext === nothing && return nothing
    return (next, restnext...)
end
@inline function iterate(P::ProductIterator)
    isdone(P) === true && return nothing
    next = _piterate(P.iterators...)
    next === nothing && return nothing
    return (map(x -> x[1], next), next)
end

@inline _piterate1(::Tuple{}, ::Tuple{}) = nothing
@inline function _piterate1(iters, states)
    iter1 = first(iters)
    next = iterate(iter1, first(states)[2])
    restnext = tail(states)
    if next === nothing
        isdone(iter1) === true && return nothing
        restnext = _piterate1(tail(iters), restnext)
        restnext === nothing && return nothing
        next = iterate(iter1)
        next === nothing && return nothing
    end
    return (next, restnext...)
end
@inline function iterate(P::ProductIterator, states)
    isdone(P, states) === true && return nothing
    next = _piterate1(P.iterators, states)
    next === nothing && return nothing
    return (map(x -> x[1], next), next)
end

reverse(p::ProductIterator) = ProductIterator(map(reverse, p.iterators))

# flatten an iterator of iterators

struct Flatten{I}
    it::I
end

"""
    flatten(iter)

Given an iterator that yields iterators, return an iterator that yields the
elements of those iterators.
Put differently, the elements of the argument iterator are concatenated.

# Examples
```jldoctest
julia> collect(Iterators.flatten((1:2, 8:9)))
4-element Array{Int64,1}:
 1
 2
 8
 9
```
"""
flatten(itr) = Flatten(itr)

eltype(::Type{Flatten{I}}) where {I} = eltype(eltype(I))
IteratorEltype(::Type{Flatten{I}}) where {I} = _flatteneltype(I, IteratorEltype(I))
_flatteneltype(I, ::HasEltype) = IteratorEltype(eltype(I))
_flatteneltype(I, et) = EltypeUnknown()

flatten_iteratorsize(::Union{HasShape, HasLength}, ::Type{<:NTuple{N,Any}}) where {N} = HasLength()
flatten_iteratorsize(::Union{HasShape, HasLength}, ::Type{<:Tuple}) = SizeUnknown()
flatten_iteratorsize(::Union{HasShape, HasLength}, ::Type{<:Number}) = HasLength()
flatten_iteratorsize(::Union{HasShape, HasLength}, ::Type{Union{}}) = SizeUnknown()
flatten_iteratorsize(a, b) = SizeUnknown()

_flatten_iteratorsize(sz, ::EltypeUnknown, I) = SizeUnknown()
_flatten_iteratorsize(sz, ::HasEltype, I) = flatten_iteratorsize(sz, eltype(I))

IteratorSize(::Type{Flatten{I}}) where {I} = _flatten_iteratorsize(IteratorSize(I), IteratorEltype(I), I)

function flatten_length(f, T::Type{<:NTuple{N,Any}}) where {N}
    fieldcount(T)*length(f.it)
end
flatten_length(f, ::Type{<:Number}) = length(f.it)
flatten_length(f, T) = throw(ArgumentError(
    "Iterates of the argument to Flatten are not known to have constant length"))
length(f::Flatten{I}) where {I} = flatten_length(f, eltype(I))

@propagate_inbounds function iterate(f::Flatten, state=())
    if state !== ()
        y = iterate(tail(state)...)
        y !== nothing && return (y[1], (state[1], state[2], y[2]))
    end
    x = (state === () ? iterate(f.it) : iterate(f.it, state[1]))
    x === nothing && return nothing
    iterate(f, (x[2], x[1]))
end

reverse(f::Flatten) = Flatten(reverse(itr) for itr in reverse(f.it))

"""
    partition(collection, n)

Iterate over a collection `n` elements at a time.

# Examples
```jldoctest
julia> collect(Iterators.partition([1,2,3,4,5], 2))
3-element Array{Array{Int64,1},1}:
 [1, 2]
 [3, 4]
 [5]
```
"""
partition(c::T, n::Integer) where {T} = PartitionIterator{T}(c, Int(n))


struct PartitionIterator{T}
    c::T
    n::Int
end

eltype(::Type{PartitionIterator{T}}) where {T} = Vector{eltype(T)}
partition_iteratorsize(::HasShape) = HasLength()
partition_iteratorsize(isz) = isz
function IteratorSize(::Type{PartitionIterator{T}}) where {T}
    partition_iteratorsize(IteratorSize(T))
end

IteratorEltype(::Type{<:PartitionIterator{T}}) where {T} = IteratorEltype(T)

function length(itr::PartitionIterator)
    l = length(itr.c)
    return div(l, itr.n) + ((mod(l, itr.n) > 0) ? 1 : 0)
end

function iterate(itr::PartitionIterator{<:Vector}, state=1)
    state > length(itr.c) && return nothing
    r = min(state + itr.n - 1, length(itr.c))
    return view(itr.c, state:r), r + 1
end

struct IterationCutShort; end

function iterate(itr::PartitionIterator, state...)
    v = Vector{eltype(itr.c)}(undef, itr.n)
    # This is necessary to remember whether we cut the
    # last element short. In such cases, we do return that
    # element, but not the next one
    state === (IterationCutShort(),) && return nothing
    i = 0
    y = iterate(itr.c, state...)
    while y !== nothing
        i += 1
        v[i] = y[1]
        if i >= itr.n
            break
        end
        y = iterate(itr.c, y[2])
    end
    i === 0 && return nothing
    return resize!(v, i), y === nothing ? IterationCutShort() : y[2]
end

"""
    Stateful(itr)

There are several different ways to think about this iterator wrapper:

1. It provides a mutable wrapper around an iterator and
   its iteration state.
2. It turns an iterator-like abstraction into a `Channel`-like
   abstraction.
3. It's an iterator that mutates to become its own rest iterator
   whenever an item is produced.

`Stateful` provides the regular iterator interface. Like other mutable iterators
(e.g. [`Channel`](@ref)), if iteration is stopped early (e.g. by a `break` in a `for` loop),
iteration can be resumed from the same spot by continuing to iterate over the
same iterator object (in contrast, an immutable iterator would restart from the
beginning).

# Examples
```jldoctest
julia> a = Iterators.Stateful("abcdef");

julia> isempty(a)
false

julia> popfirst!(a)
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> collect(Iterators.take(a, 3))
3-element Array{Char,1}:
 'b'
 'c'
 'd'

julia> collect(a)
2-element Array{Char,1}:
 'e'
 'f'
```

```jldoctest
julia> a = Iterators.Stateful([1,1,1,2,3,4]);

julia> for x in a; x == 1 || break; end

julia> Base.peek(a)
3

julia> sum(a) # Sum the remaining elements
7
```
"""
mutable struct Stateful{T, VS}
    itr::T
    # A bit awkward right now, but adapted to the new iteration protocol
    nextvalstate::Union{VS, Nothing}
    taken::Int
    @inline function Stateful{<:Any, Any}(itr::T) where {T}
        new{T, Any}(itr, iterate(itr), 0)
    end
    @inline function Stateful(itr::T) where {T}
        VS = approx_iter_type(T)
        return new{T, VS}(itr, iterate(itr)::VS, 0)
    end
end

function reset!(s::Stateful{T,VS}, itr::T) where {T,VS}
    s.itr = itr
    setfield!(s, :nextvalstate, iterate(itr))
    s.taken = 0
    s
end

if Base === Core.Compiler
    approx_iter_type(a::Type) = Any
else
    # Try to find an appropriate type for the (value, state tuple),
    # by doing a recursive unrolling of the iteration protocol up to
    # fixpoint.
    approx_iter_type(itrT::Type) = _approx_iter_type(itrT, Base._return_type(iterate, Tuple{itrT}))
    # Not actually called, just passed to return type to avoid
    # having to typesubtract
    function doiterate(itr, valstate::Union{Nothing, Tuple{Any, Any}})
        valstate === nothing && return nothing
        val, st = valstate
        return iterate(itr, st)
    end
    function _approx_iter_type(itrT::Type, vstate::Type)
        vstate <: Union{Nothing, Tuple{Any, Any}} || return Any
        vstate <: Union{} && return Union{}
        nextvstate = Base._return_type(doiterate, Tuple{itrT, vstate})
        return (nextvstate <: vstate ? vstate : Any)
    end
end

convert(::Type{Stateful}, itr) = Stateful(itr)

@inline isdone(s::Stateful, st=nothing) = s.nextvalstate === nothing

@inline function popfirst!(s::Stateful)
    vs = s.nextvalstate
    if vs === nothing
        throw(EOFError())
    else
        val, state = vs
        Core.setfield!(s, :nextvalstate, iterate(s.itr, state))
        s.taken += 1
        return val
    end
end

@inline peek(s::Stateful, sentinel=nothing) = s.nextvalstate !== nothing ? s.nextvalstate[1] : sentinel
@inline iterate(s::Stateful, state=nothing) = s.nextvalstate === nothing ? nothing : (popfirst!(s), nothing)
IteratorSize(::Type{Stateful{VS,T}} where VS) where {T} =
    isa(IteratorSize(T), SizeUnknown) ? SizeUnknown() : HasLength()
eltype(::Type{Stateful{T, VS}} where VS) where {T} = eltype(T)
IteratorEltype(::Type{Stateful{VS,T}} where VS) where {T} = IteratorEltype(T)
length(s::Stateful) = length(s.itr) - s.taken

end
