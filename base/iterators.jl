# This file is a part of Julia. License is MIT: https://julialang.org/license

module Iterators

import Base: start, done, next, isempty, length, size, eltype, iteratorsize, iteratoreltype, indices, ndims, pairs

using Base: tail, tuple_type_head, tuple_type_tail, tuple_type_cons, SizeUnknown, HasLength, HasShape,
            IsInfinite, EltypeUnknown, HasEltype, OneTo, @propagate_inbounds

export enumerate, zip, rest, countfrom, take, drop, cycle, repeated, product, flatten, partition

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
@inline start(e::Enumerate) = (1, start(e.itr))
@inline function next(e::Enumerate, state)
    n = next(e.itr,state[2])
    (state[1],n[1]), (state[1]+1,n[2])
end
@inline done(e::Enumerate, state) = done(e.itr, state[2])

eltype(::Type{Enumerate{I}}) where {I} = Tuple{Int, eltype(I)}

iteratorsize(::Type{Enumerate{I}}) where {I} = iteratorsize(I)
iteratoreltype(::Type{Enumerate{I}}) where {I} = iteratoreltype(I)

struct IndexValue{I,A<:AbstractArray}
    data::A
    itr::I
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

Specifying `IndexLinear()` ensures that `i` will be an integer;
specifying `IndexCartesian()` ensures that `i` will be a
`CartesianIndex`; specifying `IndexStyle(A)` chooses whichever has
been defined as the native indexing style for array `A`.

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
CartesianIndex{2}((1, 1)) a
CartesianIndex{2}((2, 1)) b
CartesianIndex{2}((1, 2)) d
CartesianIndex{2}((2, 2)) e
```

See also: [`IndexStyle`](@ref), [`indices`](@ref).
"""
pairs(::IndexLinear,    A::AbstractArray) = IndexValue(A, linearindices(A))
pairs(::IndexCartesian, A::AbstractArray) = IndexValue(A, CartesianRange(indices(A)))

# faster than zip(keys(a), values(a)) for arrays
pairs(A::AbstractArray)  = pairs(IndexCartesian(), A)
pairs(A::AbstractVector) = pairs(IndexLinear(), A)

length(v::IndexValue)  = length(v.itr)
indices(v::IndexValue) = indices(v.itr)
size(v::IndexValue)    = size(v.itr)
@inline start(v::IndexValue) = start(v.itr)
@propagate_inbounds function next(v::IndexValue, state)
    indx, n = next(v.itr, state)
    item = v.data[indx]
    (indx => item), n
end
@inline done(v::IndexValue, state) = done(v.itr, state)

eltype(::Type{IndexValue{I,A}}) where {I,A} = Pair{eltype(I), eltype(A)}

iteratorsize(::Type{IndexValue{I}}) where {I} = iteratorsize(I)
iteratoreltype(::Type{IndexValue{I}}) where {I} = iteratoreltype(I)

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
indices(z::Zip1) = indices(z.a)
eltype(::Type{Zip1{I}}) where {I} = Tuple{eltype(I)}
@inline start(z::Zip1) = start(z.a)
@inline function next(z::Zip1, st)
    n = next(z.a,st)
    return ((n[1],), n[2])
end
@inline done(z::Zip1, st) = done(z.a,st)

iteratorsize(::Type{Zip1{I}}) where {I} = iteratorsize(I)
iteratoreltype(::Type{Zip1{I}}) where {I} = iteratoreltype(I)

struct Zip2{I1, I2} <: AbstractZipIterator
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)
length(z::Zip2) = _min_length(z.a, z.b, iteratorsize(z.a), iteratorsize(z.b))
size(z::Zip2) = promote_shape(size(z.a), size(z.b))
indices(z::Zip2) = promote_shape(indices(z.a), indices(z.b))
eltype(::Type{Zip2{I1,I2}}) where {I1,I2} = Tuple{eltype(I1), eltype(I2)}
@inline start(z::Zip2) = (start(z.a), start(z.b))
@inline function next(z::Zip2, st)
    n1 = next(z.a,st[1])
    n2 = next(z.b,st[2])
    return ((n1[1], n2[1]), (n1[2], n2[2]))
end
@inline done(z::Zip2, st) = done(z.a,st[1]) | done(z.b,st[2])

iteratorsize(::Type{Zip2{I1,I2}}) where {I1,I2} = zip_iteratorsize(iteratorsize(I1),iteratorsize(I2))
iteratoreltype(::Type{Zip2{I1,I2}}) where {I1,I2} = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))

struct Zip{I, Z<:AbstractZipIterator} <: AbstractZipIterator
    a::I
    z::Z
end

"""
    zip(iters...)

For a set of iterable objects, returns an iterable of tuples, where the `i`th tuple contains
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
Base.Iterators.Zip2{UnitRange{Int64},Array{String,1}}(1:5, String["e", "d", "b", "c", "a"])

julia> length(c)
5

julia> first(c)
(1, "e")
```
"""
zip(a, b, c...) = Zip(a, zip(b, c...))
length(z::Zip) = _min_length(z.a, z.z, iteratorsize(z.a), iteratorsize(z.z))
size(z::Zip) = promote_shape(size(z.a), size(z.z))
indices(z::Zip) = promote_shape(indices(z.a), indices(z.z))
eltype(::Type{Zip{I,Z}}) where {I,Z} = tuple_type_cons(eltype(I), eltype(Z))
@inline start(z::Zip) = tuple(start(z.a), start(z.z))
@inline function next(z::Zip, st)
    n1 = next(z.a, st[1])
    n2 = next(z.z, st[2])
    (tuple(n1[1], n2[1]...), (n1[2], n2[2]))
end
@inline done(z::Zip, st) = done(z.a,st[1]) | done(z.z,st[2])

iteratorsize(::Type{Zip{I1,I2}}) where {I1,I2} = zip_iteratorsize(iteratorsize(I1),iteratorsize(I2))
iteratoreltype(::Type{Zip{I1,I2}}) where {I1,I2} = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))

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
Base.Iterators.Filter{Base.#isodd,Array{Int64,1}}(isodd, [1, 2, 3, 4, 5])

julia> foreach(println, f)
1
3
5
```
"""
filter(flt, itr) = Filter(flt, itr)

start(f::Filter) = start_filter(f.flt, f.itr)
function start_filter(pred, itr)
    s = start(itr)
    while !done(itr,s)
        v,t = next(itr,s)
        if pred(v)
            return (false, v, t)
        end
        s=t
    end
    (true,)
end

next(f::Filter, s) = advance_filter(f.flt, f.itr, s)
function advance_filter(pred, itr, st)
    _, v, s = st
    while !done(itr,s)
        w,t = next(itr,s)
        if pred(w)
            return v, (false, w, t)
        end
        s=t
    end
    v, (true, v, s)
end

done(f::Filter, s) = s[1]

eltype(::Type{Filter{F,I}}) where {F,I} = eltype(I)
iteratoreltype(::Type{Filter{F,I}}) where {F,I} = iteratoreltype(I)
iteratorsize(::Type{<:Filter}) = SizeUnknown()

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
3-element Array{Any,1}:
 2
 3
 4
```
"""
rest(itr,state) = Rest(itr,state)

start(i::Rest) = i.st
next(i::Rest, st) = next(i.itr, st)
done(i::Rest, st) = done(i.itr, st)

eltype(::Type{Rest{I}}) where {I} = eltype(I)
iteratoreltype(::Type{Rest{I,S}}) where {I,S} = iteratoreltype(I)
rest_iteratorsize(a) = SizeUnknown()
rest_iteratorsize(::IsInfinite) = IsInfinite()
iteratorsize(::Type{Rest{I,S}}) where {I,S} = rest_iteratorsize(iteratorsize(I))


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

start(it::Count) = it.start
next(it::Count, state) = (state, state + it.step)
done(it::Count, state) = false

iteratorsize(::Type{<:Count}) = IsInfinite()

# Take -- iterate through the first n elements

struct Take{I}
    xs::I
    n::Int
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
iteratoreltype(::Type{Take{I}}) where {I} = iteratoreltype(I)
take_iteratorsize(a) = HasLength()
take_iteratorsize(::SizeUnknown) = SizeUnknown()
iteratorsize(::Type{Take{I}}) where {I} = take_iteratorsize(iteratorsize(I))
length(t::Take) = _min_length(t.xs, 1:t.n, iteratorsize(t.xs), HasLength())

start(it::Take) = (it.n, start(it.xs))

function next(it::Take, state)
    n, xs_state = state
    v, xs_state = next(it.xs, xs_state)
    return v, (n - 1, xs_state)
end

function done(it::Take, state)
    n, xs_state = state
    return n <= 0 || done(it.xs, xs_state)
end

# Drop -- iterator through all but the first n elements

struct Drop{I}
    xs::I
    n::Int
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
iteratoreltype(::Type{Drop{I}}) where {I} = iteratoreltype(I)
drop_iteratorsize(::SizeUnknown) = SizeUnknown()
drop_iteratorsize(::Union{HasShape, HasLength}) = HasLength()
drop_iteratorsize(::IsInfinite) = IsInfinite()
iteratorsize(::Type{Drop{I}}) where {I} = drop_iteratorsize(iteratorsize(I))
length(d::Drop) = _diff_length(d.xs, 1:d.n, iteratorsize(d.xs), HasLength())

function start(it::Drop)
    xs_state = start(it.xs)
    for i in 1:it.n
        if done(it.xs, xs_state)
            break
        end

        _, xs_state = next(it.xs, xs_state)
    end
    xs_state
end

next(it::Drop, state) = next(it.xs, state)
done(it::Drop, state) = done(it.xs, state)

# Cycle an iterator forever

struct Cycle{I}
    xs::I
end

"""
    cycle(iter)

An iterator that cycles through `iter` forever.

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
iteratoreltype(::Type{Cycle{I}}) where {I} = iteratoreltype(I)
iteratorsize(::Type{Cycle{I}}) where {I} = IsInfinite()

function start(it::Cycle)
    s = start(it.xs)
    return s, done(it.xs, s)
end

function next(it::Cycle, state)
    s, d = state
    if done(it.xs, s)
        s = start(it.xs)
    end
    v, s = next(it.xs, s)
    return v, (s, false)
end

done(it::Cycle, state) = state[2]


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

start(it::Repeated) = nothing
next(it::Repeated, state) = (it.x, nothing)
done(it::Repeated, state) = false

iteratorsize(::Type{<:Repeated}) = IsInfinite()
iteratoreltype(::Type{<:Repeated}) = HasEltype()


# Product -- cartesian product of iterators
struct ProductIterator{T<:Tuple}
    iterators::T
end

"""
    product(iters...)

Returns an iterator over the product of several iterators. Each generated element is
a tuple whose `i`th element comes from the `i`th argument iterator. The first iterator
changes the fastest.

# Examples
```jldoctest
julia> collect(Iterators.product(1:2,3:5))
2×3 Array{Tuple{Int64,Int64},2}:
 (1, 3)  (1, 4)  (1, 5)
 (2, 3)  (2, 4)  (2, 5)
```
"""
product(iters...) = ProductIterator(iters)

iteratorsize(::Type{ProductIterator{Tuple{}}}) = HasShape()
iteratorsize(::Type{ProductIterator{T}}) where {T<:Tuple} =
    prod_iteratorsize( iteratorsize(tuple_type_head(T)), iteratorsize(ProductIterator{tuple_type_tail(T)}) )

prod_iteratorsize(::Union{HasLength,HasShape}, ::Union{HasLength,HasShape}) = HasShape()
# products can have an infinite iterator
prod_iteratorsize(::IsInfinite, ::IsInfinite) = IsInfinite()
prod_iteratorsize(a, ::IsInfinite) = IsInfinite()
prod_iteratorsize(::IsInfinite, b) = IsInfinite()
prod_iteratorsize(a, b) = SizeUnknown()

size(P::ProductIterator) = _prod_size(P.iterators)
_prod_size(::Tuple{}) = ()
_prod_size(t::Tuple) = (_prod_size1(t[1], iteratorsize(t[1]))..., _prod_size(tail(t))...)
_prod_size1(a, ::HasShape)  = size(a)
_prod_size1(a, ::HasLength) = (length(a),)
_prod_size1(a, A) =
    throw(ArgumentError("Cannot compute size for object of type $(typeof(a))"))

indices(P::ProductIterator) = _prod_indices(P.iterators)
_prod_indices(::Tuple{}) = ()
_prod_indices(t::Tuple) = (_prod_indices1(t[1], iteratorsize(t[1]))..., _prod_indices(tail(t))...)
_prod_indices1(a, ::HasShape)  = indices(a)
_prod_indices1(a, ::HasLength) = (OneTo(length(a)),)
_prod_indices1(a, A) =
    throw(ArgumentError("Cannot compute indices for object of type $(typeof(a))"))

ndims(p::ProductIterator) = length(indices(p))
length(P::ProductIterator) = prod(size(P))
_length(p::ProductIterator) = prod(map(unsafe_length, indices(p)))

iteratoreltype(::Type{ProductIterator{Tuple{}}}) = HasEltype()
iteratoreltype(::Type{ProductIterator{Tuple{I}}}) where {I} = iteratoreltype(I)
function iteratoreltype(::Type{ProductIterator{T}}) where {T<:Tuple}
    I = tuple_type_head(T)
    P = ProductIterator{tuple_type_tail(T)}
    iteratoreltype(I) == EltypeUnknown() ? EltypeUnknown() : iteratoreltype(P)
end

eltype(P::ProductIterator) = _prod_eltype(P.iterators)
_prod_eltype(::Tuple{}) = Tuple{}
_prod_eltype(t::Tuple) = Base.tuple_type_cons(eltype(t[1]),_prod_eltype(tail(t)))

start(::ProductIterator{Tuple{}}) = false
next(::ProductIterator{Tuple{}}, state) = (), true
done(::ProductIterator{Tuple{}}, state) = state

function start(P::ProductIterator)
    iterators = P.iterators
    iter1 = first(iterators)
    state1 = start(iter1)
    d, states, nvalues = _prod_start(tail(iterators))
    d |= done(iter1, state1)
    return (d, (state1, states...), nvalues)
end
function next(P::ProductIterator, state)
    iterators = P.iterators
    d, states, nvalues = state
    iter1 = first(iterators)
    value1, state1 = next(iter1, states[1])
    tailstates = tail(states)
    values = (value1, map(unsafe_get, nvalues)...) # safe if not done(P, state)
    if done(iter1, state1)
        d, tailstates, nvalues = _prod_next(tail(iterators), tailstates, nvalues)
        if !d # only restart iter1 if not completely done
            state1 = start(iter1)
        end
    end
    return values, (d, (state1, tailstates...), nvalues)
end
done(P::ProductIterator, state) = state[1]

_prod_start(iterators::Tuple{}) = false, (), ()
function _prod_start(iterators)
    iter1 = first(iterators)
    state1 = start(iter1)
    d, tailstates, tailnvalues = _prod_start(tail(iterators))
    if done(iter1, state1)
        d = true
        nvalue1 = Nullable{eltype(iter1)}()
    else
        value1, state1 = next(iter1, state1)
        nvalue1 = Nullable{eltype(iter1)}(value1)
    end
    return (d, (state1, tailstates...), (nvalue1, tailnvalues...))
end

_prod_next(iterators::Tuple{}, states, nvalues) = true, (), ()
function _prod_next(iterators, states, nvalues)
    iter1 = first(iterators)
    state1 = first(states)
    if !done(iter1, state1)
        value1, state1 = next(iter1, state1)
        nvalue1 = Nullable{eltype(iter1)}(value1)
        return false, (state1, tail(states)...), (nvalue1, tail(nvalues)...)
    else
        d, tailstates, tailnvalues = _prod_next(tail(iterators), tail(states), tail(nvalues))
        if d # all iterators are done
            nvalue1 = Nullable{eltype(iter1)}()
        else
            value1, state1 = next(iter1, start(iter1)) # iter cannot be done immediately
            nvalue1 = Nullable{eltype(iter1)}(value1)
        end
        return d, (state1, tailstates...), (nvalue1, tailnvalues...)
    end
end

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
iteratoreltype(::Type{Flatten{I}}) where {I} = _flatteneltype(I, iteratoreltype(I))
_flatteneltype(I, ::HasEltype) = iteratoreltype(eltype(I))
_flatteneltype(I, et) = EltypeUnknown()

flatten_iteratorsize(::Union{HasShape, HasLength}, ::Type{<:NTuple{N,Any}}) where {N} = HasLength()
flatten_iteratorsize(::Union{HasShape, HasLength}, ::Type{<:Tuple}) = SizeUnknown()
flatten_iteratorsize(::Union{HasShape, HasLength}, ::Type{<:Number}) = HasLength()
flatten_iteratorsize(a, b) = SizeUnknown()

iteratorsize(::Type{Flatten{I}}) where {I} = flatten_iteratorsize(iteratorsize(I), eltype(I))

function flatten_length(f, T::Type{<:NTuple{N,Any}}) where {N}
    fieldcount(T)*length(f.it)
end
flatten_length(f, ::Type{<:Number}) = length(f.it)
flatten_length(f, T) = throw(ArgumentError(
    "Iterates of the argument to Flatten are not known to have constant length"))
length(f::Flatten{I}) where {I} = flatten_length(f, eltype(I))

function start(f::Flatten)
    local inner, s2
    s = start(f.it)
    d = done(f.it, s)
    # this is a simple way to make this function type stable
    d && throw(ArgumentError("argument to Flatten must contain at least one iterator"))
    while !d
        inner, s = next(f.it, s)
        s2 = start(inner)
        !done(inner, s2) && break
        d = done(f.it, s)
    end
    return s, inner, s2
end

@inline function next(f::Flatten, state)
    s, inner, s2 = state
    val, s2 = next(inner, s2)
    while done(inner, s2) && !done(f.it, s)
        inner, s = next(f.it, s)
        s2 = start(inner)
    end
    return val, (s, inner, s2)
end

@inline function done(f::Flatten, state)
    s, inner, s2 = state
    return done(f.it, s) && done(inner, s2)
end


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


mutable struct PartitionIterator{T}
    c::T
    n::Int
end

eltype(::Type{PartitionIterator{T}}) where {T} = Vector{eltype(T)}
partition_iteratorsize(::HasShape) = HasLength()
partition_iteratorsize(isz) = isz
function iteratorsize(::Type{PartitionIterator{T}}) where {T}
    partition_iteratorsize(iteratorsize(T))
end

function length(itr::PartitionIterator)
    l = length(itr.c)
    return div(l, itr.n) + ((mod(l, itr.n) > 0) ? 1 : 0)
end

start(itr::PartitionIterator) = start(itr.c)

done(itr::PartitionIterator, state) = done(itr.c, state)

function next(itr::PartitionIterator{<:Vector}, state)
    l = state
    r = min(state + itr.n-1, length(itr.c))
    return view(itr.c, l:r), r + 1
end

function next(itr::PartitionIterator, state)
    v = Vector{eltype(itr.c)}(itr.n)
    i = 0
    while !done(itr.c, state) && i < itr.n
        i += 1
        v[i], state = next(itr.c, state)
    end
    return resize!(v, i), state
end

end
