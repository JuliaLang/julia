# This file is a part of Julia. License is MIT: http://julialang.org/license

module Iterators

import Base: start, done, next, isempty, length, size, eltype, iteratorsize, iteratoreltype, indices, ndims

using Base: tuple_type_cons, SizeUnknown, HasLength, HasShape, IsInfinite, EltypeUnknown, HasEltype, OneTo

export enumerate, zip, rest, countfrom, take, drop, cycle, repeated, product, flatten, partition

_min_length(a, b, ::IsInfinite, ::IsInfinite) = min(length(a),length(b)) # inherit behaviour, error
_min_length(a, b, A, ::IsInfinite) = length(a)
_min_length(a, b, ::IsInfinite, B) = length(b)
_min_length(a, b, A, B) = min(length(a),length(b))

_diff_length(a, b, A, ::IsInfinite) = 0
_diff_length(a, b, ::IsInfinite, ::IsInfinite) = 0
_diff_length(a, b, ::IsInfinite, B) = length(a) # inherit behaviour, error
_diff_length(a, b, A, B) = max(length(a)-length(b), 0)

and_iteratorsize{T}(isz::T, ::T) = isz
and_iteratorsize(::HasLength, ::HasShape) = HasLength()
and_iteratorsize(::HasShape, ::HasLength) = HasLength()
and_iteratorsize(a, b) = SizeUnknown()

and_iteratoreltype{T}(iel::T, ::T) = iel
and_iteratoreltype(a, b) = EltypeUnknown()

# enumerate

immutable Enumerate{I}
    itr::I
end

"""
    enumerate(iter)

An iterator that yields `(i, x)` where `i` is a counter starting at 1,
and `x` is the `i`th value from the given iterator. It's useful when
you need not only the values `x` over which you are iterating, but
also the number of iterations so far. Note that `i` may not be valid
for indexing `iter`; it's also possible that `x != iter[i]`, if `iter`
has indices that do not start at 1.

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
start(e::Enumerate) = (1, start(e.itr))
function next(e::Enumerate, state)
    n = next(e.itr,state[2])
    (state[1],n[1]), (state[1]+1,n[2])
end
done(e::Enumerate, state) = done(e.itr, state[2])

eltype{I}(::Type{Enumerate{I}}) = Tuple{Int, eltype(I)}

iteratorsize{I}(::Type{Enumerate{I}}) = iteratorsize(I)
iteratoreltype{I}(::Type{Enumerate{I}}) = iteratoreltype(I)

# zip

abstract AbstractZipIterator

zip_iteratorsize(a, b) = and_iteratorsize(a,b) # as `and_iteratorsize` but inherit `Union{HasLength,IsInfinite}` of the shorter iterator
zip_iteratorsize(::HasLength, ::IsInfinite) = HasLength()
zip_iteratorsize(::HasShape, ::IsInfinite) = HasLength()
zip_iteratorsize(a::IsInfinite, b) = zip_iteratorsize(b,a)
zip_iteratorsize(a::IsInfinite, b::IsInfinite) = IsInfinite()


immutable Zip1{I} <: AbstractZipIterator
    a::I
end
zip(a) = Zip1(a)
length(z::Zip1) = length(z.a)
size(z::Zip1) = size(z.a)
indices(z::Zip1) = indices(z.a)
eltype{I}(::Type{Zip1{I}}) = Tuple{eltype(I)}
@inline start(z::Zip1) = start(z.a)
@inline function next(z::Zip1, st)
    n = next(z.a,st)
    return ((n[1],), n[2])
end
@inline done(z::Zip1, st) = done(z.a,st)

iteratorsize{I}(::Type{Zip1{I}}) = iteratorsize(I)
iteratoreltype{I}(::Type{Zip1{I}}) = iteratoreltype(I)

immutable Zip2{I1, I2} <: AbstractZipIterator
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)
length(z::Zip2) = _min_length(z.a, z.b, iteratorsize(z.a), iteratorsize(z.b))
size(z::Zip2) = promote_shape(size(z.a), size(z.b))
indices(z::Zip2) = promote_shape(indices(z.a), indices(z.b))
eltype{I1,I2}(::Type{Zip2{I1,I2}}) = Tuple{eltype(I1), eltype(I2)}
@inline start(z::Zip2) = (start(z.a), start(z.b))
@inline function next(z::Zip2, st)
    n1 = next(z.a,st[1])
    n2 = next(z.b,st[2])
    return ((n1[1], n2[1]), (n1[2], n2[2]))
end
@inline done(z::Zip2, st) = done(z.a,st[1]) | done(z.b,st[2])

iteratorsize{I1,I2}(::Type{Zip2{I1,I2}}) = zip_iteratorsize(iteratorsize(I1),iteratorsize(I2))
iteratoreltype{I1,I2}(::Type{Zip2{I1,I2}}) = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))

immutable Zip{I, Z<:AbstractZipIterator} <: AbstractZipIterator
    a::I
    z::Z
end

"""
    zip(iters...)

For a set of iterable objects, returns an iterable of tuples, where the `i`th tuple contains
the `i`th component of each input iterable.

Note that [`zip`](@ref) is its own inverse: `collect(zip(zip(a...)...)) == collect(a)`.

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
eltype{I,Z}(::Type{Zip{I,Z}}) = tuple_type_cons(eltype(I), eltype(Z))
@inline start(z::Zip) = tuple(start(z.a), start(z.z))
@inline function next(z::Zip, st)
    n1 = next(z.a, st[1])
    n2 = next(z.z, st[2])
    (tuple(n1[1], n2[1]...), (n1[2], n2[2]))
end
@inline done(z::Zip, st) = done(z.a,st[1]) | done(z.z,st[2])

iteratorsize{I1,I2}(::Type{Zip{I1,I2}}) = zip_iteratorsize(iteratorsize(I1),iteratorsize(I2))
iteratoreltype{I1,I2}(::Type{Zip{I1,I2}}) = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))

# filter

immutable Filter{F,I}
    flt::F
    itr::I
end

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

eltype{F,I}(::Type{Filter{F,I}}) = eltype(I)
iteratoreltype{F,I}(::Type{Filter{F,I}}) = iteratoreltype(I)
iteratorsize{T<:Filter}(::Type{T}) = SizeUnknown()

# Rest -- iterate starting at the given state

immutable Rest{I,S}
    itr::I
    st::S
end

"""
    rest(iter, state)

An iterator that yields the same elements as `iter`, but starting at the given `state`.
"""
rest(itr,state) = Rest(itr,state)

start(i::Rest) = i.st
next(i::Rest, st) = next(i.itr, st)
done(i::Rest, st) = done(i.itr, st)

eltype{I}(::Type{Rest{I}}) = eltype(I)
iteratoreltype{I,S}(::Type{Rest{I,S}}) = iteratoreltype(I)
rest_iteratorsize(a) = SizeUnknown()
rest_iteratorsize(::IsInfinite) = IsInfinite()
iteratorsize{I,S}(::Type{Rest{I,S}}) = rest_iteratorsize(iteratorsize(I))


# Count -- infinite counting

immutable Count{S<:Number}
    start::S
    step::S
end

"""
    countfrom(start=1, step=1)

An iterator that counts forever, starting at `start` and incrementing by `step`.
"""
countfrom(start::Number, step::Number) = Count(promote(start, step)...)
countfrom(start::Number)               = Count(start, oneunit(start))
countfrom()                            = Count(1, 1)

eltype{S}(::Type{Count{S}}) = S

start(it::Count) = it.start
next(it::Count, state) = (state, state + it.step)
done(it::Count, state) = false

iteratorsize{S}(::Type{Count{S}}) = IsInfinite()

# Take -- iterate through the first n elements

immutable Take{I}
    xs::I
    n::Int
end

"""
    take(iter, n)

An iterator that generates at most the first `n` elements of `iter`.

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

eltype{I}(::Type{Take{I}}) = eltype(I)
iteratoreltype{I}(::Type{Take{I}}) = iteratoreltype(I)
take_iteratorsize(a) = HasLength()
take_iteratorsize(::SizeUnknown) = SizeUnknown()
iteratorsize{I}(::Type{Take{I}}) = take_iteratorsize(iteratorsize(I))
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

immutable Drop{I}
    xs::I
    n::Int
end

"""
    drop(iter, n)

An iterator that generates all but the first `n` elements of `iter`.

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

eltype{I}(::Type{Drop{I}}) = eltype(I)
iteratoreltype{I}(::Type{Drop{I}}) = iteratoreltype(I)
drop_iteratorsize(::SizeUnknown) = SizeUnknown()
drop_iteratorsize(::Union{HasShape, HasLength}) = HasLength()
drop_iteratorsize(::IsInfinite) = IsInfinite()
iteratorsize{I}(::Type{Drop{I}}) = drop_iteratorsize(iteratorsize(I))
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

immutable Cycle{I}
    xs::I
end

"""
    cycle(iter)

An iterator that cycles through `iter` forever.
"""
cycle(xs) = Cycle(xs)

eltype{I}(::Type{Cycle{I}}) = eltype(I)
iteratoreltype{I}(::Type{Cycle{I}}) = iteratoreltype(I)
iteratorsize{I}(::Type{Cycle{I}}) = IsInfinite()

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

immutable Repeated{O}
    x::O
end
repeated(x) = Repeated(x)

"""
    repeated(x[, n::Int])

An iterator that generates the value `x` forever. If `n` is specified, generates `x` that
many times (equivalent to `take(repeated(x), n)`).

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

eltype{O}(::Type{Repeated{O}}) = O

start(it::Repeated) = nothing
next(it::Repeated, state) = (it.x, nothing)
done(it::Repeated, state) = false

iteratorsize{O}(::Type{Repeated{O}}) = IsInfinite()
iteratoreltype{O}(::Type{Repeated{O}}) = HasEltype()


# Product -- cartesian product of iterators

abstract AbstractProdIterator

length(p::AbstractProdIterator) = prod(size(p))
_length(p::AbstractProdIterator) = prod(map(unsafe_length, indices(p)))
size(p::AbstractProdIterator) = _prod_size(p.a, p.b, iteratorsize(p.a), iteratorsize(p.b))
indices(p::AbstractProdIterator) = _prod_indices(p.a, p.b, iteratorsize(p.a), iteratorsize(p.b))
ndims(p::AbstractProdIterator) = length(indices(p))

# generic methods to handle size of Prod* types
_prod_size(a, ::HasShape)  = size(a)
_prod_size(a, ::HasLength) = (length(a), )
_prod_size(a, A) =
    throw(ArgumentError("Cannot compute size for object of type $(typeof(a))"))
_prod_size(a, b, ::HasLength, ::HasLength)  = (length(a),  length(b))
_prod_size(a, b, ::HasLength, ::HasShape)   = (length(a),  size(b)...)
_prod_size(a, b, ::HasShape,  ::HasLength)  = (size(a)..., length(b))
_prod_size(a, b, ::HasShape,  ::HasShape)   = (size(a)..., size(b)...)
_prod_size(a, b, A, B) =
    throw(ArgumentError("Cannot construct size for objects of types $(typeof(a)) and $(typeof(b))"))

_prod_indices(a, ::HasShape)  = indices(a)
_prod_indices(a, ::HasLength) = (OneTo(length(a)), )
_prod_indices(a, A) =
    throw(ArgumentError("Cannot compute indices for object of type $(typeof(a))"))
_prod_indices(a, b, ::HasLength, ::HasLength)  = (OneTo(length(a)),  OneTo(length(b)))
_prod_indices(a, b, ::HasLength, ::HasShape)   = (OneTo(length(a)),  indices(b)...)
_prod_indices(a, b, ::HasShape,  ::HasLength)  = (indices(a)..., OneTo(length(b)))
_prod_indices(a, b, ::HasShape,  ::HasShape)   = (indices(a)..., indices(b)...)
_prod_indices(a, b, A, B) =
    throw(ArgumentError("Cannot construct indices for objects of types $(typeof(a)) and $(typeof(b))"))

# one iterator
immutable Prod1{I} <: AbstractProdIterator
    a::I
end
product(a) = Prod1(a)

eltype{I}(::Type{Prod1{I}}) = Tuple{eltype(I)}
size(p::Prod1) = _prod_size(p.a, iteratorsize(p.a))
indices(p::Prod1) = _prod_indices(p.a, iteratorsize(p.a))

@inline start(p::Prod1) = start(p.a)
@inline function next(p::Prod1, st)
    n, st = next(p.a, st)
    (n, ), st
end
@inline done(p::Prod1, st) = done(p.a, st)

iteratoreltype{I}(::Type{Prod1{I}}) = iteratoreltype(I)
iteratorsize{I}(::Type{Prod1{I}}) = iteratorsize(I)

# two iterators
immutable Prod2{I1, I2} <: AbstractProdIterator
    a::I1
    b::I2
end

"""
    product(iters...)

Returns an iterator over the product of several iterators. Each generated element is
a tuple whose `i`th element comes from the `i`th argument iterator. The first iterator
changes the fastest. Example:

```jldoctest
julia> collect(Iterators.product(1:2,3:5))
2×3 Array{Tuple{Int64,Int64},2}:
 (1, 3)  (1, 4)  (1, 5)
 (2, 3)  (2, 4)  (2, 5)
```
"""
product(a, b) = Prod2(a, b)

eltype{I1,I2}(::Type{Prod2{I1,I2}}) = Tuple{eltype(I1), eltype(I2)}

iteratoreltype{I1,I2}(::Type{Prod2{I1,I2}}) = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))
iteratorsize{I1,I2}(::Type{Prod2{I1,I2}}) = prod_iteratorsize(iteratorsize(I1),iteratorsize(I2))

function start(p::AbstractProdIterator)
    s1, s2 = start(p.a), start(p.b)
    s1, s2, Nullable{eltype(p.b)}(), (done(p.a,s1) || done(p.b,s2))
end

@inline function prod_next(p, st)
    s1, s2 = st[1], st[2]
    v1, s1 = next(p.a, s1)

    nv2 = st[3]
    if isnull(nv2)
        v2, s2 = next(p.b, s2)
    else
        v2 = nv2.value
    end

    if done(p.a, s1)
        return (v1,v2), (start(p.a), s2, oftype(nv2,nothing), done(p.b,s2))
    end
    return (v1,v2), (s1, s2, Nullable(v2), false)
end

@inline next(p::Prod2, st) = prod_next(p, st)
@inline done(p::AbstractProdIterator, st) = st[4]

# n iterators
immutable Prod{I1, I2<:AbstractProdIterator} <: AbstractProdIterator
    a::I1
    b::I2
end
product(a, b, c...) = Prod(a, product(b, c...))

eltype{I1,I2}(::Type{Prod{I1,I2}}) = tuple_type_cons(eltype(I1), eltype(I2))

iteratoreltype{I1,I2}(::Type{Prod{I1,I2}}) = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))
iteratorsize{I1,I2}(::Type{Prod{I1,I2}}) = prod_iteratorsize(iteratorsize(I1),iteratorsize(I2))

@inline function next{I1,I2}(p::Prod{I1,I2}, st)
    x = prod_next(p, st)
    ((x[1][1],x[1][2]...), x[2])
end

prod_iteratorsize(::Union{HasLength,HasShape}, ::Union{HasLength,HasShape}) = HasShape()
# products can have an infinite iterator
prod_iteratorsize(::IsInfinite, ::IsInfinite) = IsInfinite()
prod_iteratorsize(a, ::IsInfinite) = IsInfinite()
prod_iteratorsize(::IsInfinite, b) = IsInfinite()
prod_iteratorsize(a, b) = SizeUnknown()


# flatten an iterator of iterators

immutable Flatten{I}
    it::I
end

"""
    flatten(iter)

Given an iterator that yields iterators, return an iterator that yields the
elements of those iterators.
Put differently, the elements of the argument iterator are concatenated. Example:

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

eltype{I}(::Type{Flatten{I}}) = eltype(eltype(I))
iteratorsize{I}(::Type{Flatten{I}}) = SizeUnknown()
iteratoreltype{I}(::Type{Flatten{I}}) = _flatteneltype(I, iteratoreltype(I))
_flatteneltype(I, ::HasEltype) = iteratoreltype(eltype(I))
_flatteneltype(I, et) = EltypeUnknown()

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

```jldoctest
julia> collect(Iterators.partition([1,2,3,4,5], 2))
3-element Array{Array{Int64,1},1}:
 [1, 2]
 [3, 4]
 [5]
```
"""
partition{T}(c::T, n::Integer) = PartitionIterator{T}(c, Int(n))


type PartitionIterator{T}
    c::T
    n::Int
end

eltype{T}(::Type{PartitionIterator{T}}) = Vector{eltype(T)}

function length(itr::PartitionIterator)
    l = length(itr.c)
    return div(l, itr.n) + ((mod(l, itr.n) > 0) ? 1 : 0)
end

start(itr::PartitionIterator) = start(itr.c)

done(itr::PartitionIterator, state) = done(itr.c, state)

function next{T<:Vector}(itr::PartitionIterator{T}, state)
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
