# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Methods for working with Iterators.
"""
module Iterators

# small dance to make this work from Base or Intrinsics
import ..@__MODULE__, ..module_parent
const Base = module_parent(@__MODULE__)
using .Base:
    @inline, Pair, AbstractDict, IndexLinear, IndexCartesian, IndexStyle, AbstractVector, Vector,
    tail, tuple_type_head, tuple_type_tail, tuple_type_cons, SizeUnknown, HasLength, HasShape,
    IsInfinite, EltypeUnknown, HasEltype, OneTo, @propagate_inbounds, Generator, AbstractRange

import .Base:
    first, last,
    isempty, length, size, axes, ndims,
    eltype, iteratorsize, iteratoreltype,
    haskey, keys, values, pairs,
    getindex, setindex!, get, iterate

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

## Reverse-order iteration for arrays and other collections.  Collections
## should implement start/next/done etcetera if possible/practical.
"""
    Iterators.reverse(itr)

Given an iterator `itr`, then `reverse(itr)` is an iterator over the
same collection but in the reverse order.

This iterator is "lazy" in that it does not make a copy of the collection in
order to reverse it; see [`Base.reverse`](@ref) for an eager implementation.

Not all iterator types `T` support reverse-order iteration.  If `T`
doesn't, then iterating over `Iterators.reverse(itr::T)` will throw a [`MethodError`](@ref)
because of the missing [`start`](@ref), [`next`](@ref), and [`done`](@ref)
methods for `Iterators.Reverse{T}`.  (To implement these methods, the original iterator
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
eltype(r::Reverse) = eltype(r.itr)
length(r::Reverse) = length(r.itr)
size(r::Reverse) = size(r.itr)
iteratorsize(r::Reverse) = iteratorsize(r.itr)
iteratoreltype(r::Reverse) = iteratoreltype(r.itr)
last(r::Reverse) = first(r.itr) # the first shall be last
first(r::Reverse) = last(r.itr) # and the last shall be first
isempty(r::Reverse) = isempty(r.itr)

# reverse-order array iterators: assumes more-specialized Reverse for eachindex
@propagate_inbounds function iterate(A::Reverse{<:AbstractArray}, state=(reverse(eachindex(A.itr)),))
    y = iterate(state...)
    y == nothing && return y
    idx, itrs = y
    (A.itr[idx], (state[1], itrs))
end

reverse(R::AbstractRange) = Base.reverse(R) # copying ranges is cheap
reverse(G::Generator) = Generator(G.f, reverse(G.iter))
reverse(r::Reverse) = r.itr
reverse(x::Union{Number,Char}) = x
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
    n == nothing && return n
    (i, n[1]), (i+1, n[2])
end

eltype(::Type{Enumerate{I}}) where {I} = Tuple{Int, eltype(I)}

iteratorsize(::Type{Enumerate{I}}) where {I} = iteratorsize(I)
iteratoreltype(::Type{Enumerate{I}}) where {I} = iteratoreltype(I)

@inline function iterate(r::Reverse{<:Enumerate})
    ri = reverse(r.itr.itr)
    iterate(r, (length(ri), ri))
end
@inline function iterate(r::Reverse{<:Enumerate}, state)
    i, ri, rest = state[1], state[2], tail(tail(state))
    n = iterate(ri, rest...)
    n == nothing && return n
    (i, n[1]), (i-1, ri, n[2])
end

"""
    Iterators.IndexValue(values, keys) <: AbstractDict{eltype(keys), eltype(values)}

Transforms an indexable container into an Dictionary-view of the same data.
Modifying the key-space of the underlying data may invalidate this object.
"""
struct IndexValue{K, V, I, A} <: AbstractDict{K, V}
    data::A
    itr::I
    IndexValue(data::A, itr::I) where {A, I} = new{eltype(I), eltype(A), I, A}(data, itr)
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
pairs(::IndexLinear,    A::AbstractArray) = IndexValue(A, linearindices(A))
pairs(::IndexCartesian, A::AbstractArray) = IndexValue(A, CartesianIndices(axes(A)))

# preserve indexing capabilities for known indexable types
# faster than zip(keys(a), values(a)) for arrays
pairs(A::AbstractArray)  = pairs(IndexCartesian(), A)
pairs(A::AbstractVector) = pairs(IndexLinear(), A)
pairs(tuple::Tuple) = IndexValue(tuple, keys(tuple))
pairs(nt::NamedTuple) = IndexValue(nt, keys(nt))
# pairs(v::IndexValue) = v # listed for reference, but already defined from being an AbstractDict

length(v::IndexValue)  = length(v.itr)
axes(v::IndexValue) = axes(v.itr)
size(v::IndexValue)    = size(v.itr)

@propagate_inbounds function iterate(v::IndexValue, state...)
    x = iterate(v.itr, state...)
    x == nothing && return x
    indx, n = x
    item = v.data[indx]
    return (Pair(indx, item), n)
end

eltype(::Type{IndexValue{K, V}}) where {K, V} = Pair{K, V}

iteratorsize(::Type{IndexValue{<:Any, <:Any, I}}) where {I} = iteratorsize(I)
iteratoreltype(::Type{IndexValue{<:Any, <:Any, I}}) where {I} = iteratoreltype(I)

reverse(v::IndexValue) = IndexValue(v.data, reverse(v.itr))

haskey(v::IndexValue, key) = (key in v.itr)
keys(v::IndexValue) = v.itr
values(v::IndexValue) = v.data
getindex(v::IndexValue, key) = v.data[key]
setindex!(v::IndexValue, value, key) = (v.data[key] = value; v)
get(v::IndexValue, key, default) = get(v.data, key, default)
get(f::Base.Callable, collection::IndexValue, key) = get(f, v.data, key)

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
    n == nothing && return n
    return ((n[1],), n[2])
end

iteratorsize(::Type{Zip1{I}}) where {I} = iteratorsize(I)
iteratoreltype(::Type{Zip1{I}}) where {I} = iteratoreltype(I)

struct Zip2{I1, I2} <: AbstractZipIterator
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)
length(z::Zip2) = _min_length(z.a, z.b, iteratorsize(z.a), iteratorsize(z.b))
size(z::Zip2) = promote_shape(size(z.a), size(z.b))
axes(z::Zip2) = promote_shape(axes(z.a), axes(z.b))
@inline isempty(z::Zip2) = isempty(z.a) | isempty(z.b)
eltype(::Type{Zip2{I1,I2}}) where {I1,I2} = Tuple{eltype(I1), eltype(I2)}
let interleave = (a, b)->((a == nothing || b == nothing) ? nothing : ((a[1],b[1]), (a[2], b[2])))
    global iterate
    @propagate_inbounds iterate(z::Zip2) = interleave(iterate(z.a), iterate(z.b))
    @propagate_inbounds iterate(z::Zip2, st) = interleave(iterate(z.a, st[1]), iterate(z.b, st[2]))
end

iteratorsize(::Type{Zip2{I1,I2}}) where {I1,I2} = zip_iteratorsize(iteratorsize(I1),iteratorsize(I2))
iteratoreltype(::Type{Zip2{I1,I2}}) where {I1,I2} = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))

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
length(z::Zip) = _min_length(z.a, z.z, iteratorsize(z.a), iteratorsize(z.z))
size(z::Zip) = promote_shape(size(z.a), size(z.z))
axes(z::Zip) = promote_shape(axes(z.a), axes(z.z))
eltype(::Type{Zip{I,Z}}) where {I,Z} = tuple_type_cons(eltype(I), eltype(Z))
isempty(z::Zip) = isempty(z.a) | isempty(z.z)
let interleave = (a, b)->((a == nothing || b == nothing) ? nothing : ((a[1],b[1]...), (a[2], b[2])))
    global iterate
    @propagate_inbounds iterate(z::Zip) = interleave(iterate(z.a), iterate(z.z))
    @propagate_inbounds iterate(z::Zip, st) = interleave(iterate(z.a, st[1]), iterate(z.z, st[2]))
end

iteratorsize(::Type{Zip{I1,I2}}) where {I1,I2} = zip_iteratorsize(iteratorsize(I1),iteratorsize(I2))
iteratoreltype(::Type{Zip{I1,I2}}) where {I1,I2} = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))

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
Base.Iterators.Filter{Base.#isodd,Array{Int64,1}}(isodd, [1, 2, 3, 4, 5])

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
iteratoreltype(::Type{Filter{F,I}}) where {F,I} = iteratoreltype(I)
iteratorsize(::Type{<:Filter}) = SizeUnknown()

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
3-element Array{Any,1}:
 2
 3
 4
```
"""
rest(itr,state) = Rest(itr,state)
rest(itr) = itr

@propagate_inbounds iterate(i::Rest, st=i.st) = iterate(i.itr, st)

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

iterate(it::Count, state=it.start) = (state, state + it.step)

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

@propagate_inbounds function iterate(it::Take, state=(it.n,))
    n, rest = state[1], tail(state)
    n <= 0 && return nothing
    y = iterate(it.xs, rest...)
    y == nothing && return nothing
    return y[1], (n - 1, y[2])
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

function iterate(it::Drop)
    y = iterate(it.xs)
    for i in 1:it.n
        y == nothing && return y
        y = iterate(it.xs, y[2])
    end
    y
end
iterate(it::Drop, state) = iterate(it.xs, state)

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

iterate(it::Cycle) = iterate(it.xs)
function iterate(it::Cycle, state)
    y = iterate(it.xs, state)
    y == nothing && return iterate(it)
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

iteratorsize(::Type{<:Repeated}) = IsInfinite()
iteratoreltype(::Type{<:Repeated}) = HasEltype()

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

axes(P::ProductIterator) = _prod_indices(P.iterators)
_prod_indices(::Tuple{}) = ()
_prod_indices(t::Tuple) = (_prod_indices1(t[1], iteratorsize(t[1]))..., _prod_indices(tail(t))...)
_prod_indices1(a, ::HasShape)  = axes(a)
_prod_indices1(a, ::HasLength) = (OneTo(length(a)),)
_prod_indices1(a, A) =
    throw(ArgumentError("Cannot compute indices for object of type $(typeof(a))"))

ndims(p::ProductIterator) = length(axes(p))
length(P::ProductIterator) = prod(size(P))
_length(p::ProductIterator) = prod(map(unsafe_length, axes(p)))

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

iterate(::ProductIterator{Tuple{}}) = (), true
iterate(::ProductIterator{Tuple{}}, state) = nothing

function iterate(P::ProductIterator)
    svs = map(iterate, P.iterators)
    any(x->x==nothing, svs) && return nothing
    map(x->x[1], svs), svs
end

update_svs(::Tuple{}, ::Tuple{}) = nothing
function update_svs(its, svs)
    x = iterate(first(its), first(svs)[2])
    x != nothing && return (x, tail(svs)...)
    svs′ = update_svs(tail(its), tail(svs))
    svs′ == nothing && return nothing
    x = iterate(first(its))
    x == nothing && return nothing
    (x, svs′...)
end

function iterate(P::ProductIterator, svs)
    iterators = P.iterators
    svs′ = update_svs(P.iterators, svs)
    svs′ == nothing && return svs′
    map(x->x[1], svs′), svs′
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

@propagate_inbounds function iterate(f::Flatten, state=())
    if state !== ()
        y = iterate(tail(state)...)
        y !== nothing && return (y[1], (state[1], state[2], y[2]))
    end
    x = (state == () ? iterate(f.it) : iterate(f.it, state[1]))
    x == nothing && return nothing
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

function iterate(itr::PartitionIterator{<:Vector}, state=firstind(itr.c))
    state > endof(itr.c) && return nothing
    l = state
    r = min(state + itr.n-1, length(itr.c))
    return view(itr.c, l:r), r + 1
end

struct IterationCutShort; end

function iterate(itr::PartitionIterator, state...)
    v = Vector{eltype(itr.c)}(uninitialized, itr.n)
    # This is necessary to remember whether we cut the
    # last element short. In such cases, we do return that
    # element, but not the next one
    state == (IterationCutShort(),) && return nothing
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
    i == 0 && return nothing
    return resize!(v, i), y == nothing ? IterationCutShort() : y[2]
end

end
