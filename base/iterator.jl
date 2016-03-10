# This file is a part of Julia. License is MIT: http://julialang.org/license

isempty(itr) = done(itr, start(itr))

_min_length(a, b, ::IsInfinite, ::IsInfinite) = min(length(a),length(b)) # inherit behaviour, error
_min_length(a, b, A, ::IsInfinite) = length(a)
_min_length(a, b, ::IsInfinite, B) = length(b)
_min_length(a, b, A, B) = min(length(a),length(b))

_diff_length(a, b, A, ::IsInfinite) = 0
_diff_length(a, b, ::IsInfinite, ::IsInfinite) = 0
_diff_length(a, b, ::IsInfinite, B) = length(a) # inherit behaviour, error
_diff_length(a, b, A, B) = max(length(a)-length(b), 0) 

# enumerate

immutable Enumerate{I}
    itr::I
end
enumerate(itr) = Enumerate(itr)

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


immutable Zip1{I} <: AbstractZipIterator
    a::I
end
zip(a) = Zip1(a)
length(z::Zip1) = length(z.a)
size(z::Zip1) = size(z.a)
eltype{I}(::Type{Zip1{I}}) = Tuple{eltype(I)}
@inline start(z::Zip1) = (start(z.a),)
@inline function next(z::Zip1, st)
    n = next(z.a,st[1])
    return ((n[1],), (n[2],))
end
@inline done(z::Zip1, st) = done(z.a,st[1])

iteratorsize{I}(::Type{Zip1{I}}) = iteratorsize(I)
iteratoreltype{I}(::Type{Zip1{I}}) = iteratoreltype(I)

immutable Zip2{I1, I2} <: AbstractZipIterator
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)
length(z::Zip2) = _min_length(z.a, z.b, iteratorsize(z.a), iteratorsize(z.b))
size(z::Zip2) = promote_shape(size(z.a), size(z.b))
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
zip(a, b, c...) = Zip(a, zip(b, c...))
length(z::Zip) = _min_length(z.a, z.z, iteratorsize(z.a), iteratorsize(z.z))
size(z::Zip) = promote_shape(size(z.a), size(z.z))
tuple_type_cons{S}(::Type{S}, ::Type{Union{}}) = Union{}
function tuple_type_cons{S,T<:Tuple}(::Type{S}, ::Type{T})
    @_pure_meta
    Tuple{S, T.parameters...}
end
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
    v, (true,)
end

done(f::Filter, s) = s[1]

eltype{I}(::Type{Filter{I}}) = eltype(I)
iteratoreltype{F,I}(::Type{Filter{F,I}}) = iteratoreltype(I)
iteratorsize{T<:Filter}(::Type{T}) = SizeUnknown()

# Rest -- iterate starting at the given state

immutable Rest{I,S}
    itr::I
    st::S
end
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
countfrom(start::Number, step::Number) = Count(promote(start, step)...)
countfrom(start::Number)               = Count(start, one(start))
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
take(xs, n::Int) = Take(xs, n)

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
drop(xs, n::Int) = Drop(xs, n)

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

# Repeat an object infinitely many times

immutable Repeated{O}
    x::O
end
repeated(x) = Repeated(x)
eltype{O}(::Type{Repeated{O}}) = O
iteratorsize{O}(::Type{Repeated{O}}) = IsInfinite()
start(it::Repeated) = nothing
next(it::Repeated, state) = (it.x, nothing)
done(it::Repeated, state) = false

repeated(x, n::Int) = take(repeated(x), n)

# product

abstract AbstractProdIterator

immutable Prod2{I1, I2} <: AbstractProdIterator
    a::I1
    b::I2
end

"""
    product(iters...)

Returns an iterator over the product of several iterators. Each generated element is
a tuple whose `i`th element comes from the `i`th argument iterator. The first iterator
changes the fastest. Example:

    julia> collect(product(1:2,3:5))
    6-element Array{Tuple{Int64,Int64},1}:
     (1,3)
     (2,3)
     (1,4)
     (2,4)
     (1,5)
     (2,5)
"""
product(a) = Zip1(a)
product(a, b) = Prod2(a, b)
eltype{I1,I2}(::Type{Prod2{I1,I2}}) = Tuple{eltype(I1), eltype(I2)}
iteratoreltype{I1,I2}(::Type{Prod2{I1,I2}}) = and_iteratoreltype(iteratoreltype(I1),iteratoreltype(I2))
length(p::AbstractProdIterator) = length(p.a)*length(p.b)
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

prod_iteratorsize(::Union{HasLength,HasShape}, ::Union{HasLength,HasShape}) = HasLength()
prod_iteratorsize(a, ::IsInfinite) = IsInfinite() # products can have an infinite last iterator (which moves slowest)
prod_iteratorsize(a, b) = SizeUnknown()

_size(p::Prod2) = (length(p.a), length(p.b))
_size(p::Prod) = (length(p.a), _size(p.b)...)

"""
    IteratorND(iter, dims)

Given an iterator `iter` and dimensions tuple `dims`, return an iterator that
yields the same values as `iter`, but with the specified multi-dimensional shape.
For example, this determines the shape of the array returned when `collect` is
applied to this iterator.
"""
immutable IteratorND{I,N}
    iter::I
    dims::NTuple{N,Int}

    function (::Type{IteratorND}){I,N}(iter::I, shape::NTuple{N,Integer})
        li = length(iter)
        if li != prod(shape)
            throw(DimensionMismatch("dimensions $shape must be consistent with iterator length $li"))
        end
        new{I,N}(iter, shape)
    end
    (::Type{IteratorND}){I<:AbstractProdIterator}(p::I) = IteratorND(p, _size(p))
end

start(i::IteratorND) = start(i.iter)
done(i::IteratorND, s) = done(i.iter, s)
next(i::IteratorND, s) = next(i.iter, s)

size(i::IteratorND) = i.dims
length(i::IteratorND) = prod(size(i))
ndims{I,N}(::IteratorND{I,N}) = N
iteratorsize{T<:IteratorND}(::Type{T}) = HasShape()

eltype{I}(::IteratorND{I}) = eltype(I)
iteratoreltype{I}(::Type{IteratorND{I}}) = iteratoreltype(I)
