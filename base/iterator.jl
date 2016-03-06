# This file is a part of Julia. License is MIT: http://julialang.org/license

isempty(itr) = done(itr, start(itr))

# enumerate

immutable Enumerate{I}
    itr::I
end
enumerate(itr) = Enumerate(itr)

length(e::Enumerate) = length(e.itr)
start(e::Enumerate) = (1, start(e.itr))
function next(e::Enumerate, state)
    n = next(e.itr,state[2])
    (state[1],n[1]), (state[1]+1,n[2])
end
done(e::Enumerate, state) = done(e.itr, state[2])

eltype{I}(::Type{Enumerate{I}}) = Tuple{Int, eltype(I)}

# zip

abstract AbstractZipIterator

immutable Zip1{I} <: AbstractZipIterator
    a::I
end
zip(a) = Zip1(a)
length(z::Zip1) = length(z.a)
eltype{I}(::Type{Zip1{I}}) = Tuple{eltype(I)}
start(z::Zip1) = (start(z.a),)
function next(z::Zip1, st)
    n = next(z.a,st[1])
    return ((n[1],), (n[2],))
end
done(z::Zip1, st) = done(z.a,st[1])

immutable Zip2{I1, I2} <: AbstractZipIterator
    a::I1
    b::I2
end
zip(a, b) = Zip2(a, b)
length(z::Zip2) = min(length(z.a), length(z.b))
eltype{I1,I2}(::Type{Zip2{I1,I2}}) = Tuple{eltype(I1), eltype(I2)}
start(z::Zip2) = (start(z.a), start(z.b))
function next(z::Zip2, st)
    n1 = next(z.a,st[1])
    n2 = next(z.b,st[2])
    return ((n1[1], n2[1]), (n1[2], n2[2]))
end
done(z::Zip2, st) = done(z.a,st[1]) | done(z.b,st[2])

immutable Zip{I, Z<:AbstractZipIterator} <: AbstractZipIterator
    a::I
    z::Z
end
zip(a, b, c...) = Zip(a, zip(b, c...))
length(z::Zip) = min(length(z.a), length(z.z))
tuple_type_cons{S}(::Type{S}, ::Type{Union{}}) = Union{}
function tuple_type_cons{S,T<:Tuple}(::Type{S}, ::Type{T})
    @_pure_meta
    Tuple{S, T.parameters...}
end
eltype{I,Z}(::Type{Zip{I,Z}}) = tuple_type_cons(eltype(I), eltype(Z))
start(z::Zip) = tuple(start(z.a), start(z.z))
function next(z::Zip, st)
    n1 = next(z.a, st[1])
    n2 = next(z.z, st[2])
    (tuple(n1[1], n2[1]...), (n1[2], n2[2]))
end
done(z::Zip, st) = done(z.a,st[1]) | done(z.z,st[2])

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

# Take -- iterate through the first n elements

immutable Take{I}
    xs::I
    n::Int
end
take(xs, n::Int) = Take(xs, n)

eltype{I}(::Type{Take{I}}) = eltype(I)

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
length(p::AbstractProdIterator) = length(p.a)*length(p.b)

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

@inline function next{I1,I2}(p::Prod{I1,I2}, st)
    x = prod_next(p, st)
    ((x[1][1],x[1][2]...), x[2])
end

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

eltype{I}(::IteratorND{I}) = eltype(I)

collect(i::IteratorND) = copy!(Array(eltype(i),size(i)), i)

function collect{I<:IteratorND}(g::Generator{I})
    sz = size(g.iter)
    if length(g.iter) == 0
        return Array(Union{}, sz)
    end
    st = start(g)
    first, st = next(g, st)
    dest = Array(typeof(first), sz)
    dest[1] = first
    return map_to!(g.f, 2, st, dest, g.iter)
end

"""
    @shareiterators

This macro may improve the efficiency of loops that have multiple iterators.
For example,
```
@shareiterators for (IA, IB) in zip(eachindex(A), eachindex(B))
    # body
end
```
generates, by default, a loop with two indexes that need to be
incremented and tested on each iteration. However, if it happens that
the two iterators in the `zip` call have the same value, then it runs
a variant of the loop that uses a single index.

Note this is only recommended for cases where the iterators can be
compared efficiently. The example above demonstrates good usage of
this macro; in contrast,
```
@shareiterators for (a, b) in zip(A, B)
    # body
end
```
would be much worse than the version without `@shareiterators`,
because it would check each element of `A` and `B` before deciding
which variant of the loop to run (and hence pass through `A` and `B`
twice).
"""
macro shareiterators(ex)
    _shareiterators(ex)
end

function _shareiterators(ex::Expr)
    if ex.head == :block
        # Skip to the :for loop
        i = 1
        while i <= length(ex.args) && (a = ex.args[i]; !isa(a, Expr) || a.head != :for)
            i += 1
        end
        i > length(ex.args) && error("expression must be a for loop")
        ex.args[i] = _shareiterators(ex.args[i])
        return ex
    end
    ex.head == :for || error("expression must be a for loop")
    iteration, body = ex.args
    statevars, iterex = iteration.args
    isa(statevars, Symbol) && return ex  # just one variable
    # A couple of sanity checks
    statevars.head == :tuple || error("iteration variables must be expressed as a tuple")
    iterex.head == :call && iterex.args[1] == :zip || error("iterators must be zipped")
    iteratorexs = iterex.args[2:end]
    n = length(statevars.args)
    length(iteratorexs) == n || error("number of items does not match the number of iterators")
    (n < 2 || n > 3) && return ex    # just special-case 2 or 3 iterators
    statesyms = statevars.args
    # Evaluate the iterators
    itersyms = [gensym(string("R", i)) for i = 1:n]
    iterevals = [Expr(:(=), itersyms[i], iteratorexs[i]) for i = 1:n]
    # Prepare a variant using a single iterator
    state1 = gensym(:I)
    body1 = itersub(body, statesyms, state1)
    n == 2 && return esc(quote
        $(iterevals...)
        if $(itersyms[1]) == $(itersyms[2])
            for $state1 in $(itersyms[1])
                $body1
            end
        else
            for $statevars in zip($(itersyms...))
                $body
            end
        end
    end)
    # With 3 iterators, we have to consider pairs as well as the triple
    state12, state13, state23 = gensym(:I12), gensym(:I13), gensym(:I23)
    body12 = itersub(body, statesyms[[1,2]], state12)
    body13 = itersub(body, statesyms[[1,3]], state13)
    body23 = itersub(body, statesyms[[2,3]], state23)
    esc(quote
        $(iterevals...)
        if $(itersyms[1]) == $(itersyms[2]) == $(itersyms[3])
            for $state1 in $(itersyms[1])
                $body1
            end
        elseif $(itersyms[1]) == $(itersyms[2])
            for ($state12, $(statesyms[3])) in zip($(itersyms[1]), $(itersyms[3]))
                $body12
            end
        elseif $(itersyms[1]) == $(itersyms[3])
            for ($state13, $(statesyms[2])) in zip($(itersyms[1]), $(itersyms[2]))
                $body13
            end
        elseif $(itersyms[2]) == $(itersyms[3])
            for ($state23, $(statesyms[1])) in zip($(itersyms[2]), $(itersyms[1]))
                $body23
            end
        else
            for $statevars in zip($(itersyms...))
                $body
            end
        end
    end)
end

itersub(ex, statesyms, replacement) = itersub!(copy(ex), statesyms, replacement)
function itersub!(ex::Expr, statesyms, replacement)
    for i = 1:length(ex.args)
        ex.args[i] = itersub!(ex.args[i], statesyms, replacement)
    end
    ex
end
itersub!(sym::Symbol, statesyms, replacement) = sym in statesyms ? replacement : sym
itersub!(arg, statesyms, replacement) = arg
