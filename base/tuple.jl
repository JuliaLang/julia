# This file is a part of Julia. License is MIT: http://julialang.org/license

## indexing ##

length(t::Tuple) = nfields(t)
endof(t::Tuple) = length(t)
size(t::Tuple, d) = d==1 ? length(t) : throw(ArgumentError("invalid tuple dimension $d"))
getindex(t::Tuple, i::Int) = getfield(t, i)
getindex(t::Tuple, i::Real) = getfield(t, convert(Int, i))
getindex(t::Tuple, r::AbstractArray) = tuple([t[ri] for ri in r]...)
getindex(t::Tuple, b::AbstractArray{Bool}) = getindex(t,find(b))

## iterating ##

start(t::Tuple) = 1
done(t::Tuple, i::Int) = (length(t) < i)
next(t::Tuple, i::Int) = (t[i], i+1)

# this allows partial evaluation of bounded sequences of next() calls on tuples,
# while reducing to plain next() for arbitrary iterables.
indexed_next(t::Tuple, i::Int, state) = (t[i], i+1)
indexed_next(a::Array, i::Int, state) = (a[i], i+1)
indexed_next(I, i, state) = done(I,state) ? throw(BoundsError()) : next(I, state)

# eltype

eltype{T,_}(::Type{NTuple{_,T}}) = T

## mapping ##

ntuple(f::Function, n::Integer) =
    n<=0 ? () :
    n==1 ? (f(1),) :
    n==2 ? (f(1),f(2),) :
    n==3 ? (f(1),f(2),f(3),) :
    n==4 ? (f(1),f(2),f(3),f(4),) :
    n==5 ? (f(1),f(2),f(3),f(4),f(5),) :
    tuple(ntuple(f,n-5)..., f(n-4), f(n-3), f(n-2), f(n-1), f(n))

ntuple(f, ::Type{Val{0}}) = ()
ntuple(f, ::Type{Val{1}}) = (f(1),)
ntuple(f, ::Type{Val{2}}) = (f(1),f(2))
ntuple(f, ::Type{Val{3}}) = (f(1),f(2),f(3))
ntuple(f, ::Type{Val{4}}) = (f(1),f(2),f(3),f(4))
ntuple(f, ::Type{Val{5}}) = (f(1),f(2),f(3),f(4),f(5))
@generated function ntuple{N}(f, ::Type{Val{N}})
    M = N-5
    :(tuple(ntuple(f, Val{$M})..., f($N-4), f($N-3), f($N-2), f($N-1), f($N)))
end

# 0 argument function
map(f) = f()
# 1 argument function
map(f, t::Tuple{})              = ()
map(f, t::Tuple{Any,})          = (f(t[1]),)
map(f, t::Tuple{Any, Any})      = (f(t[1]), f(t[2]))
map(f, t::Tuple{Any, Any, Any}) = (f(t[1]), f(t[2]), f(t[3]))
map(f, t::Tuple)                = (f(t[1]), map(f,tail(t))...)
# 2 argument function
map(f, t::Tuple{},        s::Tuple{})        = ()
map(f, t::Tuple{Any,},    s::Tuple{Any,})    = (f(t[1],s[1]),)
map(f, t::Tuple{Any,Any}, s::Tuple{Any,Any}) = (f(t[1],s[1]), f(t[2],s[2]))
# n argument function
heads() = ()
heads(t::Tuple, ts::Tuple...) = (t[1], heads(ts...)...)
tails() = ()
tails(t::Tuple, ts::Tuple...) = (tail(t), tails(ts...)...)
map(f, ::Tuple{}, ts::Tuple...) = ()
map(f, ts::Tuple...) = (f(heads(ts...)...), map(f, tails(ts...)...)...)

## comparison ##

function isequal(t1::Tuple, t2::Tuple)
    if length(t1) != length(t2)
        return false
    end
    for i = 1:length(t1)
        if !isequal(t1[i], t2[i])
            return false
        end
    end
    return true
end

function ==(t1::Tuple, t2::Tuple)
    if length(t1) != length(t2)
        return false
    end
    for i = 1:length(t1)
        if !(t1[i] == t2[i])
            return false
        end
    end
    return true
end

const tuplehash_seed = UInt === UInt64 ? 0x77cfa1eef01bca90 : 0xf01bca90
hash( ::Tuple{}, h::UInt)        = h + tuplehash_seed
hash(x::Tuple{Any,}, h::UInt)    = hash(x[1], hash((), h))
hash(x::Tuple{Any,Any}, h::UInt) = hash(x[1], hash(x[2], hash((), h)))
hash(x::Tuple, h::UInt)          = hash(x[1], hash(x[2], hash(tail(tail(x)), h)))

function isless(t1::Tuple, t2::Tuple)
    n1, n2 = length(t1), length(t2)
    for i = 1:min(n1, n2)
        a, b = t1[i], t2[i]
        if !isequal(a, b)
            return isless(a, b)
        end
    end
    return n1 < n2
end

## functions ##

isempty(x::Tuple{}) = true
isempty(x::Tuple) = false

revargs() = ()
revargs(x, r...) = (revargs(r...)..., x)

reverse(t::Tuple) = revargs(t...)

## specialized reduction ##

# TODO: these definitions cannot yet be combined, since +(x...)
# where x might be any tuple matches too many methods.
sum(x::Tuple{Any, Vararg{Any}}) = +(x...)

# NOTE: should remove, but often used on array sizes
prod(x::Tuple{}) = 1
prod(x::Tuple{Any, Vararg{Any}}) = *(x...)

all(x::Tuple{}) = true
all(x::Tuple{Any, Vararg{Any}}) = (&)(x...)

any(x::Tuple{}) = false
any(x::Tuple{Any, Vararg{Any}}) = |(x...)
