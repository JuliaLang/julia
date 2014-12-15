## indexing ##

length(t::Tuple) = tuplelen(t)
endof(t::Tuple) = tuplelen(t)
size(t::Tuple, d) = d==1 ? tuplelen(t) : error("invalid tuple dimension $(d)")
getindex(t::Tuple, i::Int) = tupleref(t, i)
getindex(t::Tuple, i::Real) = tupleref(t, convert(Int, i))
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

eltype{T}(x::(T...)) = T

## mapping ##

ntuple(n::Integer, f::Function) = ntuple(f, n) # TODO: deprecate this?
ntuple(f::Function, n::Integer) =
    n<=0 ? () :
    n==1 ? (f(1),) :
    n==2 ? (f(1),f(2),) :
    n==3 ? (f(1),f(2),f(3),) :
    n==4 ? (f(1),f(2),f(3),f(4),) :
    n==5 ? (f(1),f(2),f(3),f(4),f(5),) :
    tuple(ntuple(n-2,f)..., f(n-1), f(n))

# 0 argument function
map(f::Callable) = f()
# 1 argument function
map(f::Callable, t::())                   = ()
map(f::Callable, t::(Any,))               = (f(t[1]),)
map(f::Callable, t::(Any, Any))           = (f(t[1]), f(t[2]))
map(f::Callable, t::(Any, Any, Any))      = (f(t[1]), f(t[2]), f(t[3]))
map(f::Callable, t::Tuple)                = tuple(f(t[1]), map(f,tail(t))...)
# 2 argument function
map(f::Callable, t::(),        s::())        = ()
map(f::Callable, t::(Any,),    s::(Any,))    = (f(t[1],s[1]),)
map(f::Callable, t::(Any,Any), s::(Any,Any)) = (f(t[1],s[1]), f(t[2],s[2]))
# n argument function
heads() = ()
heads(t::Tuple, ts::Tuple...) = tuple(t[1], heads(ts...)...)
tails() = ()
tails(t::Tuple, ts::Tuple...) = tuple(tail(t), tails(ts...)...)
map(f::Callable, ::(), ts::Tuple...) = ()
map(f::Callable, ts::Tuple...) =
    tuple(f(heads(ts...)...), map(f, tails(ts...)...)...)

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
hash(::(), h::UInt) = h + tuplehash_seed
hash(x::(Any,), h::UInt)    = hash(x[1], hash((), h))
hash(x::(Any,Any), h::UInt) = hash(x[1], hash(x[2], hash((), h)))
hash(x::Tuple, h::UInt)     = hash(x[1], hash(x[2], hash(tail(x), h)))

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

isempty(x::()) = true
isempty(x::Tuple) = false

revargs() = ()
revargs(x, r...) = tuple(revargs(r...)..., x)

reverse(t::Tuple) = revargs(t...)

## specialized reduction ##

# TODO: these definitions cannot yet be combined, since +(x...)
# where x might be any tuple matches too many methods.
sum(x::(Any, Any...)) = +(x...)

# NOTE: should remove, but often used on array sizes
prod(x::()) = 1
prod(x::(Any, Any...)) = *(x...)

all(x::()) = true
all(x::(Any, Any...)) = (&)(x...)

any(x::()) = false
any(x::(Any, Any...)) = |(x...)
