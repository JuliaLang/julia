## indexing ##

length(t::Tuple) = tuplelen(t)
endof(t::Tuple) = tuplelen(t)
size(t::Tuple, d) = d==1 ? tuplelen(t) : error("invalid tuple dimension")
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
map(f::Callable, t::(Any, Any, Any, Any)) = (f(t[1]), f(t[2]), f(t[3]), f(t[4]))
map(f::Callable, t::Tuple)                = tuple([f(ti) for ti in t]...)
# 2 argument function
map(f::Callable, t::(),        s::())        = ()
map(f::Callable, t::(Any,),    s::(Any,))    = (f(t[1],s[1]),)
map(f::Callable, t::(Any,Any), s::(Any,Any)) = (f(t[1],s[1]), f(t[2],s[2]))
map(f::Callable, t::(Any,Any,Any), s::(Any,Any,Any)) =
    (f(t[1],s[1]), f(t[2],s[2]), f(t[3],s[3]))
map(f::Callable, t::(Any,Any,Any,Any), s::(Any,Any,Any,Any)) =
    (f(t[1],s[1]), f(t[2],s[2]), f(t[3],s[3]), f(t[4],s[4]))
# n argument function
map(f::Callable, ts::Tuple...) = tuple([f(map(t->t[n],ts)...) for n=1:length_checked_equal(ts...)]...)

function length_checked_equal(args...) 
    n = length(args[1])
    for i=2:length(args)
        if length(args[i]) != n
            error("argument dimensions must match")
        end
    end
    n
end

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

reverse(x::Tuple) = (n=length(x); tuple([x[n-k+1] for k=1:n]...))

## specialized reduction ##

sum(x::()) = 0
sum(x::NTuple{1}) = x[1]
sum(x::NTuple{2}) = x[1] + x[2]
sum(x::NTuple{3}) = x[1] + x[2] + x[3]
sum(x::NTuple{4}) = x[1] + x[2] + x[3] + x[4]

prod(x::()) = 1
prod(x::NTuple{1}) = x[1]
prod(x::NTuple{2}) = x[1] * x[2]
prod(x::NTuple{3}) = x[1] * x[2] * x[3]
prod(x::NTuple{4}) = x[1] * x[2] * x[3] * x[4]

max(x::()) = error("max: argument is empty")
max(x::NTuple{1}) = x[1]
max(x::NTuple{2}) = max(x[1], x[2])
max(x::NTuple{3}) = max(max(x[1], x[2]), x[3])
max(x::NTuple{4}) = max(max(max(x[1], x[2]), x[3]), x[4])

min(x::()) = error("min: argument is empty")
min(x::NTuple{1}) = x[1]
min(x::NTuple{2}) = min(x[1], x[2])
min(x::NTuple{3}) = min(min(x[1], x[2]), x[3])
min(x::NTuple{4}) = min(min(min(x[1], x[2]), x[3]), x[4])

all(x::()) = true
all(x::NTuple{1,Bool}) = x[1]
all(x::NTuple{2,Bool}) = x[1] && x[2]
all(x::NTuple{3,Bool}) = x[1] && x[2] && x[3]
all(x::NTuple{4,Bool}) = x[1] && x[2] && x[3] && x[4]

any(x::()) = false
any(x::NTuple{1,Bool}) = x[1]
any(x::NTuple{2,Bool}) = x[1] || x[2]
any(x::NTuple{3,Bool}) = x[1] || x[2] || x[3]
any(x::NTuple{4,Bool}) = x[1] || x[2] || x[3] || x[4]

