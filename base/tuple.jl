# This file is a part of Julia. License is MIT: http://julialang.org/license

# Document NTuple here where we have everything needed for the doc system
"""
    NTuple{N, T}

A compact way of representing the type for a tuple of length `N` where all elements are of type `T`.

```jldoctest
julia> isa((1, 2, 3, 4, 5, 6), NTuple{6, Int})
true
```
"""
NTuple

## indexing ##

length(t::Tuple) = nfields(t)
endof(t::Tuple) = length(t)
size(t::Tuple, d) = d==1 ? length(t) : throw(ArgumentError("invalid tuple dimension $d"))
getindex(t::Tuple, i::Int) = getfield(t, i)
getindex(t::Tuple, i::Real) = getfield(t, convert(Int, i))
getindex(t::Tuple, r::AbstractArray{<:Any,1}) = ([t[ri] for ri in r]...)
getindex(t::Tuple, b::AbstractArray{Bool,1}) = length(b) == length(t) ? getindex(t,find(b)) : throw(BoundsError(t, b))

# returns new tuple; N.B.: becomes no-op if i is out-of-bounds
setindex(x::Tuple, v, i::Integer) = _setindex((), x, v, i::Integer)
function _setindex(y::Tuple, r::Tuple, v, i::Integer)
    @_inline_meta
    _setindex((y..., ifelse(length(y) + 1 == i, v, first(r))), tail(r), v, i)
end
_setindex(y::Tuple, r::Tuple{}, v, i::Integer) = y

## iterating ##

start(t::Tuple) = 1
done(t::Tuple, i::Int) = (length(t) < i)
next(t::Tuple, i::Int) = (t[i], i+1)

eachindex(t::Tuple) = 1:length(t)

function eachindex(t::Tuple, t2::Tuple...)
    @_inline_meta
    1:_maxlength(t, t2...)
end
_maxlength(t::Tuple) = length(t)
function _maxlength(t::Tuple, t2::Tuple, t3::Tuple...)
    @_inline_meta
    max(length(t), _maxlength(t2, t3...))
end

# this allows partial evaluation of bounded sequences of next() calls on tuples,
# while reducing to plain next() for arbitrary iterables.
indexed_next(t::Tuple, i::Int, state) = (t[i], i+1)
indexed_next(a::Array, i::Int, state) = (a[i], i+1)
indexed_next(I, i, state) = done(I,state) ? throw(BoundsError()) : next(I, state)

# Use dispatch to avoid a branch in first
first(::Tuple{}) = throw(ArgumentError("tuple must be non-empty"))
first(t::Tuple) = t[1]

# eltype

eltype(::Type{Tuple{}}) = Bottom
eltype(::Type{<:Tuple{Vararg{E}}}) where {E} = E
function eltype(t::Type{<:Tuple})
    @_pure_meta
    t isa Union && return typejoin(eltype(t.a), eltype(t.b))
    t´ = unwrap_unionall(t)
    r = Union{}
    for ti in t´.parameters
        r = typejoin(r, rewrap_unionall(unwrapva(ti), t))
    end
    return r
end

# version of tail that doesn't throw on empty tuples (used in array indexing)
safe_tail(t::Tuple) = tail(t)
safe_tail(t::Tuple{}) = ()

# front (the converse of tail: it skips the last entry)

function front(t::Tuple)
    @_inline_meta
    _front((), t...)
end
front(::Tuple{}) = throw(ArgumentError("Cannot call front on an empty tuple"))
_front(out, v) = out
function _front(out, v, t...)
    @_inline_meta
    _front((out..., v), t...)
end

## mapping ##

"""
    ntuple(f::Function, n::Integer)

Create a tuple of length `n`, computing each element as `f(i)`,
where `i` is the index of the element.

```jldoctest
julia> ntuple(i -> 2*i, 4)
(2, 4, 6, 8)
```
"""
function ntuple{F}(f::F, n::Integer)
    t = n <= 0  ? () :
        n == 1  ? (f(1),) :
        n == 2  ? (f(1), f(2)) :
        n == 3  ? (f(1), f(2), f(3)) :
        n == 4  ? (f(1), f(2), f(3), f(4)) :
        n == 5  ? (f(1), f(2), f(3), f(4), f(5)) :
        n == 6  ? (f(1), f(2), f(3), f(4), f(5), f(6)) :
        n == 7  ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7)) :
        n == 8  ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8)) :
        n == 9  ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9)) :
        n == 10 ? (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10)) :
        _ntuple(f, n)
    return t
end

_ntuple(f, n) = (@_noinline_meta; ([f(i) for i = 1:n]...))

# inferrable ntuple
ntuple(f, ::Type{Val{0}}) = (@_inline_meta; ())
ntuple(f, ::Type{Val{1}}) = (@_inline_meta; (f(1),))
ntuple(f, ::Type{Val{2}}) = (@_inline_meta; (f(1), f(2)))
ntuple(f, ::Type{Val{3}}) = (@_inline_meta; (f(1), f(2), f(3)))
ntuple(f, ::Type{Val{4}}) = (@_inline_meta; (f(1), f(2), f(3), f(4)))
ntuple(f, ::Type{Val{5}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5)))
ntuple(f, ::Type{Val{6}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6)))
ntuple(f, ::Type{Val{7}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7)))
ntuple(f, ::Type{Val{8}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8)))
ntuple(f, ::Type{Val{9}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9)))
ntuple(f, ::Type{Val{10}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10)))
ntuple(f, ::Type{Val{11}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10), f(11)))
ntuple(f, ::Type{Val{12}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10), f(11), f(12)))
ntuple(f, ::Type{Val{13}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10), f(11), f(12), f(13)))
ntuple(f, ::Type{Val{14}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10), f(11), f(12), f(13), f(14)))
ntuple(f, ::Type{Val{15}}) = (@_inline_meta; (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10), f(11), f(12), f(13), f(14), f(15)))

function ntuple{F,N}(f::F, ::Type{Val{N}})
    Core.typeassert(N, Int)
    _ntuple((), f, Val{N})
end

# Build up the output until it has length N
_ntuple{F,N}(out::NTuple{N,Any}, f::F, ::Type{Val{N}}) = out
function _ntuple{F,N,M}(out::NTuple{M,Any}, f::F, ::Type{Val{N}})
    @_inline_meta
    _ntuple((out..., f(M+1)), f, Val{N})
end

# 1 argument function
map(f, t::Tuple{})              = ()
map(f, t::Tuple{Any,})          = (f(t[1]),)
map(f, t::Tuple{Any, Any})      = (f(t[1]), f(t[2]))
map(f, t::Tuple{Any, Any, Any}) = (f(t[1]), f(t[2]), f(t[3]))
map(f, t::Tuple)                = (@_inline_meta; (f(t[1]), map(f,tail(t))...))
# stop inlining after some number of arguments to avoid code blowup
Any16{N}   = Tuple{Any,Any,Any,Any,Any,Any,Any,Any,
                   Any,Any,Any,Any,Any,Any,Any,Any,Vararg{Any,N}}
All16{T,N} = Tuple{T,T,T,T,T,T,T,T,
                   T,T,T,T,T,T,T,T,Vararg{T,N}}
function map(f, t::Any16)
    n = length(t)
    A = Array{Any,1}(n)
    for i=1:n
        A[i] = f(t[i])
    end
    (A...)
end
# 2 argument function
map(f, t::Tuple{},        s::Tuple{})        = ()
map(f, t::Tuple{Any,},    s::Tuple{Any,})    = (f(t[1],s[1]),)
map(f, t::Tuple{Any,Any}, s::Tuple{Any,Any}) = (f(t[1],s[1]), f(t[2],s[2]))
function map(f, t::Tuple, s::Tuple)
    @_inline_meta
    (f(t[1],s[1]), map(f, tail(t), tail(s))...)
end
function map(f, t::Any16, s::Any16)
    n = length(t)
    A = Array{Any,1}(n)
    for i = 1:n
        A[i] = f(t[i], s[i])
    end
    (A...)
end
# n argument function
heads(ts::Tuple...) = map(t -> t[1], ts)
tails(ts::Tuple...) = map(tail, ts)
map(f, ::Tuple{}...) = ()
function map(f, t1::Tuple, t2::Tuple, ts::Tuple...)
    @_inline_meta
    (f(heads(t1, t2, ts...)...), map(f, tails(t1, t2, ts...)...)...)
end
function map(f, t1::Any16, t2::Any16, ts::Any16...)
    n = length(t1)
    A = Array{Any,1}(n)
    for i = 1:n
        A[i] = f(t1[i], t2[i], map(t -> t[i], ts)...)
    end
    (A...)
end


# type-stable padding
fill_to_length{N}(t::Tuple, val, ::Type{Val{N}}) = _ftl((), val, Val{N}, t...)
_ftl{N}(out::NTuple{N,Any}, val, ::Type{Val{N}}) = out
function _ftl{N}(out::NTuple{N,Any}, val, ::Type{Val{N}}, t...)
    @_inline_meta
    throw(ArgumentError("input tuple of length $(N+length(t)), requested $N"))
end
function _ftl{N}(out, val, ::Type{Val{N}}, t1, t...)
    @_inline_meta
    _ftl((out..., t1), val, Val{N}, t...)
end
function _ftl{N}(out, val, ::Type{Val{N}})
    @_inline_meta
    _ftl((out..., val), val, Val{N})
end

# constructing from an iterator

# only define these in Base, to avoid overwriting the constructors
if isdefined(Main, :Base)

(::Type{T})(x::Tuple) where {T<:Tuple} = convert(T, x)  # still use `convert` for tuples

# resolve ambiguity between preceding and following methods
(::Type{All16{E,N}})(x::Tuple) where {E,N} = convert(All16{E,N}, x)

function (T::Type{All16{E,N}}){E,N}(itr)
    len = N+16
    elts = collect(E, Iterators.take(itr,len))
    if length(elts) != len
        _totuple_err(T)
    end
    (elts...,)
end

(::Type{T})(itr) where {T<:Tuple} = _totuple(T, itr, start(itr))

_totuple(::Type{Tuple{}}, itr, s) = ()

function _totuple_err(T::ANY)
    @_noinline_meta
    throw(ArgumentError("too few elements for tuple type $T"))
end

function _totuple(T, itr, s)
    @_inline_meta
    done(itr, s) && _totuple_err(T)
    v, s = next(itr, s)
    (convert(tuple_type_head(T), v), _totuple(tuple_type_tail(T), itr, s)...)
end

_totuple(::Type{Tuple{Vararg{E}}}, itr, s) where {E} = (collect(E, Iterators.rest(itr,s))...,)

_totuple(::Type{Tuple}, itr, s) = (collect(Iterators.rest(itr,s))...,)

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
all(x::Tuple{Bool}) = x[1]
all(x::Tuple{Bool, Bool}) = x[1]&x[2]
all(x::Tuple{Bool, Bool, Bool}) = x[1]&x[2]&x[3]
# use generic reductions for the rest

any(x::Tuple{}) = false
any(x::Tuple{Bool}) = x[1]
any(x::Tuple{Bool, Bool}) = x[1]|x[2]
any(x::Tuple{Bool, Bool, Bool}) = x[1]|x[2]|x[3]
