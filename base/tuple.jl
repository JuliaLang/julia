# This file is a part of Julia. License is MIT: https://julialang.org/license

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
firstindex(t::Tuple) = 1
lastindex(t::Tuple) = length(t)
size(t::Tuple, d) = (d == 1) ? length(t) : throw(ArgumentError("invalid tuple dimension $d"))
@eval getindex(t::Tuple, i::Int) = getfield(t, i, $(Expr(:boundscheck)))
@eval getindex(t::Tuple, i::Real) = getfield(t, convert(Int, i), $(Expr(:boundscheck)))
getindex(t::Tuple, r::AbstractArray{<:Any,1}) = ([t[ri] for ri in r]...,)
getindex(t::Tuple, b::AbstractArray{Bool,1}) = length(b) == length(t) ? getindex(t, findall(b)) : throw(BoundsError(t, b))

# returns new tuple; N.B.: becomes no-op if i is out-of-bounds
setindex(x::Tuple, v, i::Integer) = (@_inline_meta; _setindex(v, i, x...))
function _setindex(v, i::Integer, first, tail...)
    @_inline_meta
    return (ifelse(i == 1, v, first), _setindex(v, i - 1, tail...)...)
end
_setindex(v, i::Integer) = ()


## iterating ##

start(t::Tuple) = 1
done(t::Tuple, i::Int) = (length(t) < i)
next(t::Tuple, i::Int) = (t[i], i+1)

keys(t::Tuple) = 1:length(t)

prevind(t::Tuple, i::Integer) = Int(i)-1
nextind(t::Tuple, i::Integer) = Int(i)+1

function keys(t::Tuple, t2::Tuple...)
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
indexed_next(I, i, state) = done(I,state) ? throw(BoundsError(I, i)) : next(I, state)

# Use dispatch to avoid a branch in first
first(::Tuple{}) = throw(ArgumentError("tuple must be non-empty"))
first(t::Tuple) = t[1]

# eltype

eltype(::Type{Tuple{}}) = Bottom
function eltype(t::Type{<:Tuple{Vararg{E}}}) where {E}
    if @isdefined(E)
        return E
    else
        # TODO: need to guard against E being miscomputed by subtyping (ref #23017)
        # and compute the result manually in this case
        return _compute_eltype(t)
    end
end
eltype(t::Type{<:Tuple}) = _compute_eltype(t)
function _compute_eltype(t::Type{<:Tuple})
    @_pure_meta
    t isa Union && return promote_typejoin(eltype(t.a), eltype(t.b))
    t´ = unwrap_unionall(t)
    r = Union{}
    for ti in t´.parameters
        r = promote_typejoin(r, rewrap_unionall(unwrapva(ti), t))
    end
    return r
end

# version of tail that doesn't throw on empty tuples (used in array indexing)
safe_tail(t::Tuple) = tail(t)
safe_tail(t::Tuple{}) = ()

# front (the converse of tail: it skips the last entry)

function front(t::Tuple)
    @_inline_meta
    _front(t...)
end
_front() = throw(ArgumentError("Cannot call front on an empty tuple"))
_front(v) = ()
function _front(v, t...)
    @_inline_meta
    (v, _front(t...)...)
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
function ntuple(f::F, n::Integer) where F
    t = n == 0  ? () :
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

function _ntuple(f, n)
    @_noinline_meta
    (n >= 0) || throw(ArgumentError(string("tuple length should be ≥0, got ", n)))
    ([f(i) for i = 1:n]...,)
end

# inferrable ntuple (enough for bootstrapping)
ntuple(f, ::Val{0}) = ()
ntuple(f, ::Val{1}) = (@_inline_meta; (f(1),))
ntuple(f, ::Val{2}) = (@_inline_meta; (f(1), f(2)))
ntuple(f, ::Val{3}) = (@_inline_meta; (f(1), f(2), f(3)))

# 1 argument function
map(f, t::Tuple{})              = ()
map(f, t::Tuple{Any,})          = (f(t[1]),)
map(f, t::Tuple{Any, Any})      = (f(t[1]), f(t[2]))
map(f, t::Tuple{Any, Any, Any}) = (f(t[1]), f(t[2]), f(t[3]))
map(f, t::Tuple)                = (@_inline_meta; (f(t[1]), map(f,tail(t))...))
# stop inlining after some number of arguments to avoid code blowup
const Any16{N} = Tuple{Any,Any,Any,Any,Any,Any,Any,Any,
                       Any,Any,Any,Any,Any,Any,Any,Any,Vararg{Any,N}}
const All16{T,N} = Tuple{T,T,T,T,T,T,T,T,
                         T,T,T,T,T,T,T,T,Vararg{T,N}}
function map(f, t::Any16)
    n = length(t)
    A = Vector{Any}(undef, n)
    for i=1:n
        A[i] = f(t[i])
    end
    (A...,)
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
    A = Vector{Any}(undef, n)
    for i = 1:n
        A[i] = f(t[i], s[i])
    end
    (A...,)
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
    A = Vector{Any}(undef, n)
    for i = 1:n
        A[i] = f(t1[i], t2[i], map(t -> t[i], ts)...)
    end
    (A...,)
end


# type-stable padding
fill_to_length(t::NTuple{N,Any}, val, ::Val{N}) where {N} = t
fill_to_length(t::Tuple{}, val, ::Val{1}) = (val,)
fill_to_length(t::Tuple{Any}, val, ::Val{2}) = (t..., val)
fill_to_length(t::Tuple{}, val, ::Val{2}) = (val, val)
#function fill_to_length(t::Tuple, val, ::Val{N}) where {N}
#    @_inline_meta
#    return (t..., ntuple(i -> val, N - length(t))...)
#end

# constructing from an iterator

# only define these in Base, to avoid overwriting the constructors
# NOTE: this means this constructor must be avoided in Core.Compiler!
if nameof(@__MODULE__) === :Base

(::Type{T})(x::Tuple) where {T<:Tuple} = convert(T, x)  # still use `convert` for tuples

# resolve ambiguity between preceding and following methods
All16{E,N}(x::Tuple) where {E,N} = convert(All16{E,N}, x)

function (T::All16{E,N})(itr) where {E,N}
    len = N+16
    elts = collect(E, Iterators.take(itr,len))
    if length(elts) != len
        _totuple_err(T)
    end
    (elts...,)
end

(::Type{T})(itr) where {T<:Tuple} = _totuple(T, itr, start(itr))

_totuple(::Type{Tuple{}}, itr, s) = ()

function _totuple_err(@nospecialize T)
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

isequal(t1::Tuple, t2::Tuple) = (length(t1) == length(t2)) && _isequal(t1, t2)
_isequal(t1::Tuple{}, t2::Tuple{}) = true
_isequal(t1::Tuple{Any}, t2::Tuple{Any}) = isequal(t1[1], t2[1])
_isequal(t1::Tuple, t2::Tuple) = isequal(t1[1], t2[1]) && _isequal(tail(t1), tail(t2))
function _isequal(t1::Any16, t2::Any16)
    for i = 1:length(t1)
        if !isequal(t1[i], t2[i])
            return false
        end
    end
    return true
end

==(t1::Tuple, t2::Tuple) = (length(t1) == length(t2)) && _eq(t1, t2, false)
_eq(t1::Tuple{}, t2::Tuple{}, anymissing) = anymissing ? missing : true
function _eq(t1::Tuple, t2::Tuple, anymissing)
    eq = t1[1] == t2[1]
    if eq === false
        return false
    else
        return _eq(tail(t1), tail(t2), anymissing | ismissing(eq))
    end
end
function _eq(t1::Any16, t2::Any16, anymissing)
    for i = 1:length(t1)
        eq = (t1[i] == t2[i])
        if ismissing(eq)
            anymissing = true
        elseif !eq
           return false
       end
    end
    return anymissing ? missing : true
end

const tuplehash_seed = UInt === UInt64 ? 0x77cfa1eef01bca90 : 0xf01bca90
hash(::Tuple{}, h::UInt) = h + tuplehash_seed
hash(t::Tuple, h::UInt) = hash(t[1], hash(tail(t), h))
function hash(t::Any16, h::UInt)
    out = h + tuplehash_seed
    for i = length(t):-1:1
        out = hash(t[i], out)
    end
    return out
end

<(::Tuple{}, ::Tuple{}) = false
<(::Tuple{}, ::Tuple) = true
<(::Tuple, ::Tuple{}) = false
function <(t1::Tuple, t2::Tuple)
    a, b = t1[1], t2[1]
    eq = (a == b)
    if ismissing(eq)
        return missing
    elseif !eq
        return a < b
    end
    return tail(t1) < tail(t2)
end
function <(t1::Any16, t2::Any16)
    n1, n2 = length(t1), length(t2)
    for i = 1:min(n1, n2)
        a, b = t1[i], t2[i]
        eq = (a == b)
        if ismissing(eq)
            return missing
        elseif !eq
           return a < b
        end
    end
    return n1 < n2
end

isless(::Tuple{}, ::Tuple{}) = false
isless(::Tuple{}, ::Tuple) = true
isless(::Tuple, ::Tuple{}) = false

"""
    isless(t1::Tuple, t2::Tuple)

Returns true when t1 is less than t2 in lexicographic order.
"""
function isless(t1::Tuple, t2::Tuple)
    a, b = t1[1], t2[1]
    isless(a, b) || (isequal(a, b) && isless(tail(t1), tail(t2)))
end
function isless(t1::Any16, t2::Any16)
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
# TODO: this is inconsistent with the regular sum in cases where the arguments
# require size promotion to system size.
sum(x::Tuple{Any, Vararg{Any}}) = +(x...)

# NOTE: should remove, but often used on array sizes
# TODO: this is inconsistent with the regular prod in cases where the arguments
# require size promotion to system size.
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

"""
    empty(x::Tuple)

Returns an empty tuple, `()`.
"""
empty(x::Tuple) = ()
