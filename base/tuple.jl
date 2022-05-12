# This file is a part of Julia. License is MIT: https://julialang.org/license

# Document NTuple here where we have everything needed for the doc system
"""
    NTuple{N, T}

A compact way of representing the type for a tuple of length `N` where all elements are of type `T`.

# Examples
```jldoctest
julia> isa((1, 2, 3, 4, 5, 6), NTuple{6, Int})
true
```
"""
NTuple

# convenience function for extracting N from a Tuple (if defined)
# else return `nothing` for anything else given (such as Vararg or other non-sized Union)
_counttuple(::Type{<:NTuple{N,Any}}) where {N} = N
_counttuple(::Type) = nothing

## indexing ##

length(@nospecialize t::Tuple) = nfields(t)
firstindex(@nospecialize t::Tuple) = 1
lastindex(@nospecialize t::Tuple) = length(t)
size(@nospecialize(t::Tuple), d::Integer) = (d == 1) ? length(t) : throw(ArgumentError("invalid tuple dimension $d"))
axes(@nospecialize t::Tuple) = (OneTo(length(t)),)
@eval getindex(@nospecialize(t::Tuple), i::Int) = getfield(t, i, $(Expr(:boundscheck)))
@eval getindex(@nospecialize(t::Tuple), i::Integer) = getfield(t, convert(Int, i), $(Expr(:boundscheck)))
getindex(t::Tuple, r::AbstractArray{<:Any,1}) = (eltype(t)[t[ri] for ri in r]...,)
getindex(t::Tuple, b::AbstractArray{Bool,1}) = length(b) == length(t) ? getindex(t, findall(b)) : throw(BoundsError(t, b))
getindex(t::Tuple, c::Colon) = t

get(t::Tuple, i::Integer, default) = i in 1:length(t) ? getindex(t, i) : default
get(f::Callable, t::Tuple, i::Integer) = i in 1:length(t) ? getindex(t, i) : f()

# returns new tuple; N.B.: becomes no-op if `i` is out-of-bounds

"""
    setindex(c::Tuple, v, i::Integer)

Creates a new tuple similar to `x` with the value at index `i` set to `v`.
Throws a `BoundsError` when out of bounds.

# Examples
```jldoctest
julia> Base.setindex((1, 2, 6), 2, 3) == (1, 2, 2)
true
```
"""
function setindex(x::Tuple, v, i::Integer)
    @boundscheck 1 <= i <= length(x) || throw(BoundsError(x, i))
    @inline
    _setindex(v, i, x...)
end

function _setindex(v, i::Integer, args...)
    @inline
    return ntuple(j -> ifelse(j == i, v, args[j]), length(args))
end


## iterating ##

function iterate(@nospecialize(t::Tuple), i::Int=1)
    @inline
    return (1 <= i <= length(t)) ? (@inbounds t[i], i + 1) : nothing
end

keys(@nospecialize t::Tuple) = OneTo(length(t))

prevind(@nospecialize(t::Tuple), i::Integer) = Int(i)-1
nextind(@nospecialize(t::Tuple), i::Integer) = Int(i)+1

function keys(t::Tuple, t2::Tuple...)
    @inline
    OneTo(_maxlength(t, t2...))
end
_maxlength(t::Tuple) = length(t)
function _maxlength(t::Tuple, t2::Tuple, t3::Tuple...)
    @inline
    max(length(t), _maxlength(t2, t3...))
end

# this allows partial evaluation of bounded sequences of next() calls on tuples,
# while reducing to plain next() for arbitrary iterables.
indexed_iterate(t::Tuple, i::Int, state=1) = (@inline; (getfield(t, i), i+1))
indexed_iterate(a::Array, i::Int, state=1) = (@inline; (a[i], i+1))
function indexed_iterate(I, i)
    x = iterate(I)
    x === nothing && throw(BoundsError(I, i))
    x
end
function indexed_iterate(I, i, state)
    x = iterate(I, state)
    x === nothing && throw(BoundsError(I, i))
    x
end

"""
    Base.rest(collection[, itr_state])

Generic function for taking the tail of `collection`, starting from a specific iteration
state `itr_state`. Return a `Tuple`, if `collection` itself is a `Tuple`, a subtype of
`AbstractVector`, if `collection` is an `AbstractArray`, a subtype of `AbstractString`
if `collection` is an `AbstractString`, and an arbitrary iterator, falling back to
`Iterators.rest(collection[, itr_state])`, otherwise.

Can be overloaded for user-defined collection types to customize the behavior of [slurping
in assignments](@ref destructuring-assignment) in final position, like `a, b... = collection`.

!!! compat "Julia 1.6"
    `Base.rest` requires at least Julia 1.6.

See also: [`first`](@ref first), [`Iterators.rest`](@ref), [`Base.split_rest`](@ref).

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> first, state = iterate(a)
(1, 2)

julia> first, Base.rest(a, state)
(1, [3, 2, 4])
```
"""
function rest end
rest(t::Tuple) = t
rest(t::Tuple, i::Int) = ntuple(x -> getfield(t, x+i-1), length(t)-i+1)
rest(a::Array, i::Int=1) = a[i:end]
rest(a::Core.SimpleVector, i::Int=1) = a[i:end]
rest(itr, state...) = Iterators.rest(itr, state...)

"""
    Base.split_rest(collection, n::Int[, itr_state]) -> (rest_but_n, last_n)

Generic function for splitting the tail of `collection`, starting from a specific iteration
state `itr_state`. Returns a tuple of two new collections. The first one contains all
elements of the tail but the `n` last ones, which make up the second collection.

The type of the first collection generally follows that of [`Base.rest`](@ref), except that
the fallback case is not lazy, but is collected eagerly into a vector.

Can be overloaded for user-defined collection types to customize the behavior of [slurping
in assignments](@ref destructuring-assignment) in non-final position, like `a, b..., c = collection`.

!!! compat "Julia 1.9"
    `Base.split_rest` requires at least Julia 1.9.

See also: [`Base.rest`](@ref).

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Matrix{Int64}:
 1  2
 3  4

julia> first, state = iterate(a)
(1, 2)

julia> first, Base.split_rest(a, 1, state)
(1, ([3, 2], [4]))
```
"""
function split_rest end
function split_rest(itr, n::Int, state...)
    if IteratorSize(itr) == IsInfinite()
        throw(ArgumentError("Cannot split an infinite iterator in the middle."))
    end
    return _split_rest(rest(itr, state...), n)
end
_split_rest(itr, n::Int) = _split_rest(collect(itr), n)
function _check_length_split_rest(len, n)
    len < n && throw(ArgumentError(
        "The iterator only contains $len elements, but at least $n were requested."
    ))
end
function _split_rest(a::Union{AbstractArray, Core.SimpleVector}, n::Int)
    _check_length_split_rest(length(a), n)
    return a[begin:end-n], a[end-n+1:end]
end

split_rest(t::Tuple, n::Int, i=1) = t[i:end-n], t[end-n+1:end]

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
function _tuple_unique_fieldtypes(@nospecialize t)
    @_total_meta
    types = IdSet()
    t´ = unwrap_unionall(t)
    # Given t = Tuple{Vararg{S}} where S<:Real, the various
    # unwrapping/wrapping/va-handling here will return Real
    if t isa Union
        union!(types, _tuple_unique_fieldtypes(rewrap_unionall(t´.a, t)))
        union!(types, _tuple_unique_fieldtypes(rewrap_unionall(t´.b, t)))
    else
        r = Union{}
        for ti in (t´::DataType).parameters
            r = push!(types, rewrap_unionall(unwrapva(ti), t))
        end
    end
    return Core.svec(types...)
end
function _compute_eltype(@nospecialize t)
    @_total_meta # TODO: the compiler shouldn't need this
    types = _tuple_unique_fieldtypes(t)
    return afoldl(types...) do a, b
        # if we've already reached Any, it can't widen any more
        a === Any && return Any
        b === Any && return Any
        return promote_typejoin(a, b)
    end
end

# version of tail that doesn't throw on empty tuples (used in array indexing)
safe_tail(t::Tuple) = tail(t)
safe_tail(t::Tuple{}) = ()

# front (the converse of tail: it skips the last entry)

"""
    front(x::Tuple)::Tuple

Return a `Tuple` consisting of all but the last component of `x`.

See also: [`first`](@ref), [`tail`](@ref Base.tail).

# Examples
```jldoctest
julia> Base.front((1,2,3))
(1, 2)

julia> Base.front(())
ERROR: ArgumentError: Cannot call front on an empty tuple.
```
"""
function front(t::Tuple)
    @inline
    _front(t...)
end
_front() = throw(ArgumentError("Cannot call front on an empty tuple."))
_front(v) = ()
function _front(v, t...)
    @inline
    (v, _front(t...)...)
end

## mapping ##

# 1 argument function
map(f, t::Tuple{})              = ()
map(f, t::Tuple{Any,})          = (@inline; (f(t[1]),))
map(f, t::Tuple{Any, Any})      = (@inline; (f(t[1]), f(t[2])))
map(f, t::Tuple{Any, Any, Any}) = (@inline; (f(t[1]), f(t[2]), f(t[3])))
map(f, t::Tuple)                = (@inline; (f(t[1]), map(f,tail(t))...))
# stop inlining after some number of arguments to avoid code blowup
const Any32{N} = Tuple{Any,Any,Any,Any,Any,Any,Any,Any,
                       Any,Any,Any,Any,Any,Any,Any,Any,
                       Any,Any,Any,Any,Any,Any,Any,Any,
                       Any,Any,Any,Any,Any,Any,Any,Any,
                       Vararg{Any,N}}
const All32{T,N} = Tuple{T,T,T,T,T,T,T,T,
                         T,T,T,T,T,T,T,T,
                         T,T,T,T,T,T,T,T,
                         T,T,T,T,T,T,T,T,
                         Vararg{T,N}}
function map(f, t::Any32)
    n = length(t)
    A = Vector{Any}(undef, n)
    for i=1:n
        A[i] = f(t[i])
    end
    (A...,)
end
# 2 argument function
map(f, t::Tuple{},        s::Tuple{})        = ()
map(f, t::Tuple{Any,},    s::Tuple{Any,})    = (@inline; (f(t[1],s[1]),))
map(f, t::Tuple{Any,Any}, s::Tuple{Any,Any}) = (@inline; (f(t[1],s[1]), f(t[2],s[2])))
function map(f, t::Tuple, s::Tuple)
    @inline
    (f(t[1],s[1]), map(f, tail(t), tail(s))...)
end
function map(f, t::Any32, s::Any32)
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
    @inline
    (f(heads(t1, t2, ts...)...), map(f, tails(t1, t2, ts...)...)...)
end
function map(f, t1::Any32, t2::Any32, ts::Any32...)
    n = length(t1)
    A = Vector{Any}(undef, n)
    for i = 1:n
        A[i] = f(t1[i], t2[i], map(t -> t[i], ts)...)
    end
    (A...,)
end

_foldl_impl(op, init, itr::Tuple) = afoldl(op, init, itr...)

# type-stable padding
fill_to_length(t::NTuple{N,Any}, val, ::Val{N}) where {N} = t
fill_to_length(t::Tuple{}, val, ::Val{1}) = (val,)
fill_to_length(t::Tuple{Any}, val, ::Val{2}) = (t..., val)
fill_to_length(t::Tuple{}, val, ::Val{2}) = (val, val)
#function fill_to_length(t::Tuple, val, ::Val{N}) where {N}
#    @inline
#    return (t..., ntuple(i -> val, N - length(t))...)
#end

# constructing from an iterator

# only define these in Base, to avoid overwriting the constructors
# NOTE: this means this constructor must be avoided in Core.Compiler!
if nameof(@__MODULE__) === :Base

function tuple_type_tail(T::Type)
    @_total_may_throw_meta # TODO: this method is wrong (and not :total_may_throw)
    if isa(T, UnionAll)
        return UnionAll(T.var, tuple_type_tail(T.body))
    elseif isa(T, Union)
        return Union{tuple_type_tail(T.a), tuple_type_tail(T.b)}
    else
        T.name === Tuple.name || throw(MethodError(tuple_type_tail, (T,)))
        if isvatuple(T) && length(T.parameters) == 1
            va = unwrap_unionall(T.parameters[1])::Core.TypeofVararg
            (isdefined(va, :N) && isa(va.N, Int)) || return T
            return Tuple{Vararg{va.T, va.N-1}}
        end
        return Tuple{argtail(T.parameters...)...}
    end
end

(::Type{T})(x::Tuple) where {T<:Tuple} = convert(T, x)  # still use `convert` for tuples

Tuple(x::Ref) = tuple(getindex(x))  # faster than iterator for one element
Tuple(x::Array{T,0}) where {T} = tuple(getindex(x))

(::Type{T})(itr) where {T<:Tuple} = _totuple(T, itr)

_totuple(::Type{Tuple{}}, itr, s...) = ()

function _totuple_err(@nospecialize T)
    @noinline
    throw(ArgumentError("too few elements for tuple type $T"))
end

function _totuple(::Type{T}, itr, s::Vararg{Any,N}) where {T,N}
    @inline
    y = iterate(itr, s...)
    y === nothing && _totuple_err(T)
    t1 = convert(fieldtype(T, 1), y[1])
    # inference may give up in recursive calls, so annotate here to force accurate return type to be propagated
    rT = tuple_type_tail(T)
    ts = _totuple(rT, itr, y[2])::rT
    return (t1, ts...)
end

# use iterative algorithm for long tuples
function _totuple(T::Type{All32{E,N}}, itr) where {E,N}
    len = N+32
    elts = collect(E, Iterators.take(itr,len))
    if length(elts) != len
        _totuple_err(T)
    end
    (elts...,)
end

_totuple(::Type{Tuple{Vararg{E}}}, itr, s...) where {E} = (collect(E, Iterators.rest(itr,s...))...,)

_totuple(::Type{Tuple}, itr, s...) = (collect(Iterators.rest(itr,s...))...,)

# for types that `apply` knows about, just splatting is faster than collecting first
_totuple(::Type{Tuple}, itr::Array) = (itr...,)
_totuple(::Type{Tuple}, itr::SimpleVector) = (itr...,)
_totuple(::Type{Tuple}, itr::NamedTuple) = (itr...,)

end

## find ##

_findfirst_rec(f, i::Int, ::Tuple{}) = nothing
_findfirst_rec(f, i::Int, t::Tuple) = (@inline; f(first(t)) ? i : _findfirst_rec(f, i+1, tail(t)))
function _findfirst_loop(f::Function, t)
    for i in 1:length(t)
        f(t[i]) && return i
    end
    return nothing
end
findfirst(f::Function, t::Tuple) = length(t) < 32 ? _findfirst_rec(f, 1, t) : _findfirst_loop(f, t)

findlast(f::Function, t::Tuple) = length(t) < 32 ? _findlast_rec(f, t) : _findlast_loop(f, t)
function _findlast_rec(f::Function, x::Tuple)
    r = findfirst(f, reverse(x))
    return isnothing(r) ? r : length(x) - r + 1
end
function _findlast_loop(f::Function, t)
    for i in reverse(1:length(t))
        f(t[i]) && return i
    end
    return nothing
end

## filter ##

filter_rec(f, xs::Tuple) = afoldl((ys, x) -> f(x) ? (ys..., x) : ys, (), xs...)

# use Array for long tuples
filter(f, t::Tuple) = length(t) < 32 ? filter_rec(f, t) : Tuple(filter(f, collect(t)))

## comparison ##

isequal(t1::Tuple, t2::Tuple) = length(t1) == length(t2) && _isequal(t1, t2)
_isequal(::Tuple{}, ::Tuple{}) = true
function _isequal(t1::Tuple{Any,Vararg{Any}}, t2::Tuple{Any,Vararg{Any}})
    return isequal(t1[1], t2[1]) && _isequal(tail(t1), tail(t2))
end
function _isequal(t1::Any32, t2::Any32)
    for i = 1:length(t1)
        if !isequal(t1[i], t2[i])
            return false
        end
    end
    return true
end

==(t1::Tuple, t2::Tuple) = (length(t1) == length(t2)) && _eq(t1, t2)
_eq(t1::Tuple{}, t2::Tuple{}) = true
_eq_missing(t1::Tuple{}, t2::Tuple{}) = missing
function _eq(t1::Tuple, t2::Tuple)
    eq = t1[1] == t2[1]
    if eq === false
        return false
    elseif ismissing(eq)
        return _eq_missing(tail(t1), tail(t2))
    else
        return _eq(tail(t1), tail(t2))
    end
end
function _eq_missing(t1::Tuple, t2::Tuple)
    eq = t1[1] == t2[1]
    if eq === false
        return false
    else
        return _eq_missing(tail(t1), tail(t2))
    end
end
function _eq(t1::Any32, t2::Any32)
    anymissing = false
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
function hash(t::Any32, h::UInt)
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
function <(t1::Any32, t2::Any32)
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
function isless(t1::Any32, t2::Any32)
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
isempty(@nospecialize x::Tuple) = false

revargs() = ()
revargs(x, r...) = (revargs(r...)..., x)

reverse(t::Tuple) = revargs(t...)

## specialized reduction ##

prod(x::Tuple{}) = 1
# This is consistent with the regular prod because there is no need for size promotion
# if all elements in the tuple are of system size.
# It is defined here separately in order to support bootstrap, because it's needed earlier
# than the general prod definition is available.
prod(x::Tuple{Int, Vararg{Int}}) = *(x...)

all(x::Tuple{}) = true
all(x::Tuple{Bool}) = x[1]
all(x::Tuple{Bool, Bool}) = x[1]&x[2]
all(x::Tuple{Bool, Bool, Bool}) = x[1]&x[2]&x[3]
# use generic reductions for the rest

any(x::Tuple{}) = false
any(x::Tuple{Bool}) = x[1]
any(x::Tuple{Bool, Bool}) = x[1]|x[2]
any(x::Tuple{Bool, Bool, Bool}) = x[1]|x[2]|x[3]

# equivalent to any(f, t), to be used only in bootstrap
_tuple_any(f::Function, t::Tuple) = _tuple_any(f, false, t...)
function _tuple_any(f::Function, tf::Bool, a, b...)
    @inline
    _tuple_any(f, tf | f(a), b...)
end
_tuple_any(f::Function, tf::Bool) = tf


# a version of `in` esp. for NamedTuple, to make it pure, and not compiled for each tuple length
function sym_in(x::Symbol, @nospecialize itr::Tuple{Vararg{Symbol}})
    @_total_meta
    for y in itr
        y === x && return true
    end
    return false
end
in(x::Symbol, @nospecialize itr::Tuple{Vararg{Symbol}}) = sym_in(x, itr)


"""
    empty(x::Tuple)

Returns an empty tuple, `()`.
"""
empty(@nospecialize x::Tuple) = ()

foreach(f, itr::Tuple) = foldl((_, x) -> (f(x); nothing), itr, init=nothing)
foreach(f, itrs::Tuple...) = foldl((_, xs) -> (f(xs...); nothing), zip(itrs...), init=nothing)
