# This file is a part of Julia. License is MIT: https://julialang.org/license

# generic operations on associative collections

"""
    KeyError(key)

An indexing operation into an `Associative` (`Dict`) or `Set` like object tried to access or
delete a non-existent element.
"""
struct KeyError <: Exception
    key
end

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::Associative, k) = in(k, keys(d))


function summary(t::Associative)
    n = length(t)
    return string(typeof(t), " with ", n, (n==1 ? " entry" : " entries"))
end

struct KeyIterator{T<:Associative}
    dict::T
end
struct PairIterator{T<:Associative}
    dict::T
end

summary(iter::T) where {T<:Union{KeyIterator,PairIterator}} =
    string(T.name, " for a ", summary(iter.dict))

show(io::IO, iter::Union{KeyIterator,PairIterator}) = show(io, collect(iter))

length(v::Union{KeyIterator,PairIterator}) = length(v.dict)
isempty(v::Union{KeyIterator,PairIterator}) = isempty(v.dict)
_tt1(::Type{Pair{A,B}}) where {A,B} = A
_tt2(::Type{Pair{A,B}}) where {A,B} = B
eltype(::Type{KeyIterator{<:Associative{K}}}) where {K} = K
eltype(::Type{PairIterator{<:Associative{K, D}}}) where {K, D} = Pair{K, D}

start(v::Union{KeyIterator,PairIterator}) = start(v.dict)
done(v::Union{KeyIterator,PairIterator}, state) = done(v.dict, state)

function next(v::KeyIterator, state)
    key, state = next(v.dict, state)
    n[1][1], n[2]
end

function next(v::PairIterator, state)
    n = next(v.dict, state)
    n[1][2], n[2]
end

in(k, v::KeyIterator) = get(v.dict, k, secret_table_token) !== secret_table_token

"""
    keys(iterator)

For an iterator or collection that has keys and values (e.g. arrays and dictionaries),
return an iterator over the keys.
"""
function keys end

"""
    keys(a::Associative)

Return an iterator over all keys in an associative collection.
`collect(keys(a))` returns an array of keys.
Since the keys are stored internally in a hash table,
the order in which they are returned may vary.
But `keys(a)` and `values(a)` both iterate `a` and
return the elements in the same order.

# Examples
```jldoctest
julia> a = Dict('a'=>2, 'b'=>3)
Dict{Char,Int64} with 2 entries:
  'b' => 3
  'a' => 2

julia> collect(keys(a))
2-element Array{Char,1}:
 'b'
 'a'
```
"""
keys(a::Associative) = KeyIterator(a)

"""
    pairs(collection)

Return an iterator over `key => value` pairs for any
collection that maps a set of keys to a set of values.
This includes arrays, where the keys are the array indices.
"""
pairs(collection) = Generator(=>, keys(collection), collection)

function copy(a::Associative)
    b = similar(a)
    for (k,v) in a
        b[k] = v
    end
    return b
end

"""
    merge!(d::Associative, others::Associative...)

Update collection with pairs from the other collections.
See also [`merge`](@ref).

# Examples
```jldoctest
julia> d1 = Dict(1 => 2, 3 => 4);

julia> d2 = Dict(1 => 4, 4 => 5);

julia> merge!(d1, d2);

julia> d1
Dict{Int64,Int64} with 3 entries:
  4 => 5
  3 => 4
  1 => 4
```
"""
function merge!(d::Associative, others::Associative...)
    for other in others
        for (k,v) in other
            d[k] = v
        end
    end
    return d
end

"""
    merge!(combine, d::Associative, others::Associative...)

Update collection with pairs from the other collections.
Values with the same key will be combined using the
combiner function.

# Examples
```jldoctest
julia> d1 = Dict(1 => 2, 3 => 4);

julia> d2 = Dict(1 => 4, 4 => 5);

julia> merge!(+, d1, d2);

julia> d1
Dict{Int64,Int64} with 3 entries:
  4 => 5
  3 => 4
  1 => 6

julia> merge!(-, d1, d1);

julia> d1
Dict{Int64,Int64} with 3 entries:
  4 => 0
  3 => 0
  1 => 0
```
"""
function merge!(combine::Function, d::Associative, others::Associative...)
    for other in others
        for (k,v) in other
            d[k] = haskey(d, k) ? combine(d[k], v) : v
        end
    end
    return d
end

# very similar to `merge!`, but accepts any iterable and extends code
# that would otherwise only use `copy!` with arrays.
function copy!(dest::Union{Associative,AbstractSet}, src)
    for x in src
        push!(dest, x)
    end
    return dest
end

eltype(::Type{<:Associative{<:Any, V}}) where {V} = V

"""
    keytype(type)

Get the key type of an associative collection type. Behaves similarly to [`eltype`](@ref).

# Examples
```jldoctest
julia> keytype(Dict(Int32(1) => "foo"))
Int32
```
"""
keytype(::Type{Associative{K,V}}) where {K,V} = K
keytype(a::Associative) = keytype(typeof(a))
keytype(::Type{A}) where {A<:Associative} = keytype(supertype(A))

"""
    pairtype(type)

Get the key-value `Pair` type of an associative collection type. Behaves similarly to
[`eltype`](@ref).

# Examples
```jldoctest
julia> pairtype(Dict(Int32(1) => "foo"))
Pair{Int32,String}
```
"""
pairtype(::Type{<:Associative{K,V}}) where {K,V} = Pair{K, V}
pairtype(a::Associative) = pairtype(typeof(a))

"""
    merge(d::Associative, others::Associative...)

Construct a merged collection from the given collections. If necessary, the
types of the resulting collection will be promoted to accommodate the types of
the merged collections. If the same key is present in another collection, the
value for that key will be the value it has in the last collection listed.

# Examples
```jldoctest
julia> a = Dict("foo" => 0.0, "bar" => 42.0)
Dict{String,Float64} with 2 entries:
  "bar" => 42.0
  "foo" => 0.0

julia> b = Dict("baz" => 17, "bar" => 4711)
Dict{String,Int64} with 2 entries:
  "bar" => 4711
  "baz" => 17

julia> merge(a, b)
Dict{String,Float64} with 3 entries:
  "bar" => 4711.0
  "baz" => 17.0
  "foo" => 0.0

julia> merge(b, a)
Dict{String,Float64} with 3 entries:
  "bar" => 42.0
  "baz" => 17.0
  "foo" => 0.0
```
"""
merge(d::Associative, others::Associative...) =
    merge!(_typeddict(d, others...), others...)

"""
    merge(combine, d::Associative, others::Associative...)

Construct a merged collection from the given collections. If necessary, the
types of the resulting collection will be promoted to accommodate the types of
the merged collections. Values with the same key will be combined using the
combiner function.

# Examples
```jldoctest
julia> a = Dict("foo" => 0.0, "bar" => 42.0)
Dict{String,Float64} with 2 entries:
  "bar" => 42.0
  "foo" => 0.0

julia> b = Dict("baz" => 17, "bar" => 4711)
Dict{String,Int64} with 2 entries:
  "bar" => 4711
  "baz" => 17

julia> merge(+, a, b)
Dict{String,Float64} with 3 entries:
  "bar" => 4753.0
  "baz" => 17.0
  "foo" => 0.0
```
"""
merge(combine::Function, d::Associative, others::Associative...) =
    merge!(combine, _typeddict(d, others...), others...)

promoteK(K) = K
promoteV(V) = V
promoteK(K, d, ds...) = promoteK(promote_type(K, keytype(d)), ds...)
promoteV(V, d, ds...) = promoteV(promote_type(V, eltype(d)), ds...)
function _typeddict(d::Associative, others::Associative...)
    K = promoteK(keytype(d), others...)
    V = promoteV(eltype(d), others...)
    Dict{K,V}(d)
end

"""
    filter!(f, d::Associative)

Update `d`, removing elements for which `f` is `false`.
The function `f` is passed `key=>value` pairs.

# Example
```jldoctest
julia> d = Dict(1=>"a", 2=>"b", 3=>"c")
Dict{Int64,String} with 3 entries:
  2 => "b"
  3 => "c"
  1 => "a"

julia> filter!(p->isodd(p.first), d)
Dict{Int64,String} with 2 entries:
  3 => "c"
  1 => "a"
```
"""
function filter!(f, d::Associative)
    badkeys = Vector{keytype(d)}(0)
    for (k, v) in pairs(d)
        # don't delete!(d, k) here, since associative types
        # may not support mutation during iteration
        f(v) || push!(badkeys, k)
    end
    for k in badkeys
        delete!(d, k)
    end
    return d
end

"""
    filter(f, d::Associative)

Return a copy of `d`, removing elements for which `f` is `false`.
The function `f` is passed `key=>value` pairs.

# Examples
```jldoctest
julia> d = Dict(1=>"a", 2=>"b")
Dict{Int64,String} with 2 entries:
  2 => "b"
  1 => "a"

julia> filter(p->isodd(p.first), d)
Dict{Int64,String} with 1 entry:
  1 => "a"
```
"""
function filter(f, d::Associative)
    # don't just do filter!(f, copy(d)): avoid making a whole copy of d
    df = similar(d)
    for (k, v) in pairs(d)
        if f(v)
            df[k] = v
        end
    end
    return df
end


function isequal(l::Associative, r::Associative)
    l === r && return true
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for pair in pairs(l)
        if !in(pair, pairs(r), isequal)
            return false
        end
    end
    true
end

function ==(l::Associative, r::Associative)
    l === r && return true
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for pair in pairs(l)
        if !in(pair, pairs(r), ==)
            return false
        end
    end
    true
end

const hasha_seed = UInt === UInt64 ? 0x6d35bb51952d5539 : 0x952d5539
function hash(a::Associative, h::UInt)
    hv = hasha_seed
    for (k,v) in a
        hv ⊻= hash(k, hash(v))
    end
    hash(hv, h)
end

function getindex(t::Associative, key)
    v = get(t, key, secret_table_token)
    if v === secret_table_token
        throw(KeyError(key))
    end
    return v
end

# t[k1,k2,ks...] is syntactic sugar for t[(k1,k2,ks...)].  (Note
# that we need to avoid dispatch loops if setindex!(t,v,k) is not defined.)
getindex(t::Associative, k1, k2, ks...) = getindex(t, tuple(k1,k2,ks...))
setindex!(t::Associative, v, k1, k2, ks...) = setindex!(t, v, tuple(k1,k2,ks...))

push!(t::Associative, p::Pair) = setindex!(t, p.second, p.first)
push!(t::Associative, p::Pair, q::Pair) = push!(push!(t, p), q)
push!(t::Associative, p::Pair, q::Pair, r::Pair...) = push!(push!(push!(t, p), q), r...)

# hashing objects by identity

"""
    ObjectIdDict([itr])

`ObjectIdDict()` constructs a hash table where the keys are (always)
object identities.  Unlike `Dict` it is not parameterized on its key
and value type and thus its `eltype` and `keytype` is always `Any`.

See [`Dict`](@ref) for further help.
"""
mutable struct ObjectIdDict <: Associative{Any,Any}
    ht::Vector{Any}
    ndel::Int
    ObjectIdDict() = new(Vector{Any}(32), 0)

    function ObjectIdDict(itr)
        d = ObjectIdDict()
        for (k,v) in itr; d[k] = v; end
        d
    end

    function ObjectIdDict(pairs::Pair...)
        d = ObjectIdDict()
        for (k,v) in pairs; d[k] = v; end
        d
    end

    ObjectIdDict(o::ObjectIdDict) = new(copy(o.ht))
end

similar(d::ObjectIdDict) = ObjectIdDict()

function rehash!(t::ObjectIdDict, newsz = length(t.ht))
    t.ht = ccall(:jl_idtable_rehash, Any, (Any, Csize_t), t.ht, newsz)
    t
end

function sizehint!(t::ObjectIdDict, newsz)
    newsz = _tablesz(newsz*2)  # *2 for keys and values in same array
    oldsz = length(t.ht)
    # grow at least 25%
    if newsz < (oldsz*5)>>2
        return t
    end
    rehash!(t, newsz)
end

function setindex!(t::ObjectIdDict, @nospecialize(v), @nospecialize(k))
    if t.ndel >= ((3*length(t.ht))>>2)
        rehash!(t, max(length(t.ht)>>1, 32))
        t.ndel = 0
    end
    t.ht = ccall(:jl_eqtable_put, Array{Any,1}, (Any, Any, Any), t.ht, k, v)
    return t
end

get(t::ObjectIdDict, @nospecialize(key), @nospecialize(default)) =
    ccall(:jl_eqtable_get, Any, (Any, Any, Any), t.ht, key, default)

function pop!(t::ObjectIdDict, @nospecialize(key), @nospecialize(default))
    val = ccall(:jl_eqtable_pop, Any, (Any, Any, Any), t.ht, key, default)
    # TODO: this can underestimate `ndel`
    val === default || (t.ndel += 1)
    return val
end

function pop!(t::ObjectIdDict, @nospecialize(key))
    val = pop!(t, key, secret_table_token)
    val !== secret_table_token ? val : throw(KeyError(key))
end

function delete!(t::ObjectIdDict, @nospecialize(key))
    pop!(t, key, secret_table_token)
    t
end

function empty!(t::ObjectIdDict)
    resize!(t.ht, 32)
    ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t), t.ht, 0, sizeof(t.ht))
    t.ndel = 0
    return t
end

_oidd_nextind(a, i) = reinterpret(Int,ccall(:jl_eqtable_nextind, Csize_t, (Any, Csize_t), a, i))

start(d::ObjectIdDict) = _oidd_nextind(d.ht, 0)
done(d::ObjectIdDict, i) = (i == -1)
next(d::ObjectIdDict, i) = (d.ht[i+2], _oidd_nextind(d.ht, i+2))

start(d::KeyIterator{ObjectIdDict}) = _oidd_nextind(d.dict.ht, 0)
done(d::KeyIterator{ObjectIdDict}, i) = (i == -1)
next(d::KeyIterator{ObjectIdDict}, i) = (d.dict.ht[i+1], _oidd_nextind(d.dict.ht, i+2))

start(d::PairIterator{ObjectIdDict}) = _oidd_nextind(d.dict.ht, 0)
done(d::PairIterator{ObjectIdDict}, i) = (i == -1)
next(d::PairIterator{ObjectIdDict}, i) = (Pair{Any,Any}(d.dict.ht[i+1],d.dict.ht[i+2]), _oidd_nextind(d.dict.ht, i+2))

function length(d::ObjectIdDict)
    n = 0
    for x in d
        n += 1
    end
    n
end

copy(o::ObjectIdDict) = ObjectIdDict(o)

get!(o::ObjectIdDict, key, default) = (o[key] = get(o, key, default))
