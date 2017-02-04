# This file is a part of Julia. License is MIT: http://julialang.org/license

# generic operations on associative collections

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::Associative, k) = in(k,keys(d))

function in(p::Pair, a::Associative, valcmp=(==))
    v = get(a,p[1],secret_table_token)
    if v !== secret_table_token
        valcmp(v, p[2]) && return true
    end
    return false
end

function in(p, a::Associative)
    error("""Associative collections only contain Pairs;
             Either look for e.g. A=>B instead, or use the `keys` or `values`
             function if you are looking for a key or value respectively.""")
end

function summary(t::Associative)
    n = length(t)
    return string(typeof(t), " with ", n, (n==1 ? " entry" : " entries"))
end

immutable KeyIterator{T<:Associative}
    dict::T
end
immutable ValueIterator{T<:Associative}
    dict::T
end

summary{T<:Union{KeyIterator,ValueIterator}}(iter::T) =
    string(T.name, " for a ", summary(iter.dict))

show(io::IO, iter::Union{KeyIterator,ValueIterator}) = show(io, collect(iter))

length(v::Union{KeyIterator,ValueIterator}) = length(v.dict)
isempty(v::Union{KeyIterator,ValueIterator}) = isempty(v.dict)
_tt1{A,B}(::Type{Pair{A,B}}) = A
_tt2{A,B}(::Type{Pair{A,B}}) = B
eltype{D}(::Type{KeyIterator{D}}) = _tt1(eltype(D))
eltype{D}(::Type{ValueIterator{D}}) = _tt2(eltype(D))

start(v::Union{KeyIterator,ValueIterator}) = start(v.dict)
done(v::Union{KeyIterator,ValueIterator}, state) = done(v.dict, state)

function next(v::KeyIterator, state)
    n = next(v.dict, state)
    n[1][1], n[2]
end

function next(v::ValueIterator, state)
    n = next(v.dict, state)
    n[1][2], n[2]
end

in(k, v::KeyIterator) = get(v.dict, k, secret_table_token) !== secret_table_token


"""
    keys(a::Associative)

Return an iterator over all keys in a collection.
`collect(keys(a))` returns an array of keys.
Since the keys are stored internally in a hash table,
the order in which they are returned may vary.
But `keys(a)` and `values(a)` both iterate `a` and
return the elements in the same order.

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
eachindex(a::Associative) = KeyIterator(a)

"""
    values(a::Associative)

Return an iterator over all values in a collection.
`collect(values(a))` returns an array of values.
Since the values are stored internally in a hash table,
the order in which they are returned may vary.
But `keys(a)` and `values(a)` both iterate `a` and
return the elements in the same order.

```jldoctest
julia> a = Dict('a'=>2, 'b'=>3)
Dict{Char,Int64} with 2 entries:
  'b' => 3
  'a' => 2

julia> collect(values(a))
2-element Array{Int64,1}:
 3
 2
```
"""
values(a::Associative) = ValueIterator(a)

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
"""
function merge!(d::Associative, others::Associative...)
    for other in others
        for (k,v) in other
            d[k] = v
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

"""
    keytype(type)

Get the key type of an associative collection type. Behaves similarly to [`eltype`](@ref).
"""
keytype{K,V}(::Type{Associative{K,V}}) = K
keytype(a::Associative) = keytype(typeof(a))
keytype{A<:Associative}(::Type{A}) = keytype(supertype(A))

"""
    valtype(type)

Get the value type of an associative collection type. Behaves similarly to [`eltype`](@ref).
"""
valtype{K,V}(::Type{Associative{K,V}}) = V
valtype{A<:Associative}(::Type{A}) = valtype(supertype(A))
valtype(a::Associative) = valtype(typeof(a))

"""
    merge(d::Associative, others::Associative...)

Construct a merged collection from the given collections. If necessary, the
types of the resulting collection will be promoted to accommodate the types of
the merged collections. If the same key is present in another collection, the
value for that key will be the value it has in the last collection listed.

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
function merge(d::Associative, others::Associative...)
    K, V = keytype(d), valtype(d)
    for other in others
        K = promote_type(K, keytype(other))
        V = promote_type(V, valtype(other))
    end
    merge!(Dict{K,V}(), d, others...)
end

function filter!(f, d::Associative)
    badkeys = Array{keytype(d)}(0)
    for (k,v) in d
        # don't delete!(d, k) here, since associative types
        # may not support mutation during iteration
        f(k,v) || push!(badkeys, k)
    end
    for k in badkeys
        delete!(d, k)
    end
    return d
end
function filter(f, d::Associative)
    # don't just do filter!(f, copy(d)): avoid making a whole copy of d
    df = similar(d)
    for (k,v) in d
        if f(k,v)
            df[k] = v
        end
    end
    return df
end

eltype{K,V}(::Type{Associative{K,V}}) = Pair{K,V}

function isequal(l::Associative, r::Associative)
    l === r && return true
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for pair in l
        if !in(pair, r, isequal)
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
    for pair in l
        if !in(pair, r, ==)
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
and value type and thus its `eltype` is always `Pair{Any,Any}`.

See [`Dict`](@ref) for further help.
"""
type ObjectIdDict <: Associative{Any,Any}
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

function setindex!(t::ObjectIdDict, v::ANY, k::ANY)
    if t.ndel >= ((3*length(t.ht))>>2)
        rehash!(t, max(length(t.ht)>>1, 32))
        t.ndel = 0
    end
    t.ht = ccall(:jl_eqtable_put, Array{Any,1}, (Any, Any, Any), t.ht, k, v)
    return t
end

get(t::ObjectIdDict, key::ANY, default::ANY) =
    ccall(:jl_eqtable_get, Any, (Any, Any, Any), t.ht, key, default)

function pop!(t::ObjectIdDict, key::ANY, default::ANY)
    val = ccall(:jl_eqtable_pop, Any, (Any, Any, Any), t.ht, key, default)
    # TODO: this can underestimate `ndel`
    val === default || (t.ndel += 1)
    return val
end

function pop!(t::ObjectIdDict, key::ANY)
    val = pop!(t, key, secret_table_token)
    val !== secret_table_token ? val : throw(KeyError(key))
end

function delete!(t::ObjectIdDict, key::ANY)
    pop!(t, key, secret_table_token)
    t
end

empty!(t::ObjectIdDict) = (t.ht = Vector{Any}(length(t.ht)); t.ndel = 0; t)

_oidd_nextind(a, i) = reinterpret(Int,ccall(:jl_eqtable_nextind, Csize_t, (Any, Csize_t), a, i))

start(t::ObjectIdDict) = _oidd_nextind(t.ht, 0)
done(t::ObjectIdDict, i) = (i == -1)
next(t::ObjectIdDict, i) = (Pair{Any,Any}(t.ht[i+1],t.ht[i+2]), _oidd_nextind(t.ht, i+2))

function length(d::ObjectIdDict)
    n = 0
    for pair in d
        n+=1
    end
    n
end

copy(o::ObjectIdDict) = ObjectIdDict(o)

get!(o::ObjectIdDict, key, default) = (o[key] = get(o, key, default))
