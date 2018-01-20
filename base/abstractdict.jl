# This file is a part of Julia. License is MIT: https://julialang.org/license

# generic operations on dictionaries

"""
    KeyError(key)

An indexing operation into an `AbstractDict` (`Dict`) or `Set` like object tried to access or
delete a non-existent element.
"""
struct KeyError <: Exception
    key
end

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::AbstractDict, k) = in(k, keys(d))

function in(p::Pair, a::AbstractDict, valcmp=(==))
    v = get(a,p[1],secret_table_token)
    if v !== secret_table_token
        valcmp(v, p[2]) && return true
    end
    return false
end

function in(p, a::AbstractDict)
    error("""AbstractDict collections only contain Pairs;
             Either look for e.g. A=>B instead, or use the `keys` or `values`
             function if you are looking for a key or value respectively.""")
end

function summary(t::AbstractDict)
    n = length(t)
    return string(typeof(t), " with ", n, (n==1 ? " entry" : " entries"))
end

struct KeySet{K, T <: AbstractDict{K}} <: AbstractSet{K}
    dict::T
end
KeySet(dict::AbstractDict) = KeySet{keytype(dict), typeof(dict)}(dict)

struct ValueIterator{T<:AbstractDict}
    dict::T
end

summary(iter::T) where {T<:Union{KeySet,ValueIterator}} =
    string(T.name, " for a ", summary(iter.dict))

show(io::IO, iter::Union{KeySet,ValueIterator}) = show(io, collect(iter))

length(v::Union{KeySet,ValueIterator}) = length(v.dict)
isempty(v::Union{KeySet,ValueIterator}) = isempty(v.dict)
_tt2(::Type{Pair{A,B}}) where {A,B} = B
eltype(::Type{ValueIterator{D}}) where {D} = _tt2(eltype(D))

start(v::Union{KeySet,ValueIterator}) = start(v.dict)
done(v::Union{KeySet,ValueIterator}, state) = done(v.dict, state)

function next(v::KeySet, state)
    n = next(v.dict, state)
    n[1][1], n[2]
end

function next(v::ValueIterator, state)
    n = next(v.dict, state)
    n[1][2], n[2]
end

in(k, v::KeySet) = get(v.dict, k, secret_table_token) !== secret_table_token

"""
    keys(iterator)

For an iterator or collection that has keys and values (e.g. arrays and dictionaries),
return an iterator over the keys.
"""
function keys end

"""
    keys(a::AbstractDict)

Return an iterator over all keys in a dictionary.
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
keys(a::AbstractDict) = KeySet(a)

"""
    values(a::AbstractDict)

Return an iterator over all values in a collection.
`collect(values(a))` returns an array of values.
Since the values are stored internally in a hash table,
the order in which they are returned may vary.
But `keys(a)` and `values(a)` both iterate `a` and
return the elements in the same order.

# Examples
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
values(a::AbstractDict) = ValueIterator(a)

"""
    pairs(collection)

Return an iterator over `key => value` pairs for any
collection that maps a set of keys to a set of values.
This includes arrays, where the keys are the array indices.
"""
pairs(collection) = Generator(=>, keys(collection), values(collection))

pairs(a::AbstractDict) = a

"""
    empty(a::AbstractDict, [index_type=keytype(a)], [value_type=valtype(a)])

Create an empty `AbstractDict` container which can accept indices of type `index_type` and
values of type `value_type`. The second and third arguments are optional and default to the
input's `keytype` and `valtype`, respectively. (If only one of the two types is specified,
it is assumed to be the `value_type`, and the `index_type` we default to `keytype(a)`).

Custom `AbstractDict` subtypes may choose which specific dictionary type is best suited to
return for the given index and value types, by specializing on the three-argument signature.
The default is to return an empty `Dict`.
"""
empty(a::AbstractDict) = empty(a, keytype(a), valtype(a))
empty(a::AbstractDict, ::Type{V}) where {V} = empty(a, keytype(a), V) # Note: this is the form which makes sense for `Vector`.

function copy(a::AbstractDict)
    b = empty(a)
    for (k,v) in a
        b[k] = v
    end
    return b
end

"""
    merge!(d::AbstractDict, others::AbstractDict...)

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
function merge!(d::AbstractDict, others::AbstractDict...)
    for other in others
        for (k,v) in other
            d[k] = v
        end
    end
    return d
end

"""
    merge!(combine, d::AbstractDict, others::AbstractDict...)

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
function merge!(combine::Function, d::AbstractDict, others::AbstractDict...)
    for other in others
        for (k,v) in other
            d[k] = haskey(d, k) ? combine(d[k], v) : v
        end
    end
    return d
end

"""
    keytype(type)

Get the key type of an dictionary type. Behaves similarly to [`eltype`](@ref).

# Examples
```jldoctest
julia> keytype(Dict(Int32(1) => "foo"))
Int32
```
"""
keytype(::Type{AbstractDict{K,V}}) where {K,V} = K
keytype(a::AbstractDict) = keytype(typeof(a))
keytype(::Type{A}) where {A<:AbstractDict} = keytype(supertype(A))

"""
    valtype(type)

Get the value type of an dictionary type. Behaves similarly to [`eltype`](@ref).

# Examples
```jldoctest
julia> valtype(Dict(Int32(1) => "foo"))
String
```
"""
valtype(::Type{AbstractDict{K,V}}) where {K,V} = V
valtype(::Type{A}) where {A<:AbstractDict} = valtype(supertype(A))
valtype(a::AbstractDict) = valtype(typeof(a))

"""
    merge(d::AbstractDict, others::AbstractDict...)

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
merge(d::AbstractDict, others::AbstractDict...) =
    merge!(_typeddict(d, others...), others...)

"""
    merge(combine, d::AbstractDict, others::AbstractDict...)

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
merge(combine::Function, d::AbstractDict, others::AbstractDict...) =
    merge!(combine, _typeddict(d, others...), others...)

promoteK(K) = K
promoteV(V) = V
promoteK(K, d, ds...) = promoteK(promote_type(K, keytype(d)), ds...)
promoteV(V, d, ds...) = promoteV(promote_type(V, valtype(d)), ds...)
function _typeddict(d::AbstractDict, others::AbstractDict...)
    K = promoteK(keytype(d), others...)
    V = promoteV(valtype(d), others...)
    Dict{K,V}(d)
end

"""
    filter!(f, d::AbstractDict)

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
function filter!(f, d::AbstractDict)
    badkeys = Vector{keytype(d)}()
    try
        for pair in d
            # don't delete!(d, k) here, since dictionary types
            # may not support mutation during iteration
            f(pair) || push!(badkeys, pair.first)
        end
    catch e
        return filter!_dict_deprecation(e, f, d)
    end
    for k in badkeys
        delete!(d, k)
    end
    return d
end

function filter_in_one_pass!(f, d::AbstractDict)
    try
        for pair in d
            if !f(pair)
                delete!(d, pair.first)
            end
        end
    catch e
        return filter!_dict_deprecation(e, f, d)
    end
    return d
end

function filter!_dict_deprecation(e, f, d::AbstractDict)
    if isa(e, MethodError) && e.f === f
        depwarn("In `filter!(f, dict)`, `f` is now passed a single pair instead of two arguments.", :filter!)
        badkeys = Vector{keytype(d)}()
        for (k,v) in d
            # don't delete!(d, k) here, since dictionary types
            # may not support mutation during iteration
            f(k, v) || push!(badkeys, k)
        end
        for k in badkeys
            delete!(d, k)
        end
    else
        rethrow(e)
    end
    return d
end

"""
    filter(f, d::AbstractDict)

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
function filter(f, d::AbstractDict)
    # don't just do filter!(f, copy(d)): avoid making a whole copy of d
    df = empty(d)
    try
        for pair in d
            if f(pair)
                df[pair.first] = pair.second
            end
        end
    catch e
        if isa(e, MethodError) && e.f === f
            depwarn("In `filter(f, dict)`, `f` is now passed a single pair instead of two arguments.", :filter)
            for (k, v) in d
                if f(k, v)
                    df[k] = v
                end
            end
        else
            rethrow(e)
        end
    end
    return df
end

eltype(::Type{AbstractDict{K,V}}) where {K,V} = Pair{K,V}

function isequal(l::AbstractDict, r::AbstractDict)
    l === r && return true
    if isa(l,IdDict) != isa(r,IdDict)
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

function ==(l::AbstractDict, r::AbstractDict)
    l === r && return true
    if isa(l,IdDict) != isa(r,IdDict)
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
function hash(a::AbstractDict, h::UInt)
    hv = hasha_seed
    for (k,v) in a
        hv ⊻= hash(k, hash(v))
    end
    hash(hv, h)
end

function getindex(t::AbstractDict, key)
    v = get(t, key, secret_table_token)
    if v === secret_table_token
        throw(KeyError(key))
    end
    return v
end

# t[k1,k2,ks...] is syntactic sugar for t[(k1,k2,ks...)].  (Note
# that we need to avoid dispatch loops if setindex!(t,v,k) is not defined.)
getindex(t::AbstractDict, k1, k2, ks...) = getindex(t, tuple(k1,k2,ks...))
setindex!(t::AbstractDict, v, k1, k2, ks...) = setindex!(t, v, tuple(k1,k2,ks...))

push!(t::AbstractDict, p::Pair) = setindex!(t, p.second, p.first)
push!(t::AbstractDict, p::Pair, q::Pair) = push!(push!(t, p), q)
push!(t::AbstractDict, p::Pair, q::Pair, r::Pair...) = push!(push!(push!(t, p), q), r...)

# AbstractDicts are convertible
convert(::Type{T}, x::T) where {T<:AbstractDict} = x

function convert(::Type{T}, x::AbstractDict) where T<:AbstractDict
    h = T(x)
    if length(h) != length(x)
        error("key collision during dictionary conversion")
    end
    return h
end

# hashing objects by identity
_tablesz(x::Integer) = x < 16 ? 16 : one(x)<<((sizeof(x)<<3)-leading_zeros(x-1))

"""
    IdDict([itr])

`IdDict{K,V}()` constructs a hash table using object-id as hash and
`===` as equality with keys of type `K` and values of type `V`.

See [`Dict`](@ref) for further help.
"""
mutable struct IdDict{K,V} <: AbstractDict{K,V}
    ht::Vector{Any}
    ndel::Int
    IdDict{K,V}() where {K, V} = new{K,V}(Vector{Any}(uninitialized, 32), 0)

    function IdDict{K,V}(itr) where {K, V}
        d = IdDict{K,V}()
        for (k,v) in itr; d[k] = v; end
        d
    end

    function IdDict{K,V}(pairs::Pair...) where {K, V}
        d = IdDict{K,V}()
        sizehint!(d, length(pairs))
        for (k,v) in pairs; d[k] = v; end
        d
    end

    IdDict{K,V}(d::IdDict{K,V}) where {K, V} = new{K,V}(copy(d.ht))
end

IdDict() = IdDict{Any,Any}()
IdDict(kv::Tuple{}) = IdDict()

IdDict(ps::Pair{K,V}...)           where {K,V} = IdDict{K,V}(ps)
IdDict(ps::Pair{K}...)             where {K}   = IdDict{K,Any}(ps)
IdDict(ps::(Pair{K,V} where K)...) where {V}   = IdDict{Any,V}(ps)
IdDict(ps::Pair...)                            = IdDict{Any,Any}(ps)

function IdDict(kv)
    try
        dict_with_eltype((K, V) -> IdDict{K, V}, kv, eltype(kv))
    catch e
        if !applicable(start, kv) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError(
                "IdDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

empty(d::IdDict, ::Type{K}, ::Type{V}) where {K, V} = IdDict{K,V}()

function rehash!(d::IdDict, newsz = length(d.ht))
    d.ht = ccall(:jl_idtable_rehash, Any, (Any, Csize_t), d.ht, newsz)
    d
end

function sizehint!(d::IdDict, newsz)
    newsz = _tablesz(newsz*2)  # *2 for keys and values in same array
    oldsz = length(d.ht)
    # grow at least 25%
    if newsz < (oldsz*5)>>2
        return d
    end
    rehash!(d, newsz)
end

function setindex!(d::IdDict{K,V}, @nospecialize(val), @nospecialize(key)) where {K, V}
    !isa(key, K) && throw(ArgumentError("$key is not a valid key for type $K"))
    val = convert(V, val)
    if d.ndel >= ((3*length(d.ht))>>2)
        rehash!(d, max(length(d.ht)>>1, 32))
        d.ndel = 0
    end
    d.ht = ccall(:jl_eqtable_put, Array{Any,1}, (Any, Any, Any), d.ht, key, val)
    return d
end

function get(d::IdDict{K,V}, @nospecialize(key), @nospecialize(default)) where {K, V}
    val = ccall(:jl_eqtable_get, Any, (Any, Any, Any), d.ht, key, default)
    val === default ? default : val::V
end
function getindex(d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    val = get(d, key, secret_table_token)
    val === secret_table_token && throw(KeyError(key))
    return val::V
end

function pop!(d::IdDict{K,V}, @nospecialize(key), @nospecialize(default)) where {K, V}
    val = ccall(:jl_eqtable_pop, Any, (Any, Any, Any), d.ht, key, default)
    # TODO: this can underestimate `ndel`
    if val === default
        return default
    else
        (d.ndel += 1)
        return val::V
    end
end

function pop!(d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    val = pop!(d, key, secret_table_token)
    val === secret_table_token && throw(KeyError(key))
    return val::V
end

function delete!(d::IdDict{K}, @nospecialize(key)) where K
    pop!(d, key, secret_table_token)
    d
end

function empty!(d::IdDict)
    resize!(d.ht, 32)
    ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), d.ht, 0, sizeof(d.ht))
    d.ndel = 0
    return d
end

_oidd_nextind(a, i) = reinterpret(Int, ccall(:jl_eqtable_nextind, Csize_t, (Any, Csize_t), a, i))

start(d::IdDict) = _oidd_nextind(d.ht, 0)
done(d::IdDict, i) = (i == -1)
next(d::IdDict{K,V}, i) where {K, V} = (Pair{K,V}(d.ht[i+1], d.ht[i+2]), _oidd_nextind(d.ht, i+2))

function length(d::IdDict)
    n = 0
    for pair in d
        n+=1
    end
    n
end

copy(d::IdDict) = IdDict(d)

get!(d::IdDict{K,V}, @nospecialize(key), @nospecialize(default)) where {K, V} = (d[key] = get(d, key, default))::V

# For some AbstractDict types, it is safe to implement filter!
# by deleting keys during iteration.
filter!(f, d::IdDict) = filter_in_one_pass!(f, d)
