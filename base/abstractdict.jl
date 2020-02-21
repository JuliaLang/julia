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
        return valcmp(v, p[2])
    end
    return false
end

function in(p, a::AbstractDict)
    error("""AbstractDict collections only contain Pairs;
             Either look for e.g. A=>B instead, or use the `keys` or `values`
             function if you are looking for a key or value respectively.""")
end

function summary(io::IO, t::AbstractDict)
    n = length(t)
    showarg(io, t, true)
    print(io, " with ", n, (n==1 ? " entry" : " entries"))
end

struct KeySet{K, T <: AbstractDict{K}} <: AbstractSet{K}
    dict::T
end

struct ValueIterator{T<:AbstractDict}
    dict::T
end

function summary(io::IO, iter::T) where {T<:Union{KeySet,ValueIterator}}
    print(io, T.name, " for a ")
    summary(io, iter.dict)
end

show(io::IO, iter::Union{KeySet,ValueIterator}) = show_vector(io, iter)

length(v::Union{KeySet,ValueIterator}) = length(v.dict)
isempty(v::Union{KeySet,ValueIterator}) = isempty(v.dict)
_tt2(::Type{Pair{A,B}}) where {A,B} = B
eltype(::Type{ValueIterator{D}}) where {D} = _tt2(eltype(D))

function iterate(v::Union{KeySet,ValueIterator}, state...)
    y = iterate(v.dict, state...)
    y === nothing && return nothing
    return (y[1][isa(v, KeySet) ? 1 : 2], y[2])
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
When the keys are stored internally in a hash table,
as is the case for `Dict`,
the order in which they are returned may vary.
But `keys(a)` and `values(a)` both iterate `a` and
return the elements in the same order.

# Examples
```jldoctest
julia> D = Dict('a'=>2, 'b'=>3)
Dict{Char,Int64} with 2 entries:
  'a' => 2
  'b' => 3

julia> collect(keys(D))
2-element Array{Char,1}:
 'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
 'b': ASCII/Unicode U+0062 (category Ll: Letter, lowercase)
```
"""
keys(a::AbstractDict) = KeySet(a)

"""
    values(a::AbstractDict)

Return an iterator over all values in a collection.
`collect(values(a))` returns an array of values.
When the values are stored internally in a hash table,
as is the case for `Dict`,
the order in which they are returned may vary.
But `keys(a)` and `values(a)` both iterate `a` and
return the elements in the same order.

# Examples
```jldoctest
julia> D = Dict('a'=>2, 'b'=>3)
Dict{Char,Int64} with 2 entries:
  'a' => 2
  'b' => 3

julia> collect(values(D))
2-element Array{Int64,1}:
 2
 3
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

copy(a::AbstractDict) = merge!(empty(a), a)
copy!(dst::AbstractDict, src::AbstractDict) = merge!(empty!(dst), src)

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
    mergewith!(combine, d::AbstractDict, others::AbstractDict...) -> d
    mergewith!(combine)
    merge!(combine, d::AbstractDict, others::AbstractDict...) -> d

Update collection with pairs from the other collections.
Values with the same key will be combined using the
combiner function.  The curried form `mergewith!(combine)` returns the
function `(args...) -> mergewith!(combine, args...)`.

Method `merge!(combine::Union{Function,Type}, args...)` as an alias of
`mergewith!(combine, args...)` is still available for backward
compatibility.

!!! compat "Julia 1.5"
    `mergewith!` requires Julia 1.5 or later.

# Examples
```jldoctest
julia> d1 = Dict(1 => 2, 3 => 4);

julia> d2 = Dict(1 => 4, 4 => 5);

julia> mergewith!(+, d1, d2);

julia> d1
Dict{Int64,Int64} with 3 entries:
  4 => 5
  3 => 4
  1 => 6

julia> mergewith!(-, d1, d1);

julia> d1
Dict{Int64,Int64} with 3 entries:
  4 => 0
  3 => 0
  1 => 0

julia> foldl(mergewith!(+), [d1, d2]; init=Dict{Int64,Int64}())
Dict{Int64,Int64} with 3 entries:
  4 => 5
  3 => 0
  1 => 4
```
"""
function mergewith!(combine, d::AbstractDict, others::AbstractDict...)
    for other in others
        for (k,v) in other
            d[k] = haskey(d, k) ? combine(d[k], v) : v
        end
    end
    return d
end

mergewith!(combine) = (args...) -> mergewith!(combine, args...)

merge!(combine::Callable, args...) = mergewith!(combine, args...)

"""
    keytype(type)

Get the key type of an dictionary type. Behaves similarly to [`eltype`](@ref).

# Examples
```jldoctest
julia> keytype(Dict(Int32(1) => "foo"))
Int32
```
"""
keytype(::Type{<:AbstractDict{K,V}}) where {K,V} = K
keytype(a::AbstractDict) = keytype(typeof(a))

"""
    valtype(type)

Get the value type of an dictionary type. Behaves similarly to [`eltype`](@ref).

# Examples
```jldoctest
julia> valtype(Dict(Int32(1) => "foo"))
String
```
"""
valtype(::Type{<:AbstractDict{K,V}}) where {K,V} = V
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
    mergewith(combine, d::AbstractDict, others::AbstractDict...)
    mergewith(combine)
    merge(combine, d::AbstractDict, others::AbstractDict...)

Construct a merged collection from the given collections. If necessary, the
types of the resulting collection will be promoted to accommodate the types of
the merged collections. Values with the same key will be combined using the
combiner function.  The curried form `mergewith(combine)` returns the function
`(args...) -> mergewith(combine, args...)`.

Method `merge(combine::Union{Function,Type}, args...)` as an alias of
`mergewith(combine, args...)` is still available for backward compatibility.

!!! compat "Julia 1.5"
    `mergewith` requires Julia 1.5 or later.

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

julia> mergewith(+, a, b)
Dict{String,Float64} with 3 entries:
  "bar" => 4753.0
  "baz" => 17.0
  "foo" => 0.0

julia> ans == mergewith(+)(a, b)
true
```
"""
mergewith(combine, d::AbstractDict, others::AbstractDict...) =
    mergewith!(combine, _typeddict(d, others...), others...)
mergewith(combine) = (args...) -> mergewith(combine, args...)
merge(combine::Callable, d::AbstractDict, others::AbstractDict...) =
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
    for pair in d
        # don't delete!(d, k) here, since dictionary types
        # may not support mutation during iteration
        f(pair) || push!(badkeys, pair.first)
    end
    for k in badkeys
        delete!(d, k)
    end
    return d
end

function filter_in_one_pass!(f, d::AbstractDict)
    for pair in d
        if !f(pair)
            delete!(d, pair.first)
        end
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
            rethrow()
        end
    end
    return df
end

function eltype(::Type{<:AbstractDict{K,V}}) where {K,V}
    if @isdefined(K)
        if @isdefined(V)
            return Pair{K,V}
        else
            return Pair{K}
        end
    elseif @isdefined(V)
        return Pair{k,V} where k
    else
        return Pair
    end
end

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
    length(l) != length(r) && return false
    anymissing = false
    for pair in l
        isin = in(pair, r, ==)
        if ismissing(isin)
            anymissing = true
        elseif !isin
            return false
        end
    end
    return anymissing ? missing : true
end

const hasha_seed = UInt === UInt64 ? 0x6d35bb51952d5539 : 0x952d5539
function hash(a::AbstractDict, h::UInt)
    hv = hasha_seed
    for (k,v) in a
        hv ⊻= hash(k, hash(v))
    end
    hash(hv, h)
end

function get(default::Callable, d::AbstractDict, key)
    val = get(d, key)
    val === nothing ? default() : something(val)
end

function get(d::AbstractDict, key, default)
    val = get(d, key)
    val === nothing ? default : something(val)
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

TP{K,V} = Union{Type{Tuple{K,V}},Type{Pair{K,V}}}

dict_with_eltype(DT_apply, kv, ::TP{K,V}) where {K,V} = DT_apply(K, V)(kv)
dict_with_eltype(DT_apply, kv::Generator, ::TP{K,V}) where {K,V} = DT_apply(K, V)(kv)
dict_with_eltype(DT_apply, ::Type{Pair{K,V}}) where {K,V} = DT_apply(K, V)()
dict_with_eltype(DT_apply, ::Type) = DT_apply(Any, Any)()
dict_with_eltype(DT_apply::F, kv, t) where {F} = grow_to!(dict_with_eltype(DT_apply, @default_eltype(typeof(kv))), kv)
function dict_with_eltype(DT_apply::F, kv::Generator, t) where F
    T = @default_eltype(kv)
    if T <: Union{Pair, Tuple{Any, Any}} && isconcretetype(T)
        return dict_with_eltype(DT_apply, kv, T)
    end
    return grow_to!(dict_with_eltype(DT_apply, T), kv)
end

"""
    map!(f, values(dict::AbstractDict))

Modifies `dict` by transforming each value from `val` to `f(val)`.
Note that the type of `dict` cannot be changed: if `f(val)` is not an instance of the value type
of `dict` then it will be converted to the value type if possible and otherwise raise an error.

# Examples
```jldoctest
julia> d = Dict(:a => 1, :b => 2)
Dict{Symbol,Int64} with 2 entries:
  :a => 1
  :b => 2

julia> map!(v -> v-1, values(d))
Base.ValueIterator for a Dict{Symbol,Int64} with 2 entries. Values:
  0
  1
```
"""
function map!(f, iter::ValueIterator)
    # This is the naive fallback which requires hash evaluations
    # Contrary to the example Dict has an implementation which does not require hash evaluations
    dict = iter.dict
    for (key, val) in pairs(dict)
        dict[key] = f(val)
    end
    return iter
end
