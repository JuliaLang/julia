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

get!(t::AbstractDict, key, default) = get!(() -> default, t, key)
function get!(default::Callable, t::AbstractDict{K,V}, key0) where K where V
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$(limitrepr(key0)) is not a valid key for type $K"))
    end
    haskey(t, key) && return t[key]
    val = convert(V, default())
    t[key] = val
    return val
end

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
    count::Int
    ndel::Int
    IdDict{K,V}() where {K, V} = new{K,V}(Vector{Any}(undef, 32), 0, 0)

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

    IdDict{K,V}(d::IdDict{K,V}) where {K, V} = new{K,V}(copy(d.ht), d.count, d.ndel)
end

IdDict() = IdDict{Any,Any}()
IdDict(kv::Tuple{}) = IdDict()

IdDict(ps::Pair{K,V}...)           where {K,V} = IdDict{K,V}(ps)
IdDict(ps::Pair{K}...)             where {K}   = IdDict{K,Any}(ps)
IdDict(ps::(Pair{K,V} where K)...) where {V}   = IdDict{Any,V}(ps)
IdDict(ps::Pair...)                            = IdDict{Any,Any}(ps)

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

function IdDict(kv)
    try
        dict_with_eltype((K, V) -> IdDict{K, V}, kv, eltype(kv))
    catch
        if !applicable(iterate, kv) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError(
                "IdDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow()
        end
    end
end

empty(d::IdDict, ::Type{K}, ::Type{V}) where {K, V} = IdDict{K,V}()

function rehash!(d::IdDict, newsz = length(d.ht))
    d.ht = ccall(:jl_idtable_rehash, Vector{Any}, (Any, Csize_t), d.ht, newsz)
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
    !isa(key, K) && throw(ArgumentError("$(limitrepr(key)) is not a valid key for type $K"))
    if !(val isa V) # avoid a dynamic call
        val = convert(V, val)
    end
    if d.ndel >= ((3*length(d.ht))>>2)
        rehash!(d, max(length(d.ht)>>1, 32))
        d.ndel = 0
    end
    inserted = RefValue{Cint}(0)
    d.ht = ccall(:jl_eqtable_put, Array{Any,1}, (Any, Any, Any, Ptr{Cint}), d.ht, key, val, inserted)
    d.count += inserted[]
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
    found = RefValue{Cint}(0)
    val = ccall(:jl_eqtable_pop, Any, (Any, Any, Any, Ptr{Cint}), d.ht, key, default, found)
    if found[] === Cint(0)
        return default
    else
        d.count -= 1
        d.ndel += 1
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
    d.count = 0
    return d
end

_oidd_nextind(a, i) = reinterpret(Int, ccall(:jl_eqtable_nextind, Csize_t, (Any, Csize_t), a, i))

function iterate(d::IdDict{K,V}, idx=0) where {K, V}
    idx = _oidd_nextind(d.ht, idx)
    idx == -1 && return nothing
    return (Pair{K, V}(d.ht[idx + 1]::K, d.ht[idx + 2]::V), idx + 2)
end

length(d::IdDict) = d.count

copy(d::IdDict) = typeof(d)(d)

get!(d::IdDict{K,V}, @nospecialize(key), @nospecialize(default)) where {K, V} = (d[key] = get(d, key, default))::V

function get(default::Callable, d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    val = get(d, key, secret_table_token)
    if val === secret_table_token
        val = default()
    end
    return val
end

function get!(default::Callable, d::IdDict{K,V}, @nospecialize(key)) where {K, V}
    val = get(d, key, secret_table_token)
    if val === secret_table_token
        val = default()
        setindex!(d, val, key)
    end
    return val
end

in(@nospecialize(k), v::KeySet{<:Any,<:IdDict}) = get(v.dict, k, secret_table_token) !== secret_table_token

# For some AbstractDict types, it is safe to implement filter!
# by deleting keys during iteration.
filter!(f, d::IdDict) = filter_in_one_pass!(f, d)

# Like Set, but using IdDict
mutable struct IdSet{T} <: AbstractSet{T}
    dict::IdDict{T,Nothing}

    IdSet{T}() where {T} = new(IdDict{T,Nothing}())
    IdSet{T}(s::IdSet{T}) where {T} = new(copy(s.dict))
end

IdSet{T}(itr) where {T} = union!(IdSet{T}(), itr)
IdSet() = IdSet{Any}()

copymutable(s::IdSet) = typeof(s)(s)
copy(s::IdSet) = typeof(s)(s)

isempty(s::IdSet) = isempty(s.dict)
length(s::IdSet)  = length(s.dict)
in(@nospecialize(x), s::IdSet) = haskey(s.dict, x)
push!(s::IdSet, @nospecialize(x)) = (s.dict[x] = nothing; s)
pop!(s::IdSet, @nospecialize(x)) = (pop!(s.dict, x); x)
pop!(s::IdSet, @nospecialize(x), @nospecialize(default)) = (x in s ? pop!(s, x) : default)
delete!(s::IdSet, @nospecialize(x)) = (delete!(s.dict, x); s)

sizehint!(s::IdSet, newsz) = (sizehint!(s.dict, newsz); s)
empty!(s::IdSet) = (empty!(s.dict); s)

filter!(f, d::IdSet) = unsafe_filter!(f, d)

function iterate(s::IdSet, state...)
    y = iterate(s.dict, state...)
    y === nothing && return nothing
    ((k, _), i) = y
    return (k, i)
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
