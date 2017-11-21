# This file is a part of Julia. License is MIT: https://julialang.org/license

function _truncate_at_width_or_chars(str, width, chars="", truncmark="…")
    truncwidth = textwidth(truncmark)
    (width <= 0 || width < truncwidth) && return ""

    wid = truncidx = lastidx = 0
    idx = start(str)
    while !done(str, idx)
        lastidx = idx
        c, idx = next(str, idx)
        wid += textwidth(c)
        wid >= width - truncwidth && truncidx == 0 && (truncidx = lastidx)
        (wid >= width || c in chars) && break
    end

    lastidx != 0 && str[lastidx] in chars && (lastidx = prevind(str, lastidx))
    truncidx == 0 && (truncidx = lastidx)
    if lastidx < endof(str)
        return String(SubString(str, 1, truncidx) * truncmark)
    else
        return String(str)
    end
end

function show(io::IO, t::Associative{K,V}) where V where K
    recur_io = IOContext(io, :SHOWN_SET => t)
    limit::Bool = get(io, :limit, false)
    if !haskey(io, :compact)
        recur_io = IOContext(recur_io, :compact => true)
    end

    # show in a Julia-syntax-like form: Dict(k=>v, ...)
    if isempty(t)
        print(io, typeof(t), "()")
    else
        if _isleaftype(K) && _isleaftype(V)
            print(io, typeof(t).name)
        else
            print(io, typeof(t))
        end
        print(io, '(')
        if !show_circular(io, t)
            first = true
            n = 0
            for pair in t
                first || print(io, ',')
                first = false
                show(recur_io, pair)
                n+=1
                limit && n >= 10 && (print(io, "…"); break)
            end
        end
        print(io, ')')
    end
end

abstract type AbstractSerializer end

# Dict

# These can be changed, to trade off better performance for space
const global maxallowedprobe = 16
const global maxprobeshift   = 6

_tablesz(x::Integer) = x < 16 ? 16 : one(x)<<((sizeof(x)<<3)-leading_zeros(x-1))

"""
    Dict([itr])

`Dict{K,V}()` constructs a hash table with keys of type `K` and values of type `V`.

Given a single iterable argument, constructs a [`Dict`](@ref) whose key-value pairs
are taken from 2-tuples `(key,value)` generated by the argument.

```jldoctest
julia> Dict([("A", 1), ("B", 2)])
Dict{String,Int64} with 2 entries:
  "B" => 2
  "A" => 1
```

Alternatively, a sequence of pair arguments may be passed.

```jldoctest
julia> Dict("A"=>1, "B"=>2)
Dict{String,Int64} with 2 entries:
  "B" => 2
  "A" => 1
```
"""
mutable struct Dict{K,V} <: Associative{K,V}
    slots::Array{UInt8,1}
    keys::Array{K,1}
    vals::Array{V,1}
    ndel::Int
    count::Int
    age::UInt
    idxfloor::Int  # an index <= the indexes of all used slots
    maxprobe::Int

    function Dict{K,V}() where V where K
        n = 16
        new(zeros(UInt8,n), Array{K,1}(n), Array{V,1}(n), 0, 0, 0, 1, 0)
    end
    function Dict{K,V}(d::Dict{K,V}) where V where K
        if d.ndel > 0
            rehash!(d)
        end
        @assert d.ndel == 0
        new(copy(d.slots), copy(d.keys), copy(d.vals), 0, d.count, d.age, d.idxfloor,
            d.maxprobe)
    end
end
function Dict{K,V}(kv) where V where K
    h = Dict{K,V}()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
Dict{K,V}(p::Pair) where {K,V} = setindex!(Dict{K,V}(), p.second, p.first)
function Dict{K,V}(ps::Pair...) where V where K
    h = Dict{K,V}()
    sizehint!(h, length(ps))
    for p in ps
        h[p.first] = p.second
    end
    return h
end
# Note the constructors of WeakKeyDict mirror these here, keep in sync.
Dict() = Dict{Any,Any}()
Dict(kv::Tuple{}) = Dict()
copy(d::Dict) = Dict(d)

const AnyDict = Dict{Any,Any}

Dict(ps::Pair{K,V}...)           where {K,V} = Dict{K,V}(ps)
Dict(ps::Pair{K}...)             where {K}   = Dict{K,Any}(ps)
Dict(ps::(Pair{K,V} where K)...) where {V}   = Dict{Any,V}(ps)
Dict(ps::Pair...)                            = Dict{Any,Any}(ps)

function Dict(kv)
    try
        associative_with_eltype((K, V) -> Dict{K, V}, kv, eltype(kv))
    catch e
        if !applicable(start, kv) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("Dict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

TP{K,V} = Union{Type{Tuple{K,V}},Type{Pair{K,V}}}

associative_with_eltype(DT_apply, kv, ::TP{K,V}) where {K,V} = DT_apply(K, V)(kv)
associative_with_eltype(DT_apply, kv::Generator, ::TP{K,V}) where {K,V} = DT_apply(K, V)(kv)
associative_with_eltype(DT_apply, ::Type{Pair{K,V}}) where {K,V} = DT_apply(K, V)()
associative_with_eltype(DT_apply, ::Type) = DT_apply(Any, Any)()
associative_with_eltype(DT_apply::F, kv, t) where {F} = grow_to!(associative_with_eltype(DT_apply, @default_eltype(typeof(kv))), kv)
function associative_with_eltype(DT_apply::F, kv::Generator, t) where F
    T = @default_eltype(typeof(kv))
    if T <: Union{Pair, Tuple{Any, Any}} && _isleaftype(T)
        return associative_with_eltype(DT_apply, kv, T)
    end
    return grow_to!(associative_with_eltype(DT_apply, T), kv)
end

# this is a special case due to (1) allowing both Pairs and Tuples as elements,
# and (2) Pair being invariant. a bit annoying.
function grow_to!(dest::Associative, itr)
    out = grow_to!(similar(dest, Pair{Union{},Union{}}), itr, start(itr))
    return isempty(out) ? dest : out
end

function grow_to!(dest::Associative{K,V}, itr, st) where V where K
    while !done(itr, st)
        (k,v), st = next(itr, st)
        if isa(k,K) && isa(v,V)
            dest[k] = v
        else
            new = similar(dest, Pair{typejoin(K,typeof(k)), typejoin(V,typeof(v))})
            copy!(new, dest)
            new[k] = v
            return grow_to!(new, itr, st)
        end
    end
    return dest
end

similar(d::Dict{K,V}) where {K,V} = Dict{K,V}()
similar(d::Dict, ::Type{Pair{K,V}}) where {K,V} = Dict{K,V}()

# conversion between Dict types
function convert(::Type{Dict{K,V}},d::Associative) where V where K
    h = Dict{K,V}()
    for (k,v) in d
        ck = convert(K,k)
        if !haskey(h,ck)
            h[ck] = convert(V,v)
        else
            error("key collision during dictionary conversion")
        end
    end
    return h
end
convert(::Type{Dict{K,V}},d::Dict{K,V}) where {K,V} = d

hashindex(key, sz) = (((hash(key)%Int) & (sz-1)) + 1)::Int

@propagate_inbounds isslotempty(h::Dict, i::Int) = h.slots[i] == 0x0
@propagate_inbounds isslotfilled(h::Dict, i::Int) = h.slots[i] == 0x1
@propagate_inbounds isslotmissing(h::Dict, i::Int) = h.slots[i] == 0x2

function rehash!(h::Dict{K,V}, newsz = length(h.keys)) where V where K
    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    sz = length(olds)
    newsz = _tablesz(newsz)
    h.age += 1
    h.idxfloor = 1
    if h.count == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        h.ndel = 0
        return h
    end

    slots = zeros(UInt8,newsz)
    keys = Array{K,1}(newsz)
    vals = Array{V,1}(newsz)
    age0 = h.age
    count = 0
    maxprobe = h.maxprobe

    for i = 1:sz
        @inbounds if olds[i] == 0x1
            k = oldk[i]
            v = oldv[i]
            index0 = index = hashindex(k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            probe = (index - index0) & (newsz-1)
            probe > maxprobe && (maxprobe = probe)
            slots[index] = 0x1
            keys[index] = k
            vals[index] = v
            count += 1

            if h.age != age0
                # if `h` is changed by a finalizer, retry
                return rehash!(h, newsz)
            end
        end
    end

    h.slots = slots
    h.keys = keys
    h.vals = vals
    h.count = count
    h.ndel = 0
    h.maxprobe = maxprobe
    @assert h.age == age0

    return h
end

max_values(::Type) = typemax(Int)
max_values(T::Type{<:Union{Void,BitIntegerSmall}}) = 1 << (8*sizeof(T))
max_values(T::Union) = max(max_values(T.a), max_values(T.b))
max_values(::Type{Bool}) = 2

function sizehint!(d::Dict{T}, newsz) where T
    oldsz = length(d.slots)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash!() assumes everything fits. it was only designed
        # for growing.
        return d
    end
    # grow at least 25%
    newsz = min(max(newsz, (oldsz*5)>>2),
                max_values(T))
    rehash!(d, newsz)
end

"""
    empty!(collection) -> collection

Remove all elements from a `collection`.

```jldoctest
julia> A = Dict("a" => 1, "b" => 2)
Dict{String,Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> empty!(A);

julia> A
Dict{String,Int64} with 0 entries
```
"""
function empty!(h::Dict{K,V}) where V where K
    fill!(h.slots, 0x0)
    sz = length(h.slots)
    empty!(h.keys)
    empty!(h.vals)
    resize!(h.keys, sz)
    resize!(h.vals, sz)
    h.ndel = 0
    h.count = 0
    h.age += 1
    h.idxfloor = 1
    return h
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex(h::Dict{K,V}, key) where V where K
    sz = length(h.keys)
    iter = 0
    maxprobe = h.maxprobe
    index = hashindex(key, sz)
    keys = h.keys

    @inbounds while true
        if isslotempty(h,index)
            break
        end
        if !isslotmissing(h,index) && (key === keys[index] || isequal(key,keys[index]))
            return index
        end

        index = (index & (sz-1)) + 1
        iter += 1
        iter > maxprobe && break
    end
    return -1
end

# get the index where a key is stored, or -pos if not present
# and the key would be inserted at pos
# This version is for use by setindex! and get!
function ht_keyindex2(h::Dict{K,V}, key) where V where K
    age0 = h.age
    sz = length(h.keys)
    iter = 0
    maxprobe = h.maxprobe
    index = hashindex(key, sz)
    avail = 0
    keys = h.keys

    @inbounds while true
        if isslotempty(h,index)
            if avail < 0
                return avail
            end
            return -index
        end

        if isslotmissing(h,index)
            if avail == 0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = -index
            end
        elseif key === keys[index] || isequal(key, keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter += 1
        iter > maxprobe && break
    end

    avail < 0 && return avail

    maxallowed = max(maxallowedprobe, sz>>maxprobeshift)
    # Check if key is not present, may need to keep searching to find slot
    @inbounds while iter < maxallowed
        if !isslotfilled(h,index)
            h.maxprobe = iter
            return -index
        end
        index = (index & (sz-1)) + 1
        iter += 1
    end

    rehash!(h, h.count > 64000 ? sz*2 : sz*4)

    return ht_keyindex2(h, key)
end

@propagate_inbounds function _setindex!(h::Dict, v, key, index)
    h.slots[index] = 0x1
    h.keys[index] = key
    h.vals[index] = v
    h.count += 1
    h.age += 1
    if index < h.idxfloor
        h.idxfloor = index
    end

    sz = length(h.keys)
    # Rehash now if necessary
    if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash!(h, h.count > 64000 ? h.count*2 : h.count*4)
    end
end

function setindex!(h::Dict{K,V}, v0, key0) where V where K
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    setindex!(h, v0, key)
end

function setindex!(h::Dict{K,V}, v0, key::K) where V where K
    v = convert(V, v0)
    index = ht_keyindex2(h, key)

    if index > 0
        h.age += 1
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        @inbounds _setindex!(h, v, key, -index)
    end

    return h
end

"""
    get!(collection, key, default)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => default`, and return `default`.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2, "c"=>3);

julia> get!(d, "a", 5)
1

julia> get!(d, "d", 4)
4

julia> d
Dict{String,Int64} with 4 entries:
  "c" => 3
  "b" => 2
  "a" => 1
  "d" => 4
```
"""
get!(collection, key, default)

get!(h::Dict{K,V}, key0, default) where {K,V} = get!(()->default, h, key0)

"""
    get!(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => f()`, and return `f()`.

This is intended to be called using `do` block syntax:
```julia
get!(dict, key) do
    # default value calculated here
    time()
end
```
"""
get!(f::Function, collection, key)

function get!(default::Callable, h::Dict{K,V}, key0) where V where K
    key = convert(K, key0)
    if !isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    return get!(default, h, key)
end

function get!(default::Callable, h::Dict{K,V}, key::K) where V where K
    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    age0 = h.age
    v = convert(V, default())
    if h.age != age0
        index = ht_keyindex2(h, key)
    end
    if index > 0
        h.age += 1
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        @inbounds _setindex!(h, v, key, -index)
    end
    return v
end

# NOTE: this macro is trivial, and should
#       therefore not be exported as-is: it's for internal use only.
macro get!(h, key0, default)
    return quote
        get!(()->$(esc(default)), $(esc(h)), $(esc(key0)))
    end
end


function getindex(h::Dict{K,V}, key) where V where K
    index = ht_keyindex(h, key)
    @inbounds return (index < 0) ? throw(KeyError(key)) : h.vals[index]::V
end

"""
    get(collection, key, default)

Return the value stored for the given key, or the given default value if no mapping for the
key is present.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2);

julia> get(d, "a", 3)
1

julia> get(d, "c", 3)
3
```
"""
get(collection, key, default)

function get(h::Dict{K,V}, key, default) where V where K
    index = ht_keyindex(h, key)
    @inbounds return (index < 0) ? default : h.vals[index]::V
end

"""
    get(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, return
`f()`.  Use [`get!`](@ref) to also store the default value in the dictionary.

This is intended to be called using `do` block syntax

```julia
get(dict, key) do
    # default value calculated here
    time()
end
```
"""
get(::Function, collection, key)

function get(default::Callable, h::Dict{K,V}, key) where V where K
    index = ht_keyindex(h, key)
    @inbounds return (index < 0) ? default() : h.vals[index]::V
end

"""
    haskey(collection, key) -> Bool

Determine whether a collection has a mapping for a given key.

```jldoctest
julia> a = Dict('a'=>2, 'b'=>3)
Dict{Char,Int64} with 2 entries:
  'b' => 3
  'a' => 2

julia> haskey(a,'a')
true

julia> haskey(a,'c')
false
```
"""
haskey(h::Dict, key) = (ht_keyindex(h, key) >= 0)
in(key, v::KeyIterator{<:Dict}) = (ht_keyindex(v.dict, key) >= 0)

"""
    getkey(collection, key, default)

Return the key matching argument `key` if one exists in `collection`, otherwise return `default`.

```jldoctest
julia> a = Dict('a'=>2, 'b'=>3)
Dict{Char,Int64} with 2 entries:
  'b' => 3
  'a' => 2

julia> getkey(a,'a',1)
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> getkey(a,'d','a')
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```
"""
function getkey(h::Dict{K,V}, key, default) where V where K
    index = ht_keyindex(h, key)
    @inbounds return (index<0) ? default : h.keys[index]::K
end

function _pop!(h::Dict, index)
    val = h.vals[index]
    _delete!(h, index)
    return val
end

function pop!(h::Dict, key)
    index = ht_keyindex(h, key)
    return index > 0 ? _pop!(h, index) : throw(KeyError(key))
end

"""
    pop!(collection, key[, default])

Delete and return the mapping for `key` if it exists in `collection`, otherwise return
`default`, or throw an error if `default` is not specified.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2, "c"=>3);

julia> pop!(d, "a")
1

julia> pop!(d, "d")
ERROR: KeyError: key "d" not found
Stacktrace:
[...]

julia> pop!(d, "e", 4)
4
```
"""
pop!(collection, key, default)

function pop!(h::Dict, key, default)
    index = ht_keyindex(h, key)
    return index > 0 ? _pop!(h, index) : default
end

function pop!(h::Dict)
    isempty(h) && throw(ArgumentError("dict must be non-empty"))
    idx = start(h)
    @inbounds key = h.keys[idx]
    @inbounds val = h.vals[idx]
    _delete!(h, idx)
    key => val
end

function _delete!(h::Dict, index)
    h.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, UInt), h.keys, index-1)
    ccall(:jl_arrayunset, Void, (Any, UInt), h.vals, index-1)
    h.ndel += 1
    h.count -= 1
    h.age += 1
    return h
end

"""
    delete!(collection, key)

Delete the mapping for the given key in a collection, and return the collection.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2)
Dict{String,Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> delete!(d, "b")
Dict{String,Int64} with 1 entry:
  "a" => 1
```
"""
delete!(collection, key)

function delete!(h::Dict, key)
    index = ht_keyindex(h, key)
    if index > 0
        _delete!(h, index)
    end
    return h
end

function skip_deleted(h::Dict, i)
    L = length(h.slots)
    @inbounds while i<=L && !isslotfilled(h,i)
        i += 1
    end
    return i
end

function start(t::Dict)
    i = skip_deleted(t, t.idxfloor)
    t.idxfloor = i
    return i
end
done(t::Dict, i) = i > length(t.vals)
@propagate_inbounds function next(t::Dict{K,V}, i) where {K,V}
    return (Pair{K,V}(t.keys[i],t.vals[i]), skip_deleted(t,i+1))
end

isempty(t::Dict) = (t.count == 0)
length(t::Dict) = t.count

@propagate_inbounds function next(v::KeyIterator{<:Dict}, i)
    return (v.dict.keys[i], skip_deleted(v.dict,i+1))
end
@propagate_inbounds function next(v::ValueIterator{<:Dict}, i)
    return (v.dict.vals[i], skip_deleted(v.dict,i+1))
end

function filter_in_one_pass!(f, d::Associative)
    try
        for (k, v) in d
            if !f(k => v)
                delete!(d, k)
            end
        end
    catch e
        return filter!_dict_deprecation(e, f, d)
    end
    return d
end

# For these Associative types, it is safe to implement filter!
# by deleting keys during iteration.
filter!(f, d::Union{ObjectIdDict,Dict}) = filter_in_one_pass!(f, d)

struct ImmutableDict{K,V} <: Associative{K,V}
    parent::ImmutableDict{K,V}
    key::K
    value::V
    ImmutableDict{K,V}() where {K,V} = new() # represents an empty dictionary
    ImmutableDict{K,V}(key, value) where {K,V} = (empty = new(); new(empty, key, value))
    ImmutableDict{K,V}(parent::ImmutableDict, key, value) where {K,V} = new(parent, key, value)
end

"""
    ImmutableDict

ImmutableDict is a Dictionary implemented as an immutable linked list,
which is optimal for small dictionaries that are constructed over many individual insertions
Note that it is not possible to remove a value, although it can be partially overridden and hidden
by inserting a new value with the same key

    ImmutableDict(KV::Pair)

Create a new entry in the Immutable Dictionary for the key => value pair

 - use `(key => value) in dict` to see if this particular combination is in the properties set
 - use `get(dict, key, default)` to retrieve the most recent value for a particular key

"""
ImmutableDict
ImmutableDict(KV::Pair{K,V}) where {K,V} = ImmutableDict{K,V}(KV[1], KV[2])
ImmutableDict(t::ImmutableDict{K,V}, KV::Pair) where {K,V} = ImmutableDict{K,V}(t, KV[1], KV[2])

function in(key_value::Pair, dict::ImmutableDict, valcmp=isequal)
    key, value = key_value
    while isdefined(dict, :parent)
        if isequal(dict.key, key)
            valcmp(value, dict.value) && return true
        end
        dict = dict.parent
    end
    return false
end

function haskey(dict::ImmutableDict, key)
    while isdefined(dict, :parent)
        dict.key == key && return true
        dict = dict.parent
    end
    return false
end

function getindex(dict::ImmutableDict, key)
    while isdefined(dict, :parent)
        dict.key == key && return dict.value
        dict = dict.parent
    end
    throw(KeyError(key))
end
function get(dict::ImmutableDict, key, default)
    while isdefined(dict, :parent)
        dict.key == key && return dict.value
        dict = dict.parent
    end
    return default
end

# this actually defines reverse iteration (e.g. it should not be used for merge/copy/filter type operations)
start(t::ImmutableDict) = t
next(::ImmutableDict{K,V}, t) where {K,V} = (Pair{K,V}(t.key, t.value), t.parent)
done(::ImmutableDict, t) = !isdefined(t, :parent)
length(t::ImmutableDict) = count(x->true, t)
isempty(t::ImmutableDict) = done(t, start(t))
function similar(t::ImmutableDict)
    while isdefined(t, :parent)
        t = t.parent
    end
    return t
end

_similar_for(c::Dict, ::Type{P}, itr, isz) where {P<:Pair} = similar(c, P)
_similar_for(c::Associative, T, itr, isz) = throw(ArgumentError("for Associatives, similar requires an element type of Pair;\n  if calling map, consider a comprehension instead"))
