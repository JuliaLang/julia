# This file is a part of Julia. License is MIT: https://julialang.org/license

function show(io::IO, t::AbstractDict{K,V}) where V where K
    recur_io = IOContext(io, :SHOWN_SET => t,
                             :typeinfo => eltype(t))

    limit = get(io, :limit, false)::Bool
    # show in a Julia-syntax-like form: Dict(k=>v, ...)
    print(io, typeinfo_prefix(io, t)[1])
    print(io, '(')
    if !isempty(t) && !show_circular(io, t)
        first = true
        n = 0
        for pair in t
            first || print(io, ", ")
            first = false
            show(recur_io, pair)
            n+=1
            limit && n >= 10 && (print(io, "…"); break)
        end
    end
    print(io, ')')
end

# Dict

# These can be changed, to trade off better performance for space
const global maxallowedprobe = 16
const global maxprobeshift   = 6

"""
    Dict([itr])

`Dict{K,V}()` constructs a hash table with keys of type `K` and values of type `V`.
Keys are compared with [`isequal`](@ref) and hashed with [`hash`](@ref).

Given a single iterable argument, constructs a [`Dict`](@ref) whose key-value pairs
are taken from 2-tuples `(key,value)` generated by the argument.

# Examples
```jldoctest
julia> Dict([("A", 1), ("B", 2)])
Dict{String, Int64} with 2 entries:
  "B" => 2
  "A" => 1
```

Alternatively, a sequence of pair arguments may be passed.

```jldoctest
julia> Dict("A"=>1, "B"=>2)
Dict{String, Int64} with 2 entries:
  "B" => 2
  "A" => 1
```

!!! warning

    Keys are allowed to be mutable, but if you do mutate stored
    keys, the hash table may become internally inconsistent, in which case
    the `Dict` will not work properly. [`IdDict`](@ref) can be an
    alternative if you need to mutate keys.

"""
mutable struct Dict{K,V} <: AbstractDict{K,V}
    # Metadata: empty => 0x00, removed => 0x7f, full => 0b1[7 most significant hash bits]
    slots::Vector{UInt8}
    keys::Array{K,1}
    vals::Array{V,1}
    ndel::Int
    count::Int
    age::UInt
    idxfloor::Int  # an index <= the indices of all used slots
    maxprobe::Int

    function Dict{K,V}() where V where K
        n = 16
        new(zeros(UInt8,n), Vector{K}(undef, n), Vector{V}(undef, n), 0, 0, 0, n, 0)
    end
    function Dict{K,V}(d::Dict{K,V}) where V where K
        new(copy(d.slots), copy(d.keys), copy(d.vals), d.ndel, d.count, d.age,
            d.idxfloor, d.maxprobe)
    end
    function Dict{K, V}(slots, keys, vals, ndel, count, age, idxfloor, maxprobe) where {K, V}
        new(slots, keys, vals, ndel, count, age, idxfloor, maxprobe)
    end
end
function Dict{K,V}(kv) where V where K
    h = Dict{K,V}()
    haslength(kv) && sizehint!(h, Int(length(kv))::Int)
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

Dict(ps::Pair{K,V}...) where {K,V} = Dict{K,V}(ps)
Dict(ps::Pair...)                  = Dict(ps)

function Dict(kv)
    try
        dict_with_eltype((K, V) -> Dict{K, V}, kv, eltype(kv))
    catch
        if !isiterable(typeof(kv)) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("Dict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow()
        end
    end
end

function grow_to!(dest::AbstractDict{K, V}, itr) where V where K
    y = iterate(itr)
    y === nothing && return dest
    ((k,v), st) = y
    dest2 = empty(dest, typeof(k), typeof(v))
    dest2[k] = v
    grow_to!(dest2, itr, st)
end

# this is a special case due to (1) allowing both Pairs and Tuples as elements,
# and (2) Pair being invariant. a bit annoying.
function grow_to!(dest::AbstractDict{K,V}, itr, st) where V where K
    y = iterate(itr, st)
    while y !== nothing
        (k,v), st = y
        if isa(k,K) && isa(v,V)
            dest[k] = v
        else
            new = empty(dest, promote_typejoin(K,typeof(k)), promote_typejoin(V,typeof(v)))
            merge!(new, dest)
            new[k] = v
            return grow_to!(new, itr, st)
        end
        y = iterate(itr, st)
    end
    return dest
end

empty(a::AbstractDict, ::Type{K}, ::Type{V}) where {K, V} = Dict{K, V}()

# Gets 7 most significant bits from the hash (hsh), first bit is 1
_shorthash7(hsh::UInt) = (hsh >> (8sizeof(UInt)-7))%UInt8 | 0x80

# hashindex (key, sz) - computes optimal position and shorthash7
#     idx - optimal position in the hash table
#     sh::UInt8 - short hash (7 highest hash bits)
function hashindex(key, sz)
    hsh = hash(key)::UInt
    idx = (((hsh % Int) & (sz-1)) + 1)::Int
    return idx, _shorthash7(hsh)
end

@propagate_inbounds isslotempty(h::Dict, i::Int) = h.slots[i] == 0x00
@propagate_inbounds isslotfilled(h::Dict, i::Int) = (h.slots[i] & 0x80) != 0
@propagate_inbounds isslotmissing(h::Dict, i::Int) = h.slots[i] == 0x7f

@constprop :none function rehash!(h::Dict{K,V}, newsz = length(h.keys)) where V where K
    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    sz = length(olds)
    newsz = _tablesz(newsz)
    h.age += 1
    h.idxfloor = 1
    if h.count == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0x0)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        h.ndel = 0
        h.maxprobe = 0
        return h
    end

    slots = zeros(UInt8,newsz)
    keys = Vector{K}(undef, newsz)
    vals = Vector{V}(undef, newsz)
    age0 = h.age
    count = 0
    maxprobe = 0

    for i = 1:sz
        @inbounds if (olds[i] & 0x80) != 0
            k = oldk[i]
            v = oldv[i]
            index, sh = hashindex(k, newsz)
            index0 = index
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            probe = (index - index0) & (newsz-1)
            probe > maxprobe && (maxprobe = probe)
            slots[index] = olds[i]
            keys[index] = k
            vals[index] = v
            count += 1
        end
    end

    @assert h.age == age0 "Multiple concurrent writes to Dict detected!"
    h.age += 1
    h.slots = slots
    h.keys = keys
    h.vals = vals
    h.count = count
    h.ndel = 0
    h.maxprobe = maxprobe
    return h
end

function _sizehint!(d::Dict{T}, newsz; shrink = true) where T
    oldsz = length(d.slots)
    # limit new element count to max_values of the key type
    newsz = min(max(newsz, length(d)), max_values(T)::Int)
    # need at least 1.5n space to hold n elements
    newsz = _tablesz(cld(3 * newsz, 2))
    return (shrink ? newsz == oldsz : newsz <= oldsz) ? d : rehash!(d, newsz)
end

sizehint!(d::Dict{T}, newsz) where T = _sizehint!(d, newsz)

"""
    empty!(collection) -> collection

Remove all elements from a `collection`.

# Examples
```jldoctest
julia> A = Dict("a" => 1, "b" => 2)
Dict{String, Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> empty!(A);

julia> A
Dict{String, Int64}()
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
    h.maxprobe = 0
    h.age += 1
    h.idxfloor = sz
    return h
end

# get the index where a key is stored, or -1 if not present
@assume_effects :terminates_locally function ht_keyindex(h::Dict{K,V}, key) where V where K
    isempty(h) && return -1
    sz = length(h.keys)
    iter = 0
    maxprobe = h.maxprobe
    maxprobe < sz || throw(AssertionError()) # This error will never trigger, but is needed for terminates_locally to be valid
    index, sh = hashindex(key, sz)
    keys = h.keys

    @inbounds while true
        isslotempty(h,index) && return -1
        if h.slots[index] == sh
            k = keys[index]
            if (key ===  k || isequal(key, k))
                return index
            end
        end

        index = (index & (sz-1)) + 1
        (iter += 1) > maxprobe && return -1
    end
    # This line is unreachable
end

# get (index, sh) for the key
#     index - where a key is stored, or -pos if not present
#             and the key would be inserted at pos
#     sh::UInt8 - short hash (7 highest hash bits)
# This version is for use by setindex! and get!
function ht_keyindex2_shorthash!(h::Dict{K,V}, key) where V where K
    sz = length(h.keys)
    iter = 0
    maxprobe = h.maxprobe
    index, sh = hashindex(key, sz)
    avail = 0
    keys = h.keys

    @inbounds while true
        if isslotempty(h,index)
            return (avail < 0 ? avail : -index), sh
        end

        if isslotmissing(h,index)
            if avail == 0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = -index
            end
        elseif h.slots[index] == sh
            k = keys[index]
            if key === k || isequal(key, k)
                return index, sh
            end
        end

        index = (index & (sz-1)) + 1
        iter += 1
        iter > maxprobe && break
    end

    avail < 0 && return avail, sh

    maxallowed = max(maxallowedprobe, sz>>maxprobeshift)
    # Check if key is not present, may need to keep searching to find slot
    @inbounds while iter < maxallowed
        if !isslotfilled(h,index)
            h.maxprobe = iter
            return -index, sh
        end
        index = (index & (sz-1)) + 1
        iter += 1
    end

    rehash!(h, h.count > 64000 ? sz*2 : sz*4)

    return ht_keyindex2_shorthash!(h, key)
end

# Only for better backward compatibility. It can be removed in the future.
ht_keyindex2!(h::Dict, key) = ht_keyindex2_shorthash!(h, key)[1]

@propagate_inbounds function _setindex!(h::Dict, v, key, index, sh = _shorthash7(hash(key)))
    h.ndel -= isslotmissing(h, index)
    h.slots[index] = sh
    h.keys[index] = key
    h.vals[index] = v
    h.count += 1
    h.age += 1
    if index < h.idxfloor
        h.idxfloor = index
    end

    sz = length(h.keys)
    # Rehash now if necessary
    if (h.count + h.ndel)*3 > sz*2
        # > 2/3 full (including tombstones)
        rehash!(h, h.count > 64000 ? h.count*2 : h.count*4)
    end
    nothing
end

function setindex!(h::Dict{K,V}, v0, key0) where V where K
    if key0 isa K
        key = key0
    else
        key = convert(K, key0)::K
        if !(isequal(key, key0)::Bool)
            throw(ArgumentError("$(limitrepr(key0)) is not a valid key for type $K"))
        end
    end
    setindex!(h, v0, key)
end

function setindex!(h::Dict{K,V}, v0, key::K) where V where K
    v = v0 isa V ? v0 : convert(V, v0)::V
    index, sh = ht_keyindex2_shorthash!(h, key)

    if index > 0
        h.age += 1
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        @inbounds _setindex!(h, v, key, -index, sh)
    end

    return h
end

function setindex!(h::Dict{K,Any}, v, key::K) where K
    @nospecialize v
    index, sh = ht_keyindex2_shorthash!(h, key)

    if index > 0
        h.age += 1
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        @inbounds _setindex!(h, v, key, -index, sh)
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
Dict{String, Int64} with 4 entries:
  "c" => 3
  "b" => 2
  "a" => 1
  "d" => 4
```
"""
get!(collection, key, default)

"""
    get!(f::Union{Function, Type}, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => f()`, and return `f()`.

This is intended to be called using `do` block syntax.

# Examples
```jldoctest
julia> squares = Dict{Int, Int}();

julia> function get_square!(d, i)
           get!(d, i) do
               i^2
           end
       end
get_square! (generic function with 1 method)

julia> get_square!(squares, 2)
4

julia> squares
Dict{Int64, Int64} with 1 entry:
  2 => 4
```
"""
get!(f::Callable, collection, key)

function get!(default::Callable, h::Dict{K,V}, key0) where V where K
    if key0 isa K
        key = key0
    else
        key = convert(K, key0)::K
        if !isequal(key, key0)
            throw(ArgumentError("$(limitrepr(key0)) is not a valid key for type $K"))
        end
    end
    return get!(default, h, key)
end

function get!(default::Callable, h::Dict{K,V}, key::K) where V where K
    index, sh = ht_keyindex2_shorthash!(h, key)

    index > 0 && return h.vals[index]

    age0 = h.age
    v = default()
    if !isa(v, V)
        v = convert(V, v)::V
    end
    if h.age != age0
        index, sh = ht_keyindex2_shorthash!(h, key)
    end
    if index > 0
        h.age += 1
        @inbounds h.keys[index] = key
        @inbounds h.vals[index] = v
    else
        @inbounds _setindex!(h, v, key, -index, sh)
    end
    return v
end

function getindex(h::Dict{K,V}, key) where V where K
    index = ht_keyindex(h, key)
    @inbounds return (index < 0) ? throw(KeyError(key)) : h.vals[index]::V
end

"""
    get(collection, key, default)

Return the value stored for the given key, or the given default value if no mapping for the
key is present.

!!! compat "Julia 1.7"
    For tuples and numbers, this function requires at least Julia 1.7.

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
    get(f::Union{Function, Type}, collection, key)

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
get(::Callable, collection, key)

function get(default::Callable, h::Dict{K,V}, key) where V where K
    index = ht_keyindex(h, key)
    @inbounds return (index < 0) ? default() : h.vals[index]::V
end

"""
    haskey(collection, key) -> Bool

Determine whether a collection has a mapping for a given `key`.

# Examples
```jldoctest
julia> D = Dict('a'=>2, 'b'=>3)
Dict{Char, Int64} with 2 entries:
  'a' => 2
  'b' => 3

julia> haskey(D, 'a')
true

julia> haskey(D, 'c')
false
```
"""
haskey(h::Dict, key) = (ht_keyindex(h, key) >= 0)
in(key, v::KeySet{<:Any, <:Dict}) = (ht_keyindex(v.dict, key) >= 0)

"""
    getkey(collection, key, default)

Return the key matching argument `key` if one exists in `collection`, otherwise return `default`.

# Examples
```jldoctest
julia> D = Dict('a'=>2, 'b'=>3)
Dict{Char, Int64} with 2 entries:
  'a' => 2
  'b' => 3

julia> getkey(D, 'a', 1)
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> getkey(D, 'd', 'a')
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```
"""
function getkey(h::Dict{K,V}, key, default) where V where K
    index = ht_keyindex(h, key)
    @inbounds return (index<0) ? default : h.keys[index]::K
end

function _pop!(h::Dict, index)
    @inbounds val = h.vals[index]
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
    idx = skip_deleted_floor!(h)
    @inbounds key = h.keys[idx]
    @inbounds val = h.vals[idx]
    _delete!(h, idx)
    key => val
end

function _delete!(h::Dict{K,V}, index) where {K,V}
    @inbounds begin
    slots = h.slots
    sz = length(slots)
    _unsetindex!(h.keys, index)
    _unsetindex!(h.vals, index)
    # if the next slot is empty we don't need a tombstone
    # and can remove all tombstones that were required by the element we just deleted
    ndel = 1
    nextind = (index & (sz-1)) + 1
    if isslotempty(h, nextind)
        while true
            ndel -= 1
            slots[index] = 0x00
            index = ((index - 2) & (sz-1)) + 1
            isslotmissing(h, index) || break
        end
    else
        slots[index] = 0x7f
    end
    h.ndel += ndel
    h.count -= 1
    h.age += 1
    return h
    end
end

"""
    delete!(collection, key)

Delete the mapping for the given key in a collection, if any, and return the collection.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2)
Dict{String, Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> delete!(d, "b")
Dict{String, Int64} with 1 entry:
  "a" => 1

julia> delete!(d, "b") # d is left unchanged
Dict{String, Int64} with 1 entry:
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
    for i = i:L
        @inbounds if isslotfilled(h,i)
            return  i
        end
    end
    return 0
end
function skip_deleted_floor!(h::Dict)
    idx = skip_deleted(h, h.idxfloor)
    if idx != 0
        h.idxfloor = idx
    end
    idx
end

@propagate_inbounds _iterate(t::Dict{K,V}, i) where {K,V} = i == 0 ? nothing : (Pair{K,V}(t.keys[i],t.vals[i]), i == typemax(Int) ? 0 : i+1)
@propagate_inbounds function iterate(t::Dict)
    _iterate(t, skip_deleted(t, t.idxfloor))
end
@propagate_inbounds iterate(t::Dict, i) = _iterate(t, skip_deleted(t, i))

isempty(t::Dict) = (t.count == 0)
length(t::Dict) = t.count

@propagate_inbounds function Base.iterate(v::T, i::Int = v.dict.idxfloor) where T <: Union{KeySet{<:Any, <:Dict}, ValueIterator{<:Dict}}
    i == 0 && return nothing
    i = skip_deleted(v.dict, i)
    i == 0 && return nothing
    vals = T <: KeySet ? v.dict.keys : v.dict.vals
    (@inbounds vals[i], i == typemax(Int) ? 0 : i+1)
end

function filter!(pred, h::Dict{K,V}) where {K,V}
    h.count == 0 && return h
    @inbounds for i=1:length(h.slots)
        if ((h.slots[i] & 0x80) != 0) && !pred(Pair{K,V}(h.keys[i], h.vals[i]))
            _delete!(h, i)
        end
    end
    return h
end

function reduce(::typeof(merge), items::Vector{<:Dict})
    K = mapreduce(keytype, promote_type, items)
    V = mapreduce(valtype, promote_type, items)
    return reduce(merge!, items; init=Dict{K,V}())
end

function map!(f, iter::ValueIterator{<:Dict})
    dict = iter.dict
    vals = dict.vals
    # @inbounds is here so that it gets propagated to isslotfilled
    @inbounds for i = dict.idxfloor:lastindex(vals)
        if isslotfilled(dict, i)
            vals[i] = f(vals[i])
        end
    end
    return iter
end

function mergewith!(combine, d1::Dict{K, V}, d2::AbstractDict) where {K, V}
    haslength(d2) && _sizehint!(d1, length(d1) + length(d2), shrink = false)
    for (k, v) in d2
        i, sh = ht_keyindex2_shorthash!(d1, k)
        if i > 0
            d1.vals[i] = combine(d1.vals[i], v)
        else
            if !(k isa K)
                k1 = convert(K, k)::K
                if !isequal(k, k1)
                    throw(ArgumentError("$(limitrepr(k)) is not a valid key for type $K"))
                end
                k = k1
            end
            if !isa(v, V)
                v = convert(V, v)::V
            end
            @inbounds _setindex!(d1, v, k, -i, sh)
        end
    end
    return d1
end

struct ImmutableDict{K,V} <: AbstractDict{K,V}
    parent::ImmutableDict{K,V}
    key::K
    value::V
    ImmutableDict{K,V}() where {K,V} = new() # represents an empty dictionary
    ImmutableDict{K,V}(key, value) where {K,V} = (empty = new(); new(empty, key, value))
    ImmutableDict{K,V}(parent::ImmutableDict, key, value) where {K,V} = new(parent, key, value)
end

"""
    ImmutableDict

`ImmutableDict` is a dictionary implemented as an immutable linked list,
which is optimal for small dictionaries that are constructed over many individual insertions.
Note that it is not possible to remove a value, although it can be partially overridden and hidden
by inserting a new value with the same key.

    ImmutableDict(KV::Pair)

Create a new entry in the `ImmutableDict` for a `key => value` pair

 - use `(key => value) in dict` to see if this particular combination is in the properties set
 - use `get(dict, key, default)` to retrieve the most recent value for a particular key

"""
ImmutableDict
ImmutableDict(KV::Pair{K,V}) where {K,V} = ImmutableDict{K,V}(KV[1], KV[2])
ImmutableDict(t::ImmutableDict{K,V}, KV::Pair) where {K,V} = ImmutableDict{K,V}(t, KV[1], KV[2])
ImmutableDict(t::ImmutableDict{K,V}, KV::Pair, rest::Pair...) where {K,V} =
    ImmutableDict(ImmutableDict(t, KV), rest...)
ImmutableDict(KV::Pair, rest::Pair...) = ImmutableDict(ImmutableDict(KV), rest...)

function in(key_value::Pair, dict::ImmutableDict, valcmp=(==))
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
        isequal(dict.key, key) && return true
        dict = dict.parent
    end
    return false
end

function getindex(dict::ImmutableDict, key)
    while isdefined(dict, :parent)
        isequal(dict.key, key) && return dict.value
        dict = dict.parent
    end
    throw(KeyError(key))
end
function get(dict::ImmutableDict, key, default)
    while isdefined(dict, :parent)
        isequal(dict.key, key) && return dict.value
        dict = dict.parent
    end
    return default
end

function get(default::Callable, dict::ImmutableDict, key)
    while isdefined(dict, :parent)
        isequal(dict.key, key) && return dict.value
        dict = dict.parent
    end
    return default()
end

# this actually defines reverse iteration (e.g. it should not be used for merge/copy/filter type operations)
function iterate(d::ImmutableDict{K,V}, t=d) where {K, V}
    !isdefined(t, :parent) && return nothing
    (Pair{K,V}(t.key, t.value), t.parent)
end
length(t::ImmutableDict) = count(Returns(true), t)
isempty(t::ImmutableDict) = !isdefined(t, :parent)
empty(::ImmutableDict, ::Type{K}, ::Type{V}) where {K, V} = ImmutableDict{K,V}()

_similar_for(c::AbstractDict, ::Type{Pair{K,V}}, itr, isz, len) where {K, V} = empty(c, K, V)
_similar_for(c::AbstractDict, ::Type{T}, itr, isz, len) where {T} =
    throw(ArgumentError("for AbstractDicts, similar requires an element type of Pair;\n  if calling map, consider a comprehension instead"))


include("hamt.jl")
using .HashArrayMappedTries
const HAMT = HashArrayMappedTries

struct PersistentDict{K,V} <: AbstractDict{K,V}
    trie::HAMT.HAMT{K,V}
end

"""
    PersistentDict

`PersistentDict` is a dictionary implemented as an hash array mapped trie,
which is optimal for situations where you need persistence, each operation
returns a new dictonary separate from the previous one, but the underlying
implementation is space-efficient and may share storage across multiple
separate dictionaries.

    PersistentDict(KV::Pair)

# Examples

```jldoctest
julia> dict = Base.PersistentDict(:a=>1)
Base.PersistentDict{Symbol, Int64} with 1 entry:
  :a => 1

julia> dict2 = Base.delete(dict, :a)
Base.PersistentDict{Symbol, Int64}()

julia> dict3 = Base.PersistentDict(dict, :a=>2)
Base.PersistentDict{Symbol, Int64} with 1 entry:
  :a => 2
```
"""
PersistentDict

PersistentDict{K,V}() where {K,V} = PersistentDict(HAMT.HAMT{K,V}())
PersistentDict{K,V}(KV::Pair) where {K,V} = PersistentDict(HAMT.HAMT{K,V}(KV...))
PersistentDict(KV::Pair{K,V}) where {K,V} = PersistentDict(HAMT.HAMT{K,V}(KV...))
PersistentDict(dict::PersistentDict, pair::Pair) = PersistentDict(dict, pair...)
PersistentDict{K,V}(dict::PersistentDict{K,V}, pair::Pair) where {K,V} = PersistentDict(dict, pair...)
function PersistentDict(dict::PersistentDict{K,V}, key, val) where {K,V}
    key = convert(K, key)
    val = convert(V, val)
    trie = dict.trie
    h = hash(key)
    found, present, trie, i, bi, top, hs = HAMT.path(trie, key, h, #=persistent=# true)
    HAMT.insert!(found, present, trie, i, bi, hs, val)
    return PersistentDict(top)
end

function PersistentDict(kv::Pair, rest::Pair...)
    dict = PersistentDict(kv)
    for kv in rest
        key, value = kv
        dict = PersistentDict(dict, key, value)
    end
    return dict
end

eltype(::PersistentDict{K,V}) where {K,V} = Pair{K,V}

function in(key_val::Pair{K,V}, dict::PersistentDict{K,V}, valcmp=(==)) where {K,V}
    trie = dict.trie
    if HAMT.islevel_empty(trie)
        return false
    end

    key, val = key_val

    h = hash(key)
    found, present, trie, i, _, _, _ = HAMT.path(trie, key, h)
    if found && present
        leaf = @inbounds trie.data[i]::HAMT.Leaf{K,V}
        return valcmp(val, leaf.val) && return true
    end
    return false
end

function haskey(dict::PersistentDict{K}, key::K) where K
    trie = dict.trie
    h = hash(key)
    found, present, _, _, _, _, _ = HAMT.path(trie, key, h)
    return found && present
end

function getindex(dict::PersistentDict{K,V}, key::K) where {K,V}
    trie = dict.trie
    if HAMT.islevel_empty(trie)
        throw(KeyError(key))
    end
    h = hash(key)
    found, present, trie, i, _, _, _ = HAMT.path(trie, key, h)
    if found && present
        leaf = @inbounds trie.data[i]::HAMT.Leaf{K,V}
        return leaf.val
    end
    throw(KeyError(key))
end

function get(dict::PersistentDict{K,V}, key::K, default) where {K,V}
    trie = dict.trie
    if HAMT.islevel_empty(trie)
        return default
    end
    h = hash(key)
    found, present, trie, i, _, _, _ = HAMT.path(trie, key, h)
    if found && present
        leaf = @inbounds trie.data[i]::HAMT.Leaf{K,V}
        return leaf.val
    end
    return default
end

function get(default::Callable, dict::PersistentDict{K,V}, key::K) where {K,V}
    trie = dict.trie
    if HAMT.islevel_empty(trie)
        return default
    end
    h = hash(key)
    found, present, trie, i, _, _, _ = HAMT.path(trie, key, h)
    if found && present
        leaf = @inbounds trie.data[i]::HAMT.Leaf{K,V}
        return leaf.val
    end
    return default()
end

iterate(dict::PersistentDict, state=nothing) = HAMT.iterate(dict.trie, state)

function delete(dict::PersistentDict{K}, key::K) where K
    trie = dict.trie
    h = hash(key)
    found, present, trie, i, bi, top, _ = HAMT.path(trie, key, h, #=persistent=# true)
    if found && present
        deleteat!(trie.data, i)
        HAMT.unset!(trie, bi)
    end
    return PersistentDict(top)
end

length(dict::PersistentDict) = HAMT.length(dict.trie)
isempty(dict::PersistentDict) = HAMT.isempty(dict.trie)
empty(::PersistentDict, ::Type{K}, ::Type{V}) where {K, V} = PersistentDict{K, V}()
