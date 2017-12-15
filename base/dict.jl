# This file is a part of Julia. License is MIT: https://julialang.org/license

function _truncate_at_width_or_chars(str, width, chars="", truncmark="…")
    truncwidth = Unicode.textwidth(truncmark)
    (width <= 0 || width < truncwidth) && return ""

    wid = truncidx = lastidx = 0
    idx = start(str)
    while !done(str, idx)
        lastidx = idx
        c, idx = next(str, idx)
        wid += Unicode.textwidth(c)
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

function show(io::IO, t::AbstractDict{K,V}) where V where K
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
"""
Abstract supertype of all types using the standard, Dict-like
hash-table internal setup.  Rarely, if ever, should this be used for
dispatch in user-code, instead use `AbstractDict`.

Subtypes distinguish themselves by implementing different hashing and
equality, e.g. `Dict` uses normal `hash` & `isequal` on the other hand
`ObjectIdDict` uses `object_id` as hash and `===`.
"""
abstract type HashDict{K,V} <: AbstractDict{K,V} end

"""
    keycomparison(h::HashDict) -> used comparison function

Return (and thus set) the equality function to use for key-comparison in
an subtype of HashDict.
Needs to be consistent with the `keyhash` function:
keycomparison(h)(k1, k2) must imply keyhash(h)(k1)==keyhash(h)(k2).
"""
function keycomparison end

"""
    keyhash(h::HashDict) -> used hash function

Set the hash function to use for key-hashing in an HashDict subtype.
Needs to be consistent with `keycomparison`:
keycomparison(h)(k1, k2) must imply keyhash(h)(k1)==keyhash(h)(k2).
"""
function keyhash end

# The interface of HashDict

# A bunch of helper functions used by the constructors:

TP{K,V} = Union{Type{Tuple{K,V}},Type{Pair{K,V}}}

dict_with_eltype(DT_apply, kv, ::TP{K,V}) where {K,V} = DT_apply(K, V)(kv)
dict_with_eltype(DT_apply, kv::Generator, ::TP{K,V}) where {K,V} = DT_apply(K, V)(kv)
dict_with_eltype(DT_apply, ::Type{Pair{K,V}}) where {K,V} = DT_apply(K, V)()
dict_with_eltype(DT_apply, ::Type) = DT_apply(Any, Any)()
dict_with_eltype(DT_apply::F, kv, t) where {F} = grow_to!(dict_with_eltype(DT_apply, @default_eltype(typeof(kv))), kv)
function dict_with_eltype(DT_apply::F, kv::Generator, t) where F
    T = @default_eltype(typeof(kv))
    if T <: Union{Pair, Tuple{Any, Any}} && _isleaftype(T)
        return dict_with_eltype(DT_apply, kv, T)
    end
    return grow_to!(dict_with_eltype(DT_apply, T), kv)
end

# this is a special case due to (1) allowing both Pairs and Tuples as elements,
# and (2) Pair being invariant. a bit annoying.
function grow_to!(dest::AbstractDict, itr)
    out = grow_to!(empty(dest, Union{}, Union{}), itr, start(itr))
    return isempty(out) ? dest : out
end

function grow_to!(dest::AbstractDict{K,V}, itr, st) where V where K
    while !done(itr, st)
        (k,v), st = next(itr, st)
        if isa(k,K) && isa(v,V)
            dest[k] = v
        else
            new = empty(dest, typejoin(K,typeof(k)), typejoin(V,typeof(v)))
            copy!(new, dest)
            new[k] = v
            return grow_to!(new, itr, st)
        end
    end
    return dest
end

# constructors
function (::Type{HD})(kv) where HD<:HashDict
    h = HD()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
(::Type{HD})(p::Pair) where HD<:HashDict = setindex!(HD(), p.second, p.first)

function (::Type{HD})(ps::Pair...) where HD<:HashDict
    h = HD()
    sizehint!(h, length(ps))
    for p in ps
        h[p.first] = p.second
    end
    return h
end

copy(d::HD) where HD<:HashDict = HD(d)

max_values(::Type) = typemax(Int)
max_values(T::Type{<:Union{Void,BitIntegerSmall}}) = 1 << (8*sizeof(T))
max_values(T::Union) = max(max_values(T.a), max_values(T.b))
max_values(::Type{Bool}) = 2

function sizehint!(h::HashDict{T}, newsz) where T
    oldsz = length(h.ht.slots)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash!() assumes everything fits. it was only designed
        # for growing.
        return h
    end
    # grow at least 25%
    newsz = min(max(newsz, (oldsz*5)>>2),
                max_values(T))
    rehash!(h.ht, keyhash(h), newsz)
    return h
end

# x-ref https://github.com/JuliaLang/julia/commit/e7ce4cba44fa3b508fd50e0c3d03f6bc5a7a5032
"""
    rehash!(h)

Convenience call for rehashing a hash-based dictionary or set.

This is relevant particularly to precompilation, which requires that
any module global dicts get rehashed by the __init__ function.
"""
rehash!

rehash!(h::HashDict) = rehash!(h.ht, keyhash(h))

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
function empty!(h::HashDict{K,V}) where {K, V}
    _empty!(h.ht)
    return h
end

function setindex!(h::HashDict{K}, v0, key0) where K
    key = convert(K, key0)
    comp = keycomparison(h)
    if !comp(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    _setindex!(h.ht, v0, key, keyhash(h), keycomparison(h))
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

get!(h::HashDict{K,V}, key0, default) where {K,V} = get!(()->default, h, key0)

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

function get!(default::Callable, h::HashDict{K,V}, key0) where V where K
    key = convert(K, key0)
    comp = keycomparison(h)
    if !comp(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    return get!(default, h, key)
end
get!(default::Callable, h::HashDict{K,V}, key::K) where V where K =
    _get!(default, h.ht, key, keyhash(h), keycomparison(h))

# NOTE: this macro is trivial, and should
#       therefore not be exported as-is: it's for internal use only.
macro get!(h, key0, default)
    return quote
        get!(()->$(esc(default)), $(esc(h)), $(esc(key0)))
    end
end

function getindex(h::HashDict{K,V}, key) where V where K
    index = ht_keyindex(h.ht, keyhash(h), keycomparison(h), key)
    @inbounds return (index < 0) ? throw(KeyError(key)) : h.ht.vals[index]::V
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

function get(h::HashDict{K,V}, key, default) where V where K
    index = ht_keyindex(h.ht, keyhash(h), keycomparison(h), key)
    @inbounds return (index < 0) ? default : h.ht.vals[index]::V
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

function get(default::Callable, h::HashDict{K,V}, key) where V where K
    index = ht_keyindex(h.ht, keyhash(h), keycomparison(h), key)
    @inbounds return (index < 0) ? default() : h.ht.vals[index]::V
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
haskey(h::HashDict, key) = ht_keyindex(h.ht, keyhash(h), keycomparison(h), key) > 0
in(key, v::KeySet{<:Any, <:HashDict}) = ht_keyindex(v.dict.ht, keyhash(v.dict), keycomparison(v.dict), key) > 0

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
function getkey(h::HashDict{K,V}, key, default) where V where K
    index = ht_keyindex(h.ht, keyhash(h), keycomparison(h), key)
    @inbounds return (index<0) ? default : h.ht.keys[index]::K
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

pop!(h::HashDict, key) = _pop!(h.ht, key, keyhash(h), keycomparison(h))

function pop!(h::HashDict, key, default)
    index = ht_keyindex(h.ht, keyhash(h), keycomparison(h), key)
    return index > 0 ? _pop!(h.ht, index, key) : default
end

function pop!(h::HashDict)
    isempty(h) && throw(ArgumentError("dict must be non-empty"))
    idx = start(h)
    @inbounds key = h.ht.keys[idx]
    @inbounds val = h.ht.vals[idx]
    _delete!(h.ht, idx)
    key => val
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

function delete!(h::HashDict, key)
    _delete!(h.ht, key, keyhash(h), keycomparison(h))
    return h
end

function skip_deleted(h::HashDict, i)
    L = length(h.ht.slots)
    @inbounds while i<=L && !isslotfilled(h.ht,i)
        i += 1
    end
    return i
end

function start(h::HashDict)
    i = skip_deleted(h, h.ht.idxfloor)
    h.ht.idxfloor = i
    return i
end
done(h::HashDict, i) = i > length(h.ht.vals)
@propagate_inbounds function next(h::HashDict{K,V}, i) where {K,V}
    return (Pair{K,V}(h.ht.keys[i],h.ht.vals[i]), skip_deleted(h,i+1))
end

isempty(h::HashDict) = (h.ht.count == 0)
length(h::HashDict) = h.ht.count

@propagate_inbounds function next(v::KeySet{<:Any, <:HashDict}, i)
    return (v.dict.ht.keys[i], skip_deleted(v.dict,i+1))
end
@propagate_inbounds function next(v::ValueIterator{<:HashDict}, i)
    return (v.dict.ht.vals[i], skip_deleted(v.dict,i+1))
end

filter!(f, h::HashDict) = filter_in_one_pass!(f, h)

#####
# BaseDict is used as backend for the different Dicts

"""
BaseDict holds all fields which store the dict.
"""
mutable struct BaseDict{K,V}
    slots::Array{UInt8,1}
    keys::Array{K,1}
    vals::Array{V,1}
    ndel::Int
    count::Int
    age::UInt
    idxfloor::Int  # an index <= the indexes of all used slots
    maxprobe::Int

    function BaseDict{K,V}() where {K, V}
        n = 16
        new(zeros(UInt8,n), Vector{K}(uninitialized, n), Vector{V}(uninitialized, n), 0, 0, 0, 1, 0)
    end
    function BaseDict{K,V}(ht::BaseDict{K,V}) where {K, V}
        new(copy(ht.slots), copy(ht.keys), copy(ht.vals), ht.ndel, ht.count, ht.age,
            ht.idxfloor, ht.maxprobe)
    end
    function BaseDict{K, V}(slots, keys, vals, ndel, count, age, idxfloor, maxprobe) where {K, V}
        new(slots, keys, vals, ndel, count, age, idxfloor, maxprobe)
    end
end

# These can be changed, to trade off better performance for space
const global maxallowedprobe = 16
const global maxprobeshift   = 6

_tablesz(x::Integer) = x < 16 ? 16 : one(x)<<((sizeof(x)<<3)-leading_zeros(x-1))

hashindex(hashfn, key, sz) = (((hashfn(key)%Int) & (sz-1)) + 1)::Int

@propagate_inbounds isslotempty(ht::BaseDict, i::Int) = ht.slots[i] == 0x0
@propagate_inbounds isslotfilled(ht::BaseDict, i::Int) = ht.slots[i] == 0x1
@propagate_inbounds isslotmissing(ht::BaseDict, i::Int) = ht.slots[i] == 0x2

function rehash!(ht::BaseDict{K,V}, hashfn, newsz = length(ht.keys)) where V where K
    olds = ht.slots
    oldk = ht.keys
    oldv = ht.vals
    sz = length(olds)
    newsz = _tablesz(newsz)
    ht.age += 1
    ht.idxfloor = 1
    if ht.count == 0
        resize!(ht.slots, newsz)
        fill!(ht.slots, 0)
        resize!(ht.keys, newsz)
        resize!(ht.vals, newsz)
        ht.ndel = 0
        return ht
    end

    slots = zeros(UInt8,newsz)
    keys = Vector{K}(uninitialized, newsz)
    vals = Vector{V}(uninitialized, newsz)
    age0 = ht.age
    count = 0
    maxprobe = ht.maxprobe

    for i = 1:sz
        @inbounds if olds[i] == 0x1
            k = oldk[i]
            v = oldv[i]
            index0 = index = hashindex(hashfn, k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            probe = (index - index0) & (newsz-1)
            probe > maxprobe && (maxprobe = probe)
            slots[index] = 0x1
            keys[index] = k
            vals[index] = v
            count += 1

            if ht.age != age0
                # if `ht` is changed by a finalizer, retry
                return rehash!(ht, hashfn, newsz)
            end
        end
    end

    ht.slots = slots
    ht.keys = keys
    ht.vals = vals
    ht.count = count
    ht.ndel = 0
    ht.maxprobe = maxprobe
    @assert ht.age == age0

    return ht
end

function _empty!(ht::BaseDict{K,V}) where V where K
    fill!(ht.slots, 0x0)
    sz = length(ht.slots)
    empty!(ht.keys)
    empty!(ht.vals)
    resize!(ht.keys, sz)
    resize!(ht.vals, sz)
    ht.ndel = 0
    ht.count = 0
    ht.age += 1
    ht.idxfloor = 1
    return ht
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex(ht::BaseDict, hashfn, cmpfn, key)
    sz = length(ht.keys)
    iter = 0
    maxprobe = ht.maxprobe
    index = hashindex(hashfn, key, sz)
    keys = ht.keys

    @inbounds while true
        if isslotempty(ht,index)
            break
        end
        if !isslotmissing(ht,index) && (key === keys[index] || cmpfn(key, keys[index]))
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
function ht_keyindex2!(ht::BaseDict, hashfn, cmpfn, key)
    age0 = ht.age
    sz = length(ht.keys)
    iter = 0
    maxprobe = ht.maxprobe
    index = hashindex(hashfn, key, sz)
    avail = 0
    keys = ht.keys

    @inbounds while true
        if isslotempty(ht,index)
            if avail < 0
                return avail
            end
            return -index
        end

        if isslotmissing(ht,index)
            if avail == 0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = -index
            end
        elseif key === keys[index] || cmpfn(key, keys[index])
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
        if !isslotfilled(ht,index)
            ht.maxprobe = iter
            return -index
        end
        index = (index & (sz-1)) + 1
        iter += 1
    end

    rehash!(ht, hashfn, ht.count > 64000 ? sz*2 : sz*4)

    return ht_keyindex2!(ht, hashfn, cmpfn, key)
end

@propagate_inbounds function _setindex!(ht::BaseDict, v, key, index, hashfn, cmpfn)
    ht.slots[index] = 0x1
    ht.keys[index] = key
    ht.vals[index] = v
    ht.count += 1
    ht.age += 1
    if index < ht.idxfloor
        ht.idxfloor = index
    end

    sz = length(ht.keys)
    # Rehash now if necessary
    if ht.ndel >= ((3*sz)>>2) || ht.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash!(ht, hashfn, ht.count > 64000 ? ht.count*2 : ht.count*4)
    end

    return ht
end

function _setindex!(ht::BaseDict{K,V}, v0, key::K, hashfn, cmpfn) where {K, V}
    v = convert(V, v0)
    index = ht_keyindex2!(ht, hashfn, cmpfn, key)

    if index > 0
        ht.age += 1
        @inbounds ht.keys[index] = key
        @inbounds ht.vals[index] = v
    else
        @inbounds _setindex!(ht, v, key, -index, hashfn, cmpfn)
    end

    return ht
end

function _get!(default::Callable, ht::BaseDict{K,V}, key::K, hashfn, cmpfn) where V where K
    index = ht_keyindex2!(ht, hashfn, cmpfn, key)

    index > 0 && return ht.vals[index]

    age0 = ht.age
    v = convert(V, default())
    if ht.age != age0
        index = ht_keyindex2!(ht, hashfn, cmpfn, key)
    end
    if index > 0
        ht.age += 1
        @inbounds ht.keys[index] = key
        @inbounds ht.vals[index] = v
    else
        @inbounds _setindex!(ht, v, key, -index, hashfn, cmpfn)
    end
    return v
end

function _pop!(ht::BaseDict, key, hashfn, cmpfn)
    index = ht_keyindex(ht, hashfn, cmpfn, key)
    _pop!(ht, index, key)
end
function _pop!(ht::BaseDict, index, key)
    index < 0 && throw(KeyError(key))
    val = ht.vals[index]
    _delete!(ht, index)
    return val
end


function _delete!(ht::BaseDict, key, hashfn, cmpfn)
    index = ht_keyindex(ht, hashfn, cmpfn, key)
    _delete!(ht, index)
end
function _delete!(ht::BaseDict, index)
    if index > 0
        ht.slots[index] = 0x2
        ccall(:jl_arrayunset, Void, (Any, UInt), ht.keys, index-1)
        ccall(:jl_arrayunset, Void, (Any, UInt), ht.vals, index-1)
        ht.ndel += 1
        ht.count -= 1
        ht.age += 1
    end
    return ht
end



#################################
# The standard dict:
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
struct Dict{K,V} <: HashDict{K,V}
    ht::BaseDict{K,V}

    Dict{K,V}() where {K, V} =
        new(BaseDict{K,V}())
    Dict{K,V}(h::Dict{K,V}) where {K, V} =
        new(BaseDict{K,V}(h.ht))
end
keycomparison(::Dict) = isequal
keyhash(::Dict) = hash
const AnyDict = Dict{Any,Any}

# Default `empty` is to return an empty Dict
empty(a::AbstractDict, ::Type{K}, ::Type{V}) where {K, V} = Dict{K, V}()

# Conversion to Dict from other AbstractDict
function convert(::Type{Dict{K,V}},d::AbstractDict) where V where K
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

# The Egal Dict, aka ObjectIdDict with type-parameters
"""
    ObjectIdDict([itr])

`ObjectIdDict{K,V}()` constructs a hash table based `object_id`-hash with keys
of type `K` and values of type `V`.

Given a single iterable argument, constructs a [`ObjectIdDict`](@ref) whose key-value
pairs are taken from 2-tuples `(key,value)` generated by the argument.

```jldoctest
julia> ObjectIdDict([("A", 1), ("B", 2)])
ObjectIdDict{String,Int64} with 2 entries:
  "B" => 2
  "A" => 1
```

Alternatively, a sequence of pair arguments may be passed.

```jldoctest
julia> ObjectIdDict("A"=>1, "B"=>2)
ObjectIdDict{String,Int64} with 2 entries:
  "B" => 2
  "A" => 1
```
"""
struct ObjectIdDict{K,V} <: HashDict{K,V}
    ht::BaseDict{K,V}

    ObjectIdDict{K,V}() where {K, V} =
        new(BaseDict{K,V}())
    ObjectIdDict{K,V}(h::ObjectIdDict{K,V}) where {K, V} =
        new(BaseDict{K,V}(h.ht))
end
keycomparison(::ObjectIdDict) = ===
keyhash(::ObjectIdDict) = object_id

# constructors
for D in [:Dict, :ObjectIdDict]
    eval(
    quote
        # Note the constructors of WeakKeyDict mirror these here, keep in sync.
        $D() = $D{Any,Any}()
        $D(kv::Tuple{}) = $D()

        $D(ps::Pair{K,V}...)           where {K,V} = $D{K,V}(ps)
        $D(ps::Pair{K}...)             where {K}   = $D{K,Any}(ps)
        $D(ps::(Pair{K,V} where K)...) where {V}   = $D{Any,V}(ps)
        $D(ps::Pair...)                            = $D{Any,Any}(ps)

        function $D(kv)
            try
                dict_with_eltype((K, V) -> $D{K, V}, kv, eltype(kv))
            catch e
                if !applicable(start, kv) || !all(x->isa(x,Union{Tuple,Pair}),kv)
                    throw(ArgumentError("$($D)(kv): kv needs to be an iterator of tuples or pairs"))
                else
                    rethrow(e)
                end
            end
        end
        empty(a::$D, ::Type{K}, ::Type{V}) where {K, V} = $D{K, V}()
    end
    )
end
#################
# Immutable Dict
#################

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

function in(key_value::Pair, dict::ImmutableDict, valcmp=(==))
    key, value = key_value
    while isdefined(dict, :parent)
        if dict.key == key
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
empty(::ImmutableDict, ::Type{K}, ::Type{V}) where {K, V} = ImmutableDict{K,V}()

_similar_for(c::HashDict, ::Type{Pair{K,V}}, itr, isz) where {K, V} = empty(c, K, V)
_similar_for(c::AbstractDict, T, itr, isz) = throw(ArgumentError("for AbstractDicts, similar requires an element type of Pair;\n  if calling map, consider a comprehension instead"))
