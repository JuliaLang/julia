# This file is a part of Julia. License is MIT: https://julialang.org/license

# weak key and weak value dictionaries

mutable struct WeakKeyDictFinalizer{T}
    const d::T
end
(d::WeakKeyDictFinalizer)(k) = d.d.dirty = true


"""
    WeakKeyDict([itr])

`WeakKeyDict()` constructs a hash table where the keys are weak
references to objects which may be garbage collected even when
referenced in a hash table.

See [`Dict`](@ref) for further help.  Note, unlike [`Dict`](@ref),
`WeakKeyDict` does not convert keys on insertion, as this would imply the key
object was unreferenced anywhere before insertion.

See also [`WeakRef`](@ref).
"""
mutable struct WeakKeyDict{K,V} <: AbstractDict{K,V}
    const ht::Dict{WeakRef,V}
    const lock::ReentrantLock
    dirty::Bool
    finalizer::WeakKeyDictFinalizer

    # Constructors mirror Dict's
    function WeakKeyDict{K,V}() where {K, V}
        t = new{K,V}(Dict{WeakRef,V}(), ReentrantLock(), false)
        t.finalizer = WeakKeyDictFinalizer(t)
        return t
    end
end
function WeakKeyDict{K,V}(kv) where V where K
    h = WeakKeyDict{K,V}()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
WeakKeyDict{K,V}(p::Pair) where V where K = setindex!(WeakKeyDict{K,V}(), p.second, p.first)
function WeakKeyDict{K,V}(ps::Pair...) where V where K
    h = WeakKeyDict{K,V}()
    sizehint!(h, length(ps))
    for p in ps
        h[p.first] = p.second
    end
    return h
end
WeakKeyDict() = WeakKeyDict{Any,Any}()

WeakKeyDict(kv::Tuple{}) = WeakKeyDict()
copy(d::WeakKeyDict) = WeakKeyDict(d)

WeakKeyDict(ps::Pair{K,V}...)           where {K,V} = WeakKeyDict{K,V}(ps)
WeakKeyDict(ps::Pair{K}...)             where {K}   = WeakKeyDict{K,Any}(ps)
WeakKeyDict(ps::(Pair{K,V} where K)...) where {V}   = WeakKeyDict{Any,V}(ps)
WeakKeyDict(ps::Pair...)                            = WeakKeyDict{Any,Any}(ps)

WeakKeyDict(kv) = Base.dict_with_eltype((K, V) -> WeakKeyDict{K, V}, kv, eltype(kv))

function _cleanup_locked(h::WeakKeyDict)
    if h.dirty
        h.dirty = false
        idx = skip_deleted_floor!(h.ht)
        while idx != 0
            if h.ht.keys[idx].value === nothing
                _delete!(h.ht, idx)
            end
            idx = skip_deleted(h.ht, idx + 1)
        end
    end
    return h
end

sizehint!(d::WeakKeyDict, newsz::Integer; shrink::Bool = true) = @lock d sizehint!(d.ht, newsz; shrink = shrink)
empty(d::WeakKeyDict, ::Type{K}, ::Type{V}) where {K, V} = WeakKeyDict{K, V}()

IteratorSize(::Type{<:WeakKeyDict}) = SizeUnknown()

islocked(wkh::WeakKeyDict) = islocked(wkh.lock)
lock(wkh::WeakKeyDict) = lock(wkh.lock)
unlock(wkh::WeakKeyDict) = unlock(wkh.lock)
lock(f, wkh::WeakKeyDict) = lock(f, wkh.lock)
trylock(f, wkh::WeakKeyDict) = trylock(f, wkh.lock)

function setindex!(wkh::WeakKeyDict{K}, v, key) where K
    !isa(key, K) && throw(ArgumentError("$(limitrepr(key)) is not a valid key for type $K"))
    # 'nothing' is not valid both because 'finalizer' will reject it,
    # and because we therefore use it as a sentinel value
    key === nothing && throw(ArgumentError("`nothing` is not a valid WeakKeyDict key"))
    lock(wkh) do
        _cleanup_locked(wkh)
        k = getkey(wkh.ht, key, nothing)
        if k === nothing
            finalizer(wkh.finalizer, key)
            k = WeakRef(key)
        else
            k.value = key
        end
        wkh.ht[k] = v
    end
    return wkh
end
function get!(wkh::WeakKeyDict{K}, key, default) where {K}
    v = lock(wkh) do
        if key !== nothing && haskey(wkh.ht, key)
            wkh.ht[key]
        else
            wkh[key] = default
        end
    end
    return v
end
function get!(default::Callable, wkh::WeakKeyDict{K}, key) where {K}
    v = lock(wkh) do
        if key !== nothing && haskey(wkh.ht, key)
            wkh.ht[key]
        else
            wkh[key] = default()
        end
    end
    return v
end

function getkey(wkh::WeakKeyDict{K}, kk, default) where K
    k = lock(wkh) do
        local k = getkey(wkh.ht, kk, nothing)
        k === nothing && return nothing
        return k.value
    end
    return k === nothing ? default : k::K
end

map!(f, iter::ValueIterator{<:WeakKeyDict})= map!(f, values(iter.dict.ht))

function get(wkh::WeakKeyDict{K}, key, default) where {K}
    key === nothing && throw(KeyError(nothing))
    lock(wkh) do
        return get(wkh.ht, key, default)
    end
end
function get(default::Callable, wkh::WeakKeyDict{K}, key) where {K}
    key === nothing && throw(KeyError(nothing))
    lock(wkh) do
        return get(default, wkh.ht, key)
    end
end
function pop!(wkh::WeakKeyDict{K}, key) where {K}
    key === nothing && throw(KeyError(nothing))
    lock(wkh) do
        return pop!(wkh.ht, key)
    end
end
function pop!(wkh::WeakKeyDict{K}, key, default) where {K}
    key === nothing && return default
    lock(wkh) do
        return pop!(wkh.ht, key, default)
    end
end
function delete!(wkh::WeakKeyDict, key)
    key === nothing && return wkh
    lock(wkh) do
        delete!(wkh.ht, key)
    end
    return wkh
end
function empty!(wkh::WeakKeyDict)
    lock(wkh) do
        empty!(wkh.ht)
    end
    return wkh
end
function haskey(wkh::WeakKeyDict{K}, key) where {K}
    key === nothing && return false
    lock(wkh) do
        return haskey(wkh.ht, key)
    end
end
function getindex(wkh::WeakKeyDict{K}, key) where {K}
    key === nothing && throw(KeyError(nothing))
    lock(wkh) do
        return getindex(wkh.ht, key)
    end
end
isempty(wkh::WeakKeyDict) = length(wkh) == 0
function length(t::WeakKeyDict)
    lock(t) do
        _cleanup_locked(t)
        return length(t.ht)
    end
end

function iterate(t::WeakKeyDict{K,V}, state...) where {K, V}
    @lock t begin
        while true
            y = iterate(t.ht, state...)
            y === nothing && return nothing
            wkv, state = y
            k = wkv[1].value
            GC.safepoint() # ensure `k` is now gc-rooted
            k === nothing && continue # indicates `k` is scheduled for deletion
            kv = Pair{K,V}(k::K, wkv[2])
            return (kv, state)
        end
    end
end

@propagate_inbounds Iterators.only(d::WeakKeyDict) = Iterators._only(d, first)

filter!(f, d::WeakKeyDict) = filter_in_one_pass!(f, d)


# weak identity dictionaries

# A hash-table key wrapping a weak reference, keyed by the referent's
# identity. The objectid is cached so that the slot remains addressable after
# the referent has been collected (a dead key compares equal only to itself,
# so its entry simply lingers until the next sweep).
struct WeakIdKey
    w::WeakRef
    h::UInt
    WeakIdKey(@nospecialize(k)) = new(WeakRef(k), objectid(k))
end
hash(k::WeakIdKey, h::UInt) = hash(k.h, h)
function isequal(a::WeakIdKey, b::WeakIdKey)
    a === b && return true
    a.h == b.h || return false
    v = a.w.value
    return v !== nothing && v === b.w.value
end
==(a::WeakIdKey, b::WeakIdKey) = isequal(a, b)

mutable struct WeakIdDictFinalizer{T}
    const d::T
end
(d::WeakIdDictFinalizer)(v) = d.d.dirty = true

"""
    WeakIdDict()

`WeakIdDict()` constructs a hash table keyed by object identity that is weak
in both directions: neither the keys nor the values are kept alive by the
table, and an entry is treated as absent (and eventually removed) as soon as
either its key or its value has been garbage collected.

This is useful for canonicalization caches, which memoize a minted companion
object for as long as anybody can still observe its identity, without pinning
either side of the association. Values must be mutable objects, since value
finalizers are used to mark dead entries for cleanup; keys may be arbitrary
heap-allocated objects (entries whose key dies are swept on the next
mutation or value collection).

See also [`WeakKeyDict`](@ref) and [`WeakRef`](@ref).
"""
mutable struct WeakIdDict{K,V} <: AbstractDict{K,V}
    const ht::Dict{WeakIdKey,WeakRef}
    const lock::ReentrantLock
    dirty::Bool
    sweepsz::Int # table size that triggers the next dead-key sweep
    finalizer::WeakIdDictFinalizer

    function WeakIdDict{K,V}() where {K, V}
        t = new{K,V}(Dict{WeakIdKey,WeakRef}(), ReentrantLock(), false, 16)
        t.finalizer = WeakIdDictFinalizer(t)
        return t
    end
end
function WeakIdDict{K,V}(kv) where V where K
    h = WeakIdDict{K,V}()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
WeakIdDict{K,V}(p::Pair) where V where K = setindex!(WeakIdDict{K,V}(), p.second, p.first)
function WeakIdDict{K,V}(ps::Pair...) where V where K
    h = WeakIdDict{K,V}()
    for p in ps
        h[p.first] = p.second
    end
    return h
end
WeakIdDict() = WeakIdDict{Any,Any}()

WeakIdDict(kv::Tuple{}) = WeakIdDict()
copy(d::WeakIdDict) = WeakIdDict(d)

WeakIdDict(ps::Pair{K,V}...)           where {K,V} = WeakIdDict{K,V}(ps...)
WeakIdDict(ps::Pair{K}...)             where {K}   = WeakIdDict{K,Any}(ps...)
WeakIdDict(ps::(Pair{K,V} where K)...) where {V}   = WeakIdDict{Any,V}(ps...)
WeakIdDict(ps::Pair...)                            = WeakIdDict{Any,Any}(ps...)

WeakIdDict(kv) = dict_with_eltype((K, V) -> WeakIdDict{K, V}, kv, eltype(kv))

# an entry is live iff both its key and its value are; a dead entry is
# indistinguishable from an absent one
function _entry_live(k::WeakIdKey, wr::WeakRef)
    return k.w.value !== nothing && wr.value !== nothing
end

function _sweep_locked(h::WeakIdDict)
    h.dirty = false
    dead = WeakIdKey[]
    for (k, wr) in h.ht
        _entry_live(k, wr::WeakRef) || push!(dead, k)
    end
    for k in dead
        delete!(h.ht, k)
    end
    return h
end
_cleanup_locked(h::WeakIdDict) = h.dirty ? _sweep_locked(h) : h

empty(d::WeakIdDict, ::Type{K}, ::Type{V}) where {K, V} = WeakIdDict{K, V}()

IteratorSize(::Type{<:WeakIdDict}) = SizeUnknown()

islocked(h::WeakIdDict) = islocked(h.lock)
lock(h::WeakIdDict) = lock(h.lock)
unlock(h::WeakIdDict) = unlock(h.lock)
lock(f, h::WeakIdDict) = lock(f, h.lock)
trylock(f, h::WeakIdDict) = trylock(f, h.lock)

function setindex!(h::WeakIdDict{K,V}, v, key) where {K,V}
    !isa(key, K) && throw(ArgumentError("$(limitrepr(key)) is not a valid key for type $K"))
    # 'nothing' is not valid on either side: 'finalizer' would reject it as a
    # value, and it is the dead-reference sentinel for both keys and values
    key === nothing && throw(ArgumentError("`nothing` is not a valid WeakIdDict key"))
    v === nothing && throw(ArgumentError("`nothing` is not a valid WeakIdDict value"))
    lock(h) do
        _cleanup_locked(h)
        finalizer(h.finalizer, v)
        h.ht[WeakIdKey(key)] = WeakRef(v)
        # keys need not be finalizable, so no collection notice is guaranteed
        # for them; amortize a full sweep against table growth to also drop
        # entries whose key died
        if length(h.ht) >= h.sweepsz
            _sweep_locked(h)
            h.sweepsz = max(16, 2 * length(h.ht))
        end
    end
    return h
end

# retrieve the live value for `key`, or `nothing`
function _getvalue(h::WeakIdDict, key)
    wr = get(h.ht, WeakIdKey(key), nothing)
    wr === nothing && return nothing
    return (wr::WeakRef).value
end

function get(h::WeakIdDict{K,V}, key, default) where {K,V}
    key === nothing && throw(KeyError(nothing))
    lock(h) do
        v = _getvalue(h, key)
        return v === nothing ? default : v::V
    end
end
function get(default::Callable, h::WeakIdDict{K,V}, key) where {K,V}
    key === nothing && throw(KeyError(nothing))
    lock(h) do
        v = _getvalue(h, key)
        return v === nothing ? default() : v::V
    end
end
function get!(h::WeakIdDict{K,V}, key, default) where {K,V}
    lock(h) do
        v = _getvalue(h, key)
        v === nothing || return v::V
        h[key] = default
        return default::V
    end
end
function get!(default::Callable, h::WeakIdDict{K,V}, key) where {K,V}
    lock(h) do
        v = _getvalue(h, key)
        v === nothing || return v::V
        v = default()
        h[key] = v
        return v::V
    end
end

function getkey(h::WeakIdDict{K}, kk, default) where {K}
    kk === nothing && return default
    lock(h) do
        v = _getvalue(h, kk)
        return v === nothing ? default : kk::K
    end
end

function getindex(h::WeakIdDict{K,V}, key) where {K,V}
    key === nothing && throw(KeyError(key))
    lock(h) do
        v = _getvalue(h, key)
        v === nothing && throw(KeyError(key))
        return v::V
    end
end

function haskey(h::WeakIdDict, key)
    key === nothing && return false
    lock(h) do
        return _getvalue(h, key) !== nothing
    end
end

function delete!(h::WeakIdDict, key)
    key === nothing && return h
    lock(h) do
        delete!(h.ht, WeakIdKey(key))
    end
    return h
end

function pop!(h::WeakIdDict{K,V}, key) where {K,V}
    key === nothing && throw(KeyError(key))
    lock(h) do
        v = _getvalue(h, key)
        v === nothing && throw(KeyError(key))
        delete!(h.ht, WeakIdKey(key))
        return v::V
    end
end
function pop!(h::WeakIdDict{K,V}, key, default) where {K,V}
    key === nothing && return default
    lock(h) do
        v = _getvalue(h, key)
        delete!(h.ht, WeakIdKey(key))
        return v === nothing ? default : v::V
    end
end

function empty!(h::WeakIdDict)
    lock(h) do
        empty!(h.ht)
        h.dirty = false
    end
    return h
end

function length(h::WeakIdDict)
    lock(h) do
        n = 0
        for (k, wr) in h.ht
            n += _entry_live(k, wr::WeakRef)
        end
        return n
    end
end
isempty(h::WeakIdDict) = length(h) == 0

function iterate(h::WeakIdDict{K,V}, snapshot::Union{Nothing,Tuple{Vector{Pair{K,V}},Int}}=nothing) where {K,V}
    # iterate over a snapshot of the live entries, so that concurrent
    # collection of keys or values (or mutation under the lock) cannot corrupt
    # the walk; the snapshot's strong references keep its entries alive
    if snapshot === nothing
        live = Pair{K,V}[]
        lock(h) do
            for (wk, wr) in h.ht
                k = wk.w.value
                v = (wr::WeakRef).value
                (k === nothing || v === nothing) && continue
                push!(live, Pair{K,V}(k::K, v::V))
            end
        end
        snapshot = (live, 0)
    end
    pairs, i = snapshot
    i += 1
    i > length(pairs) && return nothing
    return pairs[i], (pairs, i)
end
