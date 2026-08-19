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

mutable struct WeakValueIdDictFinalizer{T}
    const d::T
end
(d::WeakValueIdDictFinalizer)(v) = d.d.dirty = true

"""
    WeakValueIdDict()

`WeakValueIdDict()` constructs a hash table keyed by object identity whose
values are only weakly referenced: an entry does not keep its value alive, and
is treated as absent (and eventually removed) once the value has been garbage
collected. The keys are held normally (strongly) for as long as their entry is
present; a key is released together with its entry, on the first mutation
after its value's collection.

This is useful for canonicalization caches, which memoize a minted companion
object for as long as anybody can still observe its identity, and re-mint it
afterwards. Note that weakness on the key side would be meaningless for such
caches when key egality is structural (e.g. immutable keys): the collection
of one particular key allocation is not the disappearance of the key, since
an egal key can exist elsewhere or be constructed at any time. Holding the
stored key strongly is what makes the canonical value stable across all egal
spellings of its key for as long as the value is observable.

Values must be mutable objects, since value finalizers are used to mark dead
entries for cleanup.

See also [`WeakKeyDict`](@ref), [`IdDict`](@ref) and [`WeakRef`](@ref).
"""
mutable struct WeakValueIdDict{K,V} <: AbstractDict{K,V}
    const ht::IdDict{K,WeakRef}
    const lock::ReentrantLock
    dirty::Bool
    # concretely typed (self-referentially) so that registering the finalizer
    # remains statically resolvable, e.g. under `--trim`
    finalizer::WeakValueIdDictFinalizer{WeakValueIdDict{K,V}}

    function WeakValueIdDict{K,V}() where {K, V}
        t = new{K,V}(IdDict{K,WeakRef}(), ReentrantLock(), false)
        t.finalizer = WeakValueIdDictFinalizer(t)
        return t
    end
end
function WeakValueIdDict{K,V}(kv) where V where K
    h = WeakValueIdDict{K,V}()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
WeakValueIdDict{K,V}(p::Pair) where V where K = setindex!(WeakValueIdDict{K,V}(), p.second, p.first)
function WeakValueIdDict{K,V}(ps::Pair...) where V where K
    h = WeakValueIdDict{K,V}()
    for p in ps
        h[p.first] = p.second
    end
    return h
end
WeakValueIdDict() = WeakValueIdDict{Any,Any}()

WeakValueIdDict(kv::Tuple{}) = WeakValueIdDict()
copy(d::WeakValueIdDict) = WeakValueIdDict(d)

WeakValueIdDict(ps::Pair{K,V}...)           where {K,V} = WeakValueIdDict{K,V}(ps...)
WeakValueIdDict(ps::Pair{K}...)             where {K}   = WeakValueIdDict{K,Any}(ps...)
WeakValueIdDict(ps::(Pair{K,V} where K)...) where {V}   = WeakValueIdDict{Any,V}(ps...)
WeakValueIdDict(ps::Pair...)                            = WeakValueIdDict{Any,Any}(ps...)

WeakValueIdDict(kv) = dict_with_eltype((K, V) -> WeakValueIdDict{K, V}, kv, eltype(kv))

# an entry is live iff its value is; a dead entry is indistinguishable from an
# absent one (its key stays pinned until the next sweep)
function _sweep_locked(h::WeakValueIdDict{K}) where {K}
    h.dirty = false
    dead = K[]
    for (k, wr) in h.ht
        (wr::WeakRef).value === nothing && push!(dead, k)
    end
    for k in dead
        delete!(h.ht, k)
    end
    return h
end
_cleanup_locked(h::WeakValueIdDict) = h.dirty ? _sweep_locked(h) : h

empty(d::WeakValueIdDict, ::Type{K}, ::Type{V}) where {K, V} = WeakValueIdDict{K, V}()

IteratorSize(::Type{<:WeakValueIdDict}) = SizeUnknown()

islocked(h::WeakValueIdDict) = islocked(h.lock)
lock(h::WeakValueIdDict) = lock(h.lock)
unlock(h::WeakValueIdDict) = unlock(h.lock)
lock(f, h::WeakValueIdDict) = lock(f, h.lock)
trylock(f, h::WeakValueIdDict) = trylock(f, h.lock)

function setindex!(h::WeakValueIdDict{K,V}, v, key) where {K,V}
    !isa(key, K) && throw(ArgumentError("$(limitrepr(key)) is not a valid key for type $K"))
    # 'nothing' is not valid on either side: 'finalizer' would reject it as a
    # value (and it is the dead-reference sentinel), and a 'nothing' key is
    # reserved so that lookup errors stay consistent with WeakKeyDict
    key === nothing && throw(ArgumentError("`nothing` is not a valid WeakValueIdDict key"))
    v === nothing && throw(ArgumentError("`nothing` is not a valid WeakValueIdDict value"))
    lock(h) do
        _cleanup_locked(h)
        finalizer(h.finalizer, v)
        h.ht[key] = WeakRef(v)
    end
    return h
end

# retrieve the live value for `key`, or `nothing`
function _getvalue(h::WeakValueIdDict, key)
    wr = get(h.ht, key, nothing)
    wr === nothing && return nothing
    return (wr::WeakRef).value
end

function get(h::WeakValueIdDict{K,V}, key, default) where {K,V}
    key === nothing && throw(KeyError(nothing))
    lock(h) do
        v = _getvalue(h, key)
        return v === nothing ? default : v::V
    end
end
function get(default::Callable, h::WeakValueIdDict{K,V}, key) where {K,V}
    key === nothing && throw(KeyError(nothing))
    lock(h) do
        v = _getvalue(h, key)
        return v === nothing ? default() : v::V
    end
end
function get!(h::WeakValueIdDict{K,V}, key, default) where {K,V}
    lock(h) do
        v = _getvalue(h, key)
        v === nothing || return v::V
        h[key] = default
        return default::V
    end
end
function get!(default::Callable, h::WeakValueIdDict{K,V}, key) where {K,V}
    lock(h) do
        v = _getvalue(h, key)
        v === nothing || return v::V
        v = default()
        h[key] = v
        return v::V
    end
end

function getkey(h::WeakValueIdDict{K}, kk, default) where {K}
    kk === nothing && return default
    lock(h) do
        v = _getvalue(h, kk)
        return v === nothing ? default : kk::K
    end
end

function getindex(h::WeakValueIdDict{K,V}, key) where {K,V}
    key === nothing && throw(KeyError(key))
    lock(h) do
        v = _getvalue(h, key)
        v === nothing && throw(KeyError(key))
        return v::V
    end
end

function haskey(h::WeakValueIdDict, key)
    key === nothing && return false
    lock(h) do
        return _getvalue(h, key) !== nothing
    end
end

function delete!(h::WeakValueIdDict, key)
    key === nothing && return h
    lock(h) do
        delete!(h.ht, key)
    end
    return h
end

function pop!(h::WeakValueIdDict{K,V}, key) where {K,V}
    key === nothing && throw(KeyError(key))
    lock(h) do
        v = _getvalue(h, key)
        v === nothing && throw(KeyError(key))
        delete!(h.ht, key)
        return v::V
    end
end
function pop!(h::WeakValueIdDict{K,V}, key, default) where {K,V}
    key === nothing && return default
    lock(h) do
        v = _getvalue(h, key)
        delete!(h.ht, key)
        return v === nothing ? default : v::V
    end
end

function empty!(h::WeakValueIdDict)
    lock(h) do
        empty!(h.ht)
        h.dirty = false
    end
    return h
end

function length(h::WeakValueIdDict)
    lock(h) do
        n = 0
        for (k, wr) in h.ht
            n += ((wr::WeakRef).value !== nothing)
        end
        return n
    end
end
isempty(h::WeakValueIdDict) = length(h) == 0

function iterate(h::WeakValueIdDict{K,V}, snapshot::Union{Nothing,Tuple{Vector{Pair{K,V}},Int}}=nothing) where {K,V}
    # iterate over a snapshot of the live entries, so that concurrent
    # collection of values (or mutation under the lock) cannot corrupt the
    # walk; the snapshot's strong references keep its entries alive
    if snapshot === nothing
        live = Pair{K,V}[]
        lock(h) do
            for (k, wr) in h.ht
                v = (wr::WeakRef).value
                v === nothing && continue
                push!(live, Pair{K,V}(k, v::V))
            end
        end
        snapshot = (live, 0)
    end
    pairs, i = snapshot
    i += 1
    i > length(pairs) && return nothing
    return pairs[i], (pairs, i)
end
