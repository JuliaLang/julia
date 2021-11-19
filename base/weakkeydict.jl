# This file is a part of Julia. License is MIT: https://julialang.org/license

# weak key dictionaries

"""
    WeakKeyDict([itr])

`WeakKeyDict()` constructs a hash table where the keys are weak
references to objects which may be garbage collected even when
referenced in a hash table.

See [`Dict`](@ref) for further help.  Note, unlike [`Dict`](@ref),
`WeakKeyDict` does not convert keys on insertion, as this would imply the key
object was unreferenced anywhere before insertion.
"""
mutable struct WeakKeyDict{K,V} <: AbstractDict{K,V}
    ht::Dict{WeakRef,V}
    lock::ReentrantLock
    finalizer::Function
    dirty::Bool

    # Constructors mirror Dict's
    function WeakKeyDict{K,V}() where V where K
        t = new(Dict{Any,V}(), ReentrantLock(), identity, 0)
        t.finalizer = k -> t.dirty = true
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

function WeakKeyDict(kv)
    try
        Base.dict_with_eltype((K, V) -> WeakKeyDict{K, V}, kv, eltype(kv))
    catch
        if !isiterable(typeof(kv)) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("WeakKeyDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow()
        end
    end
end

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

sizehint!(d::WeakKeyDict, newsz) = sizehint!(d.ht, newsz)
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
        k = getkey(wkh.ht, kk, nothing)
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
    return lock(t) do
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

filter!(f, d::WeakKeyDict) = filter_in_one_pass!(f, d)
