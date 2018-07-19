# This file is a part of Julia. License is MIT: https://julialang.org/license

# weak key dictionaries

# Type to wrap a WeakRef to furbish it with objectid comparison and hashing.
struct WeakRefForWeakDict
    w::WeakRef
    WeakRefForWeakDict(wr::WeakRef) = new(wr)
end
WeakRefForWeakDict(val) = WeakRefForWeakDict(WeakRef(val))
==(wr1::WeakRefForWeakDict, wr2::WeakRefForWeakDict) = wr1.w.value===wr2.w.value
hash(wr::WeakRefForWeakDict, h::UInt) = hash_uint(3h - objectid(wr.w.value))

"""
    WeakKeyDict([itr])

`WeakKeyDict()` constructs a hash table where the keys are weak
references to objects, and thus may be garbage collected even when
referenced in a hash table.

See [`Dict`](@ref) for further help.
"""
mutable struct WeakKeyDict{K,V} <: AbstractDict{K,V}
    ht::Dict{WeakRefForWeakDict,V}
    lock::Threads.RecursiveSpinLock
    finalizer::Function

    # Constructors mirror Dict's
    function WeakKeyDict{K,V}() where V where K
        t = new(Dict{WeakRefForWeakDict,V}(), Threads.RecursiveSpinLock(), identity)
        t.finalizer = function (k)
            # when a weak key is finalized, remove from dictionary if it is still there
            if islocked(t)
                finalizer(t.finalizer, k)
                return nothing
            end
            delete!(t, k)
        end
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
    catch e
        if !isiterable(typeof(kv)) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("WeakKeyDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

empty(d::WeakKeyDict, ::Type{K}, ::Type{V}) where {K, V} = WeakKeyDict{K, V}()

islocked(wkh::WeakKeyDict) = islocked(wkh.lock)
lock(f, wkh::WeakKeyDict) = lock(f, wkh.lock)
trylock(f, wkh::WeakKeyDict) = trylock(f, wkh.lock)

function setindex!(wkh::WeakKeyDict{K}, v, key) where K
    !isa(key, K) && throw(ArgumentError("$key is not a valid key for type $K"))
    finalizer(wkh.finalizer, key)
    lock(wkh) do
        wkh.ht[WeakRefForWeakDict(key)] = v
    end
    return wkh
end

function getkey(wkh::WeakKeyDict{K}, kk, default) where K
    return lock(wkh) do
        k = getkey(wkh.ht, WeakRefForWeakDict(kk), secret_table_token)
        k === secret_table_token && return default
        return k.w.value::K
    end
end

get(wkh::WeakKeyDict{K}, key, default) where {K} = lock(() -> get(wkh.ht, WeakRefForWeakDict(key), default), wkh)
get(default::Callable, wkh::WeakKeyDict{K}, key) where {K} = lock(() -> get(default, wkh.ht, WeakRefForWeakDict(key)), wkh)
get!(wkh::WeakKeyDict{K}, key, default) where {K} = lock(() -> get!(wkh.ht, WeakRefForWeakDict(key), default), wkh)
get!(default::Callable, wkh::WeakKeyDict{K}, key) where {K} = lock(() -> get!(default, wkh.ht, WeakRefForWeakDict(key)), wkh)
pop!(wkh::WeakKeyDict{K}, key) where {K} = lock(() -> pop!(wkh.ht, WeakRefForWeakDict(key)), wkh)
pop!(wkh::WeakKeyDict{K}, key, default) where {K} = lock(() -> pop!(wkh.ht, WeakRefForWeakDict(key), default), wkh)
delete!(wkh::WeakKeyDict, key) = lock(() -> delete!(wkh.ht, WeakRefForWeakDict(key)), wkh)
empty!(wkh::WeakKeyDict) = (lock(() -> empty!(wkh.ht), wkh); wkh)
haskey(wkh::WeakKeyDict{K}, key) where {K} = lock(() -> haskey(wkh.ht, WeakRefForWeakDict(key)), wkh)
getindex(wkh::WeakKeyDict{K}, key) where {K} = lock(() -> getindex(wkh.ht, WeakRefForWeakDict(key)), wkh)
isempty(wkh::WeakKeyDict) = isempty(wkh.ht)
length(t::WeakKeyDict) = length(t.ht)

function iterate(t::WeakKeyDict{K,V}) where V where K
    gc_token = Ref{Bool}(false) # no keys will be deleted via finalizers until this token is gc'd
    finalizer(gc_token) do r
        if r[]
            r[] = false
            unlock(t.lock)
        end
    end
    s = lock(t.lock)
    iterate(t, (gc_token,))
end
function iterate(t::WeakKeyDict{K,V}, state) where V where K
    gc_token = first(state)
    y = iterate(t.ht, tail(state)...)
    y === nothing && return nothing
    wkv, i = y
    kv = Pair{K,V}(wkv[1].w.value::K, wkv[2])
    return (kv, (gc_token, i))
end

filter!(f, d::WeakKeyDict) = filter_in_one_pass!(f, d)
