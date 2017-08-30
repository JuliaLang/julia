# This file is a part of Julia. License is MIT: https://julialang.org/license

# weak key dictionaries

"""
    WeakKeyDict([itr])

`WeakKeyDict()` constructs a hash table where the keys are weak
references to objects, and thus may be garbage collected even when
referenced in a hash table.

See [`Dict`](@ref) for further help.
"""
mutable struct WeakKeyDict{K,V} <: Associative{K,V}
    ht::Dict{WeakRef,V}
    lock::Threads.RecursiveSpinLock
    finalizer::Function

    # Constructors mirror Dict's
    function WeakKeyDict{K,V}() where V where K
        t = new(Dict{Any,V}(), Threads.RecursiveSpinLock(), identity)
        t.finalizer = function (k)
            # when a weak key is finalized, remove from dictionary if it is still there
            islocked(t) && return finalizer(k, t.finalizer)
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
        Base.associative_with_eltype((K, V) -> WeakKeyDict{K, V}, kv, eltype(kv))
    catch e
        if !applicable(start, kv) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("WeakKeyDict(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

similar(d::WeakKeyDict{K,V}) where {K,V} = WeakKeyDict{K,V}()
similar(d::WeakKeyDict, ::Type{Pair{K,V}}) where {K,V} = WeakKeyDict{K,V}()

islocked(wkh::WeakKeyDict) = islocked(wkh.lock)
lock(f, wkh::WeakKeyDict) = lock(f, wkh.lock)
trylock(f, wkh::WeakKeyDict) = trylock(f, wkh.lock)

function setindex!(wkh::WeakKeyDict{K}, v, key) where K
    k = convert(K, key)
    finalizer(k, wkh.finalizer)
    lock(wkh) do
        wkh.ht[WeakRef(k)] = v
    end
    return wkh
end

function getkey(wkh::WeakKeyDict{K}, kk, default) where K
    return lock(wkh) do
        k = getkey(wkh.ht, kk, secret_table_token)
        k === secret_table_token && return default
        return k.value::K
    end
end

get(wkh::WeakKeyDict{K}, key, default) where {K} = lock(() -> get(wkh.ht, key, default), wkh)
get(default::Callable, wkh::WeakKeyDict{K}, key) where {K} = lock(() -> get(default, wkh.ht, key), wkh)
get!(wkh::WeakKeyDict{K}, key, default) where {K} = lock(() -> get!(wkh.ht, key, default), wkh)
get!(default::Callable, wkh::WeakKeyDict{K}, key) where {K} = lock(() -> get!(default, wkh.ht, key), wkh)
pop!(wkh::WeakKeyDict{K}, key) where {K} = lock(() -> pop!(wkh.ht, key), wkh)
pop!(wkh::WeakKeyDict{K}, key, default) where {K} = lock(() -> pop!(wkh.ht, key, default), wkh)
delete!(wkh::WeakKeyDict, key) = lock(() -> delete!(wkh.ht, key), wkh)
empty!(wkh::WeakKeyDict) = (lock(() -> empty!(wkh.ht), wkh); wkh)
haskey(wkh::WeakKeyDict{K}, key) where {K} = lock(() -> haskey(wkh.ht, key), wkh)
getindex(wkh::WeakKeyDict{K}, key) where {K} = lock(() -> getindex(wkh.ht, key), wkh)
isempty(wkh::WeakKeyDict) = isempty(wkh.ht)
length(t::WeakKeyDict) = length(t.ht)

function start(t::WeakKeyDict{K,V}) where V where K
    gc_token = Ref{Bool}(false) # no keys will be deleted via finalizers until this token is gc'd
    finalizer(gc_token, function(r)
        if r[]
            r[] = false
            unlock(t.lock)
        end
    end)
    s = lock(t.lock)
    gc_token[] = true
    return (start(t.ht), gc_token)
end
done(t::WeakKeyDict, i) = done(t.ht, i[1])
function next(t::WeakKeyDict{K,V}, i)  where V where K
    gc_token = i[2]
    wkv, i = next(t.ht, i[1])
    kv = Pair{K,V}(wkv[1].value::K, wkv[2])
    return (kv, (i, gc_token))
end

filter!(f, d::WeakKeyDict) = filter_in_one_pass!(f, d)
