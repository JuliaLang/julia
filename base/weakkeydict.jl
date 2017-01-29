# weak key dictionaries

"""
    WeakKeyDict([itr])

`WeakKeyDict()` constructs a hash table where the keys are weak
references to objects, and thus may be garbage collected even when
referenced in a hash table.

See [`Dict`](@ref) for further help.
"""
type WeakKeyDict{K,V} <: Associative{K,V}
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

WeakKeyDict{K,V}(ps::Pair{K,V}...)            = WeakKeyDict{K,V}(ps)
WeakKeyDict{K  }(ps::Pair{K}...,)             = WeakKeyDict{K,Any}(ps)
WeakKeyDict{V  }(ps::(Pair{K,V} where K)...,) = WeakKeyDict{Any,V}(ps)
WeakKeyDict(     ps::Pair...)                 = WeakKeyDict{Any,Any}(ps)

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

similar{K,V}(d::WeakKeyDict{K,V}) = WeakKeyDict{K,V}()
similar{K,V}(d::WeakKeyDict, ::Type{Pair{K,V}}) = WeakKeyDict{K,V}()

# conversion between Dict types
function convert{K,V}(::Type{WeakKeyDict{K,V}},d::Associative)
    h = WeakKeyDict{K,V}()
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
convert{K,V}(::Type{WeakKeyDict{K,V}},d::WeakKeyDict{K,V}) = d

islocked(wkh::WeakKeyDict) = islocked(wkh.lock)
lock(f, wkh::WeakKeyDict) = lock(f, wkh.lock)
trylock(f, wkh::WeakKeyDict) = trylock(f, wkh.lock)

function setindex!{K}(wkh::WeakKeyDict{K}, v, key)
    k = convert(K, key)
    finalizer(k, wkh.finalizer)
    lock(wkh) do
        wkh.ht[WeakRef(k)] = v
    end
    return wkh
end

function getkey{K}(wkh::WeakKeyDict{K}, kk, default)
    return lock(wkh) do
        k = getkey(wkh.ht, kk, secret_table_token)
        k === secret_table_token && return default
        return k.value::K
    end
end

get{K}(wkh::WeakKeyDict{K}, key, default) = lock(() -> get(wkh.ht, key, default), wkh)
get{K}(default::Callable, wkh::WeakKeyDict{K}, key) = lock(() -> get(default, wkh.ht, key), wkh)
get!{K}(wkh::WeakKeyDict{K}, key, default) = lock(() -> get!(wkh.ht, key, default), wkh)
get!{K}(default::Callable, wkh::WeakKeyDict{K}, key) = lock(() -> get!(default, wkh.ht, key), wkh)
pop!{K}(wkh::WeakKeyDict{K}, key) = lock(() -> pop!(wkh.ht, key), wkh)
pop!{K}(wkh::WeakKeyDict{K}, key, default) = lock(() -> pop!(wkh.ht, key, default), wkh)
delete!{K}(wkh::WeakKeyDict{K}, key) = lock(() -> delete!(wkh.ht, key), wkh)
empty!(wkh::WeakKeyDict) = (lock(() -> empty!(wkh.ht), wkh); wkh)
haskey{K}(wkh::WeakKeyDict{K}, key) = lock(() -> haskey(wkh.ht, key), wkh)
getindex{K}(wkh::WeakKeyDict{K}, key) = lock(() -> getindex(wkh.ht, key), wkh)
isempty(wkh::WeakKeyDict) = isempty(wkh.ht)
length(t::WeakKeyDict) = length(t.ht)

function start{K,V}(t::WeakKeyDict{K,V})
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
function next{K,V}(t::WeakKeyDict{K,V}, i)
    gc_token = i[2]
    wkv, i = next(t.ht, i[1])
    kv = Pair{K,V}(wkv[1].value::K, wkv[2])
    return (kv, (i, gc_token))
end

function filter!(f, d::WeakKeyDict)
    for (k, v) in d
        if !f(k, v)
            delete!(d, k)
        end
    end
    return d
end
