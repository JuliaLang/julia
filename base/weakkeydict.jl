# weak key dictionaries

type WeakKeyDict{K,V} <: Associative{K,V}
    ht::Dict{Any,V}
    deleter::Function

    WeakKeyDict() = new(Dict{Any,V}(), identity)
end
WeakKeyDict() = WeakKeyDict{Any,Any}()

function weak_key_delete!(t::Dict, k)
    # when a weak key is finalized, remove from dictionary if it is still there
    wk = getkey(t, k, secret_table_token)
    if !is(wk,secret_table_token) && is(wk.value, k)
        delete!(t, k)
    end
end

function setindex!{K}(wkh::WeakKeyDict{K}, v, key)
    t = wkh.ht
    k = convert(K, key)
    if is(wkh.deleter, identity)
        wkh.deleter = x->weak_key_delete!(t, x)
    end
    t[WeakRef(k)] = v
    # TODO: it might be better to avoid the finalizer, allow
    # wiped WeakRefs to remain in the table, and delete them as
    # they are discovered by getindex and setindex!.
    finalizer(k, wkh.deleter)
    return t
end


function getkey{K}(wkh::WeakKeyDict{K}, kk, default)
    k = getkey(wkh.ht, kk, secret_table_token)
    if is(k, secret_table_token)
        return default
    end
    return k.value::K
end

get{K}(wkh::WeakKeyDict{K}, key, default) = get(wkh.ht, key, default)
get{K}(default::Callable, wkh::WeakKeyDict{K}, key) = get(default, wkh.ht, key)
get!{K}(wkh::WeakKeyDict{K}, key, default) = get!(wkh.ht, key, default)
get!{K}(default::Callable, wkh::WeakKeyDict{K}, key) = get!(default, wkh.ht, key)
pop!{K}(wkh::WeakKeyDict{K}, key) = pop!(wkh.ht, key)
pop!{K}(wkh::WeakKeyDict{K}, key, default) = pop!(wkh.ht, key, default)
delete!{K}(wkh::WeakKeyDict{K}, key) = delete!(wkh.ht, key)
empty!(wkh::WeakKeyDict)  = (empty!(wkh.ht); wkh)
haskey{K}(wkh::WeakKeyDict{K}, key) = haskey(wkh.ht, key)
getindex{K}(wkh::WeakKeyDict{K}, key) = getindex(wkh.ht, key)
isempty(wkh::WeakKeyDict) = isempty(wkh.ht)

start(t::WeakKeyDict) = start(t.ht)
done(t::WeakKeyDict, i) = done(t.ht, i)
function next{K,V}(t::WeakKeyDict{K,V}, i)
    kv, i = next(t.ht, i)
    (Pair{K,V}(kv[1].value::K,kv[2]), i)
end
length(t::WeakKeyDict) = length(t.ht)

function filter!(f, d::WeakKeyDict)
    for (k, v) in d
        if !f(k, v)
            delete!(d, k)
        end
    end
    return d
end
