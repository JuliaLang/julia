# This file is a part of Julia. License is MIT: https://julialang.org/license

# weak key dictionaries

# Type to wrap a WeakRef to furbish it with object-id comparison and hashing.
struct WeakRefForWeakIdDict
    w::WeakRef
    WeakRefForWeakIdDict(wr::WeakRef) = new(wr)
end
WeakRefForWeakIdDict(val) = WeakRefForWeakIdDict(WeakRef(val))
==(wr1::WeakRefForWeakIdDict, wr2::WeakRefForWeakIdDict) = wr1.w.value===wr2.w.value
hash(wr::WeakRefForWeakIdDict, h::UInt) = hash_uint(3h - objectid(wr.w.value))

# Type to wrap a WeakRef to furbish it with == comparison and "normal" hashing of its value
struct WeakRefForWeakDict
    w::WeakRef
    WeakRefForWeakDict(wr::WeakRef) = new(wr)
end
WeakRefForWeakDict(val) = WeakRefForWeakDict(WeakRef(val))
==(wr1::WeakRefForWeakDict, wr2::WeakRefForWeakDict) = wr1.w.value==wr2.w.value
hash(wr::WeakRefForWeakDict, h::UInt) = hash(wr.w.value, h)


abstract type AbstractWeakKeyDict{K,V} <: AbstractDict{K,V} end
"""
    WeakKeyIdDict([itr])

`WeakKeyIdDict()` constructs a hash table where the keys are weak
references to objects, and thus may be garbage collected even when
referenced in a hash table.

The hashing and comparison are based on object-id and === of the key.

See [`Dict`](@ref) for further help.
"""
mutable struct WeakKeyIdDict{K,V} <: AbstractWeakKeyDict{K,V}
    ht::Dict{WeakRefForWeakIdDict,V}
    lock::Threads.RecursiveSpinLock
    finalizer::Function

    # Constructors mirror Dict's
    function WeakKeyIdDict{K,V}() where V where K
        t = new(Dict{WeakRefForWeakIdDict,V}(), Threads.RecursiveSpinLock(), identity)
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
keytype_ht(::Type{<:WeakKeyIdDict}) = WeakRefForWeakIdDict
striptp(::Type{<:WeakKeyIdDict}) = WeakKeyIdDict

"""
    WeakKeyDict([itr])

`WeakKeyDict()` constructs a hash table where the keys are weak
references to objects, and thus may be garbage collected even when
referenced in a hash table.

The hashing and comparison are based on the normal hash and == of the
key.

See [`Dict`](@ref) for further help.
"""
mutable struct WeakKeyDict{K,V} <: AbstractWeakKeyDict{K,V}
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
keytype_ht(::Type{<:WeakKeyDict}) = WeakRefForWeakDict
striptp(::Type{<:WeakKeyDict}) = WeakKeyDict

# Constructors as for Dict
function (::Type{W})(kv) where W<:AbstractWeakKeyDict{K,V} where {K, V}
    h = W()
    for (k,v) in kv
        h[k] = v
    end
    return h
end
(::Type{W})(p::Pair) where W<:AbstractWeakKeyDict{K,V} where {K, V} = setindex!(W(), p.second, p.first)
function (::Type{W})(ps::Pair...) where W<:AbstractWeakKeyDict{K,V} where {K, V}
    h = W()
    sizehint!(h, length(ps))
    for p in ps
        h[p.first] = p.second
    end
    return h
end
(::Type{W})() where W<:AbstractWeakKeyDict = W{Any,Any}()

(::Type{W})(kv::Tuple{}) where W<:AbstractWeakKeyDict = W()
copy(d::W) where W<:AbstractWeakKeyDict = W(d)

(::Type{W})(ps::Pair{K,V}...)           where W<:AbstractWeakKeyDict where {K,V} = W{K,V}(ps)
(::Type{W})(ps::Pair{K}...)             where W<:AbstractWeakKeyDict where {K}   = W{K,Any}(ps)
(::Type{W})(ps::(Pair{K,V} where K)...) where W<:AbstractWeakKeyDict where {V}   = W{Any,V}(ps)
(::Type{W})(ps::Pair...)                where W<:AbstractWeakKeyDict             = W{Any,Any}(ps)
(::Type{W})(ps::Pair)                   where W<:AbstractWeakKeyDict             = W{Any,Any}(ps)

function (::Type{W})(kv) where W<:AbstractWeakKeyDict
    try
        Base.dict_with_eltype((K, V) -> W{K, V}, kv, eltype(kv))
    catch e
        if !isiterable(typeof(kv)) || !all(x->isa(x,Union{Tuple,Pair}),kv)
            throw(ArgumentError("$W(kv): kv needs to be an iterator of tuples or pairs"))
        else
            rethrow(e)
        end
    end
end

empty(d::W, ::Type{K}, ::Type{V}) where W<:AbstractWeakKeyDict{K,V} where {K, V} = striptp(W){K, V}()

islocked(wkh::AbstractWeakKeyDict) = islocked(wkh.lock)
lock(f, wkh::AbstractWeakKeyDict) = lock(f, wkh.lock)
trylock(f, wkh::AbstractWeakKeyDict) = trylock(f, wkh.lock)

function setindex!(wkh::W, v, key) where W<:AbstractWeakKeyDict{K} where K
    !isa(key, K) && throw(ArgumentError("$key is not a valid key for type $K"))
    finalizer(wkh.finalizer, key)
    lock(wkh) do
        wkh.ht[keytype_ht(W)(key)] = v
    end
    return wkh
end

function getkey(wkh::W, kk, default) where W<:AbstractWeakKeyDict{K} where K
    return lock(wkh) do
        k = getkey(wkh.ht, keytype_ht(W)(kk), secret_table_token)
        k === secret_table_token && return default
        return k.w.value::K
    end
end

get(wkh::W, key, default) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> get(wkh.ht, keytype_ht(W)(key), default), wkh)
get(default::Callable, wkh::W, key) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> get(default, wkh.ht, keytype_ht(W)(key)), wkh)
get!(wkh::W, key, default) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> get!(wkh.ht, keytype_ht(W)(key), default), wkh)
get!(default::Callable, wkh::W, key) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> get!(default, wkh.ht, keytype_ht(W)(key)), wkh)
pop!(wkh::W, key) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> pop!(wkh.ht, keytype_ht(W)(key)), wkh)
pop!(wkh::W, key, default) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> pop!(wkh.ht, keytype_ht(W)(key), default), wkh)
delete!(wkh::W, key) where W<:AbstractWeakKeyDict =
    lock(() -> delete!(wkh.ht, keytype_ht(W)(key)), wkh)
empty!(wkh::AbstractWeakKeyDict) = (lock(() -> empty!(wkh.ht), wkh); wkh)
haskey(wkh::W, key) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> haskey(wkh.ht, keytype_ht(W)(key)), wkh)
getindex(wkh::W, key) where W<:AbstractWeakKeyDict{K} where {K} =
    lock(() -> getindex(wkh.ht, keytype_ht(W)(key)), wkh)
isempty(wkh::AbstractWeakKeyDict) = isempty(wkh.ht)
length(t::AbstractWeakKeyDict) = length(t.ht)

function iterate(t::AbstractWeakKeyDict)
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
function iterate(t::W, state) where W<:AbstractWeakKeyDict{K,V} where {K,V}
    gc_token = first(state)
    y = iterate(t.ht, tail(state)...)
    y === nothing && return nothing
    wkv, i = y
    kv = Pair{K,V}(wkv[1].w.value::K, wkv[2])
    return (kv, (gc_token, i))
end

filter!(f, d::AbstractWeakKeyDict) = filter_in_one_pass!(f, d)
