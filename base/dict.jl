# This file is a part of Julia. License is MIT: http://julialang.org/license

# generic operations on associative collections

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::Associative, k) = in(k,keys(d))

function in(p::Pair, a::Associative, valcmp=(==))
    v = get(a,p[1],secret_table_token)
    if !is(v, secret_table_token)
        valcmp(v, p[2]) && return true
    end
    return false
end

function in(p, a::Associative)
    error("""Associative collections only contain Pairs;
             Either look for e.g. A=>B instead, or use the `keys` or `values`
             function if you are looking for a key or value respectively.""")
end

function summary(t::Associative)
    n = length(t)
    return string(typeof(t), " with ", n, (n==1 ? " entry" : " entries"))
end

function _truncate_at_width_or_chars(str, width, chars="", truncmark="…")
    truncwidth = strwidth(truncmark)
    (width <= 0 || width < truncwidth) && return ""

    wid = truncidx = lastidx = 0
    idx = start(str)
    while !done(str, idx)
        lastidx = idx
        c, idx = next(str, idx)
        wid += charwidth(c)
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

function show{K,V}(io::IO, t::Associative{K,V})
    recur_io = IOContext(io, :SHOWN_SET => t)
    limit::Bool = get(io, :limit, false)
    if !haskey(io, :compact)
        recur_io = IOContext(recur_io, :compact => true)
    end

    # show in a Julia-syntax-like form: Dict(k=>v, ...)
    if isempty(t)
        print(io, typeof(t), "()")
    else
        if isleaftype(K) && isleaftype(V)
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

immutable KeyIterator{T<:Associative}
    dict::T
end
immutable ValueIterator{T<:Associative}
    dict::T
end

summary{T<:Union{KeyIterator,ValueIterator}}(iter::T) =
    string(T.name, " for a ", summary(iter.dict))

show(io::IO, iter::Union{KeyIterator,ValueIterator}) = show(io, collect(iter))

length(v::Union{KeyIterator,ValueIterator}) = length(v.dict)
isempty(v::Union{KeyIterator,ValueIterator}) = isempty(v.dict)
_tt1{A,B}(::Type{Pair{A,B}}) = A
_tt2{A,B}(::Type{Pair{A,B}}) = B
eltype{D}(::Type{KeyIterator{D}}) = _tt1(eltype(D))
eltype{D}(::Type{ValueIterator{D}}) = _tt2(eltype(D))

start(v::Union{KeyIterator,ValueIterator}) = start(v.dict)
done(v::Union{KeyIterator,ValueIterator}, state) = done(v.dict, state)

function next(v::KeyIterator, state)
    n = next(v.dict, state)
    n[1][1], n[2]
end

function next(v::ValueIterator, state)
    n = next(v.dict, state)
    n[1][2], n[2]
end

in(k, v::KeyIterator) = !is(get(v.dict, k, secret_table_token),
                            secret_table_token)

keys(a::Associative) = KeyIterator(a)
eachindex(a::Associative) = KeyIterator(a)
values(a::Associative) = ValueIterator(a)

function copy(a::Associative)
    b = similar(a)
    for (k,v) in a
        b[k] = v
    end
    return b
end

function merge!(d::Associative, others::Associative...)
    for other in others
        for (k,v) in other
            d[k] = v
        end
    end
    return d
end

# very similar to `merge!`, but accepts any iterable and extends code
# that would otherwise only use `copy!` with arrays.
function copy!(dest::Union{Associative,AbstractSet}, src)
    for x in src
        push!(dest, x)
    end
    return dest
end

keytype{K,V}(::Type{Associative{K,V}}) = K
keytype(a::Associative) = keytype(typeof(a))
keytype{A<:Associative}(::Type{A}) = keytype(supertype(A))
valtype{K,V}(::Type{Associative{K,V}}) = V
valtype{A<:Associative}(::Type{A}) = valtype(supertype(A))
valtype(a::Associative) = valtype(typeof(a))
function merge(d::Associative, others::Associative...)
    K, V = keytype(d), valtype(d)
    for other in others
        K = promote_type(K, keytype(other))
        V = promote_type(V, valtype(other))
    end
    merge!(Dict{K,V}(), d, others...)
end

function filter!(f, d::Associative)
    badkeys = Array{keytype(d)}(0)
    for (k,v) in d
        # don't delete!(d, k) here, since associative types
        # may not support mutation during iteration
        f(k,v) || push!(badkeys, k)
    end
    for k in badkeys
        delete!(d, k)
    end
    return d
end
function filter(f, d::Associative)
    # don't just do filter!(f, copy(d)): avoid making a whole copy of d
    df = similar(d)
    for (k,v) in d
        if f(k,v)
            df[k] = v
        end
    end
    return df
end

eltype{K,V}(::Type{Associative{K,V}}) = Pair{K,V}

function isequal(l::Associative, r::Associative)
    l === r && return true
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for pair in l
        if !in(pair, r, isequal)
            return false
        end
    end
    true
end

function ==(l::Associative, r::Associative)
    l === r && return true
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for pair in l
        if !in(pair, r, ==)
            return false
        end
    end
    true
end

const hasha_seed = UInt === UInt64 ? 0x6d35bb51952d5539 : 0x952d5539
function hash(a::Associative, h::UInt)
    h = hash(hasha_seed, h)
    for (k,v) in a
        h $= hash(k, hash(v))
    end
    return h
end

# some support functions

_tablesz(x::Integer) = x < 16 ? 16 : one(x)<<((sizeof(x)<<3)-leading_zeros(x-1))

function getindex(t::Associative, key)
    v = get(t, key, secret_table_token)
    if is(v, secret_table_token)
        throw(KeyError(key))
    end
    return v
end

# t[k1,k2,ks...] is syntactic sugar for t[(k1,k2,ks...)].  (Note
# that we need to avoid dispatch loops if setindex!(t,v,k) is not defined.)
getindex(t::Associative, k1, k2, ks...) = getindex(t, tuple(k1,k2,ks...))
setindex!(t::Associative, v, k1, k2, ks...) = setindex!(t, v, tuple(k1,k2,ks...))

push!(t::Associative, p::Pair) = setindex!(t, p.second, p.first)
push!(t::Associative, p::Pair, q::Pair) = push!(push!(t, p), q)
push!(t::Associative, p::Pair, q::Pair, r::Pair...) = push!(push!(push!(t, p), q), r...)

# dict

# These can be changed, to trade off better performance for space
const global maxallowedprobe = 16
const global maxprobeshift   = 6

type CmpDict{K,V,Cmp,Hash} <: Associative{K,V}
    slots::Array{UInt8,1}
    keys::Array{K,1}
    vals::Array{V,1}
    ndel::Int
    count::Int
    dirty::Bool
    idxfloor::Int  # an index <= the indexes of all used slots
    maxprobe::Int
    isequal::Cmp
    hash::Hash

    function CmpDict(isequal::Cmp, hash::Hash)
        n = 16
        return new(zeros(UInt8,n), Array{K}(n), Array{V}(n), 0,
                   0, false, 1, 0, isequal, hash)
    end
    function CmpDict(isequal::Cmp, hash::Hash, kv)
        h = CmpDict{K,V,Cmp,Hash}(isequal, hash)
        for (k,v) in kv
            h[k] = v
        end
        return h
    end
    CmpDict(isequal::Cmp, hash::Hash, p::Pair) =
        setindex!(CmpDict{K,V,Cmp,Hash}(isequal, hash), p.second, p.first)
    function CmpDict(isequal::Cmp, hash::Hash, ps::Pair...)
        h = CmpDict{K,V,Cmp,Hash}(isequal, hash)
        sizehint!(h, length(ps))
        for p in ps
            h[p.first] = p.second
        end
        return h
    end
    function CmpDict(d::CmpDict{K,V,Cmp,Hash})
        if d.ndel > 0
            rehash!(d)
        end
        @assert d.ndel == 0
        return new(copy(d.slots), copy(d.keys), copy(d.vals), 0,
                   d.count, d.dirty, d.idxfloor, d.maxprobe, d.isequal, d.hash)
    end
end
copy{K,V,Cmp,Hash}(d::CmpDict{K,V,Cmp,Hash}) = CmpDict{K,V,Cmp,Hash}(d)

# define Dict #
let isequal = isequal, hash = hash
    global Dict, show
    typealias Dict{K,V} CmpDict{K,V,typeof(isequal),typeof(hash)}
    show(io::IO, d::Type{Dict}) = print(io, "Dict{", d.parameters[1], ",", d.parameters[2], "}")
    show{K,V}(io::IO, d::Type{Dict{K,V}}) = print(io, "Dict{", d.parameters[1], ",", d.parameters[2], "}")

    (::Type{Dict{K,V}}){K,V}() = Dict{K,V}(isequal, hash)
    (::Type{Dict{K,V}}){K,V}(kv) = Dict{K,V}(isequal, hash, kv)
    (::Type{Dict{K,V}}){K,V}(kv::Dict{K,V}) = Dict{K,V}(isequal, hash, kv)
    (::Type{Dict{K,V}}){K,V}(p::Pair) = Dict{K,V}(isequal, hash, p)
    (::Type{Dict{K,V}}){K,V}(ps::Pair...) = Dict{K,V}(isequal, hash, ps...)

    Dict() = Dict{Any,Any}()
    Dict(kv::Tuple{}) = Dict()

    global const AnyDict = Dict{Any,Any}

    Dict{K,V}(ps::Pair{K,V}...)            = Dict{K,V}(ps)
    Dict{K  }(ps::Pair{K}...,)             = Dict{K,Any}(ps)
    Dict{V  }(ps::Pair{TypeVar(:K),V}...,) = Dict{Any,V}(ps)
    Dict(     ps::Pair...)                 = Dict{Any,Any}(ps)

    function Dict(kv)
        try
            dict_with_eltype(kv, eltype(kv))
        catch e
            if false && any(x->isempty(methods(x, (typeof(kv),))), [start, next, done]) ||
                !all(x->isa(x,Union{Tuple,Pair}),kv)
                throw(ArgumentError("Dict(kv): kv needs to be an iterator of tuples or pairs"))
            else
                rethrow(e)
            end
        end
    end

    dict_with_eltype{K,V}(kv, ::Type{Tuple{K,V}}) = Dict{K,V}(kv)
    dict_with_eltype{K,V}(kv, ::Type{Pair{K,V}}) = Dict{K,V}(kv)
    dict_with_eltype(kv, t) = grow_to!(Dict{Union{},Union{}}(), kv)
end

# define IdDict #
let isequal = is, hash = object_id
    global IdDict, show
    typealias IdDict{K,V} CmpDict{K,V,typeof(isequal),typeof(hash)}
    show(io::IO, d::Type{IdDict}) = print(io, "IdDict{", d.parameters[1], ",", d.parameters[2], "}")
    show{K,V}(io::IO, d::Type{IdDict{K,V}}) = print(io, "IdDict{", d.parameters[1], ",", d.parameters[2], "}")

    (::Type{IdDict{K,V}}){K,V}() = IdDict{K,V}(isequal, hash)
    (::Type{IdDict{K,V}}){K,V}(kv) = IdDict{K,V}(isequal, hash, kv)
    (::Type{IdDict{K,V}}){K,V}(kv::Dict{K,V}) = IdDict{K,V}(isequal, hash, kv)
    (::Type{IdDict{K,V}}){K,V}(p::Pair) = IdDict{K,V}(isequal, hash, p)
    (::Type{IdDict{K,V}}){K,V}(ps::Pair...) = IdDict{K,V}(isequal, hash, ps...)

    IdDict() = IdDict{Any,Any}()
    IdDict(kv::Tuple{}) = IdDict()

    global const AnyIdDict = IdDict{Any,Any}

    IdDict{K,V}(ps::Pair{K,V}...)            = IdDict{K,V}(ps)
    IdDict{K  }(ps::Pair{K}...,)             = IdDict{K,Any}(ps)
    IdDict{V  }(ps::Pair{TypeVar(:K),V}...,) = IdDict{Any,V}(ps)
    IdDict(     ps::Pair...)                 = IdDict{Any,Any}(ps)

    function IdDict(kv)
        try
            iddict_with_eltype(kv, eltype(kv))
        catch e
            if any(x->isempty(methods(x, (typeof(kv),))), [start, next, done]) ||
                !all(x->isa(x,Union{Tuple,Pair}),kv)
                throw(ArgumentError("Dict(kv): kv needs to be an iterator of tuples or pairs"))
            else
                rethrow(e)
            end
        end
    end

    iddict_with_eltype{K,V}(kv, ::Type{Tuple{K,V}}) = IdDict{K,V}(kv)
    iddict_with_eltype{K,V}(kv, ::Type{Pair{K,V}}) = IdDict{K,V}(kv)
    iddict_with_eltype(kv, t) = grow_to!(IdDict{Union{},Union{}}(), kv)
end
# hashing objects by identity
const ObjectIdDict = IdDict{Any,Any}
abstract AbstractSerializer

# this is a special case due to (1) allowing both Pairs and Tuples as elements,
# and (2) Pair being invariant. a bit annoying.
function grow_to!{K,V}(dest::Associative{K,V}, itr, st = start(itr))
    while !done(itr, st)
        (k,v), st = next(itr, st)
        if isa(k,K) && isa(v,V)
            dest[k] = v
        else
            new = similar(dest, Pair{typejoin(K,typeof(k)), typejoin(V,typeof(v))})
            copy!(new, dest)
            new[k] = v
            return grow_to!(new, itr, st)
        end
    end
    return dest
end

similar(d::CmpDict) = typeof(d)(d.isequal, d.hash)
similar{K,V,KD,VD,Cmp,Hash}(d::CmpDict{KD,VD,Cmp,Hash}, ::Type{Pair{K,V}}) =
    Dict{K,V,Cmp,Hash}(d.isequal, d.hash)

# conversion between Dict types
function convert{K,V}(::Type{Dict{K,V}}, d::Associative)
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
convert{K,V}(::Type{Dict{K,V}}, d::Dict{K,V}) = d

hashindex(hash, sz) = ((hash % Int) & (sz - 1)) + 1

isslotempty(h::CmpDict, i::Int) = h.slots[i] == 0x0
isslotfilled(h::CmpDict, i::Int) = h.slots[i] == 0x1
isslotmissing(h::CmpDict, i::Int) = h.slots[i] == 0x2

function rehash!{K,V}(h::CmpDict{K,V}, newsz = length(h.keys))
    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    sz = length(olds)
    newsz = _tablesz(newsz)
    h.dirty = true
    h.idxfloor = 1
    if h.count == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        h.ndel = 0
        return h
    end

    slots = zeros(UInt8,newsz)
    keys = Array{K}(newsz)
    vals = Array{V}(newsz)
    count0 = h.count
    count = 0
    maxprobe = h.maxprobe

    for i = 1:sz
        if olds[i] == 0x1
            k = oldk[i]
            v = oldv[i]
            index0 = index = hashindex(h.hash(k), newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            probe = (index - index0) & (newsz-1)
            probe > maxprobe && (maxprobe = probe)
            slots[index] = 0x1
            keys[index] = k
            vals[index] = v
            count += 1

            if h.count != count0
                # if items are removed by finalizers, retry
                return rehash!(h, newsz)
            end
        end
    end

    h.slots = slots
    h.keys = keys
    h.vals = vals
    h.count = count
    h.ndel = 0
    h.maxprobe = maxprobe

    return h
end

function sizehint!(d::CmpDict, newsz)
    oldsz = length(d.slots)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash!() assumes everything fits. it was only designed
        # for growing.
        return d
    end
    # grow at least 25%
    newsz = max(newsz, (oldsz*5)>>2)
    rehash!(d, newsz)
end

function empty!{K,V}(h::CmpDict{K,V})
    fill!(h.slots, 0x0)
    sz = length(h.slots)
    empty!(h.keys)
    empty!(h.vals)
    resize!(h.keys, sz)
    resize!(h.vals, sz)
    h.ndel = 0
    h.count = 0
    h.dirty = true
    h.idxfloor = 1
    return h
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex{K,V}(h::CmpDict{K,V}, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = h.maxprobe
    index = hashindex(h.hash(key), sz)
    keys = h.keys

    while true
        if isslotempty(h,index)
            break
        end
        if !isslotmissing(h, index) && h.isequal(key, keys[index])
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
function ht_keyindex2{K,V}(h::CmpDict{K,V}, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = h.maxprobe
    index = hashindex(h.hash(key), sz)
    avail = 0
    keys = h.keys

    while true
        if isslotempty(h,index)
            avail < 0 && return avail
            return -index
        end

        if isslotmissing(h,index)
            if avail == 0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = -index
            end
        elseif h.isequal(key, keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter += 1
        iter > maxprobe && break
    end

    avail < 0 && return avail

    maxallowed = max(maxallowedprobe, sz>>maxprobeshift)
    # Check if key is not present, may need to keep searching to find slot
    while iter < maxallowed
        if !isslotfilled(h,index)
            h.maxprobe = iter
            return -index
        end
        index = (index & (sz-1)) + 1
        iter += 1
    end

    rehash!(h, h.count > 64000 ? sz*2 : sz*4)

    return ht_keyindex2(h, key)
end

function _setindex!(h::CmpDict, v, key, index)
    h.slots[index] = 0x1
    h.keys[index] = key
    h.vals[index] = v
    h.count += 1
    h.dirty = true
    if index < h.idxfloor
        h.idxfloor = index
    end

    sz = length(h.keys)
    # Rehash now if necessary
    if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash!(h, h.count > 64000 ? h.count*2 : h.count*4)
    end
end

function setindex!{K,V}(h::CmpDict{K,V}, v0, key0)
    key = convert(K, key0)
    if !h.isequal(key, key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    return setindex!(h, v0, key)
end

function setindex!{K,V}(h::CmpDict{K,V}, v0, key::K)
    v = convert(V, v0)
    index = ht_keyindex2(h, key)

    if index > 0
        h.keys[index] = key
        h.vals[index] = v
    else
        _setindex!(h, v, key, -index)
    end

    return h
end

function get!{K,V}(h::CmpDict{K,V}, key0, default)
    key = convert(K,key0)
    if !h.isequal(key,key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    return get!(h, key, default)
end

function get!{K, V}(h::CmpDict{K,V}, key::K, default)
    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    v = convert(V, default)
    _setindex!(h, v, key, -index)
    return v
end

function get!{K,V}(default::Callable, h::CmpDict{K,V}, key0)
    key = convert(K,key0)
    if !h.isequal(key,key0)
        throw(ArgumentError("$key0 is not a valid key for type $K"))
    end
    return get!(default, h, key)
end

function get!{K,V}(default::Callable, h::CmpDict{K,V}, key::K)
    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    h.dirty = false
    v = convert(V,  default())
    if h.dirty
        index = ht_keyindex2(h, key)
    end
    if index > 0
        h.keys[index] = key
        h.vals[index] = v
    else
        _setindex!(h, v, key, -index)
    end
    return v
end

# NOTE: this macro is specific to Dict, not Associative, and should
#       therefore not be exported as-is: it's for internal use only.
macro get!(h, key0, default)
    quote
        K, V = keytype($(esc(h))), valtype($(esc(h)))
        key = convert(K, $(esc(key0)))
        if !h.isequal(key, $(esc(key0)))
            throw(ArgumentError(string($(esc(key0)), " is not a valid key for type ", K)))
        end
        idx = ht_keyindex2($(esc(h)), key)
        if idx < 0
            idx = -idx
            v = convert(V, $(esc(default)))
            _setindex!($(esc(h)), v, key, idx)
        else
            @inbounds v = $(esc(h)).vals[idx]
        end
        v
    end
end


function getindex{K,V}(h::CmpDict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get{K,V}(h::CmpDict{K,V}, key, default)
    index = ht_keyindex(h, key)
    return (index<0) ? default : h.vals[index]::V
end

function get{K,V}(default::Callable, h::CmpDict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? default() : h.vals[index]::V
end

haskey(h::CmpDict, key) = (ht_keyindex(h, key) >= 0)
in{T<:CmpDict}(key, v::KeyIterator{T}) = (ht_keyindex(v.dict, key) >= 0)

function getkey{K,V}(h::CmpDict{K,V}, key, default)
    index = ht_keyindex(h, key)
    return (index<0) ? default : h.keys[index]::K
end

function _pop!(h::CmpDict, index)
    val = h.vals[index]
    _delete!(h, index)
    return val
end

function pop!(h::CmpDict, key)
    index = ht_keyindex(h, key)
    index > 0 ? _pop!(h, index) : throw(KeyError(key))
end

function pop!(h::CmpDict, key, default)
    index = ht_keyindex(h, key)
    index > 0 ? _pop!(h, index) : default
end

function _delete!(h::CmpDict, index)
    h.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, UInt), h.keys, index-1)
    ccall(:jl_arrayunset, Void, (Any, UInt), h.vals, index-1)
    h.ndel += 1
    h.count -= 1
    h.dirty = true
    h
end

function delete!(h::CmpDict, key)
    index = ht_keyindex(h, key)
    if index > 0; _delete!(h, index); end
    h
end

function skip_deleted(h::CmpDict, i)
    L = length(h.slots)
    while i<=L && !isslotfilled(h,i)
        i += 1
    end
    return i
end

function start(t::CmpDict)
    i = skip_deleted(t, t.idxfloor)
    t.idxfloor = i
    return i
end
done(t::CmpDict, i) = i > length(t.vals)
next{K,V}(t::CmpDict{K,V}, i) = (Pair{K,V}(t.keys[i],t.vals[i]), skip_deleted(t,i+1))

isempty(t::CmpDict) = (t.count == 0)
length(t::CmpDict) = t.count

next{T<:CmpDict}(v::KeyIterator{T}, i) = (v.dict.keys[i], skip_deleted(v.dict,i+1))
next{T<:CmpDict}(v::ValueIterator{T}, i) = (v.dict.vals[i], skip_deleted(v.dict,i+1))

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

# For these Associative types, it is safe to implement filter!
# by deleting keys during iteration.
function filter!(f, d::Union{ObjectIdDict,Dict,WeakKeyDict})
    for (k,v) in d
        if !f(k,v)
            delete!(d,k)
        end
    end
    return d
end

immutable ImmutableDict{K, V} <: Associative{K,V}
    parent::ImmutableDict{K, V}
    key::K
    value::V
    ImmutableDict() = new() # represents an empty dictionary
    ImmutableDict(key, value) = (empty = new(); new(empty, key, value))
    ImmutableDict(parent::ImmutableDict, key, value) = new(parent, key, value)
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
ImmutableDict{K,V}(KV::Pair{K,V}) = ImmutableDict{K,V}(KV[1], KV[2])
ImmutableDict{K,V}(t::ImmutableDict{K,V}, KV::Pair) = ImmutableDict{K,V}(t, KV[1], KV[2])

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
next{K,V}(::ImmutableDict{K,V}, t) = (Pair{K,V}(t.key, t.value), t.parent)
done(::ImmutableDict, t) = !isdefined(t, :parent)
length(t::ImmutableDict) = count(x->1, t)
isempty(t::ImmutableDict) = done(t, start(t))
function similar(t::ImmutableDict)
    while isdefined(t, :parent)
        t = t.parent
    end
    return t
end
