# generic operations on associative collections

abstract Associative{K,V}

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::Associative, k) = in(k,keys(d))

function in(p::(Any,Any), a::Associative)
    v = get(a,p[1],secret_table_token)
    !is(v, secret_table_token) && (v == p[2])
end

function summary(t::Associative)
    n = length(t)
    string(typeof(t), " with ", n, (n==1 ? " entry" : " entries"))
end

function show{K,V}(io::IO, t::Associative{K,V})
    if isempty(t)
        print(io, typeof(t), "()")
    else
        if isleaftype(K) && isleaftype(V)
            print(io, typeof(t).name)
        else
            print(io, typeof(t))
        end
        print(io, '(')
        first = true
        for (k, v) in t
            first || print(io, ',')
            first = false
            show(io, k)
            print(io, "=>")
            show(io, v)
        end
        print(io, ')')
    end
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
    if lastidx < sizeof(str)
        return bytestring(SubString(str, 1, truncidx) * truncmark)
    else
        return bytestring(str)
    end
end

showdict(t::Associative; kw...) = showdict(STDOUT, t; kw...)
function showdict{K,V}(io::IO, t::Associative{K,V}; limit::Bool = false,
                       sz=(s = tty_size(); (s[1]-3, s[2])))
    rows, cols = sz
    print(io, summary(t))
    isempty(t) && return
    print(io, ":")

    if limit
        rows < 2   && (print(io, " …"); return)
        cols < 12  && (cols = 12) # Minimum widths of 2 for key, 4 for value
        cols -= 6 # Subtract the widths of prefix "  " separator " => "
        rows -= 2 # Subtract the summary and final ⋮ continuation lines

        # determine max key width to align the output, caching the strings
        ks = Array(String, min(rows, length(t)))
        keylen = 0
        for (i, k) in enumerate(keys(t))
            i > rows && break
            ks[i] = sprint(show, k)
            keylen = clamp(length(ks[i]), keylen, div(cols, 3))
        end
    end

    for (i, (k, v)) in enumerate(t)
        print(io, "\n  ")
        limit && i > rows && (print(io, rpad("⋮", keylen), " => ⋮"); break)

        if limit
            key = rpad(_truncate_at_width_or_chars(ks[i], keylen, "\r\n"), keylen)
        else
            key = sprint(show, k)
        end
        print(io, key)
        print(io, " => ")

        val = sprint(show, v)
        if limit
            val = _truncate_at_width_or_chars(val, cols - keylen, "\r\n")
        end
        print(io, val)
    end
end

immutable KeyIterator{T<:Associative}
    dict::T
end
immutable ValueIterator{T<:Associative}
    dict::T
end

summary{T<:Union(KeyIterator,ValueIterator)}(iter::T) =
    string(T.name, " for a ", summary(iter.dict))

show(io::IO, iter::Union(KeyIterator,ValueIterator)) = show(io, collect(iter))

showkv(iter::Union(KeyIterator,ValueIterator); kw...) = showkv(STDOUT, iter; kw...)
function showkv{T<:Union(KeyIterator,ValueIterator)}(io::IO, iter::T; limit::Bool = false,
                                                     sz=(s = tty_size(); (s[1]-3, s[2])))
    rows, cols = sz
    print(io, summary(iter))
    isempty(iter) && return
    print(io, ". ", T<:KeyIterator ? "Keys" : "Values", ":")
    if limit
        rows < 2 && (print(io, " …"); return)
        cols < 4 && (cols = 4)
        cols -= 2 # For prefix "  "
        rows -= 2 # For summary and final ⋮ continuation lines
    end

    for (i, v) in enumerate(iter)
        print(io, "\n  ")
        limit && i >= rows && (print(io, "⋮"); break)

        str = sprint(show, v)
        limit && (str = _truncate_at_width_or_chars(str, cols, "\r\n"))
        print(io, str)
    end
end

length(v::Union(KeyIterator,ValueIterator)) = length(v.dict)
isempty(v::Union(KeyIterator,ValueIterator)) = isempty(v.dict)
eltype(v::KeyIterator) = eltype(v.dict)[1]
eltype(v::ValueIterator) = eltype(v.dict)[2]

start(v::Union(KeyIterator,ValueIterator)) = start(v.dict)
done(v::Union(KeyIterator,ValueIterator), state) = done(v.dict, state)

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
merge(d::Associative, others::Associative...) = merge!(copy(d), others...)

function filter!(f::Function, d::Associative)
    for (k,v) in d
        if !f(k,v)
            delete!(d,k)
        end
    end
    return d
end
filter(f::Function, d::Associative) = filter!(f,copy(d))

eltype{K,V}(a::Associative{K,V}) = (K,V)

function isequal(l::Associative, r::Associative)
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for (key, value) in l
        if !isequal(value, get(r, key, secret_table_token))
            return false
        end
    end
    true
end

function ==(l::Associative, r::Associative)
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for (key, value) in l
        if value != get(r, key, secret_table_token)
            return false
        end
    end
    true
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

push!(t::Associative, key, v) = setindex!(t, v, key)

# hashing objects by identity

type ObjectIdDict <: Associative{Any,Any}
    ht::Array{Any,1}
    ObjectIdDict() = new(cell(32))

    function ObjectIdDict(itr)
        d = ObjectIdDict()
        for (k,v) in itr
            d[k] = v
        end
        d
    end

    function ObjectIdDict(o::ObjectIdDict)
        N = length(o.ht)
        ht = cell(N)
        ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Uint),
              ht, o.ht, N*sizeof(Ptr))
        new(ht)
    end
end

similar(d::ObjectIdDict) = ObjectIdDict()

function setindex!(t::ObjectIdDict, v::ANY, k::ANY)
    t.ht = ccall(:jl_eqtable_put, Array{Any,1}, (Any, Any, Any), t.ht, k, v)
    return t
end

get(t::ObjectIdDict, key::ANY, default::ANY) =
    ccall(:jl_eqtable_get, Any, (Any, Any, Any), t.ht, key, default)

pop!(t::ObjectIdDict, key::ANY, default::ANY) =
    ccall(:jl_eqtable_pop, Any, (Any, Any, Any), t.ht, key, default)

function pop!(t::ObjectIdDict, key::ANY)
    val = pop!(t, key, secret_table_token)
    !is(val,secret_table_token) ? val : throw(KeyError(key))
end

function delete!(t::ObjectIdDict, key::ANY)
    ccall(:jl_eqtable_pop, Any, (Any, Any), t.ht, key)
    t
end

empty!(t::ObjectIdDict) = (t.ht = cell(length(t.ht)); t)

_oidd_nextind(a, i) = reinterpret(Int,ccall(:jl_eqtable_nextind, Csize_t, (Any, Csize_t), a, i))

start(t::ObjectIdDict) = _oidd_nextind(t.ht, 0)
done(t::ObjectIdDict, i) = (i == -1)
next(t::ObjectIdDict, i) = ((t.ht[i+1],t.ht[i+2]), _oidd_nextind(t.ht, i+2))

function length(d::ObjectIdDict)
    n = 0
    for pair in d
        n+=1
    end
    n
end

copy(o::ObjectIdDict) = ObjectIdDict(o)

# dict

type Dict{K,V} <: Associative{K,V}
    slots::Array{Uint8,1}
    keys::Array{K,1}
    vals::Array{V,1}
    ndel::Int
    count::Int
    deleter::Function

    function Dict()
        n = 16
        new(zeros(Uint8,n), Array(K,n), Array(V,n), 0, 0, identity)
    end
    function Dict(kv)
        h = Dict{K,V}()
        for (k,v) in kv
            h[k] = v
        end
        return h
    end
    Dict(p::Pair) = setindex!(Dict{K,V}(), p.second, p.first)
    function Dict(ps::Pair...)
        h = Dict{K,V}()
        sizehint(h, length(ps))
        for p in ps
            h[p.first] = p.second
        end
        return h
    end
end
Dict() = Dict{Any,Any}()
Dict(kv::()) = Dict()

const AnyDict = Dict{Any,Any}

# TODO: this can probably be simplified using `eltype` as a THT (Tim Holy trait)
Dict{K,V}(kv::((K,V)...,))               = Dict{K,V}(kv)
Dict{K  }(kv::((K,Any)...,))             = Dict{K,Any}(kv)
Dict{V  }(kv::((Any,V)...,))             = Dict{Any,V}(kv)
Dict{K,V}(kv::(Pair{K,V}...,))           = Dict{K,V}(kv)
Dict{K}  (kv::(Pair{K}...,))             = Dict{K,Any}(kv)
Dict{V}  (kv::(Pair{TypeVar(:K),V}...,)) = Dict{Any,V}(kv)
Dict     (kv::(Pair...,))                = Dict{Any,Any}(kv)

Dict{K,V}(kv::AbstractArray{(K,V)})     = Dict{K,V}(kv)
Dict{K,V}(kv::AbstractArray{Pair{K,V}}) = Dict{K,V}(kv)
Dict{K,V}(kv::Associative{K,V})         = Dict{K,V}(kv)

Dict{K,V}(ps::Pair{K,V}...)            = Dict{K,V}(ps)
Dict{K}  (ps::Pair{K}...,)             = Dict{K,Any}(ps)
Dict{V}  (ps::Pair{TypeVar(:K),V}...,) = Dict{Any,V}(ps)
Dict     (ps::Pair...)                 = Dict{Any,Any}(ps)

Dict(kv) = dict_with_eltype(kv, eltype(kv))
dict_with_eltype{K,V}(kv, ::Type{(K,V)}) = Dict{K,V}(kv)
dict_with_eltype{K,V}(kv, ::Type{Pair{K,V}}) = Dict{K,V}(kv)
dict_with_eltype(kv, t) = Dict{Any,Any}(kv)

similar{K,V}(d::Dict{K,V}) = Dict{K,V}()

# conversion between Dict types
function convert{K,V}(::Type{Dict{K,V}},d::Associative)
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
convert{K,V}(::Type{Dict{K,V}},d::Dict{K,V}) = d

function serialize(s, t::Dict)
    serialize_type(s, typeof(t))
    write(s, int32(length(t)))
    for (k,v) in t
        serialize(s, k)
        serialize(s, v)
    end
end

function deserialize{K,V}(s, T::Type{Dict{K,V}})
    n = read(s, Int32)
    t = T(); sizehint(t, n)
    for i = 1:n
        k = deserialize(s)
        v = deserialize(s)
        t[k] = v
    end
    return t
end

hashindex(key, sz) = (reinterpret(Int,hash(key)) & (sz-1)) + 1

isslotempty(h::Dict, i::Int) = h.slots[i] == 0x0
isslotfilled(h::Dict, i::Int) = h.slots[i] == 0x1
isslotmissing(h::Dict, i::Int) = h.slots[i] == 0x2

function rehash{K,V}(h::Dict{K,V}, newsz)
    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    sz = length(olds)
    newsz = _tablesz(newsz)
    if h.count == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        h.ndel = 0
        return h
    end

    slots = zeros(Uint8,newsz)
    keys = Array(K, newsz)
    vals = Array(V, newsz)
    count0 = h.count
    count = 0

    for i = 1:sz
        if olds[i] == 0x1
            k = oldk[i]
            v = oldv[i]
            index = hashindex(k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            slots[index] = 0x1
            keys[index] = k
            vals[index] = v
            count += 1

            if h.count != count0
                # if items are removed by finalizers, retry
                return rehash(h, newsz)
            end
        end
    end

    h.slots = slots
    h.keys = keys
    h.vals = vals
    h.count = count
    h.ndel = 0

    return h
end

function sizehint(d::Dict, newsz)
    oldsz = length(d.slots)
    if newsz <= oldsz
        # todo: shrink
        # be careful: rehash() assumes everything fits. it was only designed
        # for growing.
        return d
    end
    # grow at least 25%
    newsz = max(newsz, (oldsz*5)>>2)
    rehash(d, newsz)
end

function empty!{K,V}(h::Dict{K,V})
    fill!(h.slots, 0x0)
    sz = length(h.slots)
    empty!(h.keys)
    empty!(h.vals)
    resize!(h.keys, sz)
    resize!(h.vals, sz)
    h.ndel = 0
    h.count = 0
    return h
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex{K,V}(h::Dict{K,V}, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    keys = h.keys

    while true
        if isslotempty(h,index)
            break
        end
        if !isslotmissing(h,index) && isequal(key,keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter+=1
        iter > maxprobe && break
    end

    return -1
end

# get the index where a key is stored, or -pos if not present
# and the key would be inserted at pos
# This version is for use by setindex! and get!
function ht_keyindex2{K,V}(h::Dict{K,V}, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
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
        elseif isequal(key, keys[index])
            return index
        end

        index = (index & (sz-1)) + 1
        iter+=1
        iter > maxprobe && break
    end

    avail < 0 && return avail

    rehash(h, h.count > 64000 ? sz*2 : sz*4)

    return ht_keyindex2(h, key)
end

function _setindex!(h::Dict, v, key, index)
    h.slots[index] = 0x1
    h.keys[index] = key
    h.vals[index] = v
    h.count += 1

    sz = length(h.keys)
    # Rehash now if necessary
    if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash(h, h.count > 64000 ? h.count*2 : h.count*4)
    end
end

function setindex!{K,V}(h::Dict{K,V}, v0, key0)
    key = convert(K,key0)
    if !isequal(key,key0)
        error(key0, " is not a valid key for type ", K)
    end
    v = convert(V,  v0)

    index = ht_keyindex2(h, key)

    if index > 0
        h.keys[index] = key
        h.vals[index] = v
    else
        _setindex!(h, v, key, -index)
    end

    return h
end

function get!{K,V}(h::Dict{K,V}, key0, default)
    key = convert(K,key0)
    if !isequal(key,key0)
        error(key0, " is not a valid key for type ", K)
    end

    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    v = convert(V,  default)
    _setindex!(h, v, key, -index)
    return v
end

function get!{K,V}(default::Callable, h::Dict{K,V}, key0)
    key = convert(K,key0)
    if !isequal(key,key0)
        error(key0, " is not a valid key for type ", K)
    end

    index = ht_keyindex2(h, key)

    index > 0 && return h.vals[index]

    v = convert(V,  default())
    _setindex!(h, v, key, -index)
    return v
end

# NOTE: this macro is specific to Dict, not Associative, and should
#       therefore not be exported as-is: it's for internal use only.
macro get!(h, key0, default)
    quote
        K, V = eltype($(esc(h)))
        key = convert(K, $(esc(key0)))
        isequal(key, $(esc(key0))) || error($(esc(key0)), " is not a valid key for type ", K)
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


function getindex{K,V}(h::Dict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get{K,V}(h::Dict{K,V}, key, default)
    index = ht_keyindex(h, key)
    return (index<0) ? default : h.vals[index]::V
end

function get{K,V}(default::Callable, h::Dict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? default() : h.vals[index]::V
end

haskey(h::Dict, key) = (ht_keyindex(h, key) >= 0)
in{T<:Dict}(key, v::KeyIterator{T}) = (ht_keyindex(v.dict, key) >= 0)

function getkey{K,V}(h::Dict{K,V}, key, default)
    index = ht_keyindex(h, key)
    return (index<0) ? default : h.keys[index]::K
end

function _pop!(h::Dict, index)
    val = h.vals[index]
    _delete!(h, index)
    return val
end

function pop!(h::Dict, key)
    index = ht_keyindex(h, key)
    index > 0 ? _pop!(h, index) : throw(KeyError(key))
end

function pop!(h::Dict, key, default)
    index = ht_keyindex(h, key)
    index > 0 ? _pop!(h, index) : default
end

function _delete!(h::Dict, index)
    h.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, Uint), h.keys, index-1)
    ccall(:jl_arrayunset, Void, (Any, Uint), h.vals, index-1)
    h.ndel += 1
    h.count -= 1
    h
end

function delete!(h::Dict, key)
    index = ht_keyindex(h, key)
    if index > 0; _delete!(h, index); end
    h
end

function skip_deleted(h::Dict, i)
    L = length(h.slots)
    while i<=L && !isslotfilled(h,i)
        i += 1
    end
    return i
end

start(t::Dict) = skip_deleted(t, 1)
done(t::Dict, i) = done(t.vals, i)
next(t::Dict, i) = ((t.keys[i],t.vals[i]), skip_deleted(t,i+1))

isempty(t::Dict) = (t.count == 0)
length(t::Dict) = t.count

next{T<:Dict}(v::KeyIterator{T}, i) = (v.dict.keys[i], skip_deleted(v.dict,i+1))
next{T<:Dict}(v::ValueIterator{T}, i) = (v.dict.vals[i], skip_deleted(v.dict,i+1))

# weak key dictionaries

function weak_key_delete!(t::Dict, k)
    # when a weak key is finalized, remove from dictionary if it is still there
    wk = getkey(t, k, secret_table_token)
    if !is(wk,secret_table_token) && is(wk.value, k)
        delete!(t, k)
    end
end

function add_weak_key(t::Dict, k, v)
    if is(t.deleter, identity)
        t.deleter = x->weak_key_delete!(t, x)
    end
    t[WeakRef(k)] = v
    # TODO: it might be better to avoid the finalizer, allow
    # wiped WeakRefs to remain in the table, and delete them as
    # they are discovered by getindex and setindex!.
    finalizer(k, t.deleter)
    return t
end

function weak_value_delete!(t::Dict, k, v)
    # when a weak value is finalized, remove from dictionary if it is still there
    wv = get(t, k, secret_table_token)
    if !is(wv,secret_table_token) && is(wv.value, v)
        delete!(t, k)
    end
end

function add_weak_value(t::Dict, k, v)
    t[k] = WeakRef(v)
    finalizer(v, x->weak_value_delete!(t, k, x))
    return t
end

type WeakKeyDict{K,V} <: Associative{K,V}
    ht::Dict{Any,V}

    WeakKeyDict() = new(Dict{Any,V}())
end
WeakKeyDict() = WeakKeyDict{Any,Any}()

setindex!{K}(wkh::WeakKeyDict{K}, v, key) = add_weak_key(wkh.ht, convert(K,key), v)

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
function next{K}(t::WeakKeyDict{K}, i)
    kv, i = next(t.ht, i)
    ((kv[1].value::K,kv[2]), i)
end
length(t::WeakKeyDict) = length(t.ht)
