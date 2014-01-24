# generic operations on associative collections

abstract Associative{K,V}

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::Associative, k) = in(k,keys(d))

function in(p::(Any,Any), a::Associative)
    v = get(a,p[1],secret_table_token)
    !is(v, secret_table_token) && (v == p[2])
end

function show{K,V}(io::IO, t::Associative{K,V})
    if isempty(t)
        print(io, typeof(t),"()")
    else
        if K === Any && V === Any
            delims = ['{','}']
        else
            delims = ['[',']']
        end
        print(io, delims[1])
        first = true
        for (k, v) = t
            first || print(io, ',')
            first = false
            show(io, k)
            print(io, "=>")
            show(io, v)
        end
        print(io, delims[2])
    end
end

immutable KeyIterator{T<:Associative}
    dict::T
end
immutable ValueIterator{T<:Associative}
    dict::T
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

function hash(d::Associative)
    h::Uint = 0
    for (k,v) in d
        h $= bitmix(hash(k),~hash(v))
    end
    h
end

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

start(t::ObjectIdDict) = 0
done(t::ObjectIdDict, i) = is(next(t,i),())
next(t::ObjectIdDict, i) = ccall(:jl_eqtable_next, Any, (Any, Uint32), t.ht, i)

isempty(t::ObjectIdDict) = is(next(t,0),())

function length(d::ObjectIdDict)
    n = 0
    for pair in d
        n+=1
    end
    n
end

# hashing

bitmix(a::Union(Int32,Uint32), b::Union(Int32,Uint32)) =
    ccall(:int64to32hash, Uint32, (Uint64,),
          or_int(shl_int(zext_int(Uint64,unbox(Uint32,a)), 32), zext_int(Uint64,unbox(Uint32,b))))

bitmix(a::Union(Int64,Uint64), b::Union(Int64, Uint64)) =
    ccall(:int64hash, Uint64, (Uint64,),
          xor_int(unbox(Uint64,a), or_int(lshr_int(unbox(Uint64,b), 32),
                                           shl_int(unbox(Uint64,b), 32))))

if WORD_SIZE == 64
    hash64(x::Float64) =
        ccall(:int64hash, Uint64, (Uint64,), box(Uint64,unbox(Float64,x)))
    hash64(x::Union(Int64,Uint64)) =
        ccall(:int64hash, Uint64, (Uint64,), x)
else
    hash64(x::Float64) =
        ccall(:int64to32hash, Uint32, (Uint64,), box(Uint64,unbox(Float64,x)))
    hash64(x::Union(Int64,Uint64)) =
        ccall(:int64to32hash, Uint32, (Uint64,), x)
end

hash(x::Union(Bool,Char,Int8,Uint8,Int16,Uint16,Int32,Uint32,Int64,Uint64)) =
    hash64(uint64(x))

function hash(x::Integer)
    h::Uint = hash(uint64(x&0xffffffffffffffff))
    if typemin(Int64) <= x <= typemax(Uint64)
        return h
    end
    x >>>= 64
    while x != 0 && x != -1
        h = bitmix(h, hash(uint64(x&0xffffffffffffffff)))
        x >>>= 64
    end
    return h
end

hash(x::Float32) = hash(reinterpret(Uint32, isnan(x) ? NaN32 : x))
hash(x::Float64) = hash(reinterpret(Uint64, isnan(x) ? NaN   : x))

function hash(t::Tuple)
    h::Uint = 0
    for i=1:length(t)
        h = bitmix(h,int(hash(t[i]))+42)
    end
    return h
end

function hash(a::AbstractArray)
    h::Uint = hash(size(a))+1
    for i=1:length(a)
        h = bitmix(h,int(hash(a[i])))
    end
    return h
end

# make sure Array{Bool} and BitArray can be equivalent
hash(a::AbstractArray{Bool}) = hash(bitpack(a))

hash(x::ANY) = object_id(x)

if WORD_SIZE == 64
    hash(s::ByteString) =
        ccall(:memhash, Uint64, (Ptr{Void}, Int), s.data, length(s.data))
    hash(s::ByteString, seed::Union(Int,Uint)) =
        ccall(:memhash_seed, Uint64, (Ptr{Void}, Int, Uint32),
              s.data, length(s.data), uint32(seed))
else
    hash(s::ByteString) =
        ccall(:memhash32, Uint32, (Ptr{Void}, Int), s.data, length(s.data))
    hash(s::ByteString, seed::Union(Int,Uint)) =
        ccall(:memhash32_seed, Uint32, (Ptr{Void}, Int, Uint32),
              s.data, length(s.data), uint32(seed))
end

hash(s::String) = hash(bytestring(s))

hash(x::Expr) = bitmix(hash(x.head),hash(x.args)+43)


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
    function Dict(ks, vs)
        # TODO: eventually replace with a call to Dict(zip(ks,vs))
        n = min(length(ks), length(vs))
        h = Dict{K,V}()
        for i=1:n
            h[ks[i]] = vs[i]
        end
        return h
    end
    function Dict(kv)
        h = Dict{K,V}()
        for (k,v) in kv
            h[k] = v
        end
        return h
    end
end
Dict() = Dict{Any,Any}()

Dict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = Dict{K,V}(ks,vs)
Dict(ks, vs) = Dict{Any,Any}(ks, vs)

# syntax entry points
Dict{K,V}(ks::(K...), vs::(V...)) = Dict{K  ,V  }(ks, vs)
Dict{K  }(ks::(K...), vs::Tuple ) = Dict{K  ,Any}(ks, vs)
Dict{V  }(ks::Tuple , vs::(V...)) = Dict{Any,V  }(ks, vs)

Dict{K,V}(kv::AbstractArray{(K,V)}) = Dict{K,V}(kv)

similar{K,V}(d::Dict{K,V}) = (K=>V)[]

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

hashindex(key, sz) = (int(hash(key)) & (sz-1)) + 1

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
    h.keys = Array(K, sz)
    h.vals = Array(V, sz)
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

    if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash(h, h.count > 64000 ? h.count*2 : h.count*4)
        sz = length(h.keys)  # rehash may resize the table at this point!
    end

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
end

function setindex!{K,V}(h::Dict{K,V}, v0, key0)
    key = convert(K,key0)
    if !isequal(key,key0)
        error(key0, " is not a valid key for type ", K)
    end
    v = convert(V,  v0)

    index = ht_keyindex2(h, key)

    if index > 0
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

function getindex{K,V}(h::Dict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get{K,V}(h::Dict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.vals[index]::V
end

haskey(h::Dict, key) = (ht_keyindex(h, key) >= 0)
in{T<:Dict}(key, v::KeyIterator{T}) = (ht_keyindex(v.dict, key) >= 0)

function getkey{K,V}(h::Dict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.keys[index]::K
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

    WeakKeyDict() = new((Any=>V)[])
end
WeakKeyDict() = WeakKeyDict{Any,Any}()

setindex!{K}(wkh::WeakKeyDict{K}, v, key) = add_weak_key(wkh.ht, convert(K,key), v)

function getkey{K}(wkh::WeakKeyDict{K}, kk, deflt)
    k = getkey(wkh.ht, kk, secret_table_token)
    if is(k, secret_table_token)
        return deflt
    end
    return k.value::K
end

get{K}(wkh::WeakKeyDict{K}, key, def) = get(wkh.ht, key, def)
pop!{K}(wkh::WeakKeyDict{K}, key) = pop!(wkh.ht, key)
pop!{K}(wkh::WeakKeyDict{K}, key, def) = pop!(wkh.ht, key, def)
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
