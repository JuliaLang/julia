# generic operations on associative collections

abstract Associative{K,V}

const secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

haskey(d::Associative, k) = contains(keys(d),k)

function contains(a::Associative, p::(Any,Any))
    v = get(a,p[1],secret_table_token)
    !is(v, secret_table_token) && isequal(v, p[2])
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

contains(v::KeyIterator, k) = !is(get(v.dict, k, secret_table_token),
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
    h = 0
    for (k,v) in d
        h $= bitmix(hash(k),~hash(v))
    end
    h
end

# Used as default value arg to get in isequal: something that will
# never be found in any dictionary.
const _MISSING = gensym()

function isequal(l::Associative, r::Associative)
    if isa(l,ObjectIdDict) != isa(r,ObjectIdDict)
        return false
    end
    if length(l) != length(r) return false end
    for (key, value) in l
        if !isequal(value, get(r, key, _MISSING))
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

delete!(t::ObjectIdDict, key::ANY, default::ANY) =
    ccall(:jl_eqtable_del, Any, (Any, Any, Any), t.ht, key, default)

function delete!(t::ObjectIdDict, key::ANY)
    val = delete!(t, key, secret_table_token)
    !is(val,secret_table_token) ? val : throw(KeyError(key))
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

@eval function hash(x::FloatingPoint)
    if trunc(x) == x
        # hash as integer if equal to some integer. note the result of
        # float to int conversion is only defined for in-range values.
        if x < 0
            if $(float64(typemin(Int64))) <= x
                return hash(int64(x))
            end
        else
            # note: float64(typemax(Uint64)) == 2^64
            if x < $(float64(typemax(Uint64)))
                return hash(uint64(x))
            end
        end
    end
    isnan(x) ? $(hash64(NaN)) : hash64(float64(x))
end

function hash(t::Tuple)
    h = int(0)
    for i=1:length(t)
        h = bitmix(h,int(hash(t[i])))
    end
    return uint(h)
end

function hash(a::Array)
    h = hash(size(a))+1
    for i=1:length(a)
        h = bitmix(h,int(hash(a[i])))
    end
    return uint(h)
end

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


# dict

typealias Unordered Nothing
typealias Ordered   Int

type Dict{K,V,O<:Union(Ordered,Unordered)} <: Associative{K,V}
    slots::Array{Uint8,1}
    keys::Array{K,1}
    vals::Array{V,1}
    idxs::Array{O,1}
    order::Array{O,1}
    ndel::Int
    count::Int
    deleter::Function

    function Dict()
        n = 16
        new(zeros(Uint8,n), Array(K,n), Array(V,n), Array(O,n), Array(O,0), 0, 0, identity)
    end
    function Dict(ks, vs)
        n = length(ks)
        h = Dict{K,V,O}()
        for i=1:n
            h[ks[i]] = vs[i]
        end
        return h
    end
end

Dict() = Dict{Any,Any,Unordered}()

Dict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = Dict{K,V,Unordered}(ks,vs)
Dict(ks, vs) = Dict{Any,Any,Unordered}(ks, vs)

# syntax entry points
Dict{K,V}(ks::(K...), vs::(V...)) = Dict{K  ,V  ,Unordered}(ks, vs)
Dict{K  }(ks::(K...), vs::Tuple ) = Dict{K  ,Any,Unordered}(ks, vs)
Dict{V  }(ks::Tuple , vs::(V...)) = Dict{Any,V  ,Unordered}(ks, vs)

# ordered dict

type OrderedDict{K,V} <: Associative{K,V}
    OrderedDict() = Dict{K,V,Ordered}()
    OrderedDict(ks,vs) = Dict{K,V,Ordered}(ks,vs)
end

OrderedDict() = OrderedDict{Any,Any}()

OrderedDict{K,V}(ks::AbstractArray{K}, vs::AbstractArray{V}) = OrderedDict{K,V}(ks,vs)
OrderedDict(ks, vs) = OrderedDict{Any,Any}(ks, vs)

OrderedDict{K,V}(ks::(K...), vs::(V...)) = OrderedDict{K  ,V  }(ks, vs)
OrderedDict{K  }(ks::(K...), vs::Tuple ) = OrderedDict{K  ,Any}(ks, vs)
OrderedDict{V  }(ks::Tuple , vs::(V...)) = OrderedDict{Any,V  }(ks, vs)


similar{K,V,O}(d::Dict{K,V,O}) = Dict{K,V,O}()

function serialize(s, t::Dict)
    serialize_type(s, typeof(t))
    write(s, int32(length(t)))
    for (k,v) in t
        serialize(s, k)
        serialize(s, v)
    end
end

function deserialize{K,V,O}(s, T::Type{Dict{K,V,O}})
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

function rehash{K,V}(h::Dict{K,V,Unordered}, newsz)
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

function rehash{K,V}(h::Dict{K,V,Ordered}, newsz)
    _compact_order(h)

    olds = h.slots
    oldk = h.keys
    oldv = h.vals
    oldi = h.idxs
    oldo = h.order
    sz = length(olds)
    newsz = _tablesz(newsz)
    if h.count == 0
        resize!(h.slots, newsz)
        fill!(h.slots, 0)
        resize!(h.keys, newsz)
        resize!(h.vals, newsz)
        resize!(h.idxs, newsz)
        resize!(h.order, 0)
        h.ndel = 0
        return h
    end

    slots = zeros(Uint8,newsz)
    keys = Array(K, newsz)
    vals = Array(V, newsz)
    idxs = Array(Int, newsz)
    order = Array(Int, h.count)
    count0 = h.count
    count = 0

    for i = 1:sz
        if olds[i] == 0x1
            k = oldk[i]
            v = oldv[i]
            idx = oldi[i]
            index = hashindex(k, newsz)
            while slots[index] != 0
                index = (index & (newsz-1)) + 1
            end
            slots[index] = 0x1
            keys[index] = k
            vals[index] = v
            idxs[index] = idx
            order[idx] = index
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
    h.idxs = idxs
    h.order = order
    h.count = count
    h.ndel = 0

    return h
end


function _compact_order{K,V}(h::Dict{K,V,Ordered})
    if h.count == length(h.order)
        return
    end

    i = 1
    while h.order[i] > 0;  i += 1; end

    j = i+1
    while h.order[j] == 0; j += 1; end

    for k = j:length(h.order)
        idx = h.order[k]
        if idx > 0
            h.order[i] = idx
            h.idxs[idx] = i
            i += 1
        end
    end

    resize!(h.order, h.count)

    nothing
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

function empty!{K,V}(h::Dict{K,V,Ordered})
    sz = length(h.slots)
    fill!(h.slots, 0x0)
    h.keys = Array(K, sz)
    h.vals = Array(V, sz)
    h.idxs = Array(Int, sz)
    h.order = Array(Int, 0)
    h.ndel = 0
    h.count = 0
    return h
end

function _setindex!(h::Dict, index, key, v)
    h.slots[index] = 0x1
    h.keys[index] = key
    h.vals[index] = v
    h.count += 1
    return h
end

function _setindex!{K,V}(h::Dict{K,V,Ordered}, index, key, v)
    h.slots[index] = 0x1
    h.keys[index] = key
    h.vals[index] = v
    push!(h.order, index)
    h.idxs[index] = length(h.order)
    h.count += 1
    return h
end

function setindex!{K,V}(h::Dict{K,V}, v, key)
    key = convert(K,key)
    v   = convert(V,  v)

    sz = length(h.keys)

    if h.ndel >= ((3*sz)>>2) || h.count*3 > sz*2
        # > 3/4 deleted or > 2/3 full
        rehash(h, h.count > 64000 ? h.count*2 : h.count*4)
        sz = length(h.keys)  # rehash may resize the table at this point!
    end

    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    orig = index
    avail = -1  # an available slot
    keys = h.keys; vals = h.vals

    while true
        if isslotempty(h,index)
            if avail > 0; index = avail; end
            return _setindex!(h, index, key, v)
        end

        if isslotmissing(h,index)
            if avail<0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = index
            end
        elseif isequal(key, keys[index])
            vals[index] = v
            return h
        end

        index = (index & (sz-1)) + 1
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    if avail>0
        index = avail
        return _setindex!(h, index, key, v)
    end

    rehash(h, h.count > 64000 ? sz*2 : sz*4)

    setindex!(h, v, key)
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex{K,V}(h::Dict{K,V}, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = max(16, sz>>6)
    index = hashindex(key, sz)
    orig = index
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
        if iter > maxprobe || index==orig
            break
        end
    end

    return -1
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
contains{T<:Dict}(v::KeyIterator{T}, key) = (ht_keyindex(v.dict, key) >= 0)

function getkey{K,V}(h::Dict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.keys[index]::K
end

function _delete!(h::Dict, index)
    val = h.vals[index]
    h.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, Uint), h.keys, index-1)
    ccall(:jl_arrayunset, Void, (Any, Uint), h.vals, index-1)
    h.ndel += 1
    h.count -= 1
    return val
end

function _delete!{K,V}(h::Dict{K,V,Ordered}, index)
    val = h.vals[index]
    h.slots[index] = 0x2
    ccall(:jl_arrayunset, Void, (Any, Uint), h.keys, index-1)
    ccall(:jl_arrayunset, Void, (Any, Uint), h.vals, index-1)
    h.order[h.idxs[index]] = 0
    h.ndel += 1
    h.count -= 1
    return val
end

function delete!(h::Dict, key)
    index = ht_keyindex(h, key)
    index > 0 ? _delete!(h, index) : throw(KeyError(key))
end

function delete!(h::Dict, key, default)
    index = ht_keyindex(h, key)
    index > 0 ? _delete!(h, index) : default
end

function skip_deleted(h::Dict, i)
    L = length(h.slots)
    while i<=L && !isslotfilled(h,i)
        i += 1
    end
    return i
end

function skip_deleted{K,V}(h::Dict{K,V,Ordered}, i)
    L = length(h.order)
    while i<=L && h.order[i] == 0
        i += 1
    end
    return i
end

start(t::Dict) = skip_deleted(t, 1)
done(t::Dict, i) = done(t.vals, i)
next(t::Dict, i) = ((t.keys[i],t.vals[i]), skip_deleted(t,i+1))

done{K,V}(t::Dict{K,V,Ordered}, i) = done(t.order, i)
next{K,V}(t::Dict{K,V,Ordered}, i) = ((t.keys[t.order[i]],t.vals[t.order[i]]), skip_deleted(t,i+1))

isempty(t::Dict) = (t.count == 0)
length(t::Dict) = t.count

next(v::KeyIterator{Dict}, i) = (v.dict.keys[i], skip_deleted(v.dict,i+1))
next(v::ValueIterator{Dict}, i) = (v.dict.vals[i], skip_deleted(v.dict,i+1))

next{K,V}(v::KeyIterator{Dict{K,V,Ordered}}, i) = (v.dict.keys[v.dict.order[i]], skip_deleted(v.dict,i+1))
next{K,V}(v::ValueIterator{Dict{K,V,Ordered}}, i) = (v.dict.vals[v.dict.order[i]], skip_deleted(v.dict,i+1))

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
    ht::Dict{Any,V,Nothing}

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
delete!{K}(wkh::WeakKeyDict{K}, key) = delete!(wkh.ht, key)
delete!{K}(wkh::WeakKeyDict{K}, key, def) = delete!(wkh.ht, key, def)
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
