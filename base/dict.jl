# generic operations on associative collections

abstract Associative{K,V}

const _jl_secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

has(t::Associative, key) = !is(get(t, key, _jl_secret_table_token),
                               _jl_secret_table_token)

function show(io, t::Associative)
    if isempty(t)
        print(io, typeof(t),"()")
    else
        print(io, "{")
        first = true
        for (k, v) = t
            first || print(io, ',')
            first = false
            show(io, k)
            print(io, "=>")
            show(io, v)
        end
        print(io, "}")
    end
end

function keys(T, a::Associative)
    i = 0
    keyz = Array(T,length(a))
    for (k,v) in a
        keyz[i+=1] = k
    end
    return keyz
end
keys{K,V}(a::Associative{K,V}) = keys(K,a)

function values(T, a::Associative)
    i = 0
    vals = Array(T,length(a))
    for (k,v) in a
        vals[i+=1] = v
    end
    return vals
end
values{K,V}(a::Associative{K,V}) = values(V,a)

function pairs(T::(Union(Type,Tuple),Union(Type,Tuple)), a::Associative)
    i = 0
    pairz = Array(T,length(a))
    for (k,v) in a
        pairz[i+=1] = (k,v)
    end
    return pairz
end
pairs{K,V}(a::Associative{K,V}) = pairs((K,V),a)

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
            del(d,k)
        end
    end
    return d
end
filter(f::Function, d::Associative) = filter!(f,copy(d))

# some support functions

function _tablesz(i::Integer)
    if i < 16
        return 16
    end
    if i&(i-1) == 0
        return i
    end
    while (i&(i-1) != 0)
        i = i&(i-1)
    end
    return i<<1
end

function ref(t::Associative, key)
    v = get(t, key, _jl_secret_table_token)
    if is(v,_jl_secret_table_token)
        throw(KeyError(key))
    end
    return v
end

# hashing objects by identity

type ObjectIdDict <: Associative{Any,Any}
    ht::Array{Any,1}
    ObjectIdDict(sz::Integer) = new(cell(2*_tablesz(sz)))
    ObjectIdDict() = ObjectIdDict(0)
end

similar(d::ObjectIdDict) = ObjectIdDict()

function assign(t::ObjectIdDict, v::ANY, k::ANY)
    t.ht = ccall(:jl_eqtable_put, Array{Any,1}, (Any, Any, Any), t.ht, k, v)
    return t
end

get(t::ObjectIdDict, key::ANY, default::ANY) =
    ccall(:jl_eqtable_get, Any, (Any, Any, Any), t.ht, key, default)

del(t::ObjectIdDict, key::ANY) =
    (ccall(:jl_eqtable_del, Int32, (Any, Any), t.ht, key); t)

del_all(t::ObjectIdDict) = (t.ht = cell(length(t.ht)); t)

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
          or_int(shl_int(zext64(unbox(Uint32,a)), 32), zext64(unbox(Uint32,b))))

bitmix(a::Union(Int64,Uint64), b::Union(Int64, Uint64)) =
    ccall(:int64hash, Uint64, (Uint64,),
          xor_int(unbox(Uint64,a), or_int(lshr_int(unbox(Uint64,b), 32),
                                           shl_int(unbox(Uint64,b), 32))))

if WORD_SIZE == 64
    _jl_hash64(x::Union(Int64,Uint64,Float64)) =
        ccall(:int64hash, Uint64, (Uint64,), box(Uint64,unbox(Uint64,x)))
else
    _jl_hash64(x::Union(Int64,Uint64,Float64)) =
        ccall(:int64to32hash, Uint32, (Uint64,), box(Uint64,unbox(Uint64,x)))
end

hash(x::Integer) = _jl_hash64(uint64(x))
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
    isnan(x) ? $(_jl_hash64(NaN)) : _jl_hash64(float64(x))
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

type Dict{K,V} <: Associative{K,V}
    keys::Array{Any,1}
    vals::Array{V,1}
    ndel::Int
    count::Int
    deleter::Function

    Dict() = Dict{K,V}(0)
    function Dict(n::Integer)
        n = _tablesz(n)
        new(fill!(cell(n), _jl_secret_table_token), Array(V,n),
            0, 0, identity)
    end
    function Dict(ks::Tuple, vs::Tuple)
        n = length(ks)
        h = Dict{K,V}(n)
        for i=1:n
            h[ks[i]] = vs[i]
        end
        return h
    end
end
Dict() = Dict(0)
Dict(n::Integer) = Dict{Any,Any}(n)

similar{K,V}(d::Dict{K,V}) = Dict{K,V}()

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
    t = T(n)
    for i = 1:n
        k = force(deserialize(s))
        v = force(deserialize(s))
        t[k] = v
    end
    return t
end

# syntax entry point
dict{K,V}(ks::(K...), vs::(V...)) = Dict{K,V}    (ks, vs)
dict{K}  (ks::(K...), vs::Tuple ) = Dict{K,Any}  (ks, vs)
dict{V}  (ks::Tuple , vs::(V...)) = Dict{Any,V}  (ks, vs)
dict     (ks::Tuple , vs::Tuple)  = Dict{Any,Any}(ks, vs)

hashindex(key, sz) = (int(hash(key)) & (sz-1)) + 1

const _jl_missing_token = :__c782dbf1cf4d6a2e5e3965d7e95634f2e09b5901__

function rehash{K,V}(h::Dict{K,V}, newsz)
    oldk = copy(h.keys)
    oldv = h.vals
    sz = length(oldk)
    newsz = _tablesz(newsz)
    if newsz > sz
        grow(h.keys, newsz-sz)
    end
    h.vals = Array(V, newsz)
    del_all(h)

    for i = 1:length(oldk)
        k = oldk[i]
        if !is(k,_jl_secret_table_token) && !is(k,_jl_missing_token)
            h[k] = oldv[i]
        end
    end

    return h
end

function del_all{K,V}(h::Dict{K,V})
    fill!(h.keys, _jl_secret_table_token)
    h.ndel = 0
    h.count = 0
    return h
end

function assign{K,V}(h::Dict{K,V}, v, key)
    key = convert(K,key)

    sz = length(h.keys)

    if h.ndel >= ((3*sz)>>2)
        rehash(h, sz)
    end

    iter = 0
    maxprobe = sz>>3
    index = hashindex(key, sz)
    orig = index
    avail = -1  # an available slot
    keys = h.keys; vals = h.vals

    while true
        hk = keys[index]
        if is(hk,_jl_secret_table_token)
            if avail<0
                keys[index] = key
                vals[index] = v
            else
                keys[avail] = key
                vals[avail] = v
            end
            h.count += 1
            return h
        end

        if is(hk,_jl_missing_token)
            if avail<0
                # found an available slot, but need to keep scanning
                # in case "key" already exists in a later collided slot.
                avail = index
            end
        elseif isequal(key, hk::K)
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
        keys[avail] = key
        vals[avail] = v
        h.count += 1
        return h
    end

    rehash(h, sz*2)

    assign(h, v, key)
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex{K,V}(h::Dict{K,V}, key)
    key = convert(K,key)

    sz = length(h.keys)
    iter = 0
    maxprobe = sz>>3
    index = hashindex(key, sz)
    orig = index
    keys = h.keys

    while true
        hk = keys[index]
        if is(hk,_jl_secret_table_token)
            break
        end
        if !is(hk,_jl_missing_token) && isequal(key,hk::K)
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

function ref{K,V}(h::Dict{K,V}, key)
    index = ht_keyindex(h, key)
    return (index<0) ? throw(KeyError(key)) : h.vals[index]::V
end

function get{K,V}(h::Dict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.vals[index]::V
end

has(h::Dict, key) = (ht_keyindex(h, key) >= 0)

function key{K,V}(h::Dict{K,V}, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.keys[index]::K
end

function del(h::Dict, key)
    index = ht_keyindex(h, key)
    if index > 0
        h.keys[index] = _jl_missing_token
        h.ndel += 1
        h.count -= 1
    end
    return h
end

function skip_deleted(keys, i)
    L = length(keys)
    while i<=L && (is(keys[i],_jl_secret_table_token) ||
                   is(keys[i],_jl_missing_token))
        i += 1
    end
    return i
end

start(t::Dict) = skip_deleted(t.keys, 1)
done(t::Dict, i) = done(t.vals, i)
next(t::Dict, i) = ((t.keys[i],t.vals[i]), skip_deleted(t.keys,i+1))

isempty(t::Dict) = (t.count == 0)
length(t::Dict) = t.count

# weak key dictionaries

function add_weak_key(t::Dict, k, v)
    if is(t.deleter, identity)
        t.deleter = x->del(t, x)
    end
    t[WeakRef(k)] = v
    # TODO: it might be better to avoid the finalizer, allow
    # wiped WeakRefs to remain in the table, and delete them as
    # they are discovered by ref and assign.
    finalizer(k, t.deleter)
    return t
end

function add_weak_value(t::Dict, k, v)
    t[k] = WeakRef(v)
    finalizer(v, x->del(t, k))
    return t
end

type WeakKeyDict{K,V} <: Associative{K,V}
    ht::Dict{Any,V}

    WeakKeyDict() = new(Dict{Any,V}())
end
WeakKeyDict() = WeakKeyDict{Any,Any}()

assign{K}(wkh::WeakKeyDict{K}, v, key) = add_weak_key(wkh.ht, convert(K,key), v)

function key{K}(wkh::WeakKeyDict{K}, kk, deflt)
    k = key(wkh.ht, convert(K,kk), _jl_secret_table_token)
    if is(k, _jl_secret_table_token)
        return deflt
    end
    return k.value::K
end

get{K}(wkh::WeakKeyDict{K}, key, deflt) = get(wkh.ht, convert(K,key), deflt)
del{K}(wkh::WeakKeyDict{K}, key) = del(wkh.ht, convert(K,key))
del_all(wkh::WeakKeyDict)  = (del_all(wkh.ht); wkh)
has{K}(wkh::WeakKeyDict{K}, key) = has(wkh.ht, convert(K,key))
ref{K}(wkh::WeakKeyDict{K}, key) = ref(wkh.ht, convert(K,key))
isempty(wkh::WeakKeyDict) = isempty(wkh.ht)

start(t::WeakKeyDict) = start(t.ht)
done(t::WeakKeyDict, i) = done(t.ht, i)
function next{K}(t::WeakKeyDict{K}, i)
    kv, i = next(t.ht, i)
    ((kv[1].value::K,kv[2]), i)
end
length(t::WeakKeyDict) = length(t.ht)
