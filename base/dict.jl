# generic operations on associative collections

abstract Associative{K,V}

const _jl_secret_table_token = :__c782dbf1cf4d6a2e5e3865d7e95634f2e09b5902__

has(t::Associative, key) = !is(get(t, key, _jl_secret_table_token),
                               _jl_secret_table_token)

function show(io, t::Associative)
    if isempty(t)
        print(io, typeof(t).name.name,"()")
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

function keys(T::Type, a::Associative)
    i = 0
    keyz = Array(T,length(a))
    for (k,v) in a
        keyz[i+=1] = k
    end
    return keyz
end
keys{K,V}(a::Associative{K,V}) = keys(K,a)

function values(T::Type, a::Associative)
    i = 0
    vals = Array(T,length(a))
    for (k,v) in a
        vals[i+=1] = v
    end
    return vals
end
values{K,V}(a::Associative{K,V}) = values(V,a)

function pairs(T::Union(Type,(Type,Type)), a::Associative)
    i = 0
    pairz = Array(T,length(a))
    for (k,v) in a
        pairz[i+=1] = (k,v)
    end
    return pairz
end
pairs{K,V}(a::Associative{K,V}) = pairs((K,V),a)

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

function assign(t::ObjectIdDict, v::ANY, k::ANY)
    t.ht = ccall(:jl_eqtable_put, Any, (Any, Any, Any), t.ht, k, v)::Array{Any,1}
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

# hashing

bitmix(a::Union(Int32,Uint32), b::Union(Int32,Uint32)) =
    ccall(:int64to32hash, Uint32, (Uint64,),
          or_int(shl_int(zext64(unbox32(a)), 32), zext64(unbox32(b))))

bitmix(a::Union(Int64,Uint64), b::Union(Int64, Uint64)) =
    ccall(:int64hash, Uint64, (Uint64,),
          xor_int(unbox64(a), or_int(lshr_int(unbox64(b), 32),
                                     shl_int(unbox64(b), 32))))

if WORD_SIZE == 64
    _jl_hash64(x::Union(Int64,Uint64,Float64)) =
        ccall(:int64hash, Uint64, (Uint64,), boxui64(unbox64(x)))
else
    _jl_hash64(x::Union(Int64,Uint64,Float64)) =
        ccall(:int64to32hash, Uint32, (Uint64,), boxui64(unbox64(x)))
end

hash(x::Integer) = _jl_hash64(uint64(x))
@eval function hash(x::Float)
    if trunc(x) == x
        # hash as integer if equal to some integer. note the result of
        # float to int conversion is only defined for in-range values.
        if x < 0
            if $float64(typemin(Int64)) <= x
                return hash(int64(x))
            end
        else
            # note: float64(typemax(Uint64)) == 2^64
            if x < $float64(typemax(Uint64))
                return hash(uint64(x))
            end
        end
    end
    isnan(x) ? $_jl_hash64(NaN) : _jl_hash64(float64(x))
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

hash(x::ANY) = uid(x)

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
    vals::Array{Any,1}
    ndel::Int
    deleter::Function

    Dict() = Dict{K,V}(0)
    function Dict(n::Integer)
        n = _tablesz(n)
        new(fill!(cell(n), _jl_secret_table_token),
            fill!(cell(n), _jl_secret_table_token),
            0, identity)
    end
    function Dict(ks::Tuple, vs::Tuple)
        n = length(ks)
        h = Dict{K,V}(n)
        for i=1:n
            h[ks[i]] = vs[i]
        end
        return h
    end
    global copy
    copy(d::Dict{K,V}) = new(copy(d.keys),copy(d.vals),d.ndel,d.deleter)
end
Dict() = Dict(0)
Dict(n::Integer) = Dict{Any,Any}(n)

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

function rehash{K,V}(h::Dict{K,V}, newsz)
    oldk = copy(h.keys)
    oldv = copy(h.vals)
    sz = length(oldk)
    newsz = _tablesz(newsz)
    if newsz > sz
        grow(h.keys, newsz-sz)
        grow(h.vals, newsz-sz)
    end
    del_all(h)

    for i = 1:length(oldv)
        v = oldv[i]
        if !is(v,_jl_secret_table_token)
            h[oldk[i]] = v
        end
    end

    return h
end

function del_all{K,V}(h::Dict{K,V})
    fill!(h.keys, _jl_secret_table_token)
    fill!(h.vals, _jl_secret_table_token)
    h.ndel = 0
    return h
end

function assign{K,V}(h::Dict{K,V}, v, key)
    key = convert(K,key)
    v = convert(V,v)

    sz = length(h.keys)

    if h.ndel >= ((3*sz)>>2)
        rehash(h, sz)
    end

    iter = 0
    maxprobe = sz>>3
    index = hashindex(key, sz)
    orig = index
    avail = -1  # an available slot

    while true
        hk = h.keys[index]
        if is(hk,_jl_secret_table_token)
            if avail<0
                h.keys[index] = key
                h.vals[index] = v
            else
                h.keys[avail] = key
                h.vals[avail] = v
            end
            return h
        end

        if isequal(key, hk::K)
            h.vals[index] = v
            return h
        end

        if is(h.vals[index],_jl_secret_table_token) && avail<0
            # found an available slot, but need to keep scanning
            # in case "key" already exists in a later collided slot.
            avail = index
        end

        index = (index & (sz-1)) + 1
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    if avail>0
        h.keys[avail] = key
        h.vals[avail] = v
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

    while true
        hk = h.keys[index]
        if is(hk,_jl_secret_table_token)
            break
        end
        if !is(h.vals[index],_jl_secret_table_token) && isequal(key,hk::K)
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

function get(h::Dict, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.vals[index]
end

function key(h::Dict, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.keys[index]
end

function del(h::Dict, key)
    index = ht_keyindex(h, key)
    if index > 0
        h.vals[index] = _jl_secret_table_token
        h.ndel += 1
    end
    return h
end

function skip_deleted(vals, i)
    L = length(vals)
    while i<=L && is(vals[i],_jl_secret_table_token)
        i += 1
    end
    return i
end

start(t::Dict) = skip_deleted(t.vals, 1)
done(t::Dict, i) = done(t.vals, i)
next(t::Dict, i) = ((t.keys[i],t.vals[i]), skip_deleted(t.vals,i+1))

isempty(t::Dict) = done(t, start(t))
function length(t::Dict)
    n = 0
    for v in t.vals
        n += int(!is(v,_jl_secret_table_token))
    end
    return n
end

function merge!(d::Dict, others::Dict...)
    for other in others
        for (k,v) in other
            d[k] = v
        end
    end
    return d
end
merge(d::Dict, others::Dict...) = merge!(copy(d), others...)

function filter!(f::Function, d::Dict)
    for (k,v) in d
        if !f(k,v)
            del(d,k)
        end
    end
    return d
end
filter(f::Function, d::Dict) = filter!(f,copy(d))

# weak key dictionaries

function add_weak_key(t::Dict, k, v)
    if is(t.deleter, identity)
        t.deleter = x->del(t, x)
    end
    t[WeakRef(k)] = v
    finalizer(k, t.deleter)
    return t
end

function add_weak_value(t::Dict, k, v)
    t[k] = WeakRef(v)
    finalizer(v, x->del(t, k))
    return t
end

type WeakKeyDict{K,V} <: Associative{K,V}
    ht::Dict{K,V}

    WeakKeyDict() = new(Dict{K,V}())
end
WeakKeyDict() = WeakKeyDict{Any,Any}()

assign(wkh::WeakKeyDict, v, key) = add_weak_key(wkh.ht, key, v)

function key(wkh::WeakKeyDict, kk, deflt)
    k = key(wkh.ht, kk, _jl_secret_table_token)
    if is(k, _jl_secret_table_token)
        return deflt
    end
    return k.value
end

get(wkh::WeakKeyDict, key, deflt) = get(wkh.ht, key, deflt)
del(wkh::WeakKeyDict, key) = del(wkh.ht, key)
del_all(wkh::WeakKeyDict)  = (del_all(wkh.ht); wkh)
has(wkh::WeakKeyDict, key) = has(wkh.ht, key)
ref(wkh::WeakKeyDict, key) = ref(wkh.ht, key)
isempty(wkh::WeakKeyDict) = isempty(wkh.ht)

start(t::WeakKeyDict) = start(t.ht)
done(t::WeakKeyDict, i) = done(t.ht, i)
next(t::WeakKeyDict, i) = next(t.ht, i)
