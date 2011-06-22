type IdTable
    ht::Array{Any,1}
end

function _tablesz(i::Int)
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

idtable(sz::Int) = IdTable(cell(2*_tablesz(sz)))
idtable() = idtable(0)

function assign(t::IdTable, v, k)
    t.ht = ccall(:jl_eqtable_put,
                 Any, (Any, Any, Any), t.ht, k, v)::Array{Any,1}
    t
end

get(t::IdTable, key, default) =
    ccall(:jl_eqtable_get, Any, (Any, Any, Any), t.ht, key, default)

del(t::IdTable, key) =
    (ccall(:jl_eqtable_del, Int32, (Any, Any), t.ht, key); t)

_secret_table_token_ = (:BOO,)

start(t::IdTable) = 0
done(t::IdTable, i) = is(next(t,i),())
next(t::IdTable, i) = ccall(:jl_eqtable_next, Any, (Any, Uint32), t.ht, uint32(i))

isempty(t::IdTable) = is(next(t,0),())

# hashing

bitmix(a::Union(Int32,Uint32), b::Union(Int32,Uint32)) =
    ccall(:int64to32hash, Uint32, (Uint64,),
          or_int(shl_int(zext64(unbox32(a)),unbox32(32)), zext64(unbox32(b))))

bitmix(a::Union(Int64,Uint64), b::Union(Int64, Uint64)) =
    ccall(:int64hash, Uint64, (Uint64,),
          xor_int(unbox64(a), or_int(lshr_int(unbox64(b),unbox32(32)),
                                     shl_int(unbox64(b),unbox32(32)))))

#hash(x::Union(Int32,Uint32,Char)) = ccall(:int32hash, Uint32, (Uint32,), uint32(x))
#hash(x::Union(Int64,Uint64)) = ccall(:int64hash, Uint64, (Uint64,), uint64(x))

hash_f64(x::Float64) =
    ccall(:int64hash, Uint64, (Uint64,), boxui64(unbox64(x)))

if WORD_SIZE == 64
    hash(s::Symbol) = ccall(:jl_hash_symbol, Uint64, (Any,), s)
else
    hash(s::Symbol) = ccall(:jl_hash_symbol, Uint32, (Any,), s)
end

hash(x::Float64) = (isnan(x) ? hash_f64(NaN) : hash_f64(x))
hash(x::Float) = hash(float64(x))
hash(x::Number) = hash_f64(float64(x))

function hash(t::Tuple)
    h = int64(0)
    for i=1:length(t)
        h = bitmix(h,hash(t[i]))
    end
    h
end

function hash(a::Array)
    h = hash(size(a))+1
    for i=1:length(a)
        h = bitmix(h,hash(a[i]))
    end
    h
end

hash(s::ByteString) = ccall(:memhash32, Uint32, (Ptr{Void}, Size), s.data, length(s.data))

# hash table

type HashTable{K,V}
    keys::Array{K,1}
    vals::Array{V,1}
    used::IntSet
    deleted::IntSet
    deleter::Function

    HashTable(n) = (n = _tablesz(n);
                    this.keys = Array(K,n);
                    this.vals = Array(V,n);
                    this.used = IntSet(n+1);
                    this.deleted = IntSet(n+1);
                    this.deleter = identity)
end
HashTable() = HashTable(0)
HashTable(n::Int) = HashTable{Any,Any}(n)

hashindex(key, sz) = (int32(hash(key)) & (sz-1)) + 1

function rehash{K,V}(h::HashTable{K,V}, newsz)
    oldk = h.keys
    oldv = h.vals
    oldu = h.used
    oldd = h.deleted
    newht = HashTable{K,V}(newsz)

    for i = oldu
        if !has(oldd,i)
            newht[oldk[i]] = oldv[i]
        end
    end

    h.keys = newht.keys
    h.vals = newht.vals
    h.used = newht.used
    h.deleted = newht.deleted
    h
end

function assign{K,V}(h::HashTable{K,V}, v, key)
    sz = length(h.keys)

    if numel(h.deleted) >= ((3*sz)>>2)
        rehash(h, sz)
    end

    iter = 0
    maxprobe = sz>>3
    index = hashindex(key, sz)
    orig = index

    while true
        if !has(h.used,index)
            h.keys[index] = key
            h.vals[index] = v
            add(h.used, index)
            return h
        end
        if has(h.deleted,index)
            h.keys[index] = key
            h.vals[index] = v
            del(h.deleted, index)
            return h
        end

        if isequal(key, h.keys[index])
            h.vals[index] = v
            return h
        end

        index = (index & (sz-1)) + 1
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    rehash(h, sz*2)

    assign(h, v, key)
end

# get the index where a key is stored, or -1 if not present
function ht_keyindex(h::HashTable, key)
    sz = length(h.keys)
    iter = 0
    maxprobe = sz>>3
    index = hashindex(key, sz)
    orig = index

    while true
        if !has(h.used,index)
            break
        end
        if !has(h.deleted,index) && isequal(key, h.keys[index])
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

function get(h::HashTable, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.vals[index]
end

function key(h::HashTable, key, deflt)
    index = ht_keyindex(h, key)
    return (index<0) ? deflt : h.keys[index]
end

function del(h::HashTable, key)
    index = ht_keyindex(h, key)
    if index > 0
        add(h.deleted, index)
    end
    h
end

function skip_deleted(used, deleted, i)
    while !done(used, i)
        (i, ip1) = next(used, i)
        if !has(deleted,i)
            break
        end
        i = ip1
    end
    return i
end

start(t::HashTable) = skip_deleted(t.used, t.deleted, 0)
done(t::HashTable, i) = done(t.used, i)
next(t::HashTable, i) = ((n, nxt) = next(t.used, i);
                         ((t.keys[n],t.vals[n]),
                          skip_deleted(t.used,t.deleted,nxt)))

isempty(t::HashTable) = done(t, start(t))

function ref(t::Union(IdTable,HashTable), key)
    v = get(t, key, _secret_table_token_)
    if is(v,_secret_table_token_)
        throw(KeyError(key))
    end
    return v
end

has(t::Union(IdTable,HashTable), key) =
    !is(get(t, key, _secret_table_token_),
        _secret_table_token_)

function add_weak_key(t::HashTable, k, v)
    if is(t.deleter, identity)
        t.deleter = x->del(t, x)
    end
    t[WeakRef(k)] = v
    finalizer(k, t.deleter)
    t
end

function add_weak_value(t::HashTable, k, v)
    t[k] = WeakRef(v)
    finalizer(v, x->del(t, k))
    t
end

function show(t::Union(IdTable,HashTable))
    if isempty(t)
        print(typeof(t).name.name,"()")
    else
        print("{")
        for (k, v) = t
            show(k)
            print("=>")
            show(v)
            print(",")
        end
        print("}")
    end
end

type WeakKeyHashTable{K,V}
    ht::HashTable{K,V}

    WeakKeyHashTable() = (this.ht = HashTable{K,V}(0))
end
WeakKeyHashTable() = WeakKeyHashTable{Any,Any}()

assign(wkh::WeakKeyHashTable, v, key) = add_weak_key(wkh.ht, key, v)

function key(wkh::WeakKeyHashTable, kk, deflt)
    k = key(wkh.ht, kk, _secret_table_token_)
    if is(k, _secret_table_token_)
        return deflt
    end
    return k.value
end

get(wkh::WeakKeyHashTable, key, deflt) = get(wkh.ht, key, deflt)
del(wkh::WeakKeyHashTable, key) = del(wkh.ht, key)
has(wkh::WeakKeyHashTable, key) = has(wkh.ht, key)
ref(wkh::WeakKeyHashTable, key) = ref(wkh.ht, key)
isempty(wkh::WeakKeyHashTable) = isempty(wkh.ht)

start(t::WeakKeyHashTable) = start(t.ht)
done(t::WeakKeyHashTable, i) = done(t.ht, i)
next(t::WeakKeyHashTable, i) = next(t.ht, i)
