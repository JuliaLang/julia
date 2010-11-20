struct IdTable
    ht::Array{Any,1}
end

function _tablesz(i::Int)
    if i < 32
        return 32
    end
    while (i&(i-1) != 0)
        i = i&(i-1)
    end
    return i<<1
end

idtable(sz::Int) = IdTable(cell(_tablesz(sz)))
idtable() = idtable(0)

function assign(t::IdTable, v, k)
    t.ht = ccall(dlsym(JuliaDLHandle,"jl_eqtable_put"),
                 Any, (Any, Any, Any), t.ht, k, v)::Array{Any,1}
    t
end

get(t::IdTable, key, default) = ccall(dlsym(JuliaDLHandle,"jl_eqtable_get"),
                                      Any, (Any, Any, Any),
                                      t.ht, key, default)

del(t::IdTable, key) = (ccall(dlsym(JuliaDLHandle,"jl_eqtable_del"),
                              Int32, (Any, Any), t.ht, key);
                        t)

_secret_table_token_ = {`BOO}

start(t::IdTable) = 0
done(t::IdTable, i) = is(next(t,i),())
next(t::IdTable, i) = ccall(dlsym(JuliaDLHandle,"jl_eqtable_next"),
                            Any, (Any, Uint32),
                            t.ht, uint32(i))

isempty(t::IdTable) = is(next(t,0),())

# hashing

bitmix(a::Union(Int32,Uint32), b::Union(Int32,Uint32)) =
    ccall(dlsym(JuliaDLHandle,"int64to32hash"), Uint32, (Uint64,),
          or_int(shl_int(zext64(unbox32(a)),unbox32(32)), zext64(unbox32(b))))

bitmix(a::Union(Int64,Uint64), b::Union(Int64, Uint64)) =
    ccall(dlsym(JuliaDLHandle,"int64hash"), Uint64, (Uint64,),
          xor_int(unbox64(a), or_int(lshr_int(unbox64(b),unbox32(32)),
                                     shl_int(unbox64(b),unbox32(32)))))

#hash(x::Union(Int32,Uint32,Char)) =
#    ccall(dlsym(JuliaDLHandle,"int32hash"), Uint32, (Uint32,), uint32(x))
#hash(x::Union(Int64,Uint64)) =
#    ccall(dlsym(JuliaDLHandle,"int64hash"), Uint64, (Uint64,), uint64(x))

hash_f64(x::Float64) =
    ccall(dlsym(JuliaDLHandle,"int64hash"), Uint64, (Uint64,),
          boxui64(unbox64(x)))

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

# TODO: should we distinguish a UTF8String and
# a Latin1String containing the same exact data?

hash(s::Union(UTF8String,Latin1String)) =
    ccall(dlsym(JuliaDLHandle,"memhash32"), Uint32,
          (Ptr{Void}, Size), s.data, length(s.data))

# hash table

struct HashTable{K,V}
    keys::Array{K,1}
    vals::Array{V,1}
    used::IntSet

    HashTable() = HashTable(Any,Any)
    HashTable(k, v) = HashTable(k, v, 16)
    HashTable(k, v, n) = (n = _tablesz(n);
                          new(Array(k,n), Array(v,n), IntSet(n)))
end

function assign{K,V}(h::HashTable{K,V}, v, key)
    hv = int32(hash(key))
    sz = length(h.keys)
    iter = 0
    maxprobe = sz>>3
    index = hv & (sz-1)
    orig = index

    while true
        if !contains(h.used,index)
            h.keys[index] = key
            h.vals[index] = v
            adjoin(h.used, index)
            return h
        end

        if isequal(key, h.keys[index])
            h.vals[index] = v
            return h
        end

        index = (index+1) & (sz-1)
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    oldk = h.keys
    oldv = h.vals
    oldu = h.used
    newsz = sz*2
    h.keys = Array(K,newsz)
    h.vals = Array(V,newsz)
    h.used = IntSet(newsz)

    for i = oldu
        h[oldk[i]] = oldv[i]
    end

    assign(h, key, v)
end

function get(h::HashTable, key, deflt)
    hv = int32(hash(key))
    sz = length(h.keys)
    iter = 0
    maxprobe = sz>>3
    index = hv & (sz-1)
    orig = index

    while true
        if !contains(h.used,index)
            break
        end
        if isequal(key, h.keys[index])
            return h.vals[index]
        end

        index = (index+1) & (sz-1)
        iter+=1
        if iter > maxprobe || index==orig
            break
        end
    end

    return deflt
end

start(t::HashTable) = 0
done(t::HashTable, i) = done(t.used, i)
next(t::HashTable, i) = ((n, nxt) = next(t.used, i);
                         ((t.keys[n],t.vals[n]), nxt))

isempty(t::HashTable) = done(t, start(t))

function ref(t::Union(IdTable,HashTable), key)
    v = get(t, key, _secret_table_token_)
    if is(v,_secret_table_token_)
        error("key not found")
    end
    return v
end

has(t::Union(IdTable,HashTable), key) =
    !is(get(t, key, _secret_table_token_),
        _secret_table_token_)

function show(t::Union(IdTable,HashTable))
    if isempty(t)
        print(typeof(t).name.name,"()")
    else
        print("{")
        for (k, v) = t
            show(k)
            print("=>")
            show(v)
            print(", ")
        end
        print("}")
    end
end
