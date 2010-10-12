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

_secret_idtable_token_ = {`BOO}

function ref(t::IdTable, key)
    v = get(t, key, _secret_idtable_token_)
    if is(v,_secret_idtable_token_)
        error("key not found")
    end
    return v
end

has(t::IdTable, key) = !is(get(t, key, _secret_idtable_token_),
                           _secret_idtable_token_)

start(t::IdTable) = 0
done(t::IdTable, i) = is(next(t,i),())
next(t::IdTable, i) = ccall(dlsym(JuliaDLHandle,"jl_eqtable_next"),
                            Any, (Any, Uint32),
                            t.ht, uint32(i))

isempty(t::IdTable) = is(next(t,0),())

function show(t::IdTable)
    if isempty(t)
        print("idtable()")
        return ()
    end
    print("{")
    for (k, v) = t
        show(k)
        print("=>")
        show(v)
        print(", ")
    end
    print("}")
end

# hashing

bitmix(a::Union(Int32,Uint32), b::Union(Int32,Uint32)) =
    ccall(dlsym(JuliaDLHandle,"int64to32hash"), Uint32, (Uint64,),
          or_int(shl_int(zext64(unbox32(a)),unbox32(32)), zext64(unbox32(b))))

bitmix(a::Union(Int64,Uint64), b::Union(Int64, Uint64)) =
    ccall(dlsym(JuliaDLHandle,"int64hash"), Uint64, (Uint64,),
          xor_int(unbox64(a), or_int(lshr_int(unbox64(b),unbox32(32)),
                                     shl_int(unbox64(b),unbox32(32)))))
