struct IdTable
    htptr::Ptr{Void}
end

idtable(sz::Int) = IdTable(ccall(dlsym(JuliaDLHandle,"jl_new_eqtable"),
                                 Ptr{Void}, (Uint32,), uint32(sz)))
idtable() = idtable(0)

assign(t::IdTable, v, k) = (ccall(dlsym(JuliaDLHandle,"jl_eqtable_put"),
                                  Void, (Ptr{Void}, Any, Any),
                                  t.htptr, k, v);
                            t)

get(t::IdTable, key, default) = ccall(dlsym(JuliaDLHandle,"jl_eqtable_get"),
                                      Any, (Ptr{Void}, Any, Any),
                                      t.htptr, key, default)

del(t::IdTable, key) = (ccall(dlsym(JuliaDLHandle,"jl_eqtable_del"),
                              Void, (Ptr{Void}, Any), t.htptr, key);
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
                            Any, (Ptr{Void}, Uint32),
                            t.htptr, uint32(i))

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
