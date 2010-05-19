struct IdTable
    htptr::Ptr{Void}
end

idtable(sz::Int) = IdTable(ccall(dlsym(JuliaDLHandle,"jl_new_eqtable"),
                                 Ptr{Void}, (Uint32,), uint32(sz)))
idtable() = idtable(0)

set(t::IdTable, v, k) = (ccall(dlsym(JuliaDLHandle,"jl_eqtable_put"),
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

start(t::IdTable) = 0
done(t::IdTable, i) = is(next(t,i),())
next(t::IdTable, i) = ccall(dlsym(JuliaDLHandle,"jl_eqtable_next"),
                            Any, (Ptr{Void}, Uint32),
                            t.htptr, uint32(i))

isempty(t::IdTable) = is(next(t,0),())

function print(t::IdTable)
    if isempty(t)
        print("idtable()")
        return ()
    end
    print("{")
    for (k, v) = t
        print(k,"=>",v,", ")
    end
    print("}")
end
