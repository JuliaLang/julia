
### Thread type

type Thread
    handle::Ptr{Void}
end

join(t::Thread) = (ccall(:jl_join_thread,Void,(Ptr{Void},),t.handle))
run(t::Thread) = (ccall(:jl_run_thread,Void,(Ptr{Void},),t.handle))
destroy(t::Thread) = (ccall(:jl_destroy_thread,Void,(Ptr{Void},),t.handle))

function Thread(f::Function,args...)
    t = Thread(ccall(:jl_create_thread,Ptr{Void},(Any,Any),f,args))
    finalizer(t, destroy)
    t    
end

### Mutex type

type Mutex
    handle::Ptr{Void}
end

lock(m::Mutex) = (ccall(:jl_lock_mutex,Void,(Ptr{Void},),m.handle))
unlock(m::Mutex) = (ccall(:jl_unlock_mutex,Void,(Ptr{Void},),m.handle))
destroy(m::Mutex) = (ccall(:jl_destroy_mutex,Void,(Ptr{Void},),m.handle))

function Mutex()
    m = Mutex(ccall(:jl_create_mutex,Ptr{Void},()))
    finalizer(m, destroy)
    m
end


### parallel apply function (scheduling in julia).

function parapply(f::Function, r::UnitRange, args...; preapply::Bool = false, numthreads::Int = CPU_CORES)

    st = start(r)
    len = length(r)

    t = Array(Base.Thread,numthreads)

    if(preapply)
        f(range(st, 1), args...)
        st = st + 1
        len = len - 1
    end

    chunk = ifloor(len / numthreads)
    rem = len

    gc_disable()
    for i=0:(numthreads-2)
        t[i+1] = Base.Thread(f,range(int(st+i*chunk), chunk), args...)
        rem -= chunk
    end
    t[numthreads] = Base.Thread(f,range(int(st+(numthreads-1)*chunk),  rem), args...)  

    for i=1:numthreads
        Base.run(t[i])
    end

    for i=1:numthreads
        Base.join(t[i])
    end

    gc_enable()
    #gc()
end
