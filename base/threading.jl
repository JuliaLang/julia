#=
This file contains experimental threading support for Julia. The following rules 
should be taken into account when using the threading functions.

- The function Base.parapply is the best way to use multiple threads. It takes as argument
a function of the form f(r::UnitRange, args...) that operates over a certain range r. parapply
gets as second argument the full range over that f should be executed wheras internally the 
range will be split into chunks that are executed in different threads. The variables f is 
supposed to work on are passes as varargs after the range argument. parapply has two keyword arguments:
preapply is used to execute the function f once on the main thread. This is very useful for finding
syntax errors that would otherwise crash the Julia process. The other keyword argument is numthreads
that can be used to control the number of threads used.

- Base.parapply does switch the garbage collector of. Thus one should not allocate more memory in the
threads than absolutely needed. It is much better to preallocate it and pass it to the threads.

- Do not use print statements in the threading functions (Hopefully we can fix this)

- When locking is required there is a Mutex type.

- Running threads in an @async task is probably a very bad idea (Fixme)

- There is a low level Thread type. When using this the main thread should directly join it so that the main
thread is waiting until the worker threads are done. Further one should switch the garbage collector off before
running the threads.
=#


### Thread type

type Thread
    handle::Ptr{Void}
end

function join(t::Thread)
    ccall(:jl_join_thread,Void,(Ptr{Void},),t.handle)
    e = exception(t)
    if e != nothing
        throw(e) 
    end
end
run(t::Thread) = (ccall(:jl_run_thread,Void,(Ptr{Void},),t.handle))
destroy(t::Thread) = (ccall(:jl_destroy_thread,Void,(Ptr{Void},),t.handle))
exception(t::Thread) = ccall(:jl_thread_exception,Exception,(Ptr{Void},),t.handle)

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

function parapply(f::Function, r::UnitRange, args...; preapply::Bool = true, numthreads::Int = CPU_CORES)

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
