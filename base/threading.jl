### parallel apply function (scheduling in C).

function parapply(f::Function, args::ANY, numthreads, start, step, len)
  ccall(:jl_par_apply,Void,(Any,Any,Int,Int,Int,Int),f,args,numthreads, start, step, len)
end

### Thread type

type Thread
  handle::Ptr{Void}
end

join(t::Thread)=(ccall(:jl_join_thread,Void,(Ptr{Void},),t.handle))
run(t::Thread)=(ccall(:jl_run_thread,Void,(Ptr{Void},),t.handle))
destroy(t::Thread)=(ccall(:jl_destroy_thread,Void,(Ptr{Void},),t.handle))

function Thread(f::Function,args...)
  t=Thread(ccall(:jl_create_thread,Ptr{Void},(Any,Any),f,args))
    
  finalizer(t, destroy)
  t    
end

### Mutex type

type Mutex
  handle::Ptr{Void}
end

lock(m::Mutex)=(ccall(:jl_lock_mutex,Void,(Ptr{Void},),m.handle))
unlock(m::Mutex)=(ccall(:jl_unlock_mutex,Void,(Ptr{Void},),m.handle))
destroy(m::Mutex)=(ccall(:jl_destroy_mutex,Void,(Ptr{Void},),m.handle))

function Mutex()
  m=Mutex(ccall(:jl_create_mutex,Ptr{Void},()))

  finalizer(m, destroy)
  m
end

global_lock() = ccall(:jl_global_lock,Void,())
global_unlock() = ccall(:jl_global_unlock,Void,())


### parallel apply function (scheduling in julia).

function par_do_work(f::Function, args, start::Int, step::Int, len::Int)
  local i = start
  while i<=(start+(len-1)*step)
    f(args...,i)
    i += step
  end
end

function parapply_jl(f::Function, args,  numthreads::Int, start::Int, step::Int, len::Int; preapply::Bool = false)
  gc_disable()

  t = Array(Base.Thread,numthreads)

  if(preapply)
    par_do_work(f,args,start,1,1)
    start = start + 1
    len = len - 1
  end

  chunk = ifloor(len / numthreads)
  rem = len

  for i=0:(numthreads-2)
    t[i+1] = Base.Thread(par_do_work,f,args,int(start+i*chunk*step), step, chunk)
    rem -= chunk
  end
  t[numthreads] = Base.Thread(par_do_work,f,args,int(start+(numthreads-1)*chunk*step), step, rem)

  for i=1:numthreads
    Base.run(t[i])
  end

  for i=1:numthreads
    Base.join(t[i])
  end

  gc_enable()
end
