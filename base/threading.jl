### parallel apply function (scheduling in C).

function parapply(f::Function, args::ANY, numthreads, r::Range)
  ccall(:jl_par_apply,Void,(Any,Any,Int,Int,Int,Int),f,args,numthreads, start(r), step(r), length(r))
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


### parallel apply function (scheduling in julia).

function par_do_work(f::Function, args, start::Int, step::Int, len::Int)
  local i = start
  while i<=(start+(len-1)*step)
    f(args...,i)
    i += step
  end
end

function parapply_jl(f::Function, args,  numthreads::Int, r::Range; preapply::Bool = false)
  gc_disable()

  st = start(r)
  len = length(r)
  s = step(r)

  t = Array(Base.Thread,numthreads)

  if(preapply)
    par_do_work(f,args,st,1,1)
    st = st + 1
    len = len - 1
  end

  chunk = ifloor(len / numthreads)
  rem = len

  for i=0:(numthreads-2)
    t[i+1] = Base.Thread(par_do_work,f,args,int(st+i*chunk*s), s, chunk)
    rem -= chunk
  end
  t[numthreads] = Base.Thread(par_do_work,f,args,int(st+(numthreads-1)*chunk*s), s, rem)

  for i=1:numthreads
    Base.run(t[i])
  end

  for i=1:numthreads
    Base.join(t[i])
  end

  gc_enable()
end
