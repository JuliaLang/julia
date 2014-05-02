
function par_apply(f::Function, args::ANY, numthreads, start, step, len)
  ccall(:jl_par_apply,Void,(Any,Any,Int,Int,Int,Int),f,args,numthreads, start, step, len)
end

function thread(f::Function,args...; precomp=true)
  if precomp
    precompile(f, ntuple(length(args),d->typeof(args[d])) )
  end
  ccall(:jl_create_thread,Ptr{Void},(Any,Any),f,args)
end

join(t)=(ccall(:jl_join_thread,Void,(Ptr{Void},),t))

destroy(t)=(ccall(:jl_destroy_thread,Void,(Ptr{Void},),t))

function par_do_work(f::Function, args, start, step, len)
  for i=start:step:(start+(len-1)*step)
    f(args,i)
  end
end
  
function par_apply2(f::Function, args,  numthreads, start, step, len)
  gc_disable()

  # precompilation TODO: Don't compute the first time
  # f(args, start)
  par_do_work(f,args, start, 1, 1)

  t = Ptr{Void}[]

  N = len - 1
  chunk = ifloor(N / numthreads)
  rem = N

  for i=0:(numthreads-1)
    push!(t, Base.thread(par_do_work,f,args,start+1+i*chunk*step, step, chunk, precomp=true))
    rem -= chunk
  end
  push!(t, Base.thread(par_do_work,f,args,start+1+(numthreads-1)*chunk*step, step, rem, precomp=true))

  for i=1:numthreads
    Base.join(t[i])
    Base.destroy(t[i])
  end
 
  gc_enable()

end

