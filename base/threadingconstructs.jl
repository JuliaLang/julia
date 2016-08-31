# This file is a part of Julia. License is MIT: http://julialang.org/license

export threadid, nthreads, @threads

"""
    Threads.threadid()

Get the ID number of the current thread of execution. The master thread has ID `1`.
"""
threadid() = Int(ccall(:jl_threadid, Int16, ())+1)

# Inclusive upper bound on threadid()
"""
    Threads.nthreads()

Get the number of threads available to the Julia process. This is the inclusive upper bound
on `threadid()`.
"""
nthreads() = Int(unsafe_load(cglobal(:jl_n_threads, Cint)))

function _threadsfor(iters,lbody)
    fun = gensym("_threadsfor")
    if iters.head == :(=)
        lidxs = Expr(:tuple, iters.args[1])
        ranges = Expr(:tuple, iters.args[2])
    else
        # note the `reverse` to put the last "lidx" first (since it varies
        # first) to match array ordering and make the code below clearer
        lidxs = Expr(:tuple, (iter.args[1] for iter in reverse(iters.args))...)
        ranges = Expr(:tuple, (iter.args[2] for iter in reverse(iters.args))...)
    end
    ndim = length(lidxs.args)
    # recursively get all symbols in the iteration variables, which might
    # look like ((i,j),k,..), so we can declare them local
    allsyms(ex) = (typeof(ex) == Symbol) ? [ex] : vcat(map(allsyms,ex.args)...)
    localvars = allsyms(lidxs)
    quote
        function $fun()
            # get dimensions of loops, N1 x N2 x N3....
            rs = $(esc(ranges))
            dims = map(length,rs)

            # split linear index of work, 1:N1*N2*N3*..., evenly among threads
            tid = threadid()
            q, r = divrem(*(dims...), nthreads())
            len = q + (tid <= r ? 1 : 0)
            if len == 0
                return
            end
            start = q*(tid-1) + min(tid-1,r) + 1

            # get N-d index (i1,i2,i3...) from linear index (and move back one
            # since the first thing we do in the for loop is increment)
            i_lidxs = [ind2sub(dims,start-1)...]

            for i=1:len
                # increment and carry N-d index (i1,i2,i3,...)
                i_lidxs[1] += 1
                for j=1:$ndim
                    if i_lidxs[j]>dims[j]
                        i_lidxs[j+1] += 1
                        i_lidxs[j] = 1
                    end
                end
                $(esc(Expr(:local,localvars...)))
                $(esc(lidxs)) = $(Expr(:tuple,(:(Base.unsafe_getindex(rs[$i],i_lidxs[$i])) for i=1:ndim)...))
                $(esc(lbody))
            end
        end
        ccall(:jl_threading_run, Void, (Any,), Core.svec($fun))
    end
end
"""
    Threads.@threads

A macro to parallelize a for-loop to run with multiple threads. This spawns `nthreads()`
number of threads, splits the iteration space amongst them, and iterates in parallel.
A barrier is placed at the end of the loop which waits for all the threads to finish
execution, and the loop returns.
"""
macro threads(args...)
    na = length(args)
    if na != 1
        throw(ArgumentError("wrong number of arguments in @threads"))
    end
    ex = args[1]
    if !isa(ex, Expr)
        throw(ArgumentError("need an expression argument to @threads"))
    end
    if is(ex.head, :for)
        return _threadsfor(ex.args[1],ex.args[2])
    else
        throw(ArgumentError("unrecognized argument to @threads"))
    end
end
