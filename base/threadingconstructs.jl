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
        lidxs = Expr(:tuple, (iter.args[1] for iter in iters.args)...)
        ranges = Expr(:tuple, (iter.args[2] for iter in iters.args)...)
    end
    ndim = length(lidxs.args)
    quote
        function $fun()
            tid = threadid()
            rs = $(esc(ranges))
            dims = map(length,rs)
            # convert a "flattened" 1-d index into the N-tuple index into an N-d array
            ndi(i,dims::NTuple{1,Integer}) = i
            function ndi{N}(i,dims::NTuple{N,Integer})
                d,r = divrem(i-1,dims[1])
                tuple(r+1,ndi(d+1,dims[2:end])...)
            end
            # each thread does every `nthreads`-th flattened index
            for i = tid:nthreads():*(dims...)
                $(Expr(:tuple,(Symbol("i_$i") for i=1:ndim)...)) = ndi(i,dims)
                $(esc(lidxs)) = $(Expr(:tuple,(:(Base.unsafe_getindex(rs[$i],$(Symbol("i_$i")))) for i=1:ndim)...))
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
