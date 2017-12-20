# This file is a part of Julia. License is MIT: https://julialang.org/license

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

# Only read/written by the main thread
const in_threaded_loop = Ref(false)

function _threadsfor(iter,lbody)
    lidx = iter.args[1]         # index
    range = iter.args[2]
    quote
        local threadsfor_fun
        let range = $(esc(range))
        function threadsfor_fun(onethread=false)
            r = range # Load into local variable
            lenr = length(r)
            # divide loop iterations among threads
            if onethread
                tid = 1
                len, rem = lenr, 0
            else
                tid = threadid()
                len, rem = divrem(lenr, nthreads())
            end
            # not enough iterations for all the threads?
            if len == 0
                if tid > rem
                    return
                end
                len, rem = 1, 0
            end
            # compute this thread's iterations
            f = 1 + ((tid-1) * len)
            l = f + len - 1
            # distribute remaining iterations evenly
            if rem > 0
                if tid <= rem
                    f = f + (tid-1)
                    l = l + tid
                else
                    f = f + rem
                    l = l + rem
                end
            end
            # run this thread's iterations
            for i = f:l
                local $(esc(lidx)) = Base.unsafe_getindex(r,i)
                $(esc(lbody))
            end
        end
        end
        # Hack to make nested threaded loops kinda work
        if threadid() != 1 || in_threaded_loop[]
            # We are in a nested threaded loop
            threadsfor_fun(true)
        else
            in_threaded_loop[] = true
            # the ccall is not expected to throw
            ccall(:jl_threading_run, Ref{Cvoid}, (Any,), threadsfor_fun)
            in_threaded_loop[] = false
        end
        nothing
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
    if ex.head === :for
        return _threadsfor(ex.args[1],ex.args[2])
    else
        throw(ArgumentError("unrecognized argument to @threads"))
    end
end
