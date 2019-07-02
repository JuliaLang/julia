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

function _threadsfor(iter,lbody)
    lidx = iter.args[1]         # index
    range = iter.args[2]
    quote
        let range = $(esc(range))
        function threadsfor_fun(grain)
            r = range # Load into local variable
            lenr = length(r)
            # divide loop iterations among tasks
            ngrains = min(nthreads(), lenr)
            len, rem = divrem(lenr, ngrains)
            # not enough iterations for all the threads?
            if len == 0
                if grain > rem
                    return
                end
                len, rem = 1, 0
            end
            # compute this thread's iterations
            f = firstindex(r) + ((grain-1) * len)
            l = f + len - 1
            # distribute remaining iterations evenly
            if rem > 0
                if grain <= rem
                    f = f + (grain-1)
                    l = l + grain
                else
                    f = f + rem
                    l = l + rem
                end
            end
            # run this thread's iterations
            for i = f:l
                local $(esc(lidx)) = @inbounds r[i]
                $(esc(lbody))
            end
        end
        threading_run(threadsfor_fun, length(range))
        end
        nothing
    end
end

function threading_run(func, len)
    ngrains = min(nthreads(), len)
    tasks = Vector{Task}(undef, ngrains)
    for grain in 1:ngrains
        t = Task(()->func(grain))
        t.sticky = false
        tasks[grain] = t
        schedule(t)
    end
    Base.sync_end(tasks)
    return nothing
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
        return _threadsfor(ex.args[1], ex.args[2])
    else
        throw(ArgumentError("unrecognized argument to @threads"))
    end
end

"""
    Threads.@spawn expr

Create and run a [`Task`](@ref) on any available thread. To wait for the task to
finish, call [`wait`](@ref) on the result of this macro, or call [`fetch`](@ref)
to wait and then obtain its return value.

!!! note
    This feature is currently considered experimental.

!!! compat "Julia 1.3"
    This macro is available as of Julia 1.3.
"""
macro spawn(expr)
    thunk = esc(:(()->($expr)))
    var = esc(Base.sync_varname)
    quote
        local task = Task($thunk)
        task.sticky = false
        if $(Expr(:islocal, var))
            push!($var, task)
        end
        schedule(task)
        task
    end
end
