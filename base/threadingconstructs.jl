# This file is a part of Julia. License is MIT: https://julialang.org/license

export threadid, nthreads, @threads, @spawn,
       threadpool, nthreadpools

"""
    Threads.threadid() -> Int

Get the ID number of the current thread of execution. The master thread has
ID `1`.
"""
threadid() = Int(ccall(:jl_threadid, Int16, ())+1)

"""
    Threads.nthreads([:default|:interactive]) -> Int

Get the number of threads (across all thread pools or within the specified
thread pool) available to Julia. The number of threads across all thread
pools is the inclusive upper bound on [`threadid()`](@ref).

See also: `BLAS.get_num_threads` and `BLAS.set_num_threads` in the
[`LinearAlgebra`](@ref man-linalg) standard library, and `nprocs()` in the
[`Distributed`](@ref man-distributed) standard library.
"""
function nthreads end

nthreads() = Int(unsafe_load(cglobal(:jl_n_threads, Cint)))
function nthreads(pool::Symbol)
    if pool === :default
        tpid = Int8(0)
    elseif pool === :interactive
        tpid = Int8(1)
    else
        error("invalid threadpool specified")
    end
    return _nthreads_in_pool(tpid)
end
function _nthreads_in_pool(tpid::Int8)
    p = unsafe_load(cglobal(:jl_n_threads_per_pool, Ptr{Cint}))
    return Int(unsafe_load(p, tpid + 1))
end

"""
    Threads.threadpool(tid = threadid()) -> Symbol

Returns the specified thread's threadpool; either `:default` or `:interactive`.
"""
function threadpool(tid = threadid())
    tpid = ccall(:jl_threadpoolid, Int8, (Int16,), tid-1)
    return tpid == 0 ? :default : :interactive
end

"""
    Threads.nthreadpools() -> Int

Returns the number of threadpools currently configured.
"""
nthreadpools() = Int(unsafe_load(cglobal(:jl_n_threadpools, Cint)))


function threading_run(fun, static)
    ccall(:jl_enter_threaded_region, Cvoid, ())
    n = nthreads()
    tasks = Vector{Task}(undef, n)
    for i = 1:n
        t = Task(() -> fun(i)) # pass in tid
        t.sticky = static
        static && ccall(:jl_set_task_tid, Cint, (Any, Cint), t, i-1)
        tasks[i] = t
        schedule(t)
    end
    for i = 1:n
        Base._wait(tasks[i])
    end
    ccall(:jl_exit_threaded_region, Cvoid, ())
    failed_tasks = filter(istaskfailed, tasks)
    if !isempty(failed_tasks)
        throw(CompositeException(map(TaskFailedException, failed_tasks)))
    end
end

function _threadsfor(iter, lbody, schedule)
    lidx = iter.args[1]         # index
    range = iter.args[2]
    quote
        local threadsfor_fun
        let range = $(esc(range))
        function threadsfor_fun(tid = 1; onethread = false)
            r = range # Load into local variable
            lenr = length(r)
            # divide loop iterations among threads
            if onethread
                tid = 1
                len, rem = lenr, 0
            else
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
            f = firstindex(r) + ((tid-1) * len)
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
                local $(esc(lidx)) = @inbounds r[i]
                $(esc(lbody))
            end
        end
        end
        if $(schedule === :dynamic || schedule === :default)
            threading_run(threadsfor_fun, false)
        elseif ccall(:jl_in_threaded_region, Cint, ()) != 0 # :static
            error("`@threads :static` cannot be used concurrently or nested")
        else # :static
            threading_run(threadsfor_fun, true)
        end
        nothing
    end
end

"""
    Threads.@threads [schedule] for ... end

A macro to execute a `for` loop in parallel. The iteration space is distributed to
coarse-grained tasks. This policy can be specified by the `schedule` argument. The
execution of the loop waits for the evaluation of all iterations.

See also: [`@spawn`](@ref Threads.@spawn) and
`pmap` in [`Distributed`](@ref man-distributed).

# Extended help

## Semantics

Unless stronger guarantees are specified by the scheduling option, the loop executed by
`@threads` macro have the following semantics.

The `@threads` macro executes the loop body in an unspecified order and potentially
concurrently. It does not specify the exact assignments of the tasks and the worker threads.
The assignments can be different for each execution. The loop body code (including any code
transitively called from it) must not make any assumptions about the distribution of
iterations to tasks or the worker thread in which they are executed. The loop body for each
iteration must be able to make forward progress independent of other iterations and be free
from data races. As such, invalid synchronizations across iterations may deadlock while
unsynchronized memory accesses may result in undefined behavior.

For example, the above conditions imply that:

- The lock taken in an iteration *must* be released within the same iteration.
- Communicating between iterations using blocking primitives like `Channel`s is incorrect.
- Write only to locations not shared across iterations (unless a lock or atomic operation is
  used).
- The value of [`threadid()`](@ref Threads.threadid) may change even within a single
  iteration.

## Schedulers

Without the scheduler argument, the exact scheduling is unspecified and varies across Julia
releases. Currently, `:dynamic` is used when the scheduler is not specified.

!!! compat "Julia 1.5"
    The `schedule` argument is available as of Julia 1.5.

### `:dynamic` (default)

`:dynamic` scheduler executes iterations dynamically to available worker threads. Current
implementation assumes that the workload for each iteration is uniform. However, this
assumption may be removed in the future.

This scheduling option is merely a hint to the underlying execution mechanism. However, a
few properties can be expected. The number of `Task`s used by `:dynamic` scheduler is
bounded by a small constant multiple of the number of available worker threads
([`nthreads()`](@ref Threads.nthreads)). Each task processes contiguous regions of the
iteration space. Thus, `@threads :dynamic for x in xs; f(x); end` is typically more
efficient than `@sync for x in xs; @spawn f(x); end` if `length(xs)` is significantly
larger than the number of the worker threads and the run-time of `f(x)` is relatively
smaller than the cost of spawning and synchronizing a task (typically less than 10
microseconds).

!!! compat "Julia 1.8"
    The `:dynamic` option for the `schedule` argument is available and the default as of Julia 1.8.

### `:static`

`:static` scheduler creates one task per thread and divides the iterations equally among
them, assigning each task specifically to each thread. In particular, the value of
[`threadid()`](@ref Threads.threadid) is guranteed to be constant within one iteration.
Specifying `:static` is an error if used from inside another `@threads` loop or from a
thread other than 1.

!!! note
    `:static` scheduling exists for supporting transition of code written before Julia 1.3.
    In newly written library functions, `:static` scheduling is discouraged because the
    functions using this option cannot be called from arbitrary worker threads.

## Example

To illustrate of the different scheduling strategies, consider the following function
`busywait` containing a non-yielding timed loop that runs for a given number of seconds.

```julia-repl
julia> function busywait(seconds)
            tstart = time_ns()
            while (time_ns() - tstart) / 1e9 < seconds
            end
        end

julia> @time begin
            Threads.@spawn busywait(5)
            Threads.@threads :static for i in 1:Threads.nthreads()
                busywait(1)
            end
        end
6.003001 seconds (16.33 k allocations: 899.255 KiB, 0.25% compilation time)

julia> @time begin
            Threads.@spawn busywait(5)
            Threads.@threads :dynamic for i in 1:Threads.nthreads()
                busywait(1)
            end
        end
2.012056 seconds (16.05 k allocations: 883.919 KiB, 0.66% compilation time)
```

The `:dynamic` example takes 2 seconds since one of the non-occupied threads is able
to run two of the 1-second iterations to complete the for loop.
"""
macro threads(args...)
    na = length(args)
    if na == 2
        sched, ex = args
        if sched isa QuoteNode
            sched = sched.value
        elseif sched isa Symbol
            # for now only allow quoted symbols
            sched = nothing
        end
        if sched !== :static && sched !== :dynamic
            throw(ArgumentError("unsupported schedule argument in @threads"))
        end
    elseif na == 1
        sched = :default
        ex = args[1]
    else
        throw(ArgumentError("wrong number of arguments in @threads"))
    end
    if !(isa(ex, Expr) && ex.head === :for)
        throw(ArgumentError("@threads requires a `for` loop expression"))
    end
    if !(ex.args[1] isa Expr && ex.args[1].head === :(=))
        throw(ArgumentError("nested outer loops are not currently supported by @threads"))
    end
    return _threadsfor(ex.args[1], ex.args[2], sched)
end

"""
    Threads.@spawn [:default|:interactive] expr

Create a [`Task`](@ref) and [`schedule`](@ref) it to run on any available
thread in the specified threadpool (`:default` if unspecified). The task is
allocated to a thread once one becomes available. To wait for the task to
finish, call [`wait`](@ref) on the result of this macro, or call
[`fetch`](@ref) to wait and then obtain its return value.

Values can be interpolated into `@spawn` via `\$`, which copies the value
directly into the constructed underlying closure. This allows you to insert
the _value_ of a variable, isolating the asynchronous code from changes to
the variable's value in the current task.

!!! note
    See the manual chapter on [multi-threading](@ref man-multithreading)
    for important caveats. See also the chapter on [threadpools](@ref man-threadpools).

!!! compat "Julia 1.3"
    This macro is available as of Julia 1.3.

!!! compat "Julia 1.4"
    Interpolating values via `\$` is available as of Julia 1.4.

!!! compat "Julia 1.9"
    A threadpool may be specified as of Julia 1.9.
"""
macro spawn(args...)
    tpid = Int8(0)
    na = length(args)
    if na == 2
        ttype, ex = args
        if ttype isa QuoteNode
            ttype = ttype.value
        elseif ttype isa Symbol
            # TODO: allow unquoted symbols
            ttype = nothing
        end
        if ttype === :interactive
            tpid = Int8(1)
        elseif ttype !== :default
            throw(ArgumentError("unsupported threadpool in @spawn: $ttype"))
        end
    elseif na == 1
        ex = args[1]
    else
        throw(ArgumentError("wrong number of arguments in @spawn"))
    end

    letargs = Base._lift_one_interp!(ex)

    thunk = esc(:(()->($ex)))
    var = esc(Base.sync_varname)
    quote
        let $(letargs...)
            local task = Task($thunk)
            task.sticky = false
            ccall(:jl_set_task_threadpoolid, Cint, (Any, Int8), task, $tpid)
            if $(Expr(:islocal, var))
                put!($var, task)
            end
            schedule(task)
            task
        end
    end
end

# This is a stub that can be overloaded for downstream structures like `Channel`
function foreach end

# Scheduling traits that can be employed for downstream overloads
abstract type AbstractSchedule end
struct StaticSchedule <: AbstractSchedule end
struct FairSchedule <: AbstractSchedule end
