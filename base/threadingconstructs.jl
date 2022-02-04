# This file is a part of Julia. License is MIT: https://julialang.org/license

export threadid, nthreads, @threads, @spawn

"""
    Threads.threadid()

Get the ID number of the current thread of execution. The master thread has ID `1`.
"""
threadid() = Int(ccall(:jl_threadid, Int16, ())+1)

# Inclusive upper bound on threadid()
"""
    Threads.nthreads()

Get the number of threads available to the Julia process. This is the inclusive upper bound
on [`threadid()`](@ref).

See also: `BLAS.get_num_threads` and `BLAS.set_num_threads` in the
[`LinearAlgebra`](@ref man-linalg) standard library, and `nprocs()` in the
[`Distributed`](@ref man-distributed) standard library.
"""
nthreads() = Int(unsafe_load(cglobal(:jl_n_threads, Cint)))

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
    try
        for i = 1:n
            wait(tasks[i])
        end
    finally
        ccall(:jl_exit_threaded_region, Cvoid, ())
    end
end

function _threadsfor(iter, lbody, schedule)
    lidx = iter.args[1]         # index
    range = iter.args[2]
    quote
        local threadsfor_fun
        let range = $(esc(range))
        function threadsfor_fun(tid=1; onethread=false)
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
        if $(schedule === :dynamic)
            threading_run(threadsfor_fun, false)
        elseif ccall(:jl_in_threaded_region, Cint, ()) != 0
            $(if schedule === :static
              :(error("`@threads :static` cannot be used concurrently or nested"))
              else
              # only use threads when called from outside @threads
              :(threadsfor_fun(onethread = true))
              end)
        else
            threading_run(threadsfor_fun, true)
        end
        nothing
    end
end

"""
    Threads.@threads [schedule] for ... end

A macro to parallelize a `for` loop to run with multiple threads. Splits the iteration
space among multiple tasks and runs those tasks on threads according to a scheduling
policy.
A barrier is placed at the end of the loop which waits for all tasks to finish
execution.

The `schedule` argument can be used to request a particular scheduling policy.

Except for `:static` scheduling, how the iterations are assigned to tasks, and how the tasks
are assigned to the worker threads is undefined. The exact assignments can be different
for each execution. The scheduling option is a hint. The loop body code (including any code
transitively called from it) must not make assumptions about the distribution of iterations
to tasks or the worker thread in which they are executed. The loop body for each iteration
must be able to make forward progress independent of other iterations and be free from data
races. As such, synchronizations across iterations may deadlock.

For example, the above conditions imply that:

- The lock taken in an iteration *must* be released within the same iteration.
- Communicating between iterations using blocking primitives like `Channel`s is incorrect.
- Write only to locations not shared across iterations (unless a lock or atomic operation is used).


Schedule options are:
- `:static` creates one task per thread and divides the iterations equally among
            them, assigning each task specifically to each thread.
            Specifying `:static` is an error if used from inside another `@threads` loop
            or from a thread other than 1.
- `:dynamic` will schedule iterations dynamically to available worker threads,
            assuming that the workload for each iteration is uniform.

Without the scheduler argument, the exact scheduling is unspecified; i.e. it may be
different across Julia releases. Currently, the behavior is dependent on the calling thread.
When called from thread 1 the default is `:static`. When called from other threads the loop
will be executed without threading.

The default schedule (used when no `schedule` argument is present) is subject to change.

For example, an illustration of the different scheduling strategies where `busywait`
is a non-yielding timed loop that runs for a number of seconds.

```julia-repl
julia> function busywait(seconds)
            tstart = Base.time()
            while Base.time() - tstart < seconds
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

!!! compat "Julia 1.5"
    The `schedule` argument is available as of Julia 1.5.

!!! compat "Julia 1.8"
    The `:dynamic` option for the `schedule` argument is available as of Julia 1.8.

See also: [`@spawn`](@ref Threads.@spawn), [`nthreads()`](@ref Threads.nthreads),
[`threadid()`](@ref Threads.threadid), `pmap` in [`Distributed`](@ref man-distributed),
`BLAS.set_num_threads` in [`LinearAlgebra`](@ref man-linalg).
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
    Threads.@spawn expr

Create a [`Task`](@ref) and [`schedule`](@ref) it to run on any available thread.
The task is allocated to a thread after it becomes available. To wait for the task
to finish, call [`wait`](@ref) on the result of this macro, or call [`fetch`](@ref) to
wait and then obtain its return value.

Values can be interpolated into `@spawn` via `\$`, which copies the value directly into the
constructed underlying closure. This allows you to insert the _value_ of a variable,
isolating the asynchronous code from changes to the variable's value in the current task.

!!! note
    See the manual chapter on threading for important caveats.

!!! compat "Julia 1.3"
    This macro is available as of Julia 1.3.

!!! compat "Julia 1.4"
    Interpolating values via `\$` is available as of Julia 1.4.
"""
macro spawn(expr)
    letargs = Base._lift_one_interp!(expr)

    thunk = esc(:(()->($expr)))
    var = esc(Base.sync_varname)
    quote
        let $(letargs...)
            local task = Task($thunk)
            task.sticky = false
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
