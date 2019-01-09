# This file is a part of Julia. License is MIT: https://julialang.org/license

## basic task functions and TLS

# Container for a captured exception and its backtrace. Can be serialized.
struct CapturedException <: Exception
    ex::Any
    processed_bt::Vector{Any}

    function CapturedException(ex, bt_raw::Vector)
        # bt_raw MUST be a vector that can be processed by StackTraces.stacktrace
        # Typically the result of a catch_backtrace()

        # Process bt_raw so that it can be safely serialized
        bt_lines = process_backtrace(bt_raw, 100) # Limiting this to 100 lines.
        CapturedException(ex, bt_lines)
    end

    CapturedException(ex, processed_bt::Vector{Any}) = new(ex, processed_bt)
end

function showerror(io::IO, ce::CapturedException)
    showerror(io, ce.ex, ce.processed_bt, backtrace=true)
end

"""
    CompositeException

Wrap a `Vector` of exceptions thrown by a [`Task`](@ref) (e.g. generated from a remote worker over a channel
or an asynchronously executing local I/O write or a remote worker under `pmap`) with information about the series of exceptions.
For example, if a group of workers are executing several tasks, and multiple workers fail, the resulting `CompositeException` will
contain a "bundle" of information from each worker indicating where and why the exception(s) occurred.
"""
struct CompositeException <: Exception
    exceptions::Vector{Any}
    CompositeException() = new(Any[])
    CompositeException(exceptions) = new(exceptions)
end
length(c::CompositeException) = length(c.exceptions)
push!(c::CompositeException, ex) = push!(c.exceptions, ex)
isempty(c::CompositeException) = isempty(c.exceptions)
iterate(c::CompositeException, state...) = iterate(c.exceptions, state...)
eltype(::Type{CompositeException}) = Any

function showerror(io::IO, ex::CompositeException)
    if !isempty(ex)
        showerror(io, ex.exceptions[1])
        remaining = length(ex) - 1
        if remaining > 0
            print(io, string("\n\n...and ", remaining, " more exception(s).\n"))
        end
    else
        print(io, "CompositeException()\n")
    end
end

function show(io::IO, t::Task)
    print(io, "Task ($(t.state)) @0x$(string(convert(UInt, pointer_from_objref(t)), base = 16, pad = Sys.WORD_SIZE>>2))")
end

"""
    @task

Wrap an expression in a [`Task`](@ref) without executing it, and return the [`Task`](@ref). This only
creates a task, and does not run it.

# Examples
```jldoctest
julia> a1() = sum(i for i in 1:1000);

julia> b = @task a1();

julia> istaskstarted(b)
false

julia> schedule(b);

julia> yield();

julia> istaskdone(b)
true
```
"""
macro task(ex)
    :(Task(()->$(esc(ex))))
end

"""
    current_task()

Get the currently running [`Task`](@ref).
"""
current_task() = ccall(:jl_get_current_task, Ref{Task}, ())

"""
    istaskdone(t::Task) -> Bool

Determine whether a task has exited.

# Examples
```jldoctest
julia> a2() = sum(i for i in 1:1000);

julia> b = Task(a2);

julia> istaskdone(b)
false

julia> schedule(b);

julia> yield();

julia> istaskdone(b)
true
```
"""
istaskdone(t::Task) = ((t.state == :done) | istaskfailed(t))

"""
    istaskstarted(t::Task) -> Bool

Determine whether a task has started executing.

# Examples
```jldoctest
julia> a3() = sum(i for i in 1:1000);

julia> b = Task(a3);

julia> istaskstarted(b)
false
```
"""
istaskstarted(t::Task) = ccall(:jl_is_task_started, Cint, (Any,), t) != 0

istaskfailed(t::Task) = (t.state == :failed)

task_result(t::Task) = t.result

task_local_storage() = get_task_tls(current_task())
function get_task_tls(t::Task)
    if t.storage === nothing
        t.storage = IdDict()
    end
    (t.storage)::IdDict{Any,Any}
end

"""
    task_local_storage(key)

Look up the value of a key in the current task's task-local storage.
"""
task_local_storage(key) = task_local_storage()[key]

"""
    task_local_storage(key, value)

Assign a value to a key in the current task's task-local storage.
"""
task_local_storage(key, val) = (task_local_storage()[key] = val)

"""
    task_local_storage(body, key, value)

Call the function `body` with a modified task-local storage, in which `value` is assigned to
`key`; the previous value of `key`, or lack thereof, is restored afterwards. Useful
for emulating dynamic scoping.
"""
function task_local_storage(body::Function, key, val)
    tls = task_local_storage()
    hadkey = haskey(tls,key)
    old = get(tls,key,nothing)
    tls[key] = val
    try body()
    finally
        hadkey ? (tls[key] = old) : delete!(tls,key)
    end
end

# NOTE: you can only wait for scheduled tasks
function wait(t::Task)
    if !istaskdone(t)
        if t.donenotify === nothing
            t.donenotify = Condition()
        end
    end
    while !istaskdone(t)
        wait(t.donenotify)
    end
    if istaskfailed(t)
        throw(t.exception)
    end
end

"""
    fetch(t::Task)

Wait for a Task to finish, then return its result value. If the task fails with an
exception, the exception is propagated (re-thrown in the task that called fetch).
"""
function fetch(t::Task)
    wait(t)
    task_result(t)
end


## lexically-scoped waiting for multiple items

function sync_end(refs)
    c_ex = CompositeException()
    for r in refs
        try
            wait(r)
        catch
            if !isa(r, Task) || (isa(r, Task) && !istaskfailed(r))
                rethrow()
            end
        finally
            if isa(r, Task) && istaskfailed(r)
                push!(c_ex, CapturedException(task_result(r), r.backtrace))
            end
        end
    end

    if !isempty(c_ex)
        throw(c_ex)
    end
    nothing
end

const sync_varname = gensym(:sync)

"""
    @sync

Wait until all lexically-enclosed uses of `@async`, `@spawn`, `@spawnat` and `@distributed`
are complete. All exceptions thrown by enclosed async operations are collected and thrown as
a `CompositeException`.
"""
macro sync(block)
    var = esc(sync_varname)
    quote
        let $var = Any[]
            v = $(esc(block))
            sync_end($var)
            v
        end
    end
end

# schedule an expression to run asynchronously

"""
    @async

Wrap an expression in a [`Task`](@ref) and add it to the local machine's scheduler queue.
"""
macro async(expr)
    thunk = esc(:(()->($expr)))
    var = esc(sync_varname)
    quote
        local task = Task($thunk)
        if $(Expr(:isdefined, var))
            push!($var, task)
        end
        schedule(task)
    end
end


function register_taskdone_hook(t::Task, hook)
    tls = get_task_tls(t)
    push!(get!(tls, :TASKDONE_HOOKS, []), hook)
    t
end

# runtime system hook called when a task finishes
function task_done_hook(t::Task)
    # `finish_task` sets `sigatomic` before entering this function
    err = istaskfailed(t)
    result = task_result(t)
    handled = false
    if err
        t.backtrace = catch_backtrace()
    end

    if isa(t.donenotify, Condition) && !isempty(t.donenotify.waitq)
        handled = true
        notify(t.donenotify, result, true, err)
    end

    # Execute any other hooks registered in the TLS
    if isa(t.storage, IdDict) && haskey(t.storage, :TASKDONE_HOOKS)
        foreach(hook -> hook(t), t.storage[:TASKDONE_HOOKS])
        delete!(t.storage, :TASKDONE_HOOKS)
        handled = true
    end

    if err && !handled
        if isa(result,InterruptException) && isdefined(Base,:active_repl_backend) &&
            active_repl_backend.backend_task.state == :runnable && isempty(Workqueue) &&
            active_repl_backend.in_eval
            throwto(active_repl_backend.backend_task, result) # this terminates the task
        end
    end
    # Clear sigatomic before waiting
    sigatomic_end()
    try
        wait() # this will not return
    catch e
        # If an InterruptException happens while blocked in the event loop, try handing
        # the exception to the REPL task since the current task is done.
        # issue #19467
        if isa(e,InterruptException) && isdefined(Base,:active_repl_backend) &&
            active_repl_backend.backend_task.state == :runnable && isempty(Workqueue) &&
            active_repl_backend.in_eval
            throwto(active_repl_backend.backend_task, e)
        else
            rethrow()
        end
    end
end

"""
    timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

Waits until `testcb` returns `true` or for `secs` seconds, whichever is earlier.
`testcb` is polled every `pollint` seconds.
"""
function timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)
    pollint > 0 || throw(ArgumentError("cannot set pollint to $pollint seconds"))
    start = time()
    done = Channel(1)
    timercb(aw) = begin
        try
            if testcb()
                put!(done, :ok)
            elseif (time() - start) > secs
                put!(done, :timed_out)
            end
        catch e
            put!(done, :error)
        finally
            isready(done) && close(aw)
        end
    end

    if !testcb()
        t = Timer(timercb, pollint, interval = pollint)
        ret = fetch(done)
        close(t)
    else
        ret = :ok
    end
    ret
end


## scheduler and work queue

global const Workqueue = Task[]

function enq_work(t::Task)
    t.state == :runnable || error("schedule: Task not runnable")
    ccall(:uv_stop, Cvoid, (Ptr{Cvoid},), eventloop())
    push!(Workqueue, t)
    t.state = :queued
    return t
end

schedule(t::Task) = enq_work(t)

"""
    schedule(t::Task, [val]; error=false)

Add a [`Task`](@ref) to the scheduler's queue. This causes the task to run constantly when the system
is otherwise idle, unless the task performs a blocking operation such as [`wait`](@ref).

If a second argument `val` is provided, it will be passed to the task (via the return value of
[`yieldto`](@ref)) when it runs again. If `error` is `true`, the value is raised as an exception in
the woken task.

# Examples
```jldoctest
julia> a5() = sum(i for i in 1:1000);

julia> b = Task(a5);

julia> istaskstarted(b)
false

julia> schedule(b);

julia> yield();

julia> istaskstarted(b)
true

julia> istaskdone(b)
true
```
"""
function schedule(t::Task, arg; error=false)
    # schedule a task to be (re)started with the given value or exception
    if error
        t.exception = arg
    else
        t.result = arg
    end
    return enq_work(t)
end

# fast version of `schedule(t, arg); wait()`
function schedule_and_wait(t::Task, arg=nothing)
    t.state == :runnable || error("schedule: Task not runnable")
    if isempty(Workqueue)
        return yieldto(t, arg)
    else
        t.result = arg
        push!(Workqueue, t)
        t.state = :queued
    end
    return wait()
end

"""
    yield()

Switch to the scheduler to allow another scheduled task to run. A task that calls this
function is still runnable, and will be restarted immediately if there are no other runnable
tasks.
"""
yield() = (enq_work(current_task()); wait())

"""
    yield(t::Task, arg = nothing)

A fast, unfair-scheduling version of `schedule(t, arg); yield()` which
immediately yields to `t` before calling the scheduler.
"""
function yield(t::Task, @nospecialize x = nothing)
    t.state == :runnable || error("schedule: Task not runnable")
    t.result = x
    enq_work(current_task())
    return try_yieldto(ensure_rescheduled, Ref(t))
end

"""
    yieldto(t::Task, arg = nothing)

Switch to the given task. The first time a task is switched to, the task's function is
called with no arguments. On subsequent switches, `arg` is returned from the task's last
call to `yieldto`. This is a low-level call that only switches tasks, not considering states
or scheduling in any way. Its use is discouraged.
"""
function yieldto(t::Task, @nospecialize x = nothing)
    t.result = x
    return try_yieldto(identity, Ref(t))
end

function try_yieldto(undo, reftask::Ref{Task})
    try
        ccall(:jl_switchto, Cvoid, (Any,), reftask)
    catch
        undo(reftask[])
        rethrow()
    end
    ct = current_task()
    exc = ct.exception
    if exc !== nothing
        ct.exception = nothing
        throw(exc)
    end
    result = ct.result
    ct.result = nothing
    return result
end

# yield to a task, throwing an exception in it
function throwto(t::Task, @nospecialize exc)
    t.exception = exc
    return yieldto(t)
end

function ensure_rescheduled(othertask::Task)
    ct = current_task()
    if ct !== othertask && othertask.state == :runnable
        # we failed to yield to othertask
        # return it to the head of the queue to be scheduled later
        pushfirst!(Workqueue, othertask)
        othertask.state = :queued
    end
    if ct.state == :queued
        # if the current task was queued,
        # also need to return it to the runnable state
        # before throwing an error
        i = findfirst(t->t===ct, Workqueue)
        i === nothing || deleteat!(Workqueue, i)
        ct.state = :runnable
    end
    nothing
end

@noinline function poptask()
    t = popfirst!(Workqueue)
    if t.state != :queued
        # assume this somehow got queued twice,
        # probably broken now, but try discarding this switch and keep going
        # can't throw here, because it's probably not the fault of the caller to wait
        # and don't want to use print() here, because that may try to incur a task switch
        ccall(:jl_safe_printf, Cvoid, (Ptr{UInt8}, Int32...),
            "\nWARNING: Workqueue inconsistency detected: popfirst!(Workqueue).state != :queued\n")
        return
    end
    t.state = :runnable
    return Ref(t)
end

function wait()
    while true
        if isempty(Workqueue)
            c = process_events(true)
            if c == 0 && eventloop() != C_NULL && isempty(Workqueue)
                # if there are no active handles and no runnable tasks, just
                # wait for signals.
                pause()
            end
        else
            reftask = poptask()
            if reftask !== nothing
                result = try_yieldto(ensure_rescheduled, reftask)
                process_events(false)
                # return when we come out of the queue
                return result
            end
        end
    end
    # unreachable
end

if Sys.iswindows()
    pause() = ccall(:Sleep, stdcall, Cvoid, (UInt32,), 0xffffffff)
else
    pause() = ccall(:pause, Cvoid, ())
end
