# This file is a part of Julia. License is MIT: https://julialang.org/license

## basic task functions and TLS

Core.Task(@nospecialize(f), reserved_stack::Int=0) = Core._Task(f, reserved_stack, ThreadSynchronizer())

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

"""
    TaskFailedException

This exception is thrown by a `wait(t)` call when task `t` fails.
`TaskFailedException` wraps the failed task `t`.
"""
struct TaskFailedException <: Exception
    task::Task
end

function showerror(io::IO, ex::TaskFailedException)
    stacks = []
    while isa(ex.task.exception, TaskFailedException)
        pushfirst!(stacks, ex.task.backtrace)
        ex = ex.task.exception
    end
    println(io, "TaskFailedException:")
    showerror(io, ex.task.exception, ex.task.backtrace)
    if !isempty(stacks)
        for bt in stacks
            show_backtrace(io, bt)
        end
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
istaskdone(t::Task) = ((t.state === :done) | istaskfailed(t))

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

"""
    istaskfailed(t::Task) -> Bool

Determine whether a task has exited because an exception was thrown.

# Examples
```jldoctest
julia> a4() = error("task failed");

julia> b = Task(a4);

julia> istaskfailed(b)
false

julia> schedule(b);

julia> yield();

julia> istaskfailed(b)
true
```
"""
istaskfailed(t::Task) = (t.state === :failed)

Threads.threadid(t::Task) = Int(ccall(:jl_get_task_tid, Int16, (Any,), t)+1)

task_result(t::Task) = t.result

task_local_storage() = get_task_tls(current_task())
function get_task_tls(t::Task)
    if t.storage === nothing
        t.storage = IdDict()
    end
    return (t.storage)::IdDict{Any,Any}
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
    hadkey = haskey(tls, key)
    old = get(tls, key, nothing)
    tls[key] = val
    try
        return body()
    finally
        hadkey ? (tls[key] = old) : delete!(tls, key)
    end
end

# just wait for a task to be done, no error propagation
function _wait(t::Task)
    if !istaskdone(t)
        lock(t.donenotify)
        try
            while !istaskdone(t)
                wait(t.donenotify)
            end
        finally
            unlock(t.donenotify)
        end
    end
    nothing
end

# have `waiter` wait for `t`
function _wait2(t::Task, waiter::Task)
    if !istaskdone(t)
        lock(t.donenotify)
        if !istaskdone(t)
            push!(t.donenotify.waitq, waiter)
            unlock(t.donenotify)
            return nothing
        else
            unlock(t.donenotify)
        end
    end
    schedule(waiter)
    nothing
end

function wait(t::Task)
    t === current_task() && error("deadlock detected: cannot wait on current task")
    _wait(t)
    if istaskfailed(t)
        throw(TaskFailedException(t))
    end
    nothing
end

fetch(@nospecialize x) = x

"""
    fetch(t::Task)

Wait for a Task to finish, then return its result value.
If the task fails with an exception, a `TaskFailedException` (which wraps the failed task)
is thrown.
"""
function fetch(t::Task)
    wait(t)
    return task_result(t)
end


## lexically-scoped waiting for multiple items

function sync_end(c::Channel{Any})
    local c_ex
    while isready(c)
        r = take!(c)
        if isa(r, Task)
            _wait(r)
            if istaskfailed(r)
                if !@isdefined(c_ex)
                    c_ex = CompositeException()
                end
                push!(c_ex, TaskFailedException(r))
            end
        else
            try
                wait(r)
            catch e
                if !@isdefined(c_ex)
                    c_ex = CompositeException()
                end
                push!(c_ex, e)
            end
        end
    end
    close(c)
    if @isdefined(c_ex)
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
        let $var = Channel(Inf)
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

Values can be interpolated into `@async` via `\$`, which copies the value directly into the
constructed underlying closure. This allows you to insert the _value_ of a variable,
isolating the aysnchronous code from changes to the variable's value in the current task.

!!! compat "Julia 1.4"
    Interpolating values via `\$` is available as of Julia 1.4.
"""
macro async(expr)
    letargs = Base._lift_one_interp!(expr)

    thunk = esc(:(()->($expr)))
    var = esc(sync_varname)
    quote
        let $(letargs...)
            local task = Task($thunk)
            if $(Expr(:islocal, var))
                put!($var, task)
            end
            schedule(task)
            task
        end
    end
end

# Capture interpolated variables in $() and move them to let-block
function _lift_one_interp!(e)
    letargs = Any[]  # store the new gensymed arguments
    _lift_one_interp_helper(e, false, letargs) # Start out _not_ in a quote context (false)
    letargs
end
_lift_one_interp_helper(v, _, _) = v
function _lift_one_interp_helper(expr::Expr, in_quote_context, letargs)
    if expr.head === :$
        if in_quote_context  # This $ is simply interpolating out of the quote
            # Now, we're out of the quote, so any _further_ $ is ours.
            in_quote_context = false
        else
            newarg = gensym()
            push!(letargs, :($(esc(newarg)) = $(esc(expr.args[1]))))
            return newarg  # Don't recurse into the lifted $() exprs
        end
    elseif expr.head === :quote
        in_quote_context = true   # Don't try to lift $ directly out of quotes
    elseif expr.head === :macrocall
        return expr  # Don't recur into macro calls, since some other macros use $
    end
    for (i,e) in enumerate(expr.args)
        expr.args[i] = _lift_one_interp_helper(e, in_quote_context, letargs)
    end
    expr
end


# add a wait-able object to the sync pool
macro sync_add(expr)
    var = esc(sync_varname)
    quote
        local ref = $(esc(expr))
        put!($var, ref)
        ref
    end
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

    donenotify = t.donenotify
    if isa(donenotify, ThreadSynchronizer)
        lock(donenotify)
        try
            if !isempty(donenotify.waitq)
                handled = true
                notify(donenotify)
            end
        finally
            unlock(donenotify)
        end
    end

    if err && !handled && Threads.threadid() == 1
        if isa(result, InterruptException) && isdefined(Base, :active_repl_backend) &&
            active_repl_backend.backend_task.state === :runnable && isempty(Workqueue) &&
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
        if Threads.threadid() == 1 &&
            isa(e, InterruptException) && isdefined(Base, :active_repl_backend) &&
            active_repl_backend.backend_task.state === :runnable && isempty(Workqueue) &&
            active_repl_backend.in_eval
            throwto(active_repl_backend.backend_task, e)
        else
            rethrow()
        end
    end
end


## scheduler and work queue

struct InvasiveLinkedListSynchronized{T}
    queue::InvasiveLinkedList{T}
    lock::Threads.SpinLock
    InvasiveLinkedListSynchronized{T}() where {T} = new(InvasiveLinkedList{T}(), Threads.SpinLock())
end
isempty(W::InvasiveLinkedListSynchronized) = isempty(W.queue)
length(W::InvasiveLinkedListSynchronized) = length(W.queue)
function push!(W::InvasiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        push!(W.queue, t)
    finally
        unlock(W.lock)
    end
    return W
end
function pushfirst!(W::InvasiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        pushfirst!(W.queue, t)
    finally
        unlock(W.lock)
    end
    return W
end
function pop!(W::InvasiveLinkedListSynchronized)
    lock(W.lock)
    try
        return pop!(W.queue)
    finally
        unlock(W.lock)
    end
end
function popfirst!(W::InvasiveLinkedListSynchronized)
    lock(W.lock)
    try
        return popfirst!(W.queue)
    finally
        unlock(W.lock)
    end
end
function list_deletefirst!(W::InvasiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        list_deletefirst!(W.queue, t)
    finally
        unlock(W.lock)
    end
    return W
end

const StickyWorkqueue = InvasiveLinkedListSynchronized{Task}
global const Workqueues = [StickyWorkqueue()]
global const Workqueue = Workqueues[1] # default work queue is thread 1
function __preinit_threads__()
    if length(Workqueues) < Threads.nthreads()
        resize!(Workqueues, Threads.nthreads())
        for i = 2:length(Workqueues)
            Workqueues[i] = StickyWorkqueue()
        end
    end
    nothing
end

function enq_work(t::Task)
    (t.state === :runnable && t.queue === nothing) || error("schedule: Task not runnable")
    tid = Threads.threadid(t)
    # Note there are three reasons a Task might be put into a sticky queue
    # even if t.sticky == false:
    # 1. The Task's stack is currently being used by the scheduler for a certain thread.
    # 2. There is only 1 thread.
    # 3. The multiq is full (can be fixed by making it growable).
    if t.sticky || tid != 0 || Threads.nthreads() == 1
        if tid == 0
            tid = Threads.threadid()
            ccall(:jl_set_task_tid, Cvoid, (Any, Cint), t, tid-1)
        end
        push!(Workqueues[tid], t)
    else
        tid = 0
        if ccall(:jl_enqueue_task, Cint, (Any,), t) != 0
            # if multiq is full, give to a random thread (TODO fix)
            tid = mod(time_ns() % Int, Threads.nthreads()) + 1
            ccall(:jl_set_task_tid, Cvoid, (Any, Cint), t, tid-1)
            push!(Workqueues[tid], t)
        end
    end
    ccall(:jl_wakeup_thread, Cvoid, (Int16,), (tid - 1) % Int16)
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
function schedule(t::Task, @nospecialize(arg); error=false)
    # schedule a task to be (re)started with the given value or exception
    t.state === :runnable || Base.error("schedule: Task not runnable")
    if error
        t.queue === nothing || Base.list_deletefirst!(t.queue, t)
        setfield!(t, :exception, arg)
    else
        t.queue === nothing || Base.error("schedule: Task not runnable")
        setfield!(t, :result, arg)
    end
    enq_work(t)
    return t
end

"""
    yield()

Switch to the scheduler to allow another scheduled task to run. A task that calls this
function is still runnable, and will be restarted immediately if there are no other runnable
tasks.
"""
function yield()
    ct = current_task()
    enq_work(ct)
    try
        wait()
    catch
        ct.queue === nothing || list_deletefirst!(ct.queue, ct)
        rethrow()
    end
end

@inline set_next_task(t::Task) = ccall(:jl_set_next_task, Cvoid, (Any,), t)

"""
    yield(t::Task, arg = nothing)

A fast, unfair-scheduling version of `schedule(t, arg); yield()` which
immediately yields to `t` before calling the scheduler.
"""
function yield(t::Task, @nospecialize(x=nothing))
    t.result = x
    enq_work(current_task())
    set_next_task(t)
    return try_yieldto(ensure_rescheduled)
end

"""
    yieldto(t::Task, arg = nothing)

Switch to the given task. The first time a task is switched to, the task's function is
called with no arguments. On subsequent switches, `arg` is returned from the task's last
call to `yieldto`. This is a low-level call that only switches tasks, not considering states
or scheduling in any way. Its use is discouraged.
"""
function yieldto(t::Task, @nospecialize(x=nothing))
    t.result = x
    set_next_task(t)
    return try_yieldto(identity)
end

function try_yieldto(undo)
    try
        ccall(:jl_switch, Cvoid, ())
    catch
        undo(ccall(:jl_get_next_task, Ref{Task}, ()))
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
    W = Workqueues[Threads.threadid()]
    if ct !== othertask && othertask.state === :runnable
        # we failed to yield to othertask
        # return it to the head of a queue to be retried later
        tid = Threads.threadid(othertask)
        Wother = tid == 0 ? W : Workqueues[tid]
        pushfirst!(Wother, othertask)
    end
    # if the current task was queued,
    # also need to return it to the runnable state
    # before throwing an error
    list_deletefirst!(W, ct)
    nothing
end

function trypoptask(W::StickyWorkqueue)
    isempty(W) && return
    t = popfirst!(W)
    if t.state !== :runnable
        # assume this somehow got queued twice,
        # probably broken now, but try discarding this switch and keep going
        # can't throw here, because it's probably not the fault of the caller to wait
        # and don't want to use print() here, because that may try to incur a task switch
        ccall(:jl_safe_printf, Cvoid, (Ptr{UInt8}, Int32...),
            "\nWARNING: Workqueue inconsistency detected: popfirst!(Workqueue).state != :runnable\n")
        return
    end
    return t
end

@noinline function poptask(W::StickyWorkqueue)
    task = trypoptask(W)
    if !(task isa Task)
        task = ccall(:jl_task_get_next, Ref{Task}, (Any, Any), trypoptask, W)
    end
    set_next_task(task)
    nothing
end

function wait()
    W = Workqueues[Threads.threadid()]
    poptask(W)
    result = try_yieldto(ensure_rescheduled)
    process_events()
    # return when we come out of the queue
    return result
end

if Sys.iswindows()
    pause() = ccall(:Sleep, stdcall, Cvoid, (UInt32,), 0xffffffff)
else
    pause() = ccall(:pause, Cvoid, ())
end
