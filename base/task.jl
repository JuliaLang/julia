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
    capture_exception(ex, bt)::Exception

Returns an exception, possibly incorporating information from a backtrace `bt`. Defaults to returning [`CapturedException(ex, bt)`](@ref).

Used in [`asyncmap`](@ref) and [`asyncmap!`](@ref) to capture exceptions thrown during
the user-supplied function call.
"""
capture_exception(ex, bt) = CapturedException(ex, bt)

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
pushfirst!(c::CompositeException, ex) = pushfirst!(c.exceptions, ex)
isempty(c::CompositeException) = isempty(c.exceptions)
iterate(c::CompositeException, state...) = iterate(c.exceptions, state...)

function showerror(io::IO, ex::CompositeException)
    if !isempty(ex)
        showerror(io, ex.exceptions[1])
        remaining = length(ex) - 1
        if remaining > 0
            print(io, "\n\n...and ", remaining, " more exception", remaining > 1 ? "s" : "", ".\n")
        end
    else
        print(io, "CompositeException()\n")
    end
end

"""
    TaskFailedException

This exception is thrown by a [`wait(t)`](@ref) call when task `t` fails.
`TaskFailedException` wraps the failed task `t`.
"""
struct TaskFailedException <: Exception
    task::Task
end

function showerror(io::IO, ex::TaskFailedException, bt = nothing; backtrace=true)
    print(io, "TaskFailedException")
    if bt !== nothing && backtrace
        show_backtrace(io, bt)
    end
    println(io)
    printstyled(io, "\n    nested task error: ", color=error_color())
    show_task_exception(io, ex.task)
end

function show_task_exception(io::IO, t::Task; indent = true)
    stack = current_exceptions(t)
    b = IOBuffer()
    if isempty(stack)
        # exception stack buffer not available; probably a serialized task
        showerror(IOContext(b, io), t.result)
    else
        show_exception_stack(IOContext(b, io), stack)
    end
    str = String(take!(b))
    if indent
        str = replace(str, "\n" => "\n    ")
    end
    print(io, str)
end

function show(io::IO, t::Task)
    state = t.state
    state_str = "$state" * ((state == :runnable && istaskstarted(t)) ? ", started" : "")
    print(io, "Task ($state_str) @0x$(string(convert(UInt, pointer_from_objref(t)), base = 16, pad = Sys.WORD_SIZE>>2))")
end

"""
    @task

Wrap an expression in a [`Task`](@ref) without executing it, and return the [`Task`](@ref). This only
creates a task, and does not run it.

!!! warning
    By default tasks will have the sticky bit set to true `t.sticky`. This models the
    historic default for [`@async`](@ref). Sticky tasks can only be run on the worker thread
    they are first scheduled on, and when scheduled will make the task that they were scheduled
    from sticky. To obtain the behavior of [`Threads.@spawn`](@ref) set the sticky
    bit manually to `false`.

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
    thunk = Base.replace_linenums!(:(()->$(esc(ex))), __source__)
    :(Task($thunk))
end

# task states

const task_state_runnable = UInt8(0)
const task_state_done     = UInt8(1)
const task_state_failed   = UInt8(2)

@inline function getproperty(t::Task, field::Symbol)
    if field === :state
        # TODO: this field name should be deprecated in 2.0
        st = @atomic :acquire t._state
        if st === task_state_runnable
            return :runnable
        elseif st === task_state_done
            return :done
        elseif st === task_state_failed
            return :failed
        else
            @assert false
        end
    elseif field === :backtrace
        # TODO: this field name should be deprecated in 2.0
        return current_exceptions(t)[end][2]
    elseif field === :exception
        # TODO: this field name should be deprecated in 2.0
        return t._isexception ? t.result : nothing
    elseif field === :scope
        error("""
            Querying a Task's `scope` field is disallowed.
            The private `Core.current_scope()` function is better, though still an implementation detail.""")
    else
        return getfield(t, field)
    end
end

@inline function setproperty!(t::Task, field::Symbol, @nospecialize(v))
    if field === :scope
        istaskstarted(t) && error("Setting scope on a started task directly is disallowed.")
    end
    return @invoke setproperty!(t::Any, field::Symbol, v::Any)
end

"""
    istaskdone(t::Task)::Bool

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
istaskdone(t::Task) = (@atomic :acquire t._state) !== task_state_runnable

"""
    istaskstarted(t::Task)::Bool

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
    istaskfailed(t::Task)::Bool

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

!!! compat "Julia 1.3"
    This function requires at least Julia 1.3.
"""
istaskfailed(t::Task) = ((@atomic :acquire t._state) === task_state_failed)

Threads.threadid(t::Task) = Int(ccall(:jl_get_task_tid, Int16, (Any,), t)+1)
function Threads.threadpool(t::Task)
    tpid = ccall(:jl_get_task_threadpoolid, Int8, (Any,), t)
    return Threads._tpid_to_sym(tpid)
end

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
    t === current_task() && Core.throw(ConcurrencyViolationError("deadlock detected: cannot wait on current task"))
    if !istaskdone(t)
        donenotify = t.donenotify::ThreadSynchronizer
        lock(donenotify)
        try
            while !istaskdone(t)
                wait(donenotify)
            end
        finally
            unlock(donenotify)
        end
    end
    nothing
end

# have `waiter` wait for `t`
function _wait2(t::Task, waiter::Task)
    if !istaskdone(t)
        # since _wait2 is similar to schedule, we should observe the sticky
        # bit, even if we don't call `schedule` with early-return below
        if waiter.sticky && Threads.threadid(waiter) == 0 && !GC.in_finalizer()
            # Issue #41324
            # t.sticky && tid == 0 is a task that needs to be co-scheduled with
            # the parent task. If the parent (current_task) is not sticky we must
            # set it to be sticky.
            # XXX: Ideally we would be able to unset this
            current_task().sticky = true
            tid = Threads.threadid()
            ccall(:jl_set_task_tid, Cint, (Any, Cint), waiter, tid-1)
        end
        donenotify = t.donenotify::ThreadSynchronizer
        lock(donenotify)
        if !istaskdone(t)
            push!(donenotify.waitq, waiter)
            unlock(donenotify)
            return nothing
        else
            unlock(donenotify)
        end
    end
    schedule(waiter)
    nothing
end

"""
    wait(t::Task; throw=true)

Wait for a `Task` to finish.

The keyword `throw` (defaults to `true`) controls whether a failed task results
in an error, thrown as a [`TaskFailedException`](@ref) which wraps the failed task.

Throws a `ConcurrencyViolationError` if `t` is the currently running task, to prevent deadlocks.
"""
function wait(t::Task; throw=true)
    _wait(t)
    if throw && istaskfailed(t)
        Core.throw(TaskFailedException(t))
    end
    nothing
end

# Wait multiple tasks

"""
    waitany(tasks; throw=true) -> (done_tasks, remaining_tasks)

Wait until at least one of the given tasks have been completed.

If `throw` is `true`, throw `CompositeException` when one of the
completed tasks completes with an exception.

The return value consists of two task vectors. The first one consists of
completed tasks, and the other consists of uncompleted tasks.

!!! warning
    This may scale poorly compared to writing code that uses multiple individual tasks that
    each runs serially, since this needs to scan the list of `tasks` each time and
    synchronize with each one every time this is called. Or consider using
    [`waitall(tasks; failfast=true)`](@ref waitall) instead.
"""
waitany(tasks; throw=true) = _wait_multiple(tasks, throw)

"""
    waitall(tasks; failfast=true, throw=true) -> (done_tasks, remaining_tasks)

Wait until all the given tasks have been completed.

If `failfast` is `true`, the function will return when at least one of the
given tasks is finished by exception. If `throw` is `true`, throw
`CompositeException` when one of the completed tasks has failed.

`failfast` and `throw` keyword arguments work independently; when only
`throw=true` is specified, this function waits for all the tasks to complete.

The return value consists of two task vectors. The first one consists of
completed tasks, and the other consists of uncompleted tasks.
"""
waitall(tasks; failfast=true, throw=true) = _wait_multiple(tasks, throw, true, failfast)

function _wait_multiple(waiting_tasks, throwexc=false, all=false, failfast=false)
    tasks = Task[]

    for t in waiting_tasks
        t isa Task || error("Expected an iterator of `Task` object")
        push!(tasks, t)
    end

    if (all && !failfast) || length(tasks) <= 1
        exception = false
        # Force everything to finish synchronously for the case of waitall
        # with failfast=false
        for t in tasks
            _wait(t)
            exception |= istaskfailed(t)
        end
        if exception && throwexc
            exceptions = [TaskFailedException(t) for t in tasks if istaskfailed(t)]
            throw(CompositeException(exceptions))
        else
            return tasks, Task[]
        end
    end

    exception = false
    nremaining::Int = length(tasks)
    done_mask = falses(nremaining)
    for (i, t) in enumerate(tasks)
        if istaskdone(t)
            done_mask[i] = true
            exception |= istaskfailed(t)
            nremaining -= 1
        else
            done_mask[i] = false
        end
    end

    if nremaining == 0
        return tasks, Task[]
    elseif any(done_mask) && (!all || (failfast && exception))
        if throwexc && (!all || failfast) && exception
            exceptions = [TaskFailedException(t) for t in tasks[done_mask] if istaskfailed(t)]
            throw(CompositeException(exceptions))
        else
            return tasks[done_mask], tasks[.~done_mask]
        end
    end

    chan = Channel{Int}(Inf)
    sentinel = current_task()
    waiter_tasks = fill(sentinel, length(tasks))

    for (i, done) in enumerate(done_mask)
        done && continue
        t = tasks[i]
        if istaskdone(t)
            done_mask[i] = true
            exception |= istaskfailed(t)
            nremaining -= 1
            exception && failfast && break
        else
            waiter = @task put!(chan, i)
            waiter.sticky = false
            _wait2(t, waiter)
            waiter_tasks[i] = waiter
        end
    end

    while nremaining > 0
        i = take!(chan)
        t = tasks[i]
        waiter_tasks[i] = sentinel
        done_mask[i] = true
        exception |= istaskfailed(t)
        nremaining -= 1

        # stop early if requested, unless there is something immediately
        # ready to consume from the channel (using a race-y check)
        if (!all || (failfast && exception)) && !isready(chan)
            break
        end
    end

    close(chan)

    if nremaining == 0
        return tasks, Task[]
    else
        remaining_mask = .~done_mask
        for i in findall(remaining_mask)
            waiter = waiter_tasks[i]
            donenotify = tasks[i].donenotify::ThreadSynchronizer
            @lock donenotify Base.list_deletefirst!(donenotify.waitq, waiter)
        end
        done_tasks = tasks[done_mask]
        if throwexc && exception
            exceptions = [TaskFailedException(t) for t in done_tasks if istaskfailed(t)]
            throw(CompositeException(exceptions))
        else
            return done_tasks, tasks[remaining_mask]
        end
    end
end

"""
    fetch(x::Any)

Return `x`.
"""
fetch(@nospecialize x) = x

"""
    fetch(t::Task)

Wait for a [`Task`](@ref) to finish, then return its result value.
If the task fails with an exception, a [`TaskFailedException`](@ref) (which wraps the failed task)
is thrown.
"""
function fetch(t::Task)
    wait(t)
    return task_result(t)
end


## lexically-scoped waiting for multiple items

struct ScheduledAfterSyncException <: Exception
    values::Vector{Any}
end

function showerror(io::IO, ex::ScheduledAfterSyncException)
    print(io, "ScheduledAfterSyncException: ")
    if isempty(ex.values)
        print(io, "(no values)")
        return
    end
    show(io, ex.values[1])
    if length(ex.values) == 1
        print(io, " is")
    elseif length(ex.values) == 2
        print(io, " and one more ")
        print(io, nameof(typeof(ex.values[2])))
        print(io, " are")
    else
        print(io, " and ", length(ex.values) - 1, " more objects are")
    end
    print(io, " registered after the end of a `@sync` block")
end

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

    # Capture all waitable objects scheduled after the end of `@sync` and
    # include them in the exception. This way, the user can check what was
    # scheduled by examining at the exception object.
    if isready(c)
        local racy
        for r in c
            if !@isdefined(racy)
                racy = []
            end
            push!(racy, r)
        end
        if @isdefined(racy)
            if !@isdefined(c_ex)
                c_ex = CompositeException()
            end
            # Since this is a clear programming error, show this exception first:
            pushfirst!(c_ex, ScheduledAfterSyncException(racy))
        end
    end

    if @isdefined(c_ex)
        throw(c_ex)
    end
    nothing
end

const sync_varname = gensym(:sync)

"""
    @sync

Wait until all lexically-enclosed uses of [`@async`](@ref), [`@spawn`](@ref Threads.@spawn),
`Distributed.@spawnat` and `Distributed.@distributed`
are complete. All exceptions thrown by enclosed async operations are collected and thrown as
a [`CompositeException`](@ref).

# Examples
```julia-repl
julia> Threads.nthreads()
4

julia> @sync begin
           Threads.@spawn println("Thread-id \$(Threads.threadid()), task 1")
           Threads.@spawn println("Thread-id \$(Threads.threadid()), task 2")
       end;
Thread-id 3, task 1
Thread-id 1, task 2
```
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
isolating the asynchronous code from changes to the variable's value in the current task.

!!! warning
    It is strongly encouraged to favor `Threads.@spawn` over `@async` always **even when no
    parallelism is required** especially in publicly distributed libraries.  This is
    because a use of `@async` disables the migration of the *parent* task across worker
    threads in the current implementation of Julia.  Thus, seemingly innocent use of
    `@async` in a library function can have a large impact on the performance of very
    different parts of user applications.

!!! compat "Julia 1.4"
    Interpolating values via `\$` is available as of Julia 1.4.
"""
macro async(expr)
    do_async_macro(expr, __source__)
end

# generate the code for @async, possibly wrapping the task in something before
# pushing it to the wait queue.
function do_async_macro(expr, linenums; wrap=identity)
    letargs = Base._lift_one_interp!(expr)

    thunk = Base.replace_linenums!(:(()->($(esc(expr)))), linenums)
    var = esc(sync_varname)
    quote
        let $(letargs...)
            local task = Task($thunk)
            if $(Expr(:islocal, var))
                put!($var, $(wrap(:task)))
            end
            schedule(task)
            task
        end
    end
end

# task wrapper that doesn't create exceptions wrapped in TaskFailedException
struct UnwrapTaskFailedException <: Exception
    task::Task
end

# common code for wait&fetch for UnwrapTaskFailedException
function unwrap_task_failed(f::Function, t::UnwrapTaskFailedException)
    try
        f(t.task)
    catch ex
        if ex isa TaskFailedException
            throw(ex.task.exception)
        else
            rethrow()
        end
    end
end

# the unwrapping for above task wrapper (gets triggered in sync_end())
wait(t::UnwrapTaskFailedException) = unwrap_task_failed(wait, t)

# same for fetching the tasks, for convenience
fetch(t::UnwrapTaskFailedException) = unwrap_task_failed(fetch, t)

# macro for running async code that doesn't throw wrapped exceptions
macro async_unwrap(expr)
    do_async_macro(expr, __source__, wrap=task->:(Base.UnwrapTaskFailedException($task)))
end

"""
    errormonitor(t::Task)

Print an error log to `stderr` if task `t` fails.

# Examples
```julia-repl
julia> wait(errormonitor(Threads.@spawn error("task failed")); throw = false)
Unhandled Task ERROR: task failed
Stacktrace:
[...]
```
"""
function errormonitor(t::Task)
    t2 = Task() do
        if istaskfailed(t)
            local errs = stderr
            try # try to display the failure atomically
                errio = IOContext(PipeBuffer(), errs::IO)
                emphasize(errio, "Unhandled Task ")
                display_error(errio, scrub_repl_backtrace(current_exceptions(t)))
                write(errs, errio)
            catch
                try # try to display the secondary error atomically
                    errio = IOContext(PipeBuffer(), errs::IO)
                    print(errio, "\nSYSTEM: caught exception while trying to print a failed Task notice: ")
                    display_error(errio, scrub_repl_backtrace(current_exceptions()))
                    write(errs, errio)
                    flush(errs)
                    # and then the actual error, as best we can
                    Core.print(Core.stderr, "while handling: ")
                    Core.println(Core.stderr, current_exceptions(t)[end][1])
                catch e
                    # give up
                    Core.print(Core.stderr, "\nSYSTEM: caught exception of type ", typeof(e).name.name,
                            " while trying to print a failed Task notice; giving up\n")
                end
            end
        end
        nothing
    end
    t2.sticky = false
    _wait2(t, t2)
    return t
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

function repl_backend_task()
    @isdefined(active_repl_backend) || return
    backend = active_repl_backend
    isdefined(backend, :backend_task) || return
    backend_task = getfield(active_repl_backend, :backend_task)::Task
    if backend_task._state === task_state_runnable && getfield(backend, :in_eval)
        return backend_task
    end
    return
end

# runtime system hook called when a task finishes
function task_done_hook(t::Task)
    # `finish_task` sets `sigatomic` before entering this function
    err = istaskfailed(t)
    result = task_result(t)
    handled = false

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
        if isa(result, InterruptException) && isempty(Workqueue)
            backend = repl_backend_task()
            backend isa Task && throwto(backend, result)
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
        if Threads.threadid() == 1 && isa(e, InterruptException) && isempty(Workqueue)
            backend = repl_backend_task()
            backend isa Task && throwto(backend, e)
        end
        rethrow() # this will terminate the program
    end
end

function init_task_lock(t::Task) # Function only called from jl_adopt_thread so foreign tasks have a lock.
    if t.donenotify === nothing
        t.donenotify = ThreadSynchronizer()
    end
end

## scheduler and work queue

mutable struct IntrusiveLinkedListSynchronized{T}
    queue::IntrusiveLinkedList{T}
    lock::Threads.SpinLock
    IntrusiveLinkedListSynchronized{T}() where {T} = new(IntrusiveLinkedList{T}(), Threads.SpinLock())
end
isempty(W::IntrusiveLinkedListSynchronized) = isempty(W.queue)
length(W::IntrusiveLinkedListSynchronized) = length(W.queue)
function push!(W::IntrusiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        push!(W.queue, t)
    finally
        unlock(W.lock)
    end
    return W
end
function pushfirst!(W::IntrusiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        pushfirst!(W.queue, t)
    finally
        unlock(W.lock)
    end
    return W
end
function pop!(W::IntrusiveLinkedListSynchronized)
    lock(W.lock)
    try
        return pop!(W.queue)
    finally
        unlock(W.lock)
    end
end
function popfirst!(W::IntrusiveLinkedListSynchronized)
    lock(W.lock)
    try
        return popfirst!(W.queue)
    finally
        unlock(W.lock)
    end
end
function list_deletefirst!(W::IntrusiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        list_deletefirst!(W.queue, t)
    finally
        unlock(W.lock)
    end
    return W
end

const StickyWorkqueue = IntrusiveLinkedListSynchronized{Task}
global Workqueues::Vector{StickyWorkqueue} = [StickyWorkqueue()]
const Workqueues_lock = Threads.SpinLock()
const Workqueue = Workqueues[1] # default work queue is thread 1 // TODO: deprecate this variable

function workqueue_for(tid::Int)
    qs = Workqueues
    if length(qs) >= tid && isassigned(qs, tid)
        return @inbounds qs[tid]
    end
    # slow path to allocate it
    @assert tid > 0
    l = Workqueues_lock
    @lock l begin
        qs = Workqueues
        if length(qs) < tid
            nt = Threads.maxthreadid()
            @assert tid <= nt
            global Workqueues = qs = copyto!(typeof(qs)(undef, length(qs) + nt - 1), qs)
        end
        if !isassigned(qs, tid)
            @inbounds qs[tid] = StickyWorkqueue()
        end
        return @inbounds qs[tid]
    end
end

function enq_work(t::Task)
    (t._state === task_state_runnable && t.queue === nothing) || error("schedule: Task not runnable")

    # Sticky tasks go into their thread's work queue.
    if t.sticky
        tid = Threads.threadid(t)
        if tid == 0
            # The task is not yet stuck to a thread. Stick it to the current
            # thread and do the same to the parent task (the current task) so
            # that the tasks are correctly co-scheduled (issue #41324).
            # XXX: Ideally we would be able to unset this.
            if GC.in_finalizer()
                # The task was launched in a finalizer. There is no thread to sticky it
                # to, so just allow it to run anywhere as if it had been non-sticky.
                t.sticky = false
                @goto not_sticky
            else
                tid = Threads.threadid()
                ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1)
                current_task().sticky = true
            end
        end
        push!(workqueue_for(tid), t)
    else
        @label not_sticky
        tp = Threads.threadpool(t)
        if tp === :foreign || Threads.threadpoolsize(tp) == 1
            # There's only one thread in the task's assigned thread pool;
            # use its work queue.
            tid = (tp === :interactive) ? 1 : Threads.threadpoolsize(:interactive)+1
            ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1)
            push!(workqueue_for(tid), t)
        else
            # Otherwise, put the task in the multiqueue.
            Partr.multiq_insert(t, t.priority)
            tid = 0
        end
    end
    ccall(:jl_wakeup_thread, Cvoid, (Int16,), (tid - 1) % Int16)
    return t
end

function schedule(t::Task)
    # [task] created -scheduled-> wait_time
    maybe_record_enqueued!(t)
    enq_work(t)
end

"""
    schedule(t::Task, [val]; error=false)

Add a [`Task`](@ref) to the scheduler's queue. This causes the task to run constantly when the system
is otherwise idle, unless the task performs a blocking operation such as [`wait`](@ref).

If a second argument `val` is provided, it will be passed to the task (via the return value of
[`yieldto`](@ref)) when it runs again. If `error` is `true`, the value is raised as an exception in
the woken task.

!!! warning
    It is incorrect to use `schedule` on an arbitrary `Task` that has already been started.
    See [the API reference](@ref low-level-schedule-wait) for more information.

!!! warning
    By default tasks will have the sticky bit set to true `t.sticky`. This models the
    historic default for [`@async`](@ref). Sticky tasks can only be run on the worker thread
    they are first scheduled on, and when scheduled will make the task that they were scheduled
    from sticky. To obtain the behavior of [`Threads.@spawn`](@ref) set the sticky
    bit manually to `false`.

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
    t._state === task_state_runnable || Base.error("schedule: Task not runnable")
    if error
        q = t.queue; q === nothing || Base.list_deletefirst!(q::IntrusiveLinkedList{Task}, t)
        setfield!(t, :result, arg)
        setfield!(t, :_isexception, true)
    else
        t.queue === nothing || Base.error("schedule: Task not runnable")
        setfield!(t, :result, arg)
    end
    # [task] created -scheduled-> wait_time
    maybe_record_enqueued!(t)
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
        q = ct.queue; q === nothing || Base.list_deletefirst!(q::IntrusiveLinkedList{Task}, ct)
        rethrow()
    end
end

@inline set_next_task(t::Task) = ccall(:jl_set_next_task, Cvoid, (Any,), t)

"""
    yield(t::Task, arg = nothing)

A fast, unfair-scheduling version of `schedule(t, arg); yield()` which
immediately yields to `t` before calling the scheduler.

Throws a `ConcurrencyViolationError` if `t` is the currently running task.
"""
function yield(t::Task, @nospecialize(x=nothing))
    ct = current_task()
    t === ct && throw(ConcurrencyViolationError("Cannot yield to currently running task!"))
    (t._state === task_state_runnable && t.queue === nothing) || throw(ConcurrencyViolationError("yield: Task not runnable"))
    # [task] user_time -yield-> wait_time
    record_running_time!(ct)
    # [task] created -scheduled-> wait_time
    maybe_record_enqueued!(t)
    t.result = x
    enq_work(ct)
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
    ct = current_task()
    # TODO: these are legacy behaviors; these should perhaps be a scheduler
    # state error instead.
    if t._state === task_state_done
        return x
    elseif t._state === task_state_failed
        throw(t.result)
    end
    # [task] user_time -yield-> wait_time
    record_running_time!(ct)
    # [task] created -scheduled-unfairly-> wait_time
    maybe_record_enqueued!(t)
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
    # [task] wait_time -(re)started-> user_time
    if ct.metrics_enabled
        @atomic :monotonic ct.last_started_running_at = time_ns()
    end
    if ct._isexception
        exc = ct.result
        ct.result = nothing
        ct._isexception = false
        throw(exc)
    end
    result = ct.result
    ct.result = nothing
    return result
end

# yield to a task, throwing an exception in it
function throwto(t::Task, @nospecialize exc)
    ct = current_task()
    # [task] user_time -yield-> wait_time
    record_running_time!(ct)
    # [task] created -scheduled-unfairly-> wait_time
    maybe_record_enqueued!(t)
    t.result = exc
    t._isexception = true
    set_next_task(t)
    return try_yieldto(identity)
end

@inline function wait_forever()
    while true
        wait()
    end
end

const get_sched_task = OncePerThread{Task}() do
    Task(wait_forever)
end

function ensure_rescheduled(othertask::Task)
    ct = current_task()
    W = workqueue_for(Threads.threadid())
    if ct !== othertask && othertask._state === task_state_runnable
        # we failed to yield to othertask
        # return it to the head of a queue to be retried later
        tid = Threads.threadid(othertask)
        Wother = tid == 0 ? W : workqueue_for(tid)
        pushfirst!(Wother, othertask)
    end
    # if the current task was queued,
    # also need to return it to the runnable state
    # before throwing an error
    list_deletefirst!(W, ct)
    nothing
end

function trypoptask(W::StickyWorkqueue)
    while !isempty(W)
        t = popfirst!(W)
        if t._state !== task_state_runnable
            # assume this somehow got queued twice,
            # probably broken now, but try discarding this switch and keep going
            # can't throw here, because it's probably not the fault of the caller to wait
            # and don't want to use print() here, because that may try to incur a task switch
            ccall(:jl_safe_printf, Cvoid, (Ptr{UInt8}, Int32...),
                "\nWARNING: Workqueue inconsistency detected: popfirst!(Workqueue).state !== :runnable\n")
            continue
        end
        return t
    end
    return Partr.multiq_deletemin()
end

checktaskempty = Partr.multiq_check_empty

function wait()
    ct = current_task()
    # [task] user_time -yield-or-done-> wait_time
    record_running_time!(ct)
    # let GC run
    GC.safepoint()
    # check for libuv events
    process_events()

    # get the next task to run
    result = nothing
    have_result = false
    W = workqueue_for(Threads.threadid())
    task = trypoptask(W)
    if !(task isa Task)
        # No tasks to run; switch to the scheduler task to run the
        # thread sleep logic.
        sched_task = get_sched_task()
        if ct !== sched_task
            result = yieldto(sched_task)
            have_result = true
        else
            task = ccall(:jl_task_get_next, Ref{Task}, (Any, Any, Any),
                         trypoptask, W, checktaskempty)
        end
    end
    # We may have already switched tasks (via the scheduler task), so
    # only switch if we haven't.
    if !have_result
        @assert task isa Task
        set_next_task(task)
        result = try_yieldto(ensure_rescheduled)
    end
    return result
end

if Sys.iswindows()
    pause() = ccall(:Sleep, stdcall, Cvoid, (UInt32,), 0xffffffff)
else
    pause() = ccall(:pause, Cvoid, ())
end

# update the `running_time_ns` field of `t` to include the time since it last started running.
function record_running_time!(t::Task)
    if t.metrics_enabled && !istaskdone(t)
        @atomic :monotonic t.running_time_ns += time_ns() - t.last_started_running_at
    end
    return t
end

# if this is the first time `t` has been added to the run queue
# (or the first time it has been unfairly yielded to without being added to the run queue)
# then set the `first_enqueued_at` field to the current time.
function maybe_record_enqueued!(t::Task)
    if t.metrics_enabled && t.first_enqueued_at == 0
        @atomic :monotonic t.first_enqueued_at = time_ns()
    end
    return t
end
