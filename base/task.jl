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
        bt_lines = process_backtrace(stacktrace(bt_raw))[1:min(100, end)] # Limiting this to 100 lines.
        CapturedException(ex, bt_lines)
    end

    CapturedException(ex, processed_bt::Vector{Any}) = new(ex, processed_bt)
end

function showerror(io::IO, ce::CapturedException)
    showerror(io, ce.ex, ce.processed_bt, backtrace=true)
end

"""
    capture_exception(ex, bt)::Exception

Return an exception, possibly incorporating information from a backtrace `bt`. Defaults to returning [`CapturedException(ex, bt)`](@ref).

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
    str = takestring!(b)
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
    thunk = replace_linenums!(:(()->$(esc(ex))), __source__)
    :(Task($thunk))
end

# task states

const task_state_runnable  = UInt8(0)
const task_state_done      = UInt8(1)
const task_state_failed    = UInt8(2)
# like _failed, but allows schedule to succeed
const task_state_cancelled = UInt8(3)

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
        elseif st === task_state_cancelled
            return :cancelled
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
function istaskfailed(t::Task)
    state = (@atomic :acquire t._state)
    return state === task_state_failed || state === task_state_cancelled
end

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
function _wait(t::Task; expected_cancellation = nothing)
    t === current_task() && Core.throw(ConcurrencyViolationError("deadlock detected: cannot wait on current task"))
    if !istaskdone(t)
        donenotify = t.donenotify::ThreadSynchronizer
        lock(donenotify)
        try
            while !istaskdone(t) && cancellation_request() === expected_cancellation
                wait(donenotify; waitee=t)
            end
        finally
            unlock(donenotify)
        end
    end
    nothing
end
cancel_wait!(waitee::Task, waiter::Task) = cancel_wait!(waitee.donenotify, waiter, nothing; waitee)

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
    cr = cancellation_request()
    if cr !== nothing
        propagate_cancellation!(t, cr)
    end
    if throw && istaskfailed(t)
        Core.throw(TaskFailedException(t))
    end
    nothing
end

"""
    wait_nocancel(t::Task)

Like `wait`, but do not propagate cancellation of this task to the waited-on task.
"""
function wait_nocancel(t::Task; throw=true)
    _wait(t)
    if throw && istaskfailed(t)
        Core.throw(TaskFailedException(t))
    end
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

!!! compat "Julia 1.12"
    This function requires at least Julia 1.12.
"""
waitany(tasks; throw=true) = _wait_multiple(collect_tasks(tasks), throw)

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

!!! compat "Julia 1.12"
    This function requires at least Julia 1.12.
"""
waitall(tasks; failfast=true, throw=true) = _wait_multiple(collect_tasks(tasks), throw, true, failfast)

function collect_tasks(waiting_tasks)
    tasks = Task[]
    for t in waiting_tasks
        t isa Task || error("Expected an iterator of `Task` object")
        push!(tasks, t)
    end
    return tasks
end

function _wait_multiple(tasks::Vector{Task}, throwexc::Bool=false, all::Bool=false, failfast::Bool=false)
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
        end
    end

    # We can return early all tasks are done, or if any is done and we only
    # needed to wait for one, or if any task failed and we have failfast
    if nremaining == 0 || (any(done_mask) && (!all || (failfast && exception)))
        if throwexc && (!all || failfast) && exception
            exceptions = [TaskFailedException(t) for t in tasks[done_mask] if istaskfailed(t)]
            throw(CompositeException(exceptions))
        else
            if nremaining == 0
                return tasks, Task[]
            else
                return tasks[done_mask], tasks[.~done_mask]
            end
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
        exception && failfast && break
        i = take!(chan)
        t = tasks[i]
        waiter_tasks[i] = sentinel
        done_mask[i] = true
        exception |= istaskfailed(t)
        nremaining -= 1
        # stop early if requested
        all || break
    end

    close(chan)

    # now just read which tasks finished directly: the channel is not needed anymore for that
    # repeat until we get (acquire) the list of all dependent-exited tasks
    changed = true
    while changed
        changed = false
        for (i, done) in enumerate(done_mask)
            done && continue
            t = tasks[i]
            if istaskdone(t)
                done_mask[i] = true
                exception |= istaskfailed(t)
                nremaining -= 1
                changed = true
            end
        end
    end

    if nremaining == 0
        if throwexc && exception
            exceptions = [TaskFailedException(t) for t in tasks if istaskfailed(t)]
            throw(CompositeException(exceptions))
        end
        return tasks, Task[]
    else
        remaining_mask = .~done_mask
        for i in findall(remaining_mask)
            waiter = waiter_tasks[i]
            waiter === sentinel && continue
            donenotify = tasks[i].donenotify::ThreadSynchronizer
            @lock donenotify list_deletefirst!(donenotify.waitq, waiter)
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
            cr = cancellation_request()
            if cr !== nothing
                return sync_cancel!(c, r, cr, @isdefined(c_ex) ? c_ex : CompositeException())
            end
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
    do_async_macro(expr, __source__, identity)
end

# generate the code for @async, possibly wrapping the task in something before
# pushing it to the wait queue.
function do_async_macro(expr, linenums, wrap)
    letargs = _lift_one_interp!(expr)

    thunk = replace_linenums!(:(()->($(esc(expr)))), linenums)
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
    do_async_macro(expr, __source__, taskvar->:(UnwrapTaskFailedException($taskvar)))
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
function _lift_one_interp!(@nospecialize e)
    letargs = Any[]  # store the new gensymed arguments
    _lift_one_interp_helper(e, false, 0, letargs) # Start out _not_ in a quote context (false) and not needing escapes
    return letargs
end
_lift_one_interp_helper(@nospecialize(v), _::Bool, _::Int, _::Vector{Any}) = v
function _lift_one_interp_helper(expr::Expr, in_quote_context::Bool, escs::Int, letargs::Vector{Any})
    if expr.head === :$
        if in_quote_context  # This $ is simply interpolating out of the quote
            # Now, we're out of the quote, so any _further_ $ is ours.
            in_quote_context = false
        elseif escs == 0
            # if escs is non-zero, then we cannot hoist expr.args without violating hygiene rules
            newarg = gensym()
            push!(letargs, :($(esc(newarg)) = $(esc(expr.args[1]))))
            return newarg  # Don't recurse into the lifted $() exprs
        end
    elseif expr.head === :meta || expr.head === :inert
        return expr
    elseif expr.head === :quote
        in_quote_context = true   # Don't try to lift $ directly out of quotes
    elseif expr.head === :macrocall
        return expr  # Don't recur into macro calls, since some other macros use $
    elseif expr.head === :var"hygienic-scope"
        escs += 1
    elseif expr.head === :escape
        escs == 0 && return expr
        escs -= 1
    end
    for (i,e) in enumerate(expr.args)
        expr.args[i] = _lift_one_interp_helper(e, in_quote_context, escs, letargs)
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
waitqueue(l::IntrusiveLinkedListSynchronized) = ILLRef(l.queue, l)
isempty(W::IntrusiveLinkedListSynchronized) = isempty(waitqueue(W))
length(W::IntrusiveLinkedListSynchronized) = length(waitqueue(W))
function push!(W::IntrusiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        push!(waitqueue(W), t)
    finally
        unlock(W.lock)
    end
    return W
end
function pushfirst!(W::IntrusiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        pushfirst!(waitqueue(W), t)
    finally
        unlock(W.lock)
    end
    return W
end
function pop!(W::IntrusiveLinkedListSynchronized)
    lock(W.lock)
    try
        return pop!(waitqueue(W))
    finally
        unlock(W.lock)
    end
end
function popfirst!(W::IntrusiveLinkedListSynchronized)
    lock(W.lock)
    try
        return popfirst!(waitqueue(W))
    finally
        unlock(W.lock)
    end
end
function list_deletefirst!(W::IntrusiveLinkedListSynchronized{T}, t::T) where T
    lock(W.lock)
    try
        list_deletefirst!(waitqueue(W), t)
    finally
        unlock(W.lock)
    end
    return W
end

const StickyWorkqueue = IntrusiveLinkedListSynchronized{Task}
const Workqueues = OncePerThread{StickyWorkqueue}(StickyWorkqueue)
const Workqueue = Workqueues[1] # default work queue is thread 1 // TODO: deprecate this variable

workqueue_for(tid::Int) = Workqueues[tid]

function enq_work(t::Task)
    state = t._state
    if state === task_state_cancelled
        # When canelled, we allow `enq_work`, but simply transition to failed state.
        # All other task cleanup is already done.
        state = (@atomicreplace t._state task_state_cancelled => task_state_failed).old
        # Catch double `schedule` calls on cancelled tasks.
        state === task_state_cancelled && return
    end
    if !(state === task_state_runnable && t.queue === nothing)
        error("schedule: Task not runnable")
    end
    _enq_work(t)
end

function _enq_work(t::Task)
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
    state = t._state
    if t._state === task_state_runnable
        if error
            q = t.queue; q === nothing || list_deletefirst!(q::IntrusiveLinkedList{Task}, t)
            setfield!(t, :result, arg)
            setfield!(t, :_isexception, true)
        else
            t.queue === nothing || Base.error("schedule: Task not runnable")
            setfield!(t, :result, arg)
        end
    end
    # [task] created -scheduled-> wait_time
    schedule(t)
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
        q = ct.queue; q === nothing || list_deletefirst!(q::IntrusiveLinkedList{Task}, ct)
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

function wait_forever()
    while true
        try
            while true
                wait()
            end
        catch e
            local errs = stderr
            # try to display the failure atomically
            errio = IOContext(PipeBuffer(), errs::IO)
            emphasize(errio, "Internal Task ")
            display_error(errio, current_exceptions())
            write(errs, errio)
            # victimize another random Task also
            if Threads.threadid() == 1 && isa(e, InterruptException) && isempty(Workqueue)
                backend = repl_backend_task()
                backend isa Task && throwto(backend, e)
            end
        end
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
    W = workqueue_for(Threads.threadid())
    task = trypoptask(W)
    if task === nothing
        # No tasks to run; switch to the scheduler task to run the
        # thread sleep logic.
        sched_task = get_sched_task()
        if ct !== sched_task
            istaskdone(sched_task) && (sched_task = @task wait())
            return yieldto(sched_task)
        end
        task = ccall(:jl_task_get_next, Ref{Task}, (Any, Any, Any), trypoptask, W, checktaskempty)
    end
    set_next_task(task)
    return try_yieldto(ensure_rescheduled)
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

## Cancellation

struct CancellationRequest
    request::UInt8
end

"""
	CANCEL_REQUEST_SAFE

Request safe cancelation of the current task. If the task is waiting for any
other resources, it will request safe cancellation of any such resources and
wait for the cancellation of such resources to be completed.

As a result, if either the task itself or any of its dependent resources are
currently unable to process cancelation, the request may hang and a more
aggressive cancelation method may be required. However, in general _SAFE
should be tried first.
"""
const CANCEL_REQUEST_SAFE = CancellationRequest(0x0)

"""
	CANCEL_REQUEST_ACK

Set by the task itself to indicate that a (safe) cancellation request was
received and acknowledged, but that there are dependent tasks for whom
cancelation is still pending.
"""
const CANCEL_REQUEST_ACK = CancellationRequest(0x1)

"""
	CANCEL_REQUEST_QUERY

Request that the system create an asynchronous report of why the task is currently
not able to be canceled. The report will be provided in the ->cancelation_request
field of the current task (as long as this field is still CANCEL_REQUEST_QUERY).

N.B.: Transition to CANCEL_REQUEST_QUERY is only allowed from CANCEL_REQUEST_ACK.
	  Once the waiting task has read the cancelation report, it may set the cancelation
	  request back to CANCEL_REQUEST_ACK.
"""
const CANCEL_REQUEST_QUERY = CancellationRequest(0x2)

"""
	CANCEL_REQUEST_ABANDON_EXTERNAL

Request a cancelation that will cease waiting for any external resources (e.g. I/O objects)
without going through a safe cancelation procedure for such resources. However, the
task will wait for any internal computational tasks to complete cancelation.

This is a middleground between CANCEL_REQUEST_SAFE and CANCEL_REQUEST_ABANDON_ALL. As external
I/O is often engineered for robustness in case of sudden disapperance of peers
"""
const CANCEL_REQUEST_ABANDON_EXTERNAL = CancellationRequest(0x3)

"""
	CANCEL_REQUEST_ABANDON_ALL

Request a cancelation that will cease waiting for all external resources and all unacknowledged
internal tasks. Such tasks will be frozen and become unschedulable in the future.

!!! warning
	If any canceled task has acquired locks or other resources that are contested, this method of
	cancelation may leak such resources and create deadlocks in future code. It is intended as a
	last-resort method to recover a system, but the necessity of this operation should in general
	be considered a bug (e.g. due to insufficient cancellation points in computationally-heavy code).
"""
const CANCEL_REQUEST_ABANDON_ALL = CancellationRequest(0x4)

"""
	CANCEL_REQUEST_YIELD

Request that the task yield to the scheduler at the next cancellation point to
allow another task to run its cancellation propagation logic. The cancelled task
itself will reset to ordinary operation before yielding, but may of course be
canceled by said other task before it resumes operation.
"""
const CANCEL_REQUEST_YIELD = CancellationRequest(0x5)


function Base.showerror(io::IO, cr::CancellationRequest)
    print(io, "CancellationRequest: ")
    if cr === CANCEL_REQUEST_SAFE
        print(io, "Safe Cancellation (CANCEL_REQUEST_SAFE)")
    else
        print(io, "Unknown ($(cr.request))")
    end
end

function conform_cancellation_request(@nospecialize(cr))
    if isa(cr, UInt8)
        return CancellationRequest(cr)
    end
    return cr
end

# This is the slow path of @cancel_check
@noinline function handle_cancellation!(@nospecialize(_req))
    req = conform_cancellation_request(_req)
    if req === CANCEL_REQUEST_YIELD
        @atomicreplace :sequentially_consistent :monotonic current_task().cancellation_request _req => nothing
        yield()
        req = cancellation_request()
    end
    req === nothing && return
    throw(req)
end

"""
    cancellation_request()

Returns the cancellation request for the current task or `nothing` if no
cancellation has been requested. If a cancellation request is present, it is
loaded with acquire semantics.
"""
function cancellation_request()
    ct = current_task()
    req = @atomic :monotonic ct.cancellation_request
    req === nothing && return req
    cr = @atomic :acquire ct.cancellation_request
    return conform_cancellation_request(cr)
end

"""
    Core.cancellation_point!()

Like [`cancellation_request`](@ref), but additionally gives the optimizer license
to establish this point as a cancellation reset point. If safe to do, the runtime
will attempt to unwind execution to the nearest preceeding cancellation point
when a cancellation is requested.
"""
Core.cancellation_point!

function cancel!(t::Task, crequest=CANCEL_REQUEST_SAFE)
    # TODO: Raise task priority
    @atomic :release t.cancellation_request = crequest
    # TODO: SYS_membarrier() ?
    # Special case: If the task hasn't started yet at this point, we want to set
    # it up to cancel any waits, but we need to be a bit careful with concurrent
    # starts of the task.
    if !istaskstarted(t)
        t.result = crequest
        t._isexception = true
        if (@atomicreplace :sequentially_consistent :monotonic t._state task_state_runnable => task_state_cancelled).success
            lock(t.donenotify)
            notify(t.donenotify)
            unlock(t.donenotify)
        end
        return
    end
    while !istaskdone(t)
        waitee = t.queue
        waitee === nothing && (yield(); continue)
        invokelatest(cancel_wait!, waitee, t) && break
    end
    if t.sticky
        # If this task is sticky, it won't be able to run if the task currently
        # running on its thread is blocking. Use the cancellation mechanism to
        # try and pre-empt that task.
        # N.B.: This is a best-effort attempt; the task we end up with may get
        # descheduled before we get around to cancelling it. However, that's
        # fine - it's not a correctness issue to deschedule the task. The
        # important thing is that the thread re-enter the scheduler to pick up
        # our cancelled task.
        # In the future, we may want to use the same mechanism for more general
        # pre-emption, but this helps avoid situations where tasks that have
        # cancellation points, but no yield points become uncancellable.
        tid = Threads.threadid(t)
        if tid != 0
            ccall(:jl_preempt_thread_task, Cvoid, (Int16,), (tid - 1) % Int16)
        end
    end
end

function cancel_wait!(q::StickyWorkqueue, t::Task)
    # Tasks in the workqueue are runnable - we do not cancel the wait,
    # but we do need to check whether it's in there
    lock(q.lock)
    try
        return (t in q.queue)
    finally
        unlock(q.lock)
    end
end

"""
    Base.reset_cancellation!()

Resets the cancellation status of the current task.
This should only be used from the root task after normal operation has been
resumed (e.g. by returning control to the user).
"""
function reset_cancellation!()
    ct = current_task()
    @assert ct === roottask
    @atomic :release ct.cancellation_request = nothing
end

function propagate_cancellation!(t::Task, crequest)
    if crequest != CANCEL_REQUEST_SAFE
        error("Not yet supported")
    end
    cancel!(t, crequest)
    _wait(t; expected_cancellation=crequest)
end

@noinline function sync_cancel!(c::Channel{Any}, t::Task, @nospecialize(cr), c_ex::CompositeException)
    if cr !== CANCEL_REQUEST_SAFE
        error("Not yet supported")
    end
    waitees = Any[t]
    cancel!(t, cr)
    while isready(c)
        r = take!(c)
        cancel!(r, cr)
        push!(waitees, r)
    end
    close(c)
    for r in waitees
        if isa(r, Task)
            _wait(r; expected_cancellation=cr)
            if istaskfailed(r)
                push!(c_ex, TaskFailedException(r))
            end
        else
            try
                wait(r)
            catch e
                push!(c_ex, e)
            end
        end
    end
    if !isempty(c_ex)
        throw(c_ex)
    end
    return nothing
end
