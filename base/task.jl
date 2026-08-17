# This file is a part of Julia. License is MIT: https://julialang.org/license

## basic task functions and TLS

function Core.Task(@nospecialize(f), reserved_stack::Int=0)
    task = Core._task(f, reserved_stack)
    task.donenotify = ThreadSynchronizer()
    return task
end

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
# like _failed, but the task was forcibly abandoned and may have leaked resources
const task_state_abandoned = UInt8(3)

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
        elseif st === task_state_abandoned
            return :abandoned
        else
            @assert false "unexpected state"
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
    elseif field === :invoked
        error("Querying a Task's `invoked` field is disallowed because it is an implementation detail.")
    else
        return getfield(t, field)
    end
end

@inline function setproperty!(t::Task, field::Symbol, @nospecialize(v))
    if field === :scope
        istaskstarted(t) && error("Setting scope on a started task directly is disallowed.")
    elseif field === :invoked
        error("Setting a Task's `invoked` field directly is disallowed because it is an implementation detail.")
    elseif field === :result
        error("Setting a Task's `result` field directly is disallowed. The result of a task is determined by the return value of its code; to pass a value to a suspended task, use `schedule(t, val)` or `yieldto(t, val)` instead.")
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
    st = @atomic :acquire t._state
    return st === task_state_failed || st === task_state_abandoned
end

"""
    unsafe_abandon!(t::Task, next_task::Task) -> Bool

Forcibly abandon task `t` and switch its thread to `next_task`, discarding
`t`'s execution. Returns `true` if the abandonment committed: `t`'s state is
`:abandoned` and it will never run another instruction.

Returns `false` - with `t` untouched and still running - when the abandonment
could not be performed safely: `t` was not (or no longer) running on a
thread, was found holding runtime state that must not be discarded (runtime
locks, an in-flight finalizer or GC transition, a signal-deferral region),
or another abandonment was already in flight for that thread. Blocks (in a
scheduler-friendly wait) until the delivery settles the request one way or
the other; a victim thread that never services signals blocks the call
indefinitely.

`next_task` must be a fresh, never-scheduled task; it takes over the
victim's thread.

!!! warning
    Abandonment discards the victim's execution wherever it stands. Any
    non-runtime resource it holds (locks, buffers, connections) is leaked.
    This is a last-resort recovery primitive.

!!! note
    The task must be currently running on a thread for this to have effect;
    use [`cancel!`](@ref) with `CANCEL_REQUEST_ABANDON_ALL` to also stop
    parked or queued tasks.
"""
unsafe_abandon!(t::Task, next_task::Task) =
    unsafe_abandon!(t, next_task, CancellationRequest(0x4)) # CANCEL_REQUEST_ABANDON_ALL

function unsafe_abandon!(t::Task, next_task::Task, @nospecialize(result))
    # The requester's own wakeup handle, staged with the request in the
    # victim thread's abandon slot: the delivery paths ping it (from signal
    # context, where uv_async_send is the one legal wakeup) when the
    # request settles. One requester per slot means one consumer per
    # handle, which is exactly the AsyncCondition trigger's latched,
    # consume-once semantics - a settle that lands inside the check/park
    # window below is caught by the latch.
    async = AsyncCondition()
    tid = ccall(:jl_abandon_task_request, Cint, (Any, Any, Any, Ptr{Cvoid}),
                t, next_task, result, async.handle)
    if tid < 0
        close(async)
        return false
    end
    tid = tid % Int16
    ok = false
    try
        while true
            verdict = ccall(:jl_abandon_task_poll, Cint, (Int16,), tid)
            if verdict == 1 || verdict == -1
                ok = verdict == 1
                break
            elseif verdict == 2
                # mid-settle: the ping was already sent (pre-terminal, so it
                # can never race this handle's close below); the verdict is
                # microseconds away
                ccall(:jl_cpu_pause, Cvoid, ())
            else
                wait(async; cancel=nothing)
            end
        end
    finally
        # Safe only after the terminal consume: pings happen strictly
        # before the terminal state becomes visible.
        close(async)
    end
    if ok
        # A forcibly abandoned task never goes through the regular task
        # completion path, so wake up anyone waiting on it. (The waiters
        # observe the already-stored abandoned state; they do not touch the
        # task's stack. The root task's donenotify may be `nothing`.)
        donenotify = t.donenotify
        if donenotify isa ThreadSynchronizer
            lock(donenotify)
            notify(donenotify)
            unlock(donenotify)
        end
    end
    return ok
end

Threads.threadid(t::Task) = Int(ccall(:jl_get_task_tid, Int16, (Any,), t)+1)
function Threads.threadpool(t::Task)
    tpid = ccall(:jl_get_task_threadpoolid, Int8, (Any,), t)
    return Threads._tpid_to_sym(tpid)
end

task_result(t::Task) = t.result

"""
    task_local_storage()

Returns a dictionary (an [`IdDict`](@ref)) of the current task's task-local storage.

For example, this dictionary can be passed to [`get!`](@ref) in order
to either fetch or initialize the value of a key in the storage.
"""
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
Key lookup is based on object equality ([`===`](@ref)).
"""
task_local_storage(key) = task_local_storage()[key]

"""
    task_local_storage(key, value)

Assign a value to a key in the current task's task-local storage.
Key lookup is based on object equality ([`===`](@ref)).
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
_wait(t::Task; cancel::CancelTokenArg=DEFAULT_CANCEL) =
    _wait(t, resolve_cancel_token(cancel))
# With `cancel_value`, a cancellation of the governing token (at or above
# `min_severity`) is returned as the `CancellationRequest` instead of being
# thrown; `nothing` means the task completed.
function _wait(t::Task, tok::MaybeToken; min_severity::UInt8=0x00,
               cancel_value::Bool=false)
    t === current_task() && throw(ConcurrencyViolationError("deadlock detected: cannot wait on current task"))
    if !istaskdone(t)
        donenotify = t.donenotify::ThreadSynchronizer
        lock(donenotify)
        locked = true
        try
            while !istaskdone(t)
                locked = false
                r = wait(donenotify, tok; min_severity=min_severity,
                         cancel_value=cancel_value)
                locked = true
                if cancel_value && r isa CancellationRequest
                    return r
                end
            end
        finally
            locked && unlock(donenotify)
        end
    end
    nothing
end

waitqueue(t::Task) = waitqueue(t.donenotify::ThreadSynchronizer)

# Subscribe the not-yet-started `waiter` to `t`'s completion (see the
# GenericCondition method's contract in condition.jl: a start trigger
# governed by the waiter's birth cancellation source, not a park)
function schedule_on_notify!(t::Task, waiter::Task)
    _assert_fresh_waiter(waiter)
    if !istaskdone(t)
        # since this is similar to schedule, we should observe the sticky
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
        try
            if !istaskdone(t)
                schedule_on_notify!(donenotify, waiter)
                return nothing
            end
        finally
            unlock(donenotify)
        end
    end
    # `t` already done: start (or, under a cancelled birth source, kill)
    # the waiter now
    _assert_fresh_waiter(waiter)
    src = _birth_cancel_source(waiter)
    if src !== nothing && iscancelled(src)
        _schedule_subscription_cancelled(waiter, src)
    else
        schedule(waiter)
    end
    nothing
end

"""
    wait(t::Task; throw=true, cancel=Base.DEFAULT_CANCEL)

Wait for a `Task` to finish.

The keyword `throw` (defaults to `true`) controls whether a failed task results
in an error, thrown as a [`TaskFailedException`](@ref) which wraps the failed task.

The `cancel` keyword argument controls which cancellation token may interrupt
the wait (see [`CancellationToken`](@ref)); by default the scoped token. A
cancelled wait throws the [`CancellationRequest`](@ref) and leaves `t`
unaffected: cancellation reaches `t` only through its own governing token
(e.g. when both waiter and waitee run under the same cancelled scope).

Throws a `ConcurrencyViolationError` if `t` is the currently running task, to prevent deadlocks.
"""
wait(t::Task; throw=true, cancel::CancelTokenArg=DEFAULT_CANCEL) =
    wait(t, check_cancel_arg(cancel); throw)
@noinline function wait(t::Task, tok::MaybeToken; throw=true)
    # Inlining a blocking call buys nothing; this also keeps the inlineable `fetch(::Task)` small.
    _wait(t, tok)
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

!!! compat "Julia 1.12"
    This function requires at least Julia 1.12.
"""
waitany(tasks; throw=true, cancel::CancelTokenArg=DEFAULT_CANCEL) =
    _wait_multiple(collect_tasks(tasks), throw, false, false, check_cancel_arg(cancel))

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
waitall(tasks; failfast=true, throw=true, cancel::CancelTokenArg=DEFAULT_CANCEL) =
    _wait_multiple(collect_tasks(tasks), throw, true, failfast, check_cancel_arg(cancel))

function collect_tasks(waiting_tasks)
    tasks = Task[]
    for t in waiting_tasks
        t isa Task || error("Expected an iterator of `Task` object")
        push!(tasks, t)
    end
    return tasks
end

## Task completion as a waitable (see base/park.jl): a one-shot predicate
## kind - enqueue declines when the task is already done (its only notify
## has fired), and the recheck is membership-qualified: the slot witness,
## cleared when the completion notify pops the entry, is the
## already-delivered bit (without it a repark! would re-fire forever on
## consumed completions).
struct DoneWait
    t::Task
end

function wait_enqueue!(x::DoneWait, w::WaitEntry, first::Bool)
    t = x.t
    donenotify = t.donenotify::ThreadSynchronizer
    lock(donenotify)
    if istaskdone(t)
        unlock(donenotify)
        return false
    end
    # a duplicate of an already-registered task shares its slot
    if _find_slot(w, donenotify) == 0
        push!(waitqueue(t), w)
    end
    unlock(donenotify)
    return true
end

function wait_recheck(x::DoneWait, w::WaitEntry)
    t = x.t
    istaskdone(t) || return false
    return _find_slot(w, t.donenotify::ThreadSynchronizer) != 0
end

function wait_dequeue!(x::DoneWait, w::WaitEntry, why::UInt8)
    # lazy on a normal wake: the claiming completion notify popped its own
    # registration; eager everywhere else (fired slots must not re-fire,
    # withdrawal and cleanup must not leave the entry reachable)
    why == WAKE_VALUE && return nothing
    t = x.t
    donenotify = t.donenotify::ThreadSynchronizer
    lock(donenotify)
    list_deletefirst!(waitqueue(t), w)
    unlock(donenotify)
    return nothing
end

function _wait_multiple(tasks::Vector{Task}, throwexc::Bool=false, all::Bool=false, failfast::Bool=false,
                        tok::MaybeToken=default_cancel_token())
    if (all && !failfast) || length(tasks) <= 1
        exception = false
        # Force everything to finish synchronously for the case of waitall
        # with failfast=false
        for t in tasks
            _wait(t, tok)
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

    # We can return early if all tasks are done, or if any is done and we only
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

    # Park on all remaining tasks at once through a single multi-slot wait
    # entry - one flat waitable per pending task (duplicates share a slot)
    # plus the governing cancellation source. Each completion notify claims
    # the entry through the standard wake-claim protocol; the registrations
    # stay in place across re-parks, so the loop re-arms (`repark!`)
    # instead of re-registering after every completion, and the driver's
    # membership-qualified rechecks make the arm-then-suspend sound while
    # this bookkeeping runs unarmed.
    ct = current_task()
    src = cancel_source(tok)
    src === nothing || checkcancel(src)
    ws = Vector{Union{DoneWait, SourceWait}}()
    for (i, done) in enumerate(done_mask)
        done || push!(ws, DoneWait(tasks[i]))
    end
    src === nothing || push!(ws, SourceWait(src, 0x00))
    w = acquire_wait_entry!(ct, ws)
    parked = park!(ws, w, false)
    while true
        # suspend only when the park armed (a `false` park/re-park means a
        # waitable fired - a completion, or the source - and the driver
        # already dequeued the fired slot)
        parked && wait_safe_interrupt(ws, w)
        # the fired-source outcome (and, level-triggered, any cancelled
        # state) delivers here: withdraw and throw
        if src !== nothing && iscancelled(src)
            withdraw!(ws, w)
            checkcancel(src)
        end
        # collect completions (a wake happens-after its completing notify,
        # so the istaskdone reads below observe it)
        for (i, done) in enumerate(done_mask)
            done && continue
            t = tasks[i]
            if istaskdone(t)
                done_mask[i] = true
                exception |= istaskfailed(t)
                nremaining -= 1
            end
        end
        if nremaining == 0 || (!all && any(done_mask)) || (exception && failfast)
            break
        end
        parked = repark!(ws, w)
    end
    withdraw!(ws, w)

    if nremaining == 0
        if throwexc && exception
            exceptions = [TaskFailedException(t) for t in tasks if istaskfailed(t)]
            throw(CompositeException(exceptions))
        end
        return tasks, Task[]
    else
        done_tasks = tasks[done_mask]
        if throwexc && exception
            exceptions = [TaskFailedException(t) for t in done_tasks if istaskfailed(t)]
            throw(CompositeException(exceptions))
        else
            return done_tasks, tasks[.~done_mask]
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
@inline function fetch(t::Task; cancel::CancelTokenArg=DEFAULT_CANCEL)
    # `cancel` governs the *wait* for the task: a cancellation unwinds this
    # fetch, the fetched task keeps running.
    wait(t; cancel)
    # This typeassert looks redundant, but is required for soundness and must not be
    # removed: `Task.code`/`Task.result` are mutable, so the precise type inference
    # may derive here (via `PartialTask`) is a claim that must be re-checked at
    # runtime, not a proven fact.
    return task_result(t)::Core.task_result_type(t)
end

## lexically-scoped waiting for multiple items

struct ScheduledAfterSyncException <: Exception
    values::Vector{Any}
end

function showerror(io::IO, cr::CancellationRequest)
    print(io, "CancellationRequest: ")
    if cr === CANCEL_REQUEST_SAFE
        print(io, "Safe Cancellation (CANCEL_REQUEST_SAFE)")
    elseif cr === CANCEL_REQUEST_ABANDON_EXTERNAL
        print(io, "Abandonment of External Resources (CANCEL_REQUEST_ABANDON_EXTERNAL)")
    elseif cr === CANCEL_REQUEST_ABANDON_ALL
        print(io, "Task Abandonment (CANCEL_REQUEST_ABANDON_ALL)")
    else
        print(io, "Unknown ($(cr.request))")
    end
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

function sync_end(c::Channel{Any}, src::Union{Nothing, CancellationTokenSource}=nothing)
    local c_ex
    tok = src === nothing ? nothing : CancellationToken(src)
    while isready(c)
        r = take!(c)
        if isa(r, Task)
            cancelled = _wait(r, tok; cancel_value=tok !== nothing)
            if cancelled isa CancellationRequest
                # Our own scope (or an ancestor) was cancelled. The children
                # run under the same scope's token, so the tree walk already
                # cancelled them all; await their teardown per severity.
                return sync_cancel!(c, r, cancelled, tok,
                                    @isdefined(c_ex) ? c_ex : CompositeException())
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

# Teardown of a `@sync` block whose own scope was cancelled: the scope's
# token subtree (covering every child) is already cancelled; await the
# children's unwind per the severity policy. Our own acknowledgement of the
# request lets these teardown waits park; they are only re-woken by a
# severity escalation (`min_severity`).
@noinline function sync_cancel!(c::Channel{Any}, t::Task, cr::CancellationRequest,
                                tok::CancellationToken, c_ex::CompositeException)
    waitees = Any[t]
    while isready(c)
        push!(waitees, take!(c))
    end
    close(c)
    sev = severity(cr)
    for r in waitees
        if isa(r, Task)
            while sev < CANCEL_REQUEST_ABANDON_ALL.request
                # Tasks are internal: their cancellation is awaited (for
                # ABANDON_ALL they were frozen; there is nothing to wait
                # for). A severity escalation completes the teardown wait
                # (value-mode; only severities above the acknowledged one
                # are admitted) - adopt the stronger request and keep
                # awaiting internal tasks per its policy rather than
                # unwinding out of the `@sync` while children are still
                # running.
                r2 = _wait(r, tok; min_severity=sev + 0x01, cancel_value=true)
                r2 isa CancellationRequest || break
                cr = r2
                sev = severity(r2)
            end
            if istaskfailed(r)
                push!(c_ex, TaskFailedException(r))
            end
        else
            # Non-task waitees are external - the ABANDON_* severities cease
            # waiting for external resources.
            sev == CANCEL_REQUEST_SAFE.request || continue
            try
                wait(r)
            catch e
                push!(c_ex, e)
            end
        end
    end
    # Reporting the composite outcome constitutes delivery of the request;
    # include the request itself if no child failure already records it.
    if isempty(c_ex)
        throw(cr)
    end
    throw(c_ex)
end

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
    # The block runs in a new dynamic scope carrying the token of a fresh
    # cancellation source linked under the enclosing scope's token, so that
    # cancellation of the enclosing scope reaches every (transitively
    # spawned) child through the token tree. This expands the equivalent of
    # `@with CANCEL_TOKEN => token ...` manually: the ScopedValues macro API
    # is not loaded yet when Base code containing `@sync` is compiled during
    # bootstrap.
    scoped_block = Expr(:tryfinally, esc(block), nothing,
        :(Scope(Core.current_scope()::Union{Nothing, Scope},
                CANCEL_TOKEN => CancellationToken(var"#sync_src#"))))
    quote
        let var"#sync_src#" = CancellationTokenSource(default_cancel_token()),
            $var = Channel(Inf)
            v = $scoped_block
            sync_end($var, var"#sync_src#")
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
    # the monitor is diagnostic cleanup: shield it from the constructing
    # scope's cancellation so a cancelled scope still gets its report
    t2 = ScopedValues.with(CANCEL_TOKEN => nothing) do
        Task() do
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
    end
    t2.sticky = false
    schedule_on_notify!(t, t2)
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
    # a sticky cancellation-source registration of this task is garbage now
    w = t.cached_cancel_entry
    if w isa WaitEntry2
        o = @atomic :monotonic w.owner2
        o isa CancellationTokenSource && _note_dead_registration!(o)
    end
    # `finish_task` sets `sigatomic` before entering this function
    err = istaskfailed(t)
    result = task_result(t)
    handled = false

    donenotify = t.donenotify
    if isa(donenotify, ThreadSynchronizer)
        lock(donenotify)
        try
            if !isempty(donenotify.waitq)
                # only wakes whose claim was won count as having consumed the
                # result (a stale, already-claimed registration does not)
                handled = notify(donenotify) > 0
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
waitqueue(W::IntrusiveLinkedListSynchronized) = ILLRef(W.queue, W)
isempty(W::IntrusiveLinkedListSynchronized) = isempty(W.queue)
length(W::IntrusiveLinkedListSynchronized) = length(W.queue)
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
    if state === task_state_abandoned
        # A task frozen by forcible abandonment leaves its waitqueue
        # registrations behind by design; a later notify of such a stale
        # entry lands here. The wakeup is consumed by the abandoned task -
        # drop it silently.
        return t
    end
    (state === task_state_runnable && t.queue === nothing) || error("schedule: Task not runnable")
    (@atomic :monotonic t.waiting_on) === nothing ||
        throw(ConcurrencyViolationError("schedule: Task is registered on a wait queue"))

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
        if tp !== :foreign && Threads.threadpoolsize(tp) == 0
            # The task's threadpool has no threads, so it can never run;
            # fail it with a ConcurrencyViolationError rather than queueing
            # it (the multiqueue heaps are unsized for empty pools, and a
            # task queued during sysimage bootstrap would be serialized
            # into the system image).
            setfield!(t, :result, ConcurrencyViolationError("deadlock detected: cannot schedule task"))
            t._isexception = true
            @atomic :release t._state = task_state_failed
            return t
        end
        if tp === :foreign || Threads.threadpoolsize(tp) == 1
            # There's only one thread in the task's assigned thread pool;
            # use its work queue.
            tid = (tp === :interactive) ? 1 : Threads.threadpoolsize(:interactive)+1
            ccall(:jl_set_task_tid, Cint, (Any, Cint), t, tid-1)
            push!(workqueue_for(tid), t)
        else
            # Otherwise, put the task in the multiqueue.
            Partr.multiq_insert(t, t.priority)
            tid = Threads.threadid(t)
            if tid != 0 && tid != Threads.threadid()
                # The task's tid is pinned to another thread: typically it is that
                # thread's current task, parked hosting its thread-sleep logic in
                # wait(), and only that thread can resume it, so wake it directly
                # (#58689). If that thread was already awake (busy with other work),
                # this wake added no running thread; wake a pool thread too so the
                # number of running threads still scales with enqueued work — the
                # task is not sticky, so its tid may be cleared later, making it
                # runnable by any pool thread.
                if ccall(:jl_wakeup_thread, Cint, (Int16,), (tid - 1) % Int16) == 0
                    ccall(:jl_wakeup_threadpool, Cvoid, (Int8,), Threads._sym_to_tpid(tp))
                end
            else
                # Wake one sleeping thread in the task's pool rather than all of them. See #61820, #50425.
                ccall(:jl_wakeup_threadpool, Cvoid, (Int8,), Threads._sym_to_tpid(tp))
            end
            return t
        end
    end
    ccall(:jl_wakeup_thread, Cint, (Int16,), (tid - 1) % Int16)
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
        # Interrupt path: Unconditionally remove the wait (if any)
        # TODO: This should use the proper cancellation system instead
        w = @atomicswap t.waiting_on = nothing
        w isa WaitEntry && try_unlink_claimed!(w)
        q = t.queue
        q === nothing || list_deletefirst!(q::StickyWorkqueue, t)
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

# Deliver `exc` into the parked wait whose wake the caller already claimed
# by CASing `t.waiting_on` from `w` to nothing (the cancellation walk).
# Unlike `schedule(t, exc, error=true)` - whose unconditional swap *takes*
# a claim - this must not re-claim: the claim was the wake ticket. If a
# claim-less wake (a raw interrupter, `throwto`) resumed the task first and
# it has since registered a new wait, the claimed park no longer exists and
# the delivery is dropped - a task still eligibly parked under the
# cancelled source is impossible (its registration recheck refuses), and
# anything else must not observe this request.
function deliver_claimed_wake!(t::Task, w::WaitEntry, @nospecialize(exc))
    (@atomic :monotonic t.waiting_on) === nothing || return nothing
    # the claimed waitee-queue entry stays linked (the walk does not take
    # waitee locks); collect it opportunistically like an interrupter would
    try_unlink_claimed!(w)
    # a pending wake somebody enqueued claim-lessly is superseded by the
    # cancellation delivery, like an interrupt overriding a claimed value
    q = t.queue
    q === nothing || list_deletefirst!(q::StickyWorkqueue, t)
    t._state === task_state_runnable || return nothing
    setfield!(t, :result, exc)
    setfield!(t, :_isexception, true)
    maybe_record_enqueued!(t)
    enq_work(t)
    return nothing
end

# The value-mode variant of `deliver_claimed_wake!`, with the same claim
# contract: the parked wait *completes*, returning `val` (the cancellation
# walk's delivery to a watcher - `wait(::CancellationToken)` - whose wait
# the cancellation is the event for).
function deliver_claimed_value_wake!(t::Task, w::WaitEntry, @nospecialize(val))
    (@atomic :monotonic t.waiting_on) === nothing || return nothing
    try_unlink_claimed!(w)
    q = t.queue
    q === nothing || list_deletefirst!(q::StickyWorkqueue, t)
    t._state === task_state_runnable || return nothing
    setfield!(t, :result, val)
    maybe_record_enqueued!(t)
    enq_work(t)
    return nothing
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
        q = ct.queue; q === nothing || list_deletefirst!(q::StickyWorkqueue, ct)
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
    (t._state === task_state_runnable && t.queue === nothing &&
     (@atomic :monotonic t.waiting_on) === nothing) || throw(ConcurrencyViolationError("yield: Task not runnable"))
    # [task] user_time -yield-> wait_time
    record_running_time!(ct)
    # [task] created -scheduled-> wait_time
    maybe_record_enqueued!(t)
    setfield!(t, :result, x)
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
    elseif t._state === task_state_failed || t._state === task_state_abandoned
        throw(t.result)
    end
    # [task] user_time -yield-> wait_time
    record_running_time!(ct)
    # [task] created -scheduled-unfairly-> wait_time
    maybe_record_enqueued!(t)
    setfield!(t, :result, x)
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
        setfield!(ct, :result, nothing)
        ct._isexception = false
        throw(exc)
    end
    result = ct.result
    setfield!(ct, :result, nothing)
    return result
end

# yield to a task, throwing an exception in it
function throwto(t::Task, @nospecialize exc)
    ct = current_task()
    # [task] user_time -yield-> wait_time
    record_running_time!(ct)
    # [task] created -scheduled-unfairly-> wait_time
    maybe_record_enqueued!(t)
    setfield!(t, :result, exc)
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
            if Threads.threadid() == 1 && isa(e, InterruptException) && isempty(Workqueue)
                # An InterruptException landed on this internal scheduler task while
                # the thread was idle (it parked here after running a completed task).
                # N.B.: SIGINT no longer force-throws InterruptException on any
                # platform (it cancels the ^C episode source instead), so this
                # branch is reachable only via an explicit user/library throw
                # (`throwto`, `schedule(..., error=...)`) into a scheduler
                # task; kept as defense in depth.
                # Forward it to a task that can observe it: the REPL backend if it is
                # evaluating user code; nothing at an idle REPL prompt (drop it); the
                # root task otherwise, e.g. a non-interactive script blocked in wait
                # (#58689).
                victim = repl_backend_task()
                if !(victim isa Task)
                    at_repl_prompt = @isdefined(active_repl_backend) && active_repl_backend !== nothing
                    victim = (at_repl_prompt || istaskdone(roottask)) ? nothing : roottask
                end
                if victim isa Task
                    try
                        throwto(victim, e)
                    catch
                        # delivery is best-effort: the victim may have been
                        # rescheduled concurrently, or a second interrupt may arrive
                        # while this task is suspended in the switch
                    end
                end
            else
                local errs = stderr
                # try to display the failure atomically
                errio = IOContext(PipeBuffer(), errs::IO)
                emphasize(errio, "Internal Task ")
                display_error(errio, current_exceptions())
                write(errs, errio)
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

function discard_stale_workqueue_task(t::Task)
    # A task frozen in place by forcible abandonment is completed without
    # ever leaving the queues it was registered with (a workqueue, or a
    # waitqueue whose later notify re-enqueues it here); discard it. Any
    # other non-runnable state means the task somehow got queued twice -
    # probably broken now, but try discarding this switch and keep going.
    # We can't throw here, because it's probably not the fault of the caller
    # to wait, and don't want to use print() here, because that may try to
    # incur a task switch.
    if t._state !== task_state_abandoned
        ccall(:jl_safe_printf, Cvoid, (Ptr{UInt8}, Int32...),
            "\nWARNING: Workqueue inconsistency detected: popfirst!(Workqueue).state !== :runnable\n")
    end
    nothing
end

function trypoptask(W::StickyWorkqueue)
    while !isempty(W)
        t = popfirst!(W)
        if t._state !== task_state_runnable
            discard_stale_workqueue_task(t)
            continue
        end
        return t
    end
    while true
        t = Partr.multiq_deletemin()
        t === nothing && return nothing
        if t._state !== task_state_runnable
            discard_stale_workqueue_task(t)
            continue
        end
        return t
    end
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
        # No tasks to run. If the current task is done, switch to the scheduler task
        # to run the thread sleep logic, so that this task's stack can be freed
        # promptly (#57544). Otherwise run the thread sleep logic in the context of
        # the current task, so that an asynchronously thrown InterruptException
        # is delivered to a task that can observe it, rather than swallowed by
        # the internal scheduler task (#58689).
        sched_task = get_sched_task()
        if ct !== sched_task && istaskdone(ct)
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
        @atomic :monotonic t.running_time_ns +%= time_ns() -% t.last_started_running_at
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
