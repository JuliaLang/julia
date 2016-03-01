# This file is a part of Julia. License is MIT: http://julialang.org/license

## basic task functions and TLS

function show(io::IO, t::Task)
    print(io, "Task ($(t.state)) @0x$(hex(convert(UInt, pointer_from_objref(t)), WORD_SIZE>>2))")
end

# Container for a captured exception and its backtrace. Can be serialized.
type CapturedException
    ex::Any
    processed_bt::Vector{Any}

    function CapturedException(ex, bt_raw)
        # bt_raw MUST be an Array of code pointers than can be processed by jl_lookup_code_address
        # Typically the result of a catch_backtrace()

        # Process bt_raw so that it can be safely serialized
        bt_lines = Any[]
        process_func(args...) = push!(bt_lines, args)
        process_backtrace(process_func, :(:), bt_raw, 1:100) # Limiting this to 100 lines.

        new(ex, bt_lines)
    end
end

showerror(io::IO, ce::CapturedException) = showerror(io, ce.ex, ce.processed_bt, backtrace=true)

type CompositeException <: Exception
    exceptions::Vector{Any}
    CompositeException() = new(Any[])
end
length(c::CompositeException) = length(c.exceptions)
push!(c::CompositeException, ex) = push!(c.exceptions, ex)
isempty(c::CompositeException) = isempty(c.exceptions)
start(c::CompositeException) = start(c.exceptions)
next(c::CompositeException, state) = next(c.exceptions, state)
done(c::CompositeException, state) = done(c.exceptions, state)

function showerror(io::IO, ex::CompositeException)
    if !isempty(ex)
        showerror(io, ex.exceptions[1])
        remaining = length(ex) - 1
        if remaining > 0
            print(io, "\n\n...and $remaining other exceptions.\n")
        end
    else
        print(io, "CompositeException()\n")
    end
end


macro task(ex)
    :(Task(()->$(esc(ex))))
end

# schedule an expression to run asynchronously, with minimal ceremony
macro schedule(expr)
    expr = :(()->($expr))
    :(enq_work(Task($(esc(expr)))))
end

current_task() = ccall(:jl_get_current_task, Any, ())::Task
istaskdone(t::Task) = ((t.state == :done) | (t.state == :failed))

"""
    istaskstarted(task) -> Bool

Tell whether a task has started executing.
"""
istaskstarted(t::Task) = ccall(:jl_is_task_started, Cint, (Any,), t) != 0

yieldto(t::Task, x::ANY = nothing) = ccall(:jl_switchto, Any, (Any, Any), t, x)

# yield to a task, throwing an exception in it
function throwto(t::Task, exc)
    t.exception = exc
    yieldto(t)
end

task_local_storage() = get_task_tls(current_task())
function get_task_tls(t::Task)
    if is(t.storage, nothing)
        t.storage = ObjectIdDict()
    end
    (t.storage)::ObjectIdDict
end
task_local_storage(key) = task_local_storage()[key]
task_local_storage(key, val) = (task_local_storage()[key] = val)

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
        if is(t.donenotify, nothing)
            t.donenotify = Condition()
        end
    end
    while !istaskdone(t)
        wait(t.donenotify)
    end
    if t.state == :failed
        throw(t.exception)
    end
    return t.result
end

suppress_excp_printing(t::Task) = isa(t.storage, ObjectIdDict) ? get(get_task_tls(t), :SUPPRESS_EXCEPTION_PRINTING, false) : false

# runtime system hook called when a task finishes
function task_done_hook(t::Task)
    err = (t.state == :failed)
    result = t.result
    handled = false
    if err
        t.backtrace = catch_backtrace()
    end

    q = t.consumers
    t.consumers = nothing

    if isa(t.donenotify, Condition) && !isempty(t.donenotify.waitq)
        handled = true
        notify(t.donenotify, result, error=err)
    end

    #### un-optimized version
    #isa(q,Condition) && notify(q, result, error=err)
    if isa(q,Task)
        handled = true
        nexttask = q
        nexttask.state = :runnable
        if err
            nexttask.exception = result
        end
        yieldto(nexttask, result) # this terminates the task
    elseif isa(q,Condition) && !isempty(q.waitq)
        handled = true
        notify(q, result, error=err)
    end

    if err && !handled
        if isa(result,InterruptException) && isdefined(Base,:active_repl_backend) &&
            active_repl_backend.backend_task.state == :runnable && isempty(Workqueue) &&
            active_repl_backend.in_eval
            throwto(active_repl_backend.backend_task, result)
        end
        if !suppress_excp_printing(t)
            let bt = t.backtrace
                # run a new task to print the error for us
                @schedule with_output_color(:red, STDERR) do io
                    print(io, "ERROR (unhandled task failure): ")
                    showerror(io, result, bt)
                    println(io)
                end
            end
        end
    end
    wait()
end


## produce, consume, and task iteration

function produce(v)
    #### un-optimized version
    #q = current_task().consumers
    #t = shift!(q.waitq)
    #empty = isempty(q.waitq)
    ct = current_task()
    local empty, t, q
    while true
        q = ct.consumers
        if isa(q,Task)
            t = q
            ct.consumers = nothing
            empty = true
            break
        elseif isa(q,Condition) && !isempty(q.waitq)
            t = shift!(q.waitq)
            empty = isempty(q.waitq)
            break
        end
        wait()
    end

    t.state == :runnable || throw(AssertionError("producer.consumer.state == :runnable"))
    if empty
        schedule_and_wait(t, v)
        while true
            # wait until there are more consumers
            q = ct.consumers
            if isa(q,Task)
                return q.result
            elseif isa(q,Condition) && !isempty(q.waitq)
                return q.waitq[1].result
            end
            wait()
        end
    else
        schedule(t, v)
        # make sure `t` runs before us. otherwise, the producer might
        # finish before `t` runs again, causing it to see the producer
        # as done, causing done(::Task, _) to miss the value `v`.
        # see issue #7727
        yield()
        return q.waitq[1].result
    end
end
produce(v...) = produce(v)

function consume(P::Task, values...)
    if istaskdone(P)
        return wait(P)
    end

    ct = current_task()
    ct.result = length(values)==1 ? values[1] : values

    #### un-optimized version
    #if P.consumers === nothing
    #    P.consumers = Condition()
    #end
    #push!(P.consumers.waitq, ct)
    # optimized version that avoids the queue for 1 consumer
    if P.consumers === nothing || (isa(P.consumers,Condition)&&isempty(P.consumers.waitq))
        P.consumers = ct
    else
        if isa(P.consumers, Task)
            t = P.consumers
            P.consumers = Condition()
            push!(P.consumers.waitq, t)
        end
        push!(P.consumers.waitq, ct)
    end

    P.state == :runnable ? schedule_and_wait(P) : wait() # don't attempt to queue it twice
end

start(t::Task) = nothing
function done(t::Task, val)
    t.result = consume(t)
    istaskdone(t)
end
next(t::Task, val) = (t.result, nothing)
iteratorsize(::Type{Task}) = SizeUnknown()
iteratoreltype(::Type{Task}) = EltypeUnknown()

isempty(::Task) = error("isempty not defined for Tasks")

## condition variables

type Condition
    waitq::Vector{Any}

    Condition() = new([])
end

function wait(c::Condition)
    ct = current_task()

    push!(c.waitq, ct)

    try
        return wait()
    catch
        filter!(x->x!==ct, c.waitq)
        rethrow()
    end
end

notify(c::Condition, arg::ANY=nothing; all=true, error=false) = notify(c, arg, all, error)
function notify(c::Condition, arg, all, error)
    if all
        for t in c.waitq
            schedule(t, arg, error=error)
        end
        empty!(c.waitq)
    elseif !isempty(c.waitq)
        t = shift!(c.waitq)
        schedule(t, arg, error=error)
    end
    nothing
end

notify1(c::Condition, arg=nothing) = notify(c, arg, all=false)

notify_error(c::Condition, err) = notify(c, err, error=true)
notify1_error(c::Condition, err) = notify(c, err, error=true, all=false)


## scheduler and work queue

global const Workqueue = Any[]

function enq_work(t::Task)
    t.state == :runnable || error("schedule: Task not runnable")
    ccall(:uv_stop, Void, (Ptr{Void},), eventloop())
    push!(Workqueue, t)
    t.state = :queued
    t
end

schedule(t::Task) = enq_work(t)

function schedule(t::Task, arg; error=false)
    # schedule a task to be (re)started with the given value or exception
    if error
        t.exception = arg
    else
        t.result = arg
    end
    enq_work(t)
end

# fast version of schedule(t,v);wait()
function schedule_and_wait(t, v=nothing)
    t.state == :runnable || error("schedule: Task not runnable")
    if isempty(Workqueue)
        return yieldto(t, v)
    else
        t.result = v
        push!(Workqueue, t)
        t.state = :queued
    end
    wait()
end

yield() = (enq_work(current_task()); wait())

function wait()
    while true
        if isempty(Workqueue)
            c = process_events(true)
            if c==0 && eventloop()!=C_NULL && isempty(Workqueue)
                # if there are no active handles and no runnable tasks, just
                # wait for signals.
                pause()
            end
        else
            t = shift!(Workqueue)
            t.state == :queued || throw(AssertionError("shift!(Workqueue).state == :queued"))
            arg = t.result
            t.result = nothing
            t.state = :runnable
            result = yieldto(t, arg)
            current_task().state == :runnable || throw(AssertionError("current_task().state == :runnable"))
            process_events(false)
            # return when we come out of the queue
            return result
        end
    end
    assert(false)
end

function pause()
    @unix_only    ccall(:pause, Void, ())
    @windows_only ccall(:Sleep,stdcall, Void, (UInt32,), 0xffffffff)
end


## dynamically-scoped waiting for multiple items
sync_begin() = task_local_storage(:SPAWNS, ([], get(task_local_storage(), :SPAWNS, ())))

function sync_end()
    spawns = get(task_local_storage(), :SPAWNS, ())
    if is(spawns,())
        error("sync_end() without sync_begin()")
    end
    refs = spawns[1]
    task_local_storage(:SPAWNS, spawns[2])

    c_ex = CompositeException()
    for r in refs
        try
            wait(r)
        catch ex
            if !isa(r, Task) || (isa(r, Task) && !(r.state == :failed))
                rethrow(ex)
            end
        finally
            if isa(r, Task) && (r.state == :failed)
                push!(c_ex, CapturedException(r.result, r.backtrace))
            end
        end
    end

    if !isempty(c_ex)
        throw(c_ex)
    end
    nothing
end

macro sync(block)
    quote
        sync_begin()
        v = $(esc(block))
        sync_end()
        v
    end
end

function sync_add(r)
    spawns = get(task_local_storage(), :SPAWNS, ())
    if !is(spawns,())
        push!(spawns[1], r)
        if isa(r, Task)
            tls_r = get_task_tls(r)
            tls_r[:SUPPRESS_EXCEPTION_PRINTING] = true
        end
    end
    r
end

function async_run_thunk(thunk)
    t = Task(thunk)
    sync_add(t)
    enq_work(t)
    t
end

macro async(expr)
    expr = localize_vars(esc(:(()->($expr))), false)
    :(async_run_thunk($expr))
end
