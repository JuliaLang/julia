## basic task functions and TLS

show(io::IO, t::Task) = print(io, "Task ($(t.state)) @0x$(hex(unsigned(pointer_from_objref(t)), WORD_SIZE>>2))")

macro task(ex)
    :(Task(()->$(esc(ex))))
end

current_task() = ccall(:jl_get_current_task, Any, ())::Task
istaskdone(t::Task) = ((t.state == :done) | (t.state == :failed))
istaskstarted(t::Task) = isdefined(t, :last)

# yield to a task, throwing an exception in it
function throwto(t::Task, exc)
    t.exception = exc
    yieldto(t)
end

function task_local_storage()
    t = current_task()
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

# runtime system hook called when a task finishes
function task_done_hook(t::Task)
    err = (t.state == :failed)
    result = t.result
    nexttask = t.last

    q = t.consumers

    #### un-optimized version
    #isa(q,Condition) && notify(q, result, error=err)
    if isa(q,Task)
        nexttask = q
        nexttask.state = :runnable
    elseif isa(q,Condition) && !isempty(q.waitq)
        notify(q, result, error=err)
    end

    t.consumers = nothing

    isa(t.donenotify,Condition) && notify(t.donenotify, result, error=err)

    if nexttask.state == :runnable
        if err
            nexttask.exception = result
        end
        yieldto(nexttask, result)
    else
        wait()
    end
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

    t.state = :runnable
    if empty
        if isempty(Workqueue)
            yieldto(t, v)
        else
            schedule_and_wait(t, v)
        end
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
    ct.state = :waiting

    schedule_and_wait(P)
end

start(t::Task) = nothing
function done(t::Task, val)
    t.result = consume(t)
    istaskdone(t)
end
next(t::Task, val) = (t.result, nothing)

isempty(::Task) = error("isempty not defined for Tasks")

## condition variables

type Condition
    waitq::Vector{Any}

    Condition() = new([])
end

function wait(c::Condition)
    ct = current_task()

    ct.state = :waiting
    push!(c.waitq, ct)

    try
        return wait()
    catch
        filter!(x->x!==ct, c.waitq)
        if ct.state == :waiting
            ct.state = :runnable
        end
        rethrow()
    end
end

function notify(c::Condition, arg::ANY=nothing; all=true, error=false)
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
    ccall(:uv_stop,Void,(Ptr{Void},),eventloop())
    push!(Workqueue, t)
    t.state = :queued
    t
end

# schedule an expression to run asynchronously, with minimal ceremony
macro schedule(expr)
    expr = :(()->($expr))
    :(enq_work(Task($(esc(expr)))))
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
    if isempty(Workqueue)
        if t.state == :runnable
            return yieldto(t, v)
        end
    else
        if t.state == :runnable
            t.result = v
            push!(Workqueue, t)
            t.state = :queued
        end
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
            arg = t.result
            t.result = nothing
            t.state = :runnable
            result = yieldto(t, arg)
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
    for r in refs
        wait(r)
    end
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
    expr = localize_vars(:(()->($expr)), false)
    :(async_run_thunk($(esc(expr))))
end
