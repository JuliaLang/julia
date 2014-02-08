## basic task functions and TLS

show(io::IO, t::Task) = print(io, "Task")

macro task(ex)
    :(Task(()->$(esc(ex))))
end

current_task() = ccall(:jl_get_current_task, Any, ())::Task
istaskdone(t::Task) = t.done
istaskstarted(t::Task) = t.started

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
    if istaskdone(t)
        if t.exception !== nothing
            throw(t.exception)
        end
        return t.result
    end
    if is(t.donenotify, nothing)
        t.donenotify = Condition()
    end
    while !istaskdone(t)
        wait(t.donenotify)
    end
    t.result
end

function notify(t::Task, arg::ANY=nothing; error=false)
    if t.runnable
        Base.error("tried to resume task that is not stopped")
    end
    if error
        t.exception = arg
    else
        t.result = arg
    end
    enq_work(t)
    nothing
end
notify_error(t::Task, err) = notify(t, err, error=true)

# runtime system hook called when a task finishes
function task_done_hook(t::Task)
    if isa(t.donenotify, Condition)
        if isdefined(t,:exception) && t.exception !== nothing
            # TODO: maybe wrap this in a TaskExited exception
            notify_error(t.donenotify, t.exception)
        else
            notify(t.donenotify, t.result)
        end
    end
end


## produce, consume, and task iteration

function produce(v)
    ct = current_task()
    q = ct.consumers
    if isa(q,Condition)
        # make a task waiting for us runnable again
        notify1(q)
    end
    yieldto(ct.last, v)
end
produce(v...) = produce(v)

function consume(P::Task, values...)
    while !(P.runnable || P.done)
        if P.consumers === nothing
            P.consumers = Condition()
        end
        wait(P.consumers)
    end
    ct = current_task()
    prev = ct.last
    ct.runnable = false
    local v
    try
        v = yieldto(P, values...)
        if ct.last !== P
            v = yield_until((ct)->ct.last === P)
        end
    finally
        ct.last = prev
        ct.runnable = true
    end
    if P.done
        q = P.consumers
        if !is(q, nothing)
            notify(q, P.result)
        end
    end
    v
end

start(t::Task) = nothing
function done(t::Task, val)
    t.result = consume(t)
    istaskdone(t)
end
next(t::Task, val) = (t.result, nothing)


## condition variables

type Condition
    waitq::Vector{Any}

    Condition() = new({})
end

function wait(c::Condition)
    ct = current_task()

    push!(c.waitq, ct)

    ct.runnable = false
    try
        return yield()
    catch
        filter!(x->x!==ct, c.waitq)
        rethrow()
    end
end

function notify(c::Condition, arg::ANY=nothing; all=true, error=false)
    if all
        for t in c.waitq
            !error ? (t.result = arg) : (t.exception = arg)
            enq_work(t)
        end
        empty!(c.waitq)
    elseif !isempty(c.waitq)
        t = shift!(c.waitq)
        !error ? (t.result = arg) : (t.exception = arg)
        enq_work(t)
    end
    nothing
end

notify1(c::Condition, arg=nothing) = notify(c, arg, all=false)

notify_error(c::Condition, err) = notify(c, err, error=true)
notify1_error(c::Condition, err) = notify(c, err, error=true, all=false)


## work queue

function enq_work(t::Task)
    ccall(:uv_stop,Void,(Ptr{Void},),eventloop())
    unshift!(Workqueue, t)
end

function perform_work()
    perform_work(pop!(Workqueue))
end

function perform_work(t::Task)
    ct = current_task()
    if ct.runnable && t !== ct
        # still runnable; ensure we will return here
        enq_work(ct)
    end
    if !istaskstarted(t)
        # starting new task
        result = yieldto(t)
    else
        # continuing interrupted work item
        arg = t.result
        t.result = nothing
        t.runnable = true
        result = yieldto(t, arg)
    end
    return result
end


## scheduler

# schedule an expression to run asynchronously, with minimal ceremony
macro schedule(expr)
    expr = localize_vars(:(()->($expr)), false)
    :(enq_work(Task($(esc(expr)))))
end

schedule(t::Task) = enq_work(t)

function wait()
    ct = current_task()
    ct.runnable = false
    return yield()
end

# yield() --- called for all blocking operations
in_scheduler = false
yield() = yield_until()
function yield_until(return_test = (t::Task)->t.runnable)
    ct = current_task()
    # preserve Task.last across calls to the scheduler
    prev = ct.last
    global in_scheduler
    if in_scheduler
        # we don't want to execute yield recursively, because
        # the return condition would be ill-defined
        warn("yielding from inside scheduler callback")
    end
    try
        if isempty(Workqueue) && return_test(ct)
            process_events(false)
            if isempty(Workqueue) && return_test(ct)
                return nothing
            end
        end
        in_scheduler = true
        while true
            if isempty(Workqueue)
                c = process_events(true)
                if c==0 && eventloop()!=C_NULL && isempty(Workqueue)
                    # if there are no active handles and no runnable tasks, just
                    # wait for signals.
                    pause()
                end
            else
                in_scheduler = false
                result = perform_work()
                in_scheduler = true
                process_events(false)
                if return_test(ct)
                    return result
                end
            end
        end
    finally
        in_scheduler = false
        ct.last = prev
    end
    assert(false)
end

function pause()
    @unix_only    ccall(:pause, Void, ())
    @windows_only ccall(:Sleep,stdcall, Void, (Uint32,), 0xffffffff)
end

# force a task to stop waiting with an exception
function interrupt_waiting_task(t::Task, err)
    if !t.runnable
        t.exception = err
        enq_work(t)
    end
end


## dynamically-scoped waiting for multiple items

sync_begin() = task_local_storage(:SPAWNS, ({}, get(task_local_storage(), :SPAWNS, ())))

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

macro spawnlocal(expr)
    warn_once("@spawnlocal is deprecated, use @async instead.")
    :(@async $(esc(expr)))
end
