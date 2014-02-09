## basic task functions and TLS

show(io::IO, t::Task) = print(io, "Task @0x$(hex(unsigned(pointer_from_objref(t)), WORD_SIZE>>2))")

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

# runtime system hook called when a task finishes
function task_done_hook(t::Task)
    err = isdefined(t,:exception) && t.exception !== nothing
    result = err ? t.exception : t.result

    nexttask = t.last

    q = t.consumers

    if isa(q,Condition) && !isempty(q.waitq)
        nexttask = shift!(q.waitq)
        notify(q, result, error=err)
    end

    isa(t.donenotify,Condition) && notify(t.donenotify, result, error=err)

    if nexttask.runnable
        if err
            nexttask.exception = t.exception
        end
        yieldto(nexttask, t.result)
    else
        wait()
    end
end


## produce, consume, and task iteration

function produce(v)
    q = current_task().consumers
    yieldto(shift!(q.waitq), v)
    q.waitq[1].result
end
produce(v...) = produce(v)

function consume(P::Task, values...)
    if P.done
        return wait(P)
    end
    if P.consumers === nothing
        P.consumers = Condition()
    end

    ct = current_task()

    push!(P.consumers.waitq, ct)
    ct.result = length(values)==1 ? values[1] : values

    if P.runnable
        return yieldto(P)
    else
        return wait()
    end
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

    try
        return wait()
    catch
        filter!(x->x!==ct, c.waitq)
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


## work queue

function enq_work(t::Task)
    ccall(:uv_stop,Void,(Ptr{Void},),eventloop())
    push!(Workqueue, t)
    t
end

function perform_work(t::Task)
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

function schedule(t::Task, arg; error=false)
    # schedule a task to be (re)started with the given value or exception
    if error
        t.exception = arg
    else
        t.result = arg
    end
    enq_work(t)
end

yield() = (enq_work(current_task()); wait())

function wait()
    ct = current_task()
    ct.runnable = false
    while true
        if isempty(Workqueue)
            c = process_events(true)
            if c==0 && eventloop()!=C_NULL && isempty(Workqueue)
                # if there are no active handles and no runnable tasks, just
                # wait for signals.
                pause()
            end
        else
            result = perform_work(shift!(Workqueue))
            process_events(false)
            # return when we come out of the queue
            return result
        end
    end
    assert(false)
end

function pause()
    @unix_only    ccall(:pause, Void, ())
    @windows_only ccall(:Sleep,stdcall, Void, (Uint32,), 0xffffffff)
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
