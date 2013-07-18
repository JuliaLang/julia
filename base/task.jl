show(io::IO, t::Task) = print(io, "Task")

current_task() = ccall(:jl_get_current_task, Any, ())::Task
istaskdone(t::Task) = t.done

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

# NOTE: you can only wait for scheduled tasks
function wait(t::Task)
    if is(t.donenotify, nothing)
        t.donenotify = Condition()
    end
    while !istaskdone(t)
        wait(t.donenotify)
    end
    t.result
end

function produce(v)
    ct = current_task()
    q = ct.consumers
    if isa(q,Condition)
        # make a task waiting for us runnable again
        notify1(q)
    end
    yieldto(ct.last, v)
    ct.parent = ct.last  # always exit to last consumer
    nothing
end
produce(v...) = produce(v)

function consume(P::Task)
    while !(P.runnable || P.done)
        if P.consumers === nothing
            P.consumers = Condition()
        end
        wait(P.consumers)
    end
    ct = current_task()
    prev = ct.last
    ct.runnable = false
    v = yieldto(P)
    ct.last = prev
    ct.runnable = true
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

macro task(ex)
    :(Task(()->$(esc(ex))))
end

## condition variables

type Condition
    waitq::Vector{Any}

    Condition() = new({})
end

function wait(c::Condition)
    ct = current_task()
    if ct === Scheduler
        error("cannot execute blocking function from scheduler")
    end

    push!(c.waitq, ct)

    ct.runnable = false
    try
        yield(c)
    catch
        filter!(x->x!==ct, c.waitq)
        rethrow()
    end
end

function wait()
    ct = current_task()
    if ct === Scheduler
        error("cannot execute blocking function from scheduler")
    end
    ct.runnable = false
    yield()
end

function notify(t::Task, arg::ANY=nothing; error=false)
    if t.runnable == true
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


function notify(c::Condition, arg::ANY=nothing; all=true, error=false)
    if all
        for t in c.waitq
            !error ? (t.result = arg) : (t.exception = arg)
            enq_work(t)
        end
        empty!(c.waitq)
    elseif !isempty(c.waitq)
        t = shift!(c.waitq)
        !error? (t.result = arg) : (t.exception = arg)
        enq_work(t)
    end
    nothing
end

notify1(c::Condition, arg=nothing) = notify(c, arg, all=false)

notify_error(c::Condition, err) = notify(c, err, error=true)
notify1_error(c::Condition, err) = notify(c, err, error=true, all=false)
