show(io::IO, t::Task) = print(io, "Task")

current_task() = ccall(:jl_get_current_task, Any, ())::Task
istaskdone(t::Task) = t.done

function task_local_storage()
    t = current_task()
    if is(t.storage, nothing)
        t.storage = ObjectIdDict()
    end
    (t.storage)::ObjectIdDict
end
task_local_storage(key) = task_local_storage()[key]
task_local_storage(key, val) = (task_local_storage()[key] = val)

function produce(v)
    ct = current_task()
    q = ct.consumers
    if isa(q,Condition)
        # make a task waiting for us runnable again
        notify(q, P, nothing)
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
            notify(q, P, P.result, all=true)
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
    args = yield(c)

    if isa(args,InterruptException)
        filter!(x->x!==ct, c.waitq)
        error(args)
    end
    args
end

function notify(c::Condition, arg=nothing; all=true)
    if all
        for t in c.waitq
            t.result = arg
            enq_work(t)
        end
        empty!(c.waitq)
    elseif !isempty(c.waitq)
        t = shift!(c.waitq)
        t.result = arg
        enq_work(t)
    end
    nothing
end

notify1(c::Condition, arg=nothing) = notify(c, arg, all=false)
