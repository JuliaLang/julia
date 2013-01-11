show(io, t::Task) = print(io, "Task")

current_task() = ccall(:jl_get_current_task, Task, ())
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
    if !is(q,nothing)
        Q = q::Array{Any,1}
        if !isempty(Q)
            # make a task waiting for us runnable again
            enq_work(pop!(Q))
        end
    end
    yieldto(ct.last, v)
    ct.parent = ct.last  # always exit to last consumer
    nothing
end

function consume(P::Task)
    while !(P.runnable || P.done)
        yield(WaitFor(:consume, P))
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
            Q = q::Array{Any,1}
            while !isempty(Q)
                enq_work(pop!(Q))
            end
        end
    end
    v
end

start(t::Task) = consume(t)
done(t::Task, val) = istaskdone(t)
next(t::Task, val) = (val, consume(t))

macro task(ex)
    :(Task(()->$(esc(ex))))
end
