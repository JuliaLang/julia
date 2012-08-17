show(io, t::Task) = print(io, "Task")

# task-local storage
function tls()
    t = current_task()
    if is(t.tls, nothing)
        t.tls = ObjectIdDict()
    end
    (t.tls)::ObjectIdDict
end

function tls(key)
    tls()[key]
end

function tls(key, val)
    tls()[key] = val
end

let _generator_stack = {}
    global produce, consume
    function produce(v)
        caller = pop(_generator_stack::Array{Any,1})
        yieldto(caller, v)
    end

    function consume(G::Task, args...)
        push(_generator_stack::Array{Any,1}, current_task())
        v = yieldto(G, args...)
        if istaskdone(G)
            pop(_generator_stack::Array{Any,1})
        end
        v
    end
end

start(t::Task) = consume(t)
done(t::Task, val) = istaskdone(t)
next(t::Task, val) = (val, consume(t))

macro task(ex)
    :(Task(()->$esc(ex)))
end
