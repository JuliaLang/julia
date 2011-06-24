show(t::Task) = print("Task")

function wait(t::Task)
    @assert !is(t,current_task())
    while !task_done(t)
        yieldto(t)
    end
    yieldto(t)  # return last value
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
        if task_done(G)
            pop(_generator_stack::Array{Any,1})
        end
        v
    end
end

start(t::Task) = consume(t)
done(t::Task, val) = task_done(t)
next(t::Task, val) = (val, consume(t))
