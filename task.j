show(t::Task) = print("Task")

start(t::Task) = yieldto(t)
done(t::Task, val) = task_done(t)
next(t::Task, val) = (val, yieldto(t))

function wait(t::Task)
    assert(!is(t,current_task()))
    while !task_done(t)
        yield()
    end
    yieldto(t)  # return last value
end
