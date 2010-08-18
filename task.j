print(t::Task) = print("Task")

start(t::Task) = yieldto(t)
done(t::Task, val) = task_done(t)
next(t::Task, val) = (val, yieldto(t))

function wait(t::Task)
    local v
    while !task_done(t)
        v = yieldto(t)
    end
    v
end
