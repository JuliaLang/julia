print(t::Task) = print("Task")

start(t::Task) = yieldto(t)
done(t::Task, val) = task_done(t)
next(t::Task, val) = (val, yieldto(t))
