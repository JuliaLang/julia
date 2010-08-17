print(t::Task) = print("Task")

start(t::Task) = 0
done(t::Task, i) = task_done(t)
next(t::Task, i) = (yieldto(t), i)
