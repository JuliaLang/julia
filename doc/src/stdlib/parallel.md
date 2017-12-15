# Tasks

```@docs
Core.Task
Base.current_task
Base.istaskdone
Base.istaskstarted
Base.yield
Base.yieldto
Base.task_local_storage(::Any)
Base.task_local_storage(::Any, ::Any)
Base.task_local_storage(::Function, ::Any, ::Any)
Base.Condition
Base.notify
Base.schedule
Base.@schedule
Base.@task
Base.sleep
Base.Channel
Base.put!(::Channel, ::Any)
Base.take!(::Channel)
Base.isready(::Channel)
Base.fetch(::Channel)
Base.close(::Channel)
Base.bind(c::Channel, task::Task)
Base.asyncmap
Base.asyncmap!
```
