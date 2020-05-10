# Tasks

```@docs
Core.Task
Base.@task
Base.@async
Base.@sync
Base.asyncmap
Base.asyncmap!
Base.fetch(t::Task)
Base.current_task
Base.root_task
Base.isroottask
Base.istaskdone
Base.istaskstarted
Base.istaskfailed
Base.task_local_storage(::Any)
Base.task_local_storage(::Any, ::Any)
Base.task_local_storage(::Function, ::Any, ::Any)
```

# Scheduling

```@docs
Base.yield
Base.yieldto
Base.sleep
Base.wait
Base.timedwait

Base.Condition
Base.Threads.Condition
Base.notify
Base.schedule

Base.Threads.Event

Base.Semaphore
Base.acquire
Base.release

Base.AbstractLock
Base.lock
Base.unlock
Base.trylock
Base.islocked
Base.ReentrantLock

Base.Channel
Base.Channel(::Function)
Base.put!(::Channel, ::Any)
Base.take!(::Channel)
Base.isready(::Channel)
Base.fetch(::Channel)
Base.close(::Channel)
Base.bind(c::Channel, task::Task)
```
