# Tasks and Parallel Computing

## Tasks

```@docs
Core.Task
Base.yieldto
Base.current_task
Base.istaskdone
Base.istaskstarted
Base.yield
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

## General Parallel Computing Support

```@docs
Base.addprocs
Base.nprocs
Base.nworkers
Base.procs()
Base.procs(::Integer)
Base.workers
Base.rmprocs
Base.interrupt
Base.myid
Base.pmap
Base.RemoteException
Base.Future
Base.RemoteChannel(::Integer)
Base.RemoteChannel(::Function, ::Integer)
Base.wait
Base.fetch(::Any)
Base.remotecall(::Any, ::Integer, ::Any...)
Base.remotecall_wait(::Any, ::Integer, ::Any...)
Base.remotecall_fetch(::Any, ::Integer, ::Any...)
Base.remote_do(::Any, ::Integer, ::Any...)
Base.put!(::RemoteChannel, ::Any...)
Base.put!(::Future, ::Any)
Base.take!(::RemoteChannel, ::Any...)
Base.isready(::RemoteChannel, ::Any...)
Base.isready(::Future)
Base.WorkerPool
Base.CachingPool
Base.default_worker_pool
Base.clear!(::CachingPool)
Base.remote
Base.remotecall(::Any, ::Base.Parallel.AbstractWorkerPool, ::Any...)
Base.remotecall_wait(::Any, ::Base.Parallel.AbstractWorkerPool, ::Any...)
Base.remotecall_fetch(::Any, ::Base.Parallel.AbstractWorkerPool, ::Any...)
Base.remote_do(::Any, ::Base.Parallel.AbstractWorkerPool, ::Any...)
Base.timedwait
Base.@spawn
Base.@spawnat
Base.@fetch
Base.@fetchfrom
Base.@async
Base.@sync
Base.@parallel
Base.@everywhere
Base.clear!(::Any, ::Any; ::Any)
Base.remoteref_id
Base.channel_from_id
Base.worker_id_from_socket
Base.cluster_cookie()
Base.cluster_cookie(::Any)
```

## Shared Arrays

```@docs
Base.SharedArray
Base.procs(::SharedArray)
Base.sdata
Base.indexpids
Base.localindexes
```

## Multi-Threading

This experimental interface supports Julia's multi-threading capabilities. Types and functions
described here might (and likely will) change in the future.

```@docs
Base.Threads.threadid
Base.Threads.nthreads
Base.Threads.@threads
Base.Threads.Atomic
Base.Threads.atomic_cas!
Base.Threads.atomic_xchg!
Base.Threads.atomic_add!
Base.Threads.atomic_sub!
Base.Threads.atomic_and!
Base.Threads.atomic_nand!
Base.Threads.atomic_or!
Base.Threads.atomic_xor!
Base.Threads.atomic_max!
Base.Threads.atomic_min!
Base.Threads.atomic_fence
```

## ccall using a threadpool (Experimental)

```@docs
Base.@threadcall
```

## Synchronization Primitives

```@docs
Base.Threads.AbstractLock
Base.lock
Base.unlock
Base.trylock
Base.islocked
Base.ReentrantLock
Base.Threads.Mutex
Base.Threads.SpinLock
Base.Threads.RecursiveSpinLock
Base.Semaphore
Base.acquire
Base.release
```

## Cluster Manager Interface

This interface provides a mechanism to launch and manage Julia workers on different cluster environments.
There are two types of managers present in Base: `LocalManager`, for launching additional workers on the
same host, and `SSHManager`, for launching on remote hosts via `ssh`. TCP/IP sockets are used to connect
and transport messages between processes. It is possible for Cluster Managers to provide a different transport.

```@docs
Base.launch
Base.manage
Base.kill(::ClusterManager, ::Int, ::WorkerConfig)
Base.init_worker
Base.connect(::ClusterManager, ::Int, ::WorkerConfig)
Base.process_messages
```
