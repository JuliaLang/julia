# Distributed Computing

```@meta
DocTestSetup = :(using Distributed)
```

```@docs
Distributed.addprocs
Distributed.nprocs
Distributed.nworkers
Distributed.procs()
Distributed.procs(::Integer)
Distributed.workers
Distributed.rmprocs
Distributed.interrupt
Distributed.myid
Distributed.pmap
Distributed.RemoteException
Distributed.Future
Distributed.RemoteChannel
Distributed.wait
Distributed.fetch(::Any)
Distributed.remotecall(::Any, ::Integer, ::Any...)
Distributed.remotecall_wait(::Any, ::Integer, ::Any...)
Distributed.remotecall_fetch(::Any, ::Integer, ::Any...)
Distributed.remote_do(::Any, ::Integer, ::Any...)
Distributed.put!(::RemoteChannel, ::Any...)
Distributed.put!(::Future, ::Any)
Distributed.take!(::RemoteChannel, ::Any...)
Distributed.isready(::RemoteChannel, ::Any...)
Distributed.isready(::Future)
Distributed.WorkerPool
Distributed.CachingPool
Distributed.default_worker_pool
Distributed.clear!(::CachingPool)
Distributed.remote
Distributed.remotecall(::Any, ::AbstractWorkerPool, ::Any...)
Distributed.remotecall_wait(::Any, ::AbstractWorkerPool, ::Any...)
Distributed.remotecall_fetch(::Any, ::AbstractWorkerPool, ::Any...)
Distributed.remote_do(::Any, ::AbstractWorkerPool, ::Any...)
Distributed.timedwait
Distributed.@spawn
Distributed.@spawnat
Distributed.@fetch
Distributed.@fetchfrom
Distributed.@async
Distributed.@sync
Distributed.@distributed
Distributed.@everywhere
Distributed.clear!(::Any, ::Any; ::Any)
Distributed.remoteref_id
Distributed.channel_from_id
Distributed.worker_id_from_socket
Distributed.cluster_cookie()
Distributed.cluster_cookie(::Any)
```

## Cluster Manager Interface

This interface provides a mechanism to launch and manage Julia workers on different cluster environments.
There are two types of managers present in Base: `LocalManager`, for launching additional workers on the
same host, and `SSHManager`, for launching on remote hosts via `ssh`. TCP/IP sockets are used to connect
and transport messages between processes. It is possible for Cluster Managers to provide a different transport.

```@docs
Distributed.launch
Distributed.manage
Distributed.kill(::ClusterManager, ::Int, ::WorkerConfig)
Distributed.connect(::ClusterManager, ::Int, ::WorkerConfig)
Distributed.init_worker
Distributed.start_worker
Distributed.process_messages
```

```@meta
DocTestSetup = nothing
```
