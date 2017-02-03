# This file is a part of Julia. License is MIT: http://julialang.org/license

module Parallel

# imports for extension
import Base: getindex, wait, put!, take!, fetch, isready, push!, length,
             hash, ==, connect, kill, serialize, deserialize, close

# imports for use
import Base: Process, Semaphore, JLOptions, AnyDict, buffer_writes, wait_connected,
             VERSION_STRING, sync_begin, sync_add, sync_end, async_run_thunk,
            binding_module, notify_error

# The following are required but cannot be imported at this
# time as they are included after Parallel in sysimg.jl
#import Base: atexit, julia_exename, julia_cmd, AsyncGenerator, display_error

export
    @spawn,
    @spawnat,
    @fetch,
    @fetchfrom,
    @everywhere,
    @parallel,

    addprocs,
    CachingPool,
    clear!,
    ClusterManager,
    default_worker_pool,
    init_worker,
    interrupt,
    launch,
    manage,
    myid,
    nprocs,
    nworkers,
    pmap,
    procs,
    remote,
    remotecall,
    remotecall_fetch,
    remotecall_wait,
    remote_do,
    rmprocs,
    workers,
    WorkerPool,
    RemoteChannel,
    Future,
    WorkerConfig,
    RemoteException,
    ProcessExitedException,

# Add the following into Base as some Packages access them via Base.
# Also documented as such.
    process_messages,
    remoteref_id,
    channel_from_id,
    worker_id_from_socket,
    cluster_cookie,
    start_worker,

# Used only by shared arrays.
    check_same_host

include("clusterserialize.jl")
include("cluster.jl")   # cluster setup and management, addprocs
include("messages.jl")
include("process_messages.jl")  # process incoming messages
include("remotecall.jl")  # the remotecall* api
include("macros.jl")      # @spawn and friends
include("workerpool.jl")
include("pmap.jl")
include("managers.jl")    # LocalManager and SSHManager

end
