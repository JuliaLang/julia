# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type ClusterManager end

mutable struct WorkerConfig
    # Common fields relevant to all cluster managers
    io::Nullable{IO}
    host::Nullable{AbstractString}
    port::Nullable{Integer}

    # Used when launching additional workers at a host
    count::Nullable{Union{Int, Symbol}}
    exename::Nullable{Union{AbstractString, Cmd}}
    exeflags::Nullable{Cmd}

    # External cluster managers can use this to store information at a per-worker level
    # Can be a dict if multiple fields need to be stored.
    userdata::Nullable{Any}

    # SSHManager / SSH tunnel connections to workers
    tunnel::Nullable{Bool}
    bind_addr::Nullable{AbstractString}
    sshflags::Nullable{Cmd}
    max_parallel::Nullable{Integer}

    # Used by Local/SSH managers
    connect_at::Nullable{Any}

    process::Nullable{Process}
    ospid::Nullable{Integer}

    # Private dictionary used to store temporary information by Local/SSH managers.
    environ::Nullable{Dict}

    # Connections to be setup depending on the network topology requested
    ident::Nullable{Any}      # Worker as identified by the Cluster Manager.
    # List of other worker idents this worker must connect with. Used with topology T_CUSTOM.
    connect_idents::Nullable{Array}

    # Run multithreaded blas on worker
    enable_threaded_blas::Nullable{Bool}

    function WorkerConfig()
        wc = new()
        for n in 1:length(WorkerConfig.types)
            T = eltype(fieldtype(WorkerConfig, n))
            setfield!(wc, n, Nullable{T}())
        end
        wc
    end
end

@enum WorkerState W_CREATED W_CONNECTED W_TERMINATING W_TERMINATED
mutable struct Worker
    id::Int
    del_msgs::Array{Any,1}
    add_msgs::Array{Any,1}
    gcflag::Bool
    state::WorkerState
    c_state::Condition      # wait for state changes
    ct_time::Float64        # creation time
    conn_func::Nullable{Function}     # Used to setup connections lazily

    r_stream::IO
    w_stream::IO
    w_serializer::ClusterSerializer  # writes can happen from any task hence store the
                                     # serializer as part of the Worker object
    manager::ClusterManager
    config::WorkerConfig
    version::Nullable{VersionNumber}  # Julia version of the remote process

    function Worker(id::Int, r_stream::IO, w_stream::IO, manager::ClusterManager;
                             version=Nullable{VersionNumber}(), config=WorkerConfig())
        w = Worker(id)
        w.r_stream = r_stream
        w.w_stream = buffer_writes(w_stream)
        w.w_serializer = ClusterSerializer(w.w_stream)
        w.manager = manager
        w.config = config
        w.version = version
        set_worker_state(w, W_CONNECTED)
        register_worker_streams(w)
        w
    end

    Worker(id::Int) = Worker(id, Nullable{Function}())
    function Worker(id::Int, conn_func)
        @assert id > 0
        if haskey(map_pid_wrkr, id)
            return map_pid_wrkr[id]
        end
        w=new(id, [], [], false, W_CREATED, Condition(), time(), conn_func)
        register_worker(w)
        w
    end

    Worker() = Worker(get_next_pid())
end

function set_worker_state(w, state)
    w.state = state
    notify(w.c_state; all=true)
end

function check_worker_state(w::Worker)
    if w.state == W_CREATED
        if !isclusterlazy()
            if PGRP.topology == :all_to_all
                # Since higher pids connect with lower pids, the remote worker
                # may not have connected to us yet. Wait for some time.
                wait_for_conn(w)
            else
                error("peer $(w.id) is not connected to $(myid()). Topology : " * string(PGRP.topology))
            end
        else
            w.ct_time = time()
            if myid() > w.id
                @schedule exec_conn_func(w)
            else
                # route request via node 1
                @schedule remotecall_fetch((p,to_id) -> remotecall_fetch(exec_conn_func, p, to_id), 1, w.id, myid())
            end
            wait_for_conn(w)
        end
    end
end

exec_conn_func(id::Int) = exec_conn_func(worker_from_id(id))
function exec_conn_func(w::Worker)
    if isnull(w.conn_func)
        return wait_for_conn(w) # Some other task may be trying to connect at the same time.
    end

    try
        f = get(w.conn_func)
        w.conn_func = Nullable{Function}()
        f()
    catch e
        w.conn_func = () -> throw(e)
        rethrow(e)
    end
    nothing
end

function wait_for_conn(w)
    if w.state == W_CREATED
        timeout =  worker_timeout() - (time() - w.ct_time)
        timeout <= 0 && error("peer $(w.id) has not connected to $(myid())")

        @schedule (sleep(timeout); notify(w.c_state; all=true))
        wait(w.c_state)
        w.state == W_CREATED && error("peer $(w.id) didn't connect to $(myid()) within $timeout seconds")
    end
    nothing
end

## process group creation ##

mutable struct LocalProcess
    id::Int
    bind_addr::AbstractString
    bind_port::UInt16
    cookie::AbstractString
    LocalProcess() = new(1)
end


function disable_threaded_libs()
    BLAS.set_num_threads(1)
end

worker_timeout() = parse(Float64, get(ENV, "JULIA_WORKER_TIMEOUT", "60.0"))


## worker creation and setup ##
"""
    start_worker(out::IO=STDOUT)
    start_worker(cookie::AbstractString)
    start_worker(out::IO, cookie::AbstractString)

`Base.start_worker` is an internal function which is the default entry point for
worker processes connecting via TCP/IP. It sets up the process as a Julia cluster
worker.

If the cookie is unspecified, the worker tries to read it from its STDIN.

host:port information is written to stream `out` (defaults to STDOUT).

The function closes STDIN (after reading the cookie if required), redirects STDERR to STDOUT,
listens on a free port (or if specified, the port in the `--bind-to` command
line option) and schedules tasks to process incoming TCP connections and requests.

It does not return.
"""
start_worker(out::IO=STDOUT) = start_worker(out, Nullable{AbstractString}())
start_worker(cookie::AbstractString) = start_worker(STDOUT, Nullable{AbstractString}(cookie))
start_worker(out::IO, cookie::AbstractString) = start_worker(out, Nullable{AbstractString}(cookie))
function start_worker(out::IO, cookie_in::Nullable{AbstractString})
    if isnull(cookie_in)
        cookie = readline(STDIN)
    else
        cookie = get(cookie_in)
    end
    close(STDIN) # workers will not use it
    redirect_stderr(STDOUT)

    init_worker(cookie)
    interface = IPv4(LPROC.bind_addr)
    if LPROC.bind_port == 0
        (port, sock) = listenany(interface, UInt16(0))
        LPROC.bind_port = port
    else
        sock = listen(interface, LPROC.bind_port)
    end
    @schedule while isopen(sock)
        client = accept(sock)
        process_messages(client, client, true)
    end
    print(out, "julia_worker:")  # print header
    print(out, "$(dec(LPROC.bind_port))#") # print port
    print(out, LPROC.bind_addr)
    print(out, '\n')
    flush(out)

    disable_nagle(sock)

    if ccall(:jl_running_on_valgrind,Cint,()) != 0
        println(out, "PID = $(getpid())")
    end

    try
        # To prevent hanging processes on remote machines, newly launched workers exit if the
        # master process does not connect in time.
        check_master_connect()
        while true; wait(); end
    catch err
        print(STDERR, "unhandled exception on $(myid()): $(err)\nexiting.\n")
    end

    close(sock)
    exit(0)
end


function redirect_worker_output(ident, stream)
    @schedule while !eof(stream)
        line = readline(stream)
        if startswith(line, "\tFrom worker ")
            # STDOUT's of "additional" workers started from an initial worker on a host are not available
            # on the master directly - they are routed via the initial worker's STDOUT.
            println(line)
        else
            println("\tFrom worker $(ident):\t$line")
        end
    end
end


# The default TCP transport relies on the worker listening on a free
# port available and printing its bind address and port.
# The master process uses this to connect to the worker and subsequently
# setup a all-to-all network.
function read_worker_host_port(io::IO)
    t0 = time()

    # Wait at most for JULIA_WORKER_TIMEOUT seconds to read host:port
    # info from the worker
    timeout = worker_timeout()

    # We expect the first line to contain the host:port string. However, as
    # the worker may be launched via ssh or a cluster manager like SLURM,
    # ignore any informational / warning lines printed by the launch command.
    # If we do not find the host:port string in the first 1000 lines, treat it
    # as an error.

    ntries = 1000
    while ntries > 0
        readtask = @schedule readline(io)
        yield()
        while !istaskdone(readtask) && ((time() - t0) < timeout)
            sleep(0.05)
        end
        !istaskdone(readtask) && break

        conninfo = wait(readtask)
        if isempty(conninfo) && !isopen(io)
            error("Unable to read host:port string from worker. Launch command exited with error?")
        end

        ntries -= 1
        bind_addr, port = parse_connection_info(conninfo)
        if !isempty(bind_addr)
            return bind_addr, port
        end

        # TODO: Identify root cause and report a better actionable error.
        # Also print unmatched lines?
    end
    close(io)
    if ntries > 0
        error("Timed out waiting to read host:port string from worker.")
    else
        error("Unexpected output from worker launch command. Host:port string not found.")
    end
end

function parse_connection_info(str)
    m = match(r"^julia_worker:(\d+)#(.*)", str)
    if m !== nothing
        (m.captures[2], parse(UInt16, m.captures[1]))
    else
        ("", UInt16(0))
    end
end

"""
    init_worker(cookie::AbstractString, manager::ClusterManager=DefaultClusterManager())

Called by cluster managers implementing custom transports. It initializes a newly launched
process as a worker. Command line argument `--worker[=<cookie>]` has the effect of initializing a
process as a worker using TCP/IP sockets for transport.
`cookie` is a [`cluster_cookie`](@ref).
"""
function init_worker(cookie::AbstractString, manager::ClusterManager=DefaultClusterManager())
    # On workers, the default cluster manager connects via TCP sockets. Custom
    # transports will need to call this function with their own manager.
    global cluster_manager
    cluster_manager = manager

    # Since our pid has yet to be set, ensure no RemoteChannel / Future  have been created or addprocs() called.
    assert(nprocs() <= 1)
    assert(isempty(PGRP.refs))
    assert(isempty(client_refs))

    # System is started in head node mode, cleanup related entries
    empty!(PGRP.workers)
    empty!(map_pid_wrkr)

    cluster_cookie(cookie)
    nothing
end


# The main function for adding worker processes.
# `manager` is of type ClusterManager. The respective managers are responsible
# for launching the workers. All keyword arguments (plus a few default values)
# are available as a dictionary to the `launch` methods
#
# Only one addprocs can be in progress at any time
#
const worker_lock = ReentrantLock()

"""
    addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

Launches worker processes via the specified cluster manager.

For example, Beowulf clusters are supported via a custom cluster manager implemented in
the package `ClusterManagers.jl`.

The number of seconds a newly launched worker waits for connection establishment from the
master can be specified via variable `JULIA_WORKER_TIMEOUT` in the worker process's
environment. Relevant only when using TCP/IP as transport.
"""
function addprocs(manager::ClusterManager; kwargs...)
    cluster_mgmt_from_master_check()

    lock(worker_lock)
    try
        addprocs_locked(manager::ClusterManager; kwargs...)
    finally
        unlock(worker_lock)
    end
end

function addprocs_locked(manager::ClusterManager; kwargs...)
    params = merge(default_addprocs_params(), AnyDict(kwargs))
    topology(Symbol(params[:topology]))

    if PGRP.topology != :all_to_all
        params[:lazy] = false
    end

    if isnull(PGRP.lazy) || nprocs() == 1
        PGRP.lazy = Nullable{Bool}(params[:lazy])
    elseif isclusterlazy() != params[:lazy]
        throw(ArgumentError(string("Active workers with lazy=", isclusterlazy(),
                                    ". Cannot set lazy=", params[:lazy])))
    end

    # References to launched workers, filled when each worker is fully initialized and
    # has connected to all nodes.
    launched_q = Int[]   # Asynchronously filled by the launch method

    # The `launch` method should add an object of type WorkerConfig for every
    # worker launched. It provides information required on how to connect
    # to it.
    launched = WorkerConfig[]
    launch_ntfy = Condition()

    # call manager's `launch` is a separate task. This allows the master
    # process initiate the connection setup process as and when workers come
    # online
    t_launch = @schedule launch(manager, params, launched, launch_ntfy)

    @sync begin
        while true
            if isempty(launched)
                istaskdone(t_launch) && break
                @schedule (sleep(1); notify(launch_ntfy))
                wait(launch_ntfy)
            end

            if !isempty(launched)
                wconfig = shift!(launched)
                let wconfig=wconfig
                    @async setup_launched_worker(manager, wconfig, launched_q)
                end
            end
        end
    end

    wait(t_launch)      # catches any thrown errors from the launch task

    # Since all worker-to-worker setups may not have completed by the time this
    # function returns to the caller, send the complete list to all workers.
    # Useful for nprocs(), nworkers(), etc to return valid values on the workers.
    all_w = workers()
    for pid in all_w
        remote_do(set_valid_processes, pid, all_w)
    end

    sort!(launched_q)
end

function set_valid_processes(plist::Array{Int})
    for pid in setdiff(plist, workers())
        myid() != pid && Worker(pid)
    end
end

default_addprocs_params() = AnyDict(
    :topology => :all_to_all,
    :dir      => pwd(),
    :exename  => joinpath(JULIA_HOME, julia_exename()),
    :exeflags => ``,
    :enable_threaded_blas => false,
    :lazy => true)


function setup_launched_worker(manager, wconfig, launched_q)
    pid = create_worker(manager, wconfig)
    push!(launched_q, pid)

    # When starting workers on remote multi-core hosts, `launch` can (optionally) start only one
    # process on the remote machine, with a request to start additional workers of the
    # same type. This is done by setting an appropriate value to `WorkerConfig.cnt`.
    cnt = get(wconfig.count, 1)
    if cnt === :auto
        cnt = get(wconfig.environ)[:cpu_cores]
    end
    cnt = cnt - 1   # Removing self from the requested number

    if cnt > 0
        launch_n_additional_processes(manager, pid, wconfig, cnt, launched_q)
    end
end


function launch_n_additional_processes(manager, frompid, fromconfig, cnt, launched_q)
    @sync begin
        exename = get(fromconfig.exename)
        exeflags = get(fromconfig.exeflags, ``)
        cmd = `$exename $exeflags`

        new_addresses = remotecall_fetch(launch_additional, frompid, cnt, cmd)
        for address in new_addresses
            (bind_addr, port) = address

            wconfig = WorkerConfig()
            for x in [:host, :tunnel, :sshflags, :exeflags, :exename, :enable_threaded_blas]
                setfield!(wconfig, x, getfield(fromconfig, x))
            end
            wconfig.bind_addr = bind_addr
            wconfig.port = port

            let wconfig=wconfig
                @async begin
                    pid = create_worker(manager, wconfig)
                    remote_do(redirect_output_from_additional_worker, frompid, pid, port)
                    push!(launched_q, pid)
                end
            end
        end
    end
end

function create_worker(manager, wconfig)
    # only node 1 can add new nodes, since nobody else has the full list of address:port
    assert(LPROC.id == 1)

    # initiate a connect. Does not wait for connection completion in case of TCP.
    w = Worker()
    local r_s, w_s
    try
        (r_s, w_s) = connect(manager, w.id, wconfig)
    catch e
        deregister_worker(w.id)
        rethrow(e)
    end

    w = Worker(w.id, r_s, w_s, manager; config=wconfig)
    # install a finalizer to perform cleanup if necessary
    finalizer(w, (w)->if myid() == 1 manage(w.manager, w.id, w.config, :finalize) end)

    # set when the new worker has finshed connections with all other workers
    ntfy_oid = RRID()
    rr_ntfy_join = lookup_ref(ntfy_oid)
    rr_ntfy_join.waitingfor = myid()

    # Start a new task to handle inbound messages from connected worker in master.
    # Also calls `wait_connected` on TCP streams.
    process_messages(w.r_stream, w.w_stream, false)

    # send address information of all workers to the new worker.
    # Cluster managers set the address of each worker in `WorkerConfig.connect_at`.
    # A new worker uses this to setup an all-to-all network if topology :all_to_all is specified.
    # Workers with higher pids connect to workers with lower pids. Except process 1 (master) which
    # initiates connections to all workers.

    # Connection Setup Protocol:
    # - Master sends 16-byte cookie followed by 16-byte version string and a JoinPGRP message to all workers
    # - On each worker
    #   - Worker responds with a 16-byte version followed by a JoinCompleteMsg
    #   - Connects to all workers less than its pid. Sends the cookie, version and an IdentifySocket message
    #   - Workers with incoming connection requests write back their Version and an IdentifySocketAckMsg message
    # - On master, receiving a JoinCompleteMsg triggers rr_ntfy_join (signifies that worker setup is complete)

    join_list = []
    if PGRP.topology == :all_to_all
        # need to wait for lower worker pids to have completed connecting, since the numerical value
        # of pids is relevant to the connection process, i.e., higher pids connect to lower pids and they
        # require the value of config.connect_at which is set only upon connection completion
        for jw in PGRP.workers
            if (jw.id != 1) && (jw.id < w.id)
                (jw.state == W_CREATED) && wait(jw.c_state)
                push!(join_list, jw)
            end
        end

    elseif PGRP.topology == :custom
        # wait for requested workers to be up before connecting to them.
        filterfunc(x) = (x.id != 1) && isdefined(x, :config) && (get(x.config.ident) in get(wconfig.connect_idents, []))

        wlist = filter(filterfunc, PGRP.workers)
        while length(wlist) < length(get(wconfig.connect_idents, []))
            sleep(1.0)
            wlist = filter(filterfunc, PGRP.workers)
        end

        for wl in wlist
            (wl.state == W_CREATED) && wait(wl.c_state)
            push!(join_list, wl)
        end
    end

    all_locs = map(x -> isa(x, Worker) ? (get(x.config.connect_at, ()), x.id) : ((), x.id, true), join_list)
    send_connection_hdr(w, true)
    join_message = JoinPGRPMsg(w.id, all_locs, PGRP.topology, get(wconfig.enable_threaded_blas, false), isclusterlazy())
    send_msg_now(w, MsgHeader(RRID(0,0), ntfy_oid), join_message)

    @schedule manage(w.manager, w.id, w.config, :register)
    wait(rr_ntfy_join)
    lock(client_refs) do
        delete!(PGRP.refs, ntfy_oid)
    end

    return w.id
end


# Called on the first worker on a remote host. Used to optimize launching
# of multiple workers on a remote host (to leverage multi-core)

additional_io_objs=Dict()
function launch_additional(np::Integer, cmd::Cmd)
    io_objs = Vector{Any}(np)
    addresses = Vector{Any}(np)

    for i in 1:np
        io = open(detach(cmd), "r+")
        write_cookie(io)
        io_objs[i] = io.out
    end

    for (i,io) in enumerate(io_objs)
        (host, port) = read_worker_host_port(io)
        addresses[i] = (host, port)
        additional_io_objs[port] = io
    end

    return addresses
end

function redirect_output_from_additional_worker(pid, port)
    io = additional_io_objs[port]
    redirect_worker_output("$pid", io)
    delete!(additional_io_objs, port)
    nothing
end

function check_master_connect()
    timeout = worker_timeout()
    # If we do not have at least process 1 connect to us within timeout
    # we log an error and exit, unless we're running on valgrind
    if ccall(:jl_running_on_valgrind,Cint,()) != 0
        return
    end
    @schedule begin
        start = time()
        while !haskey(map_pid_wrkr, 1) && (time() - start) < timeout
            sleep(1.0)
        end

        if !haskey(map_pid_wrkr, 1)
            print(STDERR, "Master process (id 1) could not connect within $timeout seconds.\nexiting.\n")
            exit(1)
        end
    end
end


"""
    Base.cluster_cookie() -> cookie

Returns the cluster cookie.
"""
cluster_cookie() = LPROC.cookie

"""
    Base.cluster_cookie(cookie) -> cookie

Sets the passed cookie as the cluster cookie, then returns it.
"""
function cluster_cookie(cookie)
    # The cookie must be an ASCII string with length <=  HDR_COOKIE_LEN
    assert(isascii(cookie))
    assert(length(cookie) <= HDR_COOKIE_LEN)

    cookie = rpad(cookie, HDR_COOKIE_LEN)

    LPROC.cookie = cookie
    cookie
end


let next_pid = 2    # 1 is reserved for the client (always)
    global get_next_pid
    function get_next_pid()
        retval = next_pid
        next_pid += 1
        retval
    end
end

mutable struct ProcessGroup
    name::AbstractString
    workers::Array{Any,1}
    refs::Dict                  # global references
    topology::Symbol
    lazy::Nullable{Bool}

    ProcessGroup(w::Array{Any,1}) = new("pg-default", w, Dict(), :all_to_all, Nullable{Bool}())
end
const PGRP = ProcessGroup([])

function topology(t)
    assert(t in [:all_to_all, :master_slave, :custom])
    if (PGRP.topology==t) || ((myid()==1) && (nprocs()==1)) || (myid() > 1)
        PGRP.topology = t
    else
        error("Workers with Topology $(PGRP.topology) already exist. Requested Topology $(t) cannot be set.")
    end
    t
end

function isclusterlazy()
    if isnull(PGRP.lazy)
        return false
    else
        return get(PGRP.lazy)
    end
end

get_bind_addr(pid::Integer) = get_bind_addr(worker_from_id(pid))
get_bind_addr(w::LocalProcess) = LPROC.bind_addr
function get_bind_addr(w::Worker)
    if isnull(w.config.bind_addr)
        if w.id != myid()
            w.config.bind_addr = remotecall_fetch(get_bind_addr, w.id, w.id)
        end
    end
    get(w.config.bind_addr)
end

# globals
const LPROC = LocalProcess()
const HDR_VERSION_LEN=16
const HDR_COOKIE_LEN=16
const map_pid_wrkr = Dict{Int, Union{Worker, LocalProcess}}()
const map_sock_wrkr = ObjectIdDict()
const map_del_wrkr = Set{Int}()

# cluster management related API
"""
    myid()

Get the id of the current process.
"""
myid() = LPROC.id

"""
    nprocs()

Get the number of available processes.
"""
function nprocs()
    if myid() == 1 || (PGRP.topology == :all_to_all && !isclusterlazy())
        n = length(PGRP.workers)
        # filter out workers in the process of being setup/shutdown.
        for jw in PGRP.workers
            if !isa(jw, LocalProcess) && (jw.state != W_CONNECTED)
                n = n - 1
            end
        end
        return n
    else
        return length(PGRP.workers)
    end
end

"""
    nworkers()

Get the number of available worker processes. This is one less than `nprocs()`. Equal to
`nprocs()` if `nprocs() == 1`.
"""
function nworkers()
    n = nprocs()
    n == 1 ? 1 : n-1
end

"""
    procs()

Returns a list of all process identifiers.
"""
function procs()
    if myid() == 1 || (PGRP.topology == :all_to_all  && !isclusterlazy())
        # filter out workers in the process of being setup/shutdown.
        return Int[x.id for x in PGRP.workers if isa(x, LocalProcess) || (x.state == W_CONNECTED)]
    else
        return Int[x.id for x in PGRP.workers]
    end
end

function id_in_procs(id)  # faster version of `id in procs()`
    if myid() == 1 || (PGRP.topology == :all_to_all  && !isclusterlazy())
        for x in PGRP.workers
            if (x.id::Int) == id && (isa(x, LocalProcess) || (x::Worker).state == W_CONNECTED)
                return true
            end
        end
    else
        for x in PGRP.workers
            if (x.id::Int) == id
                return true
            end
        end
    end
    return false
end

"""
    procs(pid::Integer)

Returns a list of all process identifiers on the same physical node.
Specifically all workers bound to the same ip-address as `pid` are returned.
"""
function procs(pid::Integer)
    if myid() == 1
        all_workers = [x for x in PGRP.workers if isa(x, LocalProcess) || (x.state == W_CONNECTED)]
        if (pid == 1) || (isa(map_pid_wrkr[pid].manager, LocalManager))
            Int[x.id for x in filter(w -> (w.id==1) || (isa(w.manager, LocalManager)), all_workers)]
        else
            ipatpid = get_bind_addr(pid)
            Int[x.id for x in filter(w -> get_bind_addr(w) == ipatpid, all_workers)]
        end
    else
        remotecall_fetch(procs, 1, pid)
    end
end

"""
    workers()

Returns a list of all worker process identifiers.
"""
function workers()
    allp = procs()
    if length(allp) == 1
       allp
    else
       filter(x -> x != 1, allp)
    end
end

function cluster_mgmt_from_master_check()
    if myid() != 1
        throw(ErrorException("Only process 1 can add and remove workers"))
    end
end

"""
    rmprocs(pids...; waitfor=typemax(Int))

Removes the specified workers. Note that only process 1 can add or remove
workers.

Argument `waitfor` specifies how long to wait for the workers to shut down:
    - If unspecified, `rmprocs` will wait until all requested `pids` are removed.
    - An `ErrorException` is raised if all workers cannot be terminated before
      the requested `waitfor` seconds.
    - With a `waitfor` value of 0, the call returns immediately with the workers
      scheduled for removal in a different task. The scheduled `Task` object is
      returned. The user should call `wait` on the task before invoking any other
      parallel calls.
"""
function rmprocs(pids...; waitfor=typemax(Int))
    cluster_mgmt_from_master_check()

    pids = vcat(pids...)
    if waitfor == 0
        t = @schedule _rmprocs(pids, typemax(Int))
        yield()
        return t
    else
        _rmprocs(pids, waitfor)
        # return a dummy task object that user code can wait on.
        return @schedule nothing
    end
end

function _rmprocs(pids, waitfor)
    lock(worker_lock)
    try
        rmprocset = []
        for p in vcat(pids...)
            if p == 1
                warn("rmprocs: process 1 not removed")
            else
                if haskey(map_pid_wrkr, p)
                    w = map_pid_wrkr[p]
                    set_worker_state(w, W_TERMINATING)
                    kill(w.manager, p, w.config)
                    push!(rmprocset, w)
                end
            end
        end

        start = time()
        while (time() - start) < waitfor
            all(w -> w.state == W_TERMINATED, rmprocset) && break
            sleep(min(0.1, waitfor - (time() - start)))
        end

        unremoved = [wrkr.id for wrkr in filter(w -> w.state != W_TERMINATED, rmprocset)]
        if length(unremoved) > 0
            estr = string("rmprocs: pids ", unremoved, " not terminated after ", waitfor, " seconds.")
            throw(ErrorException(estr))
        end
    finally
        unlock(worker_lock)
    end
end


"""
    ProcessExitedException()

After a client Julia process has exited, further attempts to reference the dead child will
throw this exception.
"""
ProcessExitedException()
mutable struct ProcessExitedException <: Exception end

worker_from_id(i) = worker_from_id(PGRP, i)
function worker_from_id(pg::ProcessGroup, i)
    if !isempty(map_del_wrkr) && in(i, map_del_wrkr)
        throw(ProcessExitedException())
    end
    w = get(map_pid_wrkr, i, nothing)
    if w === nothing
        if myid() == 1
            error("no process with id $i exists")
        end
        w = Worker(i)
        map_pid_wrkr[i] = w
    else
        w = w::Union{Worker, LocalProcess}
    end
    w
end

"""
    Base.worker_id_from_socket(s) -> pid

A low-level API which given a `IO` connection or a `Worker`,
returns the `pid` of the worker it is connected to.
This is useful when writing custom [`serialize`](@ref) methods for a type,
which optimizes the data written out depending on the receiving process id.
"""
function worker_id_from_socket(s)
    w = get(map_sock_wrkr, s, nothing)
    if isa(w,Worker)
        if s === w.r_stream || s === w.w_stream
            return w.id
        end
    end
    if isa(s,IOStream) && fd(s)==-1
        # serializing to a local buffer
        return myid()
    end
    return -1
end


register_worker(w) = register_worker(PGRP, w)
function register_worker(pg, w)
    push!(pg.workers, w)
    map_pid_wrkr[w.id] = w
end

function register_worker_streams(w)
    map_sock_wrkr[w.r_stream] = w
    map_sock_wrkr[w.w_stream] = w
end

deregister_worker(pid) = deregister_worker(PGRP, pid)
function deregister_worker(pg, pid)
    pg.workers = filter(x -> !(x.id == pid), pg.workers)
    w = pop!(map_pid_wrkr, pid, nothing)
    if isa(w, Worker)
        if isdefined(w, :r_stream)
            pop!(map_sock_wrkr, w.r_stream, nothing)
            if w.r_stream != w.w_stream
                pop!(map_sock_wrkr, w.w_stream, nothing)
            end
        end

        if myid() == 1 && isdefined(w, :config)
            # Notify the cluster manager of this workers death
            manage(w.manager, w.id, w.config, :deregister)
            if PGRP.topology != :all_to_all || isclusterlazy()
                for rpid in workers()
                    try
                        remote_do(deregister_worker, rpid, pid)
                    catch
                    end
                end
            end
        end
    end
    push!(map_del_wrkr, pid)

    # delete this worker from our remote reference client sets
    ids = []
    tonotify = []
    lock(client_refs) do
        for (id,rv) in pg.refs
            if in(pid,rv.clientset)
                push!(ids, id)
            end
            if rv.waitingfor == pid
                push!(tonotify, (id,rv))
            end
        end
        for id in ids
            del_client(pg, id, pid)
        end

        # throw exception to tasks waiting for this pid
        for (id,rv) in tonotify
            notify_error(rv.c, ProcessExitedException())
            delete!(pg.refs, id)
        end
    end
end


function interrupt(pid::Integer)
    assert(myid() == 1)
    w = map_pid_wrkr[pid]
    if isa(w, Worker)
        manage(w.manager, w.id, w.config, :interrupt)
    end
end

"""
    interrupt(pids::Integer...)

Interrupt the current executing task on the specified workers. This is equivalent to
pressing Ctrl-C on the local machine. If no arguments are given, all workers are interrupted.
"""
interrupt(pids::Integer...) = interrupt([pids...])

"""
    interrupt(pids::AbstractVector=workers())

Interrupt the current executing task on the specified workers. This is equivalent to
pressing Ctrl-C on the local machine. If no arguments are given, all workers are interrupted.
"""
function interrupt(pids::AbstractVector=workers())
    assert(myid() == 1)
    @sync begin
        for pid in pids
            @async interrupt(pid)
        end
    end
end


function disable_nagle(sock)
    # disable nagle on all OSes
    ccall(:uv_tcp_nodelay, Cint, (Ptr{Void}, Cint), sock.handle, 1)
    @static if Sys.islinux()
        # tcp_quickack is a linux only option
        if ccall(:jl_tcp_quickack, Cint, (Ptr{Void}, Cint), sock.handle, 1) < 0
            warn_once("Networking unoptimized ( Error enabling TCP_QUICKACK : ", Libc.strerror(Libc.errno()), " )")
        end
    end
end

function check_same_host(pids)
    if myid() != 1
        return remotecall_fetch(check_same_host, 1, pids)
    else
        # We checkfirst if all test pids have been started using the local manager,
        # else we check for the same bind_to addr. This handles the special case
        # where the local ip address may change - as during a system sleep/awake
        if all(p -> (p==1) || (isa(map_pid_wrkr[p].manager, LocalManager)), pids)
            return true
        else
            first_bind_addr = get(map_pid_wrkr[pids[1]].config.bind_addr)
            return all(p -> (p != 1) && (get(map_pid_wrkr[p].config.bind_addr) == first_bind_addr), pids[2:end])
        end
    end
end

function terminate_all_workers()
    myid() != 1 && return

    if nprocs() > 1
        try
            rmprocs(workers(); waitfor=5.0)
        catch _ex
            warn("Forcibly interrupting busy workers")
            # Might be computation bound, interrupt them and try again
            interrupt(workers())
            try
                rmprocs(workers(); waitfor=5.0)
            catch _ex2
                warn("Unable to terminate all workers")
            end
        end
    end
end

# initialize the local proc network address / port
function init_bind_addr()
    opts = JLOptions()
    if opts.bindto != C_NULL
        bind_to = split(unsafe_string(opts.bindto), ":")
        bind_addr = string(parse(IPAddr, bind_to[1]))
        if length(bind_to) > 1
            bind_port = parse(Int,bind_to[2])
        else
            bind_port = 0
        end
    else
        bind_port = 0
        try
            bind_addr = string(getipaddr())
        catch
            # All networking is unavailable, initialize bind_addr to the loopback address
            # Will cause an exception to be raised only when used.
            bind_addr = "127.0.0.1"
        end
    end
    global LPROC
    LPROC.bind_addr = bind_addr
    LPROC.bind_port = UInt16(bind_port)
end

function init_parallel()
    start_gc_msgs_task()
    atexit(terminate_all_workers)

    init_bind_addr()

    # start in "head node" mode, if worker, will override later.
    global PGRP
    global LPROC
    LPROC.id = 1
    cluster_cookie(randstring(HDR_COOKIE_LEN))
    assert(isempty(PGRP.workers))
    register_worker(LPROC)
end

write_cookie(io::IO) = write(io.in, string(cluster_cookie(), "\n"))
