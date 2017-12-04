# This file is a part of Julia. License is MIT: https://julialang.org/license

# Built-in SSH and Local Managers

struct SSHManager <: ClusterManager
    machines::Dict

    function SSHManager(machines)
        # machines => array of machine elements
        # machine => address or (address, cnt)
        # address => string of form `[user@]host[:port] bind_addr[:bind_port]`
        # cnt => :auto or number
        # :auto launches NUM_CORES number of workers at address
        # number launches the specified number of workers at address
        mhist = Dict()
        for m in machines
            if isa(m, Tuple)
                host=m[1]
                cnt=m[2]
            else
                host=m
                cnt=1
            end
            current_cnt = get(mhist, host, 0)

            if isa(cnt, Number)
                mhist[host] = isa(current_cnt, Number) ? current_cnt + Int(cnt) : Int(cnt)
            else
                mhist[host] = cnt
            end
        end
        new(mhist)
    end
end


function check_addprocs_args(kwargs)
    valid_kw_names = collect(keys(default_addprocs_params()))
    for keyname in keys(kwargs)
        !(keyname in valid_kw_names) && throw(ArgumentError("Invalid keyword argument $(keyname)"))
    end
end

# SSHManager

# start and connect to processes via SSH, optionally through an SSH tunnel.
# the tunnel is only used from the head (process 1); the nodes are assumed
# to be mutually reachable without a tunnel, as is often the case in a cluster.
# Default value of kw arg max_parallel is the default value of MaxStartups in sshd_config
# A machine is either a <hostname> or a tuple of (<hostname>, count)
"""
    addprocs(machines; tunnel=false, sshflags=\`\`, max_parallel=10, kwargs...) -> List of process identifiers

Add processes on remote machines via SSH. Requires `julia` to be installed in the same
location on each node, or to be available via a shared file system.

`machines` is a vector of machine specifications. Workers are started for each specification.

A machine specification is either a string `machine_spec` or a tuple - `(machine_spec, count)`.

`machine_spec` is a string of the form `[user@]host[:port] [bind_addr[:port]]`. `user`
defaults to current user, `port` to the standard ssh port. If `[bind_addr[:port]]` is
specified, other workers will connect to this worker at the specified `bind_addr` and
`port`.

`count` is the number of workers to be launched on the specified host. If specified as
`:auto` it will launch as many workers as the number of cores on the specific host.

Keyword arguments:

* `tunnel`: if `true` then SSH tunneling will be used to connect to the worker from the
  master process. Default is `false`.

* `sshflags`: specifies additional ssh options, e.g. ```sshflags=\`-i /home/foo/bar.pem\````

* `max_parallel`: specifies the maximum number of workers connected to in parallel at a
  host. Defaults to 10.

* `dir`: specifies the working directory on the workers. Defaults to the host's current
  directory (as found by `pwd()`)

* `enable_threaded_blas`: if `true` then  BLAS will run on multiple threads in added
  processes. Default is `false`.

* `exename`: name of the `julia` executable. Defaults to `"\$JULIA_HOME/julia"` or
  `"\$JULIA_HOME/julia-debug"` as the case may be.

* `exeflags`: additional flags passed to the worker processes.

* `topology`: Specifies how the workers connect to each other. Sending a message between
  unconnected workers results in an error.

    + `topology=:all_to_all`: All processes are connected to each other. The default.

    + `topology=:master_slave`: Only the driver process, i.e. `pid` 1 connects to the
      workers. The workers do not connect to each other.

    + `topology=:custom`: The `launch` method of the cluster manager specifies the
      connection topology via fields `ident` and `connect_idents` in `WorkerConfig`.
      A worker with a cluster manager identity `ident` will connect to all workers specified
      in `connect_idents`.

* `lazy`: Applicable only with `topology=:all_to_all`. If `true`, worker-worker connections
  are setup lazily, i.e. they are setup at the first instance of a remote call between
  workers. Default is true.


Environment variables :

If the master process fails to establish a connection with a newly launched worker within
60.0 seconds, the worker treats it as a fatal situation and terminates.
This timeout can be controlled via environment variable `JULIA_WORKER_TIMEOUT`.
The value of `JULIA_WORKER_TIMEOUT` on the master process specifies the number of seconds a
newly launched worker waits for connection establishment.
"""
function addprocs(machines::AbstractVector; tunnel=false, sshflags=``, max_parallel=10, kwargs...)
    check_addprocs_args(kwargs)
    addprocs(SSHManager(machines); tunnel=tunnel, sshflags=sshflags, max_parallel=max_parallel, kwargs...)
end


function launch(manager::SSHManager, params::Dict, launched::Array, launch_ntfy::Condition)
    # Launch one worker on each unique host in parallel. Additional workers are launched later.
    # Wait for all launches to complete.
    launch_tasks = Vector{Any}(uninitialized, length(manager.machines))

    for (i,(machine, cnt)) in enumerate(manager.machines)
        let machine=machine, cnt=cnt
            launch_tasks[i] = @schedule try
                    launch_on_machine(manager, machine, cnt, params, launched, launch_ntfy)
                catch e
                    print(STDERR, "exception launching on machine $(machine) : $(e)\n")
                end
        end
    end

    for t in launch_tasks
        wait(t)
    end

    notify(launch_ntfy)
end


show(io::IO, manager::SSHManager) = println(io, "SSHManager(machines=", manager.machines, ")")


function launch_on_machine(manager::SSHManager, machine, cnt, params, launched, launch_ntfy::Condition)
    dir = params[:dir]
    exename = params[:exename]
    exeflags = params[:exeflags]

    # machine could be of the format [user@]host[:port] bind_addr[:bind_port]
    # machine format string is split on whitespace
    machine_bind = split(machine)
    if isempty(machine_bind)
        throw(ArgumentError("invalid machine definition format string: \"$machine\$"))
    end
    if length(machine_bind) > 1
        exeflags = `--bind-to $(machine_bind[2]) $exeflags`
    end
    exeflags = `$exeflags --worker`

    machine_def = split(machine_bind[1], ':')
    # if this machine def has a port number, add the port information to the ssh flags
    if length(machine_def) > 2
        throw(ArgumentError("invalid machine definition format string: invalid port format \"$machine_def\""))
    end
    host = machine_def[1]
    portopt = ``
    if length(machine_def) == 2
        portstr = machine_def[2]
        if !all(isdigit, portstr) || (p = parse(Int,portstr); p < 1 || p > 65535)
            msg = "invalid machine definition format string: invalid port format \"$machine_def\""
            throw(ArgumentError(msg))
        end
        portopt = ` -p $(machine_def[2]) `
    end
    sshflags = `$(params[:sshflags]) $portopt`

    # Build up the ssh command

    # the default worker timeout
    tval = get(ENV, "JULIA_WORKER_TIMEOUT", "")

    # Julia process with passed in command line flag arguments
    cmds = """
        cd -- $(shell_escape_posixly(dir))
        $(isempty(tval) ? "" : "export JULIA_WORKER_TIMEOUT=$(shell_escape_posixly(tval))")
        $(shell_escape_posixly(exename)) $(shell_escape_posixly(exeflags))"""

    # shell login (-l) with string command (-c) to launch julia process
    cmd = `sh -l -c $cmds`

    # remote launch with ssh with given ssh flags / host / port information
    # -T → disable pseudo-terminal allocation
    # -a → disable forwarding of auth agent connection
    # -x → disable X11 forwarding
    # -o ClearAllForwardings → option if forwarding connections and
    #                          forwarded connections are causing collisions
    cmd = `ssh -T -a -x -o ClearAllForwardings=yes $sshflags $host $(shell_escape_posixly(cmd))`

    # launch the remote Julia process

    # detach launches the command in a new process group, allowing it to outlive
    # the initial julia process (Ctrl-C and teardown methods are handled through messages)
    # for the launched processes.
    io = open(detach(cmd), "r+")
    write_cookie(io)

    wconfig = WorkerConfig()
    wconfig.io = io.out
    wconfig.host = host
    wconfig.tunnel = params[:tunnel]
    wconfig.sshflags = sshflags
    wconfig.exeflags = exeflags
    wconfig.exename = exename
    wconfig.count = cnt
    wconfig.max_parallel = params[:max_parallel]
    wconfig.enable_threaded_blas = params[:enable_threaded_blas]


    push!(launched, wconfig)
    notify(launch_ntfy)
end


function manage(manager::SSHManager, id::Integer, config::WorkerConfig, op::Symbol)
    if op == :interrupt
        ospid = get(config.ospid, 0)
        if ospid > 0
            host = get(config.host)
            sshflags = get(config.sshflags)
            if !success(`ssh -T -a -x -o ClearAllForwardings=yes -n $sshflags $host "kill -2 $ospid"`)
                warn(STDERR,"error sending a Ctrl-C to julia worker $id on $host")
            end
        else
            # This state can happen immediately after an addprocs
            warn(STDERR,"worker $id cannot be presently interrupted.")
        end
    end
end

let tunnel_port = 9201
    global next_tunnel_port
    function next_tunnel_port()
        retval = tunnel_port
        if tunnel_port > 32000
            tunnel_port = 9201
        else
            tunnel_port += 1
        end
        retval
    end
end


"""
    ssh_tunnel(user, host, bind_addr, port, sshflags) -> localport

Establish an SSH tunnel to a remote worker.
Return a port number `localport` such that `localhost:localport` connects to `host:port`.
"""
function ssh_tunnel(user, host, bind_addr, port, sshflags)
    port = Int(port)
    cnt = ntries = 100
    # if we cannot do port forwarding, bail immediately
    # the connection is forwarded to `port` on the remote server over the local port `localport`
    # the -f option backgrounds the ssh session
    # `sleep 60` command specifies that an alloted time of 60 seconds is allowed to start the
    # remote julia process and establish the network connections specified by the process topology.
    # If no connections are made within 60 seconds, ssh will exit and an error will be printed on the
    # process that launched the remote process.
    ssh = `ssh -T -a -x -o ExitOnForwardFailure=yes`
    while cnt > 0
        localport = next_tunnel_port()
        if success(detach(`$ssh -f $sshflags $user@$host -L $localport:$bind_addr:$port sleep 60`))
            return localport
        end
        cnt -= 1
    end

    throw(ErrorException(
        string("unable to create SSH tunnel after ", ntries, " tries. No free port?")))
end


# LocalManager
struct LocalManager <: ClusterManager
    np::Integer
    restrict::Bool  # Restrict binding to 127.0.0.1 only
end

"""
    addprocs(; kwargs...) -> List of process identifiers

Equivalent to `addprocs(Sys.CPU_CORES; kwargs...)`

Note that workers do not run a `.juliarc.jl` startup script, nor do they synchronize their
global state (such as global variables, new method definitions, and loaded modules) with any
of the other running processes.
"""
addprocs(; kwargs...) = addprocs(Sys.CPU_CORES; kwargs...)

"""
    addprocs(np::Integer; restrict=true, kwargs...) -> List of process identifiers

Launches workers using the in-built `LocalManager` which only launches workers on the
local host. This can be used to take advantage of multiple cores. `addprocs(4)` will add 4
processes on the local machine. If `restrict` is `true`, binding is restricted to
`127.0.0.1`. Keyword args `dir`, `exename`, `exeflags`, `topology`, `lazy` and
`enable_threaded_blas` have the same effect as documented for `addprocs(machines)`.
"""
function addprocs(np::Integer; restrict=true, kwargs...)
    check_addprocs_args(kwargs)
    addprocs(LocalManager(np, restrict); kwargs...)
end

show(io::IO, manager::LocalManager) = println(io, "LocalManager()")

function launch(manager::LocalManager, params::Dict, launched::Array, c::Condition)
    dir = params[:dir]
    exename = params[:exename]
    exeflags = params[:exeflags]
    bind_to = manager.restrict ? `127.0.0.1` : `$(LPROC.bind_addr)`

    for i in 1:manager.np
        cmd = `$(julia_cmd(exename)) $exeflags --bind-to $bind_to --worker`
        io = open(detach(setenv(cmd, dir=dir)), "r+")
        write_cookie(io)

        wconfig = WorkerConfig()
        wconfig.process = io
        wconfig.io = io.out
        wconfig.enable_threaded_blas = params[:enable_threaded_blas]
        push!(launched, wconfig)
    end

    notify(c)
end

function manage(manager::LocalManager, id::Integer, config::WorkerConfig, op::Symbol)
    if op == :interrupt
        kill(get(config.process), 2)
    end
end

"""
    launch(manager::ClusterManager, params::Dict, launched::Array, launch_ntfy::Condition)

Implemented by cluster managers. For every Julia worker launched by this function, it should
append a `WorkerConfig` entry to `launched` and notify `launch_ntfy`. The function MUST exit
once all workers, requested by `manager` have been launched. `params` is a dictionary of all
keyword arguments [`addprocs`](@ref) was called with.
"""
launch

"""
    manage(manager::ClusterManager, id::Integer, config::WorkerConfig. op::Symbol)

Implemented by cluster managers. It is called on the master process, during a worker's
lifetime, with appropriate `op` values:

- with `:register`/`:deregister` when a worker is added / removed from the Julia worker pool.
- with `:interrupt` when `interrupt(workers)` is called. The `ClusterManager`
  should signal the appropriate worker with an interrupt signal.
- with `:finalize` for cleanup purposes.
"""
manage

# DefaultClusterManager for the default TCP transport - used by both SSHManager and LocalManager

struct DefaultClusterManager <: ClusterManager
end

const tunnel_hosts_map = Dict{AbstractString, Semaphore}()

"""
    connect(manager::ClusterManager, pid::Int, config::WorkerConfig) -> (instrm::IO, outstrm::IO)

Implemented by cluster managers using custom transports. It should establish a logical
connection to worker with id `pid`, specified by `config` and return a pair of `IO`
objects. Messages from `pid` to current process will be read off `instrm`, while messages to
be sent to `pid` will be written to `outstrm`. The custom transport implementation must
ensure that messages are delivered and received completely and in order.
`connect(manager::ClusterManager.....)` sets up TCP/IP socket connections in-between
workers.
"""
function connect(manager::ClusterManager, pid::Int, config::WorkerConfig)
    if !isnull(config.connect_at)
        # this is a worker-to-worker setup call.
        return connect_w2w(pid, config)
    end

    # master connecting to workers
    if !isnull(config.io)
        (bind_addr, port) = read_worker_host_port(get(config.io))
        pubhost=get(config.host, bind_addr)
        config.host = pubhost
        config.port = port
    else
        pubhost=get(config.host)
        port=get(config.port)
        bind_addr=get(config.bind_addr, pubhost)
    end

    tunnel = get(config.tunnel, false)

    s = split(pubhost,'@')
    user = ""
    if length(s) > 1
        user = s[1]
        pubhost = s[2]
    else
        if haskey(ENV, "USER")
            user = ENV["USER"]
        elseif tunnel
            error("USER must be specified either in the environment ",
                  "or as part of the hostname when tunnel option is used")
        end
    end

    if tunnel
        if !haskey(tunnel_hosts_map, pubhost)
            tunnel_hosts_map[pubhost] = Semaphore(get(config.max_parallel, typemax(Int)))
        end
        sem = tunnel_hosts_map[pubhost]

        sshflags = get(config.sshflags)
        acquire(sem)
        try
            (s, bind_addr) = connect_to_worker(pubhost, bind_addr, port, user, sshflags)
        finally
            release(sem)
        end
    else
        (s, bind_addr) = connect_to_worker(bind_addr, port)
    end

    config.bind_addr = bind_addr

    # write out a subset of the connect_at required for further worker-worker connection setups
    config.connect_at = (bind_addr, port)

    if !isnull(config.io)
        let pid = pid
            redirect_worker_output(pid, get(config.io))
        end
    end

    (s, s)
end

function connect_w2w(pid::Int, config::WorkerConfig)
    (rhost, rport) = get(config.connect_at)
    config.host = rhost
    config.port = rport
    (s, bind_addr) = connect_to_worker(rhost, rport)
    (s,s)
end

const client_port = Ref{Cushort}(0)

function socket_reuse_port()
    if ccall(:jl_has_so_reuseport, Int32, ()) == 1
        s = TCPSocket(delay = false)

        # Some systems (e.g. Linux) require the port to be bound before setting REUSEPORT
        bind_early = Sys.islinux()

        bind_early && bind_client_port(s)
        rc = ccall(:jl_tcp_reuseport, Int32, (Ptr{Void},), s.handle)
        if rc < 0
            # This is an issue only on systems with lots of client connections, hence delay the warning
            nworkers() > 128 && warn_once("Error trying to reuse client port number, falling back to regular socket.")

            # provide a clean new socket
            return TCPSocket()
        end
        bind_early || bind_client_port(s)
        return s
    else
        return TCPSocket()
    end
end

function bind_client_port(s)
    err = ccall(:jl_tcp_bind, Int32, (Ptr{Void}, UInt16, UInt32, Cuint),
                            s.handle, hton(client_port[]), hton(UInt32(0)), 0)
    uv_error("bind() failed", err)

    _addr, port = getsockname(s)
    client_port[] = port
    return s
end

function connect_to_worker(host::AbstractString, port::Integer)
    s = socket_reuse_port()
    connect(s, host, UInt16(port))

    # Avoid calling getaddrinfo if possible - involves a DNS lookup
    # host may be a stringified ipv4 / ipv6 address or a dns name
    bind_addr = nothing
    try
        bind_addr = string(parse(IPAddr,host))
    catch
        bind_addr = string(getaddrinfo(host))
    end
    (s, bind_addr)
end


function connect_to_worker(host::AbstractString, bind_addr::AbstractString, port::Integer, tunnel_user::AbstractString, sshflags)
    s = connect("localhost", ssh_tunnel(tunnel_user, host, bind_addr, UInt16(port), sshflags))
    (s, bind_addr)
end


"""
    kill(manager::ClusterManager, pid::Int, config::WorkerConfig)

Implemented by cluster managers.
It is called on the master process, by [`rmprocs`](@ref).
It should cause the remote worker specified by `pid` to exit.
`kill(manager::ClusterManager.....)` executes a remote `exit()`
on `pid`.
"""
function kill(manager::ClusterManager, pid::Int, config::WorkerConfig)
    remote_do(exit, pid) # For TCP based transports this will result in a close of the socket
                       # at our end, which will result in a cleanup of the worker.
    nothing
end
