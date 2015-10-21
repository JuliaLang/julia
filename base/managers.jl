# This file is a part of Julia. License is MIT: http://julialang.org/license

# Built-in SSH and Local Managers

immutable SSHManager <: ClusterManager
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
    for keyname in kwargs
        !(keyname[1] in [:dir, :exename, :exeflags, :topology]) && throw(ArgumentError("Invalid keyword argument $(keyname[1])"))
    end
end

# SSHManager

# start and connect to processes via SSH, optionally through an SSH tunnel.
# the tunnel is only used from the head (process 1); the nodes are assumed
# to be mutually reachable without a tunnel, as is often the case in a cluster.
# Default value of kw arg max_parallel is the default value of MaxStartups in sshd_config
# A machine is either a <hostname> or a tuple of (<hostname>, count)
function addprocs(machines::AbstractVector; tunnel=false, sshflags=``, max_parallel=10, kwargs...)
    check_addprocs_args(kwargs)
    addprocs(SSHManager(machines); tunnel=tunnel, sshflags=sshflags, max_parallel=max_parallel, kwargs...)
end


function launch(manager::SSHManager, params::Dict, launched::Array, launch_ntfy::Condition)
    # Launch one worker on each unique host in parallel. Additional workers are launched later.
    # Wait for all launches to complete.
    launch_tasks = cell(length(manager.machines))

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
        throw(ArgumentError("invalid machine defintion format string: invalid port format \"$machine_def\""))
    end
    host = machine_def[1]
    portopt = ``
    if length(machine_def) == 2
        portstr = machine_def[2]
        if !isinteger(portstr) || (p = parse(Int,portstr); p < 1 || p > 65535)
            msg = "invalid machine definition format string: invalid port format \"$machine_def\""
            throw(ArgumentError(msg))
        end
        portopt = ` -p $(machine_def[2]) `
    end
    sshflags = `$(params[:sshflags]) $portopt`

    # Build up the ssh command

    # the default worker timeout
    tval = haskey(ENV, "JULIA_WORKER_TIMEOUT") ?
        `export JULIA_WORKER_TIMEOUT=$(ENV["JULIA_WORKER_TIMEOUT"]);` : ``

    # Julia process with passed in command line flag arguments
    cmd = `cd $dir && $tval $exename $exeflags`

    # shell login (-l) with string command (-c) to launch julia process
    cmd = `sh -l -c $(shell_escape(cmd))`

    # remote launch with ssh with given ssh flags / host / port information
    # -T → disable pseudo-terminal allocation
    # -a → disable forwarding of auth agent connection
    # -x → disable X11 forwarding
    # -o ClearAllForwardings → option if forwarding connections and
    #                          forwarded connections are causing collisions
    # -n → Redirects stdin from /dev/null (actually, prevents reading from stdin).
    #      Used when running ssh in the background.
    cmd = `ssh -T -a -x -o ClearAllForwardings=yes -n $sshflags $host $(shell_escape(cmd))`

    # launch the remote Julia process

    # detach launches the command in a new process group, allowing it to outlive
    # the initial julia process (Ctrl-C and teardown methods are handled through messages)
    # for the launched porcesses.
    io, pobj = open(detach(cmd), "r")

    wconfig = WorkerConfig()
    wconfig.io = io
    wconfig.host = host
    wconfig.tunnel = params[:tunnel]
    wconfig.sshflags = sshflags
    wconfig.exeflags = exeflags
    wconfig.exename = exename
    wconfig.count = cnt
    wconfig.max_parallel = params[:max_parallel]

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
Returns a port number `localport` such that `localhost:localport` connects to `host:port`.
"""
function ssh_tunnel(user, host, bind_addr, port, sshflags)
    port = Int(port)
    cnt  = 100
    localport = next_tunnel_port()
    # if we cannot do port forwarding, bail immediately
    # the connection is forwarded to `port` on the remote server over the local port `localport`
    # the -f option backgrounds the ssh session
    # `sleep 60` command specifies that an alloted time of 60 seconds is allowed to start the
    # remote julia process and estabilish the network connections specified the the process topology.
    # If no connections are made within 60 seconds, ssh will exit and an error will be printed on the
    # process that launched the remote process.
    ssh = `ssh -T -a -x -o ExitOnForwardFailure=yes`
    while !success(detach(`$ssh -f $sshflags $user@$host -L $localport:$bind_addr:$port sleep 60`)) && cnt > 0
        localport = next_tunnel_port()
        cnt -= 1
    end
    if cnt == 0
        throw(ErrorException(
            "unable to create SSH tunnel after $cnt tries. No free port?"))
    end
    return localport
end


# LocalManager

immutable LocalManager <: ClusterManager
    np::Integer
end

addprocs(; kwargs...) = addprocs(Sys.CPU_CORES; kwargs...)
function addprocs(np::Integer; kwargs...)
    check_addprocs_args(kwargs)
    addprocs(LocalManager(np); kwargs...)
end

show(io::IO, manager::LocalManager) = println(io, "LocalManager()")

function launch(manager::LocalManager, params::Dict, launched::Array, c::Condition)
    dir = params[:dir]
    exename = params[:exename]
    exeflags = params[:exeflags]

    for i in 1:manager.np
        io, pobj = open(detach(
            setenv(`$(julia_cmd(exename)) $exeflags --bind-to $(LPROC.bind_addr) --worker`, dir=dir)), "r")
        wconfig = WorkerConfig()
        wconfig.process = pobj
        wconfig.io = io
        push!(launched, wconfig)
    end

    notify(c)
end

function manage(manager::LocalManager, id::Integer, config::WorkerConfig, op::Symbol)
    if op == :interrupt
        kill(get(config.process), 2)
    end
end


# DefaultClusterManager for the default TCP transport - used by both SSHManager and LocalManager

immutable DefaultClusterManager <: ClusterManager
end

const tunnel_hosts_map = Dict{AbstractString, Semaphore}()

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
    if get(get(config.environ), :self_is_local, false) && get(get(config.environ), :r_is_local, false)
        # If on localhost, use the loopback address - this addresses
        # the special case of system suspend wherein the local ip
        # may be changed upon system awake.
        (s, bind_addr) = connect_to_worker("127.0.0.1", rport)
    else
        (s, bind_addr)= connect_to_worker(rhost, rport)
    end

    (s,s)
end

const client_port = Ref{Cushort}(0)

function socket_reuse_port()
    s = TCPSocket()
    client_host = Ref{Cuint}(0)
    ccall(:jl_tcp_bind, Int32,
            (Ptr{Void}, UInt16, UInt32, Cuint),
            s.handle, hton(client_port.x), hton(UInt32(0)), 0) < 0 && throw(SystemError("bind() : "))

    # TODO: Support OSX and change the above code to call setsockopt before bind once libuv provides
    # early access to a socket fd, i.e., before a bind call.

    @linux_only begin
        try
            rc = ccall(:jl_tcp_reuseport, Int32, (Ptr{Void}, ), s.handle)
            if rc > 0  # SO_REUSEPORT is unsupported, just return the ephemerally bound socket
                return s
            elseif rc < 0
                throw(SystemError("setsockopt() SO_REUSEPORT : "))
            end
            getsockname(s)
        catch e
            # This is an issue only on systems with lots of client connections, hence delay the warning....
            nworkers() > 128 && warn_once("Error trying to reuse client port number, falling back to plain socket : ", e)
            # provide a clean new socket
            return TCPSocket()
        end
    end
    return s
end

function connect_to_worker(host::AbstractString, port::Integer)
    # Connect to the loopback port if requested host has the same ipaddress as self.
    s = socket_reuse_port()
    if host == string(LPROC.bind_addr)
        s = connect(s, "127.0.0.1", UInt16(port))
    else
        s = connect(s, host, UInt16(port))
    end

    # Avoid calling getaddrinfo if possible - involves a DNS lookup
    # host may be a stringified ipv4 / ipv6 address or a dns name
    if host == "localhost"
        bind_addr = "127.0.0.1"
    else
        try
            bind_addr = string(parseip(host))
        catch
            bind_addr = string(getaddrinfo(host))
        end
    end
    (s, bind_addr)
end


function connect_to_worker(host::AbstractString, bind_addr::AbstractString, port::Integer, tunnel_user::AbstractString, sshflags)
    s = connect("localhost", ssh_tunnel(tunnel_user, host, bind_addr, UInt16(port), sshflags))
    (s, bind_addr)
end

function kill(manager::ClusterManager, pid::Int, config::WorkerConfig)
    remote_do(exit, pid) # For TCP based transports this will result in a close of the socket
                       # at our end, which will result in a cleanup of the worker.
    nothing
end


