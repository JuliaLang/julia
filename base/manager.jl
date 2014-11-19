# All functionality related to startup and management of worker processes
# Implementations of LocalManager and SSHManager

abstract ClusterManager

immutable SSHManager <: ClusterManager
    machines::Dict

    function SSHManager(; machines=[])
        mhist = Dict()
        for m in machines
            cnt = get(mhist, m, 0)
            mhist[m] = cnt + 1
        end
        new(mhist)
    end
end

show(io::IO, manager::SSHManager) = println("SSHManager(machines=", manager.machines, ")")

function launch{T<:SSHManager}(::Type{T}, config::Dict, launched::Array, machines_launch_ntfy::Condition)
    # Launch on each unique host in parallel.
    # Wait for all launches to complete.

    plaunch_ntfy = Condition()
    manager = config[:manager]
    launch_tasks = cell(length(manager.machines))

    for (i,(machine, cnt)) in  enumerate(manager.machines)
        let machine=machine, cnt=cnt
            launch_tasks[i] = @schedule begin
                try
                    launch_on_machine(manager, machine, config, launched, machines_launch_ntfy, cnt, plaunch_ntfy)
                catch e
                    println(e)
                end
            end
        end
    end

    while length(launch_tasks) > 0
        if istaskdone(launch_tasks[1])
            shift!(launch_tasks)
        else
            wait(plaunch_ntfy)
        end
    end

    notify(machines_launch_ntfy)
end


function launch_on_machine(manager::SSHManager, machine, config, launched, machines_launch_ntfy::Condition,
                           cnt, plaunch_ntfy::Condition)
    # We don't expect dir, exename and exeflags flags to change for each worker on this machine

    dir = config[:dir]
    exename = config[:exename]
    exeflags_base = config[:exeflags]

    thisconfig = copy(config) # config for workers on this machine

    # machine could be of the format [user@]host[:port] bind_addr[:bind_port]
    machine_bind = split(machine)
    if length(machine_bind) > 1
        exeflags = `--bind-to $(machine_bind[2]) $exeflags_base`
    else
        exeflags = exeflags_base
    end
    machine_def = machine_bind[1]

    machine_def = split(machine_def, ':')
    portopt = length(machine_def) == 2 ? ` -p $(machine_def[2]) ` : ``
    sshflags = `$(config[:sshflags]) $portopt`
    thisconfig[:sshflags] = sshflags

    host = machine_def[1]
    thisconfig[:host] = host

    # Build up the ssh command
    cmd = `cd $dir && $exename $exeflags` # launch julia
    cmd = `sh -l -c $(shell_escape(cmd))` # shell to launch under
    cmd = `ssh -T -a -x -o ClearAllForwardings=yes -n $sshflags $host $(shell_escape(cmd))` # use ssh to remote launch

    # start the processes first in parallel first...
    maxp = config[:max_parallel]

    if get(config, :tunnel, false)
        maxp = div(maxp,2) + 1   # Since the tunnel will also take up one ssh connection
    end

    ios_to_check = []

    t_check=time()
    while cnt > 0
        ios_to_check2 = []
        for io in ios_to_check
            if nb_available(io) == 0
                push!(ios_to_check2, io)
            else
                push!(launched, merge(AnyDict(:io=>io, :host=>host), thisconfig))
                notify(machines_launch_ntfy)
            end
        end
        ios_to_check=ios_to_check2

        maxp_in_loop = maxp - length(ios_to_check)
        if maxp_in_loop == 0
            # wait for sometime and check again
            sleep(0.1)
            if (time() - t_check) > 50
                error("Timed out waiting for launched worker")
            end
            continue
        end
        lc = cnt > maxp_in_loop ? maxp_in_loop : cnt

        for i in 1:lc
            io, pobj = open(detach(cmd), "r")
            push!(ios_to_check, io)
        end

        cnt = cnt - lc
        t_check=time()
    end

    for io in ios_to_check
        push!(launched, merge(AnyDict(:io=>io, :host=>host), thisconfig))
    end

    notify(machines_launch_ntfy)
    notify(plaunch_ntfy)
end


function manage{T<:SSHManager}(::Type{T}, id::Integer, config::Dict, op::Symbol)
    if op == :interrupt
        if haskey(config, :ospid)
            machine = config[:machine]
            if !success(`ssh -T -a -x -o ClearAllForwardings=yes -n $(config[:sshflags]) $machine "kill -2 $(config[:ospid])"`)
                println("Error sending a Ctrl-C to julia worker $id on $machine")
            end
        else
            # This state can happen immediately after an addprocs
            println("Worker $id cannot be presently interrupted.")
        end
    elseif op == :register
        config[:ospid] = remotecall_fetch(id, getpid)
    end
end



immutable LocalManager <: ClusterManager
end

show(io::IO, manager::LocalManager) = println("LocalManager()")

function launch{T<:LocalManager}(::Type{T}, config::Dict, launched::Array, c::Condition)
    dir = config[:dir]
    exename = config[:exename]
    exeflags = config[:exeflags]

    for i in 1:config[:np]
        io, pobj = open(detach(`$(dir)/$(exename) $exeflags --bind-to $(LPROC.bind_addr)`), "r")
        wconfig = copy(config)
        wconfig[:process] = pobj
        wconfig[:io] = io
        push!(launched, wconfig)
    end

    notify(c)
end

function manage{T<:LocalManager}(::Type{T}, id::Integer, config::Dict, op::Symbol)
    if op == :interrupt
        kill(config[:process], 2)
    end
end

function read_worker_host_port(io::IO)
    io.line_buffered = true
    while true
        conninfo = readline(io)
        bind_addr, port = parse_connection_info(conninfo)
        if bind_addr != ""
            return bind_addr, port
        end
    end
end

function parse_connection_info(str)
    m = match(r"^julia_worker:(\d+)#(.*)", str)
    if m != nothing
        (m.captures[2], parseint(Int16, m.captures[1]))
    else
        ("", int16(-1))
    end
end

function connect_m2w{T<:ClusterManager}(::Type{T}, pid::Int, config::Dict)
    pid = config[:pid]
    io = get(config, :io, nothing)
    if isa(io, AsyncStream)
        (bind_addr, port) = read_worker_host_port(io)
        pubhost=get(config, :host, bind_addr)
    else
        pubhost=config[:host]
        port=config[:port]
        bind_addr=pubhost
    end

    tunnel = get(config, :tunnel, false)

    s = split(pubhost,'@')
    user = ""
    if length(s) > 1
        user = s[1]
        pubhost = s[2]
    else
        if haskey(ENV, "USER")
            user = ENV["USER"]
        elseif tunnel
            error("USER must be specified either in the environment or as part of the hostname when tunnel option is used")
        end
    end

    if tunnel
        sshflags = config[:sshflags]
        (s, bind_addr) = connect_to_worker(pubhost, bind_addr, port, user, sshflags)
    else
        (s, bind_addr) = connect_to_worker(bind_addr, port)
    end

    config[:host] = pubhost
    config[:port] = port
    config[:bind_addr] = bind_addr

    # write out a subset of the connect_at required for further worker-worker connection setups
    config[:connect_at] = (string(bind_addr), port)

    if isa(io, AsyncStream)
        let pid = pid
            # redirect console output from workers to the client's stdout:
            @async begin
                while !eof(io)
                    line = readline(io)
                    print("\tFrom worker $(id):\t$line")
                end
            end
        end
    end

    (s, s)
end

function connect_w2w{T<:ClusterManager}(::Type{T}, pid::Int, config::Dict)
    (rhost, rport) = config[:connect_at]
    config[:host] = rhost
    config[:port] = rport
    if get(config, :self_is_local, false) && get(config, :r_is_local, false)
        # If on localhost, use the loopback address - this addresses
        # the special case of system suspend wherein the local ip
        # may be changed upon system awake.
        (s, bind_addr) = connect_to_worker("127.0.0.1", rport)
    else
        (s, bind_addr)= connect_to_worker(rhost, rport)
    end

    (s,s)
end


function connect_to_worker(host::AbstractString, port::Integer)
    # Connect to the loopback port if requested host has the same ipaddress as self.
    if host == string(LPROC.bind_addr)
        s = connect("127.0.0.1", uint16(port))
    else
        s = connect(host, uint16(port))
    end

    # Avoid calling getaddrinfo if possible - involves a DNS lookup
    # host may be a stringified ipv4 / ipv6 address or a dns name
    if host == "localhost"
        bind_addr = parseip("127.0.0.1")
    else
        try
            bind_addr = parseip(host)
        catch
            bind_addr = getaddrinfo(host)
        end
    end
    (s, bind_addr)
end


function connect_to_worker(host::AbstractString, bind_addr::AbstractString, port::Integer, tunnel_user::AbstractString, sshflags)
    s = connect("localhost", ssh_tunnel(tunnel_user, host, bind_addr, uint16(port), sshflags))
    (s, parseip(bind_addr))
end


tunnel_port = 9201
# establish an SSH tunnel to a remote worker
# returns P such that localhost:P connects to host:port
function ssh_tunnel(user, host, bind_addr, port, sshflags)
    global tunnel_port
    localp = tunnel_port::Int
    while !success(detach(`ssh -T -a -x -o ExitOnForwardFailure=yes -f $sshflags $(user)@$host -L $localp:$bind_addr:$(int(port)) sleep 60`)) && localp < 10000
        localp += 1
    end

    if localp >= 10000
        error("unable to assign a local tunnel port between 9201 and 10000")
    end

    tunnel_port = localp+1
    localp
end

cluster_manager = LocalManager # The default.
function init_worker{T<:ClusterManager}(::Type{T})
    reset_node()
    global cluster_manager
    cluster_manager = T
    disable_threaded_libs()
end

## worker creation and setup ##

# the entry point for julia worker processes. does not return.
# argument is descriptor to write listening port # to.
start_worker{T<:ClusterManager}(::Type{T}) = start_worker(T, STDOUT)
function start_worker{T<:ClusterManager}(::Type{T}, out::IO)
    # we only explicitly monitor worker STDOUT on the console, so redirect
    # stderr to stdout so we can see the output.
    # at some point we might want some or all worker output to go to log
    # files instead.
    # Currently disabled since this caused processes to spin instead of
    # exit when process 1 shut down. Don't yet know why.
    #redirect_stderr(STDOUT)

    init_worker(T)
    if LPROC.bind_port == 0
        (actual_port,sock) = listenany(uint16(9009))
        LPROC.bind_port = actual_port
    else
        sock = listen(LPROC.bind_port)
    end
    sock.ccb = accept_handler
    print(out, "julia_worker:")  # print header
    print(out, "$(dec(LPROC.bind_port))#") # print port
    print(out, LPROC.bind_addr)
    print(out, '\n')
    flush(out)
    # close STDIN; workers will not use it
    #close(STDIN)

    disable_nagle(sock)

    try
        check_master_connect(60.0)
        while true; wait(); end
    catch err
        print(STDERR, "unhandled exception on $(myid()): $(err)\nexiting.\n")
    end

    close(sock)
    exit(0)
end

# activity on accept fd
function accept_handler(server::TCPServer, status::Int32)
    if status == -1
        error("an error occured during the creation of the server")
    end
    client = accept_nonblock(server)
    process_messages(client, client)
end

function check_master_connect(timeout)
    # If we do not have at least process 1 connect to us within timeout
    # we log an error and exit
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

function disable_nagle(sock)
    # disable nagle on all OSes
    ccall(:uv_tcp_nodelay, Cint, (Ptr{Void}, Cint), sock.handle, 1)
    @linux_only begin
        # tcp_quickack is a linux only option
        if ccall(:jl_tcp_quickack, Cint, (Ptr{Void}, Cint), sock.handle, 1) < 0
            warn_once("Parallel networking unoptimized ( Error enabling TCP_QUICKACK : ", strerror(errno()), " )")
        end
    end
end

function interrupt(pid::Integer)
    assert(myid() == 1)
    w = map_pid_wrkr[pid]
    if isa(w, Worker)
        manage(wtype(w), w.id, w.config, :interrupt)
    end
end
interrupt(pids::Integer...) = interrupt([pids...])

function interrupt(pids::AbstractVector=workers())
    assert(myid() == 1)
    @sync begin
        for pid in pids
            @async interrupt(pid)
        end
    end
end

function check_same_host(pids)
    if myid() != 1
        return remotecall_fetch(1, check_same_host, pids)
    else
        # We checkfirst if all test pids have been started using the local manager,
        # else we check for the same bind_to addr. This handles the special case
        # where the local ip address may change - as during a system sleep/awake
        if all(p -> (p==1) || (wtype(map_pid_wrkr[p]) == LocalManager), pids)
            return true
        else
            first_bind_addr = map_pid_wrkr[pids[1]].config[:bind_addr]
            return all(p -> (p != 1) && (map_pid_wrkr[p].config[:bind_addr] == first_bind_addr), pids[2:end])
        end
    end
end

function terminate_all_workers()
    if myid() != 1
        return
    end

    if nprocs() > 1
        ret = rmprocs(workers(); waitfor=0.5)
        if ret != :ok
            warn("Forcibly interrupting busy workers")
            # Might be computation bound, interrupt them and try again
            interrupt(workers())
            ret = rmprocs(workers(); waitfor=0.5)
            if ret != :ok
                warn("Unable to terminate all workers")
            end
        end
    end
end

