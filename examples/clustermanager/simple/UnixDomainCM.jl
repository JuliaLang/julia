# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base: launch, manage, connect, exit

type UnixDomainCM <: ClusterManager
    np::Integer
end

function launch(manager::UnixDomainCM, params::Dict, launched::Array, c::Condition)
#    println("launch $(manager.np)")
    for i in 1:manager.np
        sockname = tempname()
        try
            cmd = `$(params[:exename]) $(@__FILE__) udwrkr $sockname`
            io, pobj = open(cmd, "r")

            wconfig = WorkerConfig()
            wconfig.userdata = Dict(:sockname=>sockname, :io=>io, :process=>pobj)
            push!(launched, wconfig)
            notify(c)
        catch e
            println(e)
        end
    end
end

function connect(manager::UnixDomainCM, pid::Int, config::WorkerConfig)
    if myid() == 1
#        println("connect_m2w")
        config.connect_at = get(config.userdata)[:sockname] # This will be useful in the worker-to-worker connection setup.

        print_worker_stdout(get(config.userdata)[:io], pid)
    else
#        println("connect_w2w")
        sockname = get(config.connect_at)
        config.userdata = Dict{Symbol, Any}(:sockname=>sockname)
    end

    t = time()
    while true
        try
            address = get(config.userdata)[:sockname]
            if isa(address, Tuple)
                sock = connect(address...)
            else
                sock = connect(ascii(address))
            end
            return (sock, sock)
        catch e
            if (time() - t) > 30.0
                rethrow(e)
            else
                sleep(0.1)
            end
        end
    end

end

# WORKER
function start_worker(sockname)
    Base.init_worker(UnixDomainCM(0))

    srvr = listen(ascii(sockname))
    while true
        sock = accept(srvr)
        Base.process_messages(sock, sock)
    end
end

function manage(manager::UnixDomainCM, id::Int, config::WorkerConfig, op)
    # Does not seem to be required, filesystem entry cleanup is happening automatically on process exit
#     if op == :deregister
#         try
#             rm(get(config.userdata)[:sockname])
#         end
#     end
    nothing
end

function print_worker_stdout(io, pid)
    @schedule while !eof(io)
        line = readline(io)
        print("\tFrom worker $(pid):\t$line")
    end
end

if (length(ARGS) > 0) && (ARGS[1] == "udwrkr")
    # script has been launched as a worker
    start_worker(ARGS[2])
end
