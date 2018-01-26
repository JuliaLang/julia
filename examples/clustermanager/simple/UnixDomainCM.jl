# This file is a part of Julia. License is MIT: https://julialang.org/license
using Distributed
import Distributed: launch, manage, connect, exit

mutable struct UnixDomainCM <: ClusterManager
    np::Integer
end

function launch(manager::UnixDomainCM, params::Dict, launched::Array, c::Condition)
#    println("launch $(manager.np)")
    cookie = cluster_cookie()
    for i in 1:manager.np
        sockname = tempname()
        try
            __file__ = @__FILE__
            cmd = `$(params[:exename]) --startup-file=no $__file__ udwrkr $sockname $cookie`
            pobj = open(cmd)

            wconfig = WorkerConfig()
            wconfig.userdata = Dict(:sockname=>sockname, :io=>pobj.out, :process=>pobj)
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
        # This will be useful in the worker-to-worker connection setup.
        config.connect_at = config.userdata[:sockname]

        print_worker_stdout(config.userdata[:io], pid)
    else
#        println("connect_w2w")
        sockname = config.connect_at
        config.userdata = Dict{Symbol, Any}(:sockname=>sockname)
    end

    t = time()
    while true
        try
            address = config.userdata[:sockname]
            if isa(address, Tuple)
                sock = connect(address...)
            else
                sock = connect(address)
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
function start_worker(sockname, cookie)
    init_worker(cookie, UnixDomainCM(0))

    srvr = listen(sockname)
    while true
        sock = accept(srvr)
        process_messages(sock, sock)
    end
end

function manage(manager::UnixDomainCM, id::Int, config::WorkerConfig, op)
    # Does not seem to be required, filesystem entry cleanup is happening automatically on process exit
#     if op == :deregister
#         try
#             rm(config.userdata[:sockname])
#         end
#     end
    nothing
end

function print_worker_stdout(io, pid)
    @schedule while !eof(io)
        line = readline(io)
        println("      From worker $(pid):\t$line")
    end
end

if (length(ARGS) > 0) && (ARGS[1] == "udwrkr")
    # script has been launched as a worker
    start_worker(ARGS[2], ARGS[3])
end
