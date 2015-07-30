include("testdefs.jl")
addprocs(4; topology="master_slave")
v=remotecall_fetch(2, ()->remotecall_fetch(3,myid))
@test isa(v, Exception)

function test_worker_counts()
    # check if the nprocs/nworkers/workers are the same on the remaining workers
    np=nprocs()
    nw=nworkers()
    ws=sort(workers())

    for p in workers()
        @test (true, true, true) == remotecall_fetch(p, (x,y,z)->(x==nprocs(), y==nworkers(), z==sort(workers())), np, nw, ws)
    end
end

function remove_workers_and_test()
    while nworkers() > 0
        @test :ok == rmprocs(workers()[1]; waitfor=2.0)
        test_worker_counts()
        if nworkers() == nprocs()
            break
        end
    end
end

remove_workers_and_test()

# a hack just to test the "custom" topology.
# connect even pids to other even pids, odd to odd.
function Base.launch(manager::Base.LocalManager, params::Dict, launched::Array, c::Condition)
    dir = params[:dir]
    exename = params[:exename]
    exeflags = params[:exeflags]

    for i in 1:manager.np
        io, pobj = open(detach(
            setenv(`$(Base.julia_cmd(exename)) $exeflags --bind-to $(Base.LPROC.bind_addr) --worker`, dir=dir)), "r")
        wconfig = WorkerConfig()
        wconfig.process = pobj
        wconfig.io = io
        wconfig.ident = i
        wconfig.connect_idents = collect(i+2:2:manager.np)
        push!(launched, wconfig)
    end

    notify(c)
end
const map_pid_ident=Dict()
function Base.manage(manager::Base.LocalManager, id::Integer, config::WorkerConfig, op::Symbol)
    if op == :register
        map_pid_ident[id] = get(config.ident)
    elseif op == :interrupt
        kill(get(config.process), 2)
    end
end

addprocs(8; topology="custom")

while true
    if any(x->get(map_pid_ident, x, 0)==0, workers())
        yield()
    else
        break
    end
end

for p1 in workers()
    for p2 in workers()
        i1 = map_pid_ident[p1]
        i2 = map_pid_ident[p2]
        if (iseven(i1) && iseven(i2)) || (isodd(i1) && isodd(i2))
            @test p2 == remotecall_fetch(p1, p->remotecall_fetch(p,myid), p2)
        else
            v1 = remotecall_fetch(p1, p->remotecall_fetch(p,myid), p2)
            @test isa(v1, Exception)
        end
    end
end

remove_workers_and_test()
