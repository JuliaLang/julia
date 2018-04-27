# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

pids = addprocs_with_testenv(4; topology="master_worker")

let p1 = pids[1], p2 = pids[2]
    @test_throws RemoteException remotecall_fetch(()->remotecall_fetch(myid, p2), p1)
end

function test_worker_counts()
    # check if the nprocs/nworkers/workers are the same on the remaining workers
    np=nprocs()
    nw=nworkers()
    ws=sort(workers())

    for p in workers()
        @test (true, true, true) == remotecall_fetch(p, np, nw, ws) do x,y,z
            (x==nprocs(), y==nworkers(), z==sort(workers()))
        end
    end
end

function remove_workers_and_test()
    while nworkers() > 0
        rmprocs(workers()[1])
        test_worker_counts()
        if nworkers() == nprocs()
            break
        end
    end
end

remove_workers_and_test()

# connect even pids to other even pids, odd to odd.
mutable struct TopoTestManager <: ClusterManager
    np::Integer
end

function launch(manager::TopoTestManager, params::Dict, launched::Array, c::Condition)
    dir = params[:dir]
    exename = params[:exename]
    exeflags = params[:exeflags]

    cmd = `$exename $exeflags --bind-to $(Distributed.LPROC.bind_addr) --worker`
    cmd = pipeline(detach(setenv(cmd, dir=dir)))
    for i in 1:manager.np
        io = open(cmd, "r+")
        Distributed.write_cookie(io)

        wconfig = WorkerConfig()
        wconfig.process = io
        wconfig.io = io.out
        wconfig.ident = i
        wconfig.connect_idents = Vector(i+2:2:manager.np)
        push!(launched, wconfig)
    end

    notify(c)
end

const map_pid_ident=Dict()
function manage(manager::TopoTestManager, id::Integer, config::WorkerConfig, op::Symbol)
    if op == :register
        map_pid_ident[id] = config.ident
    elseif op == :interrupt
        kill(config.process, 2)
    end
end

addprocs_with_testenv(TopoTestManager(8); topology="custom")

while true
    if any(x->get(map_pid_ident, x, 0)==0, workers())
        yield()
    else
        break
    end
end

let p1, p2
for p1 in workers()
    for p2 in workers()
        i1 = map_pid_ident[p1]
        i2 = map_pid_ident[p2]
        if (iseven(i1) && iseven(i2)) || (isodd(i1) && isodd(i2))
            @test p2 == remotecall_fetch(p->remotecall_fetch(myid, p), p1, p2)
        else
            @test_throws RemoteException remotecall_fetch(p->remotecall_fetch(myid, p), p1, p2)
        end
    end
end
end

remove_workers_and_test()

# test `lazy` connection setup
function def_count_conn()
    @everywhere function count_connected_workers()
        count(x -> isa(x, Distributed.Worker) && isdefined(x, :r_stream) && isopen(x.r_stream),
                Distributed.PGRP.workers)
    end
end

addprocs_with_testenv(8)
def_count_conn()

# Test for 10 random combinations
wl = workers()
combinations = []
while length(combinations) < 10
    from = rand(wl)
    to = rand(wl)
    if from == to || ((from,to) in combinations) || ((to,from) in combinations)
        continue
    else
        push!(combinations, (from,to))
    end
end

# Initially only master-worker connections ought to be setup
expected_num_conns = 8
let num_conns = sum(asyncmap(p->remotecall_fetch(count_connected_workers,p), workers()))
    @test num_conns == expected_num_conns
end

for (i, (from,to)) in enumerate(combinations)
    remotecall_wait(topid->remotecall_fetch(myid, topid), from, to)
    global expected_num_conns += 2    # one connection endpoint on both from and to
    let num_conns = sum(asyncmap(p->remotecall_fetch(count_connected_workers,p), workers()))
        @test num_conns == expected_num_conns
    end
end

# With lazy=false, all connections ought to be setup during `addprocs`
rmprocs(workers())
addprocs_with_testenv(8; lazy=false)
def_count_conn()
@test sum(asyncmap(p->remotecall_fetch(count_connected_workers,p), workers())) == 64

# Cannot add more workers with a different `lazy` value
@test_throws ArgumentError addprocs_with_testenv(1; lazy=true)
