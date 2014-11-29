push!(LOAD_PATH, "$(pwd())/ZMQCM")

using ZMQCM

ZMQCM.start_master(int(ARGS[1]))

resp = pmap(x -> myid() *2, [1:nworkers()])

println(resp)
