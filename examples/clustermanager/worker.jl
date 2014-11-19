push!(LOAD_PATH, "$(pwd())/ZMQCM")

using ZMQCM

ZMQCM.start_worker(int(ARGS[1]))
