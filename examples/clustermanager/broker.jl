push!(LOAD_PATH, "$(pwd())/ZMQCM")

using ZMQCM

if length(ARGS) == 2
    ZMQCM.test_broker(int(ARGS[1]), int(ARGS[2]))
else
    ZMQCM.start_broker()
end