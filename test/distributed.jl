# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run the distributed test outside of the main driver since it needs its own
# set of dedicated workers.

include("testenv.jl")
cmd = `$test_exename $test_exeflags distributed_exec.jl`

if !success(pipeline(cmd; stdout=STDOUT, stderr=STDERR)) && ccall(:jl_running_on_valgrind,Cint,()) == 0
    error("Distributed test failed, cmd : $cmd")
end
