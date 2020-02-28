# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run the distributed test outside of the main driver since it needs its own
# set of dedicated workers.
include(joinpath(Sys.BINDIR, "..", "share", "julia", "test", "testenv.jl"))
disttestfile = joinpath(@__DIR__, "distributed_exec.jl")

cmd = `$test_exename $test_exeflags $disttestfile`

if !success(pipeline(cmd; stdout=stdout, stderr=stderr)) && ccall(:jl_running_on_valgrind,Cint,()) == 0
    error("Distributed test failed, cmd : $cmd")
end

include("macros.jl")
include("managers.jl")
