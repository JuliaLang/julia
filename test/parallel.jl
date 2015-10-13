# This file is a part of Julia. License is MIT: http://julialang.org/license

# Run the parallel test outside of the main driver, since it runs off its own
# set of workers.

cmd = `$(Base.julia_cmd()) --check-bounds=yes --depwarn=error parallel_exec.jl`

(strm, proc) = open(cmd)
errors = readall(strm)
wait(proc)
if !success(proc) && ccall(:jl_running_on_valgrind,Cint,()) == 0
    println(errors);
    error("Parallel test failed, cmd : $cmd")
end
