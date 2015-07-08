# This file is a part of Julia. License is MIT: http://julialang.org/license

include("choosetests.jl")
tests, net_on = choosetests(ARGS)
n = 1
if net_on
    n = min(8, CPU_CORES, length(tests))
    n > 1 && addprocs(n; exeflags=`--check-bounds=yes --depwarn=error`)
    blas_set_num_threads(1)
end

@everywhere include("testdefs.jl")

let cwd = pwd()
    reduce(propagate_errors, nothing, pmap(test->runtests(test, cwd), tests; err_retry=false, err_stop=true))
end

@unix_only n > 1 && rmprocs(workers(), waitfor=5.0)
println("    \033[32;1mSUCCESS\033[0m")
