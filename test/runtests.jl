# This file is a part of Julia. License is MIT: http://julialang.org/license

include("choosetests.jl")
tests, net_on = choosetests(ARGS)
cd(dirname(@__FILE__)) do
    n = 1
    if net_on
        n = min(8, CPU_CORES, length(tests))
        n > 1 && addprocs(n; exeflags=`--check-bounds=yes`)
        blas_set_num_threads(1)
    end

    @everywhere include("testdefs.jl")

    reduce(propagate_errors, nothing, pmap(runtests, tests; err_retry=false, err_stop=true))

    @unix_only n > 1 && rmprocs(workers(), waitfor=5.0)
    println("    \033[32;1mSUCCESS\033[0m")
end
