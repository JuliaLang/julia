# This file is a part of Julia. License is MIT: http://julialang.org/license

include("choosetests.jl")
include("testdefs.jl")

tests, net_on = choosetests(ARGS)
tests = unique(tests)

# Base.compile only works from node 1, so compile test is handled specially
compile_test = "compile" in tests
if compile_test
    splice!(tests, findfirst(tests, "compile"))
end

cd(dirname(@__FILE__)) do
    n = net_on ? min(8, CPU_CORES, length(tests)) : 1

    if compile_test
        n > 1 && print("\tFrom worker 1:\t")
        runtests("compile")
    end

    if net_on
        n > 1 && addprocs(n; exeflags=`--check-bounds=yes --depwarn=error`)
        @everywhere include("testdefs.jl")
        blas_set_num_threads(1)
    end

    reduce(propagate_errors, nothing, pmap(runtests, tests; err_retry=false, err_stop=true))

    @unix_only n > 1 && rmprocs(workers(), waitfor=5.0)
    println("    \033[32;1mSUCCESS\033[0m")
end
