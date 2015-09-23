# This file is a part of Julia. License is MIT: http://julialang.org/license

include("choosetests.jl")
tests, net_on = choosetests(ARGS)
tests = unique(tests)

# Base.compile only works from node 1, so compile test is handled specially
compile_test = "compile" in tests
if compile_test
    splice!(tests, findfirst(tests, "compile"))
end

cd(dirname(@__FILE__)) do
    n = 1
    if net_on
        n = min(8, CPU_CORES, length(tests))
        n > 1 && addprocs(n; exeflags=`--check-bounds=yes --depwarn=error`)
        blas_set_num_threads(1)
    end

    @everywhere include("testdefs.jl")

    err_stop = haskey(ENV, "JL_TESTFAILURE_STOP")
    results = reduce(propagate_errors, nothing, pmap(runtests, tests; err_retry=false, err_stop=err_stop))

    if compile_test
        n > 1 && print("\tFrom worker 1:\t")
        runtests("compile")
    end

    @unix_only n > 1 && rmprocs(workers(), waitfor=5.0)

    # exit(1) if any test raised an error so that travis knows the build failed.
    if any(x -> x==true, results)
        exit(1)
    end

    println("    \033[32;1mSUCCESS\033[0m")
end
