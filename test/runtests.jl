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
        # n > 1 && addprocs(n; exeflags=`--check-bounds=yes --depwarn=error`)
        # blas_set_num_threads(1)
    end

    @everywhere include("testdefs.jl")

    # reduce(propagate_errors, nothing, pmap(runtests, tests; err_retry=false, err_stop=true))
    tr = Int[]
    m = 0
    while !isempty(tests)
        if m < n
            testi = shift!(tests)
            m += 1
            @schedule begin
                push!(tr, !runtests(testi))
                m -= 1
                return nothing
            end
        else
            sleep(0.5)
        end
        sleep(0.1)
    end
    while m > 0
        wait()
    end
    if sum(tr) > 0
        error("total number of errors was $(sum(tr))")
    end

    if compile_test
        n > 1 && print("\tFrom worker 1:\t")
        runtests("compile")
    end

    @unix_only n > 1 && rmprocs(workers(), waitfor=5.0)
    println("    \033[32;1mSUCCESS\033[0m")
end
