# This file is a part of Julia. License is MIT: http://julialang.org/license
using Base.Test
include("choosetests.jl")
tests, net_on = choosetests(ARGS)
tests = unique(tests)

const max_worker_rss = if haskey(ENV, "JULIA_TEST_MAXRSS_MB")
    parse(Int, ENV["JULIA_TEST_MAXRSS_MB"]) * 2^20
else
    typemax(Csize_t)
end

const node1_tests = String[]
function move_to_node1(t)
    if t in tests
        splice!(tests, findfirst(tests, t))
        push!(node1_tests, t)
    end
end
# Base.compile only works from node 1, so compile test is handled specially
move_to_node1("compile")
# In a constrained memory environment, run the parallel test after all other tests
# since it starts a lot of workers and can easily exceed the maximum memory
max_worker_rss != typemax(Csize_t) && move_to_node1("parallel")

cd(dirname(@__FILE__)) do
    n = 1
    if net_on
        n = min(Sys.CPU_CORES, length(tests))
        n > 1 && addprocs(n; exeflags=`--check-bounds=yes --startup-file=no --depwarn=error`)
        BLAS.set_num_threads(1)
    end

    @everywhere include("testdefs.jl")
    results=[]
    @sync begin
        for p in workers()
            @async begin
                while length(tests) > 0
                    test = shift!(tests)
                    local resp
                    try
                        resp = remotecall_fetch(runtests, p, test)
                    catch e
                        resp = [e]
                    end
                    push!(results, (test, resp))
                    if (isa(resp[end], Integer) && (resp[end] > max_worker_rss)) || isa(resp, Exception)
                        if n > 1
                            rmprocs(p, waitfor=0.5)
                            p = addprocs(1; exeflags=`--check-bounds=yes --startup-file=no --depwarn=error`)[1]
                            remotecall_fetch(()->include("testdefs.jl"), p)
                        else
                            # single process testing, bail if mem limit reached, or, on an exception.
                            isa(resp, Exception) ? rethrow(resp) : error("Halting tests. Memory limit reached : $resp > $max_worker_rss")
                        end
                    end
                end
            end
        end
    end
    # Free up memory =)
    n > 1 && rmprocs(workers(), waitfor=5.0)
    for t in node1_tests
        # As above, try to run each test
        # which must run on node 1. If
        # the test fails, catch the error,
        # and either way, append the results
        # to the overall aggregator
        n > 1 && print("\tFrom worker 1:\t")
        local resp
        try
            resp = runtests(t)
        catch e
            resp = [e]
        end
        push!(results, (t, resp))
    end
    #=
`   Construct a testset on the master node which will hold results from all the
    test files run on workers and on node1. The loop goes through the results,
    inserting them as children of the overall testset if they are testsets,
    handling errors otherwise.

    Since the workers don't return information about passing/broken tests, only
    errors or failures, those Result types get passed `nothing` for their test
    expressions (and expected/received result in the case of Broken).

    If a test failed, returning a `RemoteException`, the error is displayed and
    the overall testset has a child testset inserted, with the (empty) Passes
    and Brokens from the worker and the full information about all errors and
    failures encountered running the tests. This information will be displayed
    as a summary at the end of the test run.
    =#
    o_ts = Base.Test.DefaultTestSet("Overall")
    Base.Test.push_testset(o_ts)
    for res in results
        if isa(res[2][1], Base.Test.DefaultTestSet)
            Base.Test.push_testset(res[2][1])
            Base.Test.record(o_ts, res[2][1])
            Base.Test.pop_testset()
        elseif isa(res[2][1], Tuple{Int,Int})
            fake = Base.Test.DefaultTestSet(res[1])
            for i in 1:res[2][1][1]
                Base.Test.record(fake, Base.Test.Pass(:test, nothing, nothing, nothing))
            end
            for i in 1:res[2][1][2]
                Base.Test.record(fake, Base.Test.Broken(:test, nothing))
            end
            Base.Test.push_testset(fake)
            Base.Test.record(o_ts, fake)
            Base.Test.pop_testset()
        elseif isa(res[2][1], RemoteException)
            println("Worker $(res[2][1].pid) failed running test $(res[1]):")
            Base.showerror(STDOUT,res[2][1].captured)
            o_ts.anynonpass = true
            if isa(res[2][1].captured.ex, Base.Test.TestSetException)
                fake = Base.Test.DefaultTestSet(res[1])
                for i in 1:res[2][1].captured.ex.pass
                    Base.Test.record(fake, Base.Test.Pass(:test, nothing, nothing, nothing))
                end
                for i in 1:res[2][1].captured.ex.broken
                    Base.Test.record(fake, Base.Test.Broken(:test, nothing))
                end
                for t in res[2][1].captured.ex.errors_and_fails
                    Base.Test.record(fake, t)
                end
                Base.Test.push_testset(fake)
                Base.Test.record(o_ts, fake)
                Base.Test.pop_testset()
            end
        end
    end
    println()
    Base.Test.print_test_results(o_ts,1)
    #pretty print the information about gc and mem usage
    name_align    = maximum(map(x -> length(x[1]), results))
    elapsed_align = length("Total time (s):")
    gc_align      = length("GC time (s):")
    percent_align = length("Percent in gc:")
    alloc_align   = length("Allocated (MB):")
    rss_align     = length("RSS (MB):")
    print_with_color(:white, rpad("Test:",name_align," "), " | ")
    print_with_color(:white, "Total time (s): | GC time (s): | Percent in gc: | Allocated (MB): | RSS (MB):\n")
    for res in results
        if !isa(res[2][1], RemoteException)
            print_with_color(:white, rpad("$(res[1])",name_align," "), " | ")
            time_str = @sprintf("%7.2f",res[2][2])
            print_with_color(:white, rpad(time_str,elapsed_align," "), " | ")
            gc_str = @sprintf("%7.2f",res[2][5].total_time/10^9)
            print_with_color(:white, rpad(gc_str,gc_align," "), " | ")
            percent_str = @sprintf("%7.2f",100*res[2][5].total_time/(10^9*res[2][2]))
            print_with_color(:white, rpad(percent_str,percent_align," "), " | ")
            alloc_str = @sprintf("%7.2f",res[2][3]/2^20)
            print_with_color(:white, rpad(alloc_str,alloc_align," "), " | ")
            rss_str = @sprintf("%7.2f",res[2][6]/2^20)
            print_with_color(:white, rpad(rss_str,rss_align," "), "\n")
        end
    end

    if !o_ts.anynonpass
        println("    \033[32;1mSUCCESS\033[0m")
    else
        println("    \033[31;1mFAILURE\033[0m")
        Base.Test.print_test_errors(o_ts)
        error()
    end
end
