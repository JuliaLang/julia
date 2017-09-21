# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Test
include("choosetests.jl")
include("testenv.jl")

tests, net_on, exit_on_error = choosetests(ARGS)
tests = unique(tests)

const max_worker_rss = if haskey(ENV, "JULIA_TEST_MAXRSS_MB")
    parse(Int, ENV["JULIA_TEST_MAXRSS_MB"]) * 2^20
else
    typemax(Csize_t)
end

const node1_tests = String[]
function move_to_node1(t)
    if t in tests
        splice!(tests, findfirst(equalto(t), tests))
        push!(node1_tests, t)
    end
end
# Base.compile only works from node 1, so compile test is handled specially
move_to_node1("compile")
# In a constrained memory environment, run the "distributed" test after all other tests
# since it starts a lot of workers and can easily exceed the maximum memory
max_worker_rss != typemax(Csize_t) && move_to_node1("distributed")

cd(dirname(@__FILE__)) do
    n = 1
    if net_on
        n = min(Sys.CPU_CORES, length(tests))
        n > 1 && addprocs_with_testenv(n)
        BLAS.set_num_threads(1)
    end
    skipped = 0

    @everywhere include("testdefs.jl")

    #pretty print the information about gc and mem usage
    name_align    = maximum([length("Test (Worker)"); map(x -> length(x) + 3 + ndigits(nworkers()), tests)])
    elapsed_align = length("Time (s)")
    gc_align      = length("GC (s)")
    percent_align = length("GC %")
    alloc_align   = length("Alloc (MB)")
    rss_align     = length("RSS (MB)")
    print_with_color(:white, rpad("Test (Worker)",name_align," "), " | ")
    print_with_color(:white, "Time (s) | GC (s) | GC % | Alloc (MB) | RSS (MB)\n")
    results=[]
    @sync begin
        for p in workers()
            @async begin
                while length(tests) > 0
                    test = shift!(tests)
                    local resp
                    wrkr = p
                    try
                        resp = remotecall_fetch(runtests, wrkr, test)
                    catch e
                        resp = [e]
                    end
                    push!(results, (test, resp))
                    if resp[1] isa Exception
                        if exit_on_error
                            skipped = length(tests)
                            empty!(tests)
                        end
                    elseif resp[end] > max_worker_rss
                        if n > 1
                            rmprocs(wrkr, waitfor=30)
                            p = addprocs_with_testenv(1)[1]
                            remotecall_fetch(include, p, "testdefs.jl")
                        else # single process testing
                            error("Halting tests. Memory limit reached : $resp > $max_worker_rss")
                        end
                    end
                    if !isa(resp[1], Exception)
                        print_with_color(:white, rpad(test*" ($wrkr)", name_align, " "), " | ")
                        time_str = @sprintf("%7.2f",resp[2])
                        print_with_color(:white, rpad(time_str,elapsed_align," "), " | ")
                        gc_str = @sprintf("%5.2f",resp[5].total_time/10^9)
                        print_with_color(:white, rpad(gc_str,gc_align," "), " | ")

                        # since there may be quite a few digits in the percentage,
                        # the left-padding here is less to make sure everything fits
                        percent_str = @sprintf("%4.1f",100*resp[5].total_time/(10^9*resp[2]))
                        print_with_color(:white, rpad(percent_str,percent_align," "), " | ")
                        alloc_str = @sprintf("%5.2f",resp[3]/2^20)
                        print_with_color(:white, rpad(alloc_str,alloc_align," "), " | ")
                        rss_str = @sprintf("%5.2f",resp[6]/2^20)
                        print_with_color(:white, rpad(rss_str,rss_align," "), "\n")
                    end
                end
            end
        end
    end
    # Free up memory =)
    n > 1 && rmprocs(workers(), waitfor=30)
    for t in node1_tests
        # As above, try to run each test
        # which must run on node 1. If
        # the test fails, catch the error,
        # and either way, append the results
        # to the overall aggregator
        n > 1 && print("\tFrom worker 1:\t")
        local resp
        try
            resp = eval(Expr(:call, () -> runtests(t))) # runtests is defined by the include above
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

    If a test failed, returning an `Exception` that is not a `RemoteException`,
    it is likely the julia process running the test has encountered some kind
    of internal error, such as a segfault.  The entire testset is marked as
    Errored, and execution continues until the summary at the end of the test
    run, where the test file is printed out as the "failed expression".
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
        elseif isa(res[2][1], RemoteException) && isa(res[2][1].captured.ex, Base.Test.TestSetException)
            println("Worker $(res[2][1].pid) failed running test $(res[1]):")
            Base.showerror(STDOUT,res[2][1].captured)
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
        elseif isa(res[2][1], Exception)
            # If this test raised an exception that is not a remote testset exception,
            # i.e. not a RemoteException capturing a TestSetException that means
            # the test runner itself had some problem, so we may have hit a segfault,
            # deserialization errors or something similar.  Record this testset as Errored.
            fake = Base.Test.DefaultTestSet(res[1])
            Base.Test.record(fake, Base.Test.Error(:test_error, res[1], res[2][1], []))
            Base.Test.push_testset(fake)
            Base.Test.record(o_ts, fake)
            Base.Test.pop_testset()
        else
            error(string("Unknown result type : ", typeof(res)))
        end
    end
    println()
    Base.Test.print_test_results(o_ts,1)
    if !o_ts.anynonpass
        println("    \033[32;1mSUCCESS\033[0m")
    else
        println("    \033[31;1mFAILURE\033[0m\n")
        skipped > 0 &&
            println("$skipped test", skipped > 1 ? "s were" : " was", " skipped due to failure.\n")
        Base.Test.print_test_errors(o_ts)
        throw(Test.FallbackTestSetException("Test run finished with errors"))
    end
end
