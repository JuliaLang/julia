# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Distributed
using Dates
import REPL
using Printf: @sprintf

include("choosetests.jl")
include("testenv.jl")

tests, net_on, exit_on_error, use_revise, seed = choosetests(ARGS)
tests = unique(tests)

if use_revise
    using Revise
end

const max_worker_rss = if haskey(ENV, "JULIA_TEST_MAXRSS_MB")
    parse(Int, ENV["JULIA_TEST_MAXRSS_MB"]) * 2^20
else
    typemax(Csize_t)
end
limited_worker_rss = max_worker_rss != typemax(Csize_t)

function test_path(test)
    t = split(test, '/')
    if t[1] in STDLIBS
        if length(t) == 2
            return joinpath(STDLIB_DIR, t[1], "test", t[2])
        else
            return joinpath(STDLIB_DIR, t[1], "test", "runtests")
        end
    else
        return joinpath(@__DIR__, test)
    end
end

# Check all test files exist
isfiles = isfile.(test_path.(tests) .* ".jl")
if !all(isfiles)
    error("did not find test files for the following tests: ",
          join(tests[.!(isfiles)], ", "))
end

const node1_tests = String[]
function move_to_node1(t)
    if t in tests
        splice!(tests, findfirst(isequal(t), tests))
        push!(node1_tests, t)
    end
    nothing
end

# Base.compilecache only works from node 1, so precompile test is handled specially
move_to_node1("precompile")
move_to_node1("SharedArrays")
move_to_node1("threads")
# Ensure things like consuming all kernel pipe memory doesn't interfere with other tests
move_to_node1("stress")

# In a constrained memory environment, run the "distributed" test after all other tests
# since it starts a lot of workers and can easily exceed the maximum memory
limited_worker_rss && move_to_node1("Distributed")

# Shuffle LinearAlgebra tests to the front, because they take a while, so we might
# as well get them all started early.
linalg_test_ids = findall(x->occursin("LinearAlgebra", x), tests)
linalg_tests = tests[linalg_test_ids]
deleteat!(tests, linalg_test_ids)
prepend!(tests, linalg_tests)

import LinearAlgebra
cd(@__DIR__) do
    n = 1
    if net_on
        n = min(Sys.CPU_THREADS, length(tests))
        n > 1 && addprocs_with_testenv(n)
        LinearAlgebra.BLAS.set_num_threads(1)
    end
    skipped = 0

    @everywhere include("testdefs.jl")

    if use_revise
        @everywhere begin
            Revise.track(Core.Compiler)
            Revise.track(Base)
            Revise.revise()
        end
    end

    #pretty print the information about gc and mem usage
    testgroupheader = "Test"
    workerheader = "(Worker)"
    name_align    = maximum([textwidth(testgroupheader) + textwidth(" ") + textwidth(workerheader); map(x -> textwidth(x) + 3 + ndigits(nworkers()), tests)])
    elapsed_align = textwidth("Time (s)")
    gc_align      = textwidth("GC (s)")
    percent_align = textwidth("GC %")
    alloc_align   = textwidth("Alloc (MB)")
    rss_align     = textwidth("RSS (MB)")
    printstyled(testgroupheader, color=:white)
    printstyled(lpad(workerheader, name_align - textwidth(testgroupheader) + 1), " | ", color=:white)
    printstyled("Time (s) | GC (s) | GC % | Alloc (MB) | RSS (MB)\n", color=:white)
    results = []
    print_lock = stdout isa Base.LibuvStream ? stdout.lock : ReentrantLock()
    if stderr isa Base.LibuvStream
        stderr.lock = print_lock
    end

    function print_testworker_stats(test, wrkr, resp)
        @nospecialize resp
        lock(print_lock)
        try
            printstyled(test, color=:white)
            printstyled(lpad("($wrkr)", name_align - textwidth(test) + 1, " "), " | ", color=:white)
            time_str = @sprintf("%7.2f",resp[2])
            printstyled(lpad(time_str, elapsed_align, " "), " | ", color=:white)
            gc_str = @sprintf("%5.2f", resp[5].total_time / 10^9)
            printstyled(lpad(gc_str, gc_align, " "), " | ", color=:white)

            # since there may be quite a few digits in the percentage,
            # the left-padding here is less to make sure everything fits
            percent_str = @sprintf("%4.1f", 100 * resp[5].total_time / (10^9 * resp[2]))
            printstyled(lpad(percent_str, percent_align, " "), " | ", color=:white)
            alloc_str = @sprintf("%5.2f", resp[3] / 2^20)
            printstyled(lpad(alloc_str, alloc_align, " "), " | ", color=:white)
            rss_str = @sprintf("%5.2f", resp[6] / 2^20)
            printstyled(lpad(rss_str, rss_align, " "), "\n", color=:white)
        finally
            unlock(print_lock)
        end
    end

    global print_testworker_started = (name, wrkr)->begin
        lock(print_lock)
        try
            printstyled(name, color=:white)
            printstyled(lpad("($wrkr)", name_align - textwidth(name) + 1, " "), " |",
                " "^elapsed_align, "started at $(now())\n", color=:white)
        finally
            unlock(print_lock)
        end
    end

    function print_testworker_errored(name, wrkr)
        lock(print_lock)
        try
            printstyled(name, color=:red)
            printstyled(lpad("($wrkr)", name_align - textwidth(name) + 1, " "), " |",
                " "^elapsed_align, " failed at $(now())\n", color=:red)
        finally
            unlock(print_lock)
        end
    end


    all_tests = [tests; node1_tests]

    local stdin_monitor
    all_tasks = Task[]
    try
        # Monitor stdin and kill this task on ^C
        # but don't do this on Windows, because it may deadlock in the kernel
        running_tests = Dict{String, DateTime}()
        if !Sys.iswindows() && isa(stdin, Base.TTY)
            t = current_task()
            stdin_monitor = @async begin
                term = REPL.Terminals.TTYTerminal("xterm", stdin, stdout, stderr)
                try
                    REPL.Terminals.raw!(term, true)
                    while true
                        c = read(term, Char)
                        if c == '\x3'
                            Base.throwto(t, InterruptException())
                            break
                        elseif c == '?'
                            println("Currently running: ")
                            tests = sort(collect(running_tests), by=x->x[2])
                            foreach(tests) do (test, date)
                                println(test, " (running for ", round(now()-date, Minute), ")")
                            end
                        end
                    end
                catch e
                    isa(e, InterruptException) || rethrow()
                finally
                    REPL.Terminals.raw!(term, false)
                end
            end
        end
        @sync begin
            for p in workers()
                @async begin
                    push!(all_tasks, current_task())
                    while length(tests) > 0
                        test = popfirst!(tests)
                        running_tests[test] = now()
                        local resp
                        wrkr = p
                        try
                            resp = remotecall_fetch(runtests, wrkr, test, test_path(test); seed=seed)
                        catch e
                            isa(e, InterruptException) && return
                            resp = Any[e]
                        end
                        delete!(running_tests, test)
                        push!(results, (test, resp))
                        if resp[1] isa Exception
                            print_testworker_errored(test, wrkr)
                            if exit_on_error
                                skipped = length(tests)
                                empty!(tests)
                            elseif n > 1
                                # the worker encountered some failure, recycle it
                                # so future tests get a fresh environment
                                rmprocs(wrkr, waitfor=30)
                                p = addprocs_with_testenv(1)[1]
                                remotecall_fetch(include, p, "testdefs.jl")
                            end
                        else
                            print_testworker_stats(test, wrkr, resp)
                            if resp[end] > max_worker_rss
                                # the worker has reached the max-rss limit, recycle it
                                # so future tests start with a smaller working set
                                if n > 1
                                    rmprocs(wrkr, waitfor=30)
                                    p = addprocs_with_testenv(1)[1]
                                    remotecall_fetch(include, p, "testdefs.jl")
                                else # single process testing
                                    error("Halting tests. Memory limit reached : $resp > $max_worker_rss")
                                end
                            end
                       end
                    end
                    if p != 1
                        # Free up memory =)
                        rmprocs(p, waitfor=30)
                    end
                end
            end
        end

        n > 1 && length(node1_tests) > 1 && print("\nExecuting tests that run on node 1 only:\n")
        for t in node1_tests
            # As above, try to run each test
            # which must run on node 1. If
            # the test fails, catch the error,
            # and either way, append the results
            # to the overall aggregator
            isolate = true
            t == "SharedArrays" && (isolate = false)
            local resp
            try
                resp = eval(Expr(:call, () -> runtests(t, test_path(t), isolate, seed=seed))) # runtests is defined by the include above
                print_testworker_stats(t, 1, resp)
            catch e
                resp = Any[e]
            end
            push!(results, (t, resp))
        end
    catch e
        isa(e, InterruptException) || rethrow()
        # If the test suite was merely interrupted, still print the
        # summary, which can be useful to diagnose what's going on
        foreach(task -> begin
                istaskstarted(task) || return
                istaskdone(task) && return
                try
                    schedule(task, InterruptException(); error=true)
                catch ex
                    @error "InterruptException" exception=ex,catch_backtrace()
                end
            end, all_tasks)
        foreach(wait, all_tasks)
    finally
        if @isdefined stdin_monitor
            schedule(stdin_monitor, InterruptException(); error=true)
        end
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
    o_ts = Test.DefaultTestSet("Overall")
    Test.push_testset(o_ts)
    completed_tests = Set{String}()
    for (testname, (resp,)) in results
        push!(completed_tests, testname)
        if isa(resp, Test.DefaultTestSet)
            Test.push_testset(resp)
            Test.record(o_ts, resp)
            Test.pop_testset()
        elseif isa(resp, Tuple{Int,Int})
            fake = Test.DefaultTestSet(testname)
            for i in 1:resp[1]
                Test.record(fake, Test.Pass(:test, nothing, nothing, nothing))
            end
            for i in 1:resp[2]
                Test.record(fake, Test.Broken(:test, nothing))
            end
            Test.push_testset(fake)
            Test.record(o_ts, fake)
            Test.pop_testset()
        elseif isa(resp, RemoteException) && isa(resp.captured.ex, Test.TestSetException)
            println("Worker $(resp.pid) failed running test $(testname):")
            Base.showerror(stdout, resp.captured)
            println()
            fake = Test.DefaultTestSet(testname)
            for i in 1:resp.captured.ex.pass
                Test.record(fake, Test.Pass(:test, nothing, nothing, nothing))
            end
            for i in 1:resp.captured.ex.broken
                Test.record(fake, Test.Broken(:test, nothing))
            end
            for t in resp.captured.ex.errors_and_fails
                Test.record(fake, t)
            end
            Test.push_testset(fake)
            Test.record(o_ts, fake)
            Test.pop_testset()
        else
            if !isa(resp, Exception)
                resp = ErrorException(string("Unknown result type : ", typeof(resp)))
            end
            # If this test raised an exception that is not a remote testset exception,
            # i.e. not a RemoteException capturing a TestSetException that means
            # the test runner itself had some problem, so we may have hit a segfault,
            # deserialization errors or something similar.  Record this testset as Errored.
            fake = Test.DefaultTestSet(testname)
            Test.record(fake, Test.Error(:test_error, testname, nothing, Any[(resp, [])], LineNumberNode(1)))
            Test.push_testset(fake)
            Test.record(o_ts, fake)
            Test.pop_testset()
        end
    end
    for test in all_tests
        (test in completed_tests) && continue
        fake = Test.DefaultTestSet(test)
        Test.record(fake, Test.Error(:test_interrupted, test, nothing, [("skipped", [])], LineNumberNode(1)))
        Test.push_testset(fake)
        Test.record(o_ts, fake)
        Test.pop_testset()
    end
    println()
    Test.print_test_results(o_ts, 1)
    if !o_ts.anynonpass
        println("    \033[32;1mSUCCESS\033[0m")
    else
        println("    \033[31;1mFAILURE\033[0m\n")
        skipped > 0 &&
            println("$skipped test", skipped > 1 ? "s were" : " was", " skipped due to failure.")
        println("The global RNG seed was 0x$(string(seed, base = 16)).\n")
        Test.print_test_errors(o_ts)
        throw(Test.FallbackTestSetException("Test run finished with errors"))
    end
end
