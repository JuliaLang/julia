# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: cancel!, CancellationRequest, CANCEL_REQUEST_SAFE, CANCEL_REQUEST_ACK

const collatz_code = quote
    collatz(n) = (n & 1) == 1 ? (3n + 1) : (n ÷ 2)
    function find_collatz_counterexample()
        i = 1
        while true
            j = i
            while true
                Base.@cancel_check
                j = collatz(j)
                j == 1 && break
                j == i && error("$j is a collatz counterexample")
            end
            i += 1
        end
    end
    @noinline function find_collatz_counterexample_inner()
        i = 1
        while true
            j = i
            while true
                j = collatz(j)
                j == 1 && break
                j == i && return j
            end
            i += 1
        end
    end
    function find_collatz_counterexample2()
        # A single cancellation point at function entry; interrupting the inner
        # (checkless) loop requires the reset_ctx mechanism.
        Base.@cancel_check
        return find_collatz_counterexample_inner()
    end
end
eval(collatz_code)

# wait a little, so cancellation targets are (most likely) started and parked
spin(n=4) = for _ in 1:n; yield(); end

@testset "cancellation of waiting tasks" begin
    # Cancellation of a task that was never started
    t = @task nothing
    @test cancel!(t)
    @test t.state === :cancelled
    @test istaskfailed(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # Scheduling a cancelled task transitions it to failed, without running it
    schedule(t)
    @test t.state === :failed

    # Cancellation of `sleep`
    t = @async sleep(1000)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest

    # Cancellation of a task blocked on a Channel
    c = Channel{Int}(0)
    t = @async take!(c)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest
    # The channel remains usable
    t2 = @async take!(c)
    put!(c, 7)
    @test fetch(t2) == 7

    # Cancellation of a task waiting on another task propagates
    t_in = @async sleep(1000)
    t = @async wait(t_in)
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test istaskdone(t_in) && istaskfailed(t_in)
end

@testset "cancellation of computing tasks" begin
    # Polling cancellation via @cancel_check
    t = Threads.@spawn find_collatz_counterexample()
    sleep(0.2)
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CancellationRequest

    # Asynchronous interruption of a checkless loop through the reset_ctx
    # mechanism (requires an extra thread to run the cancellation from)
    if Threads.nthreads() > 1
        t = Threads.@spawn find_collatz_counterexample2()
        sleep(0.5)
        cancel!(t)
        sleep(0.5)
        @test_throws TaskFailedException wait(t)
        @test t.result isa CancellationRequest
    end
end

@testset "structured cancellation of @sync" begin
    t = @async begin
        @sync begin
            @async sleep(1000)
            @async sleep(1000)
        end
    end
    spin()
    cancel!(t)
    @test_throws TaskFailedException wait(t)
    @test t.result isa CompositeException
    @test length(t.result.exceptions) == 2
end

@testset "cancellation of blocked stream writes" begin
    p = Pipe()
    Base.link_pipe!(p, reader_supports_async=true, writer_supports_async=true)
    try
        # A write far exceeding the OS pipe buffer blocks until cancelled
        big = zeros(UInt8, 200_000_000)
        t = @async write(p, big)
        sleep(0.5)
        @test t.queue === p.in
        cancel!(t)
        @test_throws TaskFailedException wait(t)
        @test t.result isa CancellationRequest
    finally
        close(p)
    end
end

@testset "acknowledged requests do not re-trigger" begin
    t = @async begin
        try
            sleep(1000)
        catch e
            e isa CancellationRequest || rethrow()
        end
        # The request was delivered (and acknowledged); this task can still
        # perform IO and sleep.
        sleep(0.01)
        (@atomic :acquire current_task().cancellation_request) === CANCEL_REQUEST_ACK
    end
    spin()
    cancel!(t)
    @test fetch(t)
end

@testset "^C" begin
    function run_with_sigint(code::String, delays; forcekill::Bool=false)
        out = Pipe()
        p = run(pipeline(`$(Base.julia_cmd()) --startup-file=no -e $code`, stdout=out, stderr=out), wait=false)
        close(out.in)
        reader = @async read(out, String)
        killer = @async begin
            for d in delays
                sleep(d)
                process_running(p) && kill(p, Base.SIGINT)
            end
            if forcekill
                # e.g. an abandoned script has nothing left to run and idles
                sleep(3)
                process_running(p) && kill(p, Base.SIGKILL)
            end
        end
        wait(p)
        wait(killer)
        return fetch(reader), p
    end

    # Catching ^C in a script
    output, p = run_with_sigint("""
        try
            sleep(100)
            println("FAIL: not cancelled")
        catch e
            println("caught: ", typeof(e))
        end
        println("continued")
    """, [1.0])
    @test occursin("caught: Base.CancellationRequest", output)
    @test occursin("continued", output)
    @test p.exitcode == 0

    # Uncaught ^C produces a proper error report
    output, p = run_with_sigint("sleep(100)", [1.0])
    @test occursin("CancellationRequest: Safe Cancellation (CANCEL_REQUEST_SAFE)", output)
    @test p.exitcode == 1

    # ^C propagates through @sync, cancelling compute-bound and sleeping tasks
    output, p = run_with_sigint("""
        $(string(collatz_code))
        try
            @sync begin
                @async sleep(10000)
                @async find_collatz_counterexample()
            end
        catch e
            println(typeof(e))
        end
        """, [1.5])
    @test occursin("CompositeException", output)
    @test p.exitcode == 0

    # Escalation: an unresponsive process warns after 1s, and a second ^C
    # abandons the stuck task (leaving a script with nothing left to run)
    output, p = run_with_sigint("""
        x = Ref(1.0)
        while true
            x[] = x[] * 1.0000001 + 0.1
        end
        """, [1.0, 2.5]; forcekill=true)
    @test occursin("failed to acknowledge SIGINT", output)
    @test occursin("Abandoning current task", output)
end
