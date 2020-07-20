# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random
using Base: Experimental

@testset "single-threaded Condition usage" begin
    a = Condition()
    t = @async begin
        Base.notify(a, "success")
        "finished"
    end
    @test wait(a) == "success"
    @test fetch(t) == "finished"
end

@testset "various constructors" begin
    c = Channel()
    @test eltype(c) == Any
    @test c.sz_max == 0

    c = Channel(1)
    @test eltype(c) == Any
    @test put!(c, 1) == 1
    @test isready(c) == true
    @test take!(c) == 1
    @test isready(c) == false
    @test eltype(Channel(1.0)) == Any

    c = Channel{Int}(1)
    @test eltype(c) == Int
    @test_throws MethodError put!(c, "Hello")

    c = Channel{Int}()
    @test eltype(c) == Int
    @test c.sz_max == 0

    c = Channel{Int}(Inf)
    @test eltype(c) == Int
    pvals = map(i->put!(c,i), 1:10^6)
    tvals = Int[take!(c) for i in 1:10^6]
    @test pvals == tvals

    @test_throws ArgumentError Channel(-1)
    @test_throws InexactError Channel(1.5)
end

@testset "Task constructors" begin
    c = Channel{Int}() do c; map(i->put!(c,i), 1:100); end
    @test eltype(c) == Int
    @test c.sz_max == 0
    @test collect(c) == 1:100

    c = Channel() do c; put!(c, 1); put!(c, "hi") end
    @test c.sz_max == 0
    @test collect(c) == [1, "hi"]

    c = Channel(Inf) do c; put!(c,1); end
    @test eltype(c) == Any
    @test c.sz_max == typemax(Int)
    c = Channel{Int}(Inf) do c; put!(c,1); end
    @test eltype(c) == Int
    @test c.sz_max == typemax(Int)

    taskref = Ref{Task}()
    c = Channel{Int}(0, taskref=taskref) do c; put!(c, 0); end
    @test eltype(c) == Int
    @test c.sz_max == 0
    @test istaskstarted(taskref[])
    @test !istaskdone(taskref[])
    take!(c); wait(taskref[])
    @test istaskdone(taskref[])

    # Legacy constructor
    c = Channel(ctype=Float32, csize=2) do c; map(i->put!(c,i), 1:100); end
    @test eltype(c) == Float32
    @test c.sz_max == 2
    @test isopen(c)
    @test collect(c) == 1:100
end
@testset "Multithreaded task constructors" begin
    taskref = Ref{Task}()
    c = Channel(spawn=true, taskref=taskref) do c; put!(c, 0); end
    # Test that the task is using the multithreaded scheduler
    @test taskref[].sticky == false
    @test collect(c) == [0]
end

@testset "multiple concurrent put!/take! on a channel for different sizes" begin
    function testcpt(sz)
        c = Channel{Int}(sz)
        size = 0
        inc() = size += 1
        dec() = size -= 1
        @sync for i = 1:10^4
            @async (sleep(rand()); put!(c, i); inc())
            @async (sleep(rand()); take!(c); dec())
        end
        @test size == 0
    end
    testcpt(0)
    testcpt(1)
    testcpt(32)
    testcpt(Inf)
end

@testset "type conversion in put!" begin
    c = Channel{Int64}(0)
    @async put!(c, Int32(1))
    wait(c)
    @test isa(take!(c), Int64)
    @test_throws MethodError put!(c, "")
    @assert !islocked(c.cond_take)
end

@testset "multiple for loops waiting on the same channel" begin
    # Test multiple "for" loops waiting on the same channel which
    # is closed after adding a few elements.
    c = Channel(32)
    results = []
    @sync begin
        for i in 1:20
            @async for ii in c
                push!(results, ii)
            end
        end
        sleep(1.0)
        for i in 1:5
            put!(c,i)
        end
        close(c)
    end
    @test sum(results) == 15
end

# Tests for channels bound to tasks.
using Distributed
@testset "channels bound to tasks" for N in [0, 10]
    # Normal exit of task
    c = Channel(N)
    bind(c, @async (GC.gc(); yield(); nothing))
    @test_throws InvalidStateException take!(c)
    @test !isopen(c)

    # Error exception in task
    c = Channel(N)
    task = @async (GC.gc(); yield(); error("foo"))
    bind(c, task)
    @test_throws TaskFailedException(task) take!(c)
    @test !isopen(c)

    # Multiple channels closed by the same bound task
    cs = [Channel(N) for i in 1:5]
    tf2() = begin
        GC.gc()
        if N > 0
            foreach(c -> (@assert take!(c) === 2), cs)
        end
        yield()
        error("foo")
    end
    task = Task(tf2)
    foreach(c -> bind(c, task), cs)
    schedule(task)

    if N > 0
        for i in 1:5
            @test put!(cs[i], 2) === 2
        end
    end
    for i in 1:5
        while isopen(cs[i])
            yield()
        end
        @test_throws TaskFailedException(task) wait(cs[i])
        @test_throws TaskFailedException(task) take!(cs[i])
        @test_throws TaskFailedException(task) put!(cs[i], 1)
        N == 0 || @test_throws TaskFailedException(task) fetch(cs[i])
        N == 0 && @test_throws ErrorException fetch(cs[i])
    end

    # Multiple tasks, first one to terminate closes the channel
    nth = rand(1:5)
    ref = Ref(0)
    tf3(i) = begin
        GC.gc()
        if i == nth
            ref[] = i
        else
            sleep(2.0)
        end
    end

    tasks = [Task(() -> tf3(i)) for i in 1:5]
    c = Channel(N)
    foreach(t -> bind(c, t), tasks)
    foreach(schedule, tasks)
    @test_throws InvalidStateException wait(c)
    @test !isopen(c)
    @test ref[] == nth
    @assert !islocked(c.cond_take)

    # channeled_tasks
    for T in [Any, Int]
        tf_chnls1 = (c1, c2) -> (@assert take!(c1) == 1; put!(c2, 2))
        chnls, tasks = Base.channeled_tasks(2, tf_chnls1; ctypes=[T,T], csizes=[N,N])
        put!(chnls[1], 1)
        @test take!(chnls[2]) === 2
        @test_throws InvalidStateException wait(chnls[1])
        @test_throws InvalidStateException wait(chnls[2])
        @test istaskdone(tasks[1])
        @test !isopen(chnls[1])
        @test !isopen(chnls[2])

        f = Future()
        tf4 = (c1, c2) -> begin
            @assert take!(c1) === 1
            wait(f)
        end

        tf5 = (c1, c2) -> begin
            put!(c2, 2)
            wait(f)
        end

        chnls, tasks = Base.channeled_tasks(2, tf4, tf5; ctypes=[T,T], csizes=[N,N])
        put!(chnls[1], 1)
        @test take!(chnls[2]) === 2
        yield()
        put!(f, 1) # allow tf4 and tf5 to exit after now, eventually closing the channel

        @test_throws InvalidStateException wait(chnls[1])
        @test_throws InvalidStateException wait(chnls[2])
        @test istaskdone(tasks[1])
        @test istaskdone(tasks[2])
        @test !isopen(chnls[1])
        @test !isopen(chnls[2])
    end

    # channel
    tf6 = c -> begin
        @assert take!(c) === 2
        error("foo")
    end

    for T in [Any, Int]
        taskref = Ref{Task}()
        chnl = Channel{T}(tf6, N, taskref=taskref)
        put!(chnl, 2)
        yield()
        @test_throws TaskFailedException(taskref[]) wait(chnl)
        @test istaskdone(taskref[])
        @test !isopen(chnl)
        @test_throws TaskFailedException(taskref[]) take!(chnl)
    end
end

@testset "timedwait" begin
    @test timedwait(() -> true, 0) === :ok
    @test timedwait(() -> false, 0) === :timed_out
    @test_throws ArgumentError timedwait(() -> true, 0; pollint=0)

    # Allowing a smaller positive `pollint` results in `timewait` hanging
    @test_throws ArgumentError timedwait(() -> true, 0, pollint=1e-4)

    # Callback passed in raises an exception
    failure_cb = function (fail_on_call=1)
        i = 0
        function ()
            i += 1
            i >= fail_on_call && error("callback failed")
            return false
        end
    end

    try
        timedwait(failure_cb(1), 0)
        @test false
    catch e
        @test e isa CapturedException
        @test e.ex isa ErrorException
    end

    try
        timedwait(failure_cb(2), 0)
        @test false
    catch e
        @test e isa CapturedException
        @test e.ex isa ErrorException
    end

    duration = @elapsed timedwait(() -> false, 1)  # Using default pollint of 0.1
    @test duration ≈ 1 atol=0.4

    duration = @elapsed timedwait(() -> false, 0; pollint=1)
    @test duration ≈ 1 atol=0.4
end

@testset "timedwait on multiple channels" begin
    @Experimental.sync begin
        rr1 = Channel(1)
        rr2 = Channel(1)
        rr3 = Channel(1)

        callback() = all(map(isready, [rr1, rr2, rr3]))
        # precompile functions which will be tested for execution time
        @test !callback()
        @test timedwait(callback, 0) === :timed_out

        @async begin sleep(0.5); put!(rr1, :ok) end
        @async begin sleep(1.0); put!(rr2, :ok) end
        @async begin sleep(2.0); put!(rr3, :ok) end

        et = @elapsed timedwait(callback, 1)

        # assuming that 0.5 seconds is a good enough buffer on a typical modern CPU
        try
            @assert (et >= 1.0) && (et <= 1.5)
            @assert !isready(rr3)
        catch
            @warn "`timedwait` tests delayed. et=$et, isready(rr3)=$(isready(rr3))"
        end
        @test isready(rr1)
    end
end

@testset "yield/wait/event failures" begin
    # garbage_finalizer returns `nothing` rather than the garbage object so
    # that the interpreter doesn't accidentally root the garbage when
    # interpreting the calling function.
    @noinline garbage_finalizer(f) = (finalizer(f, "gar" * "bage"); nothing)
    run = Ref(0)
    garbage_finalizer(x -> nothing) # warmup
    @test GC.enable(false)
    # test for finalizers trying to yield leading to failed attempts to context switch
    garbage_finalizer((x) -> (run[] += 1; sleep(1)))
    garbage_finalizer((x) -> (run[] += 1; yield()))
    garbage_finalizer((x) -> (run[] += 1; yieldto(@task () -> ())))
    t = @task begin
        @test !GC.enable(true)
        GC.gc()
        true
    end
    oldstderr = stderr
    newstderr = redirect_stderr()
    local errstream
    try
        errstream = @async read(newstderr[1], String)
        yield(t)
    finally
        redirect_stderr(oldstderr)
        close(newstderr[2])
    end
    @test istaskdone(t)
    @test fetch(t)
    @test run[] == 3
    @test fetch(errstream) == """
        error in running finalizer: ErrorException("task switch not allowed from inside gc finalizer")
        error in running finalizer: ErrorException("task switch not allowed from inside gc finalizer")
        error in running finalizer: ErrorException("task switch not allowed from inside gc finalizer")
        """
    # test for invalid state in Workqueue during yield
    t = @async nothing
    t.state = :invalid
    newstderr = redirect_stderr()
    try
        errstream = @async read(newstderr[1], String)
        yield()
    finally
        redirect_stderr(oldstderr)
        close(newstderr[2])
    end
    @test fetch(errstream) == "\nWARNING: Workqueue inconsistency detected: popfirst!(Workqueue).state != :runnable\n"
end

@testset "throwto" begin
    t = @task(nothing)
    ct = current_task()
    testerr = ErrorException("expected")
    @async Base.throwto(t, testerr)
    @test (try
        Base.wait(t)
        false
    catch ex
        ex
    end).task.exception === testerr
end

@testset "Timer / AsyncCondition triggering and race #12719" begin
    let tc = Ref(0)
        t = Timer(0) do t
            tc[] += 1
        end
        Libc.systemsleep(0.005)
        @test isopen(t)
        Base.process_events()
        @test !isopen(t)
        @test tc[] == 0
        yield()
        @test tc[] == 1
    end

    let tc = Ref(0)
        t = Timer(0) do t
            tc[] += 1
        end
        Libc.systemsleep(0.005)
        @test isopen(t)
        close(t)
        @test !isopen(t)
        sleep(0.1)
        @test tc[] == 0
    end

    let tc = Ref(0)
        async = Base.AsyncCondition() do async
            tc[] += 1
        end
        @test isopen(async)
        ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
        ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
        Base.process_events() # schedule event
        Sys.iswindows() && Base.process_events() # schedule event (windows?)
        ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
        @test tc[] == 0
        yield() # consume event
        @test tc[] == 1
        Sys.iswindows() && Base.process_events() # schedule event (windows?)
        yield() # consume event
        @test tc[] == 2
        sleep(0.1) # no further events
        @test tc[] == 2
        ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
        ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
        Base.process_events() # schedule event
        Sys.iswindows() && Base.process_events() # schedule event (windows?)
        close(async) # and close
        @test !isopen(async)
        @test tc[] == 2
        @test tc[] == 2
        yield() # consume event & then close
        @test tc[] == 3
        sleep(0.1) # no further events
        @test tc[] == 3
    end

    let tc = Ref(0)
        async = Base.AsyncCondition() do async
            tc[] += 1
        end
        @test isopen(async)
        ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
        Base.process_events() # schedule event
        Sys.iswindows() && Base.process_events() # schedule event (windows)
        close(async)
        @test !isopen(async)
        Base.process_events() # and close
        @test tc[] == 0
        yield() # consume event & then close
        @test tc[] == 1
        sleep(0.1)
        @test tc[] == 1
    end
end

@testset "check_channel_state" begin
    c = Channel(1)
    close(c)
    @test !isopen(c)
    c.excp == nothing # to trigger the branch
    @test_throws InvalidStateException Base.check_channel_state(c)
end

# PR #36641
# Ensure that `isempty()` does not mutate a Channel's state:
@testset "isempty(::Channel) mutation" begin
    function isempty_timeout(c::Channel)
        inner_c = Channel{Union{Bool,Nothing}}()
        @async put!(inner_c, isempty(c))
        @async begin
            sleep(0.01)
            if isopen(inner_c)
                put!(inner_c, nothing)
            end
        end
        result = take!(inner_c)
        if result === nothing
            error("isempty() timed out!")
        end
        return result
    end
    # First, with a non-buffered channel
    c = Channel()
    @test isempty_timeout(c)
    t_put = @async put!(c, 1)
    @test !isempty_timeout(c)
    # check a second time to ensure `isempty(c)` didn't just consume the element.
    @test !isempty_timeout(c)
    @test take!(c) == 1
    @test isempty_timeout(c)
    wait(t_put)

    # Next, with a buffered channel:
    c = Channel(2)
    @test isempty_timeout(c)
    t_put = put!(c, 1)
    @test !isempty_timeout(c)
    @test !isempty_timeout(c)
    @test take!(c) == 1
    @test isempty_timeout(c)
end

# issue #12473
# make sure 1-shot timers work
let a = []
    Timer(t -> push!(a, 1), 0.01, interval = 0)
    sleep(0.2)
    @test a == [1]
end

# make sure repeating timers work
@noinline function make_unrooted_timer(a)
    t = Timer(0.0, interval = 0.1)
    finalizer(t -> a[] += 1, t)
    wait(t)
    e = @elapsed for i = 1:5
        wait(t)
    end
    @test 1.5 > e >= 0.4
    @test a[] == 0
    nothing
end
let a = Ref(0)
    make_unrooted_timer(a)
    GC.gc()
    @test a[] == 1
end

# trying to `schedule` a finished task
let t = @async nothing
    wait(t)
    @test_throws ErrorException("schedule: Task not runnable") schedule(t, nothing)
end

# Channel `show`
let c = Channel(3)
    @test repr(c) == "Channel{Any}(3)"
    @test repr(MIME("text/plain"), c) == "Channel{Any}(3) (empty)"
    put!(c, 0)
    @test repr(MIME("text/plain"), c) == "Channel{Any}(3) (1 item available)"
    put!(c, 1)
    @test repr(MIME("text/plain"), c) == "Channel{Any}(3) (2 items available)"
    close(c)
    @test repr(MIME("text/plain"), c) == "Channel{Any}(3) (closed)"
end
