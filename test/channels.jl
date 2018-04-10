# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random

@testset "various constructors" begin
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

    c = Channel{Int}(Inf)
    @test eltype(c) == Int
    pvals = map(i->put!(c,i), 1:10^6)
    tvals = Int[take!(c) for i in 1:10^6]
    @test pvals == tvals

    # Uncomment line below once deprecation support has been removed.
    # @test_throws MethodError Channel()
    @test_throws ArgumentError Channel(-1)
    @test_throws InexactError Channel(1.5)
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

@testset "channel iterator with done()" begin
# Test channel iterator with done() being called multiple times
# This needs to be explicitly tested since `take!` is called
# in `done()` and not `next()`
    c = Channel(32)
    foreach(i -> put!(c, i), 1:10)
    close(c)
    s = start(c)
    @test done(c, s) == false
    res = Int[]
    while !done(c, s)
        local v
        @test done(c,s) == false
        v, s = next(c, s)
        push!(res, v)
    end
    @test res == Int[1:10...]
end

using Distributed
@testset "channels bound to tasks" for N in [0, 10]
    # Normal exit of task
    c=Channel(N)
    bind(c, @schedule (yield();nothing))
    @test_throws InvalidStateException take!(c)
    @test !isopen(c)

    # Error exception in task
    c=Channel(N)
    bind(c, @schedule (yield();error("foo")))
    @test_throws ErrorException take!(c)
    @test !isopen(c)

    # Multiple channels closed by the same bound task
    cs = [Channel(N) for i in 1:5]
    tf2 = () -> begin
        if N > 0
            foreach(c->(@assert take!(c)==2), cs)
        end
        yield()
        error("foo")
    end
    task = Task(tf2)
    foreach(c->bind(c, task), cs)
    schedule(task)

    if N > 0
        for i in 1:5
            @test put!(cs[i], 2) == 2
        end
    end
    for i in 1:5
        while (isopen(cs[i])); yield(); end
        @test_throws ErrorException wait(cs[i])
        @test_throws ErrorException take!(cs[i])
        @test_throws ErrorException put!(cs[i], 1)
        @test_throws ErrorException fetch(cs[i])
    end

    # Multiple tasks, first one to terminate closes the channel
    nth = rand(1:5)
    ref = Ref(0)
    cond = Condition()
    tf3(i) = begin
        if i == nth
            ref[] = i
        else
            sleep(2.0)
        end
    end

    tasks = [Task(()->tf3(i)) for i in 1:5]
    c = Channel(N)
    foreach(t->bind(c,t), tasks)
    foreach(schedule, tasks)
    @test_throws InvalidStateException wait(c)
    @test !isopen(c)
    @test ref[] == nth

    # channeled_tasks
    for T in [Any, Int]
        chnls, tasks = Base.channeled_tasks(2, (c1,c2)->(@assert take!(c1)==1; put!(c2,2)); ctypes=[T,T], csizes=[N,N])
        put!(chnls[1], 1)
        @test take!(chnls[2]) == 2
        @test_throws InvalidStateException wait(chnls[1])
        @test_throws InvalidStateException wait(chnls[2])
        @test istaskdone(tasks[1])
        @test !isopen(chnls[1])
        @test !isopen(chnls[2])

        f=Future()
        tf4 = (c1,c2) -> begin
            @assert take!(c1)==1
            wait(f)
        end

        tf5 = (c1,c2) -> begin
            put!(c2,2)
            wait(f)
        end

        chnls, tasks = Base.channeled_tasks(2, tf4, tf5; ctypes=[T,T], csizes=[N,N])
        put!(chnls[1], 1)
        @test take!(chnls[2]) == 2
        yield()
        put!(f, 1)

        @test_throws InvalidStateException wait(chnls[1])
        @test_throws InvalidStateException wait(chnls[2])
        @test istaskdone(tasks[1])
        @test istaskdone(tasks[2])
        @test !isopen(chnls[1])
        @test !isopen(chnls[2])
    end

    # channel
    tf6 = c -> begin
        @assert take!(c)==2
        error("foo")
    end

    for T in [Any, Int]
        taskref = Ref{Task}()
        chnl = Channel(tf6, ctype=T, csize=N, taskref=taskref)
        put!(chnl, 2)
        yield()
        @test_throws ErrorException wait(chnl)
        @test istaskdone(taskref[])
        @test !isopen(chnl)
        @test_throws ErrorException take!(chnl)
    end
end

using Dates
@testset "timedwait on multiple channels" begin
    @sync begin
        rr1 = Channel(1)
        rr2 = Channel(1)
        rr3 = Channel(1)

        callback() = all(map(isready, [rr1, rr2, rr3]))
        # precompile functions which will be tested for execution time
        @test !callback()
        @test timedwait(callback, 0.0) === :timed_out

        @async begin sleep(0.5); put!(rr1, :ok) end
        @async begin sleep(1.0); put!(rr2, :ok) end
        @async begin sleep(2.0); put!(rr3, :ok) end

        et = @elapsed timedwait(callback, Dates.Second(1))

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
    @noinline garbage_finalizer(f) = finalizer(f, "gar" * "bage")
    run = Ref(0)
    GC.enable(false)
    # test for finalizers trying to yield leading to failed attempts to context switch
    garbage_finalizer((x) -> (run[] += 1; sleep(1)))
    garbage_finalizer((x) -> (run[] += 1; yield()))
    garbage_finalizer((x) -> (run[] += 1; yieldto(@task () -> ())))
    t = @task begin
        GC.enable(true)
        GC.gc()
    end
    oldstderr = stderr
    local newstderr, errstream
    try
        newstderr = redirect_stderr()
        errstream = @async read(newstderr[1], String)
        yield(t)
    finally
        redirect_stderr(oldstderr)
        close(newstderr[2])
    end
    Base._wait(t)
    @test run[] == 3
    @test fetch(errstream) == """
        error in running finalizer: ErrorException("task switch not allowed from inside gc finalizer")
        error in running finalizer: ErrorException("task switch not allowed from inside gc finalizer")
        error in running finalizer: ErrorException("task switch not allowed from inside gc finalizer")
        """
    # test for invalid state in Workqueue during yield
    t = @schedule nothing
    t.state = :invalid
    try
        newstderr = redirect_stderr()
        errstream = @async read(newstderr[1], String)
        yield()
    finally
        redirect_stderr(oldstderr)
        close(newstderr[2])
    end
    @test fetch(errstream) == "\nWARNING: Workqueue inconsistency detected: popfirst!(Workqueue).state != :queued\n"
end

@testset "schedule_and_wait" begin
    t = @schedule(nothing)
    ct = current_task()
    testobject = "testobject"
    # note: there is a low probability this test could fail, due to receiving network traffic simultaneously
    @test length(Base.Workqueue) == 1
    @test Base.schedule_and_wait(ct, 8) == 8
    @test isempty(Base.Workqueue)
    @test Base.schedule_and_wait(ct, testobject) === testobject
end

@testset "throwto" begin
    t = @task(nothing)
    ct = current_task()
    testerr = ErrorException("expected")
    @async Base.throwto(t, testerr)
    @test try
        Base._wait(t)
        false
    catch ex
        ex
    end === testerr
end

@testset "Timer / AsyncCondition triggering and race #12719" begin
    tc = Ref(0)
    t = Timer(0) do t
        tc[] += 1
    end
    @test isopen(t)
    Base.process_events(false)
    @test !isopen(t)
    @test tc[] == 0
    yield()
    @test tc[] == 1

    tc = Ref(0)
    t = Timer(0) do t
        tc[] += 1
    end
    @test isopen(t)
    close(t)
    @test !isopen(t)
    sleep(0.1)
    @test tc[] == 0

    tc = Ref(0)
    async = Base.AsyncCondition() do async
        tc[] += 1
    end
    @test isopen(async)
    ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
    Base.process_events(false) # schedule event
    ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
    Sys.iswindows() && Base.process_events(false) # schedule event (windows?)
    @test tc[] == 0
    yield() # consume event
    @test tc[] == 1
    sleep(0.1) # no further events
    @test tc[] == 1
    ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
    ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
    close(async)
    @test !isopen(async)
    @test tc[] == 1
    Base.process_events(false) # schedule event & then close
    Sys.iswindows() && Base.process_events(false) # schedule event (windows?)
    yield() # consume event & then close
    @test tc[] == 2
    sleep(0.1) # no further events
    @test tc[] == 2

    tc = Ref(0)
    async = Base.AsyncCondition() do async
        tc[] += 1
    end
    @test isopen(async)
    ccall(:uv_async_send, Cvoid, (Ptr{Cvoid},), async)
    close(async)
    @test !isopen(async)
    Base.process_events(false) # schedule event & then close
    Sys.iswindows() && Base.process_events(false) # schedule event (windows)
    @test tc[] == 0
    yield() # consume event & then close
    @test tc[] == 1
    sleep(0.1)
    @test tc[] == 1
end

@testset "check_channel_state" begin
    c = Channel(1)
    close(c)
    @test !isopen(c)
    c.excp == nothing # to trigger the branch
    @test_throws InvalidStateException Base.check_channel_state(c)
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
