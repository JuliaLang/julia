using Base.Signals

const VALID_SIGNALS = Sys.iswindows() ? [2, 4, 6, 8, 11, 15, 21, 22] : 1:31
const TEST_SIGNAL = Sys.iswindows() ? SIGABRT_COMPAT : SIGURG

@testset "signal_abbrev" begin
    @test signal_abbrev(0) === nothing
    for signum in VALID_SIGNALS
        sigabbrev = signal_abbrev(signum)
        @test !isempty(sigabbrev)
        @test all(c -> isuppercase(c) || isdigit(c) || c == '_', sigabbrev)
        @test !startswith(sigabbrev, "SIG")
    end
    @test signal_abbrev(32) === nothing
end

@testset "signal_name" begin
    @test signal_name(0) === nothing
    for signum in VALID_SIGNALS
        signame = signal_name(signum)
        @test length(signame) > 3
        @test all(c -> isuppercase(c) || isdigit(c) || c == '_', signame)
        @test startswith(signame, "SIG")
    end
    @test signal_name(32) === nothing
end

@testset "register/deregister handler" begin
    num_calls = Threads.Atomic{Int}(0)
    last_signal = Threads.Atomic{Cint}(0)

    # The handler we provide may, although unlikely, be triggered by external factors. We
    # need to be careful as to which signal we utilize for our tests.
    Signals.register_handler(TEST_SIGNAL) do signal
        num_calls[] += 1
        last_signal[] = signal
        return false
    end

    try
        @test num_calls[] == 0
        kill(TEST_SIGNAL)

        # Signal handler may not be called immediately
        @test timedwait(() -> num_calls[] > 0, 5) === :ok
        @test num_calls[] >= 1
        @test last_signal[] == TEST_SIGNAL

        Signals.deregister_handler(TEST_SIGNAL)
        last_num_calls = num_calls[]

        # Executes default signal handler. We need to choose our signal carefully to ensure
        # this would not cause Julia to abend when using the Julia default signal handlers.
        kill(TEST_SIGNAL)

        # Deregistration is immediate but we should wait a little in case the signal handler
        # is not deregistered and the handler has not yet been called.
        @test timedwait(() -> num_calls[] > last_num_calls, 5) === :timed_out
        @test num_calls[] == last_num_calls
    finally
        Signals.deregister_handler(TEST_SIGNAL)
    end
end

# The C-function used as the signal handler must return `Cvoid`
@testset "ignore handler return" begin
    num_calls = Threads.Atomic{Int}(0)
    Signals.register_handler(TEST_SIGNAL) do signal
        num_calls[] += 1
        # return 1
        return false
    end

    try
        # A failure here will look like:
        # `WARNING: cfunction: return type of _root_signal_handler does not match`
        kill(TEST_SIGNAL)
        @test timedwait(() -> num_calls[] > 0, 5) === :ok
    finally
        Signals.deregister_handler(TEST_SIGNAL)
    end
end

@testset "register multiple signal handlers on one signal" begin
    num_calls1 = Threads.Atomic{Int}(0)
    num_calls2 = Threads.Atomic{Int}(0)

    try
        # Define a signal handler
        Signals.register_handler(TEST_SIGNAL) do signal
            num_calls1[] += 1
            return false
        end

        # Overwrite the signal handler
        Signals.register_handler(TEST_SIGNAL) do signal
            num_calls2[] += 1
            return false
        end

        kill(TEST_SIGNAL)
        @test timedwait(() -> num_calls1[] + num_calls2[] > 0, 5) === :ok
        @test num_calls1[] == 0
        @test num_calls2[] >= 1
    finally
        Signals.deregister_handler(TEST_SIGNAL)
    end
end

@testset "external signal" begin
    num_calls = Threads.Atomic{Int}(0)

    Signals.register_handler(TEST_SIGNAL) do signal
        num_calls[] += 1
        return false
    end

    try
        # Receive a signal from another process
        run(`$(Base.julia_cmd()) -e "kill($(getpid()), $TEST_SIGNAL)"`)

        @test timedwait(() -> num_calls[] > 0, 5) === :ok
        @test num_calls[] >= 1
    finally
        Signals.deregister_handler(TEST_SIGNAL)
    end
end

@testset "exit within signal handler" begin
    # Rudimentary IPC without using signals to ensure that the signal handler has been
    # installed. Could also use shared memory for this.
    ready_file = tempname()

    code = quote
        using Base.Signals
        Signals._initialize_signal_router()
        Signals.register_handler(SIGTERM) do signal
            exit(2)
        end

        touch($ready_file)
        sleep(30)
        exit(1)  # Should only get here if the signal handler is never executed
    end

    cmd = `$(Base.julia_cmd()) -e $code`
    buffer = IOBuffer()
    p = run(pipeline(cmd; stdout=buffer, stderr=buffer); wait=false)
    @test timedwait(() -> process_running(p) && isfile(ready_file), 5) === :ok
    isfile(ready_file) && rm(ready_file)

    kill(getpid(p), SIGTERM)

    @test timedwait(() -> process_exited(p), 5) === :ok
    @test p.exitcode == 2
    @test String(take!(buffer)) == ""
end
