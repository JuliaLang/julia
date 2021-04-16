# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

let cmd = `$(Base.julia_cmd()) --depwarn=error --rr-detach --startup-file=no threads_exec.jl`
    for test_nthreads in (1, 2, 4, 4) # run once to try single-threaded mode, then try a couple times to trigger bad races
        new_env = copy(ENV)
        new_env["JULIA_NUM_THREADS"] = string(test_nthreads)
        run(pipeline(setenv(cmd, new_env), stdout = stdout, stderr = stderr))
    end
end

# issue #34415 - make sure external affinity settings work
if Sys.islinux()
    const SYS_rrcall_check_presence = 1008
    global running_under_rr() = 0 == ccall(:syscall, Int,
        (Int, Int, Int, Int, Int, Int, Int),
        SYS_rrcall_check_presence, 0, 0, 0, 0, 0, 0)
    if Sys.CPU_THREADS > 1 && Sys.which("taskset") !== nothing && !running_under_rr()
        run_with_affinity(spec) = readchomp(`taskset -c $spec $(Base.julia_cmd()) -e "run(\`taskset -p \$(getpid())\`)"`)
        @test endswith(run_with_affinity("1"), "2")
        @test endswith(run_with_affinity("0,1"), "3")
    end
end

# issue #34769
function idle_callback(handle)
    idle = @Base.handle_as handle UvTestIdle
    if idle.active
        idle.count += 1
        if idle.count == 1
            # We want to hit the case where we're allowing
            # the thread to go to sleep, which only happens
            # after some default amount of time (DEFAULT_THREAD_SLEEP_THRESHOLD)
            # so spend that amount of time here.
            Libc.systemsleep(0.004)
        elseif idle.count >= 10
            lock(idle.cond)
            try
                notify(idle.cond, true)
            finally
                unlock(idle.cond)
            end
            idle.active = false
        end
    end
    nothing
end

mutable struct UvTestIdle
    handle::Ptr{Cvoid}
    cond::Base.ThreadSynchronizer
    isopen::Bool
    active::Bool
    count::Int

    function UvTestIdle()
        this = new(Libc.malloc(Base._sizeof_uv_idle), Base.ThreadSynchronizer(), true, false, 0)
        Base.iolock_begin()
        Base.associate_julia_struct(this.handle, this)
        err = ccall(:uv_idle_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}),
            Base.eventloop(), this.handle)
        if err != 0
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(_UVError("uv_idle_init", err))
        end
        err = ccall(:uv_idle_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}),
            this.handle, @cfunction(idle_callback, Cvoid, (Ptr{Cvoid},)))
        if err != 0
            Libc.free(this.handle)
            this.handle = C_NULL
            throw(_UVError("uv_idle_start", err))
        end
        finalizer(Base.uvfinalize, this)
        Base.iolock_end()
        return this
    end
end
Base.unsafe_convert(::Type{Ptr{Cvoid}}, idle::UvTestIdle) = idle.handle

function Base.uvfinalize(t::UvTestIdle)
    Base.iolock_begin()
    Base.lock(t.cond)
    try
        if t.handle != C_NULL
            Base.disassociate_julia_struct(t.handle) # not going to call the usual close hooks
            if t.isopen
                t.isopen = false
                ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), t)
            end
            t.handle = C_NULL
            notify(t.cond, false)
        end
    finally
        unlock(t.cond)
    end
    Base.iolock_end()
    nothing
end

function Base.close(idle::UvTestIdle)
    Base.uvfinalize(idle)
end

function Base.wait(idle::UvTestIdle)
    Base.iolock_begin()
    Base.preserve_handle(idle)
    Base.lock(idle.cond)
    try
        idle.active = true
        Base.iolock_end()
        wait(idle.cond)
    finally
        Base.unlock(idle.cond)
        Base.iolock_begin()
        Base.unpreserve_handle(idle)
        Base.iolock_end()
    end
end

# Spawn another process as a watchdog. If this test fails, it'll unrecoverably
# hang in the event loop. Another process needs to kill it
cmd = """
    @async (Base.wait_readnb(stdin, 1); exit())
    sleep(100)
    isopen(stdin) || exit()
    println(stderr, "ERROR: Killing threads test due to watchdog expiry")
    ccall(:uv_kill, Cint, (Cint, Cint), $(getpid()), Base.SIGTERM)
"""
proc = open(pipeline(`$(Base.julia_cmd()) -e $cmd`; stderr=stderr); write=true)

let idle=UvTestIdle()
    wait(idle)
    close(idle)
end

using Base.Threads
@threads for i = 1:1
    let idle=UvTestIdle()
        wait(idle)
        close(idle)
    end
end

@test process_running(proc)

# We don't need the watchdog anymore
close(proc.in)
