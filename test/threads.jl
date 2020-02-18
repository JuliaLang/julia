# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Base.Threads

let p, cmd = `$(Base.julia_cmd()) --depwarn=error --startup-file=no threads_exec.jl`
    # test both nthreads==1 and nthreads>1. spawn a process to test whichever
    # case we are not running currently.
    other_nthreads = nthreads() == 1 ? 4 : 1
    p = run(pipeline(setenv(cmd, "JULIA_NUM_THREADS" => other_nthreads), stdout = stdout, stderr = stderr),
            wait = false)
    include("threads_exec.jl")
    if !success(p)
        error("threads test failed with nthreads == $other_nthreads")
    end
end

# issue #34415 - make sure external affinity settings work
if Sys.islinux() && Sys.CPU_THREADS > 1 && Sys.which("taskset") !== nothing
    run_with_affinity(spec) = readchomp(`taskset -c $spec $(Base.julia_cmd()) -e "run(\`taskset -p \$(getpid())\`)"`)
    @test endswith(run_with_affinity("1"), "2")
    @test endswith(run_with_affinity("0,1"), "3")
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
            ccall(:usleep, Cint, (Csize_t,), 4000)
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

function Base.wait(idle::UvTestIdle)
    Base.iolock_begin()
    Base.preserve_handle(idle)
    Base.lock(idle.cond)
    try
        idle.active = true
        wait(idle.cond)
    finally
        Base.unlock(idle.cond)
        Base.unpreserve_handle(idle)
        Base.iolock_end()
    end
end

# Spawn another process as a watchdog. If this test fails, it'll uncrecoverably
# hang in the event loop. Another process needs to kill it
cmd = """
    sleep(100)
    println(stderr, "ERROR: Killing threads test due to watchdog expiry")
    ccall(:uv_kill, Cint, (Cint, Cint), $(getpid()), Base.SIGTERM)
"""
proc = run(pipeline(`$(Base.julia_cmd()) -e $cmd`; stderr=stderr); wait=false)

let idle=UvTestIdle()
    wait(idle)
end

using Base.Threads
@threads for i = 1:1
    let idle=UvTestIdle()
        wait(idle)
    end
end

@test process_running(proc)

# We don't need the watchdog anymore
kill(proc, Base.SIGKILL)
