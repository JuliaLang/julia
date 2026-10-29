# This file is a part of Julia. License is MIT: https://julialang.org/license

# Run with 1 default and 1 interactive thread: threadid() == 1 runs the event loop.
#
# Regression test: the event loop thread, about to go to sleep after `uv_run` found nothing to
# do, ran finalizers that took the iolock. A worker that armed a timer in the meantime saw the
# iolock held, counted on its holder to get the loop serviced, and went to sleep too. Nobody ran
# the loop again, so the timer never fired.

using Base: iolock_begin, iolock_end

@assert Threads.threadid() == 1 && Threads.nthreads(:interactive) == 1 && Threads.nthreads(:default) == 1

# spin without yielding: parking a task wakes the event loop thread
function spin_until(f, timeout)
    t0 = time()
    while !f()
        time() - t0 > timeout && return false
        GC.safepoint()
        ccall(:jl_cpu_pause, Cvoid, ())
    end
    return true
end

const collected = Threads.Atomic{Bool}(false)
const fin_started = Threads.Atomic{Bool}(false)
const armed = Threads.Atomic{Bool}(false)
const fin_locked = Threads.Atomic{Bool}(false)
const waiting = Threads.Atomic{Bool}(false)

mutable struct Garbage end

# stands in for a batch of `uvfinalize` calls
function hold_iolock(_)
    # only act when run by the event loop thread right after the GC below
    (collected[] && Threads.threadid() == 1) || return
    fin_started[] = true
    spin_until(() -> armed[], 10)
    iolock_begin()
    fin_locked[] = true
    spin_until(() -> waiting[], 10)
    Libc.systemsleep(0.5) # let the worker go to sleep while we hold the iolock
    iolock_end()
end
@noinline make_garbage() = (finalizer(hold_iolock, Garbage()); nothing)
precompile(hold_iolock, (Garbage,))

# A libuv callback runs with the iolock held, so finalizers can't run after this GC; they're left
# pending until the event loop thread releases the iolock.
gc_cb(_) = (GC.gc(); collected[] = true; nothing)

function worker()
    GC.enable_finalizers(false) # leave the finalizer to the event loop thread
    spin_until(() -> collected[], 10) || error("the timer callback did not run")
    # the event loop thread is now running the finalizer on its way to sleep, unless it defers it
    racing = spin_until(() -> fin_started[], 0.5)
    t = Timer(0.01)
    armed[] = true
    racing && (spin_until(() -> fin_locked[], 10) || error("the finalizer did not take the iolock"))
    # wait without taking the iolock, which would wait for the finalizer to release it
    lock(t.cond)
    try
        waiting[] = true
        t.set || wait(t.cond)
    finally
        unlock(t.cond)
    end
    GC.enable_finalizers(true)
end

const loop = Base.eventloop()

# The ^C listener keeps a referenced handle on the loop, so it never runs empty;
# unreference it to get back to the case where `uv_run` finds nothing to do.
const alive = Ptr{Cvoid}[]
function walk_cb(h, _)
    if ccall(:uv_is_active, Cint, (Ptr{Cvoid},), h) != 0 && ccall(:uv_has_ref, Cint, (Ptr{Cvoid},), h) != 0
        push!(alive, h)
    end
    nothing
end
iolock_begin()
ccall(:uv_walk, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}, Ptr{Cvoid}),
      loop, @cfunction(walk_cb, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid})), C_NULL)
for h in alive
    @assert ccall(:uv_handle_get_type, Cint, (Ptr{Cvoid},), h) == Base.UV_ASYNC
    Base.uv_unref(h)
end
@assert ccall(:uv_loop_alive, Cint, (Ptr{Cvoid},), loop) == 0
iolock_end()

make_garbage()
w = Threads.@spawn worker()

# fire the GC from a libuv callback once this thread has gone to sleep in `uv_run`
const gc_timer = Libc.calloc(1, Base._sizeof_uv_timer)
iolock_begin()
ccall(:uv_timer_init, Cint, (Ptr{Cvoid}, Ptr{Cvoid}), loop, gc_timer)
ccall(:uv_update_time, Cvoid, (Ptr{Cvoid},), loop)
ccall(:uv_timer_start, Cint, (Ptr{Cvoid}, Ptr{Cvoid}, UInt64, UInt64),
      gc_timer, @cfunction(gc_cb, Cvoid, (Ptr{Cvoid},)), 500, 0)
iolock_end()

wait(w)

iolock_begin()
ccall(:jl_close_uv, Cvoid, (Ptr{Cvoid},), gc_timer)
iolock_end()
