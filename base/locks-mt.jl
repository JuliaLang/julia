# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Base: unsafe_convert, lock, trylock, unlock, islocked, wait, notify, AbstractLock

# Important Note: these low-level primitives defined here
#   are typically not for general usage

##########################################
# Atomic Locks
##########################################

# Test-and-test-and-set spin locks are quickest up to about 30ish
# contending threads. If you have more contention than that, perhaps
# a lock is the wrong way to synchronize.
"""
    SpinLock()

Create a non-reentrant lock.
Recursive use will result in a deadlock.
Each [`lock`](@ref) must be matched with an [`unlock`](@ref).

Test-and-test-and-set spin locks are quickest up to about 30ish
contending threads. If you have more contention than that, perhaps
a lock is the wrong way to synchronize.

See also [`Mutex`](@ref) for a more efficient version on one core or if the
lock may be held for a considerable length of time.
"""
struct SpinLock <: AbstractLock
    handle::Atomic{Int}
    SpinLock() = new(Atomic{Int}(0))
end

function lock(l::SpinLock)
    while true
        if l.handle[] == 0
            p = atomic_xchg!(l.handle, 1)
            if p == 0
                return
            end
        end
        ccall(:jl_cpu_pause, Cvoid, ())
        # Temporary solution before we have gc transition support in codegen.
        ccall(:jl_gc_safepoint, Cvoid, ())
    end
end

function trylock(l::SpinLock)
    if l.handle[] == 0
        return atomic_xchg!(l.handle, 1) == 0
    end
    return false
end

function unlock(l::SpinLock)
    l.handle[] = 0
    ccall(:jl_cpu_wake, Cvoid, ())
    return
end

function islocked(l::SpinLock)
    return l.handle[] != 0
end


##########################################
# System Mutexes
##########################################

# These are mutexes from libuv.
const UV_MUTEX_SIZE = ccall(:jl_sizeof_uv_mutex, Cint, ())

"""
    Mutex()

These are standard system mutexes for locking critical sections of logic.

On Windows, this is a critical section object,
on pthreads, this is a `pthread_mutex_t`.

See also [`SpinLock`](@ref) for a lighter-weight lock.
"""
mutable struct Mutex <: AbstractLock
    ownertid::Int16
    handle::Ptr{Cvoid}
    function Mutex()
        m = new(zero(Int16), Libc.malloc(UV_MUTEX_SIZE))
        ccall(:uv_mutex_init, Cvoid, (Ptr{Cvoid},), m.handle)
        finalizer(mutex_destroy, m)
        return m
    end
end

unsafe_convert(::Type{Ptr{Cvoid}}, m::Mutex) = m.handle

function mutex_destroy(x::Mutex)
    h = x.handle
    if h != C_NULL
        x.handle = C_NULL
        ccall(:uv_mutex_destroy, Cvoid, (Ptr{Cvoid},), h)
        Libc.free(h)
        nothing
    end
end

function lock(m::Mutex)
    m.ownertid == threadid() && concurrency_violation() # deadlock
    # Temporary solution before we have gc transition support in codegen.
    # This could mess up gc state when we add codegen support.
    gc_state = ccall(:jl_gc_safe_enter, Int8, ())
    ccall(:uv_mutex_lock, Cvoid, (Ptr{Cvoid},), m)
    ccall(:jl_gc_safe_leave, Cvoid, (Int8,), gc_state)
    m.ownertid = threadid()
    return
end

function trylock(m::Mutex)
    m.ownertid == threadid() && concurrency_violation() # deadlock
    r = ccall(:uv_mutex_trylock, Cint, (Ptr{Cvoid},), m)
    if r == 0
        m.ownertid = threadid()
    end
    return r == 0
end

function unlock(m::Mutex)
    m.ownertid == threadid() || concurrency_violation()
    m.ownertid = 0
    ccall(:uv_mutex_unlock, Cvoid, (Ptr{Cvoid},), m)
    return
end

function islocked(m::Mutex)
    return m.ownertid != 0
end
