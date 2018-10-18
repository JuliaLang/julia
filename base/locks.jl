# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Base: _uv_hook_close, unsafe_convert,
    lock, trylock, unlock, islocked

export SpinLock, RecursiveSpinLock, Mutex


##########################################
# Atomic Locks
##########################################

"""
    AbstractLock

Abstract supertype describing types that
implement the thread-safe synchronization primitives:
[`lock`](@ref), [`trylock`](@ref), [`unlock`](@ref), and [`islocked`](@ref).
"""
abstract type AbstractLock end

# Test-and-test-and-set spin locks are quickest up to about 30ish
# contending threads. If you have more contention than that, perhaps
# a lock is the wrong way to synchronize.
"""
    TatasLock()

See [`SpinLock`](@ref).
"""
struct TatasLock <: AbstractLock
    handle::Atomic{Int}
    TatasLock() = new(Atomic{Int}(0))
end

"""
    SpinLock()

Create a non-reentrant lock.
Recursive use will result in a deadlock.
Each [`lock`](@ref) must be matched with an [`unlock`](@ref).

Test-and-test-and-set spin locks are quickest up to about 30ish
contending threads. If you have more contention than that, perhaps
a lock is the wrong way to synchronize.

See also [`RecursiveSpinLock`](@ref) for a version that permits recursion.

See also [`Mutex`](@ref) for a more efficient version on one core or if the
lock may be held for a considerable length of time.
"""
const SpinLock = TatasLock

function lock(l::TatasLock)
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

function trylock(l::TatasLock)
    if l.handle[] == 0
        return atomic_xchg!(l.handle, 1) == 0
    end
    return false
end

function unlock(l::TatasLock)
    l.handle[] = 0
    ccall(:jl_cpu_wake, Cvoid, ())
    return
end

function islocked(l::TatasLock)
    return l.handle[] != 0
end


"""
    RecursiveTatasLock()

See [`RecursiveSpinLock`](@ref).
"""
struct RecursiveTatasLock <: AbstractLock
    ownertid::Atomic{Int16}
    handle::Atomic{Int}
    RecursiveTatasLock() = new(Atomic{Int16}(0), Atomic{Int}(0))
end

"""
    RecursiveSpinLock()

Creates a reentrant lock.
The same thread can acquire the lock as many times as required.
Each [`lock`](@ref) must be matched with an [`unlock`](@ref).

See also [`SpinLock`](@ref) for a slightly faster version.

See also [`Mutex`](@ref) for a more efficient version on one core or if the lock
may be held for a considerable length of time.
"""
const RecursiveSpinLock = RecursiveTatasLock

function lock(l::RecursiveTatasLock)
    if l.ownertid[] == threadid()
        l.handle[] += 1
        return
    end
    while true
        if l.handle[] == 0
            if atomic_cas!(l.handle, 0, 1) == 0
                l.ownertid[] = threadid()
                return
            end
        end
        ccall(:jl_cpu_pause, Cvoid, ())
        # Temporary solution before we have gc transition support in codegen.
        ccall(:jl_gc_safepoint, Cvoid, ())
    end
end

function trylock(l::RecursiveTatasLock)
    if l.ownertid[] == threadid()
        l.handle[] += 1
        return true
    end
    if l.handle[] == 0
        if atomic_cas!(l.handle, 0, 1) == 0
            l.ownertid[] = threadid()
            return true
        end
        return false
    end
    return false
end

function unlock(l::RecursiveTatasLock)
    @assert(l.ownertid[] == threadid(), "unlock from wrong thread")
    @assert(l.handle[] != 0, "unlock count must match lock count")
    if l.handle[] == 1
        l.ownertid[] = 0
        l.handle[] = 0
        ccall(:jl_cpu_wake, Cvoid, ())
    else
        l.handle[] -= 1
    end
    return
end

function islocked(l::RecursiveTatasLock)
    return l.handle[] != 0
end


##########################################
# System Mutexes
##########################################

# These are mutexes from libuv. We're doing some error checking (and
# paying for it in overhead), but regardless, in some situations,
# passing a bad parameter will cause an abort.

# TODO: how defensive to get, and how to turn it off?
# TODO: how to catch an abort?

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
        finalizer(_uv_hook_close, m)
        return m
    end
end

unsafe_convert(::Type{Ptr{Cvoid}}, m::Mutex) = m.handle

function _uv_hook_close(x::Mutex)
    h = x.handle
    if h != C_NULL
        x.handle = C_NULL
        ccall(:uv_mutex_destroy, Cvoid, (Ptr{Cvoid},), h)
        Libc.free(h)
        nothing
    end
end

function lock(m::Mutex)
    if m.ownertid == threadid()
        return
    end
    # Temporary solution before we have gc transition support in codegen.
    # This could mess up gc state when we add codegen support.
    gc_state = ccall(:jl_gc_safe_enter, Int8, ())
    ccall(:uv_mutex_lock, Cvoid, (Ptr{Cvoid},), m)
    ccall(:jl_gc_safe_leave, Cvoid, (Int8,), gc_state)
    m.ownertid = threadid()
    return
end

function trylock(m::Mutex)
    if m.ownertid == threadid()
        return true
    end
    r = ccall(:uv_mutex_trylock, Cint, (Ptr{Cvoid},), m)
    if r == 0
        m.ownertid = threadid()
    end
    return r == 0
end

function unlock(m::Mutex)
    @assert(m.ownertid == threadid(), "unlock from wrong thread")
    m.ownertid = 0
    ccall(:uv_mutex_unlock, Cvoid, (Ptr{Cvoid},), m)
    return
end

function islocked(m::Mutex)
    return m.ownertid != 0
end
