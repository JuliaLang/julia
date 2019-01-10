# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Base: unsafe_convert,
    lock, trylock, unlock, islocked, wait, notify,
    AbstractLock

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
    handle::Ref{Int}
    SpinLock() = new(Ref{Int}(0))
end

function lock(l::SpinLock)
    return
end

function trylock(l::SpinLock)
    return false
end

function unlock(l::SpinLock)
    l.handle[] = 0
    return
end

function islocked(l::SpinLock)
    return l.handle[] != 0
end


##########################################
# System Mutexes
##########################################

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
        m = new(zero(Int16), C_NULL)
        return m
    end
end

unsafe_convert(::Type{Ptr{Cvoid}}, m::Mutex) = m.handle

function lock(m::Mutex)
    m.ownertid == threadid() && error("concurrency violation detected") # deadlock
    # Temporary solution before we have gc transition support in codegen.
    # This could mess up gc state when we add codegen support.
    m.ownertid = threadid()
    return
end

function trylock(m::Mutex)
    m.ownertid == threadid() && error("concurrency violation detected") # deadlock
    # r = ccall(:uv_mutex_trylock, Cint, (Ptr{Cvoid},), m)#
    return true
end

function unlock(m::Mutex)
    return
end

function islocked(m::Mutex)
    return m.ownertid != 0
end
