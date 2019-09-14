# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Base: unsafe_convert, lock, trylock, unlock, islocked, wait, notify, AbstractLock

export SpinLock

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
"""
struct SpinLock <: AbstractLock
    handle::Atomic{Int}
    SpinLock() = new(Atomic{Int}(0))
end

# Note: this cannot assert that the lock is held by the correct thread, because we do not
# track which thread locked it. Users beware.
Base.assert_havelock(l::SpinLock) = islocked(l) ? nothing : concurrency_violation()

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
