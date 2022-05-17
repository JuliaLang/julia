# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Base: unsafe_convert, lock, trylock, unlock, islocked, wait, notify, AbstractLock

export SpinLock

# Important Note: these low-level primitives defined here
#   are typically not for general usage

##########################################
# Atomic Locks
##########################################

"""
    SpinLock()

Create a non-reentrant, test-and-test-and-set spin lock.
Recursive use will result in a deadlock.
This kind of lock should only be used around code that takes little time
to execute and does not block (e.g. perform I/O).
In general, [`ReentrantLock`](@ref) should be used instead.

Each [`lock`](@ref) must be matched with an [`unlock`](@ref).
If [`!islocked(lck::SpinLock)`](@ref islocked) holds, [`trylock(lck)`](@ref trylock)
succeeds unless there are other tasks attempting to hold the lock "at the same time."

Test-and-test-and-set spin locks are quickest up to about 30ish
contending threads. If you have more contention than that, different
synchronization approaches should be considered.
"""
mutable struct SpinLock <: AbstractLock
    # we make this much larger than necessary to minimize false-sharing
    @atomic owned::Int
    SpinLock() = new(0)
end

# Note: this cannot assert that the lock is held by the correct thread, because we do not
# track which thread locked it. Users beware.
Base.assert_havelock(l::SpinLock) = islocked(l) ? nothing : Base.concurrency_violation()

function lock(l::SpinLock)
    while true
        if @inline trylock(l)
            return
        end
        ccall(:jl_cpu_pause, Cvoid, ())
        # Temporary solution before we have gc transition support in codegen.
        ccall(:jl_gc_safepoint, Cvoid, ())
    end
end

function trylock(l::SpinLock)
    if l.owned == 0
        GC.disable_finalizers()
        p = @atomicswap :acquire l.owned = 1
        if p == 0
            return true
        end
        GC.enable_finalizers()
    end
    return false
end

function unlock(l::SpinLock)
    if (@atomicswap :release l.owned = 0) == 0
        error("unlock count must match lock count")
    end
    GC.enable_finalizers()
    ccall(:jl_cpu_wake, Cvoid, ())
    return
end

function islocked(l::SpinLock)
    return (@atomic :monotonic l.owned) != 0
end
