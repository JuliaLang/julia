# This file is a part of Julia. License is MIT: https://julialang.org/license

import .Base: unsafe_convert, lock, trylock, unlock, islocked, wait, notify, AbstractLock

export SpinLock
public PaddedSpinLock
# Important Note: these low-level primitives defined here
#   are typically not for general usage

##########################################
# Atomic Locks
##########################################

"""
    abstract type AbstractSpinLock <: AbstractLock end

A non-reentrant, test-and-test-and-set spin lock.
Recursive use will result in a deadlock.
This kind of lock should only be used around code that takes little time
to execute and does not block (e.g. perform I/O).
In general, [`ReentrantLock`](@ref) should be used instead.

Each [`lock`](@ref) must be matched with an [`unlock`](@ref).
If [`!islocked(lck::AbstractSpinLock)`](@ref islocked) holds, [`trylock(lck)`](@ref trylock)
succeeds unless there are other tasks attempting to hold the lock "at the same time."

Test-and-test-and-set spin locks are quickest up to about 30ish
contending threads. If you have more contention than that, different
synchronization approaches should be considered.
"""
abstract type AbstractSpinLock <: AbstractLock end

"""
    SpinLock() <: AbstractSpinLock

Spinlocks are not padded, and so may suffer from false sharing.
See also [`PaddedSpinLock`](@ref).

See the documentation for [`AbstractSpinLock`](@ref) regarding correct usage.
"""
mutable struct SpinLock <: AbstractSpinLock
    # we make this much larger than necessary to minimize false-sharing
    @atomic owned::Int
    SpinLock() = new(0)
end

# TODO: Determine the cache line size using e.g., CPUID. Meanwhile, this is correct for most
# processors.
const CACHE_LINE_SIZE = 64

"""
    PaddedSpinLock() <: AbstractSpinLock

PaddedSpinLocks are padded so that each is guaranteed to be on its own cache line, to avoid
false sharing.
See also [`SpinLock`](@ref).

See the documentation for [`AbstractSpinLock`](@ref) regarding correct usage.
"""
mutable struct PaddedSpinLock <: AbstractSpinLock
    # we make this much larger than necessary to minimize false-sharing
    _padding_before::NTuple{max(0, CACHE_LINE_SIZE - sizeof(Int)), UInt8}
    @atomic owned::Int
    _padding_after::NTuple{max(0, CACHE_LINE_SIZE - sizeof(Int)), UInt8}
    function PaddedSpinLock()
        l = new()
        @atomic l.owned = 0
        return l
    end
end

# Note: this cannot assert that the lock is held by the correct thread, because we do not
# track which thread locked it. Users beware.
Base.assert_havelock(l::AbstractSpinLock) = islocked(l) ? nothing : Base.concurrency_violation()

function lock(l::AbstractSpinLock)
    while true
        if @inline trylock(l)
            return
        end
        ccall(:jl_cpu_suspend, Cvoid, ())
        # Temporary solution before we have gc transition support in codegen.
        ccall(:jl_gc_safepoint, Cvoid, ())
    end
end

function trylock(l::AbstractSpinLock)
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

function unlock(l::AbstractSpinLock)
    if (@atomicswap :release l.owned = 0) == 0
        error("unlock count must match lock count")
    end
    GC.enable_finalizers()
    ccall(:jl_cpu_wake, Cvoid, ())
    return
end

function islocked(l::AbstractSpinLock)
    return (@atomic :monotonic l.owned) != 0
end
