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

Test-and-test-and-set spin locks are quickest up to about 30ish
contending threads. If you have more contention than that, different
synchronization approaches should be considered.
"""
mutable struct SpinLock <: AbstractLock
    handle::Int
    SpinLock() = new(0)
end

import Base.Sys.WORD_SIZE

@eval _xchg!(x::SpinLock, v::Int) =
    llvmcall($"""
             %ptr = inttoptr i$WORD_SIZE %0 to i$WORD_SIZE*
             %rv = atomicrmw xchg i$WORD_SIZE* %ptr, i$WORD_SIZE %1 acq_rel
             ret i$WORD_SIZE %rv
             """, Int, Tuple{Ptr{Int}, Int}, unsafe_convert(Ptr{Int}, pointer_from_objref(x)), v)

@eval _get(x::SpinLock) =
    llvmcall($"""
             %ptr = inttoptr i$WORD_SIZE %0 to i$WORD_SIZE*
             %rv = load atomic i$WORD_SIZE, i$WORD_SIZE* %ptr acquire, align $(gc_alignment(Int))
             ret i$WORD_SIZE %rv
             """, Int, Tuple{Ptr{Int}}, unsafe_convert(Ptr{Int}, pointer_from_objref(x)))

@eval _set!(x::SpinLock, v::Int) =
    llvmcall($"""
             %ptr = inttoptr i$WORD_SIZE %0 to i$WORD_SIZE*
             store atomic i$WORD_SIZE %1, i$WORD_SIZE* %ptr release, align $(gc_alignment(Int))
             ret void
             """, Cvoid, Tuple{Ptr{Int}, Int}, unsafe_convert(Ptr{Int}, pointer_from_objref(x)), v)

# Note: this cannot assert that the lock is held by the correct thread, because we do not
# track which thread locked it. Users beware.
Base.assert_havelock(l::SpinLock) = islocked(l) ? nothing : Base.concurrency_violation()

function lock(l::SpinLock)
    while true
        if _get(l) == 0
            p = _xchg!(l, 1)
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
    if _get(l) == 0
        return _xchg!(l, 1) == 0
    end
    return false
end

function unlock(l::SpinLock)
    _set!(l, 0)
    ccall(:jl_cpu_wake, Cvoid, ())
    return
end

function islocked(l::SpinLock)
    return _get(l) != 0
end
