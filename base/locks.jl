# This file is a part of Julia. License is MIT: http://julialang.org/license

export SpinLock, Mutex, init_lock!, destroy_lock!, lock!, trylock!, unlock!

abstract AbstractLock

# Test-and-test-and-set spin locks are quickest up to about 30ish
# contending threads. If you have more contention than that, perhaps
# a lock is the wrong way to synchronize.
type TatasLock <: AbstractLock
    handle::Atomic{Int}
    TatasLock() = new(Atomic{Int}(0))
end

typealias SpinLock TatasLock

function lock!(l::TatasLock)
    while true
        if l.handle[] == 0
            p = atomic_xchg!(l.handle, 1)
            if p == 0
                return 0
            end
        end
        # TODO: pause
    end
end

function trylock!(l::TatasLock)
    if l.handle[] == 0
        return atomic_xchg!(l.handle, 1)
    end
    return 1
end

function unlock!(l::TatasLock)
    l.handle[] = 0
    return 0
end


# Recursive test-and-test-and-set lock. Slower.
type RecursiveTatasLock <: AbstractLock
    ownertid::Atomic{Int16}
    handle::Atomic{Int}
    RecursiveTatasLock() = new(0, Atomic{Int}(0))
end

typealias RecursiveSpinLock RecursiveTatasLock

function lock!(l::RecursiveTatasLock)
    if l.ownertid[] == threadid()
        return 0
    end
    while true
        if l.handle[] == 0
            p = atomic_xchg!(l.handle, 1)
            if p == 0
                l.ownertid[] = threadid()
                return 0
            end
        end
        # TODO: pause
    end
end

function trylock!(l::RecursiveTatasLock)
    if l.ownertid[] == threadid()
        return 0
    end
    if l.handle[] == 0
        p = atomic_xchg!(l.handle, 1)
        if p == 0
            l.ownertid[] = threadid()
        end
        return p
    end
    return 1
end

function unlock!(l::RecursiveTatasLock)
    if l.ownertid[] != threadid()
        return 1
    end
    l.ownertid[] = 0
    l.handle[] = 0
    return 0
end


# These are mutexes from libuv, which abstract pthread mutexes and
# Windows critical sections. We're doing some error checking (and
# paying for it in overhead), but regardless, in some situations,
# passing a bad parameter will cause an abort.

# TODO: how defensive to get, and how to turn it off?
# TODO: how to catch an abort?

const UV_MUTEX_SIZE = ccall(:jl_sizeof_uv_mutex, Cint, ())

type Mutex <: AbstractLock
    ownertid::Int16
    handle::Array{Int8}
    Mutex() = (m = new(zero(Int16), zeros(Int8, UV_MUTEX_SIZE));
               ccall(:uv_mutex_init, Void, (Ptr{Void},), m.handle);
               finalizer(m, (x -> ccall(:uv_mutex_destroy, Void, (Ptr{Void},), x.handle)));
               m)
end

function lock!(m::Mutex)
    if m.ownertid == threadid()
        return 0
    end
    ccall(:uv_mutex_lock, Void, (Ptr{Void},), m.handle)
    m.ownertid = threadid()
    return 0
end

function trylock!(m::Mutex)
    if m.ownertid == threadid()
        return 0
    end
    r = ccall(:uv_mutex_trylock, Cint, (Ptr{Void},), m.handle)
    if r == 0
        m.ownertid = threadid()
    end
    return r
end

function unlock!(m::Mutex)
    if m.ownertid != threadid()
        return Base.UV_EPERM
    end
    m.ownertid = 0
    ccall(:uv_mutex_unlock, Void, (Ptr{Void},), m.handle)
    return 0
end

