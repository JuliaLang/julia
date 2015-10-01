# Copyright (c) 2015, Intel Corporation
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Intel Corporation nor the names of its contributors
#       may be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

include("uv_constants.jl")

export SpinLock, Mutex, init_lock!, destroy_lock!, lock!, trylock!, unlock!

abstract Lock

# Test-and-test-and-set spin locks are quickest up to about 30ish
# contending threads. If you have more contention than that, perhaps
# a lock is the wrong way to synchronize.
type TatasLock <: Lock
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
type RecursiveTatasLock <: Lock
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

# TODO: this size is tested correct for pthreads on Linux and Darwin only
const UV_MUTEX_SIZE = 48

type Mutex <: Lock
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
        return UV_EPERM
    end
    m.ownertid = 0
    ccall(:uv_mutex_unlock, Void, (Ptr{Void},), m.handle)
    return 0
end

