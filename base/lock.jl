# This file is a part of Julia. License is MIT: https://julialang.org/license

const ThreadSynchronizer = GenericCondition{Threads.SpinLock}

"""
    current_task()

Get the currently running [`Task`](@ref).
"""
current_task() = ccall(:jl_get_current_task, Ref{Task}, ())

# Advisory reentrant lock
"""
    ReentrantLock()

Creates a re-entrant lock for synchronizing [`Task`](@ref)s. The same task can
acquire the lock as many times as required (this is what the "Reentrant" part
of the name means). Each [`lock`](@ref) must be matched with an [`unlock`](@ref).

Calling `lock` will also inhibit running of finalizers on that thread until the
corresponding `unlock`. Use of the standard lock pattern illustrated below
should naturally be supported, but beware of inverting the try/lock order or
missing the try block entirely (e.g. attempting to return with the lock still
held):

This provides a acquire/release memory ordering on lock/unlock calls.

```
lock(l)
try
    <atomic work>
finally
    unlock(l)
end
```

If [`!islocked(lck::ReentrantLock)`](@ref islocked) holds, [`trylock(lck)`](@ref trylock)
succeeds unless there are other tasks attempting to hold the lock "at the same time."
"""
mutable struct ReentrantLock <: AbstractLock
    # offset = 16
    @atomic locked_by::Union{Task, Nothing}
    # offset32 = 20, offset64 = 24
    reentrancy_cnt::UInt32
    # offset32 = 24, offset64 = 28
    @atomic havelock::UInt8 # 0x0 = none, 0x1 = lock, 0x2 = conflict
    # offset32 = 28, offset64 = 32
    cond_wait::ThreadSynchronizer # 2 words
    # offset32 = 36, offset64 = 48
    # sizeof32 = 20, sizeof64 = 32
    # now add padding to make this a full cache line to minimize false sharing between objects
    _::NTuple{Int === Int32 ? 2 : 3, Int}
    # offset32 = 44, offset64 = 72 == sizeof+offset
    # sizeof32 = 28, sizeof64 = 56

    ReentrantLock() = new(nothing, 0x0000_0000, 0x00, ThreadSynchronizer())
end

assert_havelock(l::ReentrantLock) = assert_havelock(l, l.locked_by)

show(io::IO, ::ReentrantLock) = print(io, ReentrantLock, "()")

function show(io::IO, ::MIME"text/plain", l::ReentrantLock)
    show(io, l)
    if !(get(io, :compact, false)::Bool)
        locked_by = l.locked_by
        if locked_by isa Task
            print(io, " (locked by ", locked_by === current_task() ? "current " : "", locked_by, ")")
        else
            print(io, " (unlocked)")
        end
    end
end

"""
    islocked(lock) -> Status (Boolean)

Check whether the `lock` is held by any task/thread.
This function alone should not be used for synchronization. However, `islocked` combined
with [`trylock`](@ref) can be used for writing the test-and-test-and-set or exponential
backoff algorithms *if it is supported by the `typeof(lock)`* (read its documentation).

# Extended help

For example, an exponential backoff can be implemented as follows if the `lock`
implementation satisfied the properties documented below.

```julia
nspins = 0
while true
    while islocked(lock)
        GC.safepoint()
        nspins += 1
        nspins > LIMIT && error("timeout")
    end
    trylock(lock) && break
    backoff()
end
```

## Implementation

A lock implementation is advised to define `islocked` with the following properties and note
it in its docstring.

* `islocked(lock)` is data-race-free.
* If `islocked(lock)` returns `false`, an immediate invocation of `trylock(lock)` must
  succeed (returns `true`) if there is no interference from other tasks.
"""
function islocked end
# Above docstring is a documentation for the abstract interface and not the one specific to
# `ReentrantLock`.

function islocked(rl::ReentrantLock)
    return (@atomic :monotonic rl.havelock) != 0
end

"""
    trylock(lock) -> Success (Boolean)

Acquire the lock if it is available,
and return `true` if successful.
If the lock is already locked by a different task/thread,
return `false`.

Each successful `trylock` must be matched by an [`unlock`](@ref).

Function `trylock` combined with [`islocked`](@ref) can be used for writing the
test-and-test-and-set or exponential backoff algorithms *if it is supported by the
`typeof(lock)`* (read its documentation).
"""
function trylock end
# Above docstring is a documentation for the abstract interface and not the one specific to
# `ReentrantLock`.

@inline function trylock(rl::ReentrantLock)
    ct = current_task()
    if rl.locked_by === ct
        #@assert rl.havelock !== 0x00
        rl.reentrancy_cnt += 0x0000_0001
        return true
    end
    return _trylock(rl, ct)
end
@noinline function _trylock(rl::ReentrantLock, ct::Task)
    GC.disable_finalizers()
    if (@atomicreplace :acquire rl.havelock 0x00 => 0x01).success
        #@assert rl.locked_by === nothing
        #@assert rl.reentrancy_cnt === 0
        rl.reentrancy_cnt = 0x0000_0001
        @atomic :release rl.locked_by = ct
        return true
    end
    GC.enable_finalizers()
    return false
end

"""
    lock(lock)

Acquire the `lock` when it becomes available.
If the lock is already locked by a different task/thread,
wait for it to become available.

Each `lock` must be matched by an [`unlock`](@ref).
"""
@inline function lock(rl::ReentrantLock)
    trylock(rl) || (@noinline function slowlock(rl::ReentrantLock)
        Threads.lock_profiling() && Threads.inc_lock_conflict_count()
        c = rl.cond_wait
        lock(c.lock)
        try
            while true
                if (@atomicreplace rl.havelock 0x01 => 0x02).old == 0x00 # :sequentially_consistent ? # now either 0x00 or 0x02
                    # it was unlocked, so try to lock it ourself
                    _trylock(rl, current_task()) && break
                else # it was locked, so now wait for the release to notify us
                    wait(c)
                end
            end
        finally
            unlock(c.lock)
        end
    end)(rl)
    return
end

"""
    unlock(lock)

Releases ownership of the `lock`.

If this is a recursive lock which has been acquired before, decrement an
internal counter and return immediately.
"""
@inline function unlock(rl::ReentrantLock)
    rl.locked_by === current_task() ||
        error(rl.reentrancy_cnt == 0x0000_0000 ? "unlock count must match lock count" : "unlock from wrong thread")
    (@noinline function _unlock(rl::ReentrantLock)
        n = rl.reentrancy_cnt - 0x0000_0001
        rl.reentrancy_cnt = n
        if n == 0x0000_00000
            @atomic :monotonic rl.locked_by = nothing
            if (@atomicswap :release rl.havelock = 0x00) == 0x02
                (@noinline function notifywaiters(rl)
                    cond_wait = rl.cond_wait
                    lock(cond_wait)
                    try
                        notify(cond_wait)
                    finally
                        unlock(cond_wait)
                    end
                end)(rl)
            end
            return true
        end
        return false
    end)(rl) && GC.enable_finalizers()
    nothing
end

function unlockall(rl::ReentrantLock)
    n = @atomicswap :not_atomic rl.reentrancy_cnt = 0x0000_0001
    unlock(rl)
    return n
end

function relockall(rl::ReentrantLock, n::UInt32)
    lock(rl)
    old = @atomicswap :not_atomic rl.reentrancy_cnt = n
    old == 0x0000_0001 || concurrency_violation()
    return
end

"""
    lock(f::Function, lock)

Acquire the `lock`, execute `f` with the `lock` held, and release the `lock` when `f`
returns. If the lock is already locked by a different task/thread, wait for it to become
available.

When this function returns, the `lock` has been released, so the caller should
not attempt to `unlock` it.

See also: [`@lock`](@ref).

!!! compat "Julia 1.7"
    Using a [`Channel`](@ref) as the second argument requires Julia 1.7 or later.
"""
function lock(f, l::AbstractLock)
    lock(l)
    try
        return f()
    finally
        unlock(l)
    end
end

function trylock(f, l::AbstractLock)
    if trylock(l)
        try
            return f()
        finally
            unlock(l)
        end
    end
    return false
end

"""
    @lock l expr

Macro version of `lock(f, l::AbstractLock)` but with `expr` instead of `f` function.
Expands to:
```julia
lock(l)
try
    expr
finally
    unlock(l)
end
```
This is similar to using [`lock`](@ref) with a `do` block, but avoids creating a closure
and thus can improve the performance.

!!! compat
    `@lock` was added in Julia 1.3, and exported in Julia 1.10.
"""
macro lock(l, expr)
    quote
        temp = $(esc(l))
        lock(temp)
        try
            $(esc(expr))
        finally
            unlock(temp)
        end
    end
end

"""
    @lock_nofail l expr

Equivalent to `@lock l expr` for cases in which we can guarantee that the function
will not throw any error. In this case, avoiding try-catch can improve the performance.
See [`@lock`](@ref).
"""
macro lock_nofail(l, expr)
    quote
        temp = $(esc(l))
        lock(temp)
        val = $(esc(expr))
        unlock(temp)
        val
    end
end

"""
  Lockable(value, lock = ReentrantLock())

Creates a `Lockable` object that wraps `value` and
associates it with the provided `lock`. This object
supports [`@lock`](@ref), [`lock`](@ref), [`trylock`](@ref),
[`unlock`](@ref). To access the value, index the lockable object while
holding the lock.

!!! compat "Julia 1.11"
    Requires at least Julia 1.11.

## Example

```jldoctest
julia> locked_list = Base.Lockable(Int[]);

julia> @lock(locked_list, push!(locked_list[], 1)) # must hold the lock to access the value
1-element Vector{Int64}:
 1

julia> lock(summary, locked_list)
"1-element Vector{Int64}"
```
"""
struct Lockable{T, L <: AbstractLock}
    value::T
    lock::L
end

Lockable(value) = Lockable(value, ReentrantLock())
getindex(l::Lockable) = (assert_havelock(l.lock); l.value)

"""
  lock(f::Function, l::Lockable)

Acquire the lock associated with `l`, execute `f` with the lock held,
and release the lock when `f` returns. `f` will receive one positional
argument: the value wrapped by `l`. If the lock is already locked by a
different task/thread, wait for it to become available.
When this function returns, the `lock` has been released, so the caller should
not attempt to `unlock` it.

!!! compat "Julia 1.11"
    Requires at least Julia 1.11.
"""
function lock(f, l::Lockable)
    lock(l.lock) do
        f(l.value)
    end
end

# implement the rest of the Lock interface on Lockable
lock(l::Lockable) = lock(l.lock)
trylock(l::Lockable) = trylock(l.lock)
unlock(l::Lockable) = unlock(l.lock)

@eval Threads begin
    """
        Threads.Condition([lock])

    A thread-safe version of [`Base.Condition`](@ref).

    To call [`wait`](@ref) or [`notify`](@ref) on a `Threads.Condition`, you must first call
    [`lock`](@ref) on it. When `wait` is called, the lock is atomically released during
    blocking, and will be reacquired before `wait` returns. Therefore idiomatic use
    of a `Threads.Condition` `c` looks like the following:

    ```
    lock(c)
    try
        while !thing_we_are_waiting_for
            wait(c)
        end
    finally
        unlock(c)
    end
    ```

    !!! compat "Julia 1.2"
        This functionality requires at least Julia 1.2.
    """
    const Condition = Base.GenericCondition{Base.ReentrantLock}

    """
    Special note for [`Threads.Condition`](@ref):

    The caller must be holding the [`lock`](@ref) that owns a `Threads.Condition` before calling this method.
    The calling task will be blocked until some other task wakes it,
    usually by calling [`notify`](@ref) on the same `Threads.Condition` object.
    The lock will be atomically released when blocking (even if it was locked recursively),
    and will be reacquired before returning.
    """
    wait(c::Condition)
end

"""
    Semaphore(sem_size)

Create a counting semaphore that allows at most `sem_size`
acquires to be in use at any time.
Each acquire must be matched with a release.

This provides a acquire & release memory ordering on acquire/release calls.
"""
mutable struct Semaphore
    sem_size::Int
    curr_cnt::Int
    cond_wait::Threads.Condition
    Semaphore(sem_size) = sem_size > 0 ? new(sem_size, 0, Threads.Condition()) : throw(ArgumentError("Semaphore size must be > 0"))
end

"""
    acquire(s::Semaphore)

Wait for one of the `sem_size` permits to be available,
blocking until one can be acquired.
"""
function acquire(s::Semaphore)
    lock(s.cond_wait)
    try
        while s.curr_cnt >= s.sem_size
            wait(s.cond_wait)
        end
        s.curr_cnt = s.curr_cnt + 1
    finally
        unlock(s.cond_wait)
    end
    return
end

"""
    acquire(f, s::Semaphore)

Execute `f` after acquiring from Semaphore `s`,
and `release` on completion or error.

For example, a do-block form that ensures only 2
calls of `foo` will be active at the same time:

```julia
s = Base.Semaphore(2)
@sync for _ in 1:100
    Threads.@spawn begin
        Base.acquire(s) do
            foo()
        end
    end
end
```

!!! compat "Julia 1.8"
    This method requires at least Julia 1.8.

"""
function acquire(f, s::Semaphore)
    acquire(s)
    try
        return f()
    finally
        release(s)
    end
end

"""
    release(s::Semaphore)

Return one permit to the pool,
possibly allowing another task to acquire it
and resume execution.
"""
function release(s::Semaphore)
    lock(s.cond_wait)
    try
        s.curr_cnt > 0 || error("release count must match acquire count")
        s.curr_cnt -= 1
        notify(s.cond_wait; all=false)
    finally
        unlock(s.cond_wait)
    end
    return
end


"""
    Event([autoreset=false])

Create a level-triggered event source. Tasks that call [`wait`](@ref) on an
`Event` are suspended and queued until [`notify`](@ref) is called on the `Event`.
After `notify` is called, the `Event` remains in a signaled state and
tasks will no longer block when waiting for it, until `reset` is called.

If `autoreset` is true, at most one task will be released from `wait` for
each call to `notify`.

This provides an acquire & release memory ordering on notify/wait.

!!! compat "Julia 1.1"
    This functionality requires at least Julia 1.1.

!!! compat "Julia 1.8"
    The `autoreset` functionality and memory ordering guarantee requires at least Julia 1.8.
"""
mutable struct Event
    const notify::Threads.Condition
    const autoreset::Bool
    @atomic set::Bool
    Event(autoreset::Bool=false) = new(Threads.Condition(), autoreset, false)
end

function wait(e::Event)
    if e.autoreset
        (@atomicswap :acquire_release e.set = false) && return
    else
        (@atomic e.set) && return # full barrier also
    end
    lock(e.notify) # acquire barrier
    try
        if e.autoreset
            (@atomicswap :acquire_release e.set = false) && return
        else
            e.set && return
        end
        wait(e.notify)
    finally
        unlock(e.notify) # release barrier
    end
    nothing
end

function notify(e::Event)
    lock(e.notify) # acquire barrier
    try
        if e.autoreset
            if notify(e.notify, all=false) == 0
                @atomic :release e.set = true
            end
        elseif !e.set
            @atomic :release e.set = true
            notify(e.notify)
        end
    finally
        unlock(e.notify)
    end
    nothing
end

"""
    reset(::Event)

Reset an [`Event`](@ref) back into an un-set state. Then any future calls to `wait` will
block until [`notify`](@ref) is called again.
"""
function reset(e::Event)
    @atomic e.set = false # full barrier
    nothing
end

@eval Threads begin
    import .Base: Event
    export Event
end

const PerStateInitial       = 0x00
const PerStateHasrun        = 0x01
const PerStateErrored       = 0x02
const PerStateConcurrent    = 0x03

"""
    OncePerProcess{T}(init::Function)() -> T

Calling a `OncePerProcess` object returns a value of type `T` by running the
function `initializer` exactly once per process. All concurrent and future
calls in the same process will return exactly the same value. This is useful in
code that will be precompiled, as it allows setting up caches or other state
which won't get serialized.

## Example

```jldoctest
julia> const global_state = Base.OncePerProcess{Vector{UInt32}}() do
           println("Making lazy global value...done.")
           return [Libc.rand()]
       end;

julia> (procstate = global_state()) |> typeof
Making lazy global value...done.
Vector{UInt32} (alias for Array{UInt32, 1})

julia> procstate === global_state()
true

julia> procstate === fetch(@async global_state())
true
```
"""
mutable struct OncePerProcess{T, F}
    value::Union{Nothing,T}
    @atomic state::UInt8 # 0=initial, 1=hasrun, 2=error
    @atomic allow_compile_time::Bool
    const initializer::F
    const lock::ReentrantLock

    function OncePerProcess{T,F}(initializer::F) where {T, F}
        once = new{T,F}(nothing, PerStateInitial, true, initializer, ReentrantLock())
        ccall(:jl_set_precompile_field_replace, Cvoid, (Any, Any, Any),
            once, :value, nothing)
        ccall(:jl_set_precompile_field_replace, Cvoid, (Any, Any, Any),
            once, :state, PerStateInitial)
        return once
    end
end
OncePerProcess{T}(initializer::F) where {T, F} = OncePerProcess{T, F}(initializer)
OncePerProcess(initializer) = OncePerProcess{Base.promote_op(initializer), typeof(initializer)}(initializer)
@inline function (once::OncePerProcess{T})() where T
    state = (@atomic :acquire once.state)
    if state != PerStateHasrun
        (@noinline function init_perprocesss(once, state)
            state == PerStateErrored && error("OncePerProcess initializer failed previously")
            once.allow_compile_time || __precompile__(false)
            lock(once.lock)
            try
                state = @atomic :monotonic once.state
                if state == PerStateInitial
                    once.value = once.initializer()
                elseif state == PerStateErrored
                    error("OncePerProcess initializer failed previously")
                elseif state != PerStateHasrun
                    error("invalid state for OncePerProcess")
                end
            catch
                state == PerStateErrored || @atomic :release once.state = PerStateErrored
                unlock(once.lock)
                rethrow()
            end
            state == PerStateHasrun || @atomic :release once.state = PerStateHasrun
            unlock(once.lock)
            nothing
        end)(once, state)
    end
    return once.value::T
end

function copyto_monotonic!(dest::AtomicMemory, src)
    i = 1
    for j in eachindex(src)
        if isassigned(src, j)
            @atomic :monotonic dest[i] = src[j]
        #else
        #    _unsetindex_atomic!(dest, i, src[j], :monotonic)
        end
        i += 1
    end
    dest
end

function fill_monotonic!(dest::AtomicMemory, x)
    for i = 1:length(dest)
        @atomic :monotonic dest[i] = x
    end
    dest
end


# share a lock/condition, since we just need it briefly, so some contention is okay
const PerThreadLock = ThreadSynchronizer()
"""
    OncePerThread{T}(init::Function)() -> T

Calling a `OncePerThread` object returns a value of type `T` by running the function
`initializer` exactly once per thread. All future calls in the same thread, and
concurrent or future calls with the same thread id, will return exactly the
same value. The object can also be indexed by the threadid for any existing
thread, to get (or initialize *on this thread*) the value stored for that
thread. Incorrect usage can lead to data-races or memory corruption so use only
if that behavior is correct within your library's threading-safety design.

!!! warning
    It is not necessarily true that a Task only runs on one thread, therefore the value
    returned here may alias other values or change in the middle of your program. This function
    may get deprecated in the future. If initializer yields, the thread running the current
    task after the call might not be the same as the one at the start of the call.

See also: [`OncePerTask`](@ref).

## Example

```jldoctest
julia> const thread_state = Base.OncePerThread{Vector{UInt32}}() do
           println("Making lazy thread value...done.")
           return [Libc.rand()]
       end;

julia> (threadvec = thread_state()) |> typeof
Making lazy thread value...done.
Vector{UInt32} (alias for Array{UInt32, 1})

julia> threadvec === fetch(@async thread_state())
true

julia> threadvec === thread_state[Threads.threadid()]
true
```
"""
mutable struct OncePerThread{T, F}
    @atomic xs::AtomicMemory{T} # values
    @atomic ss::AtomicMemory{UInt8} # states: 0=initial, 1=hasrun, 2=error, 3==concurrent
    const initializer::F

    function OncePerThread{T,F}(initializer::F) where {T, F}
        xs, ss = AtomicMemory{T}(), AtomicMemory{UInt8}()
        once = new{T,F}(xs, ss, initializer)
        ccall(:jl_set_precompile_field_replace, Cvoid, (Any, Any, Any),
            once, :xs, xs)
        ccall(:jl_set_precompile_field_replace, Cvoid, (Any, Any, Any),
            once, :ss, ss)
        return once
    end
end
OncePerThread{T}(initializer::F) where {T, F} = OncePerThread{T,F}(initializer)
OncePerThread(initializer) = OncePerThread{Base.promote_op(initializer), typeof(initializer)}(initializer)
@inline (once::OncePerThread)() = once[Threads.threadid()]
@inline function getindex(once::OncePerThread, tid::Integer)
    tid = Int(tid)
    ss = @atomic :acquire once.ss
    xs = @atomic :monotonic once.xs
    # n.b. length(xs) >= length(ss)
    if tid <= 0 || tid > length(ss) || (@atomic :acquire ss[tid]) != PerStateHasrun
        (@noinline function init_perthread(once, tid)
            local ss = @atomic :acquire once.ss
            local xs = @atomic :monotonic once.xs
            local len = length(ss)
            # slow path to allocate it
            nt = Threads.maxthreadid()
            0 < tid <= nt || throw(ArgumentError("thread id outside of allocated range"))
            if tid <= length(ss) && (@atomic :acquire ss[tid]) == PerStateErrored
                error("OncePerThread initializer failed previously")
            end
            newxs = xs
            newss = ss
            if tid > len
                # attempt to do all allocations outside of PerThreadLock for better scaling
                @assert length(xs) >= length(ss) "logical constraint violation"
                newxs = typeof(xs)(undef, len + nt)
                newss = typeof(ss)(undef, len + nt)
            end
            # uses state and locks to ensure this runs exactly once per tid argument
            lock(PerThreadLock)
            try
                ss = @atomic :monotonic once.ss
                xs = @atomic :monotonic once.xs
                if tid > length(ss)
                    @assert len <= length(ss) <= length(newss) "logical constraint violation"
                    fill_monotonic!(newss, PerStateInitial)
                    xs = copyto_monotonic!(newxs, xs)
                    ss = copyto_monotonic!(newss, ss)
                    @atomic :release once.xs = xs
                    @atomic :release once.ss = ss
                end
                state = @atomic :monotonic ss[tid]
                while state == PerStateConcurrent
                    # lost race, wait for notification this is done running elsewhere
                    wait(PerThreadLock) # wait for initializer to finish without releasing this thread
                    ss = @atomic :monotonic once.ss
                    state = @atomic :monotonic ss[tid]
                end
                if state == PerStateInitial
                    # won the race, drop lock in exchange for state, and run user initializer
                    @atomic :monotonic ss[tid] = PerStateConcurrent
                    result = try
                        unlock(PerThreadLock)
                        once.initializer()
                    catch
                        lock(PerThreadLock)
                        ss = @atomic :monotonic once.ss
                        @atomic :release ss[tid] = PerStateErrored
                        notify(PerThreadLock)
                        rethrow()
                    end
                    # store result and notify waiters
                    lock(PerThreadLock)
                    xs = @atomic :monotonic once.xs
                    @atomic :release xs[tid] = result
                    ss = @atomic :monotonic once.ss
                    @atomic :release ss[tid] = PerStateHasrun
                    notify(PerThreadLock)
                elseif state == PerStateErrored
                    error("OncePerThread initializer failed previously")
                elseif state != PerStateHasrun
                    error("invalid state for OncePerThread")
                end
            finally
                unlock(PerThreadLock)
            end
            nothing
        end)(once, tid)
        xs = @atomic :monotonic once.xs
    end
    return xs[tid]
end

"""
    OncePerTask{T}(init::Function)() -> T

Calling a `OncePerTask` object returns a value of type `T` by running the function `initializer`
exactly once per Task. All future calls in the same Task will return exactly the same value.

See also: [`task_local_storage`](@ref).

## Example

```jldoctest
julia> const task_state = Base.OncePerTask{Vector{UInt32}}() do
           println("Making lazy task value...done.")
           return [Libc.rand()]
       end;

julia> (taskvec = task_state()) |> typeof
Making lazy task value...done.
Vector{UInt32} (alias for Array{UInt32, 1})

julia> taskvec === task_state()
true

julia> taskvec === fetch(@async task_state())
Making lazy task value...done.
false
```
"""
mutable struct OncePerTask{T, F}
    const initializer::F

    OncePerTask{T}(initializer::F) where {T, F} = new{T,F}(initializer)
    OncePerTask{T,F}(initializer::F) where {T, F} = new{T,F}(initializer)
    OncePerTask(initializer) = new{Base.promote_op(initializer), typeof(initializer)}(initializer)
end
@inline (once::OncePerTask)() = get!(once.initializer, task_local_storage(), once)
