# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    AbstractChannel{T}

Representation of a channel passing objects of type `T`.
"""
abstract type AbstractChannel{T} end

push!(c::AbstractChannel, v) = (put!(c, v); c)
popfirst!(c::AbstractChannel) = take!(c)

"""
    Channel{T=Any}(size::Int=0)

Constructs a `Channel` with an internal buffer that can hold a maximum of `size` objects
of type `T`.
[`put!`](@ref) calls on a full channel block until an object is removed with [`take!`](@ref).

`Channel(0)` constructs an unbuffered channel. `put!` blocks until a matching `take!` is called.
And vice-versa.

Other constructors:

* `Channel()`: default constructor, equivalent to `Channel{Any}(0)`
* `Channel(Inf)`: equivalent to `Channel{Any}(typemax(Int))`
* `Channel(sz)`: equivalent to `Channel{Any}(sz)`

!!! compat "Julia 1.3"
    The default constructor `Channel()` and default `size=0` were added in Julia 1.3.
"""
mutable struct Channel{T} <: AbstractChannel{T}
    cond_take::Threads.Condition                 # waiting for data to become available
    cond_wait::Threads.Condition                 # waiting for data to become maybe available
    cond_put::Threads.Condition                  # waiting for a writeable slot
    @atomic state::Symbol
    excp::Union{Exception, Nothing}      # exception to be thrown when state !== :open

    data::Vector{T}
    @atomic n_avail_items::Int           # Available items for taking, can be read without lock
    sz_max::Int                          # maximum size of channel

    function Channel{T}(sz::Integer = 0) where T
        if sz < 0
            throw(ArgumentError("Channel size must be either 0, a positive integer or Inf"))
        end
        lock = ReentrantLock()
        cond_put, cond_take = Threads.Condition(lock), Threads.Condition(lock)
        cond_wait = (sz == 0 ? Threads.Condition(lock) : cond_take) # wait is distinct from take iff unbuffered
        return new(cond_take, cond_wait, cond_put, :open, nothing, Vector{T}(), 0, sz)
    end
end

function Channel{T}(sz::Float64) where T
    sz = (sz == Inf ? typemax(Int) : convert(Int, sz))
    return Channel{T}(sz)
end
Channel(sz=0) = Channel{Any}(sz)

# special constructors
"""
    Channel{T=Any}(func::Function, size=0; taskref=nothing, spawn=false, threadpool=nothing)

Create a new task from `func`, bind it to a new channel of type
`T` and size `size`, and schedule the task, all in a single call.
The channel is automatically closed when the task terminates.

`func` must accept the bound channel as its only argument.

If you need a reference to the created task, pass a `Ref{Task}` object via
the keyword argument `taskref`.

If `spawn=true`, the `Task` created for `func` may be scheduled on another thread
in parallel, equivalent to creating a task via [`Threads.@spawn`](@ref).

If `spawn=true` and the `threadpool` argument is not set, it defaults to `:default`.

If the `threadpool` argument is set (to `:default` or `:interactive`), this implies
that `spawn=true` and the new Task is spawned to the specified threadpool.

Return a `Channel`.

# Examples
```jldoctest
julia> chnl = Channel() do ch
           foreach(i -> put!(ch, i), 1:4)
       end;

julia> typeof(chnl)
Channel{Any}

julia> for i in chnl
           @show i
       end;
i = 1
i = 2
i = 3
i = 4
```

Referencing the created task:

```jldoctest
julia> taskref = Ref{Task}();

julia> chnl = Channel(taskref=taskref) do ch
           println(take!(ch))
       end;

julia> istaskdone(taskref[])
false

julia> put!(chnl, "Hello");
Hello

julia> istaskdone(taskref[])
true
```

!!! compat "Julia 1.3"
    The `spawn=` parameter was added in Julia 1.3. This constructor was added in Julia 1.3.
    In earlier versions of Julia, Channel used keyword arguments to set `size` and `T`, but
    those constructors are deprecated.

!!! compat "Julia 1.9"
    The `threadpool=` argument was added in Julia 1.9.

```jldoctest
julia> chnl = Channel{Char}(1, spawn=true) do ch
           for c in "hello world"
               put!(ch, c)
           end
       end
Channel{Char}(1) (2 items available)

julia> String(collect(chnl))
"hello world"
```
"""
function Channel{T}(func::Function, size=0; taskref=nothing, spawn=false, threadpool=nothing) where T
    chnl = Channel{T}(size)
    task = Task(() -> func(chnl))
    if threadpool === nothing
        threadpool = :default
    else
        spawn = true
    end
    task.sticky = !spawn
    bind(chnl, task)
    if spawn
        Threads._spawn_set_thrpool(task, threadpool)
        schedule(task) # start it on (potentially) another thread
    else
        yield(task) # immediately start it, yielding the current thread
    end
    isa(taskref, Ref{Task}) && (taskref[] = task)
    return chnl
end
Channel(func::Function, args...; kwargs...) = Channel{Any}(func, args...; kwargs...)

# This constructor is deprecated as of Julia v1.3, and should not be used.
# (Note that this constructor also matches `Channel(::Function)` w/out any kwargs, which is
# of course not deprecated.)
# We use `nothing` default values to check which arguments were set in order to throw the
# deprecation warning if users try to use `spawn=` with `ctype=` or `csize=`.
function Channel(func::Function; ctype=nothing, csize=nothing, taskref=nothing, spawn=nothing, threadpool=nothing)
    # The spawn= keyword argument was added in Julia v1.3, and cannot be used with the
    # deprecated keyword arguments `ctype=` or `csize=`.
    if (ctype !== nothing || csize !== nothing) && (spawn !== nothing || threadpool !== nothing)
        throw(ArgumentError("Cannot set `spawn=` or `threadpool=` in the deprecated constructor `Channel(f; ctype=Any, csize=0)`. Please use `Channel{T=Any}(f, size=0; taskref=nothing, spawn=false, threadpool=nothing)` instead!"))
    end
    # Set the actual default values for the arguments.
    ctype === nothing && (ctype = Any)
    csize === nothing && (csize = 0)
    spawn === nothing && (spawn = false)
    return Channel{ctype}(func, csize; taskref=taskref, spawn=spawn, threadpool=threadpool)
end

closed_exception() = InvalidStateException("Channel is closed.", :closed)

isbuffered(c::Channel) = c.sz_max==0 ? false : true

function check_channel_state(c::Channel)
    if !isopen(c)
        # if the monotonic load succeed, now do an acquire fence
        (@atomic :acquire c.state) === :open && concurrency_violation()
        excp = c.excp
        excp !== nothing && throw(excp)
        throw(closed_exception())
    end
end

# This function will run `f()` and return `true` if it throws an `InvalidStateException` because
# a channel is closed, and `false` otherwise.
function _do_is_closed(f)
    try
        f()
        return false
    catch e
        if isa(e, InvalidStateException) && e.state === :closed
            return true
        else
            rethrow()
        end
    end
end

"""
    close(c::Channel[, excp::Exception])

Close a channel. An exception (optionally given by `excp`), is thrown by:

* [`put!`](@ref) on a closed channel.
* [`take!`](@ref) and [`fetch`](@ref) on an empty, closed channel.
"""
close(c::Channel) = close(c, closed_exception()) # nospecialize on default arg seems to confuse makedocs
function close(c::Channel, @nospecialize(excp::Exception))
    lock(c)
    try
        c.excp = excp
        @atomic :release c.state = :closed
        notify_error(c.cond_take, excp)
        notify_error(c.cond_wait, excp)
        notify_error(c.cond_put, excp)
    finally
        unlock(c)
    end
    nothing
end

"""
    isopen(c::Channel)
Determines whether a [`Channel`](@ref) is open for new [`put!`](@ref) operations.
Notice that a `Channel`` can be closed and still have
buffered elements which can be consumed with [`take!`](@ref).

# Examples

Buffered channel with task:
```jldoctest
julia> c = Channel(ch -> put!(ch, 1), 1);

julia> isopen(c) # The channel is closed to new `put!`s
false

julia> isready(c) # The channel is closed but still contains elements
true

julia> take!(c)
1

julia> isready(c)
false
```

Unbuffered channel:
```jldoctest
julia> c = Channel{Int}();

julia> isopen(c)
true

julia> close(c)

julia> isopen(c)
false
```
"""
function isopen(c::Channel)
    # Use acquire here to pair with release store in `close`, so that subsequent `isready` calls
    # are forced to see `isready == true` if they see `isopen == false`. This means users must
    # call `isopen` before `isready` if you are using the race-y APIs (or call `iterate`, which
    # does this right for you).
    return ((@atomic :acquire c.state) === :open)
end

"""
    empty!(c::Channel)

Empty a Channel `c` by calling `empty!` on the internal buffer.
Return the empty channel.
"""
function Base.empty!(c::Channel)
    @lock c begin
        ndrop = length(c.data)
        empty!(c.data)
        _increment_n_avail(c, -ndrop)
        notify(c.cond_put)
    end
    return c
end

"""
    bind(chnl::Channel, task::Task)

Associate the lifetime of `chnl` with a task.
`Channel` `chnl` is automatically closed when the task terminates.
Any uncaught exception in the task is propagated to all waiters on `chnl`.

The `chnl` object can be explicitly closed independent of task termination.
Terminating tasks have no effect on already closed `Channel` objects.

When a channel is bound to multiple tasks, the first task to terminate will
close the channel. When multiple channels are bound to the same task,
termination of the task will close all of the bound channels.

# Examples
```jldoctest
julia> c = Channel(0);

julia> task = @async foreach(i->put!(c, i), 1:4);

julia> bind(c,task);

julia> for i in c
           @show i
       end;
i = 1
i = 2
i = 3
i = 4

julia> isopen(c)
false
```

```jldoctest
julia> c = Channel(0);

julia> task = @async (put!(c, 1); error("foo"));

julia> bind(c, task);

julia> take!(c)
1

julia> put!(c, 1);
ERROR: TaskFailedException
Stacktrace:
[...]
    nested task error: foo
[...]
```
"""
function bind(c::Channel, task::Task)
    T = Task(() -> close_chnl_on_taskdone(task, c))
    T.sticky = false
    _wait2(task, T)
    return c
end

"""
    channeled_tasks(n::Int, funcs...; ctypes=fill(Any,n), csizes=fill(0,n))

A convenience method to create `n` channels and bind them to tasks started
from the provided functions in a single call. Each `func` must accept `n` arguments
which are the created channels. Channel types and sizes may be specified via
keyword arguments `ctypes` and `csizes` respectively. If unspecified, all channels are
of type `Channel{Any}(0)`.

Returns a tuple, `(Array{Channel}, Array{Task})`, of the created channels and tasks.
"""
function channeled_tasks(n::Int, funcs...; ctypes=fill(Any,n), csizes=fill(0,n))
    @assert length(csizes) == n
    @assert length(ctypes) == n

    chnls = map(i -> Channel{ctypes[i]}(csizes[i]), 1:n)
    tasks = Task[ Task(() -> f(chnls...)) for f in funcs ]

    # bind all tasks to all channels and schedule them
    foreach(t -> foreach(c -> bind(c, t), chnls), tasks)
    foreach(schedule, tasks)
    yield() # Allow scheduled tasks to run

    return (chnls, tasks)
end

function close_chnl_on_taskdone(t::Task, c::Channel)
    isopen(c) || return
    lock(c)
    try
        isopen(c) || return
        if istaskfailed(t)
            close(c, TaskFailedException(t))
            return
        end
        close(c)
    finally
        unlock(c)
    end
    nothing
end

struct InvalidStateException <: Exception
    msg::String
    state::Symbol
end
showerror(io::IO, ex::InvalidStateException) = print(io, "InvalidStateException: ", ex.msg)

"""
    put!(c::Channel, v)

Append an item `v` to the channel `c`. Blocks if the channel is full.

For unbuffered channels, blocks until a [`take!`](@ref) is performed by a different
task.

!!! compat "Julia 1.1"
    `v` now gets converted to the channel's type with [`convert`](@ref) as `put!` is called.
"""
function put!(c::Channel{T}, v) where T
    check_channel_state(c)
    v = convert(T, v)
    return isbuffered(c) ? put_buffered(c, v) : put_unbuffered(c, v)
end

# Atomically update channel n_avail, *assuming* we hold the channel lock.
function _increment_n_avail(c, inc)
    # We hold the channel lock so it's safe to non-atomically read and
    # increment c.n_avail_items
    newlen = c.n_avail_items + inc
    # Atomically store c.n_avail_items to prevent data races with other threads
    # reading this outside the lock.
    @atomic :monotonic c.n_avail_items = newlen
end

function put_buffered(c::Channel, v)
    lock(c)
    did_buffer = false
    try
        # Increment channel n_avail eagerly (before push!) to count data in the
        # buffer as well as offers from tasks which are blocked in wait().
        _increment_n_avail(c, 1)
        while length(c.data) == c.sz_max
            check_channel_state(c)
            wait(c.cond_put)
        end
        check_channel_state(c)
        push!(c.data, v)
        did_buffer = true
        # notify all, since some of the waiters may be on a "fetch" call.
        notify(c.cond_take, nothing, true, false)
    finally
        # Decrement the available items if this task had an exception before pushing the
        # item to the buffer (e.g., during `wait(c.cond_put)`):
        did_buffer || _increment_n_avail(c, -1)
        unlock(c)
    end
    return v
end

function put_unbuffered(c::Channel, v)
    lock(c)
    taker = try
        _increment_n_avail(c, 1)
        while isempty(c.cond_take.waitq)
            check_channel_state(c)
            notify(c.cond_wait)
            wait(c.cond_put)
        end
        check_channel_state(c)
        # unfair scheduled version of: notify(c.cond_take, v, false, false); yield()
        popfirst!(c.cond_take.waitq)
    finally
        _increment_n_avail(c, -1)
        unlock(c)
    end
    schedule(taker, v)
    yield()  # immediately give taker a chance to run, but don't block the current task
    return v
end

"""
    append!(c::Channel, iter)

Append all items in `iter` to the channel `c`. If the channel is buffered, this operation requires
fewer `lock` operations than individual `put!`s. If the length of the iterator
is greater than the available buffer space, the operation will block until enough elements
are taken from the channel to make space for the new elements.

# Examples

```jldoctest
julia> c = Channel(3);

julia> append!(c, 1:3);

julia> take!(c)
1

julia> take!(c)
2

julia> take!(c)
3
```

!!! compat "Julia 1.12"
    Requires at least Julia 1.12.
"""
function append!(c::Channel, iter::T) where {T}
    if IteratorSize(iter) isa Union{HasLength, HasShape}
        len = length(iter)
        # short circuit for small iters
        if len == 0
            return c
        elseif len == 1
            put!(c, @inbounds vec[begin])
            return c
        end
    end

    return isbuffered(c) ? append_buffered(c, iter) : append_unbuffered(c, iter)
end
function append!(c_dst::Channel, c_src::Channel{T}) where {T}
    # if either channel has a buffer size of 0 or 1, fallback to a naive `put!` loop
    if min(c_dst.sz_max, c_src.sz_max) < 2
        return isbuffered(c_dst) ? append_buffered(c_dst, c_src) : append_unbuffered(c_dst, c_src)
    end
    i = 0
    lock(c_src)
    try
        while isopen(c_src) || isready(c_src)
            # wait for data to become available
            while isempty(c_src.data)
                _do_is_closed() do
                    check_channel_state(c_src)
                    wait(c_src.cond_take)
                end
            end

            lock(c_dst)
            try
                while length(c_dst.data) == c_dst.sz_max
                    check_channel_state(c_dst)
                    wait(c_dst.cond_put)
                end

                # append as many as possible to c1
                n = min(c_dst.sz_max - length(c_dst.data), length(c_src.data))
                _increment_n_avail(c_dst, n)
                c1_len_og = length(c_dst.data)

                resize!(c_dst.data, c1_len_og + n)
                copyto!(c_dst.data, c1_len_og + 1, c_src.data, 1, n)
                deleteat!(c_src.data, 1:n)

                _increment_n_avail(c_src, -n)
                notify(c_dst.cond_take, nothing, true, false)
                foreach(_ -> notify(c_src.cond_put, nothing, true, false), 1:n)
            finally
                unlock(c_dst)
            end
        end
    finally
        unlock(c_src)
    end
    return c_dst
end

function append_unbuffered(c::Channel, iter)
    for v in iter
        put!(c, v)
    end
    return c
end

function append_buffered(c::Channel{T}, iter::I) where {T,I}
    converted_items = Iterators.Stateful(Iterators.map(x -> convert(T, x), iter))

    has_length = IteratorSize(iter) isa HasLength
    elements_to_add = if has_length
        # when we know the length, we can eagerly increment for all items
        l = length(iter)
        _increment_n_avail(c, l)
        l
    else
        0
    end

    while !isempty(converted_items)
        lock(c)
        try
            while length(c.data) == c.sz_max
                check_channel_state(c)
                wait(c.cond_put)
            end

            # Grab a chunk of items that will fit in the channel's buffer
            available_space = c.sz_max - length(c.data)
            chunk = Iterators.take(converted_items, available_space)

            # for iterators without length, we increment the available items one item at a time
            if !has_length
                chunk = Iterators.map(chunk) do x
                    elements_to_add += 1
                    _increment_n_avail(c, 1)
                    x
                end
            end

            check_channel_state(c)
            append!(c.data, chunk)

            # We successfully added chunk, so we can remove the elements from the count
            elements_to_add -= available_space

            # notify all, since some of the waiters may be on a "fetch" call.
            notify(c.cond_take, nothing, true, false)
        catch
            # Decrement the available items if this task had an exception before pushing the
            # item to the buffer (e.g., during `wait(c.cond_put)`):
            elements_to_add > 0 && _increment_n_avail(c, -elements_to_add)
            rethrow()
        finally
            unlock(c)
        end
    end
    return c
end

"""
    fetch(c::Channel)

Waits for and returns (without removing) the first available item from the `Channel`.
Note: `fetch` is unsupported on an unbuffered (0-size) `Channel`.

# Examples

Buffered channel:
```jldoctest
julia> c = Channel(3) do ch
           foreach(i -> put!(ch, i), 1:3)
       end;

julia> fetch(c)
1

julia> collect(c)  # item is not removed
3-element Vector{Any}:
 1
 2
 3
```
"""
fetch(c::Channel) = isbuffered(c) ? fetch_buffered(c) : fetch_unbuffered(c)
function fetch_buffered(c::Channel)
    lock(c)
    try
        while isempty(c.data)
            check_channel_state(c)
            wait(c.cond_take)
        end
        return c.data[1]
    finally
        unlock(c)
    end
end
fetch_unbuffered(::Channel) = throw(ErrorException("`fetch` is not supported on an unbuffered Channel."))


"""
    take!(c::Channel)

Removes and returns a value from a [`Channel`](@ref) in order. Blocks until data is available.
For unbuffered channels, blocks until a [`put!`](@ref) is performed by a different task.

# Examples

Buffered channel:
```jldoctest
julia> c = Channel(1);

julia> put!(c, 1);

julia> take!(c)
1
```

Unbuffered channel:
```jldoctest
julia> c = Channel(0);

julia> task = Task(() -> put!(c, 1));

julia> schedule(task);

julia> take!(c)
1
```
"""
take!(c::Channel) = isbuffered(c) ? take_buffered(c) : take_unbuffered(c)
function take_buffered(c::Channel)
    lock(c)
    try
        while isempty(c.data)
            check_channel_state(c)
            wait(c.cond_take)
        end
        v = popfirst!(c.data)
        _increment_n_avail(c, -1)
        notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
        return v
    finally
        unlock(c)
    end
end

# 0-size channel
function take_unbuffered(c::Channel{T}) where T
    lock(c)
    try
        check_channel_state(c)
        notify(c.cond_put, nothing, false, false)
        return wait(c.cond_take)::T
    finally
        unlock(c)
    end
end

"""
    take!(c::Channel, n::Integer[, buffer::AbstractVector])

Take at most `n` items from the [`Channel`](@ref) `c` and return them in a `Vector`.
If `buffer` is provided, it will be used to store the result. If the channel closes before
`n` items are taken, the result will contain only the items that were available. For
buffered channels, this operation requires fewer `lock` operations than individual `take!(c)`s.

# Examples

```jldoctest
julia> c = Channel(3);

julia> append!(c, 1:3);

julia> take!(c, 3, zeros(Int, 3))
3-element Vector{Int64}:
 1
 2
 3
```

!!! compat "Julia 1.12"
    Requires at least Julia 1.12.
"""
function take!(c::Channel{T}, n::Integer, buffer::AbstractVector = Vector{T}()) where {T}
    # for small-ish n, we can avoid resizing the buffer as we push elements
    if n < 2^16
        resize!(buffer, n)
    end

    buffered = isbuffered(c)
    # short circuit for small n
    if n == 0
        return buffer
    elseif n == 1
        is_closed = _do_is_closed() do
            buffer[begin] = take!(c)
        end
        # if we caught a :closed error when trying to get the element, we need to clear the
        # buffer before returning
        is_closed && empty!(buffer)
        return buffer
    end
    return buffered ? take_buffered(c, n, buffer) : take_unbuffered(c, n, buffer)
end

function _resize_buffer(buffer, n, resize_count, n_to_take=1)
    # As we take elements from the channel, we may need to resize the buffer to accommodate them
    # for small n, we resize once at the beginning. But for larger n, we resize the buffer as needed.
    # We resize exponentially larger each time until we reach 2^16, at which point we resize by 2^16.
    len = length(buffer)
    needed_space = max(
        n_to_take,
        2^resize_count
    )
    resize!(buffer, min(len + needed_space, n))
    resize_count >= 16 && return resize_count
    return resize_count + 1
end

function _rm_unused_slots(buffer, last_idx)
    if last_idx <= lastindex(buffer)
        deleteat!(buffer, last_idx+1:lastindex(buffer))
    end
end

function take_buffered(c::Channel{T}, n, buffer::AbstractVector) where {T}
    elements_taken = 0 # number of elements taken so far
    idx1 = firstindex(buffer)
    target_buffer_len = min(n, c.sz_max)
    resize_count = 1

    lock(c)
    try
        while elements_taken < n && (isopen(c) || isready(c))
            # wait until the channel has at least min_n elements or is full
            while length(c.data) < target_buffer_len && isopen(c)
                _do_is_closed(() -> wait(c.cond_take)) && break
            end
            # take as many elements as possible from the buffer
            n_to_take = min(n - elements_taken, length(c.data))
            idx_start = idx1 + elements_taken
            idx_end = idx_start + n_to_take - 1
            if idx_end > lastindex(buffer)
                resize_count = _resize_buffer(buffer, n, resize_count, n_to_take)
            end
            # since idx_start/end are both created relative to `firstindex(buffer)`, they are safe to use
            # as indices for the buffer
            @views copy!(buffer[idx_start:idx_end], c.data[1:n_to_take])
            deleteat!(c.data, 1:n_to_take)
            _increment_n_avail(c, -n_to_take)
            foreach(_ -> notify(c.cond_put, nothing, true, false), 1:n_to_take)
            elements_taken += n_to_take
        end

        _rm_unused_slots(buffer, firstindex(buffer)+elements_taken-1)
        return buffer
    finally
        unlock(c)
    end
end

function take_unbuffered(c::Channel, n, buffer::AbstractVector)
    idx1 = firstindex(buffer)
    last_idx = idx1 - 1
    resize_count = 1
    lock(c)
    try
        for i in idx1:(idx1+n-1)
            if i > lastindex(buffer)
                resize_count = _resize_buffer(buffer, n, resize_count)
            end
            _do_is_closed() do
                buffer[i] = take_unbuffered(c)
            end && break
            last_idx = i
        end

        _rm_unused_slots(buffer, last_idx)
        return buffer
    finally
        unlock(c)
    end
end

collect(c::Channel) = take!(c, typemax(UInt))

"""
    isready(c::Channel)

Determines whether a [`Channel`](@ref) has a value stored in it.
Returns immediately, does not block.

For unbuffered channels, return `true` if there are tasks waiting on a [`put!`](@ref).

# Examples

Buffered channel:
```jldoctest
julia> c = Channel(1);

julia> isready(c)
false

julia> put!(c, 1);

julia> isready(c)
true
```

Unbuffered channel:
```jldoctest
julia> c = Channel();

julia> isready(c)  # no tasks waiting to put!
false

julia> task = Task(() -> put!(c, 1));

julia> schedule(task);  # schedule a put! task

julia> isready(c)
true
```

"""
isready(c::Channel) = n_avail(c) > 0
isempty(c::Channel) = n_avail(c) == 0
function n_avail(c::Channel)
    # Lock-free equivalent to `length(c.data) + length(c.cond_put.waitq)`
    @atomic :monotonic c.n_avail_items
end

"""
    isfull(c::Channel)

Determines if a [`Channel`](@ref) is full, in the sense
that calling `put!(c, some_value)` would have blocked.
Returns immediately, does not block.

Note that it may frequently be the case that `put!` will
not block after this returns `true`. Users must take
precautions not to accidentally create live-lock bugs
in their code by calling this method, as these are
generally harder to debug than deadlocks. It is also
possible that `put!` will block after this call
returns `false`, if there are multiple producer
tasks calling `put!` in parallel.

# Examples

Buffered channel:
```jldoctest
julia> c = Channel(1); # capacity = 1

julia> isfull(c)
false

julia> put!(c, 1);

julia> isfull(c)
true
```

Unbuffered channel:
```jldoctest
julia> c = Channel(); # capacity = 0

julia> isfull(c) # unbuffered channel is always full
true
```
"""
isfull(c::Channel) = n_avail(c) ≥ c.sz_max

lock(c::Channel) = lock(c.cond_take)
lock(f, c::Channel) = lock(f, c.cond_take)
unlock(c::Channel) = unlock(c.cond_take)
trylock(c::Channel) = trylock(c.cond_take)

"""
    wait(c::Channel)

Blocks until the `Channel` [`isready`](@ref).

```jldoctest
julia> c = Channel(1);

julia> isready(c)
false

julia> task = Task(() -> wait(c));

julia> schedule(task);

julia> istaskdone(task)  # task is blocked because channel is not ready
false

julia> put!(c, 1);

julia> istaskdone(task)  # task is now unblocked
true
```
"""
function wait(c::Channel)
    isready(c) && return
    lock(c)
    try
        while !isready(c)
            check_channel_state(c)
            wait(c.cond_wait)
        end
    finally
        unlock(c)
    end
    nothing
end

eltype(::Type{Channel{T}}) where {T} = T

show(io::IO, c::Channel) = print(io, typeof(c), "(", c.sz_max, ")")

function show(io::IO, ::MIME"text/plain", c::Channel)
    show(io, c)
    if !(get(io, :compact, false)::Bool)
        if !isopen(c)
            print(io, " (closed)")
        else
            n = n_avail(c)
            if n == 0
                print(io, " (empty)")
            else
                s = n == 1 ? "" : "s"
                print(io, " (", n, " item$s available)")
            end
        end
    end
end

function iterate(c::Channel, state=nothing)
    if isopen(c) || isready(c)
        try
            return (take!(c), nothing)
        catch e
            if isa(e, InvalidStateException) && e.state === :closed
                return nothing
            else
                rethrow()
            end
        end
    else
        return nothing
    end
end

IteratorSize(::Type{<:Channel}) = SizeUnknown()
