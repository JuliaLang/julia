# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    AbstractChannel{T}

Representation of a channel passing objects of type `T`.
"""
abstract type AbstractChannel{T} end

"""
    Channel{T}(sz::Int)

Constructs a `Channel` with an internal buffer that can hold a maximum of `sz` objects
of type `T`.
[`put!`](@ref) calls on a full channel block until an object is removed with [`take!`](@ref).

`Channel(0)` constructs an unbuffered channel. `put!` blocks until a matching `take!` is called.
And vice-versa.

Other constructors:

* `Channel(Inf)`: equivalent to `Channel{Any}(typemax(Int))`
* `Channel(sz)`: equivalent to `Channel{Any}(sz)`
"""
mutable struct Channel{T} <: AbstractChannel{T}
    cond_take::Threads.Condition                 # waiting for data to become available
    cond_wait::Threads.Condition                 # waiting for data to become maybe available
    cond_put::Threads.Condition                  # waiting for a writeable slot
    state::Symbol
    excp::Union{Exception, Nothing}      # exception to be thrown when state != :open

    data::Vector{T}
    sz_max::Int                          # maximum size of channel

    function Channel{T}(sz::Integer) where T
        if sz < 0
            throw(ArgumentError("Channel size must be either 0, a positive integer or Inf"))
        end
        lock = ReentrantLock()
        cond_put, cond_take = Threads.Condition(lock), Threads.Condition(lock)
        cond_wait = (sz == 0 ? Threads.Condition(lock) : cond_take) # wait is distinct from take iff unbuffered
        return new(cond_take, cond_wait, cond_put, :open, nothing, Vector{T}(), sz)
    end
end

function Channel{T}(sz::Float64) where T
    sz = (sz == Inf ? typemax(Int) : convert(Int, sz))
    return Channel{T}(sz)
end
Channel(sz) = Channel{Any}(sz)

# special constructors
"""
    Channel(func::Function; ctype=Any, csize=0, taskref=nothing)

Create a new task from `func`, bind it to a new channel of type
`ctype` and size `csize`, and schedule the task, all in a single call.

`func` must accept the bound channel as its only argument.

If you need a reference to the created task, pass a `Ref{Task}` object via
keyword argument `taskref`.

Return a `Channel`.

# Examples
```jldoctest
julia> chnl = Channel(c->foreach(i->put!(c,i), 1:4));

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

julia> chnl = Channel(c -> println(take!(c)); taskref=taskref);

julia> istaskdone(taskref[])
false

julia> put!(chnl, "Hello");
Hello

julia> istaskdone(taskref[])
true
```
"""
function Channel(func::Function; ctype=Any, csize=0, taskref=nothing)
    chnl = Channel{ctype}(csize)
    task = Task(() -> func(chnl))
    bind(chnl, task)
    yield(task) # immediately start it

    isa(taskref, Ref{Task}) && (taskref[] = task)
    return chnl
end


closed_exception() = InvalidStateException("Channel is closed.", :closed)

isbuffered(c::Channel) = c.sz_max==0 ? false : true

function check_channel_state(c::Channel)
    if !isopen(c)
        excp = c.excp
        excp !== nothing && throw(excp)
        throw(closed_exception())
    end
end
"""
    close(c::Channel[, excp::Exception])

Close a channel. An exception (optionally given by `excp`), is thrown by:

* [`put!`](@ref) on a closed channel.
* [`take!`](@ref) and [`fetch`](@ref) on an empty, closed channel.
"""
function close(c::Channel, excp::Exception=closed_exception())
    lock(c)
    try
        c.state = :closed
        c.excp = excp
        notify_error(c.cond_take, excp)
        notify_error(c.cond_wait, excp)
        notify_error(c.cond_put, excp)
    finally
        unlock(c)
    end
    nothing
end
isopen(c::Channel) = (c.state == :open)

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

julia> task = @async (put!(c,1);error("foo"));

julia> bind(c,task);

julia> take!(c)
1

julia> put!(c,1);
ERROR: foo
Stacktrace:
[...]
```
"""
function bind(c::Channel, task::Task)
    # TODO: implement "schedulewait" and deprecate taskdone_hook
    #T = Task(() -> close_chnl_on_taskdone(task, c))
    #schedulewait(task, T)
    register_taskdone_hook(task, tsk -> close_chnl_on_taskdone(tsk, c))
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
    cleanup = () -> try
            isopen(c) || return
            if istaskfailed(t)
                excp = task_result(t)
                if excp isa Exception
                    close(c, excp)
                    return
                end
            end
            close(c)
            return
        finally
            unlock(c)
        end
    if trylock(c)
        # can't use `lock`, since attempts to task-switch to wait for it
        # will just silently fail and leave us with broken state
        cleanup()
    else
        # so schedule this to happen once we are finished destroying our task
        # (on a new Task)
        @async (lock(c); cleanup())
    end
    nothing
end

struct InvalidStateException <: Exception
    msg::AbstractString
    state::Symbol
end

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

function put_buffered(c::Channel, v)
    lock(c)
    try
        while length(c.data) == c.sz_max
            check_channel_state(c)
            wait(c.cond_put)
        end
        push!(c.data, v)
        # notify all, since some of the waiters may be on a "fetch" call.
        notify(c.cond_take, nothing, true, false)
    finally
        unlock(c)
    end
    return v
end

function put_unbuffered(c::Channel, v)
    lock(c)
    taker = try
        while isempty(c.cond_take.waitq)
            check_channel_state(c)
            notify(c.cond_wait)
            wait(c.cond_put)
        end
        # unfair scheduled version of: notify(c.cond_take, v, false, false); yield()
        popfirst!(c.cond_take.waitq)
    finally
        unlock(c)
    end
    schedule(taker, v)
    yield()  # immediately give taker a chance to run, but don't block the current task
    return v
end

push!(c::Channel, v) = put!(c, v)

"""
    fetch(c::Channel)

Wait for and get the first available item from the channel. Does not
remove the item. `fetch` is unsupported on an unbuffered (0-size) channel.
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
fetch_unbuffered(c::Channel) = throw(ErrorException("`fetch` is not supported on an unbuffered Channel."))


"""
    take!(c::Channel)

Remove and return a value from a [`Channel`](@ref). Blocks until data is available.

For unbuffered channels, blocks until a [`put!`](@ref) is performed by a different
task.
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
        notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
        return v
    finally
        unlock(c)
    end
end

popfirst!(c::Channel) = take!(c)

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
    isready(c::Channel)

Determine whether a [`Channel`](@ref) has a value stored to it. Returns
immediately, does not block.

For unbuffered channels returns `true` if there are tasks waiting
on a [`put!`](@ref).
"""
isready(c::Channel) = n_avail(c) > 0
n_avail(c::Channel) = isbuffered(c) ? length(c.data) : length(c.cond_put.waitq)

lock(c::Channel) = lock(c.cond_take)
unlock(c::Channel) = unlock(c.cond_take)
trylock(c::Channel) = trylock(c.cond_take)

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

show(io::IO, c::Channel) = print(io, "$(typeof(c))(sz_max:$(c.sz_max),sz_curr:$(n_avail(c)))")

function iterate(c::Channel, state=nothing)
    try
        return (take!(c), nothing)
    catch e
        if isa(e, InvalidStateException) && e.state == :closed
            return nothing
        else
            rethrow()
        end
    end
end

IteratorSize(::Type{<:Channel}) = SizeUnknown()
