# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    AbstractChannel{T}

Representation of a channel passing objects of type `T`.
"""
abstract type AbstractChannel{T} end

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
    state::Symbol
    excp::Union{Exception, Nothing}      # exception to be thrown when state !== :open

    data::Vector{T}
    sz_max::Int                          # maximum size of channel

    function Channel{T}(sz::Integer = 0) where T
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
Channel(sz=0) = Channel{Any}(sz)

# special constructors
"""
    Channel{T=Any}(func::Function, size=0; taskref=nothing, spawn=false)

Create a new task from `func`, bind it to a new channel of type
`T` and size `size`, and schedule the task, all in a single call.

`func` must accept the bound channel as its only argument.

If you need a reference to the created task, pass a `Ref{Task}` object via
the keyword argument `taskref`.

If `spawn = true`, the Task created for `func` may be scheduled on another thread
in parallel, equivalent to creating a task via [`Threads.@spawn`](@ref).

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

```jldoctest
julia> chnl = Channel{Char}(1, spawn=true) do ch
           for c in "hello world"
               put!(ch, c)
           end
       end
Channel{Char}(sz_max:1,sz_curr:1)

julia> String(collect(chnl))
"hello world"
```
"""
function Channel{T}(func::Function, size=0; taskref=nothing, spawn=false) where T
    chnl = Channel{T}(size)
    task = Task(() -> func(chnl))
    task.sticky = !spawn
    bind(chnl, task)
    if spawn
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
function Channel(func::Function; ctype=nothing, csize=nothing, taskref=nothing, spawn=nothing)
    # The spawn= keyword argument was added in Julia v1.3, and cannot be used with the
    # deprecated keyword arguments `ctype=` or `csize=`.
    if (ctype !== nothing || csize !== nothing) && spawn !== nothing
        throw(ArgumentError("Cannot set `spawn=` in the deprecated constructor `Channel(f; ctype=Any, csize=0)`. Please use `Channel{T=Any}(f, size=0; taskref=nothing, spawn=false)` instead!"))
    end
    # Set the actual default values for the arguments.
    ctype === nothing && (ctype = Any)
    csize === nothing && (csize = 0)
    spawn === nothing && (spawn = false)
    return Channel{ctype}(func, csize; taskref=taskref, spawn=spawn)
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
isopen(c::Channel) = (c.state === :open)

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
ERROR: TaskFailedException:
foo
Stacktrace:
[...]
```
"""
function bind(c::Channel, task::Task)
    T = Task(() -> close_chnl_on_taskdone(task, c))
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
        if istaskfailed(t) && task_result(t) isa Exception
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
isempty(c::Channel) = isbuffered(c) ? isempty(c.data) : isempty(c.cond_put.waitq)

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
        if isa(e, InvalidStateException) && e.state === :closed
            return nothing
        else
            rethrow()
        end
    end
end

IteratorSize(::Type{<:Channel}) = SizeUnknown()
