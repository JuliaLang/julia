# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractChannel end

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
mutable struct Channel{T} <: AbstractChannel
    cond_take::Condition                 # waiting for data to become available
    cond_put::Condition                  # waiting for a writeable slot
    state::Symbol
    excp::Union{Exception, Nothing}         # exception to be thrown when state != :open

    data::Vector{T}
    sz_max::Int                          # maximum size of channel

    # Used when sz_max == 0, i.e., an unbuffered channel.
    waiters::Int
    takers::Vector{Task}
    putters::Vector{Task}

    function Channel{T}(sz::Float64) where T
        if sz == Inf
            Channel{T}(typemax(Int))
        else
            Channel{T}(convert(Int, sz))
        end
    end
    function Channel{T}(sz::Integer) where T
        if sz < 0
            throw(ArgumentError("Channel size must be either 0, a positive integer or Inf"))
        end
        ch = new(Condition(), Condition(), :open, nothing, Vector{T}(), sz, 0)
        if sz == 0
            ch.takers = Vector{Task}()
            ch.putters = Vector{Task}()
        end
        return ch
    end
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

julia> chnl = Channel(c->(@show take!(c)); taskref=taskref);

julia> istaskdone(taskref[])
false

julia> put!(chnl, "Hello");
take!(c) = "Hello"

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
        c.excp !== nothing && throw(c.excp)
        throw(closed_exception())
    end
end
"""
    close(c::Channel)

Close a channel. An exception is thrown by:

* [`put!`](@ref) on a closed channel.
* [`take!`](@ref) and [`fetch`](@ref) on an empty, closed channel.
"""
function close(c::Channel)
    c.state = :closed
    c.excp = closed_exception()
    notify_error(c)
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

julia> task = @schedule foreach(i->put!(c, i), 1:4);

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

julia> task = @schedule (put!(c,1);error("foo"));

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
    ref = WeakRef(c)
    register_taskdone_hook(task, tsk->close_chnl_on_taskdone(tsk, ref))
    c
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

function close_chnl_on_taskdone(t::Task, ref::WeakRef)
    if ref.value !== nothing
        c = ref.value
        !isopen(c) && return
        if istaskfailed(t)
            c.state = :closed
            c.excp = task_result(t)
            notify_error(c)
        else
            close(c)
        end
    end
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
"""
function put!(c::Channel, v)
    check_channel_state(c)
    isbuffered(c) ? put_buffered(c,v) : put_unbuffered(c,v)
end

function put_buffered(c::Channel, v)
    while length(c.data) == c.sz_max
        wait(c.cond_put)
    end
    push!(c.data, v)

    # notify all, since some of the waiters may be on a "fetch" call.
    notify(c.cond_take, nothing, true, false)
    v
end

function put_unbuffered(c::Channel, v)
    if length(c.takers) == 0
        push!(c.putters, current_task())
        c.waiters > 0 && notify(c.cond_take, nothing, false, false)

        try
            wait()
        catch ex
            filter!(x->x!=current_task(), c.putters)
            rethrow(ex)
        end
    end
    taker = popfirst!(c.takers)
    yield(taker, v) # immediately give taker a chance to run, but don't block the current task
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
    wait(c)
    c.data[1]
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
    wait(c)
    v = popfirst!(c.data)
    notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
    v
end

popfirst!(c::Channel) = take!(c)

# 0-size channel
function take_unbuffered(c::Channel{T}) where T
    check_channel_state(c)
    push!(c.takers, current_task())
    try
        if length(c.putters) > 0
            let refputter = Ref(popfirst!(c.putters))
                return Base.try_yieldto(refputter) do putter
                    # if we fail to start putter, put it back in the queue
                    putter === current_task || pushfirst!(c.putters, putter)
                end::T
            end
        else
            return wait()::T
        end
    catch ex
        filter!(x->x!=current_task(), c.takers)
        rethrow(ex)
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
n_avail(c::Channel) = isbuffered(c) ? length(c.data) : length(c.putters)

wait(c::Channel) = isbuffered(c) ? wait_impl(c) : wait_unbuffered(c)
function wait_impl(c::Channel)
    while !isready(c)
        check_channel_state(c)
        wait(c.cond_take)
    end
    nothing
end

function wait_unbuffered(c::Channel)
    c.waiters += 1
    try
        wait_impl(c)
    finally
        c.waiters -= 1
    end
    nothing
end

function notify_error(c::Channel, err)
    notify_error(c.cond_take, err)
    notify_error(c.cond_put, err)

    # release tasks on a `wait()/yieldto()` call (on unbuffered channels)
    if !isbuffered(c)
        waiters = filter!(t->(t.state == :runnable), vcat(c.takers, c.putters))
        foreach(t->schedule(t, err; error=true), waiters)
    end
end
notify_error(c::Channel) = notify_error(c, c.excp)

eltype(::Type{Channel{T}}) where {T} = T

show(io::IO, c::Channel) = print(io, "$(typeof(c))(sz_max:$(c.sz_max),sz_curr:$(n_avail(c)))")

mutable struct ChannelIterState{T}
    hasval::Bool
    val::T
    ChannelIterState{T}(has::Bool) where {T} = new(has)
end

start(c::Channel{T}) where {T} = ChannelIterState{T}(false)
function done(c::Channel, state::ChannelIterState)
    try
        # we are waiting either for more data or channel to be closed
        state.hasval && return false
        state.val = take!(c)
        state.hasval = true
        return false
    catch e
        if isa(e, InvalidStateException) && e.state==:closed
            return true
        else
            rethrow(e)
        end
    end
end
next(c::Channel, state) = (v=state.val; state.hasval=false; (v, state))

IteratorSize(::Type{<:Channel}) = SizeUnknown()
