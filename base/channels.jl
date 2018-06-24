abstract type AbstractChannel end


const VectorChannel{T} = Union{BufferedChannel{T}, UnbufferedChannel{T}} where T


"""
    BufferedChannel{T}(sz::Int)
Constructs a `AbstractChannel` with an internal buffer that can hold a maximum of `sz` objects
of type `T`.
[`put!`](@ref) calls on a full channel block until an object is removed with [`take!`](@ref).
Other constructors:
* `BufferedChannel(Inf)`: equivalent to `Channel{Any}(typemax(Int))`
* `BufferedChannel(sz)`: equivalent to `Channel{Any}(sz)`
"""
mutable struct BufferedChannel{T} <: AbstractChannel
    cond_take::Condition                 # waiting for data to become available
    cond_put::Condition                  # waiting for a writeable slot
    state::Symbol
    excp::Union{Exception, Nothing}         # exception to be thrown when state != :open

    data::Vector{T}
    sz_max::Int                          # maximum size of channel
end

function BufferedChannel{T}(sz::Float64) where T
	if sz == Inf
		Channel{T}(typemax(Int))
	else
		Channel{T}(convert(Int, sz))
	end
end

function BufferedChannel{T}(sz::Integer) where T
	if sz <= 0
		throw(ArgumentError("Channel size must be a positive integer or Inf. Use an UnbufferedChannel for no buffer (size zero)."))
	end
	BufferedChannel(Condition(), Condition(), :open, nothing, Vector{T}(), sz)
end


"""
    BufferedChannel{T}(sz::Int)
Constructs a `AbstractChannel` with an internal buffer that can hold a maximum of `sz` objects
of type `T`.
[`put!`](@ref) calls on a full channel block until an object is removed with [`take!`](@ref).
`Channel(0)` constructs an unbuffered channel. `put!` blocks until a matching `take!` is called.
And vice-versa.
Other constructors:
* `BufferedChannel(Inf)`: equivalent to `Channel{Any}(typemax(Int))`
* `BufferedChannel(sz)`: equivalent to `Channel{Any}(sz)`
"""
mutable struct UnbufferedChannel{T} <: AbstractChannel
    cond_take::Condition                 # waiting for data to become available
    cond_put::Condition                  # waiting for a writeable slot
    state::Symbol
    excp::Union{Exception, Nothing}         # exception to be thrown when state != :open

    waiters::Int
    takers::Vector{Task}
    putters::Vector{Task}
end

function UnbufferedChannel{T}() where T
	UnbufferedChannel(Condition(), Condition(), :open, nothing, Vector{T}(), 0, Vector{Task}(), Vector{Task}())
	if sz == 0
		ch.takers = Vector{Task}()
		ch.putters = Vector{Task}()
	end
	return ch
end

"""
Channel{T}(sz)

Constructs a [BufferedChannel](@ref) or [UnbufferedChannel](@ref) depending on if `sz` is zero or not.
"""
Channel{T}(sz) where T = sz == 0 ? UnbufferedChannel{T}() : BufferedChannel{T}(sz)
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

function check_channel_state(c::VectorChannel)
    if !isopen(c)
        c.excp !== nothing && throw(c.excp)
        throw(closed_exception())
    end
end
"""
    close(c::VectorChannel)
Close a channel. An exception is thrown by:
* [`put!`](@ref) on a closed channel.
* [`take!`](@ref) and [`fetch`](@ref) on an empty, closed channel.
"""
function close(c::VectorChannel)
    c.state = :closed
    c.excp = closed_exception()
    notify_error(c)
    nothing
end
isopen(c::VectorChannel) = (c.state == :open)

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
function bind(c::VectorChannel, task::Task)
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
    put!(c::BufferedChannel, v)
Append an item `v` to the channel `c`. Blocks if the channel is full.
"""
function put!(c::BufferedChannel, v)
	check_channel_state(c)
    while length(c.data) == c.sz_max
        wait(c.cond_put)
    end
    push!(c.data, v)

    # notify all, since some of the waiters may be on a "fetch" call.
    notify(c.cond_take, nothing, true, false)
    v
end

"""
    put!(c::UnbufferedChannel, v)
Append an item `v` to the channel `c`.
Blocks  until a [`take!`](@ref) is performed by a different task.
"""
function put!(c::UnbufferedChannel, v)
	check_channel_state(c)
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

push!(c::VectorChannel, v) = put!(c, v)

"""
    fetch(c::BufferedChannel)
Wait for and get the first available item from the channel. Does not
remove the item. `fetch` is unsupported on an unbuffered (0-size) channel.
"""
fetch(c::BufferedChannel)
    wait(c)
    first(c.data)
end


"""
    take!(c::BufferedChannel)
Remove and return a value from a [`Channel`](@ref). Blocks until data is available.
"""
function take!(c::BufferedChannel)
    wait(c)
    v = popfirst!(c.data)
    notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
    v
end

popfirst!(c::VectorChannel) = take!(c)

"""
    take!(c::UnbufferedChannel)
Remove and return a value from a [`Channel`](@ref).
Blocks until a [`put!`](@ref) is performed by a different
task.
"""
function take_unbuffered(c::UnbufferedChannel{T}) where T
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
    isready(c::VectorChannel)
Determine whether a [`Vector`](@ref) has a value stored to it. Returns
immediately, does not block.
For unbuffered channels returns `true` if there are tasks waiting
on a [`put!`](@ref).
"""
isready(c::VectorChannel) = n_avail(c) > 0
n_avail(c::BufferedChannel) = length(c.data)
n_avail(c::UnbufferedChannel) = length(c.putters)


function wait(c::BufferedChannel)
    while !isready(c)
        check_channel_state(c)
        wait(c.cond_take)
    end
    nothing
end

function wait(c::UnbufferedChannel)
    c.waiters += 1
    try
        wait_impl(c)
    finally
        c.waiters -= 1
    end
    nothing
end

function notify_error(c::BufferedChannel, err)
    notify_error(c.cond_take, err)
    notify_error(c.cond_put, err)

end

function notify_error(c::UnbufferedChannel, err)
    notify_error(c.cond_take, err)
    notify_error(c.cond_put, err)

    # release tasks on a `wait()/yieldto()` call (on unbuffered channels)
	waiters = filter!(t->(t.state == :runnable), vcat(c.takers, c.putters))
	foreach(t->schedule(t, err; error=true), waiters)
end

notify_error(c::VectorChannel) = notify_error(c, c.excp)

eltype(::Type{VectorChannel{T}}) where {T} = T

show(io::IO, c::BufferedChannel) = print(io, "$(typeof(c))(sz_max:$(c.sz_max),sz_curr:$(n_avail(c)))")
show(io::IO, c::UnbufferedChannel) = print(io, "$(typeof(c))(sz_curr:$(n_avail(c)))")

function iterate(c::VectorChannel, state=nothing)
    try
        return (take!(c), nothing)
    catch e
        if isa(e, InvalidStateException) && e.state==:closed
            return nothing
        else
            rethrow(e)
        end
    end
end

length(c::VectorChannel) = n_avail
IteratorSize(::Type{<:VectorChannel}) = HasLength()
