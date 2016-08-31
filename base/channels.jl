# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractChannel

type Channel{T} <: AbstractChannel
    cond_take::Condition    # waiting for data to become available
    cond_put::Condition     # waiting for a writeable slot
    state::Symbol

    data::Array{T,1}
    sz_max::Int             # maximum size of channel

    function Channel(sz)
        new(Condition(), Condition(), :open, Array{T}(0), sz)
    end
end

Channel(sz::Int = typemax(Int)) = Channel{Any}(sz)

closed_exception() = InvalidStateException("Channel is closed.", :closed)

"""
    close(c::Channel)

Closes a channel. An exception is thrown by:

* `put!` on a closed channel.
* `take!` and `fetch` on an empty, closed channel.
"""
function close(c::Channel)
    c.state = :closed
    notify_error(c::Channel, closed_exception())
    nothing
end
isopen(c::Channel) = (c.state == :open)

type InvalidStateException <: Exception
    msg::AbstractString
    state::Symbol
end

"""
    put!(c::Channel, v)

Appends an item `v` to the channel `c`. Blocks if the channel is full.
"""
function put!(c::Channel, v)
    !isopen(c) && throw(closed_exception())
    while length(c.data) == c.sz_max
        wait(c.cond_put)
    end
    push!(c.data, v)
    notify(c.cond_take, nothing, true, false)  # notify all, since some of the waiters may be on a "fetch" call.
    v
end

push!(c::Channel, v) = put!(c, v)

function fetch(c::Channel)
    wait(c)
    c.data[1]
end

"""
    take!(c::Channel)

Removes and returns a value from a `Channel`. Blocks till data is available.
"""
function take!(c::Channel)
    wait(c)
    v = shift!(c.data)
    notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
    v
end

shift!(c::Channel) = take!(c)

"""
    isready(c::Channel)

Determine whether a `Channel` has a value stored to it.
`isready` on `Channel`s is non-blocking.
"""
isready(c::Channel) = n_avail(c) > 0

function wait(c::Channel)
    while !isready(c)
        !isopen(c) && throw(closed_exception())
        wait(c.cond_take)
    end
    nothing
end

function notify_error(c::Channel, err)
    notify_error(c.cond_take, err)
    notify_error(c.cond_put, err)
end

eltype{T}(::Type{Channel{T}}) = T

n_avail(c::Channel) = length(c.data)

show(io::IO, c::Channel) = print(io, "$(typeof(c))(sz_max:$(c.sz_max),sz_curr:$(n_avail(c)))")

start{T}(c::Channel{T}) = Ref{Nullable{T}}()
function done(c::Channel, state::Ref)
    try
        # we are waiting either for more data or channel to be closed
        state[] = take!(c)
        return false
    catch e
        if isa(e, InvalidStateException) && e.state==:closed
            return true
        else
            rethrow(e)
        end
    end
end
next{T}(c::Channel{T}, state) = (v=get(state[]); state[]=nothing; (v, state))

iteratorsize{C<:Channel}(::Type{C}) = SizeUnknown()
