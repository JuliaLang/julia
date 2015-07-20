# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractChannel{T}

type Channel{T} <: AbstractChannel{T}
    cid::Int
    cond_take::Condition    # waiting for data to become available
    cond_put::Condition     # waiting for a writeable slot
    state::Symbol

    data::Array{T,1}
    szp1::Int               # current channel size plus one
    sz_max::Int             # maximum size of channel
    take_pos::Int           # read position
    put_pos::Int            # write position

    Channel(elt, szp1, sz_max) = new(get_next_channel_id(), Condition(), Condition(), :open,
                                     Array(T, szp1), szp1, sz_max, 1, 1)
end

let next_channel_id=1
    global get_next_channel_id
    function get_next_channel_id()
        cid = next_channel_id
        next_channel_id = next_channel_id+1
        return cid
    end
end

Channel() = Channel(Any)
Channel(T::Type) = Channel(T::Type, typemax(Int))
Channel(sz::Int) = Channel(Any, sz)
function Channel(T::Type, sz::Int)
    sz_max = sz == typemax(Int) ? typemax(Int) - 1 : sz
    csz = sz > 32 ? 32 : sz
    Channel{T}(T, csz+1, sz_max)
end

closed_exception() = InvalidStateException("Channel is closed.", :closed)
function close(c::Channel)
    c.state = :closed
    notify_error(c::Channel, closed_exception())
    c
end
isopen(c::Channel) = (c.state == :open)

type InvalidStateException <: Exception
    msg::AbstractString
    state
end
InvalidStateException() = InvalidStateException("")
InvalidStateException(msg) = InvalidStateException(msg, 0)

function put!(c::Channel, v)
    !isopen(c) && throw(closed_exception())
    d = c.take_pos - c.put_pos
    if (d == 1) || (d == -(c.szp1-1))
        # grow the channel if possible
        if (c.szp1 - 1) < c.sz_max
            if ((c.szp1-1) * 2) > c.sz_max
                c.szp1 = c.sz_max + 1
            else
                c.szp1 = ((c.szp1-1) * 2) + 1
            end
            newdata = Array(eltype(c), c.szp1)
            if c.put_pos > c.take_pos
                copy!(newdata, 1, c.data, c.take_pos, (c.put_pos - c.take_pos))
                c.put_pos = c.put_pos - c.take_pos + 1
            else
                len_first_part = length(c.data) - c.take_pos + 1
                copy!(newdata, 1, c.data, c.take_pos, len_first_part)
                copy!(newdata, len_first_part+1, c.data, 1, c.put_pos-1)
                c.put_pos = len_first_part + c.put_pos
            end
            c.take_pos = 1
            c.data = newdata
        else
            wait(c.cond_put)
            check_open(c)
        end
    end

    c.data[c.put_pos] = v
    c.put_pos = (c.put_pos == c.szp1 ? 1 : c.put_pos + 1)
    notify(c.cond_take, nothing, true, false)  # notify all, since some of the waiters may be on a "fetch" call.
    v
end

function fetch(c::Channel)
    wait(c)
    c.data[c.take_pos]
end

function take!(c::Channel)
    !isopen(c) && !isready(c) && throw(closed_exception())
    while !isready(c)
        wait(c.cond_take)
    end
    v = c.data[c.take_pos]
    c.take_pos = (c.take_pos == c.szp1 ? 1 : c.take_pos + 1)
    notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
    v
end

isready(c::Channel) = (c.take_pos == c.put_pos ? false : true)

function wait(c::Channel)
    while !isready(c)
        wait(c.cond_take)
    end
    nothing
end

function notify_error(c::Channel, err)
    notify_error(c.cond_take, err)
    notify_error(c.cond_put, err)
end

eltype{T}(c::Channel{T}) = T

function length(c::Channel)
    if c.put_pos >= c.take_pos
        return c.put_pos - c.take_pos
    else
        return c.szp1 - c.take_pos + c.put_pos
    end
end

size(c::Channel) = c.sz_max

show(io::IO, c::Channel) = print(io, "$(typeof(c))(id:$(c.cid),sz_max:$(size(c)),sz_curr:$(length(c)))")
