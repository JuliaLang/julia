# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractChannel

type Channel{T} <: AbstractChannel
    cond_take::Condition    # waiting for data to become available
    cond_put::Condition     # waiting for a writeable slot
    state::Symbol

    data::Array{T,1}
    szp1::Int               # current channel size plus one
    sz_max::Int             # maximum size of channel
    take_pos::Int           # read position
    put_pos::Int            # write position

    function Channel(sz)
        sz_max = sz == typemax(Int) ? typemax(Int) - 1 : sz
        szp1 = sz > 32 ? 33 : sz+1
        new(Condition(), Condition(), :open,
            Array(T, szp1), szp1, sz_max, 1, 1)
    end
end

const DEF_CHANNEL_SZ=32

Channel(sz::Int = DEF_CHANNEL_SZ) = Channel{Any}(sz)

closed_exception() = InvalidStateException("Channel is closed.", :closed)
function close(c::Channel)
    c.state = :closed
    notify_error(c::Channel, closed_exception())
    c
end
isopen(c::Channel) = (c.state == :open)

type InvalidStateException <: Exception
    msg::AbstractString
    state::Symbol
end

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
        end
    end

    c.data[c.put_pos] = v
    c.put_pos = (c.put_pos == c.szp1 ? 1 : c.put_pos + 1)
    notify(c.cond_take, nothing, true, false)  # notify all, since some of the waiters may be on a "fetch" call.
    v
end

push!(c::Channel, v) = put!(c, v)

function fetch(c::Channel)
    wait(c)
    c.data[c.take_pos]
end

function take!(c::Channel)
    !isopen(c) && !isready(c) && throw(closed_exception())
    wait(c)
    v = c.data[c.take_pos]
    c.take_pos = (c.take_pos == c.szp1 ? 1 : c.take_pos + 1)
    notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
    v
end

shift!(c::Channel) = take!(c)

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

eltype{T}(::Type{Channel{T}}) = T

function n_avail(c::Channel)
    if c.put_pos >= c.take_pos
        return c.put_pos - c.take_pos
    else
        return c.szp1 - c.take_pos + c.put_pos
    end
end

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
