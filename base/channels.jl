# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract ChannelData{T}

type ChannelDataSingle{T} <: ChannelData{T}
    full::Bool          # Need a separate flag here to handle the case where "nothing" is a valid value in "data"
    data::Nullable{T}
end

type ChannelDataMultiple{T} <: ChannelData{T}
    data::Array{T,1}
    szp1::Int               # current channel size plus one
    sz_max::Int             # maximum size of channel
    take_pos::Int           # read position
    put_pos::Int            # write position
end

abstract AbstractChannel{T}

@enum ChannelState C_OPEN C_CLOSED

type Channel{T, Store} <: AbstractChannel{T}
    cid::Int
    store::ChannelData{T}
    cond_take::Condition    # waiting for data to become available
    cond_put::Condition     # waiting for a writeable slot
    state::ChannelState

    Channel(store) = new(get_next_channel_id(), store, Condition(), Condition(), C_OPEN)
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
Channel(T::Type) = Channel{T, ChannelDataSingle{T}}(ChannelDataSingle{T}(false, Nullable{T}()))

Channel(sz::Int) = Channel(Any, sz)
function Channel(T::Type, sz::Int)
    sz_max = sz == typemax(Int) ? typemax(Int) - 1 : sz
    csz = sz > 32 ? 32 : sz
    Channel{T, ChannelDataMultiple{T}}(ChannelDataMultiple{T}(Array(T, csz+1), csz+1, sz_max, 1, 1))
end

function close(c::Channel)
    c.state = C_CLOSED
    notify(c.cond_take, nothing, true, false)
    c
end
isopen(c::Channel) = (c.state == C_OPEN)

type ChannelClosedException <: Exception end

start(c::Channel) = nothing
function done(c::Channel, state)
    while isopen(c)
        try
            # we are waiting either for more data or channel to be closed
            wait(c)
            isready(c) && return false
        catch e
            if isa(e, ChannelClosedException)
                return true
            else
                rethrow(e)
            end
        end
    end
    return true
end
next(c::Channel, state) = (take!(c), nothing)

function put!{T}(c::Channel{T, ChannelDataMultiple{T}}, v::T)
    !isopen(c) && throw(ChannelClosedException())
    store::ChannelDataMultiple{T} = c.store
    d = store.take_pos - store.put_pos
    if (d == 1) || (d == -(store.szp1-1))
        # grow the channel if possible
        if (store.szp1 - 1) < store.sz_max
            if ((store.szp1-1) * 2) > store.sz_max
                store.szp1 = store.sz_max + 1
            else
                store.szp1 = ((store.szp1-1) * 2) + 1
            end
            newdata = Array(eltype(c), store.szp1)
            if store.put_pos > store.take_pos
                copy!(newdata, 1, store.data, store.take_pos, (store.put_pos - store.take_pos))
                store.put_pos = store.put_pos - store.take_pos + 1
            else
                len_first_part = length(store.data) - store.take_pos + 1
                copy!(newdata, 1, store.data, store.take_pos, len_first_part)
                copy!(newdata, len_first_part+1, store.data, 1, store.put_pos-1)
                store.put_pos = len_first_part + store.put_pos
            end
            store.take_pos = 1
            store.data = newdata
        else
            wait(c.cond_put)
        end
    end

    store.data[store.put_pos] = v
    store.put_pos = (store.put_pos == store.szp1 ? 1 : store.put_pos + 1)
    notify(c.cond_take, nothing, true, false)  # notify all, since some of the waiters may be on a "fetch" call.
    v
end

function put!{T}(c::Channel{T, ChannelDataSingle{T}}, v::T)
    !isopen(c) && throw(ChannelClosedException())
    store::ChannelDataSingle{T} = c.store
    if store.full
        wait(c.cond_put)
    end
    store.data = v
    store.full = true
    notify(c.cond_take, nothing, true, false)
    v
end

function fetch(c::Channel)
    wait(c)
    fetch(c.store)
end

fetch(store::ChannelDataSingle) = get(store.data)
fetch(store::ChannelDataMultiple) = store.data[store.take_pos]

function take!{T, S}(c::Channel{T, S})
    !isopen(c) && throw(ChannelClosedException())
    while !isready(c)
        wait(c.cond_take)
    end
    store::S = c.store
    v = take!(store)
    notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
    v
end

take!{T}(store::ChannelDataSingle{T}) = (v=get(store.data); store.data=Nullable{T}(); store.full=false; v)
function take!(store::ChannelDataMultiple)
    v = store.data[store.take_pos]
    store.take_pos = (store.take_pos == store.szp1 ? 1 : store.take_pos + 1)
    v
end

isready{T}(c::Channel{T, ChannelDataSingle{T}}) = c.store.full
isready{T}(c::Channel{T, ChannelDataMultiple{T}}) = (store = c.store; store.take_pos == store.put_pos ? false : true)

function wait(c::Channel)
    while !isready(c)
        !isopen(c) && throw(ChannelClosedException())
        wait(c.cond_take)
    end
    nothing
end

function notify_error(c::Channel, err)
    notify_error(c.cond_take, err)
    notify_error(c.cond_put, err)
end

eltype{T}(c::Channel{T}) = T

function length{T}(c::Channel{T, ChannelDataMultiple{T}})
    store=c.store
    if store.put_pos >= store.take_pos
        return store.put_pos - store.take_pos
    else
        return store.szp1 - store.take_pos + store.put_pos
    end
end

length{T}(c::Channel{T, ChannelDataSingle{T}}) = c.store.full ? 1 : 0

size{T}(c::Channel{T, ChannelDataSingle{T}}) = 1
size{T}(c::Channel{T, ChannelDataMultiple{T}}) = c.store.sz_max

show(io::IO, c::Channel) = print(io, "$(typeof(c))(id: $(c.cid), size: $(size(c)), num_elements: $(length(c)))")
