# This file is a part of Julia. License is MIT: http://julialang.org/license

import Base: put!, wait, isready, take!, fetch

type DictChannel <: AbstractChannel
    d::Dict
    cond_take::Condition    # waiting for data to become available
    DictChannel() = new(Dict(), Condition())
end

function put!(D::DictChannel, k, v)
    D.d[k] = v
    notify(D.cond_take)
    D
end

function take!(D::DictChannel, k)
    v=fetch(D,k)
    delete!(D.d, k)
    v
end

isready(D::DictChannel) = length(D.d) > 1
isready(D::DictChannel, k) = haskey(D.d,k)
function fetch(D::DictChannel, k)
    wait(D,k)
    D.d[k]
end

function wait(D::DictChannel, k)
    while !isready(D, k)
        wait(D.cond_take)
    end
end

# Usage:

# RemoteRef to a DictChannel on worker pid
# dc_ref=RemoteRef(()->DictChannel(), pid)

# Test if there is any data
# isready(dc_ref)

# add
# put!(dc_ref, 1, 2)

# Test if key 1 exists
# isready(dc_ref, 1)

# fetch key 1
# fetch(dc_ref, 1)

# fetch and remove key 1
# take!(dc_ref, 1)

# wait for key 3 to be added
# wait(dc_ref, 3)
