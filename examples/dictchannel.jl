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
